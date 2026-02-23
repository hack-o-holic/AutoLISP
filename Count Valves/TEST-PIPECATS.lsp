;;; TEST-PIPECATS.lsp
;;; Diagnostic utility: dynamically discovers all pipe style categories in a
;;; selection by reading LandFX XDATA directly from the drawing entities.
;;;
;;; Commands:
;;;   TESTPIPECATS   - category summary + XDATA field dump per category
;;;                    + consistency check across all pipes in each category

;;; ── Helpers ──────────────────────────────────────────────────────────────────

(defun tpc-get-lafx-xd (ename / xdall xdentry)
  (setq xdall   (entget ename '("LandFX")))
  (setq xdentry (assoc -3 xdall))
  (if xdentry (cadr xdentry) nil)
)

(defun tpc-xd-strings (appdata / result)
  (setq result '())
  (foreach pair (cdr appdata)
    (if (= (car pair) 1000)
      (setq result (append result (list (cdr pair))))
    )
  )
  result
)

(defun tpc-classify (cat)
  (cond
    ((vl-string-search "pipe-lateral" cat) "LATERAL")
    ((vl-string-search "pipe-main"    cat) "MAIN")
    ((vl-string-search "pipe-sleeve"  cat) "SLEEVE")
    (T "OTHER")
  )
)

;;; Add val to a list only if not already present
(defun tpc-add-unique (val lst)
  (if (member val lst) lst (append lst (list val)))
)

;;; ── Core scan: collect per-category data ─────────────────────────────────────
;;;
;;; Returns an assoc list keyed by category string.
;;; Each value is a list:
;;;   (count sample-strings distinct-field-sets)
;;; where distinct-field-sets is itself an assoc list of
;;;   (field-index . (list-of-distinct-values))

(defun tpc-collect (ss / i ename entdata xd strings cat n
                       result entry cnt sample dfs fi val dentry)
  (setq result '())
  (setq i 0)
  ;; Fields we want to track for consistency (indices into strings list)
  ;; 0=class, 1=type-desc, 2=size, 3-7=unknown, 8=category
  ;; We track all indices found so we can discover the structure
  (while (< i (sslength ss))
    (setq ename   (ssname ss i))
    (setq entdata (entget ename))
    (setq xd      (tpc-get-lafx-xd ename))
    (if (and xd (= (cdr (assoc 0 entdata)) "LWPOLYLINE"))
      (progn
        (setq strings (tpc-xd-strings xd))
        (setq n (length strings))
        (if (>= n 9)
          (progn
            (setq cat (nth 8 strings))
            (if (and cat (/= cat ""))
              (progn
                (setq entry (assoc cat result))
                (if (null entry)
                  ;; First pipe in this category — create entry with this as sample
                  (progn
                    ;; Build initial distinct-field-sets: one entry per string index
                    (setq dfs '())
                    (setq fi 0)
                    (foreach s strings
                      (setq dfs (append dfs (list (cons fi (list s)))))
                      (setq fi (1+ fi))
                    )
                    (setq result (append result
                      (list (list cat 1 strings dfs))))
                  )
                  ;; Subsequent pipe — update count and distinct values
                  (progn
                    (setq cnt    (cadr   entry))
                    (setq sample (caddr  entry))
                    (setq dfs    (cadddr entry))
                    ;; Merge string values into distinct sets
                    (setq fi 0)
                    (foreach s strings
                      (setq dentry (assoc fi dfs))
                      (if dentry
                        (setq dfs (subst
                          (cons fi (tpc-add-unique s (cdr dentry)))
                          dentry dfs))
                        ;; New index not seen before (strings grew) — add it
                        (setq dfs (append dfs (list (cons fi (list s)))))
                      )
                      (setq fi (1+ fi))
                    )
                    (setq result (subst
                      (list cat (1+ cnt) sample dfs)
                      entry result))
                  )
                )
              )
            )
          )
        )
      )
    )
    (setq i (1+ i))
  )
  result
)

;;; ── Report ───────────────────────────────────────────────────────────────────

(defun tpc-print-field (fi vals cnt / label)
  ;; Print one field row: index, value(s), consistency flag
  (setq label (strcat "    [" (itoa fi) "] "))
  (if (= (length vals) 1)
    ;; Consistent across all pipes
    (princ (strcat label "\"" (car vals) "\""))
    ;; Multiple distinct values
    (progn
      (princ (strcat label "*** " (itoa (length vals)) " DISTINCT VALUES ***"))
      (foreach v vals
        (princ (strcat "\n         \"" v "\""))
      )
    )
  )
  (princ "\n")
)

(defun tpc-report (data / entry cat cnt sample dfs class total)
  (setq total 0)
  (foreach entry data (setq total (+ total (cadr entry))))

  (princ "\n\n══ PIPE CATEGORY REPORT ══════════════════════════════════════")
  (princ (strcat "\n  Total LandFX polylines with XDATA: " (itoa total)))
  (princ "\n══════════════════════════════════════════════════════════════")

  (foreach entry data
    (setq cat    (car    entry))
    (setq cnt    (cadr   entry))
    (setq sample (caddr  entry))
    (setq dfs    (cadddr entry))
    (setq class  (tpc-classify cat))

    (princ (strcat "\n\n  [" class "]  category: \"" cat "\"  x" (itoa cnt)))
    (princ "\n  XDATA string fields (** = inconsistent across pipes):")
    (foreach dentry dfs
      (tpc-print-field (car dentry) (cdr dentry) cnt)
    )
  )

  (princ "\n\n══ CONSISTENCY SUMMARY ═══════════════════════════════════════")
  (foreach entry data
    (setq cat (car entry))
    (setq cnt (cadr entry))
    (setq dfs (cadddr entry))
    (setq class (tpc-classify cat))
    ;; Check for any field with multiple distinct values
    (setq problems '())
    (foreach dentry dfs
      (if (> (length (cdr dentry)) 1)
        (setq problems (append problems (list (car dentry))))
      )
    )
    (if problems
      (progn
        (princ (strcat "\n  [" class "] \"" cat "\": INCONSISTENT fields: "))
        (foreach fi problems (princ (strcat "[" (itoa fi) "] ")))
      )
      (princ (strcat "\n  [" class "] \"" cat "\": all fields consistent"))
    )
  )
  (princ "\n══════════════════════════════════════════════════════════════\n")
)

;;; ── Command ──────────────────────────────────────────────────────────────────

(defun C:TESTPIPECATS ( / ss data)
  (princ "\nTESTPIPECATS - Select pipes to scan (or Enter for all polylines): ")
  (setq ss (ssget '((0 . "LWPOLYLINE"))))
  (if ss
    (progn
      (princ (strcat "\nScanning " (itoa (sslength ss)) " polylines..."))
      (setq data (tpc-collect ss))
      (if data
        (tpc-report data)
        (princ "\nNo LandFX pipe XDATA found in selection.")
      )
    )
    (princ "\nNothing selected.")
  )
  (princ)
)

(princ "\nTESTPIPECATS loaded.")
(princ "\n  TESTPIPECATS - per-category XDATA field dump + consistency check")
(princ)
