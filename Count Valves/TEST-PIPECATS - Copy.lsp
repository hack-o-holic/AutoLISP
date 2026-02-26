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

;;; ─────────────────────────────────────────────────────────────────────────────
;;; PIPECATSCSV - Export LandFX pipe XDATA (strings only) to CSV
;;; - One row per entity found
;;; - Header is the XDATA position indexes: [0],[1],...,[N-1]
;;; - Entities without LandFX XDATA are skipped
;;; Civil 3D 2026 / AutoCAD 2026 compatible
;;; ─────────────────────────────────────────────────────────────────────────────

(vl-load-com)

;; Existing helper: get LandFX XDATA (-3) record
(defun tpc-get-lafx-xd (ename / xdall xdentry)
  (setq xdall (entget ename '("LandFX")))
  (setq xdentry (assoc -3 xdall))
  (if xdentry (cadr xdentry) nil)
)

;; Existing helper: collect only DXF 1000 (strings) from XDATA chunk
(defun tpc-xd-strings (appdata / result)
  (setq result '())
  (foreach pair (cdr appdata)
    (if (= (car pair) 1000)
      (setq result (append result (list (cdr pair))))
    )
  )
  result
)

;; Replace all occurrences of OLD with NEW in string S
(defun tpc-replace-all (s old new / pos out)
  (cond
    ((or (null s) (null old) (= old "")) s)
    (T
      (setq out "")
      (while (setq pos (vl-string-search old s))
        ;; vl-string-search is 0-based; SUBSTR is 1-based
        (setq out (strcat out (substr s 1 pos) new))
        (setq s (substr s (+ pos (strlen old) 1)))
      )
      (strcat out s)
    )
  )
)

;; CSV-escape a single field (wrap in quotes, double any embedded quotes)
(defun tpc-csv-escape (s / s2)
  (cond
    ((null s) "\"\"")
    (T
      (setq s2 (tpc-replace-all s "\r" " "))
      (setq s2 (tpc-replace-all s2 "\n" " "))
      (setq s2 (tpc-replace-all s2 "\"" "\"\""))
      (strcat "\"" s2 "\"")
    )
  )
)

;; Join list of strings with a separator (strings should already be CSV-escaped)
(defun tpc-join (lst sep / out)
  (if lst
    (progn
      (setq out (car lst))
      (foreach x (cdr lst) (setq out (strcat out sep x)))
      out
    )
    ""
  )
)

;; Scan selection set -> rows (list of string lists) and max column count
(defun tpc-export-collect (ss / i ename entdata xd strings rows maxn)
  (setq rows '() maxn 0 i 0)
  (while (< i (sslength ss))
    (setq ename (ssname ss i))
    (setq entdata (entget ename))
    (if (= (cdr (assoc 0 entdata)) "LWPOLYLINE")
      (progn
        (setq xd (tpc-get-lafx-xd ename))
        (if xd
          (progn
            (setq strings (tpc-xd-strings xd))
            (if strings
              (progn
                (setq rows (cons strings rows))
                (if (> (length strings) maxn)
                  (setq maxn (length strings))
                )
              )
            )
          )
        )
      )
    )
    (setq i (1+ i))
  )
  (list (reverse rows) maxn)
)

;; Write CSV: header [0]...[maxn-1], then each row's fields in position
(defun tpc-write-csv (path rows maxn / f header i row j val line)
  (if (setq f (open path "w"))
    (progn
      ;; Header
      (setq header '() i 0)
      (while (< i maxn)
        (setq header (cons (tpc-csv-escape (strcat "[" (itoa i) "]")) header))
        (setq i (1+ i))
      )
      (write-line (tpc-join (reverse header) ",") f)

      ;; Rows
      (foreach row rows
        (setq j 0 line '())
        (while (< j maxn)
          (setq val (if (< j (length row)) (nth j row) ""))
          (setq line (cons (tpc-csv-escape val) line))
          (setq j (1+ j))
        )
        (write-line (tpc-join (reverse line) ",") f)
      )

      (close f)
      T
    )
    nil
  )
)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; PIPECATSCSV - Export LWPOLYLINE metadata + LandFX XDATA to CSV
;;; - One row per entity (no filtering: rows are written even if no LandFX XDATA)
;;; - Metadata columns first: Handle, Layer, Length, Closed
;;; - XDATA columns follow, preserving positional order and labeling as [idx]:codes
;;;   where 'codes' is the union of DXF group codes seen at that position (e.g., 1000|1040)
;;; Civil 3D 2026 / AutoCAD 2026 compatible
;;; ─────────────────────────────────────────────────────────────────────────────

(vl-load-com)

;; --- Safe CSV helpers ---------------------------------------------------------
(defun tpc-replace-all (s old new / pos out)
  (cond
    ((or (null s) (null old) (= old "")) s)
    (T
      (setq out "")
      (while (setq pos (vl-string-search old s))
        (setq out (strcat out (substr s 1 pos) new))
        (setq s (substr s (+ pos (strlen old) 1)))
      )
      (strcat out s)
    )
  )
)

(defun tpc-csv-escape (s / s2)
  (cond
    ((null s) "\"\"")
    (T
      (setq s2 (tpc-replace-all s "\r" " "))
      (setq s2 (tpc-replace-all s2 "\n" " "))
      (setq s2 (tpc-replace-all s2 "\"" "\"\""))
      (strcat "\"" s2 "\"")
    )
  )
)

(defun tpc-join (lst sep / out)
  (if lst
    (progn
      (setq out (car lst))
      (foreach x (cdr lst) (setq out (strcat out sep x)))
      out
    )
    ""
  )
)

;; --- XDATA & geometry helpers -------------------------------------------------
;; Get LandFX XDATA (-3) list for an entity; returns the single appdata chunk or NIL
(defun tpc-get-lafx-xd (ename / xdall xdentry)
  (setq xdall   (entget ename '("LandFX")))
  (setq xdentry (assoc -3 xdall))
  (if xdentry (cadr xdentry) nil)  ;; appdata chunk (e.g., ("LandFX" (1000 . "x") ...))
)

;; Return the list of (code . value) pairs from an appdata chunk (all types)
(defun tpc-xd-pairs (appdata / out)
  (setq out '())
  (foreach pair (cdr appdata)       ;; skip app name at CAR
    (if (and (consp pair) (numberp (car pair)))
      (setq out (append out (list pair)))
    )
  )
  out
)

;; Convert an XDATA value to string based on its DXF code
(defun tpc-xd-val->str (code val / fmt n s)
  (cond
    ;; Strings, handles, group braces
    ((member code '(1000 1002 1005))
      (if (vl-stringp val) val (vl-princ-to-string val)))
    ;; 3D points -> "x,y,z" (6 decimals)
    ((member code '(1010 1011 1012))
      (if (and (listp val) (= (length val) 3))
        (strcat
          (rtos (nth 0 val) 2 6) "," (rtos (nth 1 val) 2 6) "," (rtos (nth 2 val) 2 6))
        (vl-princ-to-string val)))
    ;; Reals
    ((member code '(1040 1041 1042))
      (cond ((numberp val) (rtos val 2 6)) (T (vl-princ-to-string val))))
    ;; Integers/longs
    ((member code '(1070 1071))
      (cond ((numberp val) (itoa val)) (T (vl-princ-to-string val))))
    (T (vl-princ-to-string val))
  )
)

;; Length of a LWPOLYLINE (double)
(defun tpc-lw-length (ename / vla)
  (setq vla (vlax-ename->vla-object ename))
  (vlax-curve-getDistAtParam vla (vlax-curve-getEndParam vla))
)

;; Closed flag from DXF 70 bit 1
(defun tpc-lw-closed? (entdata / flag)
  (setq flag (cdr (assoc 70 entdata)))
  (if (and flag (= 1 (logand 1 flag))) T nil)
)

;; --- Collector: build rows + track XDATA codes by position -------------------
;; Returns: (list rows maxn code-map)
;;   rows    => list of row data, each row is (Handle Layer Length Closed x0 x1 x2 ...)
;;   maxn    => maximum count of XDATA items across all entities
;;   code-map=> assoc where key=index, value=list of distinct DXF codes seen at that index
(defun tpc-export-collect (ss / i ename entdata handle layer closed len xd pairs row
                               rows maxn idx code-map code-list)
  (setq rows '() maxn 0 code-map '() i 0)
  (while (< i (sslength ss))
    (setq ename   (ssname ss i)
          entdata (entget ename))
    (if (= (cdr (assoc 0 entdata)) "LWPOLYLINE")
      (progn
        ;; metadata
        (setq handle (cdr (assoc 5 entdata)))
        (setq layer  (cdr (assoc 8 entdata)))
        (setq len    (tpc-lw-length ename))
        (setq closed (if (tpc-lw-closed? entdata) "Yes" "No"))

        ;; LandFX XDATA (do not filter entities—row is still written even if NIL)
        (setq xd (tpc-get-lafx-xd ename))
        (setq pairs (if xd (tpc-xd-pairs xd) '()))

        ;; grow code-map with codes observed at each position
        (setq idx 0)
        (foreach p pairs
          (setq code-list (cdr (assoc idx code-map)))
          (if code-list
            (if (not (member (car p) code-list))
              (setq code-map (subst (cons idx (cons (car p) code-list))
                                    (assoc idx code-map) code-map))
            )
            ;; first time seeing this index
            (setq code-map (append code-map (list (cons idx (list (car p))))))
          )
          (setq idx (1+ idx))
        )

        ;; build row: metadata + XDATA values converted to strings
        (setq row (list
                    (if handle handle "")                      ;; Handle
                    (if layer  layer  "")                      ;; Layer
                    (rtos len 2 3)                             ;; Length (3 decimals)
                    closed))                                   ;; Closed
        (foreach p pairs
          (setq row (append row (list (tpc-xd-val->str (car p) (cdr p))))))
        (setq rows (cons row rows))

        ;; update maxn
        (if (> (length pairs) maxn) (setq maxn (length pairs)))
      )
    )
    (setq i (1+ i))
  )
  (list (reverse rows) maxn code-map)
)

;; --- CSV writer ---------------------------------------------------------------
(defun tpc-write-csv (path rows maxn code-map / f header i codes label j row line val)
  (if (setq f (open path "w"))
    (progn
      ;; Header: metadata first
      (setq header (list
        (tpc-csv-escape "Handle")
        (tpc-csv-escape "Layer")
        (tpc-csv-escape "Length")
        (tpc-csv-escape "Closed")
      ))

      ;; Then XDATA positional headers labeled "[idx]:code|code"
      (setq i 0)
      (while (< i maxn)
        (setq codes (cdr (assoc i code-map)))
        (setq label
          (if codes
            (strcat "[" (itoa i) "]:" (tpc-join (mapcar 'vl-princ-to-string (reverse codes)) "|"))
            (strcat "[" (itoa i) "]:")
          )
        )
        (setq header (append header (list (tpc-csv-escape label))))
        (setq i (1+ i))
      )
      (write-line (tpc-join header ",") f)

      ;; Rows
      (foreach row rows
        ;; Pad out to metadata(4) + maxn XDATA columns
        (setq line '())
        (setq j 0)
        (while (< j (+ 4 maxn))
          (setq val (if (< j (length row)) (nth j row) ""))
          (setq line (append line (list (tpc-csv-escape val))))
          (setq j (1+ j))
        )
        (write-line (tpc-join line ",") f)
      )

      (close f)
      T
    )
    nil
  )
)

;;; --- Command -----------------------------------------------------------------
(defun C:PIPECATSCSV ( / ss data rows maxn code-map default path)
  (regapp "LandFX")
  (princ "\nPIPECATSCSV - Select LWPOLYLINEs to export (Enter = entire drawing): ")
  ;; Try interactive pick; Enter -> whole drawing
  (setq ss (ssget '((0 . "LWPOLYLINE"))))
  (if (not ss) (setq ss (ssget "_X" '((0 . "LWPOLYLINE")))))

  (if ss
    (progn
      (princ (strcat "\nScanning " (itoa (sslength ss)) " polylines..."))
      (setq data (tpc-export-collect ss))
      (setq rows     (car data)
            maxn     (cadr data)
            code-map (caddr data))

      (if rows
        (progn
          (setq default (strcat "PipeXDATA_"
                                (menucmd "M=$(edtime,$(getvar,date),YYYYMMDD_hhmmss)")
                                ".csv"))
          (setq path (getfiled "Save CSV" default "csv" 1))
          (if path
            (if (tpc-write-csv path rows maxn code-map)
              (princ (strcat "\nCSV written: " path))
              (princ "\nFailed to write CSV.")
            )
            (princ "\nCanceled.")
          )
        )
        (princ "\nNo LWPOLYLINE rows to export.")
      )
    )
    (princ "\nNo polylines found.")
  )
  (princ)
)

(princ "\nPIPECATSCSV loaded. Run PIPECATSCSV to export metadata + XDATA to CSV.")