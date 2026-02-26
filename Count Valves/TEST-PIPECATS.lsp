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

;;; ── CSV Export: full entity metadata + all XDATA fields ──────────────────────
;;;
;;; Command:  PIPEXPORT
;;;   Prompts for a selection set, then writes a CSV where each row is one
;;;   entity and columns are: Handle, EntityType, Layer, Color, Linetype,
;;;   LineWeight, Closed, Vertices, Length, XD_AppName, followed by every
;;;   XDATA group found (XD_00, XD_01, ... formatted as "groupcode:value").
;;;   No summarizing — every entity gets its own row, every XDATA field
;;;   gets its own column.

;;; Escape a value for CSV (wrap in quotes, double any internal quotes)
(defun pxp-csv-escape (val / s pos)
  (if (null val) (setq val ""))
  (setq s (vl-princ-to-string val))
  (setq pos 0)
  (while (setq pos (vl-string-search "\"" s pos))
    (setq s (strcat
      (substr s 1 pos)
      "\"\""
      (substr s (+ pos 2))
    ))
    (setq pos (+ pos 2))
  )
  (strcat "\"" s "\"")
)

;;; Join a list of strings with a delimiter
(defun pxp-join (lst delim / out first)
  (setq out "" first T)
  (foreach s lst
    (if first
      (setq out s first nil)
      (setq out (strcat out delim s))
    )
  )
  out
)

;;; Get all registered application names from XDATA on an entity
(defun pxp-get-all-appnames (ename / xdall xd3 apps)
  (setq xdall (entget ename '("*")))
  (setq xd3   (assoc -3 xdall))
  (setq apps  '())
  (if xd3
    (foreach appblock (cdr xd3)
      (if (and (listp appblock) (= (type (car appblock)) 'STR))
        (setq apps (append apps (list (car appblock))))
      )
    )
  )
  apps
)

;;; Get all XDATA groups for a given appname — returns full DXF pairs list
(defun pxp-get-xdata-pairs (ename appname / xdall xd3 appblock)
  (setq xdall (entget ename (list appname)))
  (setq xd3   (assoc -3 xdall))
  (if xd3
    (progn
      (setq appblock (assoc appname (cdr xd3)))
      (if appblock (cdr appblock) nil)
    )
    nil
  )
)

;;; Get polyline length via vlax
(defun pxp-pline-length (ename / obj len)
  (setq len "")
  (if (setq obj (vlax-ename->vla-object ename))
    (if (vlax-property-available-p obj 'Length)
      (setq len (rtos (vlax-get-property obj 'Length) 2 4))
    )
  )
  len
)

;;; Determine if an LWPOLYLINE is closed (bit 1 of group 70)
(defun pxp-is-closed (entdata / flag)
  (setq flag (cdr (assoc 70 entdata)))
  (if (and flag (= (logand flag 1) 1)) "Yes" "No")
)

;;; Count vertices in an LWPOLYLINE (number of group 10 entries)
(defun pxp-vertex-count (entdata / n)
  (setq n 0)
  (foreach pair entdata
    (if (= (car pair) 10) (setq n (1+ n)))
  )
  n
)

;;; ── First pass: collect raw XDATA pairs per entity ───────────────────────────
;;; Returns list of row-records.  Each record is:
;;;   (meta-list  appname  xd-pair-list)
;;; where xd-pair-list is ((code . val) (code . val) ...)

(defun pxp-format-val (pair / v)
  (cond
    ((= (type (cdr pair)) 'STR)  (cdr pair))
    ((= (type (cdr pair)) 'INT)  (itoa (cdr pair)))
    ((= (type (cdr pair)) 'REAL) (rtos (cdr pair) 2 6))
    (T (vl-princ-to-string (cdr pair)))
  )
)

(defun pxp-collect-raw (ss / i ename entdata handle etype layer color ltype
                             lwt closed verts pllen apps appname xdpairs
                             rawpairs pair rows meta)
  (setq rows '() i 0)
  (while (< i (sslength ss))
    (setq ename   (ssname ss i))
    (setq entdata (entget ename))
    (setq handle  (cdr (assoc 5  entdata)))
    (setq etype   (cdr (assoc 0  entdata)))
    (setq layer   (cdr (assoc 8  entdata)))
    (setq color   (if (assoc 62 entdata) (itoa (cdr (assoc 62 entdata))) "BYLAYER"))
    (setq ltype   (if (assoc 6  entdata) (cdr (assoc 6 entdata)) "BYLAYER"))
    (setq lwt     (if (assoc 370 entdata) (itoa (cdr (assoc 370 entdata))) "DEFAULT"))

    (if (= etype "LWPOLYLINE")
      (progn
        (setq closed (pxp-is-closed entdata))
        (setq verts  (itoa (pxp-vertex-count entdata)))
        (setq pllen  (pxp-pline-length ename))
      )
      (progn
        (setq closed "" verts "" pllen "")
      )
    )

    (setq meta (list handle etype layer color ltype lwt closed verts pllen))
    (setq apps (pxp-get-all-appnames ename))

    (if apps
      (foreach appname apps
        (setq xdpairs (pxp-get-xdata-pairs ename appname))
        (setq rawpairs '())
        (if xdpairs
          (foreach pair xdpairs
            (setq rawpairs (append rawpairs
              (list (cons (car pair) (pxp-format-val pair)))))
          )
        )
        (setq rows (append rows (list (list meta appname rawpairs))))
      )
      ;; No XDATA
      (setq rows (append rows (list (list meta "" nil))))
    )
    (setq i (1+ i))
  )
  rows
)

;;; ── Build master column template ─────────────────────────────────────────────
;;; Walk every entity's pairs to find the majority group code at each position.
;;; Then expand: if position N has two different codes, the majority keeps slot N
;;; and the minority gets an extra slot inserted after it.
;;;
;;; Strategy (simple & correct for the described scenario):
;;;   1.  Collect all code sequences across entities
;;;   2.  For each position, tally which codes appear
;;;   3.  The most-frequent code owns that column
;;;   4.  Any minority code at that position gets pushed to a new column
;;;       inserted right after — and the entity gets a blank in the majority col.

;;; Return: list of integer group codes representing column order

(defun pxp-build-template (rawrows / maxpos pos tally rec pairs code
                                     entry template majority
                                     minors mc mi expanded)
  ;; Find max pair count
  (setq maxpos 0)
  (foreach rec rawrows
    (if (and (caddr rec) (> (length (caddr rec)) maxpos))
      (setq maxpos (length (caddr rec)))
    )
  )

  ;; For each position, tally codes
  (setq template '())
  (setq pos 0)
  (while (< pos maxpos)
    (setq tally '())
    (foreach rec rawrows
      (setq pairs (caddr rec))
      (if (and pairs (< pos (length pairs)))
        (progn
          (setq code (car (nth pos pairs)))
          (setq entry (assoc code tally))
          (if entry
            (setq tally (subst (cons code (1+ (cdr entry))) entry tally))
            (setq tally (append tally (list (cons code 1))))
          )
        )
      )
    )
    ;; Find majority code (highest count)
    (setq majority (caar tally) mc (cdar tally))
    (foreach entry tally
      (if (> (cdr entry) mc)
        (setq majority (car entry) mc (cdr entry))
      )
    )
    ;; Collect minority codes at this position
    (setq minors '())
    (foreach entry tally
      (if (/= (car entry) majority)
        (setq minors (append minors (list (car entry))))
      )
    )
    ;; majority column first, then one column per minority code
    (setq template (append template (list majority)))
    (foreach mi minors
      (setq template (append template (list mi)))
    )
    (setq pos (1+ pos))
  )
  template
)

;;; ── Align one entity's pairs to the master template ──────────────────────────
;;; Returns a list of string values (same length as template), blanks where
;;; the entity has no data for that column.

(defun pxp-align-row (pairs template / result tcol tcode pi pcode pval matched)
  (setq result '())
  (setq pi 0)  ;; pointer into pairs
  (foreach tcode template
    (setq matched nil)
    (if (and pairs (< pi (length pairs)))
      (progn
        (setq pcode (car (nth pi pairs)))
        (setq pval  (cdr (nth pi pairs)))
        (if (= pcode tcode)
          (progn
            (setq result (append result (list pval)))
            (setq pi (1+ pi))
            (setq matched T)
          )
        )
      )
    )
    (if (not matched)
      (setq result (append result (list "")))
    )
  )
  result
)

;;; ── CSV Writer ───────────────────────────────────────────────────────────────

(defun pxp-write-csv (fname template rawrows / fp hdr row ncols padded
                             rec meta appname pairs aligned)
  (setq fp (open fname "w"))
  (if (null fp)
    (progn (princ (strcat "\n*** Cannot open file: " fname)) (exit))
  )

  ;; Build header: metadata columns + one column per template slot
  (setq hdr (list "Handle" "EntityType" "Layer" "Color" "Linetype"
                  "LineWeight" "Closed" "Vertices" "Length" "XD_AppName"))
  (foreach tcode template
    (setq hdr (append hdr (list (itoa tcode))))
  )
  (setq ncols (length hdr))

  ;; Write header
  (write-line (pxp-join (mapcar 'pxp-csv-escape hdr) ",") fp)

  ;; Write data rows
  (foreach rec rawrows
    (setq meta    (car   rec))
    (setq appname (cadr  rec))
    (setq pairs   (caddr rec))

    (if pairs
      (setq aligned (pxp-align-row pairs template))
      (progn
        ;; No XDATA — empty columns
        (setq aligned '())
        (repeat (length template) (setq aligned (append aligned (list ""))))
      )
    )

    (setq row (append meta (list appname) aligned))
    ;; Pad if needed
    (while (< (length row) ncols)
      (setq row (append row (list "")))
    )
    (write-line (pxp-join (mapcar 'pxp-csv-escape row) ",") fp)
  )

  (close fp)
  ncols
)

;;; ── Command: PIPEXPORT ───────────────────────────────────────────────────────

(defun C:PIPEXPORT ( / ss rawrows template fname ncols)
  (princ "\nPIPEXPORT - Select pipes to export (or Enter for all polylines): ")
  (setq ss (ssget '((0 . "LWPOLYLINE"))))
  (if (null ss)
    (progn (princ "\nNothing selected.") (princ) (quit))
  )
  (princ (strcat "\nProcessing " (itoa (sslength ss)) " polylines..."))

  ;; Collect raw data
  (setq rawrows (pxp-collect-raw ss))
  (princ (strcat "\nFound " (itoa (length rawrows)) " rows."))

  ;; Build column template from majority codes
  (setq template (pxp-build-template rawrows))
  (princ (strcat "\nXDATA columns: " (itoa (length template))))

  ;; Prompt for output file
  (setq fname (getfiled "Save XDATA Export CSV" "" "csv" 1))
  (if (null fname)
    (progn (princ "\nCancelled.") (princ) (quit))
  )

  (setq ncols (pxp-write-csv fname template rawrows))

  (princ (strcat "\nExported " (itoa (length rawrows)) " rows x "
    (itoa ncols) " columns to:"))
  (princ (strcat "\n  " fname))
  (princ "\nDone.")
  (princ)
)

;;; ── Load messages ────────────────────────────────────────────────────────────

(princ "\nTESTPIPECATS loaded.")
(princ "\n  TESTPIPECATS - per-category XDATA field dump + consistency check")
(princ "\n  PIPEXPORT   - export entity metadata + all XDATA to CSV")
(princ)