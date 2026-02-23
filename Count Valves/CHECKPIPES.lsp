;;; CHECKPIPES.lsp
;;; Dialog-based diagnostic for mismatched pipe XDATA and bad valve connections.
;;; Lists problems in a scrollable list - click item to select, Zoom To to zoom,
;;; Fix Pipe to auto-correct XDATA from connected mainline.

;;; ── Helpers ──────────────────────────────────────────────────────────────────

(defun xd-strings (appdata / result)
  (setq result '())
  (foreach pair (cdr appdata)
    (if (= (car pair) 1000)
      (setq result (append result (list (cdr pair))))
    )
  )
  result
)

(defun get-lafx-xd (ename / xdall xdentry)
  (setq xdall (entget ename '("LandFX")))
  (setq xdentry (assoc -3 xdall))
  (if xdentry (cadr xdentry) nil)
)

(defun get-handles (appdata / result)
  (setq result '())
  (foreach pair (cdr appdata)
    (if (= (car pair) 1005)
      (setq result (append result (list (cdr pair))))
    )
  )
  result
)

(defun get-pipe-info (pipeent / pxd pstrings size cat)
  (setq pxd (get-lafx-xd pipeent))
  (if pxd
    (progn
      (setq pstrings (xd-strings pxd))
      (setq size (if (>= (length pstrings) 3) (nth 2 pstrings) "?"))
      (setq cat  (if (>= (length pstrings) 9) (nth 8 pstrings) "?"))
      (list size cat)
    )
    nil
  )
)

(defun find-prefixed (prefix strlist / found)
  (setq found nil)
  (foreach s strlist
    (if (and (not found)
             (>= (strlen s) (strlen prefix))
             (= (substr s 1 (strlen prefix)) prefix))
      (setq found s)
    )
  )
  found
)

(defun classify-valve (strings bname / code)
  (if (< (length strings) 2)
    "UNKNOWN"
    (progn
      (setq code (if (>= (length strings) 13) (nth 12 strings) ""))
      (cond
        ((not (= (nth 0 strings) "Valve"))          "UNKNOWN")
        ((= (nth 1 strings) "Remote Control Valve") "RCV")
        ((or (vl-string-search "SOV-005" code)
             (vl-string-search "SOV-005" bname))    "LATERAL-SOV")
        ((or (vl-string-search "SOV-001" code)
             (vl-string-search "SOV-001" bname))    "MAINLINE-SOV")
        (T "UNKNOWN")
      )
    )
  )
)

;;; Zoom to entity and highlight via selection
(defun zoom-to-ent (ent / entdata pt hs)
  (setq entdata (entget ent))
  (setq pt (cdr (assoc 10 entdata)))
  (if pt
    (command "_.ZOOM" "_C" pt (/ (getvar "VIEWSIZE") 1.5))
    (progn
      (command "_.ZOOM" "_O" ent "")
      (command "_.ZOOM" "0.5X")
    )
  )
  (command "_.REDRAW")
  (setq hs (ssadd ent (ssadd)))
  (sssetfirst hs hs)
)

;;; ── Scan Functions ───────────────────────────────────────────────────────────

(defun scan-pipes (ss / i ename entdata xd strings type-desc category
                      desc-is-lateral desc-is-main cat-is-lateral cat-is-main
                      results size desc)
  (setq results '())
  (setq i 0)
  (while (< i (sslength ss))
    (setq ename   (ssname ss i))
    (setq entdata (entget ename))
    (setq xd      (get-lafx-xd ename))
    (if (and xd (= (cdr (assoc 0 entdata)) "LWPOLYLINE"))
      (progn
        (setq strings (xd-strings xd))
        (if (>= (length strings) 9)
          (progn
            (setq type-desc (nth 1 strings))
            (setq category  (nth 8 strings))
            (setq size      (if (>= (length strings) 3) (nth 2 strings) "?"))
            (setq desc-is-lateral (and type-desc (vl-string-search "Lateral" type-desc)))
            (setq desc-is-main    (and type-desc (vl-string-search "Main"    type-desc)))
            (setq cat-is-lateral  (and category   (vl-string-search "pipe-lateral" category)))
            (setq cat-is-main     (and category   (vl-string-search "pipe-main"    category)))
            (if (or (and desc-is-lateral cat-is-main)
                    (and desc-is-main    cat-is-lateral))
              (progn
                (setq desc
                  (strcat
                    (cdr (assoc 5 entdata)) " | "
                    size " | "
                    type-desc " / " category
                  )
                )
                (setq results (append results (list (list (cdr (assoc 5 entdata)) desc ename))))
              )
            )
          )
        )
      )
    )
    (setq i (1+ i))
  )
  results
)

(defun scan-valves (ss / i ename entdata xd strings bname vtype
                       handles pipeent pinfo cat
                       lat-pipes main-pipes other-pipes
                       results desc)
  (setq results '())
  (setq i 0)
  (while (< i (sslength ss))
    (setq ename   (ssname ss i))
    (setq entdata (entget ename))
    (setq xd      (get-lafx-xd ename))
    (if (and xd (= (cdr (assoc 0 entdata)) "INSERT"))
      (progn
        (setq strings (xd-strings xd))
        (setq bname   (cdr (assoc 2 entdata)))
        (setq vtype   (classify-valve strings bname))
        (if (= vtype "LATERAL-SOV")
          (progn
            (setq handles     (get-handles xd))
            (setq lat-pipes   '())
            (setq main-pipes  '())
            (setq other-pipes '())
            (foreach h handles
              (setq pipeent (handent h))
              (if pipeent
                (progn
                  (setq pinfo (get-pipe-info pipeent))
                  (if pinfo
                    (progn
                      (setq cat (cadr pinfo))
                      (cond
                        ((vl-string-search "pipe-lateral" cat)
                         (setq lat-pipes (cons pinfo lat-pipes)))
                        ((vl-string-search "pipe-main" cat)
                         (setq main-pipes (cons pinfo main-pipes)))
                        (T
                         (setq other-pipes (cons pinfo other-pipes)))
                      )
                    )
                    (setq other-pipes (cons (list "dead" "dead") other-pipes))
                  )
                )
                (setq other-pipes (cons (list "missing" "dead") other-pipes))
              )
            )
            (if (not (and (= (length lat-pipes) 1) (= (length main-pipes) 1)))
              (progn
                (setq desc
                  (strcat
                    (cdr (assoc 5 entdata)) " | "
                    bname " | "
                    "lat:" (itoa (length lat-pipes))
                    " main:" (itoa (length main-pipes))
                    (if (> (length other-pipes) 0)
                      (strcat " dead:" (itoa (length other-pipes)))
                      ""
                    )
                  )
                )
                (setq results (append results (list (list (cdr (assoc 5 entdata)) desc ename))))
              )
            )
          )
        )
      )
    )
    (setq i (1+ i))
  )
  results
)

;;; ── DCL Builder ──────────────────────────────────────────────────────────────

(defun write-dcl (fname title / f)
  (setq f (open fname "w"))
  (write-line "check_dialog : dialog {" f)
  (write-line (strcat "  label = \"" title "\";") f)
  (write-line "  spacer;" f)
  (write-line "  : list_box {" f)
  (write-line "    key = \"problem_list\";" f)
  (write-line "    label = \"Problems (click to select, Zoom To to zoom)\";" f)
  (write-line "    height = 18;" f)
  (write-line "    width = 70;" f)
  (write-line "    allow_accept = true;" f)
  (write-line "  }" f)
  (write-line "  spacer;" f)
  (write-line "  : row {" f)
  (write-line "    : button { key = \"zoom_btn\"; label = \"Zoom To\"; width = 15; }" f)
  (write-line "    : button { key = \"fix_btn\"; label = \"Fix Pipe\"; width = 15; }" f)
  (write-line "    : button { key = \"fix_tpl_btn\"; label = \"Fix LAT 02 to MAIN 02\"; width = 22; }" f)
  (write-line "    : button { key = \"fix_all_btn\"; label = \"Fix All (Template)\"; width = 18; }" f)
  (write-line "    : button { key = \"rescan_btn\"; label = \"New Selection\"; width = 15; }" f)
  (write-line "    : spacer { width = 3; }" f)
  (write-line "    : button { key = \"close_btn\"; label = \"Close\"; width = 15; is_cancel = true; }" f)
  (write-line "  }" f)
  (write-line "}" f)
  (close f)
)

;;; ── Dialog Runner ────────────────────────────────────────────────────────────

(defun run-check-dialog (title problems sel-idx / dcl-file dcl-id list-items done)
  (setq list-items (mapcar 'cadr problems))
  (setq dcl-file (strcat (getvar "TEMPPREFIX") "checkdlg.dcl"))
  (write-dcl dcl-file title)
  (setq dcl-id (load_dialog dcl-file))
  (if (not (new_dialog "check_dialog" dcl-id))
    (progn (unload_dialog dcl-id) (princ "\nError: could not open dialog.") (exit))
  )
  (start_list "problem_list")
  (mapcar 'add_list list-items)
  (end_list)
  (set_tile "problem_list" (itoa sel-idx))
  (setq *DLG-SEL-IDX* sel-idx)
  (action_tile "problem_list" "(setq *DLG-SEL-IDX* (atoi $value)) (if (= $reason 4) (done_dialog 2))")
  (action_tile "zoom_btn"     "(setq *DLG-SEL-IDX* (atoi (get_tile \"problem_list\"))) (done_dialog 2)")
  (action_tile "fix_btn"      "(setq *DLG-SEL-IDX* (atoi (get_tile \"problem_list\"))) (done_dialog 4)")
  (action_tile "fix_tpl_btn"  "(setq *DLG-SEL-IDX* (atoi (get_tile \"problem_list\"))) (done_dialog 5)")
  (action_tile "fix_all_btn"  "(done_dialog 6)")
  (action_tile "rescan_btn"   "(done_dialog 3)")
  (action_tile "close_btn"    "(done_dialog 0)")
  (setq done (start_dialog))
  (unload_dialog dcl-id)
  done
)

;;; ── Fix Functions ────────────────────────────────────────────────────────────

;;; Walk: bad pipe -> connected entity -> if fitting follow its handles -> find pipe-main
(defun find-source-mainline (bad-ent / bad-xd bad-handles h connected-ent connected-ed
                                       connected-xd connected-strings connected-cat
                                       fit-xdata fit-handles fh fit-pipe fit-info)
  (setq bad-xd      (get-lafx-xd bad-ent))
  (setq bad-handles (get-handles bad-xd))
  (setq src-ent nil)
  (foreach h bad-handles
    (if (not src-ent)
      (progn
        (setq connected-ent (handent h))
        (if connected-ent
          (progn
            (setq connected-ed (entget connected-ent))
            (cond
              ;; Direct mainline pipe
              ((= (cdr (assoc 0 connected-ed)) "LWPOLYLINE")
               (setq connected-xd (get-lafx-xd connected-ent))
               (if connected-xd
                 (progn
                   (setq connected-strings (xd-strings connected-xd))
                   (setq connected-cat (if (>= (length connected-strings) 9)
                                         (nth 8 connected-strings) ""))
                   (if (vl-string-search "pipe-main" connected-cat)
                     (setq src-ent connected-ent)
                   )
                 )
               )
              )
              ;; Pipe fitting - unwrap (-3 ("LandFX" (1005 . "x") ...)) to get handles
              ((and (= (cdr (assoc 0 connected-ed)) "INSERT")
                    (vl-string-search "PIPEFITTING" (cdr (assoc 2 connected-ed))))
               (setq fit-handles '())
               (setq fit-xdata (cdr (cadr (assoc -3 (entget connected-ent '("LandFX"))))))
               (foreach pair fit-xdata
                 (if (= (car pair) 1005)
                   (setq fit-handles (append fit-handles (list (cdr pair))))
                 )
               )
               (foreach fh fit-handles
                 (if (not src-ent)
                   (progn
                     (setq fit-pipe (handent fh))
                     (if (and fit-pipe (not (equal fit-pipe bad-ent)))
                       (progn
                         (setq fit-info (get-pipe-info fit-pipe))
                         (if (and fit-info (vl-string-search "pipe-main" (cadr fit-info)))
                           (setq src-ent fit-pipe)
                         )
                       )
                     )
                   )
                 )
               )
              )
            )
          )
        )
      )
    )
  )
  src-ent
)

;;; Copy mainline XDATA onto bad pipe.
;;; Preserves bad pipe's Dp:/Df: (position hydraulics) and 1005 handles.
;;; Copies everything else from source including Ds:.
(defun fix-pipe-xdata (bad-ent / bad-ed bad-xd bad-handles
                                  src-ent src-xd new-pairs pair)
  (setq bad-xd      (get-lafx-xd bad-ent))
  (setq bad-handles (get-handles bad-xd))
  (setq src-ent     (find-source-mainline bad-ent))
  (if (null src-ent)
    (progn (princ "\n  Could not find connected mainline pipe - skipping.") nil)
    (progn
      (setq src-xd (get-lafx-xd src-ent))
      (if (null src-xd)
        (progn (princ "\n  Source pipe has no LandFX XDATA - skipping.") nil)
        (progn
          ;; All non-handle, non-Dp:, non-Df: entries from source
          (setq new-pairs '())
          (foreach pair (cdr src-xd)
            (if (and (not (= (car pair) 1005))
                     (not (and (= (car pair) 1000)
                               (or (= (substr (cdr pair) 1 3) "Dp:")
                                   (= (substr (cdr pair) 1 3) "Df:")))))
              (setq new-pairs (append new-pairs (list pair)))
            )
          )
          ;; Append Dp: and Df: from bad pipe if present
          (foreach pair (cdr bad-xd)
            (if (and (= (car pair) 1000)
                     (or (= (substr (cdr pair) 1 3) "Dp:")
                         (= (substr (cdr pair) 1 3) "Df:")))
              (setq new-pairs (append new-pairs (list pair)))
            )
          )
          ;; Append bad pipe's own handles
          (foreach h bad-handles
            (setq new-pairs (append new-pairs (list (cons 1005 h))))
          )
          ;; Write new XDATA
          (setq bad-ed (entget bad-ent '("LandFX")))
          (setq bad-ed (vl-remove (assoc -3 bad-ed) bad-ed))
          (setq bad-ed (append bad-ed (list (cons -3 (list (cons "LandFX" new-pairs))))))
          ;; Copy layer from source
          (setq bad-ed (subst
            (cons 8 (cdr (assoc 8 (entget src-ent))))
            (assoc 8 bad-ed)
            bad-ed))
          ;; Copy entity color from source (group code 62)
          ;; If source has explicit color, apply it; if not, remove any explicit color on bad pipe
          (setq src-color (assoc 62 (entget src-ent)))
          (setq bad-color (assoc 62 bad-ed))
          (cond
            ;; Source has explicit color - apply it to bad pipe
            ((and src-color bad-color)
             (setq bad-ed (subst src-color bad-color bad-ed)))
            ((and src-color (not bad-color))
             (setq bad-ed (append bad-ed (list src-color))))
            ;; Source has no explicit color - remove from bad pipe if present
            ((and (not src-color) bad-color)
             (setq bad-ed (vl-remove bad-color bad-ed)))
          )
          (entmod bad-ed)
          (entupd bad-ent)
          (princ (strcat "\n  Fixed - copied mainline XDATA from handle:"
                         (cdr (assoc 5 (entget src-ent)))))
          T
        )
      )
    )
  )
)

;;; Copy XDATA from the stored template pipe (*FIX-TEMPLATE*) onto bad-ent.
;;; Same rules as fix-pipe-xdata: copies everything from template except
;;; handles and Dp:/Df:, then appends bad pipe's Dp:/Df: and its own handles.
;;; Also copies layer and color from template.
(defun fix-from-template (bad-ent / bad-ed bad-xd bad-handles
                                     src-xd src-strings
                                     new-pairs pair src-color bad-color)
  (if (null *FIX-TEMPLATE*)
    (progn (princ "\nNo template set - run SETFIXTEMPLATE first.") nil)
    (progn
      (setq src-xd (get-lafx-xd *FIX-TEMPLATE*))
      (if (null src-xd)
        (progn (princ "\nTemplate pipe no longer valid - run SETFIXTEMPLATE again.") nil)
        (progn
          (setq bad-xd      (get-lafx-xd bad-ent))
          (setq bad-handles (get-handles bad-xd))
          ;; Copy everything from template except handles, Dp:, and Df:
          ;; Ds: IS copied - it defines the correct design size for this pipe type.
          ;; Dp:/Df: are not copied - those are per-pipe hydraulic results from sizing.
          (setq new-pairs '())
          (foreach pair (cdr src-xd)
            (if (and (not (= (car pair) 1005))
                     (not (and (= (car pair) 1000)
                               (or (= (substr (cdr pair) 1 3) "Dp:")
                                   (= (substr (cdr pair) 1 3) "Df:")))))
              (setq new-pairs (append new-pairs (list pair)))
            )
          )
          ;; Preserve Dp:/Df: from bad pipe if present (safety net - unlikely on a bad pipe)
          (foreach pair (cdr bad-xd)
            (if (and (= (car pair) 1000)
                     (or (= (substr (cdr pair) 1 3) "Dp:")
                         (= (substr (cdr pair) 1 3) "Df:")))
              (setq new-pairs (append new-pairs (list pair)))
            )
          )
          ;; Use bad pipe's own handles
          (foreach h bad-handles
            (setq new-pairs (append new-pairs (list (cons 1005 h))))
          )
          ;; Write new XDATA
          (setq bad-ed (entget bad-ent '("LandFX")))
          (setq bad-ed (vl-remove (assoc -3 bad-ed) bad-ed))
          (setq bad-ed (append bad-ed (list (cons -3 (list (cons "LandFX" new-pairs))))))
          ;; Copy layer from template
          (setq bad-ed (subst
            (cons 8 (cdr (assoc 8 (entget *FIX-TEMPLATE*))))
            (assoc 8 bad-ed)
            bad-ed))
          ;; Copy color from template
          (setq src-color (assoc 62 (entget *FIX-TEMPLATE*)))
          (setq bad-color (assoc 62 bad-ed))
          (cond
            ((and src-color bad-color)
             (setq bad-ed (subst src-color bad-color bad-ed)))
            ((and src-color (not bad-color))
             (setq bad-ed (append bad-ed (list src-color))))
            ((and (not src-color) bad-color)
             (setq bad-ed (vl-remove bad-color bad-ed)))
          )
          (entmod bad-ed)
          (entupd bad-ent)
          (setq src-strings (xd-strings src-xd))
          (princ (strcat "\n  Fixed from template: "
                         (if (>= (length src-strings) 2) (nth 1 src-strings) "?")
                         " / " (if (>= (length src-strings) 9) (nth 8 src-strings) "?")))
          T
        )
      )
    )
  )
)

;;; ── Main Loop ────────────────────────────────────────────────────────────────

(defun run-check-loop (title ss scan-fn / problems)
  (princ "\nScanning...")
  (setq problems (apply scan-fn (list ss)))
  (if (= (length problems) 0)
    (princ "\nNo problems found!")
    (run-check-open title problems ss scan-fn 0)
  )
)

(defun run-check-open (title problems ss scan-fn init-idx / result sel-ent new-ss sel-idx fixed-count)
  (setq sel-idx init-idx)
  (while (> (length problems) 0)
    (setq result (run-check-dialog
      (strcat title " - " (itoa (length problems)) " problem(s)")
      problems sel-idx))
    (cond
      ;; Zoom To - zoom then loop back to reshow dialog at same position
      ((= result 2)
       (if (and (>= *DLG-SEL-IDX* 0) (< *DLG-SEL-IDX* (length problems)))
         (progn
           (zoom-to-ent (caddr (nth *DLG-SEL-IDX* problems)))
           (setq sel-idx *DLG-SEL-IDX*)
         )
         (princ "\nNo item selected.")
       )
      )
      ;; Fix Pipe - walk connection graph, rescan, loop
      ((= result 4)
       (if (and (>= *DLG-SEL-IDX* 0) (< *DLG-SEL-IDX* (length problems)))
         (progn
           (setq sel-ent (caddr (nth *DLG-SEL-IDX* problems)))
           (princ (strcat "\nFixing pipe handle:" (car (nth *DLG-SEL-IDX* problems)) "..."))
           (if (fix-pipe-xdata sel-ent)
             (progn
               (sssetfirst nil nil)
               (princ "\nRe-scanning...")
               (setq problems (apply scan-fn (list ss)))
               (setq sel-idx (min *DLG-SEL-IDX* (max 0 (1- (length problems)))))
               (if (= (length problems) 0) (princ "\nAll clear!"))
             )
           )
         )
         (princ "\nNo item selected.")
       )
      )
      ;; Fix LAT 02 to MAIN 02 - apply template XDATA, rescan, loop
      ((= result 5)
       (if (and (>= *DLG-SEL-IDX* 0) (< *DLG-SEL-IDX* (length problems)))
         (progn
           (setq sel-ent (caddr (nth *DLG-SEL-IDX* problems)))
           (princ (strcat "\nApplying template to pipe handle:" (car (nth *DLG-SEL-IDX* problems)) "..."))
           ;; Unhighlight before fixing so the visual change is visible
           (sssetfirst nil nil)
           (if (fix-from-template sel-ent)
             (progn
               ;; Redraw so user can see the pipe change before dialog reopens
               (command "_.REDRAW")
               (princ "\nRe-scanning...")
               (setq problems (apply scan-fn (list ss)))
               (setq sel-idx (min *DLG-SEL-IDX* (max 0 (1- (length problems)))))
               (if (= (length problems) 0) (princ "\nAll clear!"))
             )
           )
         )
         (princ "\nNo item selected.")
       )
      )
      ;; Fix All (Template) - apply template to every problem in the list
      ((= result 6)
       (if (null *FIX-TEMPLATE*)
         (princ "\nNo template set - run SETFIXTEMPLATE first.")
         (progn
           (princ (strcat "\nFixing all " (itoa (length problems)) " pipes..."))
           (sssetfirst nil nil)
           (setq fixed-count 0)
           (foreach entry problems
             (if (fix-from-template (caddr entry))
               (setq fixed-count (1+ fixed-count))
             )
           )
           (command "_.REDRAW")
           (princ (strcat "\nFixed " (itoa fixed-count) " of " (itoa (length problems)) ". Re-scanning..."))
           (setq problems (apply scan-fn (list ss)))
           (setq sel-idx 0)
           (if (= (length problems) 0) (princ "\nAll clear!"))
         )
       )
      )
      ;; New Selection
      ((= result 3)
       (setq *CP-LAST-SS* nil)
       (setq *CV-LAST-SS* nil)
       (princ "\nSelect new objects to scan: ")
       (setq new-ss (ssget))
       (if new-ss
         (progn
           (setq *CP-LAST-SS* new-ss)
           (setq *CV-LAST-SS* new-ss)
           (setq ss new-ss)
           (princ "\nScanning...")
           (setq problems (apply scan-fn (list ss)))
           (setq sel-idx 0)
           (if (= (length problems) 0)
             (progn (princ "\nNo problems found!") (setq problems nil))
           )
         )
         (progn (princ "\nNothing selected.") (setq problems nil))
       )
      )
      ;; Closed (result 0) or any unrecognised result - exit loop
      (T
       (sssetfirst nil nil)
       (setq problems nil)
      )
    )
  )
)

;;; ── Commands ─────────────────────────────────────────────────────────────────

(defun C:CHECKPIPES ( / ss)
  (if *CP-LAST-SS*
    (setq ss *CP-LAST-SS*)
    (progn
      (princ "\nCHECKPIPES - Select objects to scan: ")
      (setq ss (ssget))
      (setq *CP-LAST-SS* ss)
    )
  )
  (if ss
    (run-check-loop "CHECKPIPES" ss 'scan-pipes)
    (princ "\nNothing selected.")
  )
  (princ)
)

(defun C:CHECKVALVES ( / ss)
  (if *CV-LAST-SS*
    (setq ss *CV-LAST-SS*)
    (progn
      (princ "\nCHECKVALVES - Select objects to scan: ")
      (setq ss (ssget))
      (setq *CV-LAST-SS* ss)
    )
  )
  (if ss
    (run-check-loop "CHECKVALVES" ss 'scan-valves)
    (princ "\nNothing selected.")
  )
  (princ)
)

;;; Pick a correctly drawn pipe to use as the XDATA template for Fix Pipe.
;;; When a template is set, Fix Pipe copies from it instead of walking the
;;; connection graph.  Run again to change template; no argument to clear it.
(defun C:SETFIXTEMPLATE ( / ent xd strings cat)
  (setq ent (car (entsel "\nSETFIXTEMPLATE - Pick template pipe (Enter to clear): ")))
  (if ent
    (progn
      (setq xd (get-lafx-xd ent))
      (if xd
        (progn
          (setq strings (xd-strings xd))
          (setq cat (if (>= (length strings) 9) (nth 8 strings) "?"))
          (setq *FIX-TEMPLATE* ent)
          (princ (strcat "\nTemplate set: "
                         (if (>= (length strings) 2) (nth 1 strings) "?")
                         " | " (if (>= (length strings) 3) (nth 2 strings) "?")
                         " | " cat))
        )
        (progn
          (princ "\nSelected entity has no LandFX XDATA - not a valid pipe.")
          (setq *FIX-TEMPLATE* nil)
        )
      )
    )
    (progn
      (setq *FIX-TEMPLATE* nil)
      (princ "\nFix template cleared - Fix Pipe will use connection graph.")
    )
  )
  (princ)
)

(princ "\nCHECKPIPES + CHECKVALVES loaded.")
(princ "\n  SETFIXTEMPLATE - pick a pipe as XDATA template for Fix Pipe")
(princ "\n  CHECKPIPES     - find and fix mismatched pipe type/category data")
(princ "\n  CHECKVALVES    - find lateral SOVs with bad pipe connections")
(princ)
