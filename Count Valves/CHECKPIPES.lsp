;;; CHECKPIPES.lsp
;;; Dialog-based diagnostic for mismatched pipe XDATA and bad valve connections.
;;; Uses two external DCL files: checkpipes.dcl and checkvalves.dcl.
;;; All dialogs open immediately; use "Select Geometry" button to pick objects.

;;; ── Load-time path setup ─────────────────────────────────────────────────────
;;; NOTE: The Count Valves folder must be on AutoCAD's Support File Search Path
;;; (Options > Files > Support File Search Path) so findfile can locate these files.

(setq *CP-DIR*
  (cond
    ((findfile "CHECKPIPES.lsp")  (vl-filename-directory (findfile "CHECKPIPES.lsp")))
    ((findfile "checkpipes.dcl")  (vl-filename-directory (findfile "checkpipes.dcl")))
    (T nil)
  )
)
(if (null *CP-DIR*)
  (princ "\nWarning: CHECKPIPES folder not found on support path - dialogs may fail.")
)

;;; ── XDATA Helpers ────────────────────────────────────────────────────────────

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
  (setq xdall  (entget ename '("LandFX")))
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

;;; ── Navigation ───────────────────────────────────────────────────────────────

;;; Zoom to entity and highlight via selection set.
;;; REDRAW before sssetfirst is required after any entmod/entupd to flush
;;; AutoCAD's display state, otherwise the highlight never renders.
(defun zoom-to-ent (ent / entdata pt hs scrsize vh vw twist cx cy)
  (setq entdata (entget ent))
  (setq pt      (cdr (assoc 10 entdata)))
  (if pt
    (progn
      ;; Compute view width from viewport aspect ratio, then shift center
      ;; left by 1/4 view width so the object lands center-right of screen,
      ;; clear of the dialog which sits in the center.
      (setq scrsize (getvar "SCREENSIZE"))
      (setq vh      (getvar "VIEWSIZE"))
      (setq vw      (* vh (/ (float (car scrsize)) (cadr scrsize))))
      (setq twist   (getvar "VIEWTWIST"))
      (setq cx (- (car  pt) (* (/ vw 4.0) (cos twist))))
      (setq cy (- (cadr pt) (* (/ vw 4.0) (sin twist))))
      (command "_.ZOOM" "_C" (list cx cy) vh)
    )
    (command "_.ZOOM" "_O" ent "")
  )
  (command "_.REDRAW")
  (setq hs (ssadd ent (ssadd)))
  (sssetfirst hs hs)
)

;;; ── Formatting Helpers ───────────────────────────────────────────────────────

;;; Right-pad string s to exactly width chars. Truncates if longer.
(defun str-pad (s width / len)
  (setq len (strlen s))
  (cond
    ((= len width) s)
    ((> len width) (substr s 1 width))
    (T (strcat s (substr "                                        " 1 (- width len))))
  )
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
                    (cdr (assoc 5 entdata)) "  |  "
                    size "  |  "
                    type-desc "  |  "
                    category
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
                    (cdr (assoc 5 entdata)) "  |  "
                    bname "  |  "
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
              ;; Pipe fitting - follow its handles to find the mainline
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

;;; Copy mainline XDATA onto bad pipe (via connection graph walk).
;;; Preserves bad pipe's Dp:/Df: (hydraulics) and 1005 handles.
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
          ;; Copy entity color from source
          (setq src-color (assoc 62 (entget src-ent)))
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
          (princ (strcat "\n  Fixed - copied mainline XDATA from handle:"
                         (cdr (assoc 5 (entget src-ent)))))
          T
        )
      )
    )
  )
)

;;; Copy XDATA from the stored template pipe (*FIX-TEMPLATE*) onto bad-ent.
;;; Copies everything from template except handles and Dp:/Df:,
;;; then appends bad pipe's Dp:/Df: and its own handles.
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
          ;; Copy everything from template except handles, Dp:, and Df:.
          ;; Ds: IS copied - it defines the correct design size for this pipe type.
          (setq new-pairs '())
          (foreach pair (cdr src-xd)
            (if (and (not (= (car pair) 1005))
                     (not (and (= (car pair) 1000)
                               (or (= (substr (cdr pair) 1 3) "Dp:")
                                   (= (substr (cdr pair) 1 3) "Df:")))))
              (setq new-pairs (append new-pairs (list pair)))
            )
          )
          ;; Preserve Dp:/Df: from bad pipe if present
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

;;; ── Dialog Helpers ───────────────────────────────────────────────────────────

;;; Returns a human-readable color name for an ACI color number.
(defun cp-color-name (n)
  (cond
    ((= n 1)   "Red")
    ((= n 2)   "Yellow")
    ((= n 3)   "Green")
    ((= n 4)   "Cyan")
    ((= n 5)   "Blue")
    ((= n 6)   "Magenta")
    ((= n 7)   "White")
    ((= n 0)   "By Block")
    ((= n 256) "By Layer")
    (T (strcat "Color " (itoa (fix n))))
  )
)

;;; Line 1 of source pipe display: type name and size.
(defun cp-tpl-display-str ( / xd strings)
  (if (null *FIX-TEMPLATE*)
    "(none set)"
    (progn
      (setq xd (get-lafx-xd *FIX-TEMPLATE*))
      (if (null xd)
        "(invalid - pick again)"
        (progn
          (setq strings (xd-strings xd))
          (strcat
            (if (>= (length strings) 2) (nth 1 strings) "?")
            "  "
            (if (>= (length strings) 3) (nth 2 strings) "?")
          )
        )
      )
    )
  )
)

;;; Returns the actual ACI color to fill the swatch (resolves ByLayer via layer table).
(defun cp-tpl-swatch-color ( / tpl-ed color-raw aci layer-ent)
  (if (null *FIX-TEMPLATE*)
    8  ; gray - nothing selected
    (progn
      (setq tpl-ed    (entget *FIX-TEMPLATE*))
      (setq color-raw (assoc 62 tpl-ed))
      (setq aci       (if color-raw (cdr color-raw) 256))
      (if (or (= aci 256) (= aci 0))
        ;; ByLayer / ByBlock - resolve to the actual layer color
        (progn
          (setq layer-ent (tblobjname "LAYER" (cdr (assoc 8 tpl-ed))))
          (if layer-ent
            (abs (cdr (assoc 62 (entget layer-ent))))
            7  ; fallback white if layer not found
          )
        )
        aci  ; explicit ACI color - use directly
      )
    )
  )
)

;;; Line 2 of source pipe display: layer and color.
(defun cp-tpl-layer-str ( / tpl-ed layer color-raw color-str)
  (if (null *FIX-TEMPLATE*)
    ""
    (progn
      (setq tpl-ed    (entget *FIX-TEMPLATE*))
      (setq layer     (cdr (assoc 8  tpl-ed)))
      (setq color-raw (assoc 62 tpl-ed))
      (setq color-str (if color-raw (cp-color-name (cdr color-raw)) "By Layer"))
      (strcat
        "Layer: " (if layer layer "?")
        "  |  "
        color-str
      )
    )
  )
)

;;; Returns the detail string for the problem at idx in *DLG-PROBLEMS*.
(defun cp-detail-str (idx)
  (if (and *DLG-PROBLEMS* (>= idx 0) (< idx (length *DLG-PROBLEMS*)))
    (cadr (nth idx *DLG-PROBLEMS*))
    ""
  )
)

;;; ── Pipes Dialog ─────────────────────────────────────────────────────────────

(defun run-pipes-dialog (ss problems sel-idx / dcl-path dcl-id result has-probs has-tpl)
  (setq dcl-path
    (cond
      ((and *CP-DIR* (findfile (strcat *CP-DIR* "\\" "checkpipes.dcl")))
       (strcat *CP-DIR* "\\" "checkpipes.dcl"))
      ((findfile "checkpipes.dcl"))
      (T nil)
    )
  )
  (if (null dcl-path)
    (progn
      (alert (strcat "Cannot find checkpipes.dcl\n\n"
                     "Add the CHECKPIPES folder to:\n"
                     "Options > Files > Support File Search Path"))
      0
    )
    (progn
      (setq dcl-id (load_dialog dcl-path))
      (if (not (new_dialog "checkpipes_dialog" dcl-id))
        (progn (unload_dialog dcl-id) (princ "\nError: could not open checkpipes dialog.") 0)
        (progn
          (setq has-probs (> (length problems) 0))
          (setq has-tpl   (not (null *FIX-TEMPLATE*)))
          ;; Status row
          (set_tile "sel_count"  (if ss (strcat (itoa (sslength ss)) " objects") "--"))
          (set_tile "prob_count" (strcat "Problems: " (itoa (length problems))))
          ;; Source pipe row
          (set_tile "tpl_value" (cp-tpl-display-str))
          (set_tile "tpl_layer" (cp-tpl-layer-str))
          (start_image "tpl_swatch")
          (fill_image 0 0 (dimx_tile "tpl_swatch") (dimy_tile "tpl_swatch") (cp-tpl-swatch-color))
          (end_image)
          ;; Problem list
          (setq *DLG-PROBLEMS* problems)
          (start_list "problem_list")
          (cond
            ((null ss)      (add_list "(no selection - click Select Geometry)"))
            ((not has-probs)(add_list "(no problems found)"))
            (T              (mapcar 'add_list (mapcar 'cadr problems)))
          )
          (end_list)
          ;; Initial selection and detail line
          (if (and has-probs (>= sel-idx 0) (< sel-idx (length problems)))
            (progn
              (set_tile "problem_list" (itoa sel-idx))
              (set_tile "detail_line"  (cp-detail-str sel-idx))
              (setq *DLG-SEL-IDX* sel-idx)
            )
            (progn
              (set_tile "detail_line" "")
              (setq *DLG-SEL-IDX* -1)
            )
          )
          ;; Enable/disable action buttons
          (mode_tile "zoom_btn"    (if has-probs 0 1))
          (mode_tile "fix_btn"     (if has-probs 0 1))
          (mode_tile "fix_tpl_btn" (if (and has-probs has-tpl) 0 1))
          (mode_tile "fix_all_btn" (if (and has-probs has-tpl) 0 1))
          ;; Action tiles - list selection updates detail line live
          (action_tile "problem_list"
            (strcat
              "(setq *DLG-SEL-IDX* (atoi $value))"
              "(set_tile \"detail_line\" (cp-detail-str *DLG-SEL-IDX*))"
              "(if (= $reason 4) (done_dialog 2))"
            )
          )
          (action_tile "zoom_btn"    "(setq *DLG-SEL-IDX* (atoi (get_tile \"problem_list\"))) (done_dialog 2)")
          (action_tile "fix_btn"     "(setq *DLG-SEL-IDX* (atoi (get_tile \"problem_list\"))) (done_dialog 4)")
          (action_tile "fix_tpl_btn" "(setq *DLG-SEL-IDX* (atoi (get_tile \"problem_list\"))) (done_dialog 5)")
          (action_tile "fix_all_btn" "(done_dialog 6)")
          (action_tile "tpl_btn"   "(done_dialog 7)")
          (action_tile "sel_btn"   "(done_dialog 3)")
          (action_tile "close_btn" "(done_dialog 0)")
          (setq result (start_dialog))
          (unload_dialog dcl-id)
          result
        )
      )
    )
  )
)

;;; ── Valves Dialog ────────────────────────────────────────────────────────────

(defun run-valves-dialog (ss problems sel-idx / dcl-path dcl-id result has-probs)
  (setq dcl-path
    (cond
      ((and *CP-DIR* (findfile (strcat *CP-DIR* "\\" "checkvalves.dcl")))
       (strcat *CP-DIR* "\\" "checkvalves.dcl"))
      ((findfile "checkvalves.dcl"))
      (T nil)
    )
  )
  (if (null dcl-path)
    (progn
      (alert (strcat "Cannot find checkvalves.dcl\n\n"
                     "Add the CHECKPIPES folder to:\n"
                     "Options > Files > Support File Search Path"))
      0
    )
    (progn
      (setq dcl-id (load_dialog dcl-path))
      (if (not (new_dialog "checkvalves_dialog" dcl-id))
        (progn (unload_dialog dcl-id) (princ "\nError: could not open checkvalves dialog.") 0)
        (progn
          (setq has-probs (> (length problems) 0))
          ;; Status row
          (set_tile "sel_count"  (if ss (strcat (itoa (sslength ss)) " objects") "--"))
          (set_tile "prob_count" (strcat "Problems: " (itoa (length problems))))
          ;; Problem list
          (setq *DLG-PROBLEMS* problems)
          (start_list "problem_list")
          (cond
            ((null ss)      (add_list "(no selection - click Select Geometry)"))
            ((not has-probs)(add_list "(no problems found)"))
            (T              (mapcar 'add_list (mapcar 'cadr problems)))
          )
          (end_list)
          ;; Initial selection and detail line
          (if (and has-probs (>= sel-idx 0) (< sel-idx (length problems)))
            (progn
              (set_tile "problem_list" (itoa sel-idx))
              (set_tile "detail_line"  (cp-detail-str sel-idx))
              (setq *DLG-SEL-IDX* sel-idx)
            )
            (progn
              (set_tile "detail_line" "")
              (setq *DLG-SEL-IDX* -1)
            )
          )
          ;; Enable/disable zoom button
          (mode_tile "zoom_btn" (if has-probs 0 1))
          ;; Action tiles
          (action_tile "problem_list"
            (strcat
              "(setq *DLG-SEL-IDX* (atoi $value))"
              "(set_tile \"detail_line\" (cp-detail-str *DLG-SEL-IDX*))"
              "(if (= $reason 4) (done_dialog 2))"
            )
          )
          (action_tile "zoom_btn"  "(setq *DLG-SEL-IDX* (atoi (get_tile \"problem_list\"))) (done_dialog 2)")
          (action_tile "sel_btn"   "(done_dialog 3)")
          (action_tile "close_btn" "(done_dialog 0)")
          (setq result (start_dialog))
          (unload_dialog dcl-id)
          result
        )
      )
    )
  )
)

;;; ── Pipes Main Loop ──────────────────────────────────────────────────────────

(defun run-pipes-open ( / ss problems sel-idx result sel-ent new-ss fixed-count done
                          ent xd strings)
  ;; Start with last selection if available
  (setq ss       *CP-LAST-SS*)
  (setq problems (if ss (progn (princ "\nScanning...") (scan-pipes ss)) '()))
  (setq sel-idx  0)
  (setq done     nil)
  (while (not done)
    (setq result (run-pipes-dialog ss problems sel-idx))
    (cond
      ;; ── Close ───────────────────────────────────────────────────────────────
      ((= result 0)
       (sssetfirst nil nil)
       (setq done T)
      )
      ;; ── Zoom To ─────────────────────────────────────────────────────────────
      ((= result 2)
       (if (and (>= *DLG-SEL-IDX* 0) (< *DLG-SEL-IDX* (length problems)))
         (progn
           (zoom-to-ent (caddr (nth *DLG-SEL-IDX* problems)))
           (setq sel-idx *DLG-SEL-IDX*)
         )
         (princ "\nNo item selected.")
       )
      )
      ;; ── Select Geometry ─────────────────────────────────────────────────────
      ((= result 3)
       (princ "\nSelect objects to scan: ")
       (setq new-ss (ssget))
       (if new-ss
         (progn
           (setq ss          new-ss)
           (setq *CP-LAST-SS* ss)
           (princ "\nScanning...")
           (setq problems (scan-pipes ss))
           (setq sel-idx  0)
           (if (= (length problems) 0) (princ "\nNo problems found."))
         )
         (princ "\nNothing selected.")
       )
      )
      ;; ── Fix Pipe (connection graph) ──────────────────────────────────────────
      ((= result 4)
       (if (and (>= *DLG-SEL-IDX* 0) (< *DLG-SEL-IDX* (length problems)))
         (progn
           (setq sel-ent (caddr (nth *DLG-SEL-IDX* problems)))
           (princ (strcat "\nFixing pipe " (car (nth *DLG-SEL-IDX* problems)) "..."))
           (if (fix-pipe-xdata sel-ent)
             (progn
               (sssetfirst nil nil)
               (command "_.REDRAW")
               (princ "\nRe-scanning...")
               (setq problems (scan-pipes ss))
               (setq sel-idx (min *DLG-SEL-IDX* (max 0 (1- (length problems)))))
               (if (= (length problems) 0) (princ "\nAll clear!"))
             )
           )
         )
         (princ "\nNo item selected.")
       )
      )
      ;; ── Fix LAT→MAIN (template) ──────────────────────────────────────────────
      ((= result 5)
       (if (and (>= *DLG-SEL-IDX* 0) (< *DLG-SEL-IDX* (length problems)))
         (progn
           (setq sel-ent (caddr (nth *DLG-SEL-IDX* problems)))
           (princ (strcat "\nApplying template to pipe " (car (nth *DLG-SEL-IDX* problems)) "..."))
           ;; Unhighlight before fixing so the visual change is visible
           (sssetfirst nil nil)
           (if (fix-from-template sel-ent)
             (progn
               (command "_.REDRAW")
               (princ "\nRe-scanning...")
               (setq problems (scan-pipes ss))
               (setq sel-idx (min *DLG-SEL-IDX* (max 0 (1- (length problems)))))
               (if (= (length problems) 0) (princ "\nAll clear!"))
             )
           )
         )
         (princ "\nNo item selected.")
       )
      )
      ;; ── Fix All (template) ───────────────────────────────────────────────────
      ((= result 6)
       (if (null *FIX-TEMPLATE*)
         (princ "\nNo template set.")
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
           (setq problems (scan-pipes ss))
           (setq sel-idx 0)
           (if (= (length problems) 0) (princ "\nAll clear!"))
         )
       )
      )
      ;; ── Set Template (in-dialog shortcut) ────────────────────────────────────
      ((= result 7)
       (setq ent (car (entsel "\nPick template pipe: ")))
       (if ent
         (progn
           (setq xd (get-lafx-xd ent))
           (if xd
             (progn
               (setq strings (xd-strings xd))
               (setq *FIX-TEMPLATE* ent)
               (princ (strcat "\nTemplate set: "
                              (if (>= (length strings) 2) (nth 1 strings) "?")
                              " | "
                              (if (>= (length strings) 3) (nth 2 strings) "?")
                              " | "
                              (if (>= (length strings) 9) (nth 8 strings) "?")))
             )
             (progn
               (setq *FIX-TEMPLATE* nil)
               (princ "\nNot a valid LandFX pipe - template cleared.")
             )
           )
         )
         (princ "\nCancelled - template unchanged.")
       )
      )
    ) ; end cond
  ) ; end while
)

;;; ── Valves Main Loop ─────────────────────────────────────────────────────────

(defun run-valves-open ( / ss problems sel-idx result sel-ent new-ss done)
  ;; Start with last selection if available
  (setq ss       *CV-LAST-SS*)
  (setq problems (if ss (progn (princ "\nScanning...") (scan-valves ss)) '()))
  (setq sel-idx  0)
  (setq done     nil)
  (while (not done)
    (setq result (run-valves-dialog ss problems sel-idx))
    (cond
      ;; ── Close ───────────────────────────────────────────────────────────────
      ((= result 0)
       (sssetfirst nil nil)
       (setq done T)
      )
      ;; ── Zoom To ─────────────────────────────────────────────────────────────
      ((= result 2)
       (if (and (>= *DLG-SEL-IDX* 0) (< *DLG-SEL-IDX* (length problems)))
         (progn
           (zoom-to-ent (caddr (nth *DLG-SEL-IDX* problems)))
           (setq sel-idx *DLG-SEL-IDX*)
         )
         (princ "\nNo item selected.")
       )
      )
      ;; ── Select Geometry ─────────────────────────────────────────────────────
      ((= result 3)
       (princ "\nSelect objects to scan: ")
       (setq new-ss (ssget))
       (if new-ss
         (progn
           (setq ss          new-ss)
           (setq *CV-LAST-SS* ss)
           (princ "\nScanning...")
           (setq problems (scan-valves ss))
           (setq sel-idx  0)
           (if (= (length problems) 0) (princ "\nNo problems found."))
         )
         (princ "\nNothing selected.")
       )
      )
    ) ; end cond
  ) ; end while
)

;;; ── Commands ─────────────────────────────────────────────────────────────────

(defun C:CHECKPIPES ()
  (run-pipes-open)
  (princ)
)

(defun C:CHECKVALVES ()
  (run-valves-open)
  (princ)
)

;;; Pick a correctly drawn pipe to use as the XDATA template for Fix Pipe.
;;; Also available as the "Set Template" button inside the CHECKPIPES dialog.
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
      (princ "\nFix template cleared.")
    )
  )
  (princ)
)

(princ "\nCHECKPIPES + CHECKVALVES loaded.")
(princ "\n  CHECKPIPES     - find and fix mismatched pipe type/category data")
(princ "\n  CHECKVALVES    - find lateral SOVs with bad pipe connections")
(princ "\n  SETFIXTEMPLATE - pick a pipe as XDATA template (also in CHECKPIPES dialog)")
(princ)
