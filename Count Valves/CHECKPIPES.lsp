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
                       results desc detail cv-lat-str cv-main-str)
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
                ;; Build richer detail: show actual connected pipe sizes and layer
                (setq cv-lat-str "")
                (foreach p lat-pipes
                  (setq cv-lat-str
                    (strcat cv-lat-str (if (= cv-lat-str "") "" ", ") (car p))))
                (setq cv-main-str "")
                (foreach p main-pipes
                  (setq cv-main-str
                    (strcat cv-main-str (if (= cv-main-str "") "" ", ") (car p))))
                (setq detail
                  (strcat
                    "Laterals: " (if (= cv-lat-str "") "none" cv-lat-str)
                    "  Mains: "  (if (= cv-main-str "") "none" cv-main-str)
                    (if (> (length other-pipes) 0)
                      (strcat "  +" (itoa (length other-pipes)) " dead handle(s)")
                      ""
                    )
                    "  |  Layer: " (cdr (assoc 8 entdata))
                  )
                )
                (setq results (append results (list (list (cdr (assoc 5 entdata)) desc ename detail))))
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

;;; Scan for lateral SOVs whose lateral and mainline pipe sizes don't match.
;;; Only checks valves that already have exactly 1 lateral + 1 main connected
;;; (i.e. pass the connection check) — size mismatch is a separate quality issue.
(defun scan-valve-sizes (ss / i ename entdata xd strings bname vtype
                             handles pipeent pinfo cat
                             lat-pipes main-pipes
                             lat-size main-size
                             results desc detail)
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
            (setq handles    (get-handles xd))
            (setq lat-pipes  '())
            (setq main-pipes '())
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
                         (setq lat-pipes  (cons pinfo lat-pipes)))
                        ((vl-string-search "pipe-main" cat)
                         (setq main-pipes (cons pinfo main-pipes)))
                      )
                    )
                  )
                )
              )
            )
            ;; Only check valves with exactly 1 lateral + 1 main (good connections)
            (if (and (= (length lat-pipes) 1) (= (length main-pipes) 1))
              (progn
                (setq lat-size  (car (car lat-pipes)))
                (setq main-size (car (car main-pipes)))
                (if (not (equal lat-size main-size))
                  (progn
                    (setq desc
                      (strcat
                        (cdr (assoc 5 entdata)) "  |  "
                        bname "  |  "
                        "lat:" lat-size "  main:" main-size
                      )
                    )
                    (setq detail
                      (strcat
                        "Layer: " (cdr (assoc 8 entdata))
                        "  |  Lateral: " lat-size
                        "  Main: " main-size
                        "  ->  main pipe needs to be updated to match lateral size"
                      )
                    )
                    (setq results (append results (list (list (cdr (assoc 5 entdata)) desc ename lat-size detail))))
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
  results
)

;;; ── Feeder Traversal ─────────────────────────────────────────────────────────

;;; Convert a size string like "10\"" or "3/4\"" to a float for comparison.
(defun cv-pipe-size-num (s)
  (atof (vl-string-subst "" "\"" (if s s "0")))
)

;;; Walk upstream from a branch main through PIPEFITTING inserts to find the
;;; trunk (feeder) mainline.  Detection rule: fitting connected to 2+ other
;;; mainlines = trunk junction; 1 other = intermediate, follow it.  Limit 3 hops.
;;; Returns the trunk main entity, or nil if not found within 3 hops.
(defun cv-find-feeder-mainline (branch-main-ent
                                / current-main hop
                                  bm-xd bm-handles h conn-ent conn-ed
                                  fit-xdata fit-handles fh fit-pipe fit-info
                                  other-mains best-ent best-num this-num
                                  found-trunk next-hop)
  (setq current-main branch-main-ent)
  (setq hop          0)
  (setq found-trunk  nil)
  (while (and (< hop 3) (not found-trunk))
    (setq bm-xd      (get-lafx-xd current-main))
    (setq bm-handles (if bm-xd (get-handles bm-xd) '()))
    (setq next-hop   nil)
    (foreach h bm-handles
      (if (not found-trunk)
        (progn
          (setq conn-ent (handent h))
          (if (and conn-ent (not (equal conn-ent current-main)))
            (progn
              (setq conn-ed (entget conn-ent))
              (if (and (= (cdr (assoc 0 conn-ed)) "INSERT")
                       (vl-string-search "PIPEFITTING" (cdr (assoc 2 conn-ed))))
                (progn
                  (setq fit-xdata
                    (cdr (cadr (assoc -3 (entget conn-ent '("LandFX"))))))
                  (setq fit-handles '())
                  (if fit-xdata
                    (foreach pair fit-xdata
                      (if (= (car pair) 1005)
                        (setq fit-handles (append fit-handles (list (cdr pair)))))))
                  (setq other-mains '())
                  (foreach fh fit-handles
                    (setq fit-pipe (handent fh))
                    (if (and fit-pipe (not (equal fit-pipe current-main)))
                      (progn
                        (setq fit-info (get-pipe-info fit-pipe))
                        (if (and fit-info (vl-string-search "pipe-main" (cadr fit-info)))
                          (setq other-mains (append other-mains (list fit-pipe)))))))
                  (cond
                    ((>= (length other-mains) 2)
                     (setq best-ent nil)
                     (setq best-num 0.0)
                     (foreach m other-mains
                       (setq this-num (cv-pipe-size-num (car (get-pipe-info m))))
                       (if (> this-num best-num)
                         (progn (setq best-ent m) (setq best-num this-num))))
                     (setq found-trunk best-ent))
                    ((= (length other-mains) 1)
                     (if (not next-hop) (setq next-hop (car other-mains))))
                  )
                )
              )
            )
          )
        )
      )
    )
    (if (and (not found-trunk) next-hop)
      (progn (setq current-main next-hop) (setq hop (1+ hop)))
      (setq hop 3)
    )
  )
  found-trunk
)

;;; Scan for lateral SOVs whose branch main does not connect to a feeder/trunk.
;;; Only checks valves with exactly 1 lateral + 1 main (connection check passes).
;;; Returns a list of (handle desc ename detail) for each unpiped valve.
(defun scan-valve-feeders (ss / i ename entdata xd strings bname vtype
                               handles pipeent pinfo cat
                               lat-pipes main-pipes
                               lat-size main-size branch-main-ent feeder-ent
                               results desc detail)
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
            (setq handles    (get-handles xd))
            (setq lat-pipes  '())
            (setq main-pipes '())
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
                         (setq lat-pipes  (cons pinfo lat-pipes)))
                        ((vl-string-search "pipe-main" cat)
                         (setq main-pipes (cons pinfo main-pipes)))
                      )
                    )
                  )
                )
              )
            )
            ;; Only check valves with exactly 1 lateral + 1 main
            (if (and (= (length lat-pipes) 1) (= (length main-pipes) 1))
              (progn
                (setq lat-size  (car (car lat-pipes)))
                (setq main-size (car (car main-pipes)))
                ;; Find branch main entity for traversal
                (setq branch-main-ent nil)
                (foreach h handles
                  (if (not branch-main-ent)
                    (progn
                      (setq pipeent (handent h))
                      (if pipeent
                        (progn
                          (setq pinfo (get-pipe-info pipeent))
                          (if (and pinfo (vl-string-search "pipe-main" (cadr pinfo)))
                            (setq branch-main-ent pipeent)))))))
                ;; Walk fittings; flag valve if feeder not found
                (setq feeder-ent (if branch-main-ent (cv-find-feeder-mainline branch-main-ent) nil))
                (if (null feeder-ent)
                  (progn
                    (setq desc
                      (strcat
                        (cdr (assoc 5 entdata)) "  |  "
                        bname "  |  "
                        "lat:" lat-size "  main:" main-size "  no feeder"
                      )
                    )
                    (setq detail
                      (strcat
                        "Layer: " (cdr (assoc 8 entdata))
                        "  |  Lateral: " lat-size
                        "  |  Main: " main-size
                        "  ->  branch main does not connect to a trunk within 3 hops"
                      )
                    )
                    (setq results (append results (list (list (cdr (assoc 5 entdata)) desc ename detail))))
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

;;; Find the mainline pipe connected to a lateral SOV via its 1005 handles.
(defun find-valve-mainline (valve-ent / valve-xd handles h conn-ent conn-info main-ent)
  (setq valve-xd (get-lafx-xd valve-ent))
  (setq handles  (get-handles valve-xd))
  (setq main-ent nil)
  (foreach h handles
    (if (not main-ent)
      (progn
        (setq conn-ent (handent h))
        (if conn-ent
          (progn
            (setq conn-info (get-pipe-info conn-ent))
            (if (and conn-info (vl-string-search "pipe-main" (cadr conn-info)))
              (setq main-ent conn-ent)
            )
          )
        )
      )
    )
  )
  main-ent
)

;;; Remove dead 1005 handles from valve-ent's LandFX XDATA.
;;; Dead handles (references to entities that no longer exist) cause false
;;; lat/main counts in the connection check.  Safe to call on any valve.
(defun fix-valve-dead-handles (valve-ent / valve-ed valve-xd new-pairs pair h removed)
  (setq valve-xd (get-lafx-xd valve-ent))
  (if (null valve-xd)
    (progn (princ "\n  No LandFX XDATA on valve - skipping.") nil)
    (progn
      (setq new-pairs '())
      (setq removed   0)
      (foreach pair (cdr valve-xd)
        (if (= (car pair) 1005)
          (progn
            (setq h (handent (cdr pair)))
            (if h
              (setq new-pairs (append new-pairs (list pair)))   ; valid - keep
              (setq removed   (1+ removed))                     ; dead - drop
            )
          )
          (setq new-pairs (append new-pairs (list pair)))       ; not a handle - keep
        )
      )
      (if (= removed 0)
        (progn (princ "\n  No dead handles found.") nil)
        (progn
          (setq valve-ed (entget valve-ent '("LandFX")))
          (setq valve-ed (vl-remove (assoc -3 valve-ed) valve-ed))
          (setq valve-ed (append valve-ed
                           (list (cons -3 (list (cons "LandFX" new-pairs))))))
          (entmod valve-ed)
          (entupd valve-ent)
          (princ (strcat "\n  Removed " (itoa removed) " dead handle(s)."))
          T
        )
      )
    )
  )
)

;;; Fix the mainline pipe connected to valve-ent using the provided template.
;;; Temporarily swaps *FIX-TEMPLATE* so fix-from-template does the heavy lifting.
(defun fix-valve-main-with-tpl (valve-ent tpl-ent / main-ent saved-tpl result)
  (setq main-ent (find-valve-mainline valve-ent))
  (if (null main-ent)
    (progn (princ "\n  No connected mainline found - skipping.") nil)
    (progn
      (setq saved-tpl    *FIX-TEMPLATE*)
      (setq *FIX-TEMPLATE* tpl-ent)
      (setq result (fix-from-template main-ent))
      (setq *FIX-TEMPLATE* saved-tpl)
      result
    )
  )
)

;;; Returns list of distinct lateral sizes from a scan-valve-sizes result list,
;;; in the order they first appear. Each entry must have lat-size as nth 3.
(defun cv-distinct-lat-sizes (problems / result sz)
  (setq result '())
  (foreach entry problems
    (setq sz (nth 3 entry))
    (if (and sz (not (member sz result)))
      (setq result (append result (list sz)))
    )
  )
  result
)

;;; Returns only the problem entries whose lateral size matches target-size.
(defun cv-problems-for-lat-size (problems target-size / result)
  (setq result '())
  (foreach entry problems
    (if (equal (nth 3 entry) target-size)
      (setq result (append result (list entry)))
    )
  )
  result
)

;;; Scan for distinct mainline pipe types found on correctly-piped lateral SOVs.
;;; "Correctly-piped" means exactly 1 lateral + 1 main connected (passes conn check).
;;; Returns a list of (display-str lat-size main-ename) entries, deduplicated by
;;; (main-type + main-size + main-category + lat-size) so the same mainline type
;;; can appear once per lateral size it was found connecting to.
(defun scan-good-valve-mains (ss / i ename entdata xd strings bname vtype
                                   handles h pipeent pinfo cat
                                   lat-pipes main-pipes
                                   lat-size main-ent main-xd main-strings
                                   main-type main-sz main-cat main-disp
                                   dedup-key results seen-keys)
  (setq results   '())
  (setq seen-keys '())
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
            (setq handles    (get-handles xd))
            (setq lat-pipes  '())
            (setq main-pipes '())
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
                         (setq lat-pipes  (cons pinfo lat-pipes)))
                        ((vl-string-search "pipe-main" cat)
                         ;; Store (size pipeent) so we have the entity for the template
                         (setq main-pipes (cons (list (car pinfo) pipeent) main-pipes)))
                      )
                    )
                  )
                )
              )
            )
            ;; Only harvest from valves with exactly 1 lateral + 1 main
            (if (and (= (length lat-pipes) 1) (= (length main-pipes) 1))
              (progn
                (setq lat-size (car  (car lat-pipes)))   ; lat-pipes entry is pinfo=(size cat)
                (setq main-ent (cadr (car main-pipes)))  ; main-pipes entry is (size pipeent)
                (setq main-xd  (get-lafx-xd main-ent))
                (if main-xd
                  (progn
                    (setq main-strings (xd-strings main-xd))
                    (setq main-type (if (>= (length main-strings) 2) (nth 1 main-strings) "?"))
                    (setq main-sz   (if (>= (length main-strings) 3) (nth 2 main-strings) "?"))
                    (setq main-cat  (if (>= (length main-strings) 9) (nth 8 main-strings) "?"))
                    (setq main-disp (if (>= (length main-strings) 6) (nth 5 main-strings) ""))
                    ;; Dedup key includes lat-size so the same mainline type can appear
                    ;; separately for each lateral size it was found connecting to
                    (setq dedup-key (strcat main-type "|" main-sz "|" main-cat "|" lat-size))
                    (if (not (member dedup-key seen-keys))
                      (progn
                        (setq seen-keys (append seen-keys (list dedup-key)))
                        (setq results (append results
                          (list (list
                            (strcat main-sz
                                    (if (= main-disp "") "" (strcat "  " main-disp))
                                    "  [" main-type "]"
                                    "  <- " lat-size " lateral")
                            lat-size
                            main-ent
                          ))
                        ))
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
    (setq i (1+ i))
  )
  results
)

;;; Scan the selection set for all distinct mainline pipe types (LWPOLYLINE with
;;; LandFX XDATA where category contains "pipe-main").  Unlike scan-good-valve-mains
;;; this does NOT require a correctly-piped valve — it finds every mainline pipe
;;; present in the selection regardless of valve connections.
;;; Returns a list of (display-str main-ename), deduplicated by type+size+category.
(defun scan-main-types (ss / i ename entdata xd strings type-desc cat size disp
                             dedup-key results seen-keys)
  (setq results   '())
  (setq seen-keys '())
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
            (setq cat (nth 8 strings))
            (if (and cat (vl-string-search "pipe-main" cat))
              (progn
                (setq type-desc (if (>= (length strings) 2) (nth 1 strings) "?"))
                (setq size      (if (>= (length strings) 3) (nth 2 strings) "?"))
                (setq disp      (if (>= (length strings) 6) (nth 5 strings) ""))
                (setq dedup-key (strcat type-desc "|" size "|" cat))
                (if (not (member dedup-key seen-keys))
                  (progn
                    (setq seen-keys (append seen-keys (list dedup-key)))
                    (setq results (append results
                      (list (list
                        (strcat size
                                (if (= disp "") "" (strcat "  " disp))
                                "  [" type-desc "]")
                        ename
                      ))
                    ))
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
  results
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

;;; Detail string for connection problems (valves dialog top section).
;;; Entry structure: (handle list-str ename detail)  — detail at index 3.
(defun cv-conn-detail (idx)
  (if (and *DLG-CONN-PROBS* (>= idx 0) (< idx (length *DLG-CONN-PROBS*)))
    (nth 3 (nth idx *DLG-CONN-PROBS*))
    ""
  )
)

;;; Detail string for size mismatch problems (valves dialog bottom section).
;;; Entry structure: (handle list-str ename lat-size detail)  — detail at index 4.
(defun cv-size-detail (idx)
  (if (and *DLG-SIZE-PROBS* (>= idx 0) (< idx (length *DLG-SIZE-PROBS*)))
    (nth 4 (nth idx *DLG-SIZE-PROBS*))
    ""
  )
)

;;; Detail string for feeder problems (valves dialog feeder section).
;;; Entry structure: (handle list-str ename detail)  -- detail at index 3.
(defun cv-feeder-detail (idx)
  (if (and *DLG-FEEDER-PROBS* (>= idx 0) (< idx (length *DLG-FEEDER-PROBS*)))
    (nth 3 (nth idx *DLG-FEEDER-PROBS*))
    ""
  )
)

;;; Show the template picker dialog populated with scan-main-types results.
;;; context-str is displayed at the top of the dialog to remind the user what
;;; they are fixing (pass "" if not applicable).
;;; Returns the 0-based index of the chosen template, or -1 if cancelled.
;;; Uses *CV-TPL-PICK-IDX* global to track selection inside action_tile strings.
(defun cv-pick-template-dialog (templates context-str / dcl-path dcl-id result)
  (if (null templates)
    -1
    (progn
      (setq dcl-path
        (cond
          ((and *CP-DIR* (findfile (strcat *CP-DIR* "\\" "checkvalves.dcl")))
           (strcat *CP-DIR* "\\" "checkvalves.dcl"))
          ((findfile "checkvalves.dcl"))
          (T nil)
        )
      )
      (if (null dcl-path)
        -1
        (progn
          (setq dcl-id (load_dialog dcl-path))
          (if (not (new_dialog "cv_pick_template" dcl-id))
            (progn (unload_dialog dcl-id) -1)
            (progn
              (set_tile "ctx_line" (if (and context-str (not (= context-str "")))
                                     context-str
                                     " "))
              (setq *CV-TPL-PICK-IDX* 0)
              (start_list "tpl_list")
              (mapcar 'add_list (mapcar 'car templates))
              (add_list "Other -- pick from drawing...")
              (end_list)
              (set_tile "tpl_list" "0")
              ;; Wire OK/Cancel explicitly -- ok_cancel alone is unreliable in Civil 3D 2026.
              ;; *CV-TPL-PICK-IDX* is updated by action_tile so get_tile is never needed.
              (action_tile "accept" "(done_dialog 1)")
              (action_tile "cancel" "(done_dialog 0)")
              (action_tile "tpl_list"
                (strcat
                  "(setq *CV-TPL-PICK-IDX* (atoi $value))"
                  "(if (= $reason 4) (done_dialog 1))"
                )
              )
              (setq result (start_dialog))
              (unload_dialog dcl-id)
              (if (= result 1) *CV-TPL-PICK-IDX* -1)
            )
          )
        )
      )
    )
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
;;; Two-section dialog: top = connection check, bottom = size mismatch check.
;;; conn-probs / size-probs are separate problem lists.
;;; size-scanned: nil = Check Sizes not yet run; T = run (even if empty result).

(defun run-valves-dialog (ss conn-probs size-probs feeder-probs conn-idx size-idx feeder-idx
                          / dcl-path dcl-id result has-conn has-size has-feeder)
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
          (setq has-conn   (> (length conn-probs)   0))
          (setq has-size   (> (length size-probs)   0))
          (setq has-feeder (> (length feeder-probs) 0))
          ;; ── Status row ──────────────────────────────────────────────────────
          (set_tile "sel_count" (if ss (strcat (itoa (sslength ss)) " objects") "--"))
          ;; ── Connection section ───────────────────────────────────────────────
          (set_tile "conn_count"
            (if ss
              (strcat "Problems: " (itoa (length conn-probs)))
              "Problems: --"
            )
          )
          (setq *DLG-CONN-PROBS* conn-probs)
          (start_list "conn_list")
          (cond
            ((null ss)      (add_list "(no selection - click Select Geometry)"))
            ((not has-conn) (add_list "(no connection problems found)"))
            (T              (mapcar 'add_list (mapcar 'cadr conn-probs)))
          )
          (end_list)
          (if (and has-conn (>= conn-idx 0) (< conn-idx (length conn-probs)))
            (progn
              (set_tile "conn_list"   (itoa conn-idx))
              (set_tile "conn_detail" (cv-conn-detail conn-idx))
              (setq *DLG-CONN-IDX* conn-idx)
            )
            (progn
              (set_tile "conn_detail" "")
              (setq *DLG-CONN-IDX* -1)
            )
          )
          ;; ── Size section ─────────────────────────────────────────────────────
          (set_tile "size_count"
            (if ss
              (strcat "Mismatches: " (itoa (length size-probs)))
              "Mismatches: --"
            )
          )
          (setq *DLG-SIZE-PROBS* size-probs)
          (start_list "size_list")
          (cond
            ((null ss)      (add_list "(no selection - click Select Geometry)"))
            ((not has-size) (add_list "(no size mismatches found)"))
            (T              (mapcar 'add_list (mapcar 'cadr size-probs)))
          )
          (end_list)
          (if (and has-size (>= size-idx 0) (< size-idx (length size-probs)))
            (progn
              (set_tile "size_list"   (itoa size-idx))
              (set_tile "size_detail" (cv-size-detail size-idx))
              (setq *DLG-SIZE-IDX* size-idx)
            )
            (progn
              (set_tile "size_detail" "")
              (setq *DLG-SIZE-IDX* -1)
            )
          )
          ;; ── Feeder section ───────────────────────────────────────────────────
          (set_tile "feeder_count"
            (if ss
              (strcat "Unpiped: " (itoa (length feeder-probs)))
              "Unpiped: --"
            )
          )
          (setq *DLG-FEEDER-PROBS* feeder-probs)
          (start_list "feeder_list")
          (cond
            ((null ss)        (add_list "(no selection - click Select Geometry)"))
            ((not has-feeder) (add_list "(no unpiped valves found)"))
            (T                (mapcar 'add_list (mapcar 'cadr feeder-probs)))
          )
          (end_list)
          (if (and has-feeder (>= feeder-idx 0) (< feeder-idx (length feeder-probs)))
            (progn
              (set_tile "feeder_list"   (itoa feeder-idx))
              (set_tile "feeder_detail" (cv-feeder-detail feeder-idx))
              (setq *DLG-FEEDER-IDX* feeder-idx)
            )
            (progn
              (set_tile "feeder_detail" "")
              (setq *DLG-FEEDER-IDX* -1)
            )
          )
          ;; ── Button enable/disable ────────────────────────────────────────────
          (mode_tile "zoom_btn"        (if has-conn   0 1))
          (mode_tile "fix_valve_btn"  (if has-conn   0 1))
          (mode_tile "size_zoom_btn"  (if has-size   0 1))
          (mode_tile "fix_main_btn"   (if has-size   0 1))
          (mode_tile "fix_all_btn"    (if has-size   0 1))
          (mode_tile "feeder_zoom_btn" (if has-feeder 0 1))
          ;; ── Action tiles ─────────────────────────────────────────────────────
          (action_tile "conn_list"
            (strcat
              "(setq *DLG-CONN-IDX* (atoi $value))"
              "(set_tile \"conn_detail\" (cv-conn-detail *DLG-CONN-IDX*))"
              "(if (= $reason 4) (done_dialog 2))"
            )
          )
          (action_tile "size_list"
            (strcat
              "(setq *DLG-SIZE-IDX* (atoi $value))"
              "(set_tile \"size_detail\" (cv-size-detail *DLG-SIZE-IDX*))"
              "(if (= $reason 4) (done_dialog 7))"
            )
          )
          (action_tile "feeder_list"
            (strcat
              "(setq *DLG-FEEDER-IDX* (atoi $value))"
              "(set_tile \"feeder_detail\" (cv-feeder-detail *DLG-FEEDER-IDX*))"
              "(if (= $reason 4) (done_dialog 4))"
            )
          )
          (action_tile "zoom_btn"       "(setq *DLG-CONN-IDX*   (atoi (get_tile \"conn_list\")))   (done_dialog 2)")
          (action_tile "fix_valve_btn"  "(setq *DLG-CONN-IDX*   (atoi (get_tile \"conn_list\")))   (done_dialog 8)")
          (action_tile "size_zoom_btn"  "(setq *DLG-SIZE-IDX*   (atoi (get_tile \"size_list\")))   (done_dialog 7)")
          (action_tile "fix_main_btn"   "(setq *DLG-SIZE-IDX*   (atoi (get_tile \"size_list\")))   (done_dialog 5)")
          (action_tile "fix_all_btn"    "(setq *DLG-SIZE-IDX*   (atoi (get_tile \"size_list\")))   (done_dialog 6)")
          (action_tile "feeder_zoom_btn" "(setq *DLG-FEEDER-IDX* (atoi (get_tile \"feeder_list\"))) (done_dialog 4)")
          (action_tile "sel_btn"        "(done_dialog 3)")
          (action_tile "close_btn"      "(done_dialog 0)")
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
;;; Maintains two independent problem lists:
;;;   conn-problems — valve connection check (1 lateral + 1 main)
;;;   size-problems — lateral vs. main pipe size mismatch

(defun run-valves-open ( / ss conn-problems size-problems feeder-problems
                           conn-sel-idx size-sel-idx feeder-sel-idx
                           result sel-ent new-ss fixed-count done
                           cv-sizes cv-target-size cv-group
                           cv-tpl cv-tpl-xd cv-tpl-strings cv-tpl-size cv-lat-size
                           cv-all-templates cv-templates cv-tpl-pick cv-ctx)
  ;; Start with last selection if available; run all three scans together
  (setq ss *CV-LAST-SS*)
  (if ss
    (progn
      (princ "\nScanning...")
      (setq conn-problems   (scan-valves ss))
      (setq size-problems   (scan-valve-sizes ss))
      (setq feeder-problems (scan-valve-feeders ss))
    )
    (progn
      (setq conn-problems   '())
      (setq size-problems   '())
      (setq feeder-problems '())
    )
  )
  (setq conn-sel-idx   0)
  (setq size-sel-idx   0)
  (setq feeder-sel-idx 0)
  (setq done           nil)
  (while (not done)
    (setq result (run-valves-dialog ss conn-problems size-problems feeder-problems
                                    conn-sel-idx size-sel-idx feeder-sel-idx))
    (cond
      ;; ── Close ───────────────────────────────────────────────────────────────
      ((= result 0)
       (sssetfirst nil nil)
       (setq done T)
      )
      ;; ── Find Valve (connection section) ─────────────────────────────────────
      ((= result 2)
       (if (and (>= *DLG-CONN-IDX* 0) (< *DLG-CONN-IDX* (length conn-problems)))
         (progn
           (zoom-to-ent (caddr (nth *DLG-CONN-IDX* conn-problems)))
           (setq conn-sel-idx *DLG-CONN-IDX*)
         )
         (princ "\nNo item selected.")
       )
      )
      ;; ── Find Valve (feeder section) ─────────────────────────────────────────
      ((= result 4)
       (if (and (>= *DLG-FEEDER-IDX* 0) (< *DLG-FEEDER-IDX* (length feeder-problems)))
         (progn
           (zoom-to-ent (caddr (nth *DLG-FEEDER-IDX* feeder-problems)))
           (setq feeder-sel-idx *DLG-FEEDER-IDX*)
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
           (setq ss              new-ss)
           (setq *CV-LAST-SS*    ss)
           (princ "\nScanning...")
           (setq conn-problems   (scan-valves ss))
           (setq size-problems   (scan-valve-sizes ss))
           (setq feeder-problems (scan-valve-feeders ss))
           (setq conn-sel-idx    0)
           (setq size-sel-idx    0)
           (setq feeder-sel-idx  0)
           (if (= (length conn-problems)   0) (princ "\nNo connection problems found."))
           (if (= (length size-problems)   0) (princ "\nNo size mismatches found."))
           (if (= (length feeder-problems) 0) (princ "\nNo unpiped valves found."))
         )
         (princ "\nNothing selected.")
       )
      )
      ;; ── Fix Main (single size problem) ──────────────────────────────────────
      ((= result 5)
       (if (and (>= *DLG-SIZE-IDX* 0) (< *DLG-SIZE-IDX* (length size-problems)))
         (progn
           (setq sel-ent     (caddr (nth *DLG-SIZE-IDX* size-problems)))
           (setq cv-lat-size (nth 3  (nth *DLG-SIZE-IDX* size-problems)))
           ;; Scan selection for all mainline pipe types; fall back to entsel if none
           (setq cv-all-templates (scan-main-types ss))
           (setq cv-ctx (strcat (cadr (nth *DLG-SIZE-IDX* size-problems))
                               "  --  pick a " cv-lat-size " mainline as template"))
           (if cv-all-templates
             (progn
               (setq cv-tpl-pick (cv-pick-template-dialog cv-all-templates cv-ctx))
               (cond
                 ((< cv-tpl-pick 0)
                  (setq cv-tpl nil))  ; cancelled
                 ((= cv-tpl-pick (length cv-all-templates))  ; "Other -- pick from drawing..."
                  (setq cv-tpl
                    (car (entsel (strcat "\nPick mainline template pipe"
                                  (if cv-lat-size (strcat " for " cv-lat-size " lateral") "")
                                  ": ")))))
                 (T
                  (setq cv-tpl (cadr (nth cv-tpl-pick cv-all-templates))))
               )
             )
             (progn
               (princ "\nNo mainline pipes found in selection - pick template manually.")
               (setq cv-tpl
                 (car (entsel (strcat "\nPick mainline template pipe"
                               (if cv-lat-size (strcat " for " cv-lat-size " lateral") "")
                               ": "))))
             )
           )
           (if cv-tpl
             (progn
               (setq cv-tpl-xd (get-lafx-xd cv-tpl))
               (if cv-tpl-xd
                 (progn
                   (setq cv-tpl-strings (xd-strings cv-tpl-xd))
                   (setq cv-tpl-size (if (>= (length cv-tpl-strings) 3) (nth 2 cv-tpl-strings) "?"))
                   (princ (strcat "\nUsing template: "
                                  (if (>= (length cv-tpl-strings) 2) (nth 1 cv-tpl-strings) "?")
                                  " " cv-tpl-size))
                   (sssetfirst nil nil)
                   (if (fix-valve-main-with-tpl sel-ent cv-tpl)
                     (progn
                       (command "_.REDRAW")
                       (princ "\nRe-scanning sizes...")
                       (setq size-problems (scan-valve-sizes ss))
                       (setq size-sel-idx (min *DLG-SIZE-IDX* (max 0 (1- (length size-problems)))))
                       (if (= (length size-problems) 0) (princ "\nAll size issues clear!"))
                     )
                   )
                 )
                 (princ "\nNot a valid LandFX pipe - cancelled.")
               )
             )
             (princ "\nCancelled.")
           )
         )
         (princ "\nNo item selected.")
       )
      )
      ;; ── Fix All Mains (all rows with same lateral size as selected row) ─────────
      ;; Highlight any row in the size list and click Fix All to fix every valve
      ;; whose lateral size matches that row.  Fix Main fixes only the selected valve.
      ((= result 6)
       (if (and (>= *DLG-SIZE-IDX* 0) (< *DLG-SIZE-IDX* (length size-problems)))
         (progn
           (setq cv-target-size (nth 3 (nth *DLG-SIZE-IDX* size-problems)))
           (setq cv-group (cv-problems-for-lat-size size-problems cv-target-size))
           (princ (strcat "\nFixing " (itoa (length cv-group))
                          " valve(s) with " cv-target-size " lateral mismatch."))
           ;; Scan selection for all mainline pipe types and show picker
           (setq cv-all-templates (scan-main-types ss))
           (setq cv-ctx (strcat "Fixing " (itoa (length cv-group))
                                " valve(s) with " cv-target-size " lateral"
                                "  --  pick a " cv-target-size " mainline as template"))
           (if cv-all-templates
             (progn
               (setq cv-tpl-pick (cv-pick-template-dialog cv-all-templates cv-ctx))
               (cond
                 ((< cv-tpl-pick 0)
                  (setq cv-tpl nil))  ; cancelled
                 ((= cv-tpl-pick (length cv-all-templates))  ; "Other -- pick from drawing..."
                  (setq cv-tpl
                    (car (entsel (strcat "\nPick " cv-target-size " mainline template pipe: ")))))
                 (T
                  (setq cv-tpl (cadr (nth cv-tpl-pick cv-all-templates))))
               )
             )
             (progn
               (princ "\nNo mainline pipes found in selection - pick template manually.")
               (setq cv-tpl
                 (car (entsel (strcat "\nPick " cv-target-size " mainline template pipe: "))))
             )
           )
           (if cv-tpl
             (progn
               (setq cv-tpl-xd (get-lafx-xd cv-tpl))
               (if cv-tpl-xd
                 (progn
                   ;; Show which template was chosen; warn if size doesn't match
                   (setq cv-tpl-strings (xd-strings cv-tpl-xd))
                   (setq cv-tpl-size (if (>= (length cv-tpl-strings) 3) (nth 2 cv-tpl-strings) "?"))
                   (princ (strcat "\nUsing template: "
                                  (if (>= (length cv-tpl-strings) 2) (nth 1 cv-tpl-strings) "?")
                                  " " cv-tpl-size))
                   (if (not (equal cv-tpl-size cv-target-size))
                     (princ (strcat "\nWarning: template is " cv-tpl-size
                                    " but expected " cv-target-size " - proceeding."))
                   )
                   (sssetfirst nil nil)
                   (setq fixed-count 0)
                   (foreach entry cv-group
                     (if (fix-valve-main-with-tpl (caddr entry) cv-tpl)
                       (setq fixed-count (1+ fixed-count))
                     )
                   )
                   (command "_.REDRAW")
                   (princ (strcat "\nFixed " (itoa fixed-count) " of "
                                  (itoa (length cv-group)) " for " cv-target-size
                                  ". Re-scanning..."))
                   (setq size-problems (scan-valve-sizes ss))
                   (setq size-sel-idx 0)
                   (if (= (length size-problems) 0) (princ "\nAll size issues clear!"))
                 )
                 (princ "\nNot a valid LandFX pipe - cancelled.")
               )
             )
             (princ "\nCancelled.")
           )
         )
         (princ "\nNo item selected.")
       )
      )
      ;; ── Find Valve (size section) ────────────────────────────────────────────
      ((= result 7)
       (if (and (>= *DLG-SIZE-IDX* 0) (< *DLG-SIZE-IDX* (length size-problems)))
         (progn
           (zoom-to-ent (caddr (nth *DLG-SIZE-IDX* size-problems)))
           (setq size-sel-idx *DLG-SIZE-IDX*)
         )
         (princ "\nNo item selected.")
       )
      )
      ;; ── Fix Manually (connection section) ────────────────────────────────────
      ;; Zooms to the selected valve and exits the command so the user can freely
      ;; delete and redraw pipe connections.  The selection set is remembered in
      ;; *CV-LAST-SS*, so running CHECKVALVES again will rescan and reopen the
      ;; dialog immediately without needing to re-select geometry.
      ((= result 8)
       (if (and (>= *DLG-CONN-IDX* 0) (< *DLG-CONN-IDX* (length conn-problems)))
         (progn
           (setq sel-ent (caddr (nth *DLG-CONN-IDX* conn-problems)))
           (zoom-to-ent sel-ent)
           (princ "\nFix the pipe connections in the drawing.")
           (princ "\nRun CHECKVALVES again when done - your selection will be rescanned automatically.")
           (setq done T)
         )
         (princ "\nNo item selected.")
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
