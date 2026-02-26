;;; COUNTVALVES.lsp
;;; Scans selected valve blocks, classifies by type, follows pipe connections,
;;; and produces a formatted report with SOV sizes and saddle tally.
;;; Optionally shows results in a dialog (requires checkvalves.dcl).

;;; ── Embedded Helpers ─────────────────────────────────────────────────────────

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

(defun strip-prefix (prefix s)
  (if (and s (>= (strlen s) (strlen prefix))
           (= (substr s 1 (strlen prefix)) prefix))
    (substr s (1+ (strlen prefix)))
    s
  )
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

(defun round1 (s / val)
  (setq val (atof s))
  (rtos val 2 1)
)

;;; Increment count for key in assoc tally list.
(defun tally-inc (key alist)
  (if (assoc key alist)
    (subst (cons key (1+ (cdr (assoc key alist)))) (assoc key alist) alist)
    (append alist (list (cons key 1)))
  )
)

;;; Append ename to the entity list for key in an assoc list of (key ename ...).
(defun ents-add (key ename alist)
  (if (assoc key alist)
    (subst (cons key (append (cdr (assoc key alist)) (list ename)))
           (assoc key alist)
           alist)
    (append alist (list (cons key (list ename))))
  )
)

;;; Convert a size string like "10\"" or "3/4\"" to a float for comparison.
(defun pipe-size-num (s)
  (atof (vl-string-subst "" "\"" (if s s "0")))
)

;;; Get lateral valve state from nth 2
(defun lateral-state (strings)
  (if (= (nth 2 strings) "Closed") "Closed" "Open")
)

;;; Get mainline valve state from Ds: entry
(defun mainline-state (strings / ds)
  (setq ds (find-prefixed "Ds:" strings))
  (if ds
    (progn
      (setq ds (strip-prefix "Ds:" ds))
      (if (or (= ds "") (= ds "Open")) "Open" "Closed")
    )
    "Open"
  )
)

;;; ── Feeder Main Traversal ────────────────────────────────────────────────────
;;;
;;; Starting from a branch main pipe, walk upstream through pipe fittings to find
;;; the trunk mainline it tees off of.
;;;
;;; Detection rule: a PIPEFITTING connected to 2+ other mainlines = trunk junction.
;;; The saddle goes on the LARGER of those two trunk pipes.
;;;
;;; If a fitting connects to only 1 other mainline, that is an intermediate segment;
;;; follow it and continue (hop count increments).
;;;
;;; Returns the trunk (feeder) main entity, or nil if not found within 3 hops.

(defun find-feeder-mainline (branch-main-ent
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
              ;; Only look at PIPEFITTING inserts
              (if (and (= (cdr (assoc 0 conn-ed)) "INSERT")
                       (vl-string-search "PIPEFITTING" (cdr (assoc 2 conn-ed))))
                (progn
                  ;; Collect all pipe-main LWPOLYLINEs from this fitting,
                  ;; excluding the pipe we came from (current-main).
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
                        (if (and fit-info
                                 (vl-string-search "pipe-main" (cadr fit-info)))
                          (setq other-mains (append other-mains (list fit-pipe)))
                        )
                      )
                    )
                  )
                  (cond
                    ;; 2+ other mains at this fitting = trunk junction found.
                    ;; Return the pipe with the largest numeric size.
                    ((>= (length other-mains) 2)
                     (setq best-ent nil)
                     (setq best-num 0.0)
                     (foreach m other-mains
                       (setq this-num (pipe-size-num (car (get-pipe-info m))))
                       (if (> this-num best-num)
                         (progn (setq best-ent m) (setq best-num this-num))
                       )
                     )
                     (setq found-trunk best-ent)
                    )
                    ;; Exactly 1 other main = intermediate segment, follow it.
                    ((= (length other-mains) 1)
                     (if (not next-hop)
                       (setq next-hop (car other-mains))
                     )
                    )
                    ;; 0 other mains = dead end, stop.
                  )
                )
              )
            )
          )
        )
      )
    )
    ;; If we found an intermediate to follow, hop to it; otherwise stop.
    (if (and (not found-trunk) next-hop)
      (progn
        (setq current-main next-hop)
        (setq hop (1+ hop))
      )
      (setq hop 3)  ; force loop exit if no next hop and no trunk
    )
  )
  found-trunk
)

;;; ── Dialog ───────────────────────────────────────────────────────────────────

(defun show-countvalves-dialog (sel-count valve-count sov-size-tally saddle-tally
                                sov-entities saddle-entities no-main-data
                                / dcl-path dcl-id result
                                  sov-rows saddle-rows nomn-rows
                                  enames ss-nav count-nav)
  ;; Locate checkvalves.dcl -- try *CP-DIR* (set by CHECKPIPES.lsp) then findfile.
  (setq dcl-path
    (cond
      ((and (boundp '*CP-DIR*) *CP-DIR*
            (findfile (strcat *CP-DIR* "\\" "checkvalves.dcl")))
       (strcat *CP-DIR* "\\" "checkvalves.dcl"))
      ((findfile "checkvalves.dcl"))
      (T nil)
    )
  )
  (if (null dcl-path)
    (princ "\n(Dialog skipped: checkvalves.dcl not found on support path.)")
    (progn
      (setq dcl-id (load_dialog dcl-path))
      ;; Index globals updated by action_tile strings.
      (setq *CV-SOV-IDX*    0)
      (setq *CV-SADDLE-IDX* 0)
      (setq *CV-NOMN-IDX*   0)
      ;; Pre-build display rows (built once, reused across while iterations).
      (setq sov-rows
        (if sov-size-tally
          (mapcar
            (function (lambda (e) (strcat (car e) "  SOV  x  " (itoa (cdr e)))))
            sov-size-tally)
          (list "(none found)")))
      (setq saddle-rows
        (if saddle-tally
          (mapcar
            (function (lambda (e) (strcat (car e) "  saddle  x  " (itoa (cdr e)))))
            saddle-tally)
          (list "(none found)")))
      (setq nomn-rows
        (if no-main-data
          (mapcar 'car no-main-data)
          (list "(none)")))
      ;; Dialog loop -- result 2 = Find saddle group, 3 = Find SOV group, 0 = close.
      (setq result 1)
      (while (> result 0)
        (if (not (new_dialog "valvetally_dialog" dcl-id))
          (setq result 0)
          (progn
            (set_tile "vt_header"
              (strcat (itoa sel-count) " objects scanned  |  "
                      (itoa valve-count) " lateral SOV(s) tallied"))
            (start_list "sov_list")
            (foreach row sov-rows (add_list row))
            (end_list)
            (start_list "saddle_list")
            (foreach row saddle-rows (add_list row))
            (end_list)
            (start_list "nomn_list")
            (foreach row nomn-rows (add_list row))
            (end_list)
            (set_tile "nomn_count"
              (strcat "Unconnected: " (itoa (length no-main-data))))
            (action_tile "sov_list"
              "(setq *CV-SOV-IDX* (atoi $value)) (if (= $reason 4) (done_dialog 3))")
            (action_tile "saddle_list"
              "(setq *CV-SADDLE-IDX* (atoi $value)) (if (= $reason 4) (done_dialog 2))")
            (action_tile "nomn_list"
              "(setq *CV-NOMN-IDX* (atoi $value)) (if (= $reason 4) (done_dialog 4))")
            (action_tile "find_sov_btn"
              "(setq *CV-SOV-IDX* (atoi (get_tile \"sov_list\"))) (done_dialog 3)")
            (action_tile "find_saddle_btn"
              "(setq *CV-SADDLE-IDX* (atoi (get_tile \"saddle_list\"))) (done_dialog 2)")
            (action_tile "find_nomn_btn"
              "(setq *CV-NOMN-IDX* (atoi (get_tile \"nomn_list\"))) (done_dialog 4)")
            (action_tile "close_btn" "(done_dialog 0)")
            (setq result (start_dialog))
          )
        )
        ;; Handle navigation -- highlight selected group's valve entities.
        (cond
          ((= result 2)
           ;; Saddle group navigation
           (if (and saddle-entities (< *CV-SADDLE-IDX* (length saddle-entities)))
             (progn
               (setq enames    (cdr (nth *CV-SADDLE-IDX* saddle-entities)))
               (setq ss-nav    (ssadd))
               (foreach en enames (ssadd en ss-nav))
               (setq count-nav (length enames))
               (command "_.REDRAW")
               (sssetfirst nil ss-nav)
               (princ (strcat "\nHighlighted " (itoa count-nav) " valve(s) for saddle: "
                              (car (nth *CV-SADDLE-IDX* saddle-tally))))
             )
             (princ "\nNo entities recorded for that saddle entry.")
           )
          )
          ((= result 3)
           ;; SOV size group navigation
           (if (and sov-entities (< *CV-SOV-IDX* (length sov-entities)))
             (progn
               (setq enames    (cdr (nth *CV-SOV-IDX* sov-entities)))
               (setq ss-nav    (ssadd))
               (foreach en enames (ssadd en ss-nav))
               (setq count-nav (length enames))
               (command "_.REDRAW")
               (sssetfirst nil ss-nav)
               (princ (strcat "\nHighlighted " (itoa count-nav) " valve(s) for SOV size: "
                              (car (nth *CV-SOV-IDX* sov-size-tally))))
             )
             (princ "\nNo entities recorded for that SOV entry.")
           )
          )
          ((= result 4)
           ;; No-main valve navigation -- highlight individual valve
           (if (and no-main-data (< *CV-NOMN-IDX* (length no-main-data)))
             (progn
               (setq ss-nav (ssadd))
               (ssadd (cdr (nth *CV-NOMN-IDX* no-main-data)) ss-nav)
               (command "_.REDRAW")
               (sssetfirst nil ss-nav)
               (princ (strcat "\nHighlighted valve with no main pipe: "
                              (car (nth *CV-NOMN-IDX* no-main-data))))
             )
             (princ "\nNo entities recorded for that entry.")
           )
          )
        )
      )
      (unload_dialog dcl-id)
    )
  )
)

;;; ── Main Command ─────────────────────────────────────────────────────────────

(defun C:COUNTVALVES ( / ss i ename entdata xd strings bname vtype vstate
                         handles pipeent pinfo pipe-a pipe-b
                         lat-tally main-tally main-detail
                         rcv-count unknown-count lat-count main-count
                         dp-entry df-entry pressure flow key size-key
                         lat-size main-size branch-main-ent feeder-ent feeder-size
                         sov-size-tally saddle-tally saddle-key
                         sov-entities saddle-entities no-main-data)

  ;; Re-use last CHECKVALVES selection if available, otherwise prompt.
  (if (and (boundp '*CV-LAST-SS*) *CV-LAST-SS*)
    (progn
      (princ (strcat "\nUsing last CHECKVALVES selection ("
                     (itoa (sslength *CV-LAST-SS*)) " objects)."))
      (setq ss *CV-LAST-SS*)
    )
    (progn
      (princ "\nCOUNTVALVES - Select objects to scan: ")
      (setq ss (ssget))
    )
  )

  (if (null ss)
    (princ "\nNothing selected.")
    (progn
      (setq lat-tally      '())
      (setq main-tally     '())
      (setq main-detail    '())
      (setq sov-size-tally '())
      (setq saddle-tally   '())
      (setq sov-entities    '())
      (setq saddle-entities '())
      (setq no-main-data    '())
      (setq lat-count      0)
      (setq main-count     0)
      (setq rcv-count      0)
      (setq unknown-count  0)

      (princ (strcat "\nScanning " (itoa (sslength ss)) " entities..."))

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

            (cond

              ;;── Lateral SOV ──────────────────────────────────────────────
              ((= vtype "LATERAL-SOV")
               (setq lat-count (1+ lat-count))
               (setq vstate  (lateral-state strings))
               (setq handles (get-handles xd))
               (setq pipe-a nil)
               (setq pipe-b nil)

               (foreach h handles
                 (setq pipeent (handent h))
                 (if pipeent
                   (progn
                     (setq pinfo (get-pipe-info pipeent))
                     (if (null pipe-a)
                       (setq pipe-a pinfo)
                       (setq pipe-b pinfo)
                     )
                   )
                 )
               )

               ;; Identify lateral and branch main sizes by category
               (setq lat-size
                 (cond
                   ((and pipe-a (vl-string-search "pipe-lateral" (cadr pipe-a))) (car pipe-a))
                   ((and pipe-b (vl-string-search "pipe-lateral" (cadr pipe-b))) (car pipe-b))
                   (T "?")
                 )
               )
               (setq main-size
                 (cond
                   ((and pipe-b (vl-string-search "pipe-main" (cadr pipe-b))) (car pipe-b))
                   ((and pipe-a (vl-string-search "pipe-main" (cadr pipe-a))) (car pipe-a))
                   (T "?")
                 )
               )

               ;; Find branch main entity for feeder traversal
               (setq branch-main-ent nil)
               (foreach h handles
                 (if (not branch-main-ent)
                   (progn
                     (setq pipeent (handent h))
                     (if pipeent
                       (progn
                         (setq pinfo (get-pipe-info pipeent))
                         (if (and pinfo (vl-string-search "pipe-main" (cadr pinfo)))
                           (setq branch-main-ent pipeent)
                         )
                       )
                     )
                   )
                 )
               )

               ;; Walk fittings to find the feeder (trunk) mainline
               (setq feeder-ent  (if branch-main-ent (find-feeder-mainline branch-main-ent) nil))
               ;; feeder-size: trunk found → its size
               ;;              no trunk but has main → use branch main (SOV is on a feeder branch,
               ;;              no upstream junction within 3 hops; saddle goes on that main)
               ;;              no main at all → nil (goes to no-main list)
               (setq feeder-size
                 (cond
                   (feeder-ent      (car (get-pipe-info feeder-ent)))
                   (branch-main-ent main-size)
                   (T               nil)
                 )
               )

               ;; Accumulate tallies and entity lists
               (setq sov-size-tally (tally-inc lat-size sov-size-tally))
               (setq sov-entities   (ents-add lat-size ename sov-entities))
               (if feeder-size
                 ;; Has a main connection -- add to saddle tally
                 (progn
                   (setq saddle-key      (strcat feeder-size " x " main-size))
                   (setq saddle-tally    (tally-inc saddle-key saddle-tally))
                   (setq saddle-entities (ents-add saddle-key ename saddle-entities))
                 )
                 ;; No main pipe found -- track separately for dialog
                 (setq no-main-data (append no-main-data (list (cons bname ename))))
               )

               ;; Detailed lateral tally (existing format)
               (setq key
                 (strcat
                   bname
                   "  |  Lateral: " lat-size
                   "  |  Main: "    main-size
                   "  |  Feeder: "  (if feeder-size feeder-size "none")
                   "  |  State: "   vstate
                 )
               )
               (setq lat-tally (tally-inc key lat-tally))
              )

              ;;── Mainline SOV ─────────────────────────────────────────────
              ((= vtype "MAINLINE-SOV")
               (setq main-count (1+ main-count))
               (setq vstate (mainline-state strings))

               (setq dp-entry (find-prefixed "Dp:" strings))
               (setq df-entry (find-prefixed "Df:" strings))
               (setq pressure (if dp-entry (round1 (strip-prefix "Dp:" dp-entry)) "undesigned"))
               (setq flow     (if df-entry (strip-prefix "Df:" df-entry) "undesigned"))

               (setq handles (get-handles xd))
               (setq pipe-a nil)
               (setq pipe-b nil)

               (foreach h handles
                 (setq pipeent (handent h))
                 (if pipeent
                   (progn
                     (setq pinfo (get-pipe-info pipeent))
                     (if (null pipe-a)
                       (setq pipe-a pinfo)
                       (setq pipe-b pinfo)
                     )
                   )
                 )
               )

               (setq size-key
                 (strcat
                   "Pipe A: " (if (and pipe-a (car pipe-a)) (car pipe-a) "?")
                   " [" (if (and pipe-a (cadr pipe-a)) (cadr pipe-a) "?") "]"
                   "  |  Pipe B: " (if (and pipe-b (car pipe-b)) (car pipe-b) "?")
                   " [" (if (and pipe-b (cadr pipe-b)) (cadr pipe-b) "?") "]"
                 )
               )
               (setq main-tally (tally-inc size-key main-tally))

               (setq key
                 (strcat
                   bname
                   "  |  Pipe A: " (if (and pipe-a (car pipe-a)) (car pipe-a) "?")
                   " [" (if (and pipe-a (cadr pipe-a)) (cadr pipe-a) "?") "]"
                   "  |  Pipe B: " (if (and pipe-b (car pipe-b)) (car pipe-b) "?")
                   " [" (if (and pipe-b (cadr pipe-b)) (cadr pipe-b) "?") "]"
                   "  |  Flow: " flow "gpm"
                   "  |  Pressure: " pressure "psi"
                   "  |  State: " vstate
                 )
               )
               (setq main-detail (append main-detail (list key)))
              )

              ;;── RCV ──────────────────────────────────────────────────────
              ((= vtype "RCV")
               (setq rcv-count (1+ rcv-count))
              )

              ;;── Unknown ──────────────────────────────────────────────────
              (T
               (setq unknown-count (1+ unknown-count))
              )
            )
          )
        )
        (setq i (1+ i))
      )

      ;; ── Command-line Report ─────────────────────────────────────────────
      (princ "\n")
      (princ "\n============================================================")
      (princ "\n  SHUTOFF VALVE REPORT")
      (princ "\n============================================================")

      (if (> lat-count 0)
        (progn
          (princ (strcat "\n  LATERAL SHUTOFF VALVES (" (itoa lat-count) " total)"))
          (princ "\n  ----------------------------------------------------------")
          (foreach entry lat-tally
            (princ (strcat "\n  [" (itoa (cdr entry)) "x]  " (car entry)))
          )
          (princ "\n")
          (princ "\n  SOV SIZE SUMMARY:")
          (foreach entry sov-size-tally
            (princ (strcat "\n    " (car entry) " SOV: " (itoa (cdr entry))))
          )
          (princ "\n")
          (princ "\n  SADDLE TALLY  (feeder main x branch main):")
          (foreach entry saddle-tally
            (princ (strcat "\n    " (car entry) ": " (itoa (cdr entry))))
          )
          (if no-main-data
            (progn
              (princ (strcat "\n\n  NO MAIN PIPE  (" (itoa (length no-main-data)) " valve(s) -- no main pipe connected):"))
              (foreach e no-main-data
                (princ (strcat "\n    " (car e)))
              )
            )
          )
          (princ "\n")
        )
      )

      (if (> main-count 0)
        (progn
          (princ (strcat "\n  MAINLINE SHUTOFF VALVES (" (itoa main-count) " total)"))
          (princ "\n  ----------------------------------------------------------")
          (princ "\n  Size Summary:")
          (foreach entry main-tally
            (princ (strcat "\n  [" (itoa (cdr entry)) "x]  " (car entry)))
          )
          (princ "\n")
          (princ "\n  Full Detail:")
          (foreach entry main-detail
            (princ (strcat "\n       " entry))
          )
          (princ "\n")
        )
      )

      (princ "\n  ----------------------------------------------------------")
      (if (> rcv-count 0)
        (princ (strcat "\n  Remote Control Valves skipped: " (itoa rcv-count)))
      )
      (if (> unknown-count 0)
        (princ (strcat "\n  Other/unknown entities skipped: " (itoa unknown-count)))
      )
      (princ "\n============================================================")
      (princ "\nCOUNTVALVES complete.")

      ;; ── Dialog ─────────────────────────────────────────────────────────
      (show-countvalves-dialog (sslength ss) lat-count sov-size-tally saddle-tally
                               sov-entities saddle-entities no-main-data)
    )
  )
  (princ)
)

(princ "\nCOUNTVALVES loaded. Type COUNTVALVES to run.")
(princ)
