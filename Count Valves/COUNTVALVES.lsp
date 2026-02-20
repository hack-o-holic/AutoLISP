;;; COUNTVALVES.lsp
;;; Scans selected valve blocks, classifies by type, follows pipe connections,
;;; and produces a formatted report.
;;; Requires helper functions - either load VALVE_HELPERS.lsp first,
;;; or helpers are embedded below.

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

(defun tally-inc (key alist)
  (if (assoc key alist)
    (subst (cons key (1+ (cdr (assoc key alist)))) (assoc key alist) alist)
    (append alist (list (cons key 1)))
  )
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

;;; ── Main Command ─────────────────────────────────────────────────────────────

(defun C:COUNTVALVES ( / ss i ename entdata xd strings bname vtype vstate
                         handles pipeent pinfo pipe-a pipe-b
                         lat-tally main-tally main-detail
                         rcv-count unknown-count lat-count main-count
                         dp-entry df-entry pressure flow key size-key)

  (princ "\nCOUNTVALVES - Select valves (window select or pick): ")
  (setq ss (ssget))

  (if (null ss)
    (princ "\nNothing selected.")
    (progn
      (setq lat-tally    '())
      (setq main-tally   '())
      (setq main-detail  '())
      (setq lat-count    0)
      (setq main-count   0)
      (setq rcv-count    0)
      (setq unknown-count 0)

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

               ;; Find lateral and main pipe sizes by category
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

               (setq key
                 (strcat
                   bname
                   "  |  Lateral: " lat-size
                   "  |  Main: "    main-size
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

               ;; Size-only tally key
               (setq size-key
                 (strcat
                   "Pipe A: " (if (and pipe-a (car pipe-a)) (car pipe-a) "?")
                   " [" (if (and pipe-a (cadr pipe-a)) (cadr pipe-a) "?") "]"
                   "  |  Pipe B: " (if (and pipe-b (car pipe-b)) (car pipe-b) "?")
                   " [" (if (and pipe-b (cadr pipe-b)) (cadr pipe-b) "?") "]"
                 )
               )
               (setq main-tally (tally-inc size-key main-tally))

               ;; Full detail entry
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

      ;; ── Report ─────────────────────────────────────────────────────────
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
    )
  )
  (princ)
)

(princ "\nCOUNTVALVES loaded. Type COUNTVALVES to run.")
(princ)
