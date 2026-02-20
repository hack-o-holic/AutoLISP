;;; CHECKVALVES.lsp
;;; Diagnostic tool - finds lateral SOV valves not connected to
;;; exactly 1 lateral pipe and 1 mainline pipe.
;;; Highlights problem valves by changing color to red (color 1).
;;; Run CHECKVALVES-CLEAR to restore all to BYLAYER.

;;; Global: *CV-PROBLEM-ENTS* tracks highlighted entities across commands

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

;;; Highlight entity safely using selection set
(defun highlight-ent (ent / ss)
  (setq ss (ssadd ent (ssadd)))
  (sssetfirst ss ss)
)

;;; ── Main Command ─────────────────────────────────────────────────────────────

(defun C:CHECKVALVES ( / ss i ename entdata xd strings bname vtype
                         handles pipeent pinfo cat
                         lat-pipes main-pipes other-pipes
                         total-checked problem-count desc)

  ;; Clear any previous highlights first
  (if (and *CV-PROBLEM-ENTS* (> (length *CV-PROBLEM-ENTS*) 0))
    (progn
      (princ "\nClearing previous highlights...")
      (sssetfirst nil nil)
      (setq *CV-PROBLEM-ENTS* nil)
    )
  )

  (princ "\nCHECKVALVES - Select valves to check (window or pick): ")
  (setq ss (ssget))

  (if (null ss)
    (princ "\nNothing selected.")
    (progn
      (setq total-checked 0)
      (setq problem-count 0)
      (setq *CV-PROBLEM-ENTS* '())

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

            (if (= vtype "LATERAL-SOV")
              (progn
                (setq total-checked (1+ total-checked))
                (setq handles (get-handles xd))

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
                        (setq other-pipes (cons (list "dead" "dead-handle") other-pipes))
                      )
                    )
                    (setq other-pipes (cons (list "missing" "dead-handle") other-pipes))
                  )
                )

                (if (not (and (= (length lat-pipes) 1)
                              (= (length main-pipes) 1)))
                  (progn
                    (setq problem-count (1+ problem-count))
                    (setq *CV-PROBLEM-ENTS* (append *CV-PROBLEM-ENTS* (list ename)))
                    (setq desc
                      (strcat
                        bname
                        "  handle:" (cdr (assoc 5 entdata))
                        "  lateral:" (itoa (length lat-pipes))
                        "  main:" (itoa (length main-pipes))
                        (if (> (length other-pipes) 0)
                          (strcat "  other/dead:" (itoa (length other-pipes)))
                          ""
                        )
                      )
                    )
                    (princ (strcat "\n  >> " desc))
                    (setq *CV-PROBLEM-ENTS* (append *CV-PROBLEM-ENTS* (list ename)))
                  )
                )
              )
            )
          )
        )
        (setq i (1+ i))
      )

      ;; ── Summary ──────────────────────────────────────────────────────────
      (princ "\n")
      (princ "\n============================================================")
      (princ "\n  CHECKVALVES - LATERAL SOV DIAGNOSTIC")
      (princ "\n============================================================")
      (princ (strcat "\n  Lateral SOVs checked: " (itoa total-checked)))
      (princ (strcat "\n  Problem valves found: " (itoa problem-count)))

      (if (= problem-count 0)
        (princ "\n  All lateral SOVs are correctly connected!")
        (progn
          ;; Highlight all problem valves using selection highlight
          (if *CV-PROBLEM-ENTS*
            (progn
              (setq hilite-ss (ssadd))
              (foreach ent *CV-PROBLEM-ENTS*
                (ssadd ent hilite-ss)
              )
              (sssetfirst hilite-ss hilite-ss)
            )
          )
          (princ (strcat "\n  " (itoa problem-count)
                         " problem valve(s) highlighted."))
          (princ "\n  Fix them and re-run CHECKVALVES to verify.")
          (princ "\n  Run CHECKVALVES-CLEAR to clear highlights.")
        )
      )
      (princ "\n============================================================")
    )
  )
  (princ)
)

;;; ── Clear Command ────────────────────────────────────────────────────────────

(defun C:CHECKVALVES-CLEAR ( / )
  (sssetfirst nil nil)
  (if *CV-PROBLEM-ENTS*
    (setq *CV-PROBLEM-ENTS* nil)
  )
  (princ "\nHighlights cleared.")
  (princ)
)

(princ "\nCHECKVALVES loaded.")
(princ "\n  CHECKVALVES       - scan and highlight problem lateral SOVs in red")
(princ "\n  CHECKVALVES-CLEAR - restore highlighted valves to BYLAYER")
(princ)
