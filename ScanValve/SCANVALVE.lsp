;;; SCANVALVE.lsp
;;; Inspect LandFX shutoff valve XDATA and follow 1005 handles to connected entities
;;; Use: Type SCANVALVE, click a shutoff valve block

(defun C:SCANVALVE ( / ent entdata ename ss picked parent xdlist app xdentry handles h ent2 ent2data layer bname)

  (princ "\nSCANVALVE - Click a shutoff valve block: ")
  
  ;; Use entsel for direct (non-nested) block selection
  (setq picked (entsel))
  
  (if (null picked)
    (princ "\nNothing selected.")
    
    (progn
      (setq ename (car picked))
      (setq entdata (entget ename))
      
      (princ "\n\n========================================")
      (princ (strcat "\nENTITY: " (cdr (assoc 0 entdata))))
      (princ (strcat "\nHANDLE: " (cdr (assoc 5 entdata))))
      (princ (strcat "\nLAYER:  " (cdr (assoc 8 entdata))))
      (if (= (cdr (assoc 0 entdata)) "INSERT")
        (princ (strcat "\nBLOCK:  " (cdr (assoc 2 entdata))))
      )
      (princ "\n========================================")
      
      ;; Now dump ALL XDATA from all registered apps
      (princ "\n\n--- XDATA DUMP ---")
      (setq xdlist (entget ename '("*")))  ; get all xdata
      
      ;; Walk the xdlist looking for -3 group (xdata container)
      (setq xdentry (assoc -3 xdlist))
      
      (if (null xdentry)
        (princ "\n  No XDATA found on this entity.")
        (progn
          ;; xdentry looks like (-3 ("AppName" (1000 . "val") (1005 . "handle") ...))
          (foreach appdata (cdr xdentry)
            (setq app (car appdata))
            (princ (strcat "\n\n  APP: [" app "]"))
            (setq handles '())
            (foreach pair (cdr appdata)
              (princ (strcat "\n    " (itoa (car pair)) " = " 
                (cond
                  ((= (type (cdr pair)) 'STR) (cdr pair))
                  ((= (type (cdr pair)) 'INT) (itoa (cdr pair)))
                  ((= (type (cdr pair)) 'REAL) (rtos (cdr pair) 2 4))
                  (T (vl-prin1-to-string (cdr pair)))
                )
              ))
              ;; Collect 1005 handles for follow-up
              (if (= (car pair) 1005)
                (setq handles (cons (cdr pair) handles))
              )
            )
            
            ;; Follow each 1005 handle and report what it points to
            (if handles
              (progn
                (princ (strcat "\n\n  >> Following " (itoa (length handles)) " handle(s) for app [" app "]:"))
                (foreach h (reverse handles)
                  (princ (strcat "\n     Handle: " h))
                  (setq ent2 (handent h))
                  (if (null ent2)
                    (princ "  --> DEAD (entity not found)")
                    (progn
                      (setq ent2data (entget ent2 '("*")))
                      (princ (strcat "  --> Type:  " (cdr (assoc 0 ent2data))))
                      (princ (strcat "\n              Layer: " (cdr (assoc 8 ent2data))))
                      (if (= (cdr (assoc 0 ent2data)) "INSERT")
                        (princ (strcat "\n              Block: " (cdr (assoc 2 ent2data))))
                      )
                      ;; If it has its own XDATA, show a summary
                      (setq ent2xd (assoc -3 ent2data))
                      (if ent2xd
                        (progn
                          (princ "\n              XDATA apps on this entity:")
                          (foreach a2 (cdr ent2xd)
                            (princ (strcat "\n                [" (car a2) "]"))
                            (foreach p2 (cdr a2)
                              (princ (strcat "\n                  " (itoa (car p2)) " = "
                                (cond
                                  ((= (type (cdr p2)) 'STR) (cdr p2))
                                  ((= (type (cdr p2)) 'INT) (itoa (cdr p2)))
                                  ((= (type (cdr p2)) 'REAL) (rtos (cdr p2) 2 4))
                                  (T (vl-prin1-to-string (cdr p2)))
                                )
                              ))
                            )
                          )
                        )
                        (princ "\n              (no XDATA on this entity)")
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
      
      (princ "\n\n========================================")
      (princ "\nSCANVALVE complete.")
    )
  )
  (princ)
)

(princ "\nSCANVALVE loaded. Type SCANVALVE to inspect a shutoff valve.")
(princ)
