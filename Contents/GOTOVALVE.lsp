;;; GOTOVALVE.lsp
;;; Zooms to an entity by handle and highlights it safely using
;;; sssetfirst (selection highlight) - NO color changes to entity or layer.

(defun C:GOTOVALVE ( / handle ent entdata pt ss)
  (setq handle (getstring "\nEnter entity handle: "))
  (if (= handle "")
    (princ "\nNo handle entered.")
    (progn
      (setq ent (handent handle))
      (if (null ent)
        (princ (strcat "\nNo entity found with handle: " handle))
        (progn
          (setq entdata (entget ent))
          ;; Zoom to insertion point
          (setq pt (cdr (assoc 10 entdata)))
          (if pt
            (command "_.ZOOM" "_C" pt (/ (getvar "VIEWSIZE") 4))
            (progn
              (command "_.ZOOM" "_O" ent "")
              (command "_.ZOOM" "0.5X")
            )
          )
          ;; Highlight using selection set - safe, no entity modification
          (setq ss (ssadd ent (ssadd)))
          (sssetfirst ss ss)
          (princ (strcat "\nZoomed to: " handle
                         "  Block: " (cdr (assoc 2 entdata))))
          (princ "\nPress ESC to deselect.")
        )
      )
    )
  )
  (princ)
)

(princ "\nGOTOVALVE loaded. Type GOTOVALVE and enter a handle to zoom to it.")
(princ)
