;;;--------------------------------------------------
;;; Yardage_21.lsp
;;; Calculates front, center, and back yardages for
;;; golf course greens from sprinkler head locations.
;;; Accepts Splines, Polylines or LWPolylines around greens
;;; Modified: February 2026  CJR & CAI
;;;--------------------------------------------------


(defun c:YARDAGE (/ ent entlst region centroid cmd-echo spline-copy vla-region block base-point intersection-point temp-line point-list dist-list min-dist max-dist closest-point farthest-point centroid-dist j direction-vector vector-length unit-vector extend-distance start-point end-point spline-hole-number text-sel continue-loop prev-closest-pt prev-farthest-pt centroid-pt-ent circle-small-ent circle-250yd-ent hole-input-valid picked-type head-type)
  (princ "\nSelect a Green to find the center...")

  ;; Save current command echo setting and turn it off for most operations
  (setq cmd-echo (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)

  ;; Set point display style - small X marks
  (setvar "PDMODE" 3)    ; X mark style
  (setvar "PDSIZE" -1)   ; 1% of screen height (negative = % of screen)

  ;; Get selection of single spline
  (if (setq ent (car (entsel)))
    (if (member (cdr (assoc 0 (entget ent))) '("SPLINE" "POLYLINE" "LWPOLYLINE"))
      (progn
        (princ "\nProcessing green boundary...")

        ;; Get entity list before creating region
        (setq entlst (entlast))

        ;; Make a copy of the spline first to preserve original
        (command "._COPY" ent "" "0,0" "0,0")
        (setq spline-copy (entlast))

        ;; Create region from the copy
        (command "._REGION" spline-copy "")

        ;; Check if a new entity was created
        (if (not (eq entlst (entlast)))
          (progn
            ;; Get the newly created region
            (setq region (entlast))
            (setq vla-region (vlax-ename->vla-object region))

            ;; Get centroid directly from VLA object
            (if (not (vl-catch-all-error-p 
                       (setq centroid (vl-catch-all-apply 'vla-get-Centroid (list vla-region)))))
              (progn
                (setq centroid (vlax-safearray->list (vlax-variant-value centroid)))

                ;; Create markers at the centroid
                (entmake (list '(0 . "POINT") (cons 10 centroid)))
                (setq centroid-pt-ent (entlast))
                (command "._CIRCLE" centroid 0.5)
                (setq circle-small-ent (entlast))
                
                ;; Create 250 yard reference circle (250 * 3 = 750 drawing units) in red
                (command "._COLOR" "1")  ; Set color to red
                (command "._CIRCLE" centroid 750.0)
                (setq circle-250yd-ent (entlast))
                (command "._COLOR" "BYLAYER")  ; Reset color to bylayer

                (princ (strcat "\nCentroid marked at: " 
                              (rtos (car centroid) 2 3) "," 
                              (rtos (cadr centroid) 2 3)))

                ;; Prompt user to select hole number text OR type it in (loop until valid input)
                ;; Turn on CMDECHO so user can see prompts
                (setvar "CMDECHO" 1)
                
                (setq hole-input-valid nil)
                (setq spline-hole-number "")
                (while (not hole-input-valid)
                  (princ "\n")
                  (princ "\nSelect text with hole number or press Enter to type it in...")
                  (setq text-sel (ssget ":S"))  ; No filter - accept anything or Enter
                  
                  (cond
                    ;; User selected something
                    (text-sel
                      ;; Check if it's TEXT or MTEXT
                      (setq picked-type (cdr (assoc 0 (entget (ssname text-sel 0)))))
                      (if (or (= picked-type "TEXT") (= picked-type "MTEXT"))
                        ;; It's valid text - extract the value
                        (progn
                          (setq spline-hole-number (GET-TEXT-VALUE (ssname text-sel 0)))
                          (if (and spline-hole-number (> (strlen spline-hole-number) 0))
                            (progn
                              (princ (strcat "\nHole number found: " spline-hole-number))
                              (setq hole-input-valid T)
                            )
                            (progn
                              (princ "\nText is empty. Try again.")
                              (setq hole-input-valid nil)
                            )
                          )
                        )
                        ;; Not text - loop back
                        (progn
                          (princ (strcat "\nThat's a " picked-type ", not text. Try again."))
                          (setq hole-input-valid nil)
                        )
                      )
                    )
                    ;; User pressed Enter without selecting - go to type mode
                    (T
                      (princ "\nType hole number: ")
                      (setq spline-hole-number (getstring))
                      (if (and spline-hole-number (> (strlen spline-hole-number) 0))
                        (progn
                          (princ (strcat "\nHole number entered: " spline-hole-number))
                          (setq hole-input-valid T)
                        )
                        (progn
                          (princ "\nNo hole number entered. Try again.")
                          (setq hole-input-valid nil)
                        )
                      )
                    )
                  )
                )
                
                ;; Turn CMDECHO back off for the rest
                (setvar "CMDECHO" 0)

                ;; Loop to allow multiple sprinkler heads per spline
                (setq continue-loop T)
                (setq prev-closest-pt nil)
                (setq prev-farthest-pt nil)
                (while continue-loop
                  ;; Ask user to select a sprinkler head block
                  (princ "\nSelect sprinkler head (or right-click to finish this command)...")
                  (if (setq block (entsel))
                  (progn
                    ;; Clean up previous intersection points now, after new sprinkler selected
                    (if prev-closest-pt (entdel prev-closest-pt))
                    (if prev-farthest-pt (entdel prev-farthest-pt))
                    
                    (setq base-point (cdr (assoc 10 (entget (car block)))))
                    
                    ;; Get the block name (sprinkler head type)
                    (setq head-type (cdr (assoc 2 (entget (car block)))))
                    (princ (strcat "\nSprinkler head type: " head-type))
                    
                    ;; Calculate distance from block to centroid (convert to yards)
                    (setq centroid-dist (fix (+ (/ (distance base-point centroid) 3.0) 0.5)))
                    
                    ;; Calculate direction vector from block to centroid
                    (setq direction-vector (mapcar '- centroid base-point))
                    (setq vector-length (sqrt (+ (* (car direction-vector) (car direction-vector))
                                                (* (cadr direction-vector) (cadr direction-vector)))))
                    (setq unit-vector (mapcar '(lambda (x) (/ x vector-length)) direction-vector))
                    
                    ;; Extend line in both directions (extend by 1000 units each way)
                    (setq extend-distance 1000.0)
                    (setq start-point (mapcar '(lambda (base unit) (- base (* extend-distance unit))) base-point unit-vector))
                    (setq end-point (mapcar '(lambda (centroid unit) (+ centroid (* extend-distance unit))) centroid unit-vector))
                    
                    ;; Draw extended temporary line
                    (command "._LINE" start-point end-point "")
                    (setq temp-line (entlast))
                    
                    ;; Find actual intersection between the extended line and the green boundary
                    (setq intersection-point (vlax-invoke (vlax-ename->vla-object temp-line) 'IntersectWith (vlax-ename->vla-object ent) acExtendNone))
                    
                    ;; Process intersection if found
                    (if (and intersection-point 
                             (not (vl-catch-all-error-p intersection-point))
                             (listp intersection-point)
                             (> (length intersection-point) 0))
                      (progn
                        
                        ;; Convert flat list to list of 3D points
                        (setq point-list '())
                        (setq j 0)
                        (while (< j (length intersection-point))
                          (setq point-list (append point-list 
                                                   (list (list (nth j intersection-point) 
                                                              (nth (+ j 1) intersection-point) 
                                                              (nth (+ j 2) intersection-point)))))
                          (setq j (+ j 3))
                        )
                        
                        ;; Convert distances to yards and round
                        (setq dist-list '())
                        (foreach pt point-list
                          (setq dist-list (append dist-list (list (fix (+ (/ (distance base-point pt) 3.0) 0.5)))))
                        )
                        
                        ;; Find closest and farthest points and distances
                        (setq min-dist (apply 'min dist-list))
                        (setq max-dist (apply 'max dist-list))
                        
                        ;; Find the actual closest and farthest points
                        (setq closest-point nil)
                        (setq farthest-point nil)
                        (setq j 0)
                        (foreach pt point-list
                          (cond
                            ((= (nth j dist-list) min-dist)
                             (setq closest-point pt))
                            ((= (nth j dist-list) max-dist)
                             (setq farthest-point pt))
                          )
                          (setq j (1+ j))
                        )
                        
                        ;; Create points only at closest and farthest intersections
                        (if closest-point
                          (progn
                            (entmake (list '(0 . "POINT") (cons 10 closest-point)))
                            (setq prev-closest-pt (entlast))
                            (princ (strcat "\nCLOSEST (Front) intersection at: " 
                                          (rtos (car closest-point) 2 3) "," 
                                          (rtos (cadr closest-point) 2 3)
                                          " (Distance: " (itoa min-dist) " yards)"))))
                        
                        (if farthest-point
                          (progn
                            (entmake (list '(0 . "POINT") (cons 10 farthest-point)))
                            (setq prev-farthest-pt (entlast))
                            (princ (strcat "\nFARTHEST (Back) intersection at: " 
                                          (rtos (car farthest-point) 2 3) "," 
                                          (rtos (cadr farthest-point) 2 3)
                                          " (Distance: " (itoa max-dist) " yards)"))))
                        
                        ;; Display centroid distance
                        (princ (strcat "\nCentroid (Center) distance: " (itoa centroid-dist) " yards"))
                        
                        ;; Store distances for potential future use (all in yards)
                        ;; You can access these variables: min-dist, max-dist, centroid-dist
                        (princ (strcat "\nSummary - Front: " (itoa min-dist) " yards" 
                                      ", Center: " (itoa centroid-dist) " yards" 
                                      ", Back: " (itoa max-dist) " yards"))
                        
                        ;; Erase the temporary line BEFORE asking for block placement
                        (entdel temp-line)
                        
                        ;; Let user place TC_YDG distance block
                        (princ "\nClick to place TC_YDG distance block...")
                        (if (setq insert-point (getpoint))
                          (progn
                            ;; Check if TC_YDG block exists
                            (if (tblsearch "BLOCK" "TC_YDG")
                              (progn
                                ;; Insert TC_YDG block
                                (command "._INSERT" "TC_YDG" insert-point "1" "1" "0"
                                         ""  ; HOLE_NO - will be updated
                                         ""  ; YDG_F - will be updated
                                         ""  ; YDG_C - will be updated
                                         ""  ; YDG_B - will be updated
                                )

                                ;; Update attributes with calculated values
                                (UPDATE-TC-YDG-ATTRIBUTES (entlast) spline-hole-number min-dist centroid-dist max-dist head-type)
                                (princ "\nTC_YDG block placed and updated.")
                              )
                              (princ "\nError: TC_YDG block not found in drawing. Please create it first.")
                            )
                          )
                          (princ "\nDistance block placement cancelled.")
                        )
                      )
                      (progn
                        ;; No intersection found - clean up temp line
                        (if temp-line (entdel temp-line))
                        (princ "\nNo intersection found between line and spline.")
                      )
                    )
                  )
                  (progn
                    ;; User right-clicked or cancelled - exit the loop for this spline
                    (princ "\nFinishing this command.")
                    (setq continue-loop nil)
                  )
                )
                ) ;; End of while continue-loop
                
                ;; After loop ends, place the three TC_PIN blocks and clean up
                (princ "\nPlacing pins on green and cleaning up markers...")
                
                ;; Place front block if we have a front point
                (if (and prev-closest-pt closest-point)
                  (progn
                    (command "._INSERT" "TC_PIN_1_FRNT" closest-point "1" "1" "0")
                    (princ "\nFront pin block placed.")
                  )
                )
                
                ;; Place center block at centroid
                (if centroid
                  (progn
                    (command "._INSERT" "TC_PIN_2_CNTR" centroid "1" "1" "0")
                    (princ "\nCenter pin block placed.")
                  )
                )
                
                ;; Place back block if we have a back point
                (if (and prev-farthest-pt farthest-point)
                  (progn
                    (command "._INSERT" "TC_PIN_3_BACK" farthest-point "1" "1" "0")
                    (princ "\nBack pin block placed.")
                  )
                )
                
                ;; Delete the temporary point markers
                (if prev-closest-pt (entdel prev-closest-pt))
                (if centroid-pt-ent (entdel centroid-pt-ent))
                (if prev-farthest-pt (entdel prev-farthest-pt))
                
                ;; Delete the circles
                (if circle-small-ent (entdel circle-small-ent))
                (if circle-250yd-ent (entdel circle-250yd-ent))
                
                (princ "\nCleanup complete.")
              )
              (princ "\nError: Could not get centroid.")
            )

            ;; Clean up - delete only the temporary region
            (entdel region)
          )
          (princ "\nError: Could not create region from spline.")
        )
        (princ "\nDone processing.")
      )
      (princ "\nError: Selected entity is not a SPLINE, POLYLINE, or LWPOLYLINE.")
    )
    (princ "\nNo spline selected.")
  )

  ;; Restore command echo setting
  (setvar "CMDECHO" cmd-echo)
  (princ)
)

;; Function to get text value from TEXT or MTEXT entity
(defun GET-TEXT-VALUE (text-ent / ent-data text-string)
  (setq ent-data (entget text-ent))
  (if (= (cdr (assoc 0 ent-data)) "TEXT")
    (setq text-string (cdr (assoc 1 ent-data)))
    ;; For MTEXT, get the text string
    (setq text-string (cdr (assoc 1 ent-data)))
  )
  ;; Clean up the string - remove formatting codes if MTEXT
  (if text-string
    (progn
      ;; Remove MTEXT formatting codes
      (while (wcmatch text-string "*\\\\[A-Z]*;*")
        (setq text-string (vl-string-subst "" (vl-string-search "\\\\" text-string 0 (+ (vl-string-search ";" text-string 0) 1)) text-string))
      )
      ;; Trim whitespace
      (vl-string-trim " \t\n" text-string)
    )
  )
)

;; Function to update TC_YDG block attributes by tag name
(defun UPDATE-TC-YDG-ATTRIBUTES (block-ent hole-val front-dist center-dist back-dist head-type / att-list tag-name)
  (if (setq att-list (vlax-invoke (vlax-ename->vla-object block-ent) 'GetAttributes))
    (progn
      ;; Loop through all attributes and update by tag name
      (foreach att att-list
        (setq tag-name (vla-get-TagString att))
        (cond
          ((= tag-name "HOLE_NO")
           (vla-put-TextString att hole-val))
          ((= tag-name "YDG_F")
           (vla-put-TextString att (itoa front-dist)))
          ((= tag-name "YDG_C") 
           (vla-put-TextString att (itoa center-dist)))
          ((= tag-name "YDG_B")
           (vla-put-TextString att (itoa back-dist)))
          ((= tag-name "HEAD_TYPE")
           (vla-put-TextString att head-type))
        )
      )
      (vla-Update (vlax-ename->vla-object block-ent))
      (princ (strcat "\nAttributes updated - Hole: \"" hole-val "\" Front: " (itoa front-dist) " Center: " (itoa center-dist) " Back: " (itoa back-dist) " Head: " head-type))
    )
    (princ "\nError: Could not get block attributes")
  )
)

;; Fixed command to change point display style
(defun c:SETPOINTSTYLE ()
  (command "._PTYPE")
  (princ)
)

(princ "\nSpline Centroid Marker loaded.")
(princ "\nCommands available:")
(princ "\n  YARDAGE - Mark centroid and calculate yardages from sprinklers")
(princ "\n  SETPOINTSTYLE - Change point display style")
(princ)
