;; FAIRWAY CENTERLINE GENERATOR - FWCENTER
;; Creates a centerline lwpolyline through a closed fairway polyline
;; Works reliably on all fairway shapes including complex geometries
;; Type FWCENTER to run

(defun C:FWCENTER (/ pline-ent vertices min-x max-x min-y max-y 
                   width height center-pts x-positions y-positions
                   pos-index test-x test-y y-points x-points
                   y-min y-max x-min x-max center-x center-y
                   vertices-list pline-data centerline-ent
                   head-spacing offset-dist width-measurements
                   fairway-width total-width min-width max-width avg-width w
                   step-x step-y ed item verts px py inside i n j xi yi xj yj
                   vertex-count)
  
  ;; Get vertices from polyline
  (defun get-poly-vertices (ent)
    (setq ed (entget ent))
    (setq verts '())
    (foreach item ed
      (if (= (car item) 10)
        (setq verts (append verts (list (list (cadr item) (caddr item)))))
      )
    )
    verts
  )
  
  ;; Point in polygon test
  (defun point-in-polygon (pt poly-verts)
    (setq px (car pt))
    (setq py (cadr pt))
    (setq inside nil)
    (setq i 0)
    (setq n (length poly-verts))
    
    (while (< i n)
      (setq j (if (= i (1- n)) 0 (1+ i)))
      (setq xi (car (nth i poly-verts)))
      (setq yi (cadr (nth i poly-verts)))
      (setq xj (car (nth j poly-verts)))
      (setq yj (cadr (nth j poly-verts)))
      
      (if (and (or (and (> yi py) (<= yj py)) (and (<= yi py) (> yj py)))
               (< px (+ xi (* (/ (- py yi) (- yj yi)) (- xj xi)))))
        (setq inside (not inside))
      )
      (setq i (1+ i))
    )
    inside
  )
  
  ;; Main function
  (princ "\nSelect closed fairway lwpolyline: ")
  (setq pline-ent (car (entsel)))
  
  (if pline-ent
    (progn
      (princ "\nAnalyzing fairway...")
      
      ;; Get vertices
      (setq vertices (get-poly-vertices pline-ent))
      (princ (strcat "\nFound " (itoa (length vertices)) " vertices"))
      
      ;; Find bounds
      (setq min-x 1e10) (setq max-x -1e10)
      (setq min-y 1e10) (setq max-y -1e10)
      
      (foreach v vertices
        (if (< (car v) min-x) (setq min-x (car v)))
        (if (> (car v) max-x) (setq max-x (car v)))
        (if (< (cadr v) min-y) (setq min-y (cadr v)))
        (if (> (cadr v) max-y) (setq max-y (cadr v)))
      )
      
      (setq width (- max-x min-x))
      (setq height (- max-y min-y))
      
      (princ (strcat "\nBounds: " (rtos width 2 1) " x " (rtos height 2 1)))
      
      ;; Create centerline points using explicit position lists
      (setq center-pts '())
      (setq width-measurements '())  ;; Track width at each position
      
      (if (> width height)
        ;; Horizontal fairway - test specific X positions
        (progn
          (princ "\nHorizontal fairway detected")
          ;; Create list of X positions to test (avoid any loop counters)
          (setq x-positions (list
            (+ min-x (* 0.1 width))
            (+ min-x (* 0.2 width))
            (+ min-x (* 0.3 width))
            (+ min-x (* 0.4 width))
            (+ min-x (* 0.5 width))
            (+ min-x (* 0.6 width))
            (+ min-x (* 0.7 width))
            (+ min-x (* 0.8 width))
            (+ min-x (* 0.9 width))
          ))
          
          (setq pos-index 1)
          (foreach test-x x-positions
            (princ (strcat "\nTesting position " (itoa pos-index)))
            
            ;; Find Y range at this X
            (setq y-points '())
            (setq test-y min-y)
            (setq step-y (/ height 50.0))
            (while (< test-y max-y)
              (if (point-in-polygon (list test-x test-y) vertices)
                (setq y-points (append y-points (list test-y)))
              )
              (setq test-y (+ test-y step-y))
            )
            
            (princ (strcat " - found " (itoa (length y-points)) " inside points"))
            
            ;; If we found Y points, calculate center and width
            (if (> (length y-points) 2)
              (progn
                (setq y-min (apply 'min y-points))
                (setq y-max (apply 'max y-points))
                (setq center-y (/ (+ y-min y-max) 2.0))
                (setq fairway-width (- y-max y-min))
                (setq center-pts (append center-pts (list (list test-x center-y))))
                (setq width-measurements (append width-measurements (list fairway-width)))
                (princ " -> center found")
              )
              (princ " -> not enough points")
            )
            (setq pos-index (1+ pos-index))
          )
        )
        ;; Vertical fairway - test specific Y positions
        (progn
          (princ "\nVertical fairway detected")
          ;; Create list of Y positions to test
          (setq y-positions (list
            (+ min-y (* 0.1 height))
            (+ min-y (* 0.2 height))
            (+ min-y (* 0.3 height))
            (+ min-y (* 0.4 height))
            (+ min-y (* 0.5 height))
            (+ min-y (* 0.6 height))
            (+ min-y (* 0.7 height))
            (+ min-y (* 0.8 height))
            (+ min-y (* 0.9 height))
          ))
          
          (setq pos-index 1)
          (foreach test-y y-positions
            (princ (strcat "\nTesting position " (itoa pos-index)))
            
            ;; Find X range at this Y
            (setq x-points '())
            (setq test-x min-x)
            (setq step-x (/ width 50.0))
            (while (< test-x max-x)
              (if (point-in-polygon (list test-x test-y) vertices)
                (setq x-points (append x-points (list test-x)))
              )
              (setq test-x (+ test-x step-x))
            )
            
            (princ (strcat " - found " (itoa (length x-points)) " inside points"))
            
            ;; If we found X points, calculate center and width
            (if (> (length x-points) 2)
              (progn
                (setq x-min (apply 'min x-points))
                (setq x-max (apply 'max x-points))
                (setq center-x (/ (+ x-min x-max) 2.0))
                (setq fairway-width (- x-max x-min))
                (setq center-pts (append center-pts (list (list center-x test-y))))
                (setq width-measurements (append width-measurements (list fairway-width)))
                (princ " -> center found")
              )
              (princ " -> not enough points")
            )
            (setq pos-index (1+ pos-index))
          )
        )
      )
      
      ;; Create centerline if we found enough points
      (if (>= (length center-pts) 2)
        (progn
          (princ (strcat "\nCreating centerline with " (itoa (length center-pts)) " points..."))
          
          ;; Calculate and display width statistics
          (if (> (length width-measurements) 0)
            (progn
              (setq total-width 0.0)
              (setq min-width (apply 'min width-measurements))
              (setq max-width (apply 'max width-measurements))
              (foreach w width-measurements
                (setq total-width (+ total-width w))
              )
              (setq avg-width (/ total-width (length width-measurements)))
              
              (princ "\n--- FAIRWAY WIDTH ANALYSIS ---")
              (princ (strcat "\nAverage width: " (rtos avg-width 2 1) " feet"))
              (princ (strcat "\nMinimum width: " (rtos min-width 2 1) " feet"))
              (princ (strcat "\nMaximum width: " (rtos max-width 2 1) " feet"))
            )
          )
          
          ;; Sort points by primary axis
          (if (> width height)
            (setq center-pts (vl-sort center-pts '(lambda (a b) (< (car a) (car b)))))
            (setq center-pts (vl-sort center-pts '(lambda (a b) (< (cadr a) (cadr b)))))
          )
          
          ;; Create LWPOLYLINE using the WORKING method from v31
          (setq vertices-list '())
          (setq vertex-count 0)
          (foreach pt center-pts
            (setq vertices-list (append vertices-list (list (cons 10 (list (car pt) (cadr pt) 0.0)))))
            (setq vertex-count (1+ vertex-count))
          )
          
          ;; Create LWPOLYLINE entity (exact working code from v31)
          (setq pline-data 
            (append
              (list
                (cons 0 "LWPOLYLINE")
                (cons 100 "AcDbEntity")
                (cons 100 "AcDbPolyline")
                (cons 90 vertex-count)
                (cons 70 0)  ;; Open polyline
                (cons 43 0.0) ;; Constant width
              )
              vertices-list
            )
          )
          
          (if (entmake pline-data)
            (progn
              (setq centerline-ent (entlast))
              (princ "\nLWPOLYLINE centerline created!")
              ;; Force regeneration to ensure it's properly recognized
              (command "._REGEN")
              
              ;; Offer offset with width info in prompt
              (princ "\n--- IRRIGATION HEAD SPACING ---")
              (if (> (length width-measurements) 0)
                (princ (strcat "\nEnter head spacing (Fairway: Avg=" (rtos avg-width 2 0) "ft, Min=" (rtos min-width 2 0) "ft, Max=" (rtos max-width 2 0) "ft): "))
                (princ "\nEnter head spacing distance: ")
              )
              (setq head-spacing (getreal))
              
              (if (and head-spacing (> head-spacing 0))
                (progn
                  (setq offset-dist (/ head-spacing 2.0))
                  (princ (strcat "\nCreating offset lines at +/- " (rtos offset-dist 2 1) " feet..."))
                  
                  (command "._OFFSET" offset-dist)
                  (command centerline-ent)
                  (command "_non" (list (+ (car (car center-pts)) offset-dist) (cadr (car center-pts))))
                  (command "")
                  
                  (command "._OFFSET" offset-dist)
                  (command centerline-ent)
                  (command "_non" (list (- (car (car center-pts)) offset-dist) (cadr (car center-pts))))
                  (command "")
                  
                  (princ "\nOffset lines created!")
                  (princ (strcat "\nCenterline + 2 offset lines at " (rtos head-spacing 2 0) "ft spacing"))
                )
                (princ "\nCenterline only - no offset lines")
              )
            )
            (progn
              (princ "\nFailed to create LWPOLYLINE with entmake")
              (princ "\nFalling back to PLINE method...")
              ;; Fallback to PLINE command
              (command "._PLINE")
              (foreach pt center-pts
                (command pt)
              )
              (command "")
              (setq centerline-ent (entlast))
              (princ "\nPLINE centerline created as fallback")
            )
          )
        )
        (princ (strcat "\nOnly found " (itoa (length center-pts)) " center points - need at least 2"))
      )
    )
    (princ "\nNo lwpolyline selected")
  )
  (princ)
)

(princ "\nFairway Centerline Generator loaded.")
(princ "\nType FWCENTER to create centerline.")
(princ)