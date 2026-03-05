;; FAIRWAY CENTERLINE GENERATOR - WALKING METHOD
;; Creates a centerline by "walking" through the fairway shape
;; Works better with kidney shapes and curved fairways
;; Usage: Type FWCENTER at command line

(defun C:FWCENTER (/ pline-ent vertices center-pts pt1 pt2 test-pt
                    min-x max-x min-y max-y centroid start-pt
                    current-pt direction step-size max-steps
                    left-dist right-dist best-pt i j
                    search-radius angle test-x test-y
                    inside-count total-tests)
  
  ;; Function to get vertices from lwpolyline
  (defun get-vertices (pline-ent)
    (setq ed (entget pline-ent))
    (setq verts '())
    (foreach item ed
      (if (= (car item) 10)
        (setq verts (append verts (list (list (cadr item) (caddr item)))))
      )
    )
    verts
  )
  
  ;; Function to test if a point is inside the polyline
  (defun point-inside-poly (pt poly-verts)
    (setq x (car pt))
    (setq y (cadr pt))
    (setq inside nil)
    (setq i 0)
    (setq n (length poly-verts))
    
    (while (< i n)
      (setq j (if (= i (1- n)) 0 (1+ i)))
      (setq xi (car (nth i poly-verts)))
      (setq yi (cadr (nth i poly-verts)))
      (setq xj (car (nth j poly-verts)))
      (setq yj (cadr (nth j poly-verts)))
      
      (if (and (or (and (> yi y) (<= yj y)) (and (<= yi y) (> yj y)))
               (< x (+ xi (* (/ (- y yi) (- yj yi)) (- xj xi)))))
        (setq inside (not inside))
      )
      (setq i (1+ i))
    )
    inside
  )
  
  ;; Function to find distance to boundary in a direction
  (defun distance-to-boundary (pt angle poly-verts max-dist)
    (setq test-dist 5.0)
    (setq found-boundary nil)
    
    (while (and (< test-dist max-dist) (not found-boundary))
      (setq test-x (+ (car pt) (* test-dist (cos angle))))
      (setq test-y (+ (cadr pt) (* test-dist (sin angle))))
      (setq test-pt (list test-x test-y))
      
      (if (not (point-inside-poly test-pt poly-verts))
        (setq found-boundary T)
        (setq test-dist (+ test-dist 5.0))
      )
    )
    
    (if found-boundary test-dist max-dist)
  )
  
  ;; Main function
  (princ "\nSelect closed fairway polyline: ")
  (setq pline-ent (car (entsel)))
  
  (if (and pline-ent (= (cdr (assoc 0 (entget pline-ent))) "LWPOLYLINE"))
    (progn
      (princ "\nAnalyzing fairway geometry...")
      
      ;; Get vertices
      (setq vertices (get-vertices pline-ent))
      (princ (strcat "\nFound " (itoa (length vertices)) " vertices"))
      
      ;; Find centroid as starting point
      (setq sum-x 0.0) (setq sum-y 0.0)
      (foreach v vertices
        (setq sum-x (+ sum-x (car v)))
        (setq sum-y (+ sum-y (cadr v)))
      )
      (setq centroid (list (/ sum-x (length vertices)) (/ sum-y (length vertices))))
      (princ (strcat "\nCentroid: " (rtos (car centroid) 2 2) "," (rtos (cadr centroid) 2 2)))
      
      ;; Check if centroid is inside (it should be for a proper fairway)
      (setq centroid-inside (point-inside-poly centroid vertices))
      (if centroid-inside
        (progn
          (princ "\nCentroid is inside fairway - good!")
          
          ;; Find the geometric center by testing points in a grid
          (setq min-x 1e10) (setq max-x -1e10) 
          (setq min-y 1e10) (setq max-y -1e10)
          
          (foreach v vertices
            (if (< (car v) min-x) (setq min-x (car v)))
            (if (> (car v) max-x) (setq max-x (car v)))
            (if (< (cadr v) min-y) (setq min-y (cadr v)))
            (if (> (cadr v) max-y) (setq max-y (cadr v)))
          )
          
          ;; Create a simple centerline by sampling points and finding their "most centered" positions
          (setq center-pts '())
          (setq num-samples 20)  ;; Increased for smoother result
          
          ;; For kidney shapes, sample along both primary axes
          (setq width (- max-x min-x))
          (setq height (- max-y min-y))
          
          (princ (strcat "\nFairway bounds: " (rtos width 2 1) " x " (rtos height 2 1)))
          
          ;; Use much finer sampling for geolocated coordinates
          (setq sample-step (if (> width height) (/ width 200.0) (/ height 200.0)))  ;; Finer sampling
          (princ (strcat "\nUsing sampling step: " (rtos sample-step 2 2)))
          
          ;; Clear any potential variable conflicts
          (setq section-num nil)  ;; Reset before use
          
          (if (> width height)
            ;; Sample along X axis
            (progn
              (princ "\nSampling along X-axis (horizontal fairway)")
              (setq i 1)
              (while (< i (1- num-samples))
                (setq x-test (+ min-x (* (/ (float i) (float (1- num-samples))) width)))
                
                ;; Find the Y range at this X by testing points with finer resolution
                (setq y-min 1e10) (setq y-max -1e10) (setq y-count 0)
                (setq y-test min-y)
                (while (< y-test max-y)
                  (if (point-inside-poly (list x-test y-test) vertices)
                    (progn
                      (if (< y-test y-min) (setq y-min y-test))
                      (if (> y-test y-max) (setq y-max y-test))
                      (setq y-count (1+ y-count))
                    )
                  )
                  (setq y-test (+ y-test sample-step))  ;; Use calculated step size
                )
                
                ;; Debug output
                (princ (strcat "\n  Sample " (itoa i) " at X=" (rtos x-test 2 1) ": " (itoa y-count) " inside points"))
                
                ;; If we found a valid Y range, add center point
                (if (> y-count 3)  ;; Need at least a few points to be confident
                  (progn
                    (setq center-y (/ (+ y-min y-max) 2.0))
                    (setq center-pts (append center-pts (list (list x-test center-y))))
                    (princ (strcat " -> Center: " (rtos center-y 2 1)))
                  )
                  (princ " -> Not enough points")
                )
                (setq i (1+ i))
              )
            )
            ;; Sample along Y axis
            (progn
              (princ "\nSampling along Y-axis (vertical fairway)")
              (setq sample-idx 1)  ;; Use different variable name
              (while (< sample-idx (1- num-samples))
                (setq y-test (+ min-y (* (/ (float sample-idx) (float (1- num-samples))) height)))
                
                ;; Find the X range at this Y with finer resolution
                (setq x-min 1e10) (setq x-max -1e10) (setq x-count 0)
                (setq x-test min-x)
                (while (< x-test max-x)
                  (if (point-inside-poly (list x-test y-test) vertices)
                    (progn
                      (if (< x-test x-min) (setq x-min x-test))
                      (if (> x-test x-max) (setq x-max x-test))
                      (setq x-count (1+ x-count))
                    )
                  )
                  (setq x-test (+ x-test sample-step))  ;; Use calculated step size
                )
                
                ;; Debug output
                (princ (strcat "\n  Sample " (itoa sample-idx) " at Y=" (rtos y-test 2 1) ": " (itoa x-count) " inside points"))
                
                ;; Add center point if valid range found
                (if (> x-count 3)  ;; Need at least a few points to be confident
                  (progn
                    (setq center-x (/ (+ x-min x-max) 2.0))
                    (setq center-pts (append center-pts (list (list center-x y-test))))
                    (princ (strcat " -> Center: " (rtos center-x 2 1)))
                  )
                  (princ " -> Not enough points")
                )
                (setq sample-idx (1+ sample-idx))
              )
            )
          )
          
          ;; Create the centerline as a single polyline
          (if (>= (length center-pts) 2)
            (progn
              (princ (strcat "\nFound " (itoa (length center-pts)) " center points"))
              
              ;; Calculate fairway width statistics from the sampling data
              (setq width-measurements '())
              (if (> width height)
                ;; For horizontal fairway, we have Y ranges at each X
                (foreach pt center-pts
                  (setq x-pos (car pt))
                  ;; Recalculate the Y range at this X position
                  (setq y-min 1e10) (setq y-max -1e10)
                  (setq y-test min-y)
                  (while (< y-test max-y)
                    (if (point-inside-poly (list x-pos y-test) vertices)
                      (progn
                        (if (< y-test y-min) (setq y-min y-test))
                        (if (> y-test y-max) (setq y-max y-test))
                      )
                    )
                    (setq y-test (+ y-test sample-step))
                  )
                  (if (< y-min 1e9)  ; Valid measurement found
                    (setq width-measurements (append width-measurements (list (- y-max y-min))))
                  )
                )
                ;; For vertical fairway, we have X ranges at each Y
                (foreach pt center-pts
                  (setq y-pos (cadr pt))
                  ;; Recalculate the X range at this Y position
                  (setq x-min 1e10) (setq x-max -1e10)
                  (setq x-test min-x)
                  (while (< x-test max-x)
                    (if (point-inside-poly (list x-test y-pos) vertices)
                      (progn
                        (if (< x-test x-min) (setq x-min x-test))
                        (if (> x-test x-max) (setq x-max x-test))
                      )
                    )
                    (setq x-test (+ x-test sample-step))
                  )
                  (if (< x-min 1e9)  ; Valid measurement found
                    (setq width-measurements (append width-measurements (list (- x-max x-min))))
                  )
                )
              )
              
              ;; Calculate statistics
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
              
              (princ "\nCreating centerline...")
              
              ;; Sort center points by their primary axis to ensure proper order
              (if (> width height)
                ;; Sort by X coordinate (horizontal fairway)
                (setq center-pts (vl-sort center-pts '(lambda (a b) (< (car a) (car b)))))
                ;; Sort by Y coordinate (vertical fairway)  
                (setq center-pts (vl-sort center-pts '(lambda (a b) (< (cadr a) (cadr b)))))
              )
              
              ;; Create polyline using entmake for better compatibility
              (setq vertices-list '())
              (setq vertex-count 0)
              (foreach pt center-pts
                (setq vertices-list (append vertices-list (list (cons 10 (list (car pt) (cadr pt) 0.0)))))
                (setq vertex-count (1+ vertex-count))
              )
              
              ;; Create LWPOLYLINE entity
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
                  (setq centerline-ent (entlast))  ; Store the centerline entity
                  (princ "\nCenterline created!")
                  ;; Force regeneration to ensure it's properly recognized
                  (command "._REGEN")
                  
                  ;; Add smoothing to the normal algorithm too
                  (princ "\nSmoothing centerline...")
                  (command "._PEDIT" centerline-ent "S" "" "")
                  
                  ;; Ask for head spacing and create offset lines
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
                      
                      ;; Create first offset line (right side)
                      (command "._OFFSET" offset-dist)
                      (command centerline-ent)
                      (command "_non" (list (+ (car (car center-pts)) offset-dist) (cadr (car center-pts))))
                      (command "")  ;; Exit OFFSET command
                      
                      ;; Create second offset line (left side)  
                      (command "._OFFSET" offset-dist)
                      (command centerline-ent)
                      (command "_non" (list (- (car (car center-pts)) offset-dist) (cadr (car center-pts))))
                      (command "")  ;; Exit OFFSET command
                      
                      (princ "\nHead spacing lines created!")
                      (princ (strcat "\nCenterline + 2 offset lines at " (rtos head-spacing 2 0) "ft spacing"))
                    )
                    (princ "\nSkipping offset lines - invalid spacing entered")
                  )
                )
                (progn
                  (princ "\nFallback: Creating with PLINE command...")
                  ;; Fallback to original method
                  (command "._PLINE")
                  (foreach pt center-pts
                    (command pt)
                  )
                  (command "")
                  (setq centerline-ent (entlast))  ;; Store the entity for smoothing
                  (princ "\nPLINE centerline created!")
                  
                  ;; Apply smoothing to the PLINE fallback too
                  (princ "\nSmoothing fallback centerline...")
                  (command "._PEDIT" centerline-ent "S" "" "")
                  
                  ;; Still offer offset option for fallback
                  (princ "\n--- IRRIGATION HEAD SPACING (Fallback Mode) ---")
                  (princ "\nEnter head spacing distance (fallback mode - no width analysis): ")
                  (setq head-spacing (getreal))
                  
                  (if (and head-spacing (> head-spacing 0))
                    (progn
                      (setq offset-dist (/ head-spacing 2.0))
                      (princ (strcat "\nCreating offset lines at +/- " (rtos offset-dist 2 1) " feet..."))
                      
                      ;; Create first offset line
                      (command "._OFFSET" offset-dist)
                      (command centerline-ent)
                      (command "_non" (list (+ (car (car center-pts)) offset-dist) (cadr (car center-pts))))
                      (command "")
                      
                      ;; Create second offset line (opposite direction)
                      (command "._OFFSET" offset-dist)
                      (command centerline-ent)  
                      (command "_non" (list (- (car (car center-pts)) offset-dist) (cadr (car center-pts))))
                      (command "")
                      
                      (princ "\nFallback head spacing lines created!")
                    )
                    (princ "\nSkipping offset lines")
                  )
                )
              )
            )
            (princ (strcat "\nOnly found " (itoa (length center-pts)) " center points - not enough for centerline"))
          )
        )
        (progn
          (princ "\nError: Centroid is outside fairway.")
          (princ "\nThis could mean:")
          (princ "\n  - Polyline is not properly closed")
          (princ "\n  - Polyline has self-intersections")
          (princ "\n  - Complex geometry confusing the algorithm")
          (princ "\nTry: 1) Check PLINETYPE, 2) Use PEDIT to close, 3) Simplify shape")
          
          ;; Try to proceed anyway with a vertex-based approach
          (princ "\nAttempting vertex-based centerline...")
          
          ;; For complex shapes, use a different approach: sample the vertices directly
          ;; and create approximate centerline points
          (setq min-x 1e10) (setq max-x -1e10) 
          (setq min-y 1e10) (setq max-y -1e10)
          
          (foreach v vertices
            (if (< (car v) min-x) (setq min-x (car v)))
            (if (> (car v) max-x) (setq max-x (car v)))
            (if (< (cadr v) min-y) (setq min-y (cadr v)))
            (if (> (cadr v) max-y) (setq max-y (cadr v)))
          )
          
          ;; Create approximate centerline by averaging vertex positions in segments
          (setq center-pts '())
          (setq width (- max-x min-x))
          (setq height (- max-y min-y))
          (setq num-segments 15)
          
          (if (> width height)
            ;; Horizontal fairway - divide into X segments and average Y positions
            (progn
              (setq segment-idx 0)
              (while (< segment-idx num-segments)
                (setq x-segment-start (+ min-x (* (/ (float segment-idx) (float num-segments)) width)))
                (setq x-segment-end (+ min-x (* (/ (float (1+ segment-idx)) (float num-segments)) width)))
                (setq x-segment-center (/ (+ x-segment-start x-segment-end) 2.0))
                
                ;; Find all vertices in this X segment
                (setq segment-vertices '())
                (foreach v vertices
                  (if (and (>= (car v) x-segment-start) (<= (car v) x-segment-end))
                    (setq segment-vertices (append segment-vertices (list v)))
                  )
                )
                
                ;; Average the Y coordinates of vertices in this segment
                (if (> (length segment-vertices) 0)
                  (progn
                    (setq sum-y 0.0)
                    (foreach v segment-vertices
                      (setq sum-y (+ sum-y (cadr v)))
                    )
                    (setq avg-y (/ sum-y (length segment-vertices)))
                    (setq center-pts (append center-pts (list (list x-segment-center avg-y))))
                  )
                )
                (setq segment-idx (1+ segment-idx))
              )
            )
            ;; Vertical fairway - divide into Y segments and average X positions
            (progn
              (setq segment-idx 0)
              (while (< segment-idx num-segments)
                (setq y-segment-start (+ min-y (* (/ (float segment-idx) (float num-segments)) height)))
                (setq y-segment-end (+ min-y (* (/ (float (1+ segment-idx)) (float num-segments)) height)))
                (setq y-segment-center (/ (+ y-segment-start y-segment-end) 2.0))
                
                ;; Find all vertices in this Y segment
                (setq segment-vertices '())
                (foreach v vertices
                  (if (and (>= (cadr v) y-segment-start) (<= (cadr v) y-segment-end))
                    (setq segment-vertices (append segment-vertices (list v)))
                  )
                )
                
                ;; Average the X coordinates of vertices in this segment
                (if (> (length segment-vertices) 0)
                  (progn
                    (setq sum-x 0.0)
                    (foreach v segment-vertices
                      (setq sum-x (+ sum-x (car v)))
                    )
                    (setq avg-x (/ sum-x (length segment-vertices)))
                    (setq center-pts (append center-pts (list (list avg-x y-segment-center))))
                  )
                )
                (setq segment-idx (1+ segment-idx))
              )
            )
          )
          
          (princ (strcat "\nCreated " (itoa (length center-pts)) " vertex-based centerline points"))
          
          ;; Continue to create the actual line using the fallback points
          (if (>= (length center-pts) 2)
            (progn
              (princ "\nCreating fallback centerline...")
              
              ;; Create simple polyline without width analysis (since we can't analyze complex geometry)
              (setq vertices-list '())
              (setq vertex-count 0)
              (foreach pt center-pts
                (setq vertices-list (append vertices-list (list (cons 10 (list (car pt) (cadr pt) 0.0)))))
                (setq vertex-count (1+ vertex-count))
              )
              
              ;; Create LWPOLYLINE entity
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
                  (princ "\nFallback centerline created!")
                  (command "._REGEN")
                  
                  ;; Smooth the fallback centerline to reduce chunkiness
                  (princ "\nSmoothing fallback centerline...")
                  (command "._PEDIT" centerline-ent "S" "" "")
                  
                  ;; Still offer offset option for fallback
                  (princ "\n--- IRRIGATION HEAD SPACING (Fallback Mode) ---")
                  (princ "\nEnter head spacing distance (fallback mode - no width analysis): ")
                  (setq head-spacing (getreal))
                  
                  (if (and head-spacing (> head-spacing 0))
                    (progn
                      (setq offset-dist (/ head-spacing 2.0))
                      (princ (strcat "\nCreating offset lines at +/- " (rtos offset-dist 2 1) " feet..."))
                      
                      ;; Create first offset line
                      (command "._OFFSET" offset-dist)
                      (command centerline-ent)
                      (command "_non" (list (+ (car (car center-pts)) offset-dist) (cadr (car center-pts))))
                      (command "")
                      
                      ;; Create second offset line (opposite direction)
                      (command "._OFFSET" offset-dist)
                      (command centerline-ent)  
                      (command "_non" (list (- (car (car center-pts)) offset-dist) (cadr (car center-pts))))
                      (command "")
                      
                      (princ "\nFallback head spacing lines created!")
                    )
                    (princ "\nSkipping offset lines")
                  )
                )
                (princ "\nFailed to create fallback centerline")
              )
            )
            (princ "\nNot enough fallback points created")
          )
        )
      )
    )
    (princ "\nError: Please select a valid LWPOLYLINE")
  )
  
  (princ)
)

;; Load message
(princ "\nFairway Centerline Generator (Walking Method) loaded.")
(princ "\nType FWCENTER to run.")
(princ)