;; FAIRWAY CENTERLINE GENERATOR - FWCENTER
;; Creates a smooth spline-based centerline through a closed fairway polyline,
;; then converts it to a polyline for LandFX head-layout compatibility.
;; Type FWCENTER to run
;;
;; TODO (dogleg support): current sampling is axis-aligned (X or Y).
;; For severely bent fairways, rotate the scan axis to the fairway's principal
;; direction, or switch to a "skeleton walking" approach.

(defun C:FWCENTER (/ pline-ent vertices min-x max-x min-y max-y
                   width height center-pts x-positions y-positions
                   pos-index test-x test-y y-points x-points
                   y-min y-max x-min x-max center-x center-y
                   vertices-list pline-data centerline-ent spline-ent
                   head-spacing offset-dist width-measurements
                   fairway-width total-width min-width max-width avg-width w
                   step-x step-y ed item verts px py inside i n j xi yi xj yj
                   vertex-count num-samples frac step-frac spline-precision)

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

      ;; Sample count — 40 evenly-spaced positions (avoids endpoints at 0/1 which hit boundary)
      (setq num-samples 40)
      (setq step-frac (/ 1.0 (+ num-samples 1)))

      ;; Create centerline points
      (setq center-pts '())
      (setq width-measurements '())

      (if (> width height)
        ;; Horizontal fairway — scan along X
        (progn
          (princ (strcat "\nHorizontal fairway detected — sampling " (itoa num-samples) " positions"))
          (setq frac step-frac)
          (setq pos-index 1)
          (while (<= pos-index num-samples)
            (setq test-x (+ min-x (* frac width)))

            ;; Find Y range at this X
            (setq y-points '())
            (setq test-y min-y)
            (setq step-y (/ height 100.0))
            (while (< test-y max-y)
              (if (point-in-polygon (list test-x test-y) vertices)
                (setq y-points (append y-points (list test-y)))
              )
              (setq test-y (+ test-y step-y))
            )

            (if (> (length y-points) 2)
              (progn
                (setq y-min (apply 'min y-points))
                (setq y-max (apply 'max y-points))
                (setq center-y (/ (+ y-min y-max) 2.0))
                (setq fairway-width (- y-max y-min))
                (setq center-pts (append center-pts (list (list test-x center-y))))
                (setq width-measurements (append width-measurements (list fairway-width)))
              )
            )
            (setq frac (+ frac step-frac))
            (setq pos-index (1+ pos-index))
          )
        )
        ;; Vertical fairway — scan along Y
        (progn
          (princ (strcat "\nVertical fairway detected — sampling " (itoa num-samples) " positions"))
          (setq frac step-frac)
          (setq pos-index 1)
          (while (<= pos-index num-samples)
            (setq test-y (+ min-y (* frac height)))

            ;; Find X range at this Y
            (setq x-points '())
            (setq test-x min-x)
            (setq step-x (/ width 100.0))
            (while (< test-x max-x)
              (if (point-in-polygon (list test-x test-y) vertices)
                (setq x-points (append x-points (list test-x)))
              )
              (setq test-x (+ test-x step-x))
            )

            (if (> (length x-points) 2)
              (progn
                (setq x-min (apply 'min x-points))
                (setq x-max (apply 'max x-points))
                (setq center-x (/ (+ x-min x-max) 2.0))
                (setq fairway-width (- x-max x-min))
                (setq center-pts (append center-pts (list (list center-x test-y))))
                (setq width-measurements (append width-measurements (list fairway-width)))
              )
            )
            (setq frac (+ frac step-frac))
            (setq pos-index (1+ pos-index))
          )
        )
      )

      (princ (strcat "\nFound " (itoa (length center-pts)) " centerline points"))

      ;; Create centerline if we found enough points
      (if (>= (length center-pts) 2)
        (progn
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

          ;; --- Build SPLINE through all centerline points ---
          (princ (strcat "\nCreating spline from " (itoa (length center-pts)) " points..."))
          (command "._SPLINE")
          (foreach pt center-pts
            (command (list (car pt) (cadr pt) 0.0))
          )
          (command "")   ;; end point input
          (command "")   ;; accept default start tangent
          (command "")   ;; accept default end tangent
          (setq spline-ent (entlast))
          (command "._REGEN")

          ;; --- Convert spline to polyline via SPLINEDIT ---
          ;; Precision 10: ~10 polyline segments per control-point span — smooth but compact
          ;; TODO: (entdel spline-ent) — add after output quality is confirmed in testing
          (setq spline-precision 10)
          (princ "\nConverting spline to polyline for LandFX...")
          (command "._SPLINEDIT" spline-ent "P" spline-precision "")
          (setq centerline-ent (entlast))
          (princ "\nSmooth polyline centerline created!")
          (princ "\n(Spline left in drawing for inspection — will be auto-deleted in final version)")
          (command "._REGEN")

          ;; --- Offer offset lines ---
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
          ;; Fallback: too few points — draw basic PLINE
          (princ (strcat "\nOnly found " (itoa (length center-pts)) " center points"))
          (if (>= (length center-pts) 2)
            (progn
              (princ "\nFalling back to basic PLINE...")
              (command "._PLINE")
              (foreach pt center-pts
                (command pt)
              )
              (command "")
              (setq centerline-ent (entlast))
              (princ "\nBasic polyline centerline created as fallback")
            )
            (princ "\nNeed at least 2 center points — cannot create centerline")
          )
        )
      )
    )
    (princ "\nNo lwpolyline selected")
  )
  (princ)
)

(princ "\nFairway Centerline Generator loaded.")
(princ "\nType FWCENTER to create centerline.")
(princ)
