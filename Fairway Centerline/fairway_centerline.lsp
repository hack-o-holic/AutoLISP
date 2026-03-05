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
                   vertex-count num-samples frac step-frac spline-precision
                   prev-layer)

  ;; Ensure layer exists (red), is on/thawed, and make it current
  (defun ensure-cl-layer ()
    (if (not (tblsearch "LAYER" "03_FAIRWAY_CL"))
      (entmake (list
        (cons 0   "LAYER")
        (cons 100 "AcDbSymbolTableRecord")
        (cons 100 "AcDbLayerTableRecord")
        (cons 2   "03_FAIRWAY_CL")
        (cons 70  0)    ;; not frozen
        (cons 62  1)    ;; red
        (cons 6   "Continuous")
      ))
    )
    ;; Turn on and thaw in case it was off/frozen
    (command "._LAYER" "ON" "03_FAIRWAY_CL" "THAW" "03_FAIRWAY_CL" "")
    (setvar "CLAYER" "03_FAIRWAY_CL")
  )

  ;; Get vertices from polyline — handles both LWPOLYLINE and classic 2D POLYLINE
  (defun get-poly-vertices (ent / ed etype verts item v-ent v-ed v-pt)
    (setq ed (entget ent))
    (setq etype (cdr (assoc 0 ed)))
    (setq verts '())
    (cond
      ((= etype "LWPOLYLINE")
       (foreach item ed
         (if (= (car item) 10)
           (setq verts (append verts (list (list (cadr item) (caddr item)))))
         )
       )
      )
      ((= etype "POLYLINE")
       (setq v-ent (entnext ent))
       (while (and v-ent (not (= (cdr (assoc 0 (entget v-ent))) "SEQEND")))
         (setq v-ed (entget v-ent))
         (if (= (cdr (assoc 0 v-ed)) "VERTEX")
           (progn
             (setq v-pt (assoc 10 v-ed))
             (if v-pt
               (setq verts (append verts (list (list (cadr v-pt) (caddr v-pt)))))
             )
           )
         )
         (setq v-ent (entnext v-ent))
       )
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
  (setq prev-layer (getvar "CLAYER"))
  (ensure-cl-layer)
  (princ "\nSelect closed fairway polyline: ")
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
  (setvar "CLAYER" prev-layer)
  (princ)
)

(princ "\nFairway Centerline Generator loaded.")
(princ "\nType FWCENTER to create centerline.")
(princ)

;;; ---------------------------------------------------------------------------
;;; FWFAST — Intersection-math variant for speed comparison
;;; Draws on layer 03_FAIRWAY_SPEED (blue) so results are visually distinct
;;; ---------------------------------------------------------------------------

(defun C:FWFAST (/ pline-ent vertices min-x max-x min-y max-y
                   width height center-pts
                   pos-index test-x test-y crossings
                   center-x center-y
                   centerline-ent spline-ent
                   head-spacing offset-dist width-measurements
                   fairway-width total-width min-width max-width avg-width w
                   ed item verts
                   num-samples frac step-frac spline-precision
                   prev-layer
                   n i j xi yi xj yj result x-int y-int)

  ;; Ensure speed-test layer exists (blue), on/thawed, current
  (defun ensure-fast-layer ()
    (if (not (tblsearch "LAYER" "03_FAIRWAY_SPEED"))
      (entmake (list
        (cons 0   "LAYER")
        (cons 100 "AcDbSymbolTableRecord")
        (cons 100 "AcDbLayerTableRecord")
        (cons 2   "03_FAIRWAY_SPEED")
        (cons 70  0)    ;; not frozen
        (cons 62  5)    ;; blue
        (cons 6   "Continuous")
      ))
    )
    (command "._LAYER" "ON" "03_FAIRWAY_SPEED" "THAW" "03_FAIRWAY_SPEED" "")
    (setvar "CLAYER" "03_FAIRWAY_SPEED")
  )

  ;; Get vertices from polyline — handles both LWPOLYLINE and classic 2D POLYLINE
  (defun get-poly-vertices-f (ent / ed etype verts item v-ent v-ed v-pt)
    (setq ed (entget ent))
    (setq etype (cdr (assoc 0 ed)))
    (setq verts '())
    (cond
      ((= etype "LWPOLYLINE")
       (foreach item ed
         (if (= (car item) 10)
           (setq verts (append verts (list (list (cadr item) (caddr item)))))
         )
       )
      )
      ((= etype "POLYLINE")
       (setq v-ent (entnext ent))
       (while (and v-ent (not (= (cdr (assoc 0 (entget v-ent))) "SEQEND")))
         (setq v-ed (entget v-ent))
         (if (= (cdr (assoc 0 v-ed)) "VERTEX")
           (progn
             (setq v-pt (assoc 10 v-ed))
             (if v-pt
               (setq verts (append verts (list (list (cadr v-pt) (caddr v-pt)))))
             )
           )
         )
         (setq v-ent (entnext v-ent))
       )
      )
    )
    verts
  )

  ;; Find all Y values where polygon boundary crosses x = test-x
  (defun find-y-crossings (test-x poly-verts / n i j xi yi xj yj y-int result)
    (setq result '())
    (setq n (length poly-verts))
    (setq i 0)
    (while (< i n)
      (setq j (if (= i (1- n)) 0 (1+ i)))
      (setq xi (car  (nth i poly-verts)))
      (setq yi (cadr (nth i poly-verts)))
      (setq xj (car  (nth j poly-verts)))
      (setq yj (cadr (nth j poly-verts)))
      (if (and (/= xi xj)
               (or (and (<= xi test-x) (> xj test-x))
                   (and (>  xi test-x) (<= xj test-x))))
        (progn
          (setq y-int (+ yi (* (/ (- test-x xi) (- xj xi)) (- yj yi))))
          (setq result (append result (list y-int)))
        )
      )
      (setq i (1+ i))
    )
    result
  )

  ;; Find all X values where polygon boundary crosses y = test-y
  (defun find-x-crossings (test-y poly-verts / n i j xi yi xj yj x-int result)
    (setq result '())
    (setq n (length poly-verts))
    (setq i 0)
    (while (< i n)
      (setq j (if (= i (1- n)) 0 (1+ i)))
      (setq xi (car  (nth i poly-verts)))
      (setq yi (cadr (nth i poly-verts)))
      (setq xj (car  (nth j poly-verts)))
      (setq yj (cadr (nth j poly-verts)))
      (if (and (/= yi yj)
               (or (and (<= yi test-y) (> yj test-y))
                   (and (>  yi test-y) (<= yj test-y))))
        (progn
          (setq x-int (+ xi (* (/ (- test-y yi) (- yj yi)) (- xj xi))))
          (setq result (append result (list x-int)))
        )
      )
      (setq i (1+ i))
    )
    result
  )

  ;; Main
  (setq prev-layer (getvar "CLAYER"))
  (ensure-fast-layer)
  (princ "\n[FWFAST] Select closed fairway polyline: ")
  (setq pline-ent (car (entsel)))

  (if pline-ent
    (progn
      (princ "\n[FWFAST] Analyzing fairway (intersection method)...")

      (setq vertices (get-poly-vertices-f pline-ent))
      (princ (strcat "\nFound " (itoa (length vertices)) " vertices"))

      (setq min-x 1e10) (setq max-x -1e10)
      (setq min-y 1e10) (setq max-y -1e10)
      (foreach v vertices
        (if (< (car v) min-x) (setq min-x (car v)))
        (if (> (car v) max-x) (setq max-x (car v)))
        (if (< (cadr v) min-y) (setq min-y (cadr v)))
        (if (> (cadr v) max-y) (setq max-y (cadr v)))
      )

      (setq width  (- max-x min-x))
      (setq height (- max-y min-y))
      (princ (strcat "\nBounds: " (rtos width 2 1) " x " (rtos height 2 1)))

      (setq num-samples 40)
      (setq step-frac (/ 1.0 (+ num-samples 1)))
      (setq center-pts '())
      (setq width-measurements '())

      (if (> width height)
        (progn
          (princ (strcat "\nHorizontal fairway — " (itoa num-samples) " intersection slices"))
          (setq frac step-frac)
          (setq pos-index 1)
          (while (<= pos-index num-samples)
            (setq test-x (+ min-x (* frac width)))
            (setq crossings (find-y-crossings test-x vertices))
            (if (>= (length crossings) 2)
              (progn
                (setq center-y    (/ (+ (apply 'min crossings) (apply 'max crossings)) 2.0))
                (setq fairway-width (- (apply 'max crossings) (apply 'min crossings)))
                (setq center-pts (append center-pts (list (list test-x center-y))))
                (setq width-measurements (append width-measurements (list fairway-width)))
              )
            )
            (setq frac (+ frac step-frac))
            (setq pos-index (1+ pos-index))
          )
        )
        (progn
          (princ (strcat "\nVertical fairway — " (itoa num-samples) " intersection slices"))
          (setq frac step-frac)
          (setq pos-index 1)
          (while (<= pos-index num-samples)
            (setq test-y (+ min-y (* frac height)))
            (setq crossings (find-x-crossings test-y vertices))
            (if (>= (length crossings) 2)
              (progn
                (setq center-x    (/ (+ (apply 'min crossings) (apply 'max crossings)) 2.0))
                (setq fairway-width (- (apply 'max crossings) (apply 'min crossings)))
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

      (if (>= (length center-pts) 2)
        (progn
          (if (> (length width-measurements) 0)
            (progn
              (setq total-width 0.0)
              (setq min-width (apply 'min width-measurements))
              (setq max-width (apply 'max width-measurements))
              (foreach w width-measurements (setq total-width (+ total-width w)))
              (setq avg-width (/ total-width (length width-measurements)))
              (princ "\n--- FAIRWAY WIDTH ANALYSIS ---")
              (princ (strcat "\nAverage width: " (rtos avg-width 2 1) " feet"))
              (princ (strcat "\nMinimum width: " (rtos min-width 2 1) " feet"))
              (princ (strcat "\nMaximum width: " (rtos max-width 2 1) " feet"))
            )
          )

          (if (> width height)
            (setq center-pts (vl-sort center-pts '(lambda (a b) (< (car a) (car b)))))
            (setq center-pts (vl-sort center-pts '(lambda (a b) (< (cadr a) (cadr b)))))
          )

          (princ (strcat "\nCreating spline from " (itoa (length center-pts)) " points..."))
          (command "._SPLINE")
          (foreach pt center-pts
            (command (list (car pt) (cadr pt) 0.0))
          )
          (command "") (command "") (command "")
          (setq spline-ent (entlast))
          (command "._REGEN")

          (setq spline-precision 10)
          (princ "\nConverting spline to polyline...")
          (command "._SPLINEDIT" spline-ent "P" spline-precision "")
          (setq centerline-ent (entlast))
          (princ "\n[FWFAST] Smooth polyline centerline created!")
          (princ "\n(Spline left for inspection)")
          (command "._REGEN")

          (princ "\n--- IRRIGATION HEAD SPACING ---")
          (if (> (length width-measurements) 0)
            (princ (strcat "\nEnter head spacing (Avg=" (rtos avg-width 2 0) "ft Min=" (rtos min-width 2 0) "ft Max=" (rtos max-width 2 0) "ft), or Enter to skip: "))
            (princ "\nEnter head spacing for offset lines, or press Enter to skip: ")
          )
          (setq head-spacing (getreal))

          (if (and head-spacing (> head-spacing 0))
            (progn
              (setq offset-dist (/ head-spacing 2.0))
              (command "._OFFSET" offset-dist)
              (command centerline-ent)
              (command "_non" (list (+ (car (car center-pts)) offset-dist) (cadr (car center-pts))))
              (command "")
              (command "._OFFSET" offset-dist)
              (command centerline-ent)
              (command "_non" (list (- (car (car center-pts)) offset-dist) (cadr (car center-pts))))
              (command "")
              (princ (strcat "\nOffset lines created at " (rtos head-spacing 2 0) "ft spacing"))
            )
            (princ "\nCenterline only — no offset lines")
          )
        )
        (princ "\nNeed at least 2 center points — cannot create centerline")
      )
    )
    (princ "\nNo lwpolyline selected")
  )
  (setvar "CLAYER" prev-layer)
  (princ)
)

(princ "\nType FWFAST to run intersection-math speed comparison.")
(princ)
