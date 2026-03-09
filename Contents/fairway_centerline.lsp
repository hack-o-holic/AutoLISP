;; FAIRWAY CENTERLINE GENERATOR - FWCENTER
;; Creates a smooth centerline through a closed fairway polyline using
;; boundary intersection math, fits a spline, converts to polyline for
;; LandFX head-layout compatibility. Draws on layer 03_FAIRWAY_CL (red).
;; Type FWCENTER to run.
;;
;; Handles both LWPOLYLINE and classic 2D POLYLINE boundary types.
;;
;; TODO (dogleg support): sampling is axis-aligned (X or Y bounding box).
;; For severely bent fairways, rotate the scan axis to the fairway's
;; principal direction, or switch to a skeleton-walking approach.

(defun C:FWCENTER (/ pline-ent vertices min-x max-x min-y max-y
                   width height center-pts
                   pos-index test-x test-y crossings
                   center-x center-y
                   centerline-ent spline-ent
                   head-spacing offset-dist width-measurements
                   fairway-width total-width min-width max-width avg-width w
                   ed item verts
                   num-samples frac step-frac spline-precision
                   prev-layer
                   n i j xi yi xj yj x-int y-int
                   dcl-id dcl-file dlg-result raw-spacing)

  ;; Ensure layer 03_FAIRWAY_CL exists (red), is on/thawed, set current
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
    (command "._LAYER" "ON" "03_FAIRWAY_CL" "THAW" "03_FAIRWAY_CL" "")
    (setvar "CLAYER" "03_FAIRWAY_CL")
  )

  ;; Get vertices — handles both LWPOLYLINE and classic 2D POLYLINE
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
  (ensure-cl-layer)
  (princ "\nSelect closed fairway polyline: ")
  (setq pline-ent (car (entsel)))

  (if pline-ent
    (progn
      (princ "\nAnalyzing fairway...")

      (setq vertices (get-poly-vertices pline-ent))
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
          (princ (strcat "\nHorizontal fairway — " (itoa num-samples) " slices"))
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
          (princ (strcat "\nVertical fairway — " (itoa num-samples) " slices"))
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
          ;; Width statistics
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

          ;; Sort points by primary axis
          (if (> width height)
            (setq center-pts (vl-sort center-pts '(lambda (a b) (< (car a) (car b)))))
            (setq center-pts (vl-sort center-pts '(lambda (a b) (< (cadr a) (cadr b)))))
          )

          ;; Build SPLINE through centerline points
          (princ (strcat "\nCreating spline from " (itoa (length center-pts)) " points..."))
          (command "._SPLINE")
          (foreach pt center-pts
            (command (list (car pt) (cadr pt) 0.0))
          )
          (command "") (command "") (command "")
          (setq spline-ent (entlast))

          ;; Convert spline to polyline (precision 10 = smooth, compact)
          (princ "\nConverting to polyline...")
          (setq spline-precision 10)
          (command "._SPLINEDIT" spline-ent "P" spline-precision "")
          (setq centerline-ent (entlast))

          ;; Remove the intermediate spline — polyline is all we need
          (entdel spline-ent)

          (princ "\nCenterline created!")
          (command "._REGEN")

          ;; Offer offset lines via DCL dialog (falls back to command line if DCL unavailable)
          (setq head-spacing nil)
          (setq dcl-file (findfile "fairway_centerline.dcl"))
          (setq dcl-id (if dcl-file (load_dialog dcl-file) -1))

          (if (and (> dcl-id 0) (new_dialog "fairway_spacing" dcl-id))
            (progn
              ;; Populate width stat tiles
              (if (> (length width-measurements) 0)
                (progn
                  (set_tile "avg_width" (strcat (rtos avg-width 2 1) " ft"))
                  (set_tile "min_width" (strcat (rtos min-width 2 1) " ft"))
                  (set_tile "max_width" (strcat (rtos max-width 2 1) " ft"))
                )
              )
              ;; Accept: validate spacing > 0, else show errtile message
              (action_tile "accept"
                "(setq raw-spacing (get_tile \"head_spacing\"))
                 (if (and raw-spacing (> (strlen raw-spacing) 0)
                          (numberp (distof raw-spacing 2))
                          (> (distof raw-spacing 2) 0))
                   (progn (setq head-spacing (distof raw-spacing 2)) (done_dialog 1))
                   (set_tile \"error\" \"Enter a head spacing value greater than 0.\"))"
              )
              (action_tile "skip" "(done_dialog 0)")
              (setq dlg-result (start_dialog))
              (unload_dialog dcl-id)
            )
            (progn
              ;; Fallback: command-line prompt
              (if (> dcl-id 0) (unload_dialog dcl-id))
              (princ "\n--- IRRIGATION HEAD SPACING ---")
              (if (> (length width-measurements) 0)
                (princ (strcat "\nEnter head spacing (Avg=" (rtos avg-width 2 0)
                               "ft Min=" (rtos min-width 2 0)
                               "ft Max=" (rtos max-width 2 0)
                               "ft), or press Enter to skip: "))
                (princ "\nEnter head spacing for offset lines, or press Enter to skip: ")
              )
              (setq head-spacing (getreal))
            )
          )

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

              (princ (strcat "\nOffset lines created at " (rtos head-spacing 2 0) "ft spacing"))
            )
            (princ "\nCenterline only — no offset lines")
          )
        )
        (princ "\nNeed at least 2 center points — cannot create centerline")
      )
    )
    (princ "\nNo polyline selected")
  )
  (setvar "CLAYER" prev-layer)
  (princ)
)

(princ "\nFairway Centerline Generator loaded.")
(princ "\nType FWCENTER to create centerline.")
(princ)
