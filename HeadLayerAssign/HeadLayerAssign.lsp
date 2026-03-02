;; HeadLayerAssign.lsp
;; Scans modelspace for Land F/X irrigation head blocks and assigns them
;; to the correct layer based on brand, model, and nozzle size.
;;
;; Supported block name patterns:
;;
;;   Rain Bird (brand = RAIN):
;;     RAIN-702-<nozzle>-...        -> 12_RB7X2-<nozzle>
;;     RAIN-702IC-<nozzle>-...      -> 12_RB7X2-<nozzle>
;;     RAIN-752-<nozzle>-...        -> 12_RB7X2-<nozzle>
;;     RAIN-752IC-<nozzle>-...      -> 12_RB7X2-<nozzle>
;;     RAIN-952-<nozzle>-...        -> 13_RB952-<nozzle>
;;     RAIN-952IC-<nozzle>-...      -> 13_RB952-<nozzle>
;;
;;   Duplicate Rain Bird (instance digit at index 2, nozzle shifts to index 3):
;;     RAIN-752-2-<nozzle>-...      -> 12_RB7X2-<nozzle>
;;     RAIN-952-2-<nozzle>-...      -> 13_RB952-<nozzle>
;;     (same logic applies to 702, 702IC, 752IC, 952IC)
;;
;;   Toro Infiniti (brand = TORO, model must be 34, 35, 54, or 55):
;;     TORO-INF-<model>-<nozzle>-...      -> 14_INF-<nozzle>
;;     TORO-INF-<model>-2-<nozzle>-...    -> 14_INF-<nozzle>  (duplicate instance)
;;
;; Unmatched blocks will prompt the user for manual layer assignment.
;;
;; Command: HEADLAYERS
;;
;; Author: Generated for Chris / Irrigation Design workflow

;;---------------------------------------------------------------------------
;; LAYER TABLE
;; Format: (block-key . layer-name)
;;---------------------------------------------------------------------------
(defun LHA:GetLayerTable ()
  (list
    ;; Rain Bird 702 / 702IC / 752 / 752IC  -> 12_RB7X2-<nozzle>
    '("RB7X2-18"  . "12_RB7X2-18")
    '("RB7X2-20"  . "12_RB7X2-20")
    '("RB7X2-22"  . "12_RB7X2-22")
    '("RB7X2-24"  . "12_RB7X2-24")
    '("RB7X2-26"  . "12_RB7X2-26")
    '("RB7X2-28"  . "12_RB7X2-28")
    '("RB7X2-32"  . "12_RB7X2-32")
    '("RB7X2-36"  . "12_RB7X2-36")
    '("RB7X2-40"  . "12_RB7X2-40")
    '("RB7X2-44"  . "12_RB7X2-44")
    '("RB7X2-48"  . "12_RB7X2-48")
    '("RB7X2-50"  . "12_RB7X2-50")
    ;; Rain Bird 952 / 952IC  -> 13_RB952-<nozzle>
    '("RB952-44"  . "13_RB952-44")
    '("RB952-46"  . "13_RB952-46")
    '("RB952-48"  . "13_RB952-48")
    '("RB952-52"  . "13_RB952-52")
    '("RB952-56"  . "13_RB952-56")
    '("RB952-60"  . "13_RB952-60")
    '("RB952-64"  . "13_RB952-64")
    ;; Toro Infiniti (models 34, 35, 54, 55) -> 14_INF-<nozzle>
    '("INF-30"    . "14_INF-30")
    '("INF-30.1"  . "14_INF-30.1")
    '("INF-30.2"  . "14_INF-30.2")
    '("INF-30.3"  . "14_INF-30.3")
    '("INF-30.4"  . "14_INF-30.4")
    '("INF-30.5"  . "14_INF-30.5")
    '("INF-31"    . "14_INF-31")
    '("INF-32"    . "14_INF-32")
    '("INF-33"    . "14_INF-33")
    '("INF-34"    . "14_INF-34")
    '("INF-35"    . "14_INF-35")
    '("INF-36"    . "14_INF-36")
    '("INF-37"    . "14_INF-37")
    '("INF-51"    . "14_INF-51")
    '("INF-52"    . "14_INF-52")
    '("INF-53"    . "14_INF-53")
    '("INF-54"    . "14_INF-54")
    '("INF-55"    . "14_INF-55")
    '("INF-56"    . "14_INF-56")
    '("INF-57"    . "14_INF-57")
    '("INF-58"    . "14_INF-58")
    '("INF-59"    . "14_INF-59")
  )
)

;;---------------------------------------------------------------------------
;; Valid Toro INF model numbers (index 2 after TORO-INF)
;;---------------------------------------------------------------------------
(defun LHA:ValidToroINFModel (model)
  (or (= model "34")
      (= model "35")
      (= model "54")
      (= model "55"))
)

;;---------------------------------------------------------------------------
;; Returns T if a string is a single digit 1-9 (dup instance marker)
;;---------------------------------------------------------------------------
(defun LHA:IsDupInstance (str)
  (and (= (strlen str) 1)
       (wcmatch str "[1-9]"))
)

;;---------------------------------------------------------------------------
;; Split a string by a delimiter character
;; Returns a list of strings
;;---------------------------------------------------------------------------
(defun LHA:SplitString (str delim / result current i ch)
  (setq result '()
        current ""
        i 0)
  (while (< i (strlen str))
    (setq ch (substr str (1+ i) 1))
    (if (= ch delim)
      (progn
        (setq result (append result (list current)))
        (setq current ""))
      (setq current (strcat current ch)))
    (setq i (1+ i))
  )
  (setq result (append result (list current)))
  result
)

;;---------------------------------------------------------------------------
;; Return just the first "word" of a string (up to first space)
;;---------------------------------------------------------------------------
(defun LHA:FirstWord (str / i ch result)
  (setq result ""
        i 0)
  (while (and (< i (strlen str))
              (/= (substr str (1+ i) 1) " "))
    (setq result (strcat result (substr str (1+ i) 1)))
    (setq i (1+ i))
  )
  result
)

;;---------------------------------------------------------------------------
;; Parse a block name into a lookup key
;; Returns the key string, or nil if not a recognized head block
;;
;; Rain Bird patterns (nozzle at index 2, or index 3 if dup instance at index 2):
;;   RAIN-702-<nozzle>-...
;;   RAIN-702IC-<nozzle>-...
;;   RAIN-752-<nozzle>-...
;;   RAIN-752IC-<nozzle>-...
;;   RAIN-952-<nozzle>-...
;;   RAIN-952IC-<nozzle>-...
;;
;; Toro INF patterns (model validated, nozzle at index 3, or index 4 if dup at index 3):
;;   TORO-INF-<model>-<nozzle>-...
;;   TORO-INF-<model>-<dup>-<nozzle>-...
;;---------------------------------------------------------------------------
(defun LHA:ParseBlockName (bname / parts brand seg2 seg3 seg4 nozzle key)
  (setq parts (LHA:SplitString (strcase bname) "-"))

  (if (< (length parts) 3)
    (setq key nil)
    (progn
      (setq brand (nth 0 parts)
            seg2  (nth 1 parts)
            seg3  (nth 2 parts))

      (cond

        ;;-------------------------------------------------------------------
        ;; Rain Bird RB7X2: 702, 702IC, 752, 752IC
        ;;-------------------------------------------------------------------
        ((and (= brand "RAIN")
              (or (= seg2 "702") (= seg2 "702IC")
                  (= seg2 "752") (= seg2 "752IC")))
         (if (LHA:IsDupInstance seg3)
           ;; Dup instance at index 2 - nozzle shifts to index 3
           (if (>= (length parts) 4)
             (setq nozzle (LHA:FirstWord (nth 3 parts)))
             (setq nozzle nil))
           ;; Normal - nozzle at index 2
           (setq nozzle (LHA:FirstWord seg3))
         )
         (if nozzle
           (setq key (strcat "RB7X2-" nozzle))
           (setq key nil))
        )

        ;;-------------------------------------------------------------------
        ;; Rain Bird RB952: 952, 952IC
        ;;-------------------------------------------------------------------
        ((and (= brand "RAIN")
              (or (= seg2 "952") (= seg2 "952IC")))
         (if (LHA:IsDupInstance seg3)
           ;; Dup instance at index 2 - nozzle shifts to index 3
           (if (>= (length parts) 4)
             (setq nozzle (LHA:FirstWord (nth 3 parts)))
             (setq nozzle nil))
           ;; Normal - nozzle at index 2
           (setq nozzle (LHA:FirstWord seg3))
         )
         (if nozzle
           (setq key (strcat "RB952-" nozzle))
           (setq key nil))
        )

        ;;-------------------------------------------------------------------
        ;; Toro Infiniti: TORO-INF-<model>-...
        ;; Model (seg3) must be one of: 34, 35, 54, 55
        ;;-------------------------------------------------------------------
        ((and (= brand "TORO") (= seg2 "INF"))
         (if (not (LHA:ValidToroINFModel seg3))
           ;; Unrecognized INF model number
           (progn
             (princ (strcat "\n  WARNING: Unrecognized Toro INF model: " seg3 " in block " bname))
             (setq key nil)
           )
           ;; Valid model - check for dup instance at index 3
           (if (>= (length parts) 4)
             (progn
               (setq seg4 (nth 3 parts))
               (if (LHA:IsDupInstance seg4)
                 ;; Dup instance at index 3 - nozzle shifts to index 4
                 (if (>= (length parts) 5)
                   (setq nozzle (LHA:FirstWord (nth 4 parts)))
                   (setq nozzle nil))
                 ;; Normal - nozzle at index 3
                 (setq nozzle (LHA:FirstWord seg4))
               )
               (if nozzle
                 (setq key (strcat "INF-" nozzle))
                 (setq key nil))
             )
             (setq key nil)
           )
         )
        )

        ;; Not a recognized head block
        (t (setq key nil))
      )
    )
  )
  key
)

;;---------------------------------------------------------------------------
;; Check if a layer exists in the drawing. Returns T if yes, nil if no.
;;---------------------------------------------------------------------------
(defun LHA:LayerExists (layname)
  (if (tblsearch "LAYER" layname) T nil)
)

;;---------------------------------------------------------------------------
;; Assign an entity to a layer
;;---------------------------------------------------------------------------
(defun LHA:SetEntityLayer (ent layname / entdata)
  (setq entdata (entget ent))
  (setq entdata (subst (cons 8 layname) (assoc 8 entdata) entdata))
  (entmod entdata)
  (entupd ent)
)

;;---------------------------------------------------------------------------
;; Prompt user to manually assign a layer for an unrecognized block
;; Highlights the block, shows its name, asks for layer name
;; Returns the layer name entered, or nil to skip
;;---------------------------------------------------------------------------
(defun LHA:PromptUnmatched (ent bname / layname)
  (princ (strcat "\n\n*** UNMATCHED BLOCK: " bname " ***"))
  (princ "\n  Block has been highlighted in the drawing.")
  (redraw ent 3)
  (setq layname
    (getstring (strcat "\n  Enter layer name for [" bname "] (Enter to skip): ")))
  (redraw ent 4)
  (if (= layname "") nil layname)
)

;;---------------------------------------------------------------------------
;; MAIN COMMAND: HEADLAYERS
;;---------------------------------------------------------------------------
(defun c:HEADLAYERS (/ layerTable ss i ent bname key layname
                       matched unmatched skipped total
                       unmatched-log)
  (princ "\n=== HEAD LAYER ASSIGNMENT ===")
  (princ "\nScanning modelspace for Land F/X irrigation head blocks...")

  (setq layerTable    (LHA:GetLayerTable)
        matched       0
        unmatched     0
        skipped       0
        unmatched-log '())

  ;; Select all INSERT (block reference) entities in modelspace
  (setq ss (ssget "_X" '((0 . "INSERT"))))

  (if (not ss)
    (progn
      (princ "\nNo block references found in drawing.")
      (princ)
      (exit)
    )
  )

  (setq total (sslength ss)
        i 0)

  (princ (strcat "\nFound " (itoa total) " block references. Filtering for head blocks...\n"))

  (while (< i total)
    (setq ent   (ssname ss i)
          bname (cdr (assoc 2 (entget ent))))

    ;; Attempt to parse the block name into a layer key
    (setq key (LHA:ParseBlockName bname))

    (if key
      (progn
        ;; Look up the key in the layer table
        (setq layname (cdr (assoc key layerTable)))

        (if layname
          (progn
            ;; Check layer exists before assigning
            (if (LHA:LayerExists layname)
              (progn
                (LHA:SetEntityLayer ent layname)
                (princ (strcat "\n  " bname " -> " layname))
                (setq matched (1+ matched))
              )
              (progn
                (princ (strcat "\n  " bname " -> " layname " [LAYER NOT FOUND - skipped]"))
                (setq skipped (1+ skipped))
                (setq unmatched-log
                  (append unmatched-log
                    (list (strcat bname " (target layer missing: " layname ")"))))
              )
            )
          )
          (progn
            ;; Key parsed OK but not in layer table - prompt user
            (princ (strcat "\n  " bname " -> key=" key " (not in layer table)"))
            (setq layname (LHA:PromptUnmatched ent bname))
            (if layname
              (progn
                (if (LHA:LayerExists layname)
                  (progn
                    (LHA:SetEntityLayer ent layname)
                    (setq matched (1+ matched))
                  )
                  (progn
                    (princ (strcat " [LAYER NOT FOUND - skipped]"))
                    (setq skipped (1+ skipped))
                    (setq unmatched-log
                      (append unmatched-log
                        (list (strcat bname " (manually entered layer missing: " layname ")"))))
                  )
                )
              )
              (progn
                (princ " [SKIPPED]")
                (setq skipped (1+ skipped))
                (setq unmatched-log
                  (append unmatched-log
                    (list (strcat bname " (key: " key ")"))))
              )
            )
          )
        )
      )
      ;; Not a head block pattern at all - silently ignore
    )

    (setq i (1+ i))
  )

  ;; Summary report
  (princ "\n\n=== SUMMARY ===")
  (princ (strcat "\n  Blocks processed : " (itoa total)))
  (princ (strcat "\n  Layers assigned  : " (itoa matched)))
  (princ (strcat "\n  Left unchanged   : " (itoa skipped)))

  (if unmatched-log
    (progn
      (princ "\n\n  Blocks left unchanged (investigate these):")
      (foreach item unmatched-log
        (princ (strcat "\n    - " item))
      )
    )
  )

  (princ "\n\nDone. Use REGEN to refresh display if needed.")
  (princ)
)

(princ "\nHeadLayerAssign.lsp loaded. Type HEADLAYERS to run.")
(princ)
