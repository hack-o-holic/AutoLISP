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
;; Command: HEADLAYERS
;;
;; Workflow:
;;   1. Run HEADLAYERS (optionally pre-select heads first).
;;   2. Choose scope in the dialog, click Scan.
;;   3. Review matched/unmatched lists, then click Apply.
;;

;;---------------------------------------------------------------------------
;; Module-level state — reset at the start of each command run
;;---------------------------------------------------------------------------
(setq *LHA:MatchedData*   nil)  ; list of (ent layname bname)  — one entry per entity
(setq *LHA:UnmatchedData* nil)  ; list of (bname reason)
(setq *LHA:RawImpliedSS*  nil)  ; ssget "I" captured before dialog opens

;; Capture this file's directory at load time.
;; When AutoCAD loads a LSP (even via full path), the directory is temporarily
;; in the search path, so findfile succeeds here.  This lets the command locate
;; the DCL later without requiring the folder to be in the persistent Support
;; File Search Path.
(setq *LHA:LoadDir*
  (vl-filename-directory (findfile "HeadLayerAssign.lsp")))

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
           (if (>= (length parts) 4)
             (setq nozzle (LHA:FirstWord (nth 3 parts)))
             (setq nozzle nil))
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
           (if (>= (length parts) 4)
             (setq nozzle (LHA:FirstWord (nth 3 parts)))
             (setq nozzle nil))
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
           (setq key nil)
           (if (>= (length parts) 4)
             (progn
               (setq seg4 (nth 3 parts))
               (if (LHA:IsDupInstance seg4)
                 (if (>= (length parts) 5)
                   (setq nozzle (LHA:FirstWord (nth 4 parts)))
                   (setq nozzle nil))
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
;; Check if a layer exists in the drawing
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
;; LHA:IncrCount
;; Increments the counter for KEY in an alist, or adds a new entry (count=1).
;; Returns the updated alist.
;;---------------------------------------------------------------------------
(defun LHA:IncrCount (alist key / entry)
  (setq entry (assoc key alist))
  (if entry
    (subst (cons key (1+ (cdr entry))) entry alist)
    (append alist (list (cons key 1)))
  )
)

;;---------------------------------------------------------------------------
;; LHA:DoScan
;; Called from the Scan button action_tile.
;; Reads the scope radio, scans blocks, populates both list boxes and the
;; status text, and enables/disables the Apply button.
;;---------------------------------------------------------------------------
(defun LHA:DoScan (/ layerTable scope ss i ent ed bname key layname reason
                     matched-counts unmatched-counts already-counts
                     matchCount unmatchCount alreadyCount dkey)

  (setq layerTable        (LHA:GetLayerTable)
        scope             (get_tile "scope_all")   ; "1" = all modelspace
        *LHA:MatchedData*   nil
        *LHA:UnmatchedData* nil
        matched-counts    nil
        unmatched-counts  nil
        already-counts    nil
        matchCount        0
        unmatchCount      0
        alreadyCount      0)

  ;; Build the selection set based on chosen scope
  (setq ss
    (if (= scope "1")
      (ssget "_X" '((0 . "INSERT")))
      *LHA:RawImpliedSS*
    )
  )

  (if (not ss)
    ;; Nothing to scan
    (progn
      (start_list "matched_list")   (end_list)
      (start_list "unmatched_list") (end_list)
      (start_list "already_list")   (end_list)
      (set_tile "status_text" "No block references found.")
      (mode_tile "accept" 1)
    )

    ;; Scan loop
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i)
              ed  (entget ent)
              i   (1+ i))

        ;; Only process INSERT entities (implied ss may contain mixed types)
        (if (= "INSERT" (cdr (assoc 0 ed)))
          (progn
            (setq bname (cdr (assoc 2 ed))
                  key   (LHA:ParseBlockName bname))

            (if key
              ;; Recognized head block pattern — look up target layer
              (progn
                (setq layname (cdr (assoc key layerTable)))
                (cond

                  ;; Layer entry found AND layer exists in drawing
                  ((and layname (LHA:LayerExists layname))
                   (setq dkey (strcat bname " | " layname))
                   (if (= (cdr (assoc 8 ed)) layname)
                     ;; Already on the correct layer — informational only
                     (progn
                       (setq already-counts (LHA:IncrCount already-counts dkey))
                       (setq alreadyCount (1+ alreadyCount))
                     )
                     ;; On a different layer — needs to be moved
                     (progn
                       (setq matched-counts (LHA:IncrCount matched-counts dkey))
                       (setq *LHA:MatchedData*
                         (append *LHA:MatchedData* (list (list ent layname bname))))
                       (setq matchCount (1+ matchCount))
                     )
                   )
                  )

                  ;; Layer entry found but the layer is missing in this drawing
                  ((and layname (not (LHA:LayerExists layname)))
                   (setq reason (strcat "layer not in drawing: " layname))
                   (setq dkey (strcat bname " | " reason))
                   (setq unmatched-counts (LHA:IncrCount unmatched-counts dkey))
                   (setq *LHA:UnmatchedData*
                     (append *LHA:UnmatchedData* (list (list bname reason))))
                   (setq unmatchCount (1+ unmatchCount))
                  )

                  ;; Key parsed but not found in layer table
                  (T
                   (setq reason (strcat "key not in table: " key))
                   (setq dkey (strcat bname " | " reason))
                   (setq unmatched-counts (LHA:IncrCount unmatched-counts dkey))
                   (setq *LHA:UnmatchedData*
                     (append *LHA:UnmatchedData* (list (list bname reason))))
                   (setq unmatchCount (1+ unmatchCount))
                  )
                )
              )
              ;; key=nil: not a head block at all — silently skip
            )
          )
          ;; Not an INSERT — skip (only relevant for implied selection scope)
        )
      )

      ;; Populate "to assign" list — grouped by block/layer pair, with counts
      (start_list "matched_list")
      (if matched-counts
        (foreach item matched-counts
          (setq dkey (car item)
                cnt  (cdr item))
          (add_list (if (> cnt 1) (strcat "(" (itoa cnt) "x)  " dkey) dkey))
        )
        (add_list "(none)")
      )
      (end_list)

      ;; Populate unmatched list — grouped with counts
      (start_list "unmatched_list")
      (if unmatched-counts
        (foreach item unmatched-counts
          (setq dkey (car item)
                cnt  (cdr item))
          (add_list (if (> cnt 1) (strcat "(" (itoa cnt) "x)  " dkey) dkey))
        )
        (add_list "(none)")
      )
      (end_list)

      ;; Populate "already correct" list — grouped with counts
      (start_list "already_list")
      (if already-counts
        (foreach item already-counts
          (setq dkey (car item)
                cnt  (cdr item))
          (add_list (if (> cnt 1) (strcat "(" (itoa cnt) "x)  " dkey) dkey))
        )
        (add_list "(none)")
      )
      (end_list)

      ;; Update status and Apply button state
      (set_tile "status_text"
        (strcat "Scan complete:  "
                (itoa matchCount) " to assign,  "
                (itoa alreadyCount) " already correct,  "
                (itoa unmatchCount) " unmatched."))
      (mode_tile "accept" (if (> matchCount 0) 0 1))
    )
  )
)

;;---------------------------------------------------------------------------
;; LHA:ApplyChanges
;; Called after the dialog closes with result=1 (Apply).
;; Iterates *LHA:MatchedData* and commits layer assignments.
;;---------------------------------------------------------------------------
(defun LHA:ApplyChanges (/ item ent layname bname assigned skipped)
  (princ "\n=== APPLYING LAYER ASSIGNMENTS ===")
  (setq assigned 0
        skipped  0)

  (foreach item *LHA:MatchedData*
    (setq ent     (nth 0 item)
          layname (nth 1 item)
          bname   (nth 2 item))
    ;; Re-verify layer exists (user could have modified drawing between Scan and Apply)
    (if (LHA:LayerExists layname)
      (progn
        (LHA:SetEntityLayer ent layname)
        (princ (strcat "\n  " bname " -> " layname))
        (setq assigned (1+ assigned))
      )
      (progn
        (princ (strcat "\n  SKIPPED (layer not found): " bname " -> " layname))
        (setq skipped (1+ skipped))
      )
    )
  )

  (princ (strcat "\n\nDone.  Assigned: " (itoa assigned)))
  (if (> skipped 0)
    (princ (strcat "   Skipped: " (itoa skipped)))
  )

  (if *LHA:UnmatchedData*
    (progn
      (princ "\n\nUnmatched blocks (investigate these):")
      (foreach item *LHA:UnmatchedData*
        (princ (strcat "\n  " (car item) " - " (cadr item)))
      )
    )
  )

  (princ "\n\nUse REGEN to refresh display if needed.")
)

;;---------------------------------------------------------------------------
;; LHA:ScanOrSelect
;; Called from the Scan button action_tile.
;; If "Current Selection" scope is active but no selection has been captured,
;; nudges the user to click Select... first.  Otherwise runs the scan.
;;---------------------------------------------------------------------------
(defun LHA:ScanOrSelect ()
  (if (and (= (get_tile "scope_all") "0") (not *LHA:RawImpliedSS*))
    (set_tile "status_text" "Click  Select...  to choose heads first, then Scan.")
    (LHA:DoScan)
  )
)

;;---------------------------------------------------------------------------
;; MAIN COMMAND: HEADLAYERS
;;---------------------------------------------------------------------------
(defun c:HEADLAYERS (/ dcl-path dcl-id result auto-scan)
  (princ "\n=== HEAD LAYER ASSIGNMENT ===")

  (setq *LHA:RawImpliedSS* nil
        *LHA:MatchedData*   nil
        *LHA:UnmatchedData* nil)

  ;; Locate the DCL file — try search path first, then fall back to load directory
  (setq dcl-path
    (cond
      ((findfile "HeadLayerAssign.dcl"))
      (*LHA:LoadDir*
       (strcat *LHA:LoadDir* "\\HeadLayerAssign.dcl"))
      (T nil)
    )
  )
  (cond
    ((not dcl-path)
     (princ "\nERROR: HeadLayerAssign.dcl not found.")
     (exit)
    )
    (T
     (setq dcl-id (load_dialog dcl-path))
     (if (< dcl-id 0)
       (progn
         (princ "\nERROR: Failed to load HeadLayerAssign.dcl")
         (exit)
       )
       (progn
         ;; ---- Dialog loop -----------------------------------------------
         ;; result=3 : user clicked Select... — prompt for selection then reopen
         ;; result=1 : Apply
         ;; result=0 : Cancel
         (setq result    3
               auto-scan nil)
         (while (= result 3)
           (if (not (new_dialog "HeadLayerAssign" dcl-id))
             (progn
               (princ "\nERROR: Failed to create dialog HeadLayerAssign")
               (setq result 0)
             )
             (progn
               ;; ---- Initialize controls ---------------------------------

               ;; Scope radios and Select... button state
               (cond
                 ;; Have a selection from a prior Select... pick
                 (*LHA:RawImpliedSS*
                  (set_tile "scope_all" "0")
                  (set_tile "scope_sel" "1")
                  (mode_tile "sel_btn"  0)
                  (mode_tile "scan_btn" 1)
                  (set_tile "status_text"
                    (strcat (itoa (sslength *LHA:RawImpliedSS*))
                            " heads selected.  Click Scan or Select... to re-select."))
                 )
                 ;; No selection yet — default to All Modelspace
                 (T
                  (set_tile "scope_all" "1")
                  (mode_tile "sel_btn" 1)
                  (set_tile "status_text" "Choose scope and click Scan or Select...")
                 )
               )

               ;; Empty list boxes
               (start_list "matched_list")   (end_list)
               (start_list "unmatched_list") (end_list)
               (start_list "already_list")   (end_list)

               ;; Apply starts disabled — LHA:DoScan enables it when ready
               (mode_tile "accept" 1)

               ;; ---- Wire action tiles -----------------------------------
               ;; scope_all: disable Select..., re-enable Scan
               (action_tile "scope_all"
                 "(mode_tile \"sel_btn\" 1) (mode_tile \"scan_btn\" 0)")
               ;; scope_sel: enable Select..., Scan is never used in this mode
               (action_tile "scope_sel"
                 "(mode_tile \"sel_btn\" 0) (mode_tile \"scan_btn\" 1)")
               ;; Select... closes dialog so user can pick heads interactively
               (action_tile "sel_btn"   "(done_dialog 3)")
               ;; Scan — warns if Current Selection chosen but nothing picked yet
               (action_tile "scan_btn"  "(LHA:ScanOrSelect)")
               (action_tile "accept"    "(done_dialog 1)")
               (action_tile "cancel"    "(done_dialog 0)")

               ;; ---- Auto-scan after a Select... pick --------------------
               ;; Runs between new_dialog and start_dialog — all tile ops are valid here
               (if (and auto-scan *LHA:RawImpliedSS*)
                 (LHA:DoScan)
               )

               ;; ---- Run dialog ------------------------------------------
               (setq result (start_dialog))

               ;; ---- Handle Select... request ----------------------------
               ;; Dialog is now closed; prompt for interactive selection
               (if (= result 3)
                 (progn
                   (setq *LHA:RawImpliedSS* (ssget '((0 . "INSERT"))))
                   (setq auto-scan T)
                 )
               )
             )
           )
         )

         (unload_dialog dcl-id)

         ;; ---- Post-dialog -----------------------------------------------
         (if (= result 1)
           (LHA:ApplyChanges)
           (princ "\nCancelled.")
         )
       )
     )
    )
  )
  (princ)
)

(princ "\nHeadLayerAssign.lsp loaded. Type HEADLAYERS to run.")
(princ)
