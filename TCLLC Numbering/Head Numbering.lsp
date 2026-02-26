;;; ========================================================================
;;; HEAD NUMBERING ROUTINE
;;; ========================================================================
;;; Interactive head numbering with counter persistence and smart renumbering
;;; 
;;; USAGE: NUMBERHEADS
;;; 
;;; Features:
;;; - DCL dialog for hole/location selection
;;; - Persistent counters stored in drawing dictionary
;;; - Two-click workflow: select head, place tag
;;; - Manual number override with collision detection
;;; - Smart renumbering when inserting gaps
;;; - Mid-stream settings change
;;; ========================================================================

;;; ------------------------------------------------------------------------
;;; CONFIGURATION
;;; ------------------------------------------------------------------------

(setq *HN-Locations*     '("TE" "FW" "FP" "RG" "GR" "GP"))
(setq *HN-MaxHole*       27)
(setq *HN-DIR*           (vl-filename-directory (findfile "Head Numbering.lsp")))
(if (not *HN-LastHole*)     (setq *HN-LastHole*     nil))
(if (not *HN-LastLocation*) (setq *HN-LastLocation* nil))

;;; ------------------------------------------------------------------------
;;; DICTIONARY MANAGEMENT FUNCTIONS
;;; ------------------------------------------------------------------------

(defun GetCounterDict (/ dict-main dict-counters)
  "Get or create the HEADNUM counter dictionary"
  (setq dict-main (namedobjdict))
  (if (not (dictsearch dict-main "HEADNUM_COUNTERS"))
    (progn
      (setq dict-counters (entmakex '((0 . "DICTIONARY") (100 . "AcDbDictionary"))))
      (dictadd dict-main "HEADNUM_COUNTERS" dict-counters)
    )
    (setq dict-counters (cdr (assoc -1 (dictsearch dict-main "HEADNUM_COUNTERS"))))
  )
  dict-counters
)

(defun GetCounter (hole location / dict-counters key xrec data value)
  "Get current counter value for hole/location combo"
  (setq dict-counters (GetCounterDict))
  (setq key (strcat (itoa hole) "_" location))
  
  (if (setq xrec (dictsearch dict-counters key))
    (progn
      (setq data (cdr (assoc -1 xrec)))
      (setq value (cdr (assoc 1 (entget data))))
      (if value
        (atoi value)
        0
      )
    )
    0  ; Return 0 if counter doesn't exist yet
  )
)

(defun SetCounter (hole location value / dict-counters key xrec old-data)
  "Set counter value for hole/location combo"
  (setq dict-counters (GetCounterDict))
  (setq key (strcat (itoa hole) "_" location))
  
  ; If counter exists, delete it first
  (if (setq xrec (dictsearch dict-counters key))
    (progn
      (setq old-data (cdr (assoc -1 xrec)))
      (entdel old-data)
      (dictremove dict-counters key)
    )
  )
  
  ; Create new counter
  (setq new-data (entmakex (list (cons 0 "XRECORD") (cons 100 "AcDbXrecord") (cons 1 (itoa value)))))
  (dictadd dict-counters key new-data)
  
  value
)

(defun SyncCounter (hole location / tags max-num)
  "Recalculate the counter from actual tags in the drawing. Returns the new max."
  (setq tags (GetAllHeadTags hole location))
  (setq max-num 0)
  (foreach tag tags
    (if (> (cadr tag) max-num)
      (setq max-num (cadr tag))
    )
  )
  (SetCounter hole location max-num)
  max-num
)

(defun NextAvailable (hole location start / tags nums n)
  "Return the first number >= start that has no existing tag."
  (setq tags (GetAllHeadTags hole location))
  (setq nums (mapcar 'cadr tags))
  (setq n start)
  (while (member n nums)
    (setq n (1+ n))
  )
  n
)

;;; ------------------------------------------------------------------------
;;; TAG SEARCH AND MANIPULATION FUNCTIONS
;;; ------------------------------------------------------------------------

(defun HeadHasTag (head-ent / head-handle ss i tag-ent xdata xdata-app xdata-vals handle-pair found)
  "Check if a head block already has a tag linked to it"
  (setq head-handle (cdr (assoc 5 (entget head-ent))))
  (setq found nil)
  
  ; Search all LAFX-TAG-SQUARE-999 blocks
  (if (setq ss (ssget "_X" (list '(0 . "INSERT") '(2 . "LAFX-TAG-SQUARE-999"))))
    (progn
      (setq i 0)
      (while (and (< i (sslength ss)) (not found))
        (setq tag-ent (ssname ss i))
        
        ; Get XDATA
        (if (setq xdata (assoc -3 (entget tag-ent '("HEADNUM"))))
          (progn
            (setq xdata-app (assoc "HEADNUM" (cdr xdata)))
            (if xdata-app
              (progn
                ; Look for 1005 group code (handle reference)
                (setq xdata-vals (cdr xdata-app))
                (if (setq handle-pair (assoc 1005 xdata-vals))
                  (if (equal (cdr handle-pair) head-handle)
                    (setq found tag-ent)
                  )
                )
              )
            )
          )
        )
        
        (setq i (1+ i))
      )
    )
  )
  
  found  ; Return the tag entity if found, nil otherwise
)

(defun GetAllHeadTags (hole location / ss i ent xdata tags hole-str loc-str num-str valid-ent)
  "Get all LAFX-TAG-SQUARE-999 blocks for specific hole/location combo"
  (setq tags '())
  
  (if (setq ss (ssget "_X" (list '(0 . "INSERT") '(2 . "LAFX-TAG-SQUARE-999"))))
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        
        ; Verify entity still exists
        (if (and ent (setq valid-ent (entget ent)))
          (if (setq xdata (assoc -3 (entget ent '("HEADNUM"))))
            (progn
              (setq xdata (cdr xdata))
              (setq xdata (cdr (assoc "HEADNUM" xdata)))
              
              ; Extract hole, location, number from XDATA
              (setq hole-str (cdr (nth 0 xdata)))
              (setq loc-str (cdr (nth 1 xdata)))
              (setq num-str (cdr (nth 2 xdata)))
              
              ; Check if matches our hole/location
              (if (and (= (atoi hole-str) hole) (= loc-str location))
                (setq tags (append tags (list (list ent (atoi num-str)))))
              )
            )
          )
        )
        (setq i (1+ i))
      )
    )
  )
  tags  ; Returns list of (entity_name . number) pairs
)

(defun TagExists (hole location number / tags)
  "Check if a specific tag number exists"
  (setq tags (GetAllHeadTags hole location))
  (vl-some '(lambda (x) (= (cadr x) number)) tags)
)

(defun RenumberTags (hole location start-num / tags-to-renumber tag ent num new-num xdata highest-num)
  "Renumber all tags >= start-num by incrementing by 1"
  (setq tags-to-renumber '())
  (setq highest-num 0)
  
  ; Get all tags for this hole/location
  (setq tags-to-renumber (GetAllHeadTags hole location))
  
  ; Filter for tags >= start-num
  (setq tags-to-renumber 
    (vl-remove-if '(lambda (x) (< (cadr x) start-num)) tags-to-renumber)
  )
  
  ; Sort by number (highest first to avoid conflicts)
  (setq tags-to-renumber (vl-sort tags-to-renumber '(lambda (a b) (> (cadr a) (cadr b)))))
  
  (if tags-to-renumber
    (progn
      (princ (strcat "\nRenumbering " (itoa (length tags-to-renumber)) " tags..."))
      
      (foreach tag tags-to-renumber
        (setq ent (car tag))
        (setq num (cadr tag))
        (setq new-num (1+ num))
        
        ; Track highest number
        (if (> new-num highest-num)
          (setq highest-num new-num)
        )
        
        ; Update the tag
        (UpdateTagNumber ent hole location new-num (GetHeadHandleFromTag ent))
        
        (princ (strcat "\n  " (itoa num) " -> " (itoa new-num)))
      )
      
      (princ (strcat "\nRenumbered " (itoa (length tags-to-renumber)) " tags."))
      
      ; Update counter to new highest
      (SetCounter hole location highest-num)
      (princ (strcat "\nCounter updated to: " (itoa highest-num)))
      
      T
    )
    nil
  )
)

(defun FillGaps (hole location / tags sorted i ent)
  "Renumber all tags in hole/location sequentially 1,2,3... eliminating all gaps"
  (setq tags (GetAllHeadTags hole location))
  (if (not tags)
    (progn (princ "\nFillGaps: no tags found.") nil)
    (progn
      (setq sorted (vl-sort tags '(lambda (a b) (< (cadr a) (cadr b)))))
      (setq i 1)
      (foreach tag sorted
        (setq ent (car tag))
        (UpdateTagNumber ent hole location i (GetHeadHandleFromTag ent))
        (setq i (1+ i))
      )
      (SetCounter hole location (1- i))
      (princ (strcat "\nFillGaps: renumbered " (itoa (length sorted)) " tags."))
      (length sorted)
    )
  )
)

(defun GetHeadHandleFromTag (tag-ent / xdata)
  "Extract linked head handle from tag XDATA"
  (if (setq xdata (assoc -3 (entget tag-ent '("HEADNUM"))))
    (progn
      (setq xdata (cdr xdata))
      (setq xdata (cdr (assoc "HEADNUM" xdata)))
      (cdr (nth 3 xdata))  ; 4th item is the handle
    )
    nil
  )
)

;;; ------------------------------------------------------------------------
;;; TAG CREATION AND UPDATE FUNCTIONS
;;; ------------------------------------------------------------------------

(defun UpdateTagNumber (tag-ent hole location number head-handle / ent-data att-ent att-data new-text temp-data xdata-list)
  "Update tag block's attribute and XDATA"
  
  ; Format the display text with padded hole number
  (setq new-text (strcat (FormatHoleNumber hole) location (FormatNumber number)))
  
  ; Update attribute
  (setq ent-data (entget tag-ent))
  (setq att-ent (entnext tag-ent))
  
  (while (and att-ent (= "ATTRIB" (cdr (assoc 0 (setq att-data (entget att-ent))))))
    (if (= "XX" (cdr (assoc 2 att-data)))
      (progn
        (entmod (subst (cons 1 new-text) (assoc 1 att-data) att-data))
        (entupd att-ent)
      )
    )
    (setq att-ent (entnext att-ent))
  )
  
  ; Update HEADNUM and LandFX XDATA
  (regapp "HEADNUM")
  (regapp "LandFX")
  
  (setq ent-data (entget tag-ent '("HEADNUM" "LandFX")))
  
  ; Remove existing XDATA
  (setq ent-data (vl-remove-if '(lambda (x) (= (car x) -3)) ent-data))
  
  ; Build HEADNUM and LandFX XDATA
  (setq ent-data 
    (append ent-data 
      (list 
        (list -3 
          (list "HEADNUM"
            (cons 1000 (itoa hole))
            (cons 1000 location)
            (cons 1000 (FormatNumber number))
            (cons 1005 head-handle)
          )
          (list "LandFX"
            '(1000 . "VALVECALLOUT")
            (cons 1005 head-handle)
          )
        )
      )
    )
  )
  
  (entmod ent-data)
  (entupd tag-ent)
  
  T
)

(defun ClassifySelection (ent / xdata lfx-data first-1000)
  "Classify a selected entity based on LandFX XDATA first 1000 field.
   Returns: 'HEAD, 'OURTAG, 'LFXTAG, or 'UNKNOWN"
  (setq xdata (entget ent '("HEADNUM" "LandFX")))
  (setq lfx-data (assoc "LandFX" (cdr (assoc -3 xdata))))
  
  (if lfx-data
    (progn
      ; Get first 1000 string value
      (setq first-1000 (cdr (assoc 1000 (cdr lfx-data))))
      (cond
        ; It's a head block
        ((= first-1000 "Head") 'HEAD)
        ; It's a tag (ours or LandFX) - check for HEADNUM to distinguish
        ((= first-1000 "VALVECALLOUT")
          (if (assoc "HEADNUM" (cdr (assoc -3 xdata)))
            'OURTAG
            'LFXTAG
          )
        )
        (T 'UNKNOWN)
      )
    )
    'UNKNOWN
  )
)

(defun ReplaceHeadTag (existing-tag hole location number head-ent
                       / xdata xdata-vals old-hole old-loc old-num old-label new-label
                         head-handle response collision-response
                         manual-settings m-hole m-loc m-num m-label)
  "Renumber an existing tag in place - highlight it, prompt to confirm, check collisions.
   Returns T on Yes renumber, 'MANUAL on manual reassign, nil on skip/cancel."

  ; Get current tag info from XDATA
  (setq xdata (assoc -3 (entget existing-tag '("HEADNUM"))))
  (setq xdata-vals (cdr (assoc "HEADNUM" (cdr xdata))))
  (setq old-hole (atoi (cdr (nth 0 xdata-vals))))
  (setq old-loc  (cdr (nth 1 xdata-vals)))
  (setq old-num  (atoi (cdr (nth 2 xdata-vals))))
  (setq old-label (strcat (FormatHoleNumber old-hole) old-loc (FormatNumber old-num)))
  (setq new-label (strcat (FormatHoleNumber hole) location (FormatNumber number)))

  ; Highlight the existing tag
  (redraw existing-tag 3)
  (princ (strcat "\n  Currently numbered: " old-label))

  ; Prompt to renumber
  (initget "Yes No Manual")
  (setq response (getkword (strcat "\n  Renumber to " new-label "? [Yes/No/Manual] <No>: ")))

  ; Unhighlight
  (redraw existing-tag 4)

  ; nil means user pressed Enter = default No
  (if (not response) (setq response "No"))

  (cond

    ; -------------------------------------------------------
    ; YES - renumber to current session hole/location/number
    ; -------------------------------------------------------
    ((= response "Yes")
      ; Check if new number already exists on a DIFFERENT tag
      (if (TagExists hole location number)
        (progn
          (princ (strcat "\nWARNING: " new-label " already exists!"))
          (initget "Yes No")
          (setq collision-response (getkword "\nRenumber existing tags to make room? [Yes/No] <No>: "))
          (if (= collision-response "Yes")
            (RenumberTags hole location number)
            (progn
              (princ "\nRenumber cancelled.")
              (setq response nil)
            )
          )
        )
      )
      (if response
        (progn
          (setq head-handle (cdr (assoc 5 (entget head-ent))))
          (UpdateTagNumber existing-tag hole location number head-handle)
          ; If tag moved out of its old group, sync that group's counter too
          (if (or (/= old-hole hole) (/= old-loc location))
            (SyncCounter old-hole old-loc)
          )
          (princ (strcat "\nRenumbered: " old-label " -> " new-label))
          T
        )
        nil
      )
    )

    ; -------------------------------------------------------
    ; MANUAL - open dialog to pick a different hole/location
    ; -------------------------------------------------------
    ((= response "Manual")
      (setq manual-settings (ShowSettingsDialog old-hole old-loc))
      (if manual-settings
        (progn
          (setq m-hole (car manual-settings))
          (setq m-loc  (cadr manual-settings))
          (setq m-num  (if (caddr manual-settings)
                         (caddr manual-settings)
                         (1+ (GetCounter m-hole m-loc))))
          (setq m-label (strcat (FormatHoleNumber m-hole) m-loc (FormatNumber m-num)))
          ; Check collision at manual target
          (if (TagExists m-hole m-loc m-num)
            (progn
              (princ (strcat "\nWARNING: " m-label " already exists!"))
              (initget "Yes No")
              (setq collision-response (getkword "\nRenumber existing tags to make room? [Yes/No] <No>: "))
              (if (= collision-response "Yes")
                (RenumberTags m-hole m-loc m-num)
                (progn
                  (princ "\nManual reassign cancelled.")
                  (setq manual-settings nil)
                )
              )
            )
          )
          (if manual-settings
            (progn
              (setq head-handle (cdr (assoc 5 (entget head-ent))))
              (UpdateTagNumber existing-tag m-hole m-loc m-num head-handle)
              (if (> m-num (GetCounter m-hole m-loc))
                (SetCounter m-hole m-loc m-num))
              (princ (strcat "\nReassigned: " old-label " -> " m-label))
              'MANUAL
            )
            nil
          )
        )
        (progn (princ "\nManual reassign cancelled.") nil)
      )
    )

    ; -------------------------------------------------------
    ; NO - skip
    ; -------------------------------------------------------
    (T
      (princ "\nSkipping.")
      nil
    )
  )
)

(defun CreateHeadTag (hole location number head-ent / old-attreq old-layer tag-ent att-ent att-data ent-data xdata-list)
  "Create a new LAFX-TAG-SQUARE-999 block - INSERT pauses for user to place it"
  
  ; Verify block exists
  (if (not (tblsearch "BLOCK" "LAFX-TAG-SQUARE-999"))
    (progn
      (alert "ERROR: LAFX-TAG-SQUARE-999 block not found in drawing!\nPlease load the block and try again.")
      (exit)
    )
  )
  
  ; Verify layer exists
  (if (not (tblsearch "LAYER" "LI-VALV-ANNO"))
    (progn
      (alert "ERROR: Layer LI-VALV-ANNO not found in drawing!\nPlease create this layer and try again.")
      (exit)
    )
  )
  
  ; Save current layer and switch to LI-VALV-ANNO
  (setq old-layer (getvar "CLAYER"))
  (setvar "CLAYER" "LI-VALV-ANNO")
  
  ; Save and disable attribute prompting
  (setq old-attreq (getvar "ATTREQ"))
  (setvar "ATTREQ" 0)
  
  ; Insert block - PAUSE lets the user place it with native drag-on-cursor behavior
  (command "_.INSERT" "LAFX-TAG-SQUARE-999" pause "" "" "")
  
  ; Restore settings
  (setvar "ATTREQ" old-attreq)
  (setvar "CLAYER" old-layer)
  
  ; Get the inserted block
  (setq tag-ent (entlast))
  
  ; STEP 1: Update attribute value
  (setq att-ent (entnext tag-ent))
  (while (and att-ent (= "ATTRIB" (cdr (assoc 0 (setq att-data (entget att-ent))))))
    (if (= "XX" (cdr (assoc 2 att-data)))
      (progn
        (entmod (subst 
          (cons 1 (strcat (FormatHoleNumber hole) location (FormatNumber number)))
          (assoc 1 att-data)
          att-data
        ))
        (entupd att-ent)
      )
    )
    (setq att-ent (entnext att-ent))
  )
  
  ; Force AutoCAD to commit attribute changes before touching XDATA
  (entupd tag-ent)
  
  ; STEP 2: Write XDATA - fresh entget AFTER attribute is committed
  (regapp "HEADNUM")
  (regapp "LandFX")
  
  (setq head-handle (cdr (assoc 5 (entget head-ent))))
  
  ; Get fresh entity data (no stale attribute state)
  (setq ent-data (entget tag-ent '("HEADNUM" "LandFX")))
  
  ; Strip any existing XDATA
  (setq ent-data (vl-remove-if '(lambda (x) (= (car x) -3)) ent-data))
  
  ; Append HEADNUM and LandFX XDATA to tag block
  (setq ent-data 
    (append ent-data 
      (list 
        (list -3 
          (list "HEADNUM"
            (cons 1000 (itoa hole))
            (cons 1000 location)
            (cons 1000 (FormatNumber number))
            (cons 1005 head-handle)
          )
          (list "LandFX"
            '(1000 . "VALVECALLOUT")
            (cons 1005 head-handle)
          )
        )
      )
    )
  )
  
  (entmod ent-data)
  (entupd tag-ent)
  
  ; Get tag handle now that it's committed
  (setq tag-handle (cdr (assoc 5 (entget tag-ent))))
  
  ; Write 1005 back-reference to head block's LandFX XDATA
  ; Surgically remove only: nil (dead) references and existing LAFX-TAG* callouts
  ; Preserve everything else: pipes, arcs, circles, unknown types
  (setq head-data (entget head-ent '("LandFX")))
  (if (setq lfx-xdata (assoc -3 head-data))
    (progn
      (setq clean-lfx-app
        (cons (car (cadr lfx-xdata))  ; preserve app name "LandFX"
          (vl-remove-if 
            '(lambda (x) 
               (and (= (car x) 1005)
                 (or
                   ; Remove dead references
                   (not (handent (cdr x)))
                   ; Remove existing callout tag references
                   (and (handent (cdr x))
                        (= "INSERT" (cdr (assoc 0 (entget (handent (cdr x))))))
                        (wcmatch (cdr (assoc 2 (entget (handent (cdr x))))) "LAFX-TAG*")
                   )
                 )
               )
            )
            (cdr (cadr lfx-xdata))
          )
        )
      )
      ; Append our fresh 1005
      (setq clean-lfx-app (append clean-lfx-app (list (cons 1005 tag-handle))))
      
      ; Write back
      (setq head-data
        (subst
          (cons -3 (list clean-lfx-app))
          lfx-xdata
          head-data
        )
      )
      (entmod head-data)
      (entupd head-ent)
    )
  )
  
  tag-ent
)

;;; ------------------------------------------------------------------------
;;; UTILITY FUNCTIONS
;;; ------------------------------------------------------------------------

(defun MakeRange (start end / result i)
  "Create a list of integers from start to end (inclusive)"
  (setq result '())
  (setq i start)
  (while (<= i end)
    (setq result (append result (list i)))
    (setq i (1+ i))
  )
  result
)

(defun FormatHoleNumber (num)
  "Format hole number with leading zero (2 digits)"
  (if (< num 10)
    (strcat "0" (itoa num))
    (itoa num)
  )
)

(defun FormatNumber (num)
  "Format number with leading zeros (3 digits)"
  (cond
    ((< num 10) (strcat "00" (itoa num)))
    ((< num 100) (strcat "0" (itoa num)))
    (T (itoa num))
  )
)

;;; ------------------------------------------------------------------------
;;; DIALOG FUNCTIONS
;;; ------------------------------------------------------------------------

(defun BuildTagListItems (hole location / tags sorted nums i expected gaps items num label)
  "Build a list of strings for the dialog list box, including gap markers"
  (setq tags (GetAllHeadTags hole location))
  (setq items '())
  
  (if (not tags)
    (setq items (list "  (none)"))
    (progn
      ; Sort tags by number ascending
      (setq sorted (vl-sort tags '(lambda (a b) (< (cadr a) (cadr b)))))
      (setq nums (mapcar 'cadr sorted))
      
      ; Walk sequence finding gaps
      (setq expected 1)
      (foreach num nums
        ; Add gap entries for any skipped numbers
        (while (< expected num)
          (setq items (append items 
            (list (strcat (FormatHoleNumber hole) location (FormatNumber expected) "  -- GAP"))
          ))
          (setq expected (1+ expected))
        )
        ; Add the actual tag entry
        (setq items (append items
          (list (strcat (FormatHoleNumber hole) location (FormatNumber num)))
        ))
        (setq expected (1+ num))
      )
    )
  )
  items
)


(defun RefreshTagList (hole location / items)
  "Repopulate the tag list box for the given hole/location"
  (setq items (BuildTagListItems hole location))
  (start_list "tag_list")
  (mapcar 'add_list items)
  (end_list)
  items
)

(defun HN-RefreshStatus (hole location / tags nums max-num gap-count next-num)
  "Update gap_label and next_label from actual tags in the drawing (ignores stored counter)."
  (setq tags (GetAllHeadTags hole location))
  (if (null tags)
    (progn
      (setq gap-count 0)
      (setq next-num  1)
    )
    (progn
      (setq nums     (mapcar 'cadr tags))
      (setq max-num  (apply 'max nums))
      (setq gap-count (- max-num (length nums)))
      (setq next-num  (NextAvailable hole location (1+ max-num)))
    )
  )
  (set_tile "gap_label"
    (cond
      ((= gap-count 0) "")
      ((= gap-count 1) "** Gap Exists **")
      (T               "** Gaps Exist **")
    )
  )
  (set_tile "next_label"
    (strcat "Auto next: " (FormatHoleNumber hole) location (FormatNumber next-num)))
  (mode_tile "fill_gaps_btn" (if (> gap-count 0) 0 1))
)

(defun HN-ToggleSprayers (/ layers doc layer-obj first-obj turn-on)
  "Toggle LI-COVR-ROTR and LI-COVR-SPRY on/off together."
  (vl-load-com)
  (setq layers '("LI-COVR-ROTR" "LI-COVR-SPRY"))
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  ; Use the first found layer's current state to decide direction
  (setq first-obj nil)
  (foreach lname layers
    (if (and (null first-obj) (tblsearch "LAYER" lname))
      (setq first-obj (vla-item (vla-get-layers doc) lname))
    )
  )
  (if (null first-obj)
    (princ "\nNeither sprayer layer found in this drawing.")
    (progn
      (setq turn-on (= (vla-get-layeron first-obj) :vlax-false))
      (foreach lname layers
        (if (tblsearch "LAYER" lname)
          (vla-put-layeron (vla-item (vla-get-layers doc) lname)
                           (if turn-on :vlax-true :vlax-false))
        )
      )
      (princ (strcat "\nSprayer layers " (if turn-on "ON" "OFF")))
    )
  )
)

;;; ------------------------------------------------------------------------
;;; EXPORT DIALOG HELPERS
;;; ------------------------------------------------------------------------

(defun MakeSelectAllStr (n / i str)
  "Build '0 1 2 ... n-1' selection string for DCL multi-select list_box"
  (setq str "" i 0)
  (repeat n
    (setq str (strcat str (if (= i 0) "" " ") (itoa i)))
    (setq i (1+ i))
  )
  str
)

(defun ScanDrawingCombos (/ ss i ent xdata xd-inner hole-str loc-str num-str num-int key
                             combos-alist existing combo-list spc hole-part loc-part gap-count
                             tagged-handles head-handle-val
                             ss2 j ent2 ent-data2 lfx-entry2 lfx-vals2
                             first-1000 ent-handle unnumbered-count)
  "Scan tags for hole/loc combos, count gaps, and count unnumbered LandFX heads.
   Returns list of (hole-int loc-str count gap-count) sorted by hole/loc, with
   (0 \"UNNUMBERED\" count 0) appended at end if unnumbered heads exist."
  (setq combos-alist  '()   ; each entry: (key count max-num)
        tagged-handles '())

  ; Pass 1: scan tags — build combo counts/max-nums AND collect linked head handles
  (if (setq ss (ssget "_X" (list '(0 . "INSERT") '(2 . "LAFX-TAG-SQUARE-999"))))
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (if (setq xdata (assoc -3 (entget ent '("HEADNUM"))))
          (progn
            (setq xd-inner        (cdr (assoc "HEADNUM" (cdr xdata))))
            (setq hole-str        (cdr (nth 0 xd-inner)))
            (setq loc-str         (cdr (nth 1 xd-inner)))
            (setq num-str         (cdr (nth 2 xd-inner)))
            (setq num-int         (if num-str (atoi num-str) 0))
            (setq head-handle-val (cdr (nth 3 xd-inner)))  ; 1005 = head handle
            (if head-handle-val
              (setq tagged-handles (cons head-handle-val tagged-handles))
            )
            (if (and hole-str loc-str)
              (progn
                (setq key (strcat hole-str "|" loc-str))
                (if (setq existing (assoc key combos-alist))
                  (setq combos-alist
                    (subst (list key (1+ (cadr existing)) (max (caddr existing) num-int))
                           existing
                           combos-alist))
                  (setq combos-alist (append combos-alist (list (list key 1 num-int))))
                )
              )
            )
          )
        )
        (setq i (1+ i))
      )
    )
  )

  ; Convert to (hole-int loc-str count gap-count) and sort
  ; gap-count = max-num - count  (numbers missing from 1..max)
  (setq combo-list '())
  (foreach pair combos-alist
    (setq key       (car  pair))
    (setq gap-count (- (caddr pair) (cadr pair)))  ; max-num - count
    (setq spc       (vl-string-search "|" key))
    (setq hole-part (substr key 1 spc))
    (setq loc-part  (substr key (+ 2 spc)))
    (setq combo-list (append combo-list
      (list (list (atoi hole-part) loc-part (cadr pair) gap-count))))
  )
  (setq combo-list
    (vl-sort combo-list
      '(lambda (a b)
         (cond
           ((< (car a) (car b)) T)
           ((= (car a) (car b)) (< (cadr a) (cadr b)))
           (T nil)
         )
       )
    )
  )

  ; Pass 2: count LandFX head blocks not linked to any tag
  (setq unnumbered-count 0)
  (if (setq ss2 (ssget "_X" (list '(0 . "INSERT") '(-3 ("LandFX")))))
    (progn
      (setq j 0)
      (repeat (sslength ss2)
        (setq ent2      (ssname ss2 j))
        (setq ent-data2 (entget ent2 '("LandFX")))
        (if (setq lfx-entry2 (assoc "LandFX" (cdr (assoc -3 ent-data2))))
          (progn
            (setq lfx-vals2  (cdr lfx-entry2))
            (setq first-1000 (cdr (assoc 1000 lfx-vals2)))
            (if (= first-1000 "Head")
              (progn
                (setq ent-handle (cdr (assoc 5 ent-data2)))
                (if (not (member ent-handle tagged-handles))
                  (setq unnumbered-count (1+ unnumbered-count))
                )
              )
            )
          )
        )
        (setq j (1+ j))
      )
    )
  )
  (if (> unnumbered-count 0)
    (setq combo-list (append combo-list (list (list 0 "UNNUMBERED" unnumbered-count 0))))
  )

  combo-list
)

(defun GetHeadDescStr (head-ent / lfx-data lfx-vals v2 v3)
  "Return 'Model-Nozzle' desc string from a head block's LandFX XDATA"
  (if (and head-ent
           (setq lfx-data (assoc "LandFX" (cdr (assoc -3 (entget head-ent '("LandFX")))))))
    (progn
      (setq lfx-vals (cdr lfx-data))
      (setq v3 (nth 3 lfx-vals))   ; model short key  e.g. "RAIN-752"
      (setq v2 (nth 2 lfx-vals))   ; nozzle number    e.g. "20"
      (if (and v3 v2 (= (car v3) 1000) (= (car v2) 1000))
        (strcat (cdr v3) "-" (cdr v2))
        "HEAD"
      )
    )
    "HEAD"
  )
)

(defun HN-ExportSelected (combo-list include-valves output-folder /
                           dwg-folder dwg-name written-count
                           hole loc tags sorted-tags
                           tag-pair tag-ent-name num head-handle head-ent head-pos
                           E-str N-str desc-str row rows csv-path fh
                           ss i ent ent-data lfx-entry lfx-vals
                           first-1000 pos v12 valve-rows valve-count
                           u-tagged-h u-ss u-i u-ent u-data u-lfx u-vals
                           u-h0 u-handle u-tag-ss u-j u-tag-ent u-hh)
  "Export PNEZD CSVs for selected hole/location combos; include-valves=T also writes VALVES.csv"
  (setq dwg-folder    output-folder)
  (setq dwg-name      (vl-filename-base (getvar "DWGNAME")))
  (setq written-count 0)

  ; Validate output folder exists before writing anything
  (if (not (vl-file-directory-p (vl-string-right-trim "\\" dwg-folder)))
    (progn
      (alert (strcat "Output folder does not exist or is not accessible:\n"
                     dwg-folder
                     "\n\nIf you created a new folder in the browser, press Enter\n"
                     "to confirm its name before clicking OK."))
      (exit)
    )
  )
  (princ (strcat "\nExporting to: " dwg-folder))

  ; --- Head CSVs (numbered and unnumbered) ---
  (foreach combo combo-list
    (setq hole (car   combo))
    (setq loc  (cadr  combo))

    (if (= loc "UNNUMBERED")

      ; --- Unnumbered branch: heads with no tag ---
      (progn
        ; Build set of all tagged head handles
        (setq u-tagged-h '())
        (if (setq u-tag-ss (ssget "_X" (list '(0 . "INSERT") '(2 . "LAFX-TAG-SQUARE-999"))))
          (progn
            (setq u-j 0)
            (repeat (sslength u-tag-ss)
              (setq u-tag-ent (ssname u-tag-ss u-j))
              (setq u-hh (GetHeadHandleFromTag u-tag-ent))
              (if u-hh (setq u-tagged-h (cons u-hh u-tagged-h)))
              (setq u-j (1+ u-j))
            )
          )
        )
        ; Scan all LandFX heads; export those not in tagged set
        (setq rows '())
        (if (setq u-ss (ssget "_X" (list '(0 . "INSERT") '(-3 ("LandFX")))))
          (progn
            (setq u-i 0)
            (repeat (sslength u-ss)
              (setq u-ent  (ssname u-ss u-i))
              (setq u-data (entget u-ent '("LandFX")))
              (if (setq u-lfx (assoc "LandFX" (cdr (assoc -3 u-data))))
                (progn
                  (setq u-vals (cdr u-lfx))
                  (setq u-h0   (cdr (assoc 1000 u-vals)))
                  (if (= u-h0 "Head")
                    (progn
                      (setq u-handle (cdr (assoc 5 u-data)))
                      (if (not (member u-handle u-tagged-h))
                        (progn
                          (setq head-pos (cdr (assoc 10 u-data)))
                          (setq E-str    (rtos (car  head-pos) 2 4))
                          (setq N-str    (rtos (cadr head-pos) 2 4))
                          (setq desc-str (GetHeadDescStr u-ent))
                          (setq rows (append rows
                            (list (list "" N-str E-str "0" desc-str))
                          ))
                        )
                      )
                    )
                  )
                )
              )
              (setq u-i (1+ u-i))
            )
          )
        )
        (if rows
          (progn
            (setq csv-path (strcat dwg-folder dwg-name "_UNNUMBERED.csv"))
            (setq fh (open csv-path "w"))
            (if fh
              (progn
                (write-line "Point,Northing,Easting,Elevation,Description" fh)
                (foreach row rows
                  (write-line
                    (strcat (car row) "," (cadr row) "," (caddr row) ","
                            (nth 3 row) "," (nth 4 row))
                    fh))
                (close fh)
                (princ (strcat "\nWrote: " csv-path " (" (itoa (length rows)) " unnumbered heads)"))
                (setq written-count (1+ written-count))
              )
              (princ (strcat "\nERROR: Cannot open " csv-path " for writing."))
            )
          )
        )
      )

      ; --- Numbered branch ---
      (progn
        (setq tags (GetAllHeadTags hole loc))
        (if (not tags)
          (princ (strcat "\nSkipping " (FormatHoleNumber hole) loc ": no tags found."))
          (progn
            (setq sorted-tags (vl-sort tags '(lambda (a b) (< (cadr a) (cadr b)))))
            (setq rows '())
            (foreach tag-pair sorted-tags
              (setq tag-ent-name (car  tag-pair))
              (setq num          (cadr tag-pair))
              (setq head-handle  (GetHeadHandleFromTag tag-ent-name))
              (if (and head-handle (setq head-ent (handent head-handle)))
                (progn
                  (setq head-pos (cdr (assoc 10 (entget head-ent))))
                  (setq E-str    (rtos (car  head-pos) 2 4))
                  (setq N-str    (rtos (cadr head-pos) 2 4))
                  (setq desc-str (GetHeadDescStr head-ent))
                  (setq rows (append rows
                    (list (list (FormatNumber num) N-str E-str "0" desc-str))
                  ))
                )
              )
            )
            (if rows
              (progn
                (setq csv-path (strcat dwg-folder dwg-name "_"
                                       (FormatHoleNumber hole) loc ".csv"))
                (setq fh (open csv-path "w"))
                (if fh
                  (progn
                    (write-line "Point,Northing,Easting,Elevation,Description" fh)
                    (foreach row rows
                      (write-line
                        (strcat (car row) "," (cadr row) "," (caddr row) ","
                                (nth 3 row) "," (nth 4 row))
                        fh))
                    (close fh)
                    (princ (strcat "\nWrote: " csv-path " (" (itoa (length rows)) " heads)"))
                    (setq written-count (1+ written-count))
                  )
                  (princ (strcat "\nERROR: Cannot open " csv-path " for writing."))
                )
              )
            )
          )
        )
      )

    ) ; end if UNNUMBERED
  ) ; end foreach

  ; --- Valve CSV (optional) ---
  (if include-valves
    (progn
      (setq valve-rows '() valve-count 0)
      (if (setq ss (ssget "_X" (list '(0 . "INSERT") '(-3 ("LandFX")))))
        (progn
          (setq i 0)
          (repeat (sslength ss)
            (setq ent      (ssname ss i))
            (setq ent-data (entget ent '("LandFX")))
            (if (setq lfx-entry (assoc "LandFX" (cdr (assoc -3 ent-data))))
              (progn
                (setq lfx-vals   (cdr lfx-entry))
                (setq first-1000 (cdr (assoc 1000 lfx-vals)))
                (if (= first-1000 "Valve")
                  (progn
                    (setq valve-count (1+ valve-count))
                    (setq pos   (cdr (assoc 10 ent-data)))
                    (setq E-str (rtos (car  pos) 2 4))
                    (setq N-str (rtos (cadr pos) 2 4))
                    (setq v12   (nth 12 lfx-vals))
                    (setq desc-str
                      (if (and v12 (= (car v12) 1000) (> (strlen (cdr v12)) 0))
                        (cdr v12) "VALVE"))
                    (setq valve-rows
                      (append valve-rows (list (list "" N-str E-str "0" desc-str))))
                  )
                )
              )
            )
            (setq i (1+ i))
          )
        )
      )
      (if (> valve-count 0)
        (progn
          (setq valve-rows
            (vl-sort valve-rows
              '(lambda (a b)
                 (cond
                   ((< (nth 4 a) (nth 4 b)) T)
                   ((= (nth 4 a) (nth 4 b)) (< (atof (caddr a)) (atof (caddr b))))
                   (T nil)
                 )
               )
            )
          )
          (setq csv-path (strcat dwg-folder dwg-name "_VALVES.csv"))
          (setq fh (open csv-path "w"))
          (if fh
            (progn
              (write-line "Point,Northing,Easting,Elevation,Description" fh)
              (foreach row valve-rows
                (write-line
                  (strcat (car row) "," (cadr row) "," (caddr row) ","
                          (nth 3 row) "," (nth 4 row))
                  fh))
              (close fh)
              (princ (strcat "\nWrote: " csv-path " (" (itoa valve-count) " valves)"))
              (setq written-count (1+ written-count))
            )
            (princ (strcat "\nERROR: Cannot open " csv-path " for writing."))
          )
        )
        (princ "\nNo valves found.")
      )
    )
  )

  (princ (strcat "\nExport complete: " (itoa written-count) " file(s) written."))
  written-count
)

(defun HN-BuildComboItems (combos)
  "Build display strings for HeadExportDialog list_box from (hole loc count gap-count) list"
  (mapcar
    '(lambda (c)
       (cond
         ((= (cadr c) "UNNUMBERED")
           (strcat "Unnumbered  (" (itoa (caddr c)) " heads)"))
         ((> (cadddr c) 0)
           (strcat (FormatHoleNumber (car c)) " " (cadr c)
                   "  (" (itoa (caddr c)) " heads, "
                   (itoa (cadddr c)) " gap" (if (= (cadddr c) 1) "" "s") ")"))
         (T
           (strcat (FormatHoleNumber (car c)) " " (cadr c)
                   "  (" (itoa (caddr c)) " heads)"))
       )
     )
    combos
  )
)

(defun HN-BrowseForFolder (prompt default-path / shell folder self-obj path)
  "Show Windows Shell folder browser dialog. Returns selected path with trailing \\ or nil."
  (vl-load-com)
  (setq shell (vlax-create-object "Shell.Application"))
  (if (not shell)
    nil
    (progn
      (setq folder
        (vl-catch-all-apply
          'vlax-invoke-method
          (list shell 'BrowseForFolder 0 prompt 64 0)  ; 64=BIF_NEWDIALOGSTYLE, 0=Desktop root (unrestricted)
        )
      )
      (vlax-release-object shell)
      (if (or (null folder) (vl-catch-all-error-p folder))
        nil
        (progn
          (setq self-obj (vlax-get-property folder 'Self))
          (setq path     (vlax-get-property self-obj 'Path))
          (vlax-release-object self-obj)
          (vlax-release-object folder)
          ; Ensure trailing backslash
          (if (/= (substr path (strlen path)) "\\")
            (setq path (strcat path "\\"))
          )
          path
        )
      )
    )
  )
)

(defun HN-RunExportDialog (/ dcl-path dcl_id export-combos combo-count combo-items
                              sel-str sel-list selected-combos include-valves result
                              output-folder browse-result fix-c)
  "Scan drawing for numbered areas, show export selection dialog, run export or fix gaps"
  (princ "\nScanning drawing for numbered heads...")
  (setq export-combos (ScanDrawingCombos))
  (if (not export-combos)
    (progn (alert "No numbered heads found in drawing.") (exit))
  )
  (setq combo-count (length export-combos))
  (setq combo-items (HN-BuildComboItems export-combos))

  ; Load and show sub-dialog
  (setq dcl-path (strcat *HN-DIR* "\\" "HeadNumbering.dcl"))
  (setq dcl_id (load_dialog dcl-path))
  (if (not (new_dialog "HeadExportDialog" dcl_id))
    (progn
      (unload_dialog dcl_id)
      (alert "ERROR: Could not load export dialog!")
      (exit)
    )
  )

  ; Default output folder = drawing's folder
  (setq output-folder (getvar "DWGPREFIX"))

  ; Populate list; default to all selected
  (start_list "export_list")
  (mapcar 'add_list combo-items)
  (end_list)
  (set_tile "export_list" (MakeSelectAllStr combo-count))
  (set_tile "include_valves" "1")
  (set_tile "output_folder" output-folder)
  ; Enable Fix Gaps button only when at least one combo has gaps
  (mode_tile "fix_gaps_btn"
    (if (vl-some '(lambda (c) (> (cadddr c) 0)) export-combos) 0 1))

  (action_tile "select_all_btn"
    "(set_tile \"export_list\" (MakeSelectAllStr combo-count))"
  )
  (action_tile "clear_all_btn"
    "(set_tile \"export_list\" \"\")"
  )
  ; Fix Gaps — operates on selection (or all if nothing selected), then refreshes list
  (action_tile "fix_gaps_btn"
    "(progn
       (setq sel-str (get_tile \"export_list\"))
       (setq sel-list
         (if (and sel-str (/= sel-str \"\"))
           (read (strcat \"(\" sel-str \")\"))
           (MakeRange 0 (1- combo-count))
         )
       )
       (foreach idx sel-list
         (setq fix-c (nth idx export-combos))
         (if (and fix-c
                  (> (cadddr fix-c) 0)
                  (/= (cadr fix-c) \"UNNUMBERED\"))
           (FillGaps (car fix-c) (cadr fix-c))
         )
       )
       (setq export-combos (ScanDrawingCombos))
       (setq combo-count   (length export-combos))
       (setq combo-items   (HN-BuildComboItems export-combos))
       (start_list \"export_list\")
       (mapcar 'add_list combo-items)
       (end_list)
       (mode_tile \"fix_gaps_btn\"
         (if (vl-some '(lambda (c) (> (cadddr c) 0)) export-combos) 0 1))
     )"
  )
  (action_tile "browse_btn"
    "(progn
       (setq browse-result
         (HN-BrowseForFolder \"Select output folder\" output-folder))
       (if browse-result
         (progn
           (setq output-folder browse-result)
           (set_tile \"output_folder\" output-folder)
         )
       )
     )"
  )
  (action_tile "accept"
    "(setq sel-str        (get_tile \"export_list\"))
     (setq include-valves (get_tile \"include_valves\"))
     (setq output-folder  (get_tile \"output_folder\"))
     (if (and output-folder
              (/= output-folder \"\")
              (/= (substr output-folder (strlen output-folder)) \"\\\\\"))
       (setq output-folder (strcat output-folder \"\\\\\"))
     )
     (done_dialog 1)"
  )
  (action_tile "cancel" "(done_dialog 0)")

  (setq result (start_dialog))
  (unload_dialog dcl_id)

  (if (= result 1)
    (progn
      (setq sel-list
        (if (and sel-str (/= sel-str ""))
          (read (strcat "(" sel-str ")"))
          '()
        )
      )
      (if (not sel-list)
        (princ "\nNo areas selected — nothing exported.")
        (progn
          (setq selected-combos
            (mapcar '(lambda (idx) (nth idx export-combos)) sel-list)
          )
          (HN-ExportSelected selected-combos (= include-valves "1") output-folder)
        )
      )
    )
  )
)

(defun ShowSettingsDialog (current-hole current-location / dcl-path dcl_id hole-choice location-choice
                            result list-items selected-idx selected-str gap-num override-number)
  "Display DCL dialog and return (hole location override-number)"

  (setq override-number nil)

  (if (null *HN-DIR*)
    (progn
      (alert "Cannot find Head Numbering.lsp\nAdd the TCLLC Numbering folder to:\nOptions > Files > Support File Search Path")
      (exit)
    )
  )
  (setq dcl-path (strcat *HN-DIR* "\\" "HeadNumbering.dcl"))
  (setq dcl_id (load_dialog dcl-path))

  (if (not (new_dialog "HeadNumDialog" dcl_id))
    (progn
      (alert "ERROR: Could not load dialog!")
      (exit)
    )
  )
  
  ; Populate hole dropdown (1-27)
  (start_list "hole_list")
  (mapcar 'add_list (mapcar 'itoa (MakeRange 1 *HN-MaxHole*)))
  (end_list)
  
  ; Populate location dropdown
  (start_list "location_list")
  (mapcar 'add_list *HN-Locations*)
  (end_list)
  
  ; Set current values
  (if current-hole
    (set_tile "hole_list" (itoa (1- current-hole)))
    (set_tile "hole_list" "0")
  )
  (if current-location
    (set_tile "location_list"
      (itoa (vl-position current-location *HN-Locations*))
    )
    (set_tile "location_list" "0")
  )
  
  ; Initial tag list population
  (setq hole-choice (get_tile "hole_list"))
  (setq location-choice (get_tile "location_list"))
  (setq list-items (RefreshTagList
    (1+ (atoi hole-choice))
    (nth (atoi location-choice) *HN-Locations*)
  ))
  (HN-RefreshStatus
    (1+ (atoi hole-choice))
    (nth (atoi location-choice) *HN-Locations*)
  )

  ; When hole changes - refresh list, clear override, refresh status
  (action_tile "hole_list"
    "(setq hole-choice $value)
     (setq override-number nil)
     (setq list-items (RefreshTagList
       (1+ (atoi hole-choice))
       (nth (atoi (get_tile \"location_list\")) *HN-Locations*)
     ))
     (HN-RefreshStatus
       (1+ (atoi hole-choice))
       (nth (atoi (get_tile \"location_list\")) *HN-Locations*)
     )"
  )

  ; When location changes - refresh list, clear override, refresh status
  (action_tile "location_list"
    "(setq location-choice $value)
     (setq override-number nil)
     (setq list-items (RefreshTagList
       (1+ (atoi (get_tile \"hole_list\")))
       (nth (atoi location-choice) *HN-Locations*)
     ))
     (HN-RefreshStatus
       (1+ (atoi (get_tile \"hole_list\")))
       (nth (atoi location-choice) *HN-Locations*)
     )"
  )

  ; When list item clicked - react to GAP lines, restore auto-next for others
  (action_tile "tag_list"
    "(setq selected-idx (atoi $value))
     (setq selected-str (nth selected-idx list-items))
     (if (and selected-str (vl-string-search \"GAP\" selected-str))
       (progn
         (setq gap-num (atoi (substr selected-str 5 3)))
         (setq override-number gap-num)
         (set_tile \"next_label\" (strcat \"Will fill: \" (substr selected-str 1 7)))
       )
       (progn
         (setq override-number nil)
         (HN-RefreshStatus
           (1+ (atoi (get_tile \"hole_list\")))
           (nth (atoi (get_tile \"location_list\")) *HN-Locations*)
         )
       )
     )"
  )
  
  ; Fill Gaps button
  (action_tile "fill_gaps_btn"
    "(progn
       (FillGaps
         (1+ (atoi (get_tile \"hole_list\")))
         (nth (atoi (get_tile \"location_list\")) *HN-Locations*)
       )
       (setq override-number nil)
       (setq list-items (RefreshTagList
         (1+ (atoi (get_tile \"hole_list\")))
         (nth (atoi (get_tile \"location_list\")) *HN-Locations*)
       ))
       (HN-RefreshStatus
         (1+ (atoi (get_tile \"hole_list\")))
         (nth (atoi (get_tile \"location_list\")) *HN-Locations*)
       )
     )"
  )

  ; Export button — opens sub-dialog to select areas and run export
  (action_tile "export_btn" "(HN-RunExportDialog)")

  ; OK button
  (action_tile "accept"
    "(setq hole-choice (get_tile \"hole_list\"))
     (setq location-choice (get_tile \"location_list\"))
     (done_dialog 1)"
  )
  (action_tile "cancel" "(done_dialog 0)")
  
  (setq result (start_dialog))
  (unload_dialog dcl_id)
  
  (if (= result 1)
    (list
      (1+ (atoi hole-choice))
      (nth (atoi location-choice) *HN-Locations*)
      override-number  ; nil if no gap selected, number if gap clicked
    )
    nil
  )
)

;;; ------------------------------------------------------------------------
;;; MAIN COMMAND
;;; ------------------------------------------------------------------------

(defun c:NUMBERHEADS (/ settings current-hole current-location current-number existing-tags
                        head-ent head-handle head-pt tag-ent place-pt existing-tag
                        user-input parent-list manual-num response highest collision-response done
                        sel-ent sel-type)
  
  (princ "\n=== HEAD NUMBERING ROUTINE ===")
  
  ; Show settings dialog - pre-populate with last used hole/location if available
  (if (not (setq settings (ShowSettingsDialog *HN-LastHole* *HN-LastLocation*)))
    (progn
      (princ "\nCancelled.")
      (exit)
    )
  )

  (setq current-hole     (car settings))
  (setq current-location (cadr settings))
  (setq *HN-LastHole*     current-hole)
  (setq *HN-LastLocation* current-location)
  
  ; Check if any tags exist for this hole/location
  (setq existing-tags (GetAllHeadTags current-hole current-location))
  
  (if (null existing-tags)
    (progn
      (SetCounter current-hole current-location 0)
      (princ "\nNo existing tags found, starting from 001.")
    )
  )
  
  ; Use gap override if selected in dialog, otherwise next auto number (skipping occupied slots)
  (if (caddr settings)
    (setq current-number (caddr settings))
    (setq current-number (NextAvailable current-hole current-location (1+ (GetCounter current-hole current-location))))
  )
  
  (princ (strcat "\nHole: " (itoa current-hole) " | Location: " current-location))
  (princ (strcat "\nNext number: " (FormatHoleNumber current-hole) current-location (FormatNumber current-number)))
  
  (setq done nil)
  
  ; Main loop
  (while (not done)
    
    ; Prompt for head selection
    (princ (strcat "\nCurrent: " (FormatHoleNumber current-hole) current-location " [Next: " (FormatNumber current-number) "]"))
    (initget "Change Manual Toggle")
    (setq user-input (nentsel "\nSelect head block [Change/Manual/Toggle]: "))

    (cond
      ; User typed "Toggle" keyword - toggle sprayer layers
      ((= user-input "Toggle")
        (HN-ToggleSprayers)
      )

      ; User typed "Manual" keyword
      ((or (= user-input "Manual") (= user-input "M"))
        (princ "\nManual number entry...")
        (setq manual-num (getint (strcat "\nEnter number (current next: " (itoa current-number) "): ")))
        
        (if manual-num
          (progn
            ; Check if this number already exists
            (if (TagExists current-hole current-location manual-num)
              (progn
                (princ (strcat "\nWARNING: " (FormatHoleNumber current-hole) current-location (FormatNumber manual-num) " already exists!"))
                (initget "Yes No")
                (setq response (getkword "\nRenumber existing tags? [Yes/No] <No>: "))
                
                (if (= response "Yes")
                  (progn
                    (princ (strcat "\nAbout to renumber tags >= " (itoa manual-num)))
                    ; Renumber all tags >= manual-num (updates counter internally)
                    (RenumberTags current-hole current-location manual-num)
                    ; Set current number to the manual number user chose
                    (setq current-number manual-num)
                    (princ (strcat "\nCurrent number set to: " (itoa current-number)))
                    (princ (strcat "\nCounter is now: " (itoa (GetCounter current-hole current-location))))
                    (princ (strcat "\nReady to place: " (FormatHoleNumber current-hole) current-location (FormatNumber current-number)))
                  )
                  (princ "\nManual number cancelled.")
                )
              )
              (progn
                ; Number doesn't exist, safe to use
                (setq current-number manual-num)
                (princ (strcat "\nManual number set to: " (itoa manual-num)))
              )
            )
          )
          (princ "\nManual number cancelled.")
        )
      )
      
      ; User typed "Change" or "C" keyword
      ((or (= user-input "Change") (= user-input "C"))
        (princ "\nChanging settings...")
        (if (setq settings (ShowSettingsDialog current-hole current-location))
          (progn
            (setq current-hole     (car settings))
            (setq current-location (cadr settings))
            (setq *HN-LastHole*     current-hole)
            (setq *HN-LastLocation* current-location)
            (if (caddr settings)
              (setq current-number (caddr settings))
              (setq current-number (NextAvailable current-hole current-location (1+ (GetCounter current-hole current-location))))
            )
            (princ (strcat "\nSwitched to Hole: " (itoa current-hole) " | Location: " current-location))
            (princ (strcat "\nNext number: " (FormatHoleNumber current-hole) current-location (FormatNumber current-number)))
          )
          (princ "\nSettings change cancelled, continuing with current settings.")
        )
      )
      
      ; User selected something
      (user-input
        ; nentsel returns: (entity pick-point) for top-level blocks
        ;              or: (entity pick-point matrix parent-list) for nested
        (setq sel-ent (car user-input))
        
        ; If nested, resolve to top-level parent
        (if (= (length user-input) 4)
          (progn
            (setq parent-list (cadddr user-input))
            (if (and parent-list (listp parent-list))
              (setq sel-ent (last parent-list))
            )
          )
        )
        
        ; Classify what was clicked
        (setq sel-type (ClassifySelection sel-ent))
        
        (cond
          ; -------------------------------------------------------
          ; Clicked our own tag - go straight to renumber prompt
          ; -------------------------------------------------------
          ((= sel-type 'OURTAG)
            (setq head-handle (GetHeadHandleFromTag sel-ent))
            (if (and head-handle (setq head-ent (handent head-handle)))
              (progn
                (setq response (ReplaceHeadTag sel-ent current-hole current-location current-number head-ent))
                (if response
                  (setq current-number (NextAvailable current-hole current-location (1+ (SyncCounter current-hole current-location))))
                )
              )
              (princ "\nERROR: Could not find linked head from tag XDATA.")
            )
          )

          ; -------------------------------------------------------
          ; Clicked a head block - check for existing tag, place or renumber
          ; -------------------------------------------------------
          ((= sel-type 'HEAD)
            (setq head-ent sel-ent)
            (setq existing-tag (HeadHasTag head-ent))
            (if existing-tag
              (progn
                ; Highlight linked tag and offer to renumber
                (setq response (ReplaceHeadTag existing-tag current-hole current-location current-number head-ent))
                (if response
                  (setq current-number (NextAvailable current-hole current-location (1+ (SyncCounter current-hole current-location))))
                )
              )
              (progn
                ; No existing tag - fresh placement with native drag
                (princ (strcat "\nPlacing: " (FormatHoleNumber current-hole) current-location (FormatNumber current-number)))
                (setq tag-ent (CreateHeadTag current-hole current-location current-number head-ent))
                (if tag-ent
                  (progn
                    (if (> current-number (GetCounter current-hole current-location))
                      (SetCounter current-hole current-location current-number)
                    )
                    (princ (strcat "\nPlaced: " (FormatHoleNumber current-hole) current-location (FormatNumber current-number)))
                    (setq current-number (NextAvailable current-hole current-location (1+ (GetCounter current-hole current-location))))
                  )
                  (princ "\nPlacement cancelled.")
                )
              )
            )
          )
          
          ; -------------------------------------------------------
          ; Clicked a LandFX tag - warn and skip
          ; -------------------------------------------------------
          ((= sel-type 'LFXTAG)
            (princ "\nThat is a Land F/X tag, not a head or your tag. Please try again.")
          )
          
          ; -------------------------------------------------------
          ; Clicked something unknown (tree, shrub, etc)
          ; -------------------------------------------------------
          (T
            (princ "\nThat doesn't look like a head or tag block. Please try again.")
          )
        )
      )
      
      ; User pressed Enter/Esc
      (T
        (setq done T)
      )
    )
  )
  
  (princ "\n=== NUMBERING COMPLETE ===")
  (princ)
)

(princ "\n=== HEAD NUMBERING ROUTINE LOADED ===")
(princ "\nCommand: NUMBERHEADS")
(princ "\nUtilities:")
(princ "\n  FINDTAG            - Click a head to find and highlight its tag")
(princ "\n  FINDHEAD           - Click a tag to find and highlight its linked head")
(princ "\n  REPAIRHEADTAGS     - Backfill XDATA on old tags missing it")
(princ "\n  REPAIRATTRIBUTES   - Fix blank attribute values using XDATA")
(princ "\n  STRIPLFXDATA       - Remove LandFX XDATA so LFX stops resetting attributes")
(princ "\n  EXPORTHEADS        - Export head locations to PNEZD CSV per location")
(princ "\n  DUMPHEADXDATA      - Dump all XDATA from a head block (diagnostic)")
(princ "\nBlock required: LAFX-TAG-SQUARE-999 (with XX attribute)")
(princ "\n==========================================\n")
(princ)

;;; ------------------------------------------------------------------------
;;; UTILITY COMMANDS - FIND TAG/HEAD
;;; ------------------------------------------------------------------------

;;; ------------------------------------------------------------------------
;;; REPAIRHEADTAGS - Backfill missing XDATA on LAFX-TAG-SQUARE-999 blocks
;;; ------------------------------------------------------------------------
;;; Use this to fix older LAFX-TAG-SQUARE-999 blocks placed before LandFX XDATA was
;;; added to the routine. Parses the attribute value to rebuild XDATA.
;;; Only processes tags that have no existing XDATA.
;;; ------------------------------------------------------------------------

(defun ParseHeadNumber (text / hole-str loc-str num-str i j)
  "Parse attribute text like '03FW007' into (hole location number-string)"
  (setq i 0)
  ; Scan past leading digits (hole number)
  (while (and (< i (strlen text))
              (wcmatch (substr text (1+ i) 1) "#"))
    (setq i (1+ i))
  )
  (if (= i 0) (progn (setq ParseHeadNumber-result nil) nil)
    (progn
      (setq hole-str (substr text 1 i))
      (setq j i)
      ; Scan past letters (location code)
      (while (and (< j (strlen text))
                  (not (wcmatch (substr text (1+ j) 1) "#")))
        (setq j (1+ j))
      )
      (if (= j i) nil
        (progn
          (setq loc-str (substr text (1+ i) (- j i)))
          (setq num-str (substr text (1+ j)))
          (if (and (> (strlen hole-str) 0)
                   (> (strlen loc-str) 0)
                   (> (strlen num-str) 0))
            (list (atoi hole-str) loc-str num-str)
            nil
          )
        )
      )
    )
  )
)

(defun c:REPAIRHEADTAGS (/ ss i ent ent-data att-ent att-data att-val parsed
                            hole loc num fixed skipped)
  "Backfill HEADNUM and LandFX XDATA on LAFX-TAG-SQUARE-999 blocks that are missing it"
  
  (princ "\n=== REPAIR HEAD TAG XDATA ===")
  (princ "\nScanning for LAFX-TAG-SQUARE-999 blocks with missing XDATA...\n")
  
  (setq fixed 0 skipped 0)
  
  (regapp "HEADNUM")
  (regapp "LandFX")
  
  (if (not (setq ss (ssget "_X" (list '(0 . "INSERT") '(2 . "LAFX-TAG-SQUARE-999")))))
    (progn (princ "\nNo LAFX-TAG-SQUARE-999 blocks found.") (exit))
  )
  
  (setq i 0)
  (repeat (sslength ss)
    (setq ent (ssname ss i))
    (setq ent-data (entget ent '("HEADNUM" "LandFX")))
    
    ; Only process blocks with no existing XDATA
    (if (not (assoc -3 ent-data))
      (progn
        ; Get attribute value
        (setq att-val nil)
        (setq att-ent (entnext ent))
        (while (and att-ent (= "ATTRIB" (cdr (assoc 0 (setq att-data (entget att-ent))))))
          (if (= "XX" (cdr (assoc 2 att-data)))
            (setq att-val (cdr (assoc 1 att-data)))
          )
          (setq att-ent (entnext att-ent))
        )
        
        (if (and att-val (> (strlen att-val) 0))
          (progn
            (setq parsed (ParseHeadNumber att-val))
            (if parsed
              (progn
                (setq hole (car parsed))
                (setq loc  (cadr parsed))
                (setq num  (caddr parsed))
                
                (princ (strcat "\nRepairing: [" att-val "]"))
                
                ; Write HEADNUM XDATA (no 1005 - head link is lost for old tags)
                (setq ent-data (vl-remove-if '(lambda (x) (= (car x) -3)) ent-data))
                (setq ent-data 
                  (append ent-data 
                    (list 
                      (list -3 
                        (list "HEADNUM"
                          (cons 1000 (itoa hole))
                          (cons 1000 loc)
                          (cons 1000 num)
                        )
                        (list "LandFX"
                          '(1000 . "VALVECALLOUT")
                        )
                      )
                    )
                  )
                )
                (entmod ent-data)
                (entupd ent)
                (setq fixed (1+ fixed))
              )
              (progn
                (princ (strcat "\nSkipping: [" att-val "] - cannot parse"))
                (setq skipped (1+ skipped))
              )
            )
          )
          (progn
            (princ (strcat "\nSkipping: handle " (cdr (assoc 5 ent-data)) " - no attribute value"))
            (setq skipped (1+ skipped))
          )
        )
      )
    )
    (setq i (1+ i))
  )
  
  (princ (strcat "\n\n=== REPAIR COMPLETE ==="))
  (princ (strcat "\nRepaired: " (itoa fixed)))
  (princ (strcat "\nSkipped:  " (itoa skipped)))
  (if (> fixed 0)
    (princ "\n\nNOTE: Repaired tags have no 1005 head link (lost for old tags).")
  )
  (princ)
)

;;; ------------------------------------------------------------------------
;;; REPAIRATTRIBUTES - Fix blank attribute values on LAFX-TAG-SQUARE-999 blocks
;;; ------------------------------------------------------------------------
;;; For tags that have correct XDATA but a blank XX attribute value.
;;; Rebuilds the display text from the HEADNUM XDATA.
;;; ------------------------------------------------------------------------

(defun c:REPAIRATTRIBUTES (/ ss i ent ent-data xdata xdata-vals hole-str loc-str num-str
                              new-text att-ent att-data fixed skipped)
  "Fix LAFX-TAG-SQUARE-999 blocks that have XDATA but blank attribute value"
  
  (princ "\n=== REPAIR HEAD TAG ATTRIBUTES ===")
  (princ "\nScanning for LAFX-TAG-SQUARE-999 blocks with blank attributes...\n")
  
  (setq fixed 0 skipped 0)
  
  (if (not (setq ss (ssget "_X" (list '(0 . "INSERT") '(2 . "LAFX-TAG-SQUARE-999")))))
    (progn (princ "\nNo LAFX-TAG-SQUARE-999 blocks found.") (exit))
  )
  
  (setq i 0)
  (repeat (sslength ss)
    (setq ent (ssname ss i))
    (setq ent-data (entget ent '("HEADNUM")))
    
    ; Only process blocks that have HEADNUM XDATA
    (if (setq xdata (assoc -3 ent-data))
      (progn
        (setq xdata-vals (cdr (assoc "HEADNUM" (cdr xdata))))
        
        (if xdata-vals
          (progn
            ; Extract hole/location/number from XDATA
            (setq hole-str (cdr (nth 0 xdata-vals)))
            (setq loc-str  (cdr (nth 1 xdata-vals)))
            (setq num-str  (cdr (nth 2 xdata-vals)))
            
            ; Build the display text
            (setq new-text (strcat (FormatHoleNumber (atoi hole-str)) loc-str num-str))
            
            ; Find the XX attribute
            (setq att-ent (entnext ent))
            (while (and att-ent (= "ATTRIB" (cdr (assoc 0 (setq att-data (entget att-ent))))))
              (if (= "XX" (cdr (assoc 2 att-data)))
                (progn
                  (setq current-val (cdr (assoc 1 att-data)))
                  ; Only fix if blank
                  (if (or (not current-val) (= current-val "") (= (strlen current-val) 0))
                    (progn
                      (princ (strcat "\nFixing: handle " (cdr (assoc 5 ent-data)) 
                        " -> [" new-text "]"))
                      (entmod (subst (cons 1 new-text) (assoc 1 att-data) att-data))
                      (entupd att-ent)
                      (entupd ent)
                      (setq fixed (1+ fixed))
                    )
                    (setq skipped (1+ skipped))  ; Already has a value
                  )
                )
              )
              (setq att-ent (entnext att-ent))
            )
          )
          (setq skipped (1+ skipped))
        )
      )
    )
    (setq i (1+ i))
  )
  
  (princ (strcat "\n\n=== REPAIR COMPLETE ==="))
  (princ (strcat "\nFixed:   " (itoa fixed)))
  (princ (strcat "\nSkipped: " (itoa skipped) " (already had values or no XDATA)"))
  (princ)
)

;;; ------------------------------------------------------------------------
;;; STRIPLFXDATA - Remove LandFX XDATA from all LAFX-TAG-SQUARE-999 blocks
;;; ------------------------------------------------------------------------
;;; LFX detects its own app name on non-LAFX blocks and resets their
;;; attributes. This strips the LandFX XDATA entry from LAFX-TAG-SQUARE-999 blocks
;;; so LFX leaves them alone entirely.
;;; ------------------------------------------------------------------------

(defun c:STRIPLFXDATA (/ ss i ent ent-data xdata-section new-xdata-apps stripped skipped)
  "Remove LandFX XDATA from LAFX-TAG-SQUARE-999 blocks to prevent LFX from resetting them"
  
  (princ "\n=== STRIP LANDFX XDATA FROM LAFX-TAG-SQUARE-999 BLOCKS ===\n")
  
  (setq stripped 0 skipped 0)
  
  (if (not (setq ss (ssget "_X" (list '(0 . "INSERT") '(2 . "LAFX-TAG-SQUARE-999")))))
    (progn (princ "\nNo LAFX-TAG-SQUARE-999 blocks found.") (exit))
  )
  
  (setq i 0)
  (repeat (sslength ss)
    (setq ent (ssname ss i))
    (setq ent-data (entget ent '("LandFX")))
    
    (if (setq xdata-section (assoc -3 ent-data))
      (progn
        ; Rebuild XDATA list without the LandFX app entry
        (setq new-xdata-apps 
          (vl-remove-if 
            '(lambda (app) (= (car app) "LandFX")) 
            (cdr xdata-section)
          )
        )
        
        ; Remove old XDATA from entity data
        (setq ent-data (vl-remove-if '(lambda (x) (= (car x) -3)) ent-data))
        
        ; Add back only if there are other apps remaining
        (if new-xdata-apps
          (setq ent-data (append ent-data (list (cons -3 new-xdata-apps))))
        )
        
        (entmod ent-data)
        (entupd ent)
        (princ (strcat "\nStripped LandFX XDATA from handle " (cdr (assoc 5 ent-data))))
        (setq stripped (1+ stripped))
      )
      (setq skipped (1+ skipped))
    )
    (setq i (1+ i))
  )
  
  (princ (strcat "\n\n=== COMPLETE ==="))
  (princ (strcat "\nStripped: " (itoa stripped)))
  (princ (strcat "\nSkipped (none found): " (itoa skipped)))
  (princ "\n\nRun REPAIRATTRIBUTES next to fix any blanked attribute values.")
  (princ)
)

(defun c:FINDTAG (/ head-sel head-ent tag-ent tag-data hole loc num ss i xdata xdata-app xdata-vals xdata-section handle-pair head-handle parent-list test-tag att-ent att-val)
  "Click a head block to find and highlight its linked tag (LAFX-TAG-SQUARE-999 or LAFX-TAG)"
  
  (setq head-sel (nentsel "\nSelect head block: "))
  
  (if head-sel
    (progn
      (setq head-ent (car head-sel))
      
      ; nentsel: (entity pick-point matrix parent-list) for nested
      ; parent-list is 4th element when nested
      (if (= (length head-sel) 4)
        (progn
          (setq parent-list (cadddr head-sel))
          (if (and parent-list (listp parent-list))
            (setq head-ent (last parent-list))
          )
        )
      )
      
      (setq head-handle (cdr (assoc 5 (entget head-ent))))
      (princ (strcat "\nHead handle: " head-handle))
      (princ (strcat "\nHead block: " (cdr (assoc 2 (entget head-ent)))))
      
      ; First try to find LAFX-TAG-SQUARE-999 (our system)
      (if (setq tag-ent (HeadHasTag head-ent))
        (progn
          (princ "\n>>> Found LAFX-TAG-SQUARE-999 (TCLLC System) <<<")
          
          ; Get tag data to show the number
          (setq tag-data (entget tag-ent '("HEADNUM")))
          
          ; Extract hole/location/number from XDATA
          (if (setq xdata (assoc -3 tag-data))
            (progn
              (setq xdata-app (assoc "HEADNUM" (cdr xdata)))
              (if xdata-app
                (progn
                  (setq xdata-vals (cdr xdata-app))
                  (setq hole (cdr (nth 0 xdata-vals)))
                  (setq loc (cdr (nth 1 xdata-vals)))
                  (setq num (cdr (nth 2 xdata-vals)))
                  (princ (strcat "\nTag number: " hole loc num))
                )
              )
            )
          )
          
          ; Highlight the tag
          (redraw tag-ent 3)
          (princ "\nTag highlighted! Press Enter to continue...")
          (getstring)
          (redraw tag-ent 4)
        )
        (progn
          ; Not found in our system, try LAFX system
          (princ "\n>>> Checking for other tag systems... <<<")
          
          ; Search all LAFX-TAG blocks
          (if (setq ss (ssget "_X" '((0 . "INSERT")(2 . "LAFX-TAG*"))))
            (progn
              (princ (strcat "\nFound " (itoa (sslength ss)) " LAFX-TAG blocks in drawing"))
              (setq i 0)
              (setq tag-ent nil)
              
              (while (and (< i (sslength ss)) (not tag-ent))
                (setq test-tag (ssname ss i))
                
                ; Get XDATA - try multiple app names
                (setq xdata (entget test-tag '("*")))
                (setq xdata-section (assoc -3 xdata))
                
                (if xdata-section
                  (progn
                    ; Look through all XDATA apps for a 1005 handle reference
                    (foreach app-data (cdr xdata-section)
                      (if (listp app-data)
                        (progn
                          (foreach data-pair (cdr app-data)
                            (if (and (listp data-pair) (= (car data-pair) 1005))
                              (if (equal (cdr data-pair) head-handle)
                                (progn
                                  (setq tag-ent test-tag)
                                  (princ "\n>>> Found LAFX-TAG (Land F/X System) <<<")
                                  (princ (strcat "\nXDATA app: " (car app-data)))
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
                
                (setq i (1+ i))
              )
              
              (if tag-ent
                (progn
                  ; Try to get valve number from attributes
                  (setq att-ent (entnext tag-ent))
                  (while (and att-ent (= "ATTRIB" (cdr (assoc 0 (entget att-ent)))))
                    (setq att-val (cdr (assoc 1 (entget att-ent))))
                    (if att-val
                      (princ (strcat "\nValve number: " att-val))
                    )
                    (setq att-ent (entnext att-ent))
                  )
                  
                  ; Highlight the tag
                  (redraw tag-ent 3)
                  (princ "\nLand F/X tag highlighted! Press Enter to continue...")
                  (getstring)
                  (redraw tag-ent 4)
                )
                (princ "\nNo tag found linked to this head.")
              )
            )
            (princ "\nNo tag found linked to this head.")
          )
        )
      )
    )
    (princ "\nNo selection.")
  )
  (princ)
)

(defun c:FINDHEAD (/ tag-sel tag-ent tag-name xdata xdata-app xdata-vals xdata-section handle-pair head-handle head-ent head-pt att-ent att-val)
  "Click a tag block to find and highlight its linked head (LAFX-TAG-SQUARE-999 or LAFX-TAG)"
  
  (setq tag-sel (entsel "\nSelect tag block: "))
  
  (if tag-sel
    (progn
      (setq tag-ent (car tag-sel))
      (setq tag-name (cdr (assoc 2 (entget tag-ent))))
      
      (princ (strcat "\nSelected block: " tag-name))
      
      ; Check if it's a LAFX-TAG-SQUARE-999 block (TCLLC system)
      (if (= "LAFX-TAG-SQUARE-999" tag-name)
        (progn
          (princ "\n>>> LAFX-TAG-SQUARE-999 (TCLLC System) <<<")
          
          ; Get XDATA
          (if (setq xdata (assoc -3 (entget tag-ent '("HEADNUM"))))
            (progn
              (setq xdata-app (assoc "HEADNUM" (cdr xdata)))
              (if xdata-app
                (progn
                  ; Get the handle reference (1005)
                  (setq xdata-vals (cdr xdata-app))
                  (if (setq handle-pair (assoc 1005 xdata-vals))
                    (progn
                      (setq head-handle (cdr handle-pair))
                      (setq head-ent (handent head-handle))
                      
                      (if head-ent
                        (progn
                          (princ (strcat "\nFound head: " (cdr (assoc 2 (entget head-ent)))))
                          (princ (strcat "\nHandle: " head-handle))
                          
                          ; Highlight the head
                          (redraw head-ent 3)
                          (princ "\nHead highlighted! Press Enter to continue...")
                          (getstring)
                          (redraw head-ent 4)
                        )
                        (princ "\nLinked head not found (broken reference).")
                      )
                    )
                    (princ "\nNo head link found in tag XDATA.")
                  )
                )
                (princ "\nNo HEADNUM data found in tag.")
              )
            )
            (princ "\nNo XDATA found on this tag.")
          )
        )
        ; Check if it's a LAFX-TAG block
        (if (wcmatch tag-name "LAFX-TAG*")
          (progn
            (princ "\n>>> LAFX-TAG (Land F/X System) <<<")
            
            ; Get valve number from attributes
            (setq att-ent (entnext tag-ent))
            (while (and att-ent (= "ATTRIB" (cdr (assoc 0 (entget att-ent)))))
              (setq att-val (cdr (assoc 1 (entget att-ent))))
              (if att-val
                (princ (strcat "\nValve number: " att-val))
              )
              (setq att-ent (entnext att-ent))
            )
            
            ; Get XDATA - try all apps
            (setq xdata (entget tag-ent '("*")))
            (setq xdata-section (assoc -3 xdata))
            
            (if xdata-section
              (progn
                ; Look through all XDATA apps for a 1005 handle reference
                (setq head-handle nil)
                (foreach app-data (cdr xdata-section)
                  (if (listp app-data)
                    (progn
                      (foreach data-pair (cdr app-data)
                        (if (and (listp data-pair) (= (car data-pair) 1005))
                          (progn
                            (setq head-handle (cdr data-pair))
                            (princ (strcat "\nXDATA app: " (car app-data)))
                          )
                        )
                      )
                    )
                  )
                )
                
                (if head-handle
                  (progn
                    (setq head-ent (handent head-handle))
                    
                    (if head-ent
                      (progn
                        (princ (strcat "\nFound head: " (cdr (assoc 2 (entget head-ent)))))
                        (princ (strcat "\nHandle: " head-handle))
                        
                        ; Get head position
                        (setq head-pt (cdr (assoc 10 (entget head-ent))))
                        (princ (strcat "\nPosition: " (rtos (car head-pt) 2 4) "," (rtos (cadr head-pt) 2 4)))
                        
                        ; Highlight the head
                        (redraw head-ent 3)
                        (princ "\nHead highlighted! Press Enter to continue...")
                        (getstring)
                        (redraw head-ent 4)
                      )
                      (princ "\nLinked head not found (broken reference).")
                    )
                  )
                  (princ "\nNo head link (1005) found in XDATA.")
                )
              )
              (princ "\nNo XDATA found on this LAFX tag.")
            )
          )
          (princ "\nSelected block is not a recognized tag block.")
        )
      )
    )
    (princ "\nNo selection.")
  )
  (princ)
)

;;; ------------------------------------------------------------------------
;;; EXPORTHEADS — Export VIH head locations to PNEZD CSV files (one per location)
;;;
;;; LandFX XDATA field map (confirmed from DUMPHEADXDATA on RAIN-752 VIH):
;;;   [0] "Head"                    - type identifier
;;;   [1] "Valve-in-Head Rotor"     - category
;;;   [2] nozzle number ("20")
;;;   [3] model key ("RAIN-752")
;;;   [5] display name ("Rain Bird 752-IC")
;;;
;;; Output: [DwgName]_[Location].csv  and  [DwgName]_UNNUMBERED.csv
;;; Format: Point,Northing,Easting,Elevation,Description
;;;   Point       = head number (e.g. 01FW007) or "" for unnumbered
;;;   Northing    = Y (state plane)
;;;   Easting     = X (state plane)
;;;   Elevation   = 0
;;;   Description = Model-Nozzle (e.g. RAIN-752-20)
;;; ------------------------------------------------------------------------

(defun c:EXPORTHEADS (/ tag-ss tag-ent tag-xd tag-vals t-h1005 tag-map j
                        ss i ent ent-data pos E-str N-str
                        lfx-entry lfx-vals first-1000 model-str nozzle-str desc-str
                        head-handle tag-info pt-num bucket-key row
                        buckets loc-key rows sorted-rows
                        dwg-folder dwg-name csv-path fh
                        head-count unnumbered-count written-files
                        v2 v3 v12
                        valve-rows valve-count)
  "Export all LandFX head blocks to PNEZD CSV files, one file per location code."

  (princ "\n=== EXPORT HEADS TO CSV (PNEZD) ===")

  ; -------------------------------------------------------
  ; Step 1: Build tag lookup map: head-handle -> (handle hole-str loc-str num-str)
  ; Much faster than calling HeadHasTag for every head (O(n) vs O(n^2))
  ; -------------------------------------------------------
  (setq tag-map '())
  (if (setq tag-ss (ssget "_X" (list '(0 . "INSERT") '(2 . "LAFX-TAG-SQUARE-999"))))
    (progn
      (setq j 0)
      (repeat (sslength tag-ss)
        (setq tag-ent (ssname tag-ss j))
        (if (setq tag-xd (assoc -3 (entget tag-ent '("HEADNUM"))))
          (progn
            (setq tag-vals (cdr (assoc "HEADNUM" (cdr tag-xd))))
            ; Need at least 4 items: hole, loc, num, 1005 handle
            (if (and tag-vals (>= (length tag-vals) 4))
              (progn
                (setq t-h1005 (assoc 1005 tag-vals))
                (if t-h1005
                  (setq tag-map
                    (cons
                      (list (cdr t-h1005)           ; [0] head handle (key for assoc)
                            (cdr (nth 0 tag-vals))  ; [1] hole as string e.g. "1"
                            (cdr (nth 1 tag-vals))  ; [2] location e.g. "FW"
                            (cdr (nth 2 tag-vals))) ; [3] number e.g. "007"
                      tag-map
                    )
                  )
                )
              )
            )
          )
        )
        (setq j (1+ j))
      )
    )
  )
  (princ (strcat "\nBuilt tag map: " (itoa (length tag-map)) " numbered heads."))

  ; -------------------------------------------------------
  ; Step 2: Scan all INSERT entities with LandFX XDATA, filter for [0]="Head"
  ; -------------------------------------------------------
  (setq ss (ssget "_X" (list '(0 . "INSERT") '(-3 ("LandFX")))))
  (if (null ss)
    (progn (princ "\nNo LandFX entities found in drawing.") (exit))
  )

  (setq head-count      0)
  (setq unnumbered-count 0)
  (setq buckets         '())

  (setq i 0)
  (repeat (sslength ss)
    (setq ent      (ssname ss i))
    (setq ent-data (entget ent '("LandFX")))

    ; Get LandFX entry and vals list
    (if (setq lfx-entry (assoc "LandFX" (cdr (assoc -3 ent-data))))
      (progn
        (setq lfx-vals   (cdr lfx-entry))
        (setq first-1000 (cdr (assoc 1000 lfx-vals)))

        (if (= first-1000 "Head")
          (progn
            (setq head-count (1+ head-count))

            ; Insertion point → E (X) and N (Y)
            (setq pos   (cdr (assoc 10 ent-data)))
            (setq E-str (rtos (car  pos) 2 4))
            (setq N-str (rtos (cadr pos) 2 4))

            ; Model [3] and Nozzle [2] from LandFX XDATA
            (setq v2 (nth 2 lfx-vals))
            (setq v3 (nth 3 lfx-vals))
            (setq nozzle-str (if (and v2 (= (car v2) 1000)) (cdr v2) ""))
            (setq model-str  (if (and v3 (= (car v3) 1000)) (cdr v3) ""))
            (if (and (> (strlen model-str) 0) (> (strlen nozzle-str) 0))
              (setq desc-str (strcat model-str "-" nozzle-str))
              (setq desc-str (cdr (assoc 2 ent-data)))  ; fallback: block name
            )

            ; Look up this head's handle in the tag map
            (setq head-handle (cdr (assoc 5 ent-data)))
            (setq tag-info    (assoc head-handle tag-map))

            (if tag-info
              (progn
                ; Numbered: build formatted point string and bucket by location
                (setq pt-num     (strcat (FormatHoleNumber (atoi (cadr tag-info)))
                                         (caddr tag-info)
                                         (cadddr tag-info)))
                (setq bucket-key (caddr tag-info))  ; location code e.g. "FW"
              )
              (progn
                ; Unnumbered
                (setq pt-num      "")
                (setq bucket-key  "UNNUMBERED")
                (setq unnumbered-count (1+ unnumbered-count))
              )
            )

            ; Build row (Point N E Z Desc) and add to location bucket
            (setq row (list pt-num N-str E-str "0" desc-str))
            (if (assoc bucket-key buckets)
              (setq buckets
                (subst
                  (cons bucket-key (append (cdr (assoc bucket-key buckets)) (list row)))
                  (assoc bucket-key buckets)
                  buckets
                )
              )
              (setq buckets (append buckets (list (cons bucket-key (list row)))))
            )
          )
        )
      )
    )
    (setq i (1+ i))
  )

  (if (= head-count 0)
    (progn (princ "\nNo head blocks found in drawing.") (exit))
  )
  (princ (strcat "\nTotal heads: "    (itoa head-count)))
  (princ (strcat "   Numbered: "      (itoa (- head-count unnumbered-count))))
  (princ (strcat "   Unnumbered: "    (itoa unnumbered-count)))

  ; -------------------------------------------------------
  ; Step 3: Write one CSV file per bucket (location or UNNUMBERED)
  ; -------------------------------------------------------
  (setq dwg-folder (getvar "DWGPREFIX"))
  (setq dwg-name   (vl-filename-base (getvar "DWGNAME")))
  (setq written-files 0)

  (foreach bucket buckets
    (setq loc-key (car bucket))
    (setq rows    (cdr bucket))

    ; Sort: numbered rows alphabetically first, then unnumbered
    (setq sorted-rows
      (vl-sort rows
        '(lambda (a b)
           (cond
             ((and (> (strlen (car a)) 0) (> (strlen (car b)) 0))
              (< (car a) (car b)))       ; both numbered: alpha (= numeric for zero-padded)
             ((> (strlen (car a)) 0) T)  ; a numbered, b not: a first
             ((> (strlen (car b)) 0) nil); b numbered, a not: b first
             (T nil)                      ; both unnumbered: stable
           )
        )
      )
    )

    (setq csv-path (strcat dwg-folder dwg-name "_" loc-key ".csv"))
    (setq fh (open csv-path "w"))

    (if fh
      (progn
        (write-line "Point,Northing,Easting,Elevation,Description" fh)
        (foreach row sorted-rows
          (write-line
            (strcat (car   row) ","
                    (cadr  row) ","
                    (caddr row) ","
                    (nth 3 row) ","
                    (nth 4 row))
            fh
          )
        )
        (close fh)
        (princ (strcat "\nWrote: " csv-path " (" (itoa (length sorted-rows)) " rows)"))
        (setq written-files (1+ written-files))
      )
      (princ (strcat "\nERROR: Cannot write to " csv-path))
    )
  )

  ; -------------------------------------------------------
  ; Step 4: Scan valve blocks — write [DWG]_VALVES.csv
  ; -------------------------------------------------------
  (setq valve-rows  '())
  (setq valve-count 0)

  (if (setq ss (ssget "_X" (list '(0 . "INSERT") '(-3 ("LandFX")))))
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq ent      (ssname ss i))
        (setq ent-data (entget ent '("LandFX")))
        (if (setq lfx-entry (assoc "LandFX" (cdr (assoc -3 ent-data))))
          (progn
            (setq lfx-vals   (cdr lfx-entry))
            (setq first-1000 (cdr (assoc 1000 lfx-vals)))
            ; DEBUG: uncomment next line to see every LandFX type encountered
            ;(princ (strcat "\n  [VALVE-SCAN] block=" (cdr (assoc 2 ent-data)) "  type=" (if first-1000 first-1000 "NIL")))
            (if (= first-1000 "Valve")
              (progn
                (setq valve-count (1+ valve-count))
                (setq pos   (cdr (assoc 10 ent-data)))
                (setq E-str (rtos (car  pos) 2 4))
                (setq N-str (rtos (cadr pos) 2 4))
                ; Type code is at index [12] (e.g. "SOV-001", "SOV-005", "RCV-002")
                (setq v12 (nth 12 lfx-vals))
                (setq desc-str
                  (if (and v12 (= (car v12) 1000) (> (strlen (cdr v12)) 0))
                    (cdr v12)
                    "VALVE"
                  )
                )
                (setq valve-rows
                  (append valve-rows (list (list "" N-str E-str "0" desc-str)))
                )
              )
            )
          )
        )
        (setq i (1+ i))
      )
      (princ (strcat "\nValve scan: checked " (itoa (sslength ss))
                     " LandFX entities, found " (itoa valve-count) " valves."))
    )
    (princ "\nValve scan: no LandFX entities found in drawing at all.")
  )

  (if (> valve-count 0)
    (progn
      ; Sort by type code (desc), then by Easting within each type
      (setq valve-rows
        (vl-sort valve-rows
          '(lambda (a b)
             (cond
               ((< (nth 4 a) (nth 4 b)) T)
               ((= (nth 4 a) (nth 4 b)) (< (atof (caddr a)) (atof (caddr b))))
               (T nil)
             )
          )
        )
      )
      (setq csv-path (strcat dwg-folder dwg-name "_VALVES.csv"))
      (setq fh (open csv-path "w"))
      (if fh
        (progn
          (write-line "Point,Northing,Easting,Elevation,Description" fh)
          (foreach row valve-rows
            (write-line
              (strcat (car   row) ","
                      (cadr  row) ","
                      (caddr row) ","
                      (nth 3 row) ","
                      (nth 4 row))
              fh
            )
          )
          (close fh)
          (princ (strcat "\nWrote: " csv-path " (" (itoa valve-count) " valves)"))
          (setq written-files (1+ written-files))
        )
        (princ (strcat "\nERROR: Cannot write to " csv-path))
      )
    )
    (princ "\nNo valve blocks found.")
  )

  (if (> written-files 0)
    (progn
      (princ (strcat "\n\n=== EXPORT COMPLETE: " (itoa written-files) " file(s) written ==="))
      (princ (strcat "\nFolder: " dwg-folder))
    )
    (princ "\n\nNo files written.")
  )
  (princ)
)

;;; ------------------------------------------------------------------------
;;; DUMPHEADXDATA — Diagnostic: dump all XDATA from a selected head block
;;; Use this to map field indices for Model / Nozzle before writing EXPORTHEADS
;;; ------------------------------------------------------------------------

(defun c:DUMPHEADXDATA (/ sel ent ename blkname pos xdata xd-section app-entry app-name pairs idx pair gcode gval att-ent att-data)
  "Click a head block (VIH rotor or any LAFX head) to dump all XDATA fields to the command line."

  (setq sel (entsel "\nSelect a head block: "))

  (if (null sel)
    (princ "\nNo selection.")
    (progn
      (setq ent     (car sel))
      (setq ename   (entget ent))
      (setq blkname (cdr (assoc 2 ename)))
      (setq pos     (cdr (assoc 10 ename)))

      (princ "\n\n=== DUMPHEADXDATA ===")
      (princ (strcat "\nBlock name : " blkname))
      (if pos
        (princ (strcat "\nInsertion  : " (rtos (car pos) 2 6) " , " (rtos (cadr pos) 2 6)))
      )

      ;; Grab XDATA from ALL registered apps
      (setq xdata      (entget ent '("*")))
      (setq xd-section (assoc -3 xdata))

      (if (null xd-section)
        (princ "\n\nNo XDATA found on this entity.")
        (progn
          ;; xd-section: (-3 ("APP1" (1000 . "val") ...) ("APP2" ...) ...)
          (foreach app-entry (cdr xd-section)
            (setq app-name (car app-entry))
            (setq pairs    (cdr app-entry))
            (princ (strcat "\n\n--- XDATA app: " app-name " ---"))
            (setq idx 0)
            (foreach pair pairs
              (setq gcode (car pair))
              (setq gval  (cdr pair))
              (princ
                (strcat "\n  [" (itoa idx) "] "
                        (itoa gcode) " : "
                        (cond
                          ((= gcode 1000) (strcat "\"" gval "\""))
                          ((= gcode 1001) (strcat "APP=" gval))
                          ((= gcode 1002) (strcat "{" gval "}"))
                          ((= gcode 1003) (strcat "layer=" gval))
                          ((= gcode 1004) (strcat "bin=" gval))
                          ((= gcode 1005) (strcat "handle=" gval))
                          ((= gcode 1010)
                           (strcat "pt=(" (rtos (car gval) 2 4) ","
                                          (rtos (cadr gval) 2 4) ","
                                          (rtos (caddr gval) 2 4) ")"))
                          ((= gcode 1040) (rtos gval 2 6))
                          ((= gcode 1070) (itoa gval))
                          ((= gcode 1071) (itoa gval))
                          (T              (vl-prin1-to-string gval))
                        )
                )
              )
              (setq idx (1+ idx))
            )
          )
        )
      )

      ;; Also dump attributes if block has any
      (setq att-ent (entnext ent))
      (if (and att-ent (= "ATTRIB" (cdr (assoc 0 (entget att-ent)))))
        (progn
          (princ "\n\n--- Block Attributes ---")
          (while (and att-ent (= "ATTRIB" (cdr (assoc 0 (entget att-ent)))))
            (setq att-data (entget att-ent))
            (princ (strcat "\n  Tag=" (cdr (assoc 2 att-data))
                           "  Value=\"" (cdr (assoc 1 att-data)) "\""))
            (setq att-ent (entnext att-ent))
          )
        )
      )

      (princ "\n\n=== END DUMP ===\n")
    )
  )
  (princ)
)
