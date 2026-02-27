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

;;; --- CONFIGURATION ---

(setq *HN-Locations*     '("TE" "FW" "FP" "RG" "GR" "GP"))
(setq *HN-MaxHole*       27)
(setq *HN-DIR*
  (cond
    ((findfile "Head Numbering.lsp") (vl-filename-directory (findfile "Head Numbering.lsp")))
    ((findfile "HeadNumbering.dcl")  (vl-filename-directory (findfile "HeadNumbering.dcl")))
    (T nil)))
(if (not *HN-LastHole*)     (setq *HN-LastHole*     nil))
(if (not *HN-LastLocation*) (setq *HN-LastLocation* nil))

;;; --- DICTIONARY MANAGEMENT ---

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

(defun GetCounter (hole location / dict key rec)
  (setq dict (GetCounterDict)
        key  (strcat (itoa hole) "_" location))
  (if (and (setq rec (dictsearch dict key))
           (setq rec (cdr (assoc 1 (entget (cdr (assoc -1 rec)))))))
    (atoi rec)
    0)
)

(defun SetCounter (hole location value / dict key xrec)
  (setq dict (GetCounterDict)
        key  (strcat (itoa hole) "_" location))
  (if (setq xrec (dictsearch dict key))
    (progn (entdel (cdr (assoc -1 xrec))) (dictremove dict key))
  )
  (dictadd dict key (entmakex (list (cons 0 "XRECORD") (cons 100 "AcDbXrecord") (cons 1 (itoa value)))))
  value
)

(defun SyncCounter (hole location / tags max-n)
  (setq tags  (GetAllHeadTags hole location)
        max-n (if tags (apply 'max (mapcar 'cadr tags)) 0))
  (SetCounter hole location max-n)
  max-n
)

(defun NextAvailable (hole location start / nums n)
  (setq nums (mapcar 'cadr (GetAllHeadTags hole location))
        n    start)
  (while (member n nums) (setq n (1+ n)))
  n
)

;;; --- TAG SEARCH AND MANIPULATION ---

(defun HeadHasTag (head-ent / head-handle ss i tag-ent info found)
  "Check if a head block already has a tag linked to it"
  (setq head-handle (cdr (assoc 5 (entget head-ent))))
  (setq found nil)
  (if (setq ss (ssget "_X" (list '(0 . "INSERT") '(2 . "LAFX-TAG-SQUARE-999"))))
    (progn
      (setq i 0)
      (while (and (< i (sslength ss)) (not found))
        (setq tag-ent (ssname ss i))
        (if (setq info (HN-GetTagInfo tag-ent))
          (if (equal (nth 3 info) head-handle)
            (setq found tag-ent)
          )
        )
        (setq i (1+ i))
      )
    )
  )
  found
)

(defun GetAllHeadTags (hole location / ss i ent info tags)
  "Get all LAFX-TAG-SQUARE-999 blocks for specific hole/location combo"
  (setq tags '())
  (if (setq ss (ssget "_X" (list '(0 . "INSERT") '(2 . "LAFX-TAG-SQUARE-999"))))
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (if (setq info (HN-GetTagInfo ent))
          (if (and (= (atoi (car info)) hole) (= (cadr info) location))
            (setq tags (append tags (list (list ent (atoi (caddr info))))))
          )
        )
        (setq i (1+ i))
      )
    )
  )
  tags
)

(defun TagExists (hole location number)
  (vl-some '(lambda (x) (= (cadr x) number)) (GetAllHeadTags hole location))
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

(defun GetHeadHandleFromTag (tag-ent / info)
  (if (setq info (HN-GetTagInfo tag-ent)) (nth 3 info))
)

;;; --- TAG CREATION AND UPDATE ---

(defun UpdateTagNumber (tag-ent hole location number head-handle / ent-data att-ent att-data new-text)
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

(defun ClassifySelection (ent / lfx-vals first-1000)
  "Classify a selected entity based on LandFX XDATA first 1000 field.
   Returns: 'HEAD, 'OURTAG, 'LFXTAG, or 'UNKNOWN"
  (setq lfx-vals (HN-GetLFXVals ent))
  (if lfx-vals
    (progn
      (setq first-1000 (cdr (assoc 1000 lfx-vals)))
      (cond
        ((= first-1000 "Head") 'HEAD)
        ((= first-1000 "VALVECALLOUT")
          (if (HN-GetTagInfo ent)
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

(defun HN-CheckCollision (hole loc num label)
  "Returns T if safe to proceed (no collision, or collision and user approved renumber), nil if cancelled."
  (if (TagExists hole loc num)
    (progn
      (princ (strcat "\nWARNING: " label " already exists!"))
      (initget "Yes No")
      (if (= (getkword "\nRenumber existing tags to make room? [Yes/No] <No>: ") "Yes")
        (progn (RenumberTags hole loc num) T)
        nil
      )
    )
    T
  )
)

(defun ReplaceHeadTag (existing-tag hole location number head-ent
                       / xdata xdata-vals old-hole old-loc old-num old-label new-label
                         head-handle response
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
      (if (not (HN-CheckCollision hole location number new-label))
        (progn (princ "\nRenumber cancelled.") (setq response nil))
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
          (setq m-num  (caddr manual-settings))
          (setq m-label (strcat (FormatHoleNumber m-hole) m-loc (FormatNumber m-num)))
          ; Check collision at manual target
          (if (not (HN-CheckCollision m-hole m-loc m-num m-label))
            (progn (princ "\nManual reassign cancelled.") (setq manual-settings nil))
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

;;; --- UTILITY FUNCTIONS ---

(defun MakeRange (start end / result i)
  (setq result '() i start)
  (while (<= i end) (setq result (append result (list i)) i (1+ i)))
  result
)

(defun FormatHoleNumber (n)
  (if (< n 10) (strcat "0" (itoa n)) (itoa n))
)

(defun FormatNumber (n)
  (cond ((< n 10)  (strcat "00" (itoa n)))
        ((< n 100) (strcat "0"  (itoa n)))
        (T         (itoa n)))
)

;;; --- SHARED HELPERS ---

(defun HN-GetLFXVals (ent)
  "Return the LandFX XDATA values list for an INSERT entity, or nil."
  (cdr (assoc "LandFX" (cdr (assoc -3 (entget ent '("LandFX"))))))
)

(defun HN-GetTagInfo (tag-ent / xd xd-inner)
  "Extract HEADNUM XDATA from a tag block.
   Returns (hole-str loc-str num-str head-handle) or nil."
  (if (and tag-ent
           (setq xd (assoc -3 (entget tag-ent '("HEADNUM"))))
           (setq xd-inner (cdr (assoc "HEADNUM" (cdr xd))))
           (>= (length xd-inner) 4))
    (list
      (cdr (nth 0 xd-inner))  ; hole as string e.g. "1"
      (cdr (nth 1 xd-inner))  ; location e.g. "FW"
      (cdr (nth 2 xd-inner))  ; number e.g. "007"
      (cdr (nth 3 xd-inner))  ; 1005 head handle
    )
    nil
  )
)

(defun HN-WriteCSV (path rows / fh)
  "Write PNEZD CSV to path from rows list of (point N E Z desc).
   Returns T on success, nil if file cannot be opened."
  (setq fh (open path "w"))
  (if fh
    (progn
      (write-line "Point,Northing,Easting,Elevation,Description" fh)
      (foreach row rows
        (write-line
          (strcat (car row) "," (cadr row) "," (caddr row) ","
                  (nth 3 row) "," (nth 4 row))
          fh))
      (close fh)
      T
    )
    nil
  )
)

(defun HN-ScanValveRows (/ ss i ent ent-data lfx-vals first-1000 pos E-str N-str v12 desc-str rows)
  "Scan all LandFX INSERT blocks and return sorted PNEZD rows for Valve entities."
  (setq rows '())
  (if (setq ss (ssget "_X" (list '(0 . "INSERT") '(-3 ("LandFX")))))
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq ent      (ssname ss i))
        (setq ent-data (entget ent '("LandFX")))
        (if (setq lfx-vals (HN-GetLFXVals ent))
          (progn
            (setq first-1000 (cdr (assoc 1000 lfx-vals)))
            (if (= first-1000 "Valve")
              (progn
                (setq pos      (cdr (assoc 10 ent-data)))
                (setq E-str    (rtos (car  pos) 2 4))
                (setq N-str    (rtos (cadr pos) 2 4))
                (setq v12      (nth 12 lfx-vals))
                (setq desc-str (if (and v12 (= (car v12) 1000) (> (strlen (cdr v12)) 0))
                                 (cdr v12) "VALVE"))
                (setq rows (append rows (list (list "" N-str E-str "0" desc-str))))
              )
            )
          )
        )
        (setq i (1+ i))
      )
    )
  )
  (vl-sort rows
    '(lambda (a b)
       (cond
         ((< (nth 4 a) (nth 4 b)) T)
         ((= (nth 4 a) (nth 4 b)) (< (atof (caddr a)) (atof (caddr b))))
         (T nil)
       )
     )
  )
)

;;; --- DIALOG FUNCTIONS ---

(defun BuildTagListItems (hole location / tags sorted expected items num tag-ent)
  "Build list of (label . tag-ent-or-nil) pairs for dialog list box, including gap markers."
  (setq tags  (GetAllHeadTags hole location)
        items '())
  (if (not tags)
    (list (cons "  (none)" nil))
    (progn
      (setq sorted   (vl-sort tags '(lambda (a b) (< (cadr a) (cadr b))))
            expected 1)
      (foreach tag sorted
        (setq num     (cadr tag)
              tag-ent (car  tag))
        (while (< expected num)
          (setq items (append items
            (list (cons (strcat (FormatHoleNumber hole) location (FormatNumber expected) "  -- GAP") nil))
          ))
          (setq expected (1+ expected))
        )
        (setq items (append items
          (list (cons (strcat (FormatHoleNumber hole) location (FormatNumber num)) tag-ent))
        ))
        (setq expected (1+ num))
      )
      items
    )
  )
)


(defun RefreshTagList (hole location / items)
  (setq items (BuildTagListItems hole location))
  (start_list "tag_list") (mapcar '(lambda (x) (add_list (car x))) items) (end_list)
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
  (set_tile "override_num" (strcat (FormatHoleNumber hole) location (FormatNumber next-num)))
  (mode_tile "fill_gaps_btn"  (if (> gap-count 0) 0 1))
  (mode_tile "clear_num_btn" 8)
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

;;; --- EXPORT DIALOG HELPERS ---

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

(defun GetHeadDescStr (head-ent / lfx-vals v2 v3)
  "Return 'Model-Nozzle' desc string from a head block's LandFX XDATA"
  (if (and head-ent (setq lfx-vals (HN-GetLFXVals head-ent)))
    (progn
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
                           E-str N-str desc-str row rows csv-path valve-rows
                           ss i ent ent-data
                           u-tagged-h u-ss u-i u-ent u-data u-vals
                           u-handle u-tag-ss u-j u-tag-ent u-hh)
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
              (setq u-data (entget u-ent))
              (if (and (setq u-vals (HN-GetLFXVals u-ent))
                       (= (cdr (assoc 1000 u-vals)) "Head"))
                (progn
                  (setq u-handle (cdr (assoc 5 u-data)))
                  (if (not (member u-handle u-tagged-h))
                    (progn
                      (setq head-pos (cdr (assoc 10 u-data)))
                      (setq E-str    (rtos (car  head-pos) 2 4))
                      (setq N-str    (rtos (cadr head-pos) 2 4))
                      (setq desc-str (GetHeadDescStr u-ent))
                      (setq rows (append rows (list (list "" N-str E-str "0" desc-str))))
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
            (if (HN-WriteCSV csv-path rows)
              (progn
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
                    (list (list (strcat (FormatHoleNumber hole) loc (FormatNumber num)) N-str E-str "0" desc-str))
                  ))
                )
              )
            )
            (if rows
              (progn
                (setq csv-path (strcat dwg-folder dwg-name "_"
                                       (FormatHoleNumber hole) loc ".csv"))
                (if (HN-WriteCSV csv-path rows)
                  (progn
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
      (setq valve-rows (HN-ScanValveRows))
      (if valve-rows
        (progn
          (setq csv-path (strcat dwg-folder dwg-name "_VALVES.csv"))
          (if (HN-WriteCSV csv-path valve-rows)
            (progn
              (princ (strcat "\nWrote: " csv-path " (" (itoa (length valve-rows)) " valves)"))
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
  (setq dcl-path (cond ((findfile "HeadNumbering.dcl")) (T (strcat *HN-DIR* "\\" "HeadNumbering.dcl"))))
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

(defun HN-ZoomToEnt (ent / pt scrsize vh vw twist cx cy hs)
  "Zoom viewport to keep current scale, shift entity to right of center (clear of dialog), highlight."
  (setq pt (cdr (assoc 10 (entget ent))))
  (if pt
    (progn
      (setq scrsize (getvar "SCREENSIZE")
            vh      (getvar "VIEWSIZE")
            vw      (* vh (/ (float (car scrsize)) (cadr scrsize)))
            twist   (getvar "VIEWTWIST")
            cx      (- (car  pt) (* (/ vw 4.0) (cos twist)))
            cy      (- (cadr pt) (* (/ vw 4.0) (sin twist))))
      (command "_.ZOOM" "_C" (list cx cy) vh)
    )
    (command "_.ZOOM" "_O" ent "")
  )
  (command "_.REDRAW")
  (setq hs (ssadd ent (ssadd)))
  (sssetfirst hs hs)
)

(defun HN-FindHead (tag-ent / info)
  "Return the linked head entity from a tag block's HEADNUM xdata, or nil."
  (if (and tag-ent (setq info (HN-GetTagInfo tag-ent)) (nth 3 info))
    (handent (nth 3 info))
    nil
  )
)

(defun ShowSettingsDialog (current-hole current-location / dcl-path dcl_id hole-choice location-choice
                            result list-items selected-idx chosen-num tag-ent head-ent auto-num)
  "Display DCL dialog and return (hole location override-number)"

  (if (null *HN-DIR*)
    (progn
      (alert "Cannot find Head Numbering.lsp\nAdd the TCLLC Numbering folder to:\nOptions > Files > Support File Search Path")
      (exit)
    )
  )
  (setq dcl-path (cond ((findfile "HeadNumbering.dcl")) (T (strcat *HN-DIR* "\\" "HeadNumbering.dcl"))))
  ; Initialize state before loop - these persist across zoom re-opens
  (setq hole-choice     (if current-hole     (itoa (1- current-hole)) "0")
        location-choice (if current-location (itoa (vl-position current-location *HN-Locations*)) "0")
        selected-idx    -1
        chosen-num      "")

  ; While loop: re-opens dialog after Find Tag/Head zoom (done_dialog 2/3), exits on OK/Cancel
  (while (progn
    (setq dcl_id (load_dialog dcl-path))
    (if (not (new_dialog "HeadNumDialog" dcl_id))
      (progn (alert "ERROR: Could not load dialog!") (exit))
    )

    ; Populate dropdowns
    (start_list "hole_list")     (mapcar 'add_list (mapcar 'itoa (MakeRange 1 *HN-MaxHole*))) (end_list)
    (start_list "location_list") (mapcar 'add_list *HN-Locations*)                            (end_list)

    ; Restore state
    (set_tile "hole_list"     hole-choice)
    (set_tile "location_list" location-choice)
    (setq list-items (RefreshTagList (1+ (atoi hole-choice)) (nth (atoi location-choice) *HN-Locations*)))
    (HN-RefreshStatus          (1+ (atoi hole-choice)) (nth (atoi location-choice) *HN-Locations*))
    (setq auto-num (get_tile "override_num"))
    (if (> (strlen chosen-num) 0)
      (progn
        (set_tile "override_num" chosen-num)
        (if (/= chosen-num auto-num)
          (mode_tile "clear_num_btn" 0)
        )
      )
    )
    (if (>= selected-idx 0)
      (progn
        (set_tile "tag_list" (itoa selected-idx))
        (mode_tile "find_tag_btn"  (if (cdr (nth selected-idx list-items)) 0 1))
        (mode_tile "find_head_btn" (if (cdr (nth selected-idx list-items)) 0 1))
      )
    )

    ; Action tiles
    (action_tile "hole_list"
      "(setq hole-choice $value  selected-idx -1)
       (setq list-items (RefreshTagList (1+ (atoi hole-choice)) (nth (atoi (get_tile \"location_list\")) *HN-Locations*)))
       (mode_tile \"find_tag_btn\" 1)  (mode_tile \"find_head_btn\" 1)
       (HN-RefreshStatus (1+ (atoi hole-choice)) (nth (atoi (get_tile \"location_list\")) *HN-Locations*))
       (setq auto-num (get_tile \"override_num\"))"
    )
    (action_tile "location_list"
      "(setq location-choice $value  selected-idx -1)
       (setq list-items (RefreshTagList (1+ (atoi (get_tile \"hole_list\"))) (nth (atoi location-choice) *HN-Locations*)))
       (mode_tile \"find_tag_btn\" 1)  (mode_tile \"find_head_btn\" 1)
       (HN-RefreshStatus (1+ (atoi (get_tile \"hole_list\"))) (nth (atoi location-choice) *HN-Locations*))
       (setq auto-num (get_tile \"override_num\"))"
    )
    (action_tile "tag_list"
      "(setq selected-idx (atoi $value))
       (set_tile \"override_num\" (substr (car (nth selected-idx list-items)) 1 7))
       (mode_tile \"clear_num_btn\" 0)
       (if (cdr (nth selected-idx list-items))
         (progn (mode_tile \"find_tag_btn\" 0)  (mode_tile \"find_head_btn\" 0))
         (progn (mode_tile \"find_tag_btn\" 1)  (mode_tile \"find_head_btn\" 1))
       )"
    )
    (action_tile "override_num"
      "(if (/= (get_tile \"override_num\") auto-num)
         (mode_tile \"clear_num_btn\" 0)
         (mode_tile \"clear_num_btn\" 1)
       )"
    )
    (action_tile "clear_num_btn"
      "(HN-RefreshStatus (1+ (atoi (get_tile \"hole_list\"))) (nth (atoi (get_tile \"location_list\")) *HN-Locations*))
       (setq auto-num (get_tile \"override_num\"))
       (setq list-items (RefreshTagList (1+ (atoi (get_tile \"hole_list\"))) (nth (atoi (get_tile \"location_list\")) *HN-Locations*)))
       (setq selected-idx -1)
       (mode_tile \"find_tag_btn\" 1) (mode_tile \"find_head_btn\" 1)"
    )
    (action_tile "fill_gaps_btn"
      "(setq hole-choice (get_tile \"hole_list\")
             location-choice (get_tile \"location_list\")
             chosen-num (get_tile \"override_num\"))
       (done_dialog 4)"
    )
    (action_tile "export_btn" "(HN-RunExportDialog)")
    ; Find buttons: capture state then dismiss with zoom codes 2/3
    (action_tile "find_tag_btn"
      "(setq hole-choice (get_tile \"hole_list\")
             location-choice (get_tile \"location_list\")
             chosen-num (get_tile \"override_num\"))
       (done_dialog 2)"
    )
    (action_tile "find_head_btn"
      "(setq hole-choice (get_tile \"hole_list\")
             location-choice (get_tile \"location_list\")
             chosen-num (get_tile \"override_num\"))
       (done_dialog 3)"
    )
    (action_tile "accept"
      "(setq hole-choice (get_tile \"hole_list\")
             location-choice (get_tile \"location_list\")
             chosen-num (get_tile \"override_num\"))
       (done_dialog 1)"
    )
    (action_tile "cancel" "(done_dialog 0)")

    (setq result (start_dialog))
    (unload_dialog dcl_id)

    ; Return T to continue loop, nil to exit (OK/Cancel)
    (cond
      ((= result 2)  ; Find Tag
        (setq tag-ent (if (>= selected-idx 0) (cdr (nth selected-idx list-items)) nil))
        (if tag-ent (HN-ZoomToEnt tag-ent))
        T
      )
      ((= result 3)  ; Find Head
        (setq tag-ent  (if (>= selected-idx 0) (cdr (nth selected-idx list-items)) nil)
              head-ent (HN-FindHead tag-ent))
        (if head-ent (HN-ZoomToEnt head-ent))
        T
      )
      ((= result 4)  ; Fill Gaps - run outside dialog so drawing updates are visible
        (FillGaps (1+ (atoi hole-choice)) (nth (atoi location-choice) *HN-Locations*))
        (command "_.REDRAW")
        (setq selected-idx -1)
        T
      )
      (T nil)  ; OK (1) or Cancel (0) - exit loop
    )
  ))

  (if (= result 1)
    (list
      (1+ (atoi hole-choice))
      (nth (atoi location-choice) *HN-Locations*)
      ; chosen-num is "01FW022" (full) or just "022" if user typed digits only
      (if (>= (strlen chosen-num) 5)
        (atoi (substr chosen-num 5))
        (atoi chosen-num)
      )
    )
    nil
  )
)

;;; --- MAIN COMMAND ---

(defun c:NUMBERHEADS (/ settings current-hole current-location current-number existing-tags
                        head-ent head-handle head-pt tag-ent place-pt existing-tag
                        user-input parent-list response highest collision-response done
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
  
  (setq current-number (caddr settings))
  
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

      ; User typed "Manual", "Change" or shortcuts - open dialog
      ((or (= user-input "Manual") (= user-input "M")
           (= user-input "Change") (= user-input "C"))
        (if (setq settings (ShowSettingsDialog current-hole current-location))
          (progn
            (setq current-hole      (car settings)
                  current-location  (cadr settings)
                  *HN-LastHole*     current-hole
                  *HN-LastLocation* current-location
                  current-number    (caddr settings))
            (princ (strcat "\nNext: " (FormatHoleNumber current-hole) current-location (FormatNumber current-number)))
          )
          (princ "\nCancelled.")
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
(princ "\n  EXPORTHEADS        - Export head locations to PNEZD CSV per location")
(princ "\nBlock required: LAFX-TAG-SQUARE-999 (with XX attribute)")
(princ "\n==========================================\n")
(princ)

;;; --- UTILITY COMMANDS - FIND TAG/HEAD ---

(defun c:FINDTAG (/ head-sel head-ent head-handle tag-ent i ss test-tag
                    xdata xdata-section att-ent parent-list)
  "Click a head block to find and highlight its linked tag"
  (setq head-sel (nentsel "\nSelect head block: "))
  (if head-sel
    (progn
      (setq head-ent (car head-sel))
      (if (= (length head-sel) 4)
        (if (and (cadddr head-sel) (listp (cadddr head-sel)))
          (setq head-ent (last (cadddr head-sel)))
        )
      )
      (setq head-handle (cdr (assoc 5 (entget head-ent))))
      (princ (strcat "\nHead: " (cdr (assoc 2 (entget head-ent))) "  handle: " head-handle))
      (if (setq tag-ent (HeadHasTag head-ent))
        (progn
          (princ "\n>>> Found LAFX-TAG-SQUARE-999 (TCLLC System) <<<")
          (if (setq xdata (assoc -3 (entget tag-ent '("HEADNUM"))))
            (if (setq xdata (cdr (assoc "HEADNUM" (cdr xdata))))
              (princ (strcat "\nTag number: " (cdr (nth 0 xdata)) (cdr (nth 1 xdata)) (cdr (nth 2 xdata))))
            )
          )
          (redraw tag-ent 3)
          (princ "\nTag highlighted! Press Enter to continue...")
          (getstring)
          (redraw tag-ent 4)
        )
        (progn
          (princ "\n>>> Checking for other tag systems... <<<")
          (if (setq ss (ssget "_X" '((0 . "INSERT")(2 . "LAFX-TAG*"))))
            (progn
              (princ (strcat "\nFound " (itoa (sslength ss)) " LAFX-TAG blocks"))
              (setq i 0 tag-ent nil)
              (while (and (< i (sslength ss)) (not tag-ent))
                (setq test-tag     (ssname ss i)
                      xdata-section (assoc -3 (entget test-tag '("*"))))
                (if xdata-section
                  (foreach app-data (cdr xdata-section)
                    (if (listp app-data)
                      (foreach pair (cdr app-data)
                        (if (and (listp pair) (= (car pair) 1005) (equal (cdr pair) head-handle))
                          (progn (setq tag-ent test-tag)
                                 (princ (strcat "\n>>> Found LAFX-TAG  app: " (car app-data) " <<<"))
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
                  (setq att-ent (entnext tag-ent))
                  (while (and att-ent (= "ATTRIB" (cdr (assoc 0 (entget att-ent)))))
                    (princ (strcat "\nValve number: " (cdr (assoc 1 (entget att-ent)))))
                    (setq att-ent (entnext att-ent))
                  )
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

(defun c:FINDHEAD (/ tag-sel tag-ent tag-name xdata xdata-vals head-handle head-ent head-pos att-ent)
  "Click a tag block to find and highlight its linked head"
  (setq tag-sel (entsel "\nSelect tag block: "))
  (if tag-sel
    (progn
      (setq tag-ent  (car tag-sel)
            tag-name (cdr (assoc 2 (entget tag-ent))))
      (princ (strcat "\nSelected: " tag-name))
      (cond
        ; TCLLC tag
        ((= "LAFX-TAG-SQUARE-999" tag-name)
          (princ "\n>>> LAFX-TAG-SQUARE-999 (TCLLC System) <<<")
          (if (setq xdata (assoc -3 (entget tag-ent '("HEADNUM"))))
            (if (setq xdata-vals (cdr (assoc "HEADNUM" (cdr xdata))))
              (if (setq head-handle (cdr (assoc 1005 xdata-vals)))
                (if (setq head-ent (handent head-handle))
                  (progn
                    (princ (strcat "\nFound: " (cdr (assoc 2 (entget head-ent))) "  handle: " head-handle))
                    (redraw head-ent 3)
                    (princ "\nHead highlighted! Press Enter to continue...")
                    (getstring)
                    (redraw head-ent 4)
                  )
                  (princ "\nLinked head not found (broken reference).")
                )
                (princ "\nNo head link found in tag XDATA.")
              )
              (princ "\nNo HEADNUM data found in tag.")
            )
            (princ "\nNo XDATA found on this tag.")
          )
        )
        ; LandFX tag
        ((wcmatch tag-name "LAFX-TAG*")
          (princ "\n>>> LAFX-TAG (Land F/X System) <<<")
          (setq att-ent (entnext tag-ent))
          (while (and att-ent (= "ATTRIB" (cdr (assoc 0 (entget att-ent)))))
            (princ (strcat "\nValve number: " (cdr (assoc 1 (entget att-ent)))))
            (setq att-ent (entnext att-ent))
          )
          (setq head-handle nil)
          (if (setq xdata (assoc -3 (entget tag-ent '("*"))))
            (foreach app-data (cdr xdata)
              (if (listp app-data)
                (foreach pair (cdr app-data)
                  (if (and (listp pair) (= (car pair) 1005))
                    (setq head-handle (cdr pair))
                  )
                )
              )
            )
          )
          (if head-handle
            (if (setq head-ent (handent head-handle))
              (progn
                (setq head-pos (cdr (assoc 10 (entget head-ent))))
                (princ (strcat "\nFound: " (cdr (assoc 2 (entget head-ent))) "  handle: " head-handle))
                (princ (strcat "\nPos: " (rtos (car head-pos) 2 4) "," (rtos (cadr head-pos) 2 4)))
                (redraw head-ent 3)
                (princ "\nHead highlighted! Press Enter to continue...")
                (getstring)
                (redraw head-ent 4)
              )
              (princ "\nLinked head not found (broken reference).")
            )
            (princ "\nNo head link (1005) found in XDATA.")
          )
        )
        (T (princ "\nNot a recognized tag block."))
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

(defun c:EXPORTHEADS (/ tag-ss tag-ent tag-info tag-map j
                        ss i ent ent-data pos E-str N-str
                        lfx-vals desc-str
                        head-handle pt-num bucket-key row
                        buckets loc-key rows sorted-rows
                        dwg-folder dwg-name csv-path
                        head-count unnumbered-count written-files
                        valve-rows)
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
        (if (setq tag-info (HN-GetTagInfo tag-ent))
          (setq tag-map
            (cons
              (list (nth 3 tag-info)   ; [0] head handle (key for assoc)
                    (nth 0 tag-info)   ; [1] hole as string e.g. "1"
                    (nth 1 tag-info)   ; [2] location e.g. "FW"
                    (nth 2 tag-info))  ; [3] number e.g. "007"
              tag-map
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

    (if (and (setq lfx-vals (HN-GetLFXVals ent))
             (= (cdr (assoc 1000 lfx-vals)) "Head"))
      (progn
        (setq head-count (1+ head-count))

        ; Insertion point → E (X) and N (Y)
        (setq pos   (cdr (assoc 10 ent-data)))
        (setq E-str (rtos (car  pos) 2 4))
        (setq N-str (rtos (cadr pos) 2 4))

        (setq desc-str (GetHeadDescStr ent))

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
    (if (HN-WriteCSV csv-path sorted-rows)
      (progn
        (princ (strcat "\nWrote: " csv-path " (" (itoa (length sorted-rows)) " rows)"))
        (setq written-files (1+ written-files))
      )
      (princ (strcat "\nERROR: Cannot write to " csv-path))
    )
  )

  ; -------------------------------------------------------
  ; Step 4: Scan valve blocks — write [DWG]_VALVES.csv
  ; -------------------------------------------------------
  (setq valve-rows (HN-ScanValveRows))
  (if valve-rows
    (progn
      (setq csv-path (strcat dwg-folder dwg-name "_VALVES.csv"))
      (if (HN-WriteCSV csv-path valve-rows)
        (progn
          (princ (strcat "\nWrote: " csv-path " (" (itoa (length valve-rows)) " valves)"))
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

