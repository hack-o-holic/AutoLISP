;; WIRECALC.LSP - Calculate total length with GUI layer selection
;; Enhanced version with scrollable checkbox dialog for _SUB and _TRUNK layer selection
;; Excludes: SPLICE_TRUNK and TSP_SUB layers
;; Usage: Type WIRECALC at the command line
;; For AutoCAD Civil 3D 2026
;; Uses scrolled_column with checkboxes for better UX and scalability

;; Function to get filtered layer names (_SUB and _TRUNK only, excluding SPLICE and TSP)
(defun get-filtered-layers (/ layer-table layer-obj layer-list layer-name)
  (setq layer-table (tblnext "LAYER" T))
  (setq layer-list '())
  
  (while layer-table
    (setq layer-name (cdr (assoc 2 layer-table)))
    ;; Include layers with "_SUB" or "_TRUNK" but exclude "SPLICE_TRUNK" and "TSP_SUB"
    (if (and 
          (or (wcmatch layer-name "*_SUB*") (wcmatch layer-name "*_TRUNK*"))
          (not (wcmatch layer-name "*_SPLICE_TRUNK*"))
          (not (wcmatch layer-name "*_TSP_SUB*"))
        )
      (setq layer-list (cons layer-name layer-list))
    )
    (setq layer-table (tblnext "LAYER"))
  )
  
  ;; Sort the layer list alphabetically
  (setq layer-list (vl-sort layer-list '<))
  layer-list
)

;; Function to extract prefix from layer name (XX_XXXX from XX_XXXX_SUB or XX_XXXX_TRUNK)
(defun extract-prefix (layer-name / suffix-pos)
  (cond
    ((wcmatch layer-name "*_SUB")
     (substr layer-name 1 (- (strlen layer-name) 4))  ; Remove "_SUB"
    )
    ((wcmatch layer-name "*_TRUNK")
     (substr layer-name 1 (- (strlen layer-name) 6))  ; Remove "_TRUNK"
    )
    (T layer-name)
  )
)

;; Function to extract 2-digit prefix from layer name (first 2 chars)
(defun extract-2digit-prefix (layer-name / prefix)
  (setq prefix (extract-prefix layer-name))
  (if (>= (strlen prefix) 2)
    (substr prefix 1 2)
    prefix
  )
)

;; Function to extract group type letter (H or C) from layer name
(defun extract-group-type (layer-name / prefix)
  (setq prefix (extract-prefix layer-name))
  (if (>= (strlen prefix) 4)
    (substr prefix 4 1)  ; Position 4 is the H or C
    ""
  )
)

;; Function to extract group number (1, 2, 3, etc.) from layer name
(defun extract-group-number (layer-name / prefix)
  (setq prefix (extract-prefix layer-name))
  (if (>= (strlen prefix) 5)
    (substr prefix 5 1)  ; Position 5 is the number
    ""
  )
)

;; Function to extract wire group (chars 6-7: WX) from layer name
(defun extract-wire-group (layer-name / prefix)
  (setq prefix (extract-prefix layer-name))
  (if (>= (strlen prefix) 7)
    (substr prefix 6 2)
    ""
  )
)

;; Function to get unique 2-digit prefixes from layer list
(defun get-unique-2digit-prefixes (layer-list / prefixes prefix)
  (setq prefixes '())
  (foreach layer layer-list
    (setq prefix (extract-2digit-prefix layer))
    (if (and (not (member prefix prefixes)) (/= prefix ""))
      (setq prefixes (cons prefix prefixes))
    )
  )
  ;; Sort alphabetically
  (vl-sort prefixes '<)
)

;; Function to get group info for a specific prefix
;; Returns (group-type . list-of-numbers)
;; group-type is "H" or "C", list-of-numbers is ("1" "2" "3" ...)
(defun get-prefix-group-info (prefix layer-list / group-type numbers type num)
  (setq group-type "")
  (setq numbers '())
  
  (foreach layer layer-list
    (if (= (extract-2digit-prefix layer) prefix)
      (progn
        (setq type (extract-group-type layer))
        (setq num (extract-group-number layer))
        
        ;; Set group type if not yet set
        (if (= group-type "")
          (setq group-type type)
        )
        
        ;; Add number if not already in list
        (if (and (/= num "") (not (member num numbers)))
          (setq numbers (cons num numbers))
        )
      )
    )
  )
  
  ;; Sort numbers
  (setq numbers (vl-sort numbers '<))
  
  (cons group-type numbers)
)

;; Function to get unique wire groups from layer list
(defun get-unique-wire-groups (layer-list / groups group)
  (setq groups '())
  (foreach layer layer-list
    (setq group (extract-wire-group layer))
    (if (and (not (member group groups)) (/= group ""))
      (setq groups (cons group groups))
    )
  )
  ;; Sort alphabetically
  (vl-sort groups '<)
)

;; Function to create DCL file with dynamic prefix sections
(defun create-dynamic-filter-dcl (prefix-data wire-groups / dcl-file dcl-path prefix group-type numbers group-label item-label num-idx prefix-info num wire)
  ;; prefix-data is a list of (prefix . (group-type . numbers))
  ;; Example: (("32" . ("H" . ("1" "2"))) ("15" . ("C" . ("1" "3"))))
  
  (setq dcl-path (strcat (getvar "TEMPPREFIX") "wirecalc_filter.dcl"))
  (setq dcl-file (open dcl-path "w"))
  
  (if (null dcl-file)
    (progn
      (princ "\nError: Cannot create DCL file!")
      nil
    )
    (progn
      (write-line "wirecalc_filter : dialog {" dcl-file)
      (write-line "  label = \"Wire Length Calculator - Select Filters\";" dcl-file)
      (write-line "  : column {" dcl-file)
      
      ;; Create a section for each prefix
      (foreach prefix-info prefix-data
        (setq prefix (car prefix-info))
        (setq group-type (car (cdr prefix-info)))
        (setq numbers (cdr (cdr prefix-info)))
        
        ;; Determine group label based on type
        (if (= group-type "H")
          (setq group-label (strcat "Toro Hubs (" prefix "_HX):"))
          (setq group-label (strcat "Rain Bird ICIs (" prefix "_CX):"))
        )
        
        ;; Create boxed column for this prefix
        (write-line "    : boxed_column {" dcl-file)
        (write-line (strcat "      label = \"" group-label "\";") dcl-file)
        (write-line "      : column {" dcl-file)
        (write-line "        width = 50;" dcl-file)
        
        ;; Add Select All/None toggle at top
        (write-line "        : toggle {" dcl-file)
        (write-line (strcat "          key = \"selectall_" prefix "\";") dcl-file)
        (write-line "          label = \"Select All/None\";" dcl-file)
        (write-line "          value = \"0\";" dcl-file)
        (write-line "        }" dcl-file)
        
        ;; Create checkboxes for each number - use actual code like H1, H2, C1, C2
        (setq num-idx 0)
        (foreach num numbers
          (setq item-label (strcat group-type num))
          
          (write-line "        : toggle {" dcl-file)
          (write-line (strcat "          key = \"" prefix "_" num "\";") dcl-file)
          (write-line (strcat "          label = \"" item-label "\";") dcl-file)
          (write-line "          value = \"0\";" dcl-file)
          (write-line "        }" dcl-file)
          (setq num-idx (1+ num-idx))
        )
        
        (write-line "      }" dcl-file)    ; Close column
        (write-line "    }" dcl-file)      ; Close boxed_column
      )
      
      ;; Wire Group section
      (write-line "    : boxed_column {" dcl-file)
      (write-line "      label = \"Wire Groups (WX):\";" dcl-file)
      (write-line "      : column {" dcl-file)
      (write-line "        width = 50;" dcl-file)
      
      ;; Add Select All/None toggle at top
      (write-line "        : toggle {" dcl-file)
      (write-line "          key = \"selectall_wires\";" dcl-file)
      (write-line "          label = \"Select All/None\";" dcl-file)
      (write-line "          value = \"0\";" dcl-file)
      (write-line "        }" dcl-file)
      
      ;; Create checkboxes for each wire group
      (foreach wire wire-groups
        (write-line "        : toggle {" dcl-file)
        (write-line (strcat "          key = \"wire_" wire "\";") dcl-file)
        (write-line (strcat "          label = \"" wire "\";") dcl-file)
        (write-line "          value = \"0\";" dcl-file)
        (write-line "        }" dcl-file)
      )
      
      (write-line "      }" dcl-file)    ; Close column
      (write-line "    }" dcl-file)      ; Close boxed_column for wires
      
      ;; Type section
      (write-line "    : boxed_column {" dcl-file)
      (write-line "      label = \"Select Types:\";" dcl-file)
      (write-line "      : toggle {" dcl-file)
      (write-line "        key = \"type_sub\";" dcl-file)
      (write-line "        label = \"SUB\";" dcl-file)
      (write-line "        value = \"0\";" dcl-file)
      (write-line "      }" dcl-file)
      (write-line "      : toggle {" dcl-file)
      (write-line "        key = \"type_trunk\";" dcl-file)
      (write-line "        label = \"TRUNK\";" dcl-file)
      (write-line "        value = \"0\";" dcl-file)
      (write-line "      }" dcl-file)
      (write-line "    }" dcl-file)      ; Close boxed_column for types
      
      (write-line "    ok_cancel;" dcl-file)
      (write-line "  }" dcl-file)
      (write-line "}" dcl-file)
      
      (close dcl-file)
      dcl-path
    )
  )
)

;; Function to show dynamic filter dialog
(defun show-dynamic-filter-dialog (prefix-data wire-groups all-layers / dcl-file dcl-id selected-layers result prefix group-type numbers type-sub type-trunk selected-combinations selected-wires wire layer-name tile-key num-list action-str combo num prefix-info)
  (setq selected-layers '())
  
  ;; Create DCL file
  (setq dcl-file (create-dynamic-filter-dcl prefix-data wire-groups))
  
  (if (null dcl-file)
    (progn
      (princ "\nError creating dialog file!")
      nil
    )
    (progn
      ;; Load and display dialog
      (setq dcl-id (load_dialog dcl-file))
      
      (if (not (new_dialog "wirecalc_filter" dcl-id))
        (progn
          (princ "\nError loading dialog!")
          (if (> dcl-id 0) (unload_dialog dcl-id))
          nil
        )
        (progn
          ;; Set up Select All/None toggle for each prefix
          (foreach prefix-info prefix-data
            (setq prefix (car prefix-info))
            (setq numbers (cdr (cdr prefix-info)))
            
            ;; Build action string for Select All/None toggle
            (setq action-str "(progn (if (= $value \"1\") (progn")
            ;; If checked, select all
            (foreach num numbers
              (setq action-str (strcat action-str " (set_tile \"" prefix "_" num "\" \"1\")"))
            )
            (setq action-str (strcat action-str ") (progn"))
            ;; If unchecked, deselect all
            (foreach num numbers
              (setq action-str (strcat action-str " (set_tile \"" prefix "_" num "\" \"0\")"))
            )
            (setq action-str (strcat action-str ")))"))
            (action_tile (strcat "selectall_" prefix) action-str)
          )
          
          ;; Set up Select All/None toggle for wires
          (setq action-str "(progn (if (= $value \"1\") (progn")
          ;; If checked, select all
          (foreach wire wire-groups
            (setq action-str (strcat action-str " (set_tile \"wire_" wire "\" \"1\")"))
          )
          (setq action-str (strcat action-str ") (progn"))
          ;; If unchecked, deselect all
          (foreach wire wire-groups
            (setq action-str (strcat action-str " (set_tile \"wire_" wire "\" \"0\")"))
          )
          (setq action-str (strcat action-str ")))"))
          (action_tile "selectall_wires" action-str)
          
          ;; Set up OK button - collect all selections
          (setq action-str "(progn (setq selected-combinations '())")
          
          ;; Collect prefix/number selections
          (foreach prefix-info prefix-data
            (setq prefix (car prefix-info))
            (setq group-type (car (cdr prefix-info)))
            (setq numbers (cdr (cdr prefix-info)))
            
            (foreach num numbers
              (setq tile-key (strcat prefix "_" num))
              (setq action-str (strcat action-str " (if (= (get_tile \"" tile-key "\") \"1\") (setq selected-combinations (cons (list \"" prefix "\" \"" group-type "\" \"" num "\") selected-combinations)))"))
            )
          )
          
          ;; Collect wire selections
          (setq action-str (strcat action-str " (setq selected-wires '())"))
          (foreach wire wire-groups
            (setq action-str (strcat action-str " (if (= (get_tile \"wire_" wire "\") \"1\") (setq selected-wires (cons \"" wire "\" selected-wires)))"))
          )
          
          (setq action-str (strcat action-str " (setq type-sub (get_tile \"type_sub\")) (setq type-trunk (get_tile \"type_trunk\")) (done_dialog 1))"))
          (action_tile "accept" action-str)
          
          (action_tile "cancel" "(done_dialog 0)")
          
          ;; Show dialog
          (setq result (start_dialog))
          
          (if (= result 1)
            (progn
              (princ (strcat "\nSelected " (itoa (length selected-combinations)) " Hub/ICI combination(s), " (itoa (length selected-wires)) " wire group(s)"))
              
              ;; Build layer list from Cartesian product
              (if (and (> (length selected-combinations) 0) (> (length selected-wires) 0) (or (= type-sub "1") (= type-trunk "1")))
                (progn
                  ;; Loop through all combinations
                  (foreach combo selected-combinations
                    (setq prefix (car combo))
                    (setq group-type (cadr combo))
                    (setq num (caddr combo))
                    
                    (foreach wire selected-wires
                      (if (= type-sub "1")
                        (progn
                          (setq layer-name (strcat prefix "_" group-type num wire "_SUB"))
                          (if (member layer-name all-layers)
                            (setq selected-layers (cons layer-name selected-layers))
                          )
                        )
                      )
                      (if (= type-trunk "1")
                        (progn
                          (setq layer-name (strcat prefix "_" group-type num wire "_TRUNK"))
                          (if (member layer-name all-layers)
                            (setq selected-layers (cons layer-name selected-layers))
                          )
                        )
                      )
                    )
                  )
                  (setq selected-layers (reverse selected-layers))
                  (princ (strcat "\nBuilt " (itoa (length selected-layers)) " layer(s) from combinations"))
                )
                (princ "\nIncomplete selection (need hub/ici, wires, and type)")
              )
            )
            (progn
              (princ "\nDialog cancelled by user.")
              (setq selected-layers '())
            )
          )
          
          ;; Clean up
          (unload_dialog dcl-id)
        )
      )
      
      ;; Delete DCL file
      (if (and dcl-file (findfile dcl-file))
        (vl-file-delete dcl-file)
      )
      
      selected-layers
    )
  )
)
(defun create-layer-dialog-dcl (layer-list / dcl-file dcl-path i)
  ;; Create DCL file in AutoCAD's temp directory
  (setq dcl-path (strcat (getvar "TEMPPREFIX") "wirecalc_layers.dcl"))
  (setq dcl-file (open dcl-path "w"))
  
  (if (null dcl-file)
    (progn
      (princ "\nError: Cannot create DCL file!")
      nil
    )
    (progn
      ;; Write DCL with scrollable column of checkboxes
      (write-line "wirecalc_layers : dialog {" dcl-file)
      (write-line "  label = \"Wire Length Calculator - Layer Selection\";" dcl-file)
      (write-line "  : column {" dcl-file)
      (write-line "    : text {" dcl-file)
      (write-line "      label = \"Select layers to calculate:\";" dcl-file)
      (write-line "    }" dcl-file)
      (write-line "    : boxed_column {" dcl-file)
      (write-line "      label = \"Available Layers\";" dcl-file)
      (write-line "      : row {" dcl-file)
      (write-line "        : button {" dcl-file)
      (write-line "          key = \"select_all\";" dcl-file)
      (write-line "          label = \"Select All\";" dcl-file)
      (write-line "          width = 12;" dcl-file)
      (write-line "        }" dcl-file)
      (write-line "        : button {" dcl-file)
      (write-line "          key = \"select_none\";" dcl-file)
      (write-line "          label = \"Select None\";" dcl-file)
      (write-line "          width = 12;" dcl-file)
      (write-line "        }" dcl-file)
      (write-line "      }" dcl-file)
      (write-line "      : column {" dcl-file)
      (write-line "        width = 50;" dcl-file)
      (write-line "        height = 20;" dcl-file)
      (write-line "        fixed_height = true;" dcl-file)
      
      ;; Create checkboxes for each layer
      (setq i 0)
      (foreach layer layer-list
        (write-line "        : toggle {" dcl-file)
        (write-line (strcat "          key = \"layer_" (itoa i) "\";") dcl-file)
        (write-line (strcat "          label = \"" layer "\";") dcl-file)
        (write-line "          value = \"1\";" dcl-file)  ; Default to checked
        (write-line "        }" dcl-file)
        (setq i (1+ i))
      )
      
      (write-line "      }" dcl-file)    ; Close column
      (write-line "    }" dcl-file)      ; Close boxed_column
      (write-line "    ok_cancel;" dcl-file)
      (write-line "  }" dcl-file)
      (write-line "}" dcl-file)
      
      (close dcl-file)
      dcl-path
    )
  )
)

;; Function to create summary results DCL
(defun create-summary-dialog-dcl (/ dcl-file dcl-path)
  ;; Create DCL file in AutoCAD's temp directory
  (setq dcl-path (strcat (getvar "TEMPPREFIX") "wirecalc_summary.dcl"))
  (setq dcl-file (open dcl-path "w"))
  
  (if (null dcl-file)
    (progn
      (princ "\nError: Cannot create summary DCL file!")
      nil
    )
    (progn
      (write-line "wirecalc_summary : dialog {" dcl-file)
      (write-line "  label = \"Wire Length Calculator - Results Summary\";" dcl-file)
      (write-line "  : column {" dcl-file)
      (write-line "    : text {" dcl-file)
      (write-line "      label = \"Results by Layer:\";" dcl-file)
      (write-line "    }" dcl-file)
      (write-line "    : list_box {" dcl-file)
      (write-line "      key = \"summary_list\";" dcl-file)
      (write-line "      width = 60;" dcl-file)
      (write-line "      height = 20;" dcl-file)
      (write-line "      fixed_width_font = true;" dcl-file)
      (write-line "    }" dcl-file)
      (write-line "    : text {" dcl-file)
      (write-line "      label = \"Note: Full details available in command line and text file\";" dcl-file)
      (write-line "    }" dcl-file)
      (write-line "    : row {" dcl-file)
      (write-line "      alignment = centered;" dcl-file)
      (write-line "      : button {" dcl-file)
      (write-line "        key = \"copy_to_file\";" dcl-file)
      (write-line "        label = \"Save to Text File\";" dcl-file)
      (write-line "        width = 18;" dcl-file)
      (write-line "      }" dcl-file)
      (write-line "      : button {" dcl-file)
      (write-line "        key = \"run_again\";" dcl-file)
      (write-line "        label = \"Run Again\";" dcl-file)
      (write-line "        width = 12;" dcl-file)
      (write-line "      }" dcl-file)
      (write-line "      : button {" dcl-file)
      (write-line "        key = \"accept\";" dcl-file)
      (write-line "        label = \"Quit\";" dcl-file)
      (write-line "        is_cancel = true;" dcl-file)
      (write-line "        width = 12;" dcl-file)
      (write-line "      }" dcl-file)
      (write-line "    }" dcl-file)
      (write-line "  }" dcl-file)
      (write-line "}" dcl-file)
      
      (close dcl-file)
      dcl-path
    )
  )
)

;; Function to show layer selection dialog with checkboxes
(defun show-layer-dialog (layer-list / dcl-file dcl-id selected-layers result layer-count i)
  (setq selected-layers '())
  
  ;; Create DCL file
  (setq dcl-file (create-layer-dialog-dcl layer-list))
  
  (if (null dcl-file)
    (progn
      (princ "\nError creating dialog file!")
      nil
    )
    (progn
      ;; Load and display dialog
      (setq dcl-id (load_dialog dcl-file))
      
      (if (not (new_dialog "wirecalc_layers" dcl-id))
        (progn
          (princ "\nError loading dialog!")
          (if (> dcl-id 0) (unload_dialog dcl-id))
          nil
        )
        (progn
          (setq layer-count (length layer-list))
          
          ;; Set up select all button action
          (action_tile "select_all" 
            (strcat "(setq i 0)"
                    "(repeat " (itoa layer-count) " "
                    "  (set_tile (strcat \"layer_\" (itoa i)) \"1\")"
                    "  (setq i (1+ i))"
                    ")"
            )
          )
          
          ;; Set up select none button action
          (action_tile "select_none" 
            (strcat "(setq i 0)"
                    "(repeat " (itoa layer-count) " "
                    "  (set_tile (strcat \"layer_\" (itoa i)) \"0\")"
                    "  (setq i (1+ i))"
                    ")"
            )
          )
          
          ;; Set up OK button action - collect checked boxes
          (action_tile "accept" 
            (strcat "(setq selected-layers '())"
                    "(setq i 0)"
                    "(foreach layer layer-list"
                    "  (if (= (get_tile (strcat \"layer_\" (itoa i))) \"1\")"
                    "    (setq selected-layers (cons layer selected-layers))"
                    "  )"
                    "  (setq i (1+ i))"
                    ")"
                    "(setq selected-layers (reverse selected-layers))"
                    "(done_dialog 1)"
            )
          )
          (action_tile "cancel" "(done_dialog 0)")
          
          ;; Show dialog
          (setq result (start_dialog))
          
          (if (= result 1)
            (princ (strcat "\nDialog OK pressed. Selected " (itoa (length selected-layers)) " layers."))
            (progn
              (princ "\nDialog cancelled by user.")
              (setq selected-layers '())
            )
          )
          
          ;; Clean up
          (unload_dialog dcl-id)
        )
      )
      
      ;; Delete DCL file
      (if (and dcl-file (findfile dcl-file))
        (vl-file-delete dcl-file)
      )
      
      selected-layers
    )
  )
)

;; Function to show summary results dialog
;; Returns 1 if user clicks Quit or red X, 2 if user clicks Run Again
(defun show-summary-dialog (summary-text summary-lines / dcl-file dcl-id result text-file f return-value)
  ;; Create DCL file
  (setq dcl-file (create-summary-dialog-dcl))
  
  (if (null dcl-file)
    (progn
      (princ "\nError creating summary dialog file!")
      (setq return-value 1)  ; Return 1 to quit on error
    )
    (progn
      ;; Load and display dialog
      (setq dcl-id (load_dialog dcl-file))
      
      (if (not (new_dialog "wirecalc_summary" dcl-id))
        (progn
          (princ "\nError loading summary dialog!")
          (if (> dcl-id 0) (unload_dialog dcl-id))
          (setq return-value 1)  ; Return 1 to quit on error
        )
        (progn
          ;; Populate the list_box with summary lines
          (start_list "summary_list")
          (mapcar 'add_list summary-lines)
          (end_list)
          
          ;; Set up save to file button - writes file and opens in Notepad
          (action_tile "copy_to_file"
            "(progn (setq text-file (strcat (getvar \"TEMPPREFIX\") \"wirecalc_results.txt\")) (setq f (open text-file \"w\")) (if f (progn (princ summary-text f) (close f) (startapp \"notepad\" text-file) (alert (strcat \"Results saved to:\\n\\n\" text-file))) (alert \"Error: Could not create text file\")))"
          )
          
          ;; Set up Run Again button
          (action_tile "run_again" "(done_dialog 2)")
          
          ;; Set up Quit button  
          (action_tile "accept" "(done_dialog 1)")
          
          ;; Show dialog and capture result
          (setq result (start_dialog))
          
          ;; Clean up
          (unload_dialog dcl-id)
          
          ;; If result is nil or 0 (red X clicked), return 1 to quit
          ;; Otherwise return the actual result (1=Quit, 2=Run Again)
          (setq return-value 
            (if (or (null result) (= result 0))
              1  ; Red X or error - treat as Quit
              result  ; Return actual button value
            )
          )
        )
      )
      
      ;; Delete DCL file
      (if (and dcl-file (findfile dcl-file))
        (vl-file-delete dcl-file)
      )
    )
  )
  
  ;; Explicitly return the captured value
  return-value
)

;; Main function
(defun C:WIRECALC (/ all-layers layer-list total-all-layers layer-total ss i ent entdata enttype objlength obj all-entities layer-results summary-text summary-lines max-layer-len line-text user-choice continue-loop prefixes prefix-data wire-groups group-info)
  
  ;; Load Visual LISP extensions
  (vl-load-com)
  
  ;; Set continue flag
  (setq continue-loop T)
  
  ;; Main loop - continues until user clicks Quit
  (while continue-loop
    
    ;; Print header
    (princ "\n--- Wire Length Calculator ---")
    (princ "\nGUI version with dialog layer selection")
    
    ;; Get filtered layers (_SUB and _TRUNK only)
    (setq all-layers (get-filtered-layers))
    
    (if (null all-layers)
      (progn
        (princ "\nNo _SUB or _TRUNK layers found (excluding SPLICE_TRUNK and TSP_SUB)!")
        (setq continue-loop nil)
      )
      (progn
        ;; Build prefix data structure
        (setq prefixes (get-unique-2digit-prefixes all-layers))
        (setq wire-groups (get-unique-wire-groups all-layers))
        (setq prefix-data '())
        
        (foreach prefix prefixes
          (setq group-info (get-prefix-group-info prefix all-layers))
          (setq prefix-data (cons (cons prefix group-info) prefix-data))
        )
        (setq prefix-data (reverse prefix-data))
        
        (princ (strcat "\nFound " (itoa (length prefixes)) " prefix area(s), " (itoa (length wire-groups)) " wire groups"))
        
        ;; Show dynamic filter dialog
        (setq layer-list (show-dynamic-filter-dialog prefix-data wire-groups all-layers))
        
        (if (or (null layer-list) (= (length layer-list) 0))
          (progn
            (princ "\nNo layers selected. Exiting.")
            (setq continue-loop nil)
          )
          (progn
            ;; Display selected layers
            (princ "\n--- Selected Layers ---")
            (foreach layer layer-list
              (princ (strcat "\n- " layer))
            )
            (princ "\n")
            
            ;; Initialize variables
            (setq total-all-layers 0.0)
            (setq all-entities (ssadd)) ; Selection set to hold all found entities
            (setq layer-results '())    ; List to store (layer . total) pairs
            
            (princ "\nCalculating lengths...")
            (princ "\n")
            
            ;; Process each layer separately
            (foreach layer layer-list
              (princ (strcat "\n=== LAYER: " layer " ==="))
              (setq layer-total 0.0)
              
              ;; Create selection set for this specific layer
              (setq ss (ssget "X" (list 
                                    (cons 8 layer)
                                    (cons -4 "<OR")
                                    (cons 0 "LINE")
                                    (cons 0 "ARC")
                                    (cons 0 "CIRCLE")
                                    (cons 0 "LWPOLYLINE")
                                    (cons 0 "POLYLINE")
                                    (cons 0 "SPLINE")
                                    (cons -4 "OR>")
                                  )))
              
              (if ss
                (progn
                  (princ (strcat "\nFound " (itoa (sslength ss)) " entities on " layer))
                  
                  ;; Add all entities from this layer to the master selection set
                  (setq i 0)
                  (repeat (sslength ss)
                    (ssadd (ssname ss i) all-entities)
                    (setq i (1+ i))
                  )
                  
                  ;; Process each entity on this layer
                  (setq i 0)
                  (repeat (sslength ss)
                    (setq ent (ssname ss i))
                    (setq entdata (entget ent))
                    (setq enttype (cdr (assoc 0 entdata)))
                    
                    ;; Convert entity to VLA object and get length
                    (setq obj (vlax-ename->vla-object ent))
                    
                    (cond
                      ;; Handle LINE entities
                      ((= enttype "LINE")
                       (setq objlength (vlax-get-property obj "Length"))
                       (princ (strcat "Line: " (rtos objlength 2 3) " units\n"))
                      )
                      
                      ;; Handle ARC entities
                      ((= enttype "ARC")
                       (setq objlength (vlax-get-property obj "ArcLength"))
                       (princ (strcat "Arc: " (rtos objlength 2 3) " units\n"))
                      )
                      
                      ;; Handle CIRCLE entities
                      ((= enttype "CIRCLE")
                       (setq objlength (vlax-get-property obj "Circumference"))
                       (princ (strcat "Circle: " (rtos objlength 2 3) " units\n"))
                      )
                      
                      ;; Handle LWPOLYLINE entities
                      ((= enttype "LWPOLYLINE")
                       (setq objlength (vlax-get-property obj "Length"))
                       (princ (strcat "LWPolyline: " (rtos objlength 2 3) " units\n"))
                      )
                      
                      ;; Handle POLYLINE entities
                      ((= enttype "POLYLINE")
                       (setq objlength (vlax-get-property obj "Length"))
                       (princ (strcat "Polyline: " (rtos objlength 2 3) " units\n"))
                      )
                      
                      ;; Handle SPLINE entities
                      ((= enttype "SPLINE")
                       (setq objlength (vlax-curve-getDistAtParam ent (vlax-curve-getEndParam ent)))
                       (princ (strcat "Spline: " (rtos objlength 2 3) " units\n"))
                      )
                      
                      ;; Skip other entity types
                      (t
                       (setq objlength 0.0)
                      )
                    )
                    
                    (setq layer-total (+ layer-total objlength))
                    (setq i (1+ i))
                  )
                  
                  ;; Show layer total
                  (princ (strcat "\n" layer " TOTAL: " (rtos layer-total 2 3) " units"))
                  (setq total-all-layers (+ total-all-layers layer-total))
                  
                  ;; Store layer result
                  (setq layer-results (cons (cons layer layer-total) layer-results))
                )
                (progn
                  (princ (strcat "\nNo entities found on " layer))
                  ;; Store zero result
                  (setq layer-results (cons (cons layer 0.0) layer-results))
                )
              )
              (princ "\n")
            )
            
            ;; Reverse the layer results to maintain order
            (setq layer-results (reverse layer-results))
            
            ;; Sort layer results alphabetically by layer name
            (setq layer-results (vl-sort layer-results '(lambda (a b) (< (car a) (car b)))))
            
            ;; Select all found entities from all layers
            (if (> (sslength all-entities) 0)
              (progn
                (sssetfirst nil all-entities)
                (princ (strcat "\n*** ALL " (itoa (sslength all-entities)) " ENTITIES ARE NOW SELECTED ***"))
              )
            )
            
            ;; Display final results in command line
            (princ "\n--- FINAL RESULTS ---")
            (princ (strcat "\nGRAND TOTAL: " (rtos total-all-layers 2 3) " units"))
            (princ "\n--- END ---\n")
            
            ;; Store result in system variable
            (setvar "USERS1" (rtos total-all-layers 2 6))
            
            ;; Build summary text for dialog
            (setq summary-text "WIRE LENGTH CALCULATION SUMMARY\n")
            (setq summary-text (strcat summary-text "================================\n\n"))
            
            ;; Build summary lines for list box
            (setq summary-lines '())
            (setq summary-lines (cons "WIRE LENGTH CALCULATION SUMMARY" summary-lines))
            (setq summary-lines (cons "================================" summary-lines))
            (setq summary-lines (cons "" summary-lines))
            
            ;; Find max layer name length for alignment
            (setq max-layer-len 0)
            (foreach result layer-results
              (if (> (strlen (car result)) max-layer-len)
                (setq max-layer-len (strlen (car result)))
              )
            )
            
            ;; Add each layer result
            (foreach result layer-results
              (setq line-text (strcat (car result) ": " (rtos (cdr result) 2 3) " units"))
              (setq summary-text (strcat summary-text line-text "\n"))
              (setq summary-lines (cons line-text summary-lines))
            )
            
            ;; Add grand total
            (setq summary-text (strcat summary-text "\n"))
            (setq summary-text (strcat summary-text "================================\n"))
            (setq summary-text (strcat summary-text "GRAND TOTAL: " (rtos total-all-layers 2 3) " units\n"))
            (setq summary-text (strcat summary-text "================================\n"))
            
            (setq summary-lines (cons "" summary-lines))
            (setq summary-lines (cons "================================" summary-lines))
            (setq summary-lines (cons (strcat "GRAND TOTAL: " (rtos total-all-layers 2 3) " units") summary-lines))
            (setq summary-lines (cons "================================" summary-lines))
            
            ;; Reverse the lines list to get correct order
            (setq summary-lines (reverse summary-lines))
            
            ;; Show summary dialog and get user choice
            (setq user-choice (show-summary-dialog summary-text summary-lines))
            
            ;; Check if user wants to run again
            ;; user-choice: 2=Run Again, 1=Quit, 0=Red X (close button)
            (if (= user-choice 2)
              (progn
                (princ "\n\nRunning again with new layer selection...\n")
                (setq continue-loop T)
              )
              (progn
                (princ "\nExiting...\n")
                (setq continue-loop nil)
              )
            )
          )
        )
      )
    )
  )
  
  (princ) ; Clean exit
)

;; Print loading message
(princ "\nWireCalc GUI routine loaded. Type WIRECALC to run.")
(princ)
