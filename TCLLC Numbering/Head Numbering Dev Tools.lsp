;;; ========================================================================
;;; HEAD NUMBERING DEV TOOLS
;;; ========================================================================
;;; Diagnostic and recovery commands developed while reverse-engineering
;;; the LandFX XDATA system. These are NOT part of normal workflow and
;;; were written to solve specific one-time problems during development.
;;;
;;; REQUIRES: Head Numbering.lsp loaded first (FormatHoleNumber is used
;;;           by REPAIRATTRIBUTES).
;;;
;;; ========================================================================
;;; COMMANDS
;;; ========================================================================
;;;
;;; REPAIRHEADTAGS
;;;   Finds LAFX-TAG-SQUARE-999 blocks that have NO XDATA at all, reads
;;;   the visible attribute text (e.g. "03FW007"), parses it into
;;;   hole/location/number, and backfills HEADNUM XDATA. The 1005 head
;;;   link cannot be restored — that relationship is lost for tags placed
;;;   before the XDATA system was in place. One-time recovery tool.
;;;
;;; REPAIRATTRIBUTES
;;;   The inverse of REPAIRHEADTAGS. Finds tag blocks that have correct
;;;   HEADNUM XDATA but a blank attribute text, and writes the display
;;;   text back from the XDATA. Companion command — the pair was written
;;;   to fix a state where LandFX was actively clearing attribute values.
;;;
;;; STRIPLFXDATA
;;;   Removes the LandFX XDATA group from every tag block while leaving
;;;   HEADNUM XDATA intact. Written as a workaround when LandFX was
;;;   resetting/overwriting tags by detecting its own XDATA app name on
;;;   them. Run this, then REPAIRATTRIBUTES to fix any blanked values.
;;;
;;; ========================================================================

;;; ------------------------------------------------------------------------
;;; ParseHeadNumber — helper for REPAIRHEADTAGS
;;; Parses "03FW007" → (3 "FW" "007")
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

;;; ------------------------------------------------------------------------
;;; REPAIRHEADTAGS
;;; ------------------------------------------------------------------------

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
;;; REPAIRATTRIBUTES
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
;;; STRIPLFXDATA
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

;;; ------------------------------------------------------------------------
;;; DUMPHEADXDATA
;;;   Click any head block (VIH rotor, fixed spray, etc.) and this command
;;;   prints every XDATA field for every registered app — showing the group
;;;   code, a zero-based index, and the value. Also lists any block
;;;   attributes (tag name + current value). Written to map LandFX field
;;;   indices before EXPORTHEADS was written. No longer part of normal
;;;   workflow.
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