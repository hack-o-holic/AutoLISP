;;; ========================================================================
;;; TCLLC-Tools Bundle Loader
;;; Auto-loads each tool on first use via autoload.
;;; Dev/diagnostic tools (SCANVALVE, TEST-PIPECATS, Dev Tools) are
;;; excluded — load manually from their source folders if needed.
;;; ========================================================================

;;; Add Contents/ to support file search path so autoload can find LSP files
(setq *TCLLC-DIR*
  (strcat (getenv "APPDATA")
          "\\Autodesk\\ApplicationPlugins\\TCLLC-Tools.bundle\\Contents\\"))
(if (not (vl-string-search *TCLLC-DIR* (getenv "ACAD")))
  (setenv "ACAD" (strcat *TCLLC-DIR* ";" (getenv "ACAD"))))

(autoload "CHECKPIPES"      '("CHECKPIPES" "SETFIXTEMPLATE"))
(autoload "CHECKVALVES"     '("CHECKVALVES" "CHECKVALVES-CLEAR"))
(autoload "COUNTVALVES"     '("COUNTVALVES"))
(autoload "GOTOVALVE"       '("GOTOVALVE"))
(autoload "HeadLayerAssign" '("HEADLAYERS"))
(autoload "Head Numbering"  '("NUMBERHEADS" "FINDTAG" "FINDHEAD" "EXPORTHEADS"))

(princ "\nTCLLC-Tools ready.")
(princ)
