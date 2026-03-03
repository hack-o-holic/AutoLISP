;;; ========================================================================
;;; TCLLC-Tools Bundle Loader
;;; Auto-loads each tool on first use via autoload.
;;; Dev/diagnostic tools (SCANVALVE, TEST-PIPECATS, Dev Tools) are
;;; excluded — load manually from their source folders if needed.
;;; ========================================================================

(autoload "CHECKPIPES"      '("CHECKPIPES" "SETFIXTEMPLATE"))
(autoload "CHECKVALVES"     '("CHECKVALVES" "CHECKVALVES-CLEAR"))
(autoload "COUNTVALVES"     '("COUNTVALVES"))
(autoload "GOTOVALVE"       '("GOTOVALVE"))
(autoload "HeadLayerAssign" '("HEADLAYERS"))
(autoload "Head Numbering"  '("NUMBERHEADS" "FINDTAG" "FINDHEAD" "EXPORTHEADS"))

(princ "\nTCLLC-Tools ready.")
(princ)
