// HeadLayerAssign.dcl
// Dialog for the HEADLAYERS command.
//
// Layout:
//   [ Scope: (o) All Modelspace  ( ) Current Selection ]  [ Scan ]
//   [ To assign - layer will be set                         ]  <- action list
//   [ Unmatched - need attention                            ]  <- problems
//   [ Already correct - no change needed                    ]  <- informational
//   Status text...
//                                          [ Apply ] [ Cancel ]

HeadLayerAssign : dialog {
  label = "Head Layer Assignment";

  // ---- Scope — two-row layout keeps radios in the same column cluster
  //   (required for mutual exclusion) while each button aligns with its radio.
  : boxed_column {
    label = "Scope";
    : row {
      : column {
        : radio_button { key = "scope_all"; label = "All Model Space";    value = "1"; }
        : radio_button { key = "scope_sel"; label = "Current Selection";             }
      }
      : column {
        : button { key = "scan_btn"; label = "  Scan  ";    width = 10; }
        : button { key = "sel_btn";  label = " Select... "; width = 12; }
      }
    }
  }

  spacer;

  // ---- To assign list --------------------------------------------------
  : boxed_column {
    label = "To assign - layer will be set";
    : list_box {
      key             = "matched_list";
      height          = 6;
      width           = 55;
      multiple_select = false;
      allow_accept    = false;
    }
  }

  // ---- Unmatched list --------------------------------------------------
  : boxed_column {
    label = "Unmatched - need attention";
    : list_box {
      key             = "unmatched_list";
      height          = 6;
      width           = 55;
      multiple_select = false;
      allow_accept    = false;
    }
  }

  // ---- Already correct list --------------------------------------------
  : boxed_column {
    label = "Already correct - no change needed";
    : list_box {
      key             = "already_list";
      height          = 6;
      width           = 55;
      multiple_select = false;
      allow_accept    = false;
    }
  }

  spacer;

  // ---- Status line -----------------------------------------------------
  : text { key = "status_text"; label = "Set scope and click Scan."; }

  spacer;

  // ---- Action buttons --------------------------------------------------
  : row {
    spacer;
    : button { key = "accept"; label = " Assign Layers "; is_default = true;  width = 16; }
    : button { key = "cancel"; label = "    Cancel    "; is_cancel  = true;  width = 10; }
  }
}
