// checkvalves.dcl
// LandFX Check - Valve Connections dialog
// Loaded by CHECKPIPES.lsp at runtime.

checkvalves_dialog : dialog {
  label = "LandFX Check - Valve Connections";

  // ── Selection status box ──────────────────────────────────────────────
  : boxed_row {
    label = "Selected Geometry";
    : button {
      key         = "sel_btn";
      label       = "Select Geometry";
      width       = 18;
      fixed_width = true;
    }
    : text {
      key         = "sel_count";
      label       = "--";
      width       = 22;
      fixed_width = true;
    }
    : spacer {}
    : text {
      key         = "prob_count";
      label       = "Problems: --";
      width       = 16;
      fixed_width = true;
    }
  }

  // ── Problem list ──────────────────────────────────────────────────────
  : list_box {
    key          = "problem_list";
    label        = "Problems  (double-click or Find Valve to navigate):";
    height       = 15;
    width        = 70;
    fixed_width  = true;
    allow_accept = true;
  }

  // ── Detail line (updates live on list selection) ──────────────────────
  : text {
    key         = "detail_line";
    label       = " ";
    width       = 70;
    fixed_width = true;
  }

  spacer;

  // ── Action buttons ────────────────────────────────────────────────────
  : row {
    : button { key = "zoom_btn";  label = "Find Valve"; width = 12; fixed_width = true; }
    : spacer {}
    : button { key = "close_btn"; label = "Close";   width = 12; fixed_width = true; is_cancel = true; }
  }
}
