// checkvalves.dcl
// LandFX Check - Valve Connections dialog
// Loaded by CHECKPIPES.lsp at runtime.

checkvalves_dialog : dialog {
  label = "LandFX Check - Valve Connections";

  // ── Selected Geometry ─────────────────────────────────────────────────
  : row {
    : button {
      key         = "sel_btn";
      label       = "Select Geometry";
      width       = 18;
      fixed_width = true;
    }
    : text {
      key         = "sel_count";
      label       = "--";
      width       = 30;
      fixed_width = true;
    }
    : spacer {}
  }

  // ── Connection check ──────────────────────────────────────────────────
  : boxed_column {
    label = "Valve Connections  (each lateral SOV needs 1 lateral + 1 main)";
    : list_box {
      key          = "conn_list";
      label        = "Problems  (double-click or Find Valve to navigate):";
      height       = 7;
      width        = 70;
      fixed_width  = true;
      allow_accept = true;
    }
    : text { key = "conn_detail"; label = " "; width = 70; fixed_width = true; }
    : row {
      : text { key = "conn_count"; label = "Problems: --"; width = 22; fixed_width = true; }
      : button { key = "zoom_btn";      label = "Find Valve"; width = 12; fixed_width = true; }
      : button { key = "fix_valve_btn"; label = "Fix Manually"; width = 13; fixed_width = true; }
      : spacer {}
    }
  }

  // ── Size mismatch check ───────────────────────────────────────────────
  : boxed_column {
    label = "Pipe Size Mismatches  (lateral and main pipe sizes should match)";
    : list_box {
      key          = "size_list";
      label        = "Mismatches  (double-click or Find Valve to navigate):";
      height       = 7;
      width        = 70;
      fixed_width  = true;
      allow_accept = true;
    }
    : text { key = "size_detail"; label = " "; width = 70; fixed_width = true; }
    : row {
      : text { key = "size_count"; label = "Mismatches: --"; width = 22; fixed_width = true; }
      : button { key = "size_zoom_btn"; label = "Find Valve"; width = 12; fixed_width = true; }
      : button { key = "fix_main_btn";  label = "Fix One";    width = 12; fixed_width = true; }
      : button { key = "fix_all_btn";   label = "Fix Group";  width = 12; fixed_width = true; }
      : spacer {}
    }
  }

  spacer;

  // ── Bottom ────────────────────────────────────────────────────────────
  : row {
    : spacer {}
    : button { key = "close_btn"; label = "Close"; width = 12; fixed_width = true; is_cancel = true; }
  }
}

// ── Template picker (opened by Fix One / Fix Group) ────────────────────────
cv_pick_template : dialog {
  label = "Choose Mainline Template";
  : text { key = "ctx_line"; label = " "; width = 62; fixed_width = true; }
  : list_box {
    key          = "tpl_list";
    label        = "Mainline pipe types found in selection -- pick one:";
    height       = 8;
    width        = 62;
    fixed_width  = true;
    allow_accept = true;
  }
  spacer;
  ok_cancel;
}
