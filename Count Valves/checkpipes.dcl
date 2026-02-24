// checkpipes.dcl
// LandFX Check - Pipe XData dialog
// Loaded by CHECKPIPES.lsp at runtime.

checkpipes_dialog : dialog {
  label = "LandFX Check - Pipe XData";

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

  // ── Source pipe (fix template) box ───────────────────────────────────
  : boxed_row {
    label = "Source Pipe";
    : button {
      key         = "tpl_btn";
      label       = "Select Pipe";
      width       = 15;
      fixed_width = true;
    }
    : column {
      : text { key = "tpl_value"; label = "(none set)"; width = 44; }
      : text { key = "tpl_layer"; label = ""; width = 44; }
    }
    : image {
      key    = "tpl_swatch";
      width  = 3;
      height = 2;
      color  = -2;
    }
  }

  // ── Problem list ──────────────────────────────────────────────────────
  : list_box {
    key          = "problem_list";
    label        = "Problems  (double-click or Find Pipe to navigate):";
    height       = 13;
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
    : button { key = "zoom_btn";    label = "Find Pipe";       width = 12; fixed_width = true; }
    : button { key = "fix_btn";     label = "Fix Pipe";        width = 12; fixed_width = true; }
    : button { key = "fix_tpl_btn"; label = "Fix LAT to MAIN"; width = 16; fixed_width = true; }
    : button { key = "fix_all_btn"; label = "Fix All";         width = 10; fixed_width = true; }
    : spacer {}
    : button { key = "close_btn";   label = "Close";           width = 12; fixed_width = true; is_cancel = true; }
  }
}
