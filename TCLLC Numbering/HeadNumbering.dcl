// ========================================================================
// HEAD NUMBERING DIALOG
// ========================================================================

HeadNumDialog : dialog {
  label = "TCLLC Head Numbering";
  initial_focus = "hole_list";

  : row {
    : column {
      label = "Hole Number";
      : popup_list {
        key = "hole_list";
        width = 12;
      }
    }

    : column {
      label = "Location Code";
      : popup_list {
        key = "location_list";
        width = 12;
      }
    }
  }

  : boxed_column {
    label = "Already placed:";

    : text {
      key = "gap_label";
      label = "";
      alignment = centered;
    }

    : list_box {
      key = "tag_list";
      width = 30;
      height = 8;
      allow_accept = false;
      multiple_select = false;
    }
  }

  : row {
    fixed_width = true;
    alignment = centered;
    : boxed_column {
      label = "Next No.:";
      fixed_width = true;
      : row {
        : edit_box { key = "override_num"; width = 7; fixed_width = true; }
        : button { key = "clear_num_btn"; label = "Auto"; fixed_width = true; width = 6; }
      }
    }
  }

  : row {
    fixed_width = true;
    alignment = centered;

    : button {
      key = "fill_gaps_btn";
      label = "Fill Gaps";
      fixed_width = true;
      width = 14;
      is_enabled = false;
    }

    : button {
      key = "export_btn";
      label = "Export...";
      fixed_width = true;
      width = 14;
    }
  }

  : row {
    fixed_width = true;
    alignment = centered;

    : button {
      key = "accept";
      label = "OK";
      is_default = true;
      fixed_width = true;
      width = 12;
    }

    : button {
      key = "cancel";
      label = "Cancel";
      is_cancel = true;
      fixed_width = true;
      width = 12;
    }
  }
}

// ========================================================================
// HEAD EXPORT DIALOG
// ========================================================================

HeadExportDialog : dialog {
  label = "Export Heads";

  : text {
    label = "Select areas to export:";
    alignment = left;
  }

  : list_box {
    key = "export_list";
    width = 30;
    height = 12;
    multiple_select = true;
  }

  : row {
    fixed_width = true;
    alignment = centered;

    : button {
      key = "select_all_btn";
      label = "Select All";
      fixed_width = true;
      width = 12;
    }

    : button {
      key = "clear_all_btn";
      label = "Clear All";
      fixed_width = true;
      width = 12;
    }
  }

  : toggle {
    key = "include_valves";
    label = "Include valves";
    value = "1";
  }

  : row {
    : text {
      label = "Output folder:";
      alignment = left;
      fixed_width = true;
      width = 13;
    }
    : edit_box {
      key = "output_folder";
      width = 22;
    }
    : button {
      key = "browse_btn";
      label = "...";
      fixed_width = true;
      width = 4;
    }
  }

  : row {
    fixed_width = true;
    alignment = centered;

    : button {
      key = "fix_gaps_btn";
      label = "Fix Gaps";
      fixed_width = true;
      width = 11;
      is_enabled = false;
    }

    : button {
      key = "accept";
      label = "Export Selected";
      is_default = true;
      fixed_width = true;
      width = 14;
    }

    : button {
      key = "cancel";
      label = "Cancel";
      is_cancel = true;
      fixed_width = true;
      width = 10;
    }
  }
}