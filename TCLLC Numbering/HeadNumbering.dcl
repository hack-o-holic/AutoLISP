// ========================================================================
// HEAD NUMBERING DIALOG
// ========================================================================

HeadNumDialog : dialog {
  label = "Head Numbering Settings";
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

  : row {
    : text {
      key = "placed_label";
      label = "Already placed:";
      alignment = left;
    }
    : text {
      key = "gap_label";
      label = "";
      alignment = right;
      width = 16;
    }
  }

  : list_box {
    key = "tag_list";
    width = 30;
    height = 8;
    allow_accept = false;
    multiple_select = false;
  }

  : text {
    key = "next_label";
    label = "";
    alignment = left;
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
