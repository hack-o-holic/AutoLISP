// FAIRWAY CENTERLINE — Head Spacing Dialog
// Shown after centerline is created. Displays width stats and prompts for optional offset lines.

fairway_spacing : dialog {
  label = "Fairway Centerline — Head Spacing";

  : text {
    label = "Centerline created on layer 03_FAIRWAY_CL.";
    alignment = centered;
  }

  spacer;

  : boxed_column {
    label = "Fairway Width Analysis";
    : row {
      : text { label = "Average width:"; width = 16; }
      : text { key = "avg_width"; label = "---"; width = 12; }
    }
    : row {
      : text { label = "Minimum width:"; width = 16; }
      : text { key = "min_width"; label = "---"; width = 12; }
    }
    : row {
      : text { label = "Maximum width:"; width = 16; }
      : text { key = "max_width"; label = "---"; width = 12; }
    }
  }

  spacer;

  : boxed_column {
    label = "Offset Lines (optional)";
    : text { label = "Enter a head spacing to create offset lines on each side of the centerline."; }
    : text { label = "Leave blank and click Skip to finish without offset lines."; }
    spacer;
    : edit_box {
      key  = "head_spacing";
      label = "Head spacing (ft):";
      edit_width = 8;
    }
  }

  spacer;

  : row {
    : button {
      key         = "accept";
      label       = "Create Offset Lines";
      is_default  = true;
      fixed_width = true;
      width       = 22;
    }
    : spacer {}
    : button {
      key         = "skip";
      label       = "Skip";
      is_cancel   = true;
      fixed_width = true;
      width       = 10;
    }
  }

  errtile;
}
