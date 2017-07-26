tabPanel(
  value='uep',
  "Upload Expression Profile",
  fluidRow(
    id='uep_help_tray',
    p('Expression profile is a table where each column corresponds to a sample,
    and each row corresponds to a feature (typically genes). The profile is expected
    to be a CSV (comma separated values) file.
    The first row is expected to be the names of the samples and the first column
    is expected to be the names of the features. The first column should have the column
    name "gene".')
  ),
  fluidRow(
    id='uep_upload_tray',
    hr(),
    fileInput('uep_expr_file_selector', 'Expression Profile'),
    fileInput('uep_sample_meta_selector', 'Sample Metadata'),
    actionButton('uep_sanity_check', 'Upload')
  ),
  fluidRow(
    id='uep_preview_tray',
    hr(),
    p('Here is a preview of the table you have just submitted.'),
    dataTableOutput('uep_preview_exp_profile_dt')
  ) %>% hidden,
  fluidRow(
    id='uep_confirm_tray',
    class='right-align',
    hr(),
    actionButton('uep_go_back', 'Dosesn\'t look right, re-upload'),
    actionButton('uep_submit', 'Looks right, submit', class='btn-primary')
  ) %>% hidden
)
