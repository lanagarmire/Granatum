library(shiny)
library(htmltools)
library(shinyjs)
library(htmlwidgets)
library(plotly)
library(dplyr)
library(markdown)
library(shinythemes)

# ui ----------------------------------------------------------------------

ui <- function(request) {
  #shinyUI(
  navbarPage(
    id = 'steps_list',
    title = "Granatum",
    theme = shinytheme("cerulean"),
    tabPanel(
      value = 'info',
      "Information",
      fluidRow(
        id = 'uep_logo_tray',
        img(id = 'uep_logo', src = 'granatum_logo.svg'),
        p(
          'Welcome to Granatum! This is a graphical single-cell RNA-seq (scRNA-seq) analysis pipeline for genomics scientists. The pipeline will graphically guide you through the analysis of scRNA-seq data, starting from expression and metadata tables. It uses a comprehensive set of modules for quality control / normalization, clustering, differential gene expression / enrichment analysis, protein network interaction visualization, and cell pseudo-time pathway construction.'
        ),
        p(
          'Note 1: if the browser window (or tab) is accidentally closed, you may resume from where you left off by opening the last page in your broswer history.'
        ),
        p(
          'Note 2: depending on dataset size, some steps may take time. Please allow computations to complete even if your browser appears to hang.'
        ),
        actionButton('info_next', 'Proceed to Step 1', class = 'btn-success')
      ),
      fluidRow(
        id = 'uep_example_tray',
        p(h2('Background')),
        p(
          'Video tutorial:',
          tags$a(href = "http://garmiregroup.org/granatum/video", "link to the video", target =
                   "_blank")
        ),
        p(
          'Survey (suggestions are welcome!):',
          tags$a(href = "http://garmiregroup.org/granatum/survey", "link to the survey", target =
                   "_blank")
        ),
        p(
          'Manuscript:',
          tags$a(href = "http://garmiregroup.org/granatum/paper", "link to the manuscript", target =
                   "_blank")
        ),
        p(
          'Manual:',
          tags$a(href = "http://garmiregroup.org/granatum/manual", "download PDF", target =
                   "_blank")
        ),
        p(
          'License:',
          tags$a(href = "http://garmiregroup.org/granatum/license", "download text", target =
                   "_blank")
        )
      ),
      fluidRow(
        id = 'uep_download_yourself',
        p(h2('DIY')),
        p('To run the server on your own computer, download it from this link:'),
        p(
          tags$a(href = "https://drive.google.com/file/d/0BzaGo-kaBuGLZ2EtUEZPUDNpSW8/view?usp=sharing", "Download server file", target =
                   "_blank")
        ),
        p('To use the file, have VirtualBox installed:'),
        p(
          tags$a(href = "https://www.virtualbox.org/wiki/Downloads", "Download VirtualBox", target =
                   "_blank")
        ),
        p(
          'After starting VirtualBox, click "File" -> "Import Appliance...", provide the file, and perform the import.'
        ),
        p(
          'Then launch Granatum, wait for it to load, and point your web browser to the following address:'
        ),
        p(
          tags$a(href = "http://localhost:8028/", "http://localhost:8028/", target =
                   "_blank")
        ),
        p('A video of this can be viewed as well:'),
        p(
          tags$a(href = "https://youtu.be/IZTDMXL0fjg?t=1m51s", "View video on YouTube", target =
                   "_blank")
        ),
        p(
          'Thank you! If there are any questions please contact us: lana.garmire.group@gmail.com'
        )
      )
    ),
    navbarMenu(
      "Steps",
      tabPanel(
        value = 'uep',
        strong("Upload"),
        includeCSS('main.css'),
        tags$head(tags$link(rel = "shortcut icon", href = "/favicon.ico")),
        tags$head(tags$link(rel = "shortcut", href = "/favicon.ico")),
        
        useShinyjs(),
        extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }", functions = "refresh"),

        actionButton('bm_button_uep', icon("bookmark-o", lib="font-awesome"), class = 'bm-success'),
        fluidRow(id = 'uep_help_tray',
                 p(h2(strong(
                   'Upload'
                 )))),
        fluidRow(
          id = 'uep_upload_tray',
          hr(),
          p(
            'Is your data Human or Mouse? Make a selection under "Species". Then provide your Expression and Metadata tables as comma separated value (CSV) files.'
          ),
          radioButtons('uep_species', 'Species', c(`Human` = 'human', `Mouse` =
                                                     'mouse')),
          br(),
          uiOutput('uep_upload_tabs'),
          actionLink(
            'uep_expression_table_format_pop_btn',
            'Before uploading your data, please refer to our format specification.'
          ),
          br(),
          p(
            'Example human data',
            tags$a(href = "https://genomebiology.biomedcentral.com/articles/10.1186/s13059-016-0945-9/", "(Kim, et al. 2016):", target =
                     "_blank")
          ),
          tags$ul(
            tags$li(
              downloadLink('uep_example_expr', 'Download Expression Table'),
              downloadLink('uep_example_expr_batch1', '(or batch1,'),
              downloadLink('uep_example_expr_batch2', 'batch2,'),
              downloadLink('uep_example_expr_batch3', 'and batch3)')
            ),
            tags$li(
              downloadLink('uep_example_meta', 'Download Metadata Table'),
              downloadLink('uep_example_meta_batch1', '(or batch1,'),
              downloadLink('uep_example_meta_batch2', 'batch2,'),
              downloadLink('uep_example_meta_batch3', 'and batch3)')
            )
          ),
          br(),
          br(),
          fileInput('uep_expr_file_selector', 'Expression Table'),
          checkboxInput('uep_no_meta', 'No metadata', F),
          fileInput('uep_sample_meta_selector', 'Metadata Table'),
          p(
            'If you would like to add more datasets, click ',
            strong("Add another dataset"),
            ' on the next page.'
          ),
          numericInput(
            'uep_num_cores',
            'Maximum number of cores Granatum can use:',
            4L,
            1L,
            8L,
            step = 1L
          ) %>% hidden,
          actionButton('uep_sanity_check', 'Add dataset')
        ),
        fluidRow(
          id = 'uep_preview_tray',
          hr(),
          strong(h5('Summary of datasets uploaded')),
          dataTableOutput('uep_overview_tb'),
          hr(),
          strong(h5('Last dataset uploaded')),
          tabsetPanel(
            tabPanel(
              'Expression Table',
              dataTableOutput('uep_preview_exp_profile_dt')
            ),
            tabPanel('Metadata Table', dataTableOutput('uep_preview_metadata_dt'))
          )
          
        ) %>% hidden,
        fluidRow(
          id = 'uep_confirm_tray',
          class = 'right-align',
          hr(),
          actionButton('uep_add_another_dataset', 'Add another dataset'),
          actionButton('uep_reset', 'Reset', class = 'btn-primary'),
          actionButton('uep_submit', 'Submit', class = 'btn-success')
        ) %>% hidden
      ),
      
      tabPanel(
        value = 'br',
        strong("Batch-effect removal"),
        actionButton('bm_button_br', icon("bookmark-o", lib="font-awesome"), class = 'bm-success'),
        fluidRow(
          id = 'br_help_tray',
          p(h2(strong(
            'Batch-effect removal'
          ))),
          p(
            'Remove confounding effects from data generated in batches. Box plots give expression statistics for a random sampling of up to 96 cells. Select a batch grouping label (factor) then click "Remove batch effect". If multiple datasets were separately uploaded, the "dataset" factor can be used.'
          )
        ),
        fluidRow(
          id = 'br_vis_tray',
          selectInput(
            "br_select_col",
            "Batch factor:",
            choices = list('No column in the metadata' = NULL),
            selected = 'dataset',
            width = '50%'
          ),
          plotOutput('br_barchart', height = '450px')
        ),
        fluidRow(id = 'br_sampling_tray',
                 actionButton('br_resample', 'Re-plot random 96 cells')),
        fluidRow(
          id = 'br_combat_tray',
          hr(),
          radioButtons(
            'br_method',
            'Batch-effect removal methods',
            c(`ComBat` = 'combat',
              `Simple median alignment` = 'mid_align')
          ),
          actionButton('br_perform', 'Remove batch effect')
        ),
        fluidRow(
          id = 'br_confirm_tray',
          class = 'right-align',
          hr(),
          actionButton('br_reset', 'Reset', class = 'btn-primary'),
          actionButton('br_confirm', 'Submit', class = 'btn-success')
        )
      ),
      
      tabPanel(
        value = 'oi',
        strong("Outlier removal"),
        actionButton('bm_button_oi', icon("bookmark-o", lib="font-awesome"), class = 'bm-success'),
        fluidRow(
          id = 'oi_help_tray',
          p(h2(strong('Outlier removal'))),
          p(
            'Remove unusual cells, e.g., those damaged by capture. Select cells by clicking points in the plot and/or using "Auto-identify", then click "Remove selected".'
          )
        ),
        fluidRow(
          id = 'oi_viz_tray',
          hr(),
          selectInput(
            "oi_select_col",
            "Cell labels (from metadata)",
            choices = list('No column in the metadata' = NULL),
            width = '50%'
          ),
          fluidRow(column(6, plotlyOutput('oi_vis_1')), column(6, plotlyOutput('oi_vis_2'))),
          checkboxInput(
            'oi_top_expressed_only',
            'Cluster using only top expressed genes (helps to identify outliers)',
            T
          ),
          hr(),
          actionButton('oi_auto_outlier', 'Auto-identify'),
          actionButton('oi_remove', 'Remove selected'),
          actionButton('oi_clear_sel', 'De-select all')
        ),
        fluidRow(
          id = 'oi_confirm_tray',
          class = 'right-align',
          hr(),
          actionButton('oi_reset', 'Reset', class = 'btn-primary'),
          actionButton('oi_confirm', 'Submit', class = 'btn-success')
        ),
        fluidRow(
          id = 'oi_preview_tray',
          hr(),
          p('Selected cells:'),
          dataTableOutput('oi_preview_tb')
        )
      ),
      
      tabPanel(
        value = 'nz',
        strong("Normalization"),
        actionButton('bm_button_nz', icon("bookmark-o", lib="font-awesome"), class = 'bm-success'),
        fluidRow(
          id = 'nz_help_tray',
          p(h2(strong('Normalization'))),
          p(
            'Adjust expression levels to correct for artificial differences between cells, e.g., differences in sequencing depth. When a rescaling/normalization button is clicked, the box plot (showing expression statistics for up to 96 randomly selected cells) will reflect changes. For example, clicking "Rescaling to geometric mean" will cause red dots (geometric means) to align. Note that clicking more than one rescaling/normalization button will apply adjustments on already adjusted values (use "Reset" to go back to unadjusted data).'
          )
        ),
        fluidRow(id = 'nz_vis_tray',
                 plotOutput('nz_barchart', height = '450px')),
        fluidRow(id = 'nz_sampling_tray',
                 actionButton('nz_resample', 'Re-plot random 96 cells')),
        fluidRow(
          id = 'nz_algo_tray',
          hr(),
          actionButton('nz_quantile', 'Quantile normalization'),
          actionButton('nz_scalecenter', 'Rescale to genometric mean'),
          actionButton('nz_sizefactor', 'Size-factor normalization'),
          actionButton('nz_voom', 'Voom')
        ),
        fluidRow(
          id = 'nz_confirm_tray',
          class = 'right-align',
          hr(),
          downloadButton('nz_download_mat', 'Download the normalized matrix'),
          actionButton('nz_reset', 'Reset', class = 'btn-primary'),
          actionButton('nz_confirm', 'Submit', class = 'btn-success')
        )
      ),
      
      tabPanel(
        value = 'ft',
        strong("Gene filtering"),
        actionButton('bm_button_ft', icon("bookmark-o", lib="font-awesome"), class = 'bm-success'),
        fluidRow(
          id = 'ft_help_tray',
          p(h2(strong('Gene filtering'))),
          p(
            'Remove genes having very low expression and/or those with little variation (dispersion) by moving the sliders. It is recommended to keep at least 2,000 genes.'
          )
        ),
        fluidRow(
          id = 'ft_vis_tray',
          hr(),
          plotOutput('ft_vis_2', height = '600px'),
          textOutput('ft_vis_des'),
          fluidRow(column(
            6,
            sliderInput(
              "ft_met",
              "Log Mean Expression Threshold",
              width = '100%',
              min = -4.3,
              max = 4.3,
              step = 0.01,
              value = -2.3
            )
          ),
          column(
            6,
            sliderInput(
              "ft_dft",
              "Dispersion Fit Threshold",
              width = '100%',
              min = 0,
              max = 5,
              value = 1,
              step = 0.01
            )
          ))
        ),
        fluidRow(id = 'ft_statistics_tray',
                 hr(),
                 fluidRow(
                   column(
                     6,
                     span('Starting number of genes: '),
                     textOutput('ft_stat_current')
                   ),
                   column(
                     6,
                     span('Post-filtering number of genes: '),
                     textOutput('ft_stat_after')
                   )
                 )),
        fluidRow(
          id = 'ft_confirm_tray',
          class = 'right-align',
          hr(),
          actionButton('ft_confirm', 'Submit', class = 'btn-success')
        )
      ),
      
      tabPanel(
        value = 'cl',
        strong("Clustering"),
        actionButton('bm_button_cl', icon("bookmark-o", lib="font-awesome"), class = 'bm-success'),
        fluidRow(
          id = 'cl_help_tray',
          p(h2(strong('Clustering'))),
          p(
            'Select a clustering method and enter a number of clusters (or check the box for auto selection), then click "Run clustering".'
          )
        ),
        fluidRow(id = 'cl_vis_tray',
                 hr(),
                 fluidRow(
                   column(
                     12,
                     selectInput(
                       "cl_select_col",
                       "Cell labels",
                       choices = list('No column in the metadata' = NULL),
                       width = '50%'
                     )
                   ),
                   column(6, plotOutput('cl_vis_1')),
                   column(6, plotOutput('cl_vis_2'))
                 )),
        fluidRow(
          id = 'cl_algos_tray',
          hr(),
          radioButtons(
            'cl_algo',
            'Clustering method',
            c(
              `K-means (Euclidean)` = 'kmeans',
              `K-means (correlation t-SNE)` = 'kmeans_corr',
              `Hierarchical clustering (Euclidean) with heatmap` =
                'hclust',
              `Hierarchical clustering (correlation t-SNE)` = 'hclust_corr',
              `Non-negative matrix factorization` = 'nmf'
            )
          ),
          br(),
          checkboxInput(
            'cl_auto_num_clusters',
            'Automatically choose the number of clusters (might take long time)'
          ),
          numericInput('cl_num_clusters', 'Number of clusters', 3L, 1L, 20L, 1L),
          h5(textOutput('cl_num_clusters_auto') %>% hidden),
          actionButton('cl_cluster', 'Run clustering')
        ),
        fluidRow(
          id = 'cl_confirm_tray',
          class = 'right-align',
          hr(),
          downloadButton('cl_download_mat', 'Download the filtered matrix'),
          downloadButton('cl_download', 'Download CSV table'),
          actionButton('cl_confirm', 'Submit', class = 'btn-success')
        ) %>% hidden
      ),
      
      tabPanel(
        value = 'de',
        strong("Differential expression"),
        actionButton('bm_button_de', icon("bookmark-o", lib="font-awesome"), class = 'bm-success'),
        fluidRow(
          id = 'de_help_tray',
          p(h2(strong(
            'Differential expression'
          ))),
          p(
            'Identify differentially expressed genes between clusters. The number of cores can be set to 2 and will run for approximately 30 minutes on the Kim, et al. 2016 dataset (116 cells, 3,788 genes, 3 clusters), when using a VirtualBox Appliance having 8 GB RAM and an Intel I7 processor. Note: the progress bar will not accurately reflect progress, please give the calculations time to complete.'
          ),
          p(
            'Once complete, the enrichment of differentially expressed genes in KEGG pathways and GO terms can be calculated.'
          )
        ),
        fluidRow(
          id = 'de_start_tray',
          hr(),
          radioButtons(
            'de_methods',
            'Method',
            c(
              `NODES (fast)` = 'nodes',
              `limma (fast)` = 'limma',
              `edgeR (fast)` = 'edgeR',
              `SCDE (slow)` = 'scde'
            )
          ),
          selectInput(
            "de_select_vec",
            "Which factor to use for DE?",
            choices = list(Clusters = 'Clusters'),
            width = '50%'
          ),
          actionButton('de_start', 'Start analysis')
        ) %>% hidden,
        fluidRow(id = 'de_too_few_genes_tray',
                 hr(),
                 p(strong(
                   textOutput('de_too_few_genes_message')
                 )),
                 actionButton('de_skip', 'Skip')) %>% hidden,
        fluidRow(
          id = 'de_vis_tray',
          hr(),
          textOutput('de_warn_message'),
          column(
            12,
            selectInput(
              "de_select_col",
              "Cell labels",
              choices = list('No column in the metadata' = NULL),
              width = '50%'
            )
          ),
          column(6, plotOutput('de_vis_1')),
          column(6, plotOutput('de_vis_2'))
        ) %>% hidden,
        fluidRow(
          id = 'de_res_tray',
          hr(),
          p(
            'Tabs indicate cluster numbers. Genes are sorted by absolute Z-score.'
          ),
          uiOutput('de_tables')
        ) %>% hidden,
        fluidRow(
          id = 'de_confirm_tray',
          class = 'right-align',
          hr(),
          downloadButton('de_download', 'Download CSV table'),
          actionButton('de_confirm', 'Submit', class = 'btn-success')
        ) %>% hidden
      ),
      
      tabPanel(
        value = 'ne',
        strong("Protein network"),
        actionButton('bm_button_ne', icon("bookmark-o", lib="font-awesome"), class = 'bm-success'),
        fluidRow(
          id = 'ne_help_tray',
          p(h2(strong('Protein network'))),
          p(
            'Proteins from top differentially expressed genes are visualized with connecting lines indicating documented biochemical interactions. Go to the next step by clicking "Proceed" (bottom right of page).'
          )
        ),
        fluidRow(id = 'ne_vis_tray',
                 hr(),
                 uiOutput('ne_networks')),
        fluidRow(
          id = 'ne_confirm_tray',
          class = 'right-align',
          hr(),
          actionButton('ne_confirm', 'Proceed', class = 'btn-success')
        )
      ),
      
      tabPanel(
        value = 'pt',
        strong("Pseudo-time construction"),
        actionButton('bm_button_pt', icon("bookmark-o", lib="font-awesome"), class = 'bm-success'),
        fluidRow(
          id = 'pt_help_tray',
          p(h2(strong(
            'Pseudo-time construction'
          ))),
          p(
            'Cells are ordered in pseudo-time using similarities between their expression profiles.'
          )
        ),
        fluidRow(
          id = 'pt_res_tray',
          hr(),
          selectInput(
            "pt_select_col",
            "Cell labels",
            choices = list('No column in the metadata' = NULL),
            width = '50%'
          ) %>% hidden,
          plotOutput('pt_plot', height = '800px')
        )
      )
    )
  )
#)
}
