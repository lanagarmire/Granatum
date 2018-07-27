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
        id = 'uploadStep_logo_tray',
        includeScript('google_analytics.js'),
        includeScript('custom_analytics.js'),
        img(id = 'uploadStep_logo', src = 'granatum_logo.svg'),
        p(
          'Welcome to Granatum! This is a graphical single-cell RNA-seq (scRNA-seq) analysis pipeline for genomics scientists. The pipeline will graphically guide you through the analysis of scRNA-seq data, starting from expression and metadata tables. It uses a comprehensive set of modules for quality control / normalization, clustering, differential gene expression / enrichment analysis, protein network interaction visualization, and cell pseudo-time pathway construction.'
        ),
        p('We welcome collaboration! If you are interested in getting help beyond standard troubleshooting please contact our PI, Lana Garmire, at: lgarmire@gmail.com'),
        p(
          strong('Please cite: ', a(href='https://genomemedicine.biomedcentral.com/articles/10.1186/s13073-017-0492-3',
            em('Zhu, Xun et al. “Granatum: A Graphical Single-Cell RNA-Seq Analysis Pipeline for Genomics Scientists.” Genome Medicine 9.1 (2017)'),
            onclick="trackOutboundLink('https://genomemedicine.biomedcentral.com/articles/10.1186/s13073-017-0492-3', 'paper'); return false;"))
        ),
        p(
          'Note 1: if the browser window (or tab) is accidentally closed, you may resume from where you left off by opening the last page in your broswer history.'
        ),
        p(
          'Note 2: depending on dataset size, some steps may take time. Please allow computations to complete even if your browser appears to hang.'
        ),
        actionButton('info_next', 'Proceed to Step 1', class = 'btn-success', onclick = "openSurveyOnce();")
      ),
      fluidRow(
        id = 'uploadStep_example_tray',
        p(h2('Visitor Map')),
        tags$iframe(id="QZG5w5YOoj", width="600", height="350", style="margin:0px;border-width:0px;overflow:hidden;", scrolling="no",
                    src="https://www.embeddedanalytics.com/reports/displayreport?reportcode=QZG5w5YOoj&chckcode=garhaApL1xSRdEw9Qs9Pm9"),
        p(h2('Background')),
        p('Please cite: Zhu, Xun et al. “Granatum: A Graphical Single-Cell RNA-Seq Analysis Pipeline for Genomics Scientists.” Genome Medicine 9.1 (2017)'),
        p(
          'Video tutorial:',
          tags$a(href = "http://garmiregroup.org/granatum/video", "link to the video", target =
                   "_blank", onclick="trackOutboundLink('http://garmiregroup.org/granatum/video', 'video'); return false;")
        ),
        p(
          'Survey (suggestions are welcome!):',
          tags$a(href = "http://garmiregroup.org/granatum/survey", "link to the survey", target =
                   "_blank", onclick="trackOutboundLink('http://garmiregroup.org/granatum/survey', 'survey'); return false;")
        ),
        p(
          'Manuscript:',
          tags$a(href = "http://garmiregroup.org/granatum/paper", "link to the manuscript", target =
                   "_blank", onclick="trackOutboundLink('http://garmiregroup.org/granatum/paper', 'manuscript'); return false;")
        ),
        p(
          'Manual:',
          tags$a(href = "http://garmiregroup.org/granatum/manual", "download PDF", target =
                   "_blank", onclick="trackOutboundLink('http://garmiregroup.org/granatum/manual', 'manual'); return false;")
        ),
        p(
          'License:',
          tags$a(href = "http://garmiregroup.org/granatum/license", "download text", target =
                   "_blank")
        )
      ),
      fluidRow(
        id = 'uploadStep_download_yourself',
        p(h2('DIY')),
        p('To run the server on your own computer, download it from this link:'),
        p(
          tags$a(href = "https://drive.google.com/file/d/0BzaGo-kaBuGLZ2EtUEZPUDNpSW8/view?usp=sharing", "Download server file", target =
                   "_blank", onclick="trackOutboundLink('https://drive.google.com/file/d/0BzaGo-kaBuGLZ2EtUEZPUDNpSW8/view?usp=sharing', 'downloadGranatum'); return false;")
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
        value = 'uploadStep',
        strong("Upload"),
        includeCSS('main.css'),
        tags$head(tags$link(rel = "shortcut icon", href = "/favicon.ico")),
        tags$head(tags$link(rel = "shortcut", href = "/favicon.ico")),

        useShinyjs(),
        extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }", functions = "refresh"),

        actionButton('bm_button_uep', icon("bookmark-o", lib="font-awesome"), class = 'bm-success'),
        fluidRow(id = 'uploadStep_help_tray',
                 p(h2(strong(
                   'Upload'
                 )))),
        fluidRow(
          id = 'uploadStep_upload_tray',
          hr(),
          p(
            'You can upload your own data or try Granatum on our sample data.'
          ),
          p(
            'Is your data Human or Mouse? Make a selection under "Species". Then provide your Expression and Metadata tables as comma separated value (CSV) files.'
          ),
          radioButtons('uploadStep_species', 'Species', c(`Human` = 'human', `Mouse` =
                                                     'mouse')),
          br(),
          uiOutput('uploadStep_upload_tabs'),
          actionLink(
            'uploadStep_expression_table_format_pop_btn',
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
              downloadLink('uploadStep_example_expr', 'Download Expression Table'),
              downloadLink('uploadStep_example_expr_batch1', '(or batch1,'),
              downloadLink('uploadStep_example_expr_batch2', 'batch2,'),
              downloadLink('uploadStep_example_expr_batch3', 'and batch3)')
            ),
            tags$li(
              downloadLink('uploadStep_example_meta', 'Download Metadata Table'),
              downloadLink('uploadStep_example_meta_batch1', '(or batch1,'),
              downloadLink('uploadStep_example_meta_batch2', 'batch2,'),
              downloadLink('uploadStep_example_meta_batch3', 'and batch3)')
            )
          ),
          br(),
          br(),
          fileInput('uploadStep_expr_file_selector', 'Expression Table'),
          checkboxInput('uploadStep_no_meta', 'No metadata', F),
          fileInput('uploadStep_sample_meta_selector', 'Metadata Table'),
          p(
            'If you would like to add more datasets, click ',
            strong("Add another dataset"),
            ' on the next page.'
          ),
          numericInput(
            'uploadStep_num_cores',
            'Maximum number of cores Granatum can use:',
            4L,
            1L,
            8L,
            step = 1L
          ) %>% hidden,
          actionButton('uploadStep_sanity_check', 'Add dataset')
        ),
        fluidRow(
          id = 'uploadStep_preview_tray',
          hr(),
          strong(h5('Summary of datasets uploaded')),
          dataTableOutput('uploadStep_overview_tb'),
          hr(),
          strong(h5('Last dataset uploaded')),
          tabsetPanel(
            tabPanel(
              'Expression Table',
              dataTableOutput('uploadStep_preview_exp_profile_dt')
            ),
            tabPanel('Metadata Table', dataTableOutput('uploadStep_preview_metadata_dt'))
          )

        ) %>% hidden,
        fluidRow(
          id = 'uploadStep_confirm_tray',
          class = 'right-align',
          hr(),
          actionButton('uploadStep_add_another_dataset', 'Add another dataset'),
          actionButton('uploadStep_reset', 'Reset', class = 'btn-primary'),
          actionButton('uploadStep_submit', 'Submit', class = 'btn-success')
        ) %>% hidden
      ),

      tabPanel(
        value = 'batchEffectStep',
        strong("Batch-effect removal"),
        actionButton('bm_button_br', icon("bookmark-o", lib="font-awesome"), class = 'bm-success'),
        fluidRow(
          id = 'batchEffectStep_help_tray',
          p(h2(strong(
            'Batch-effect removal'
          ))),
          p(
            'Remove confounding effects from data generated in batches. Box plots give expression statistics for a random sampling of up to 96 cells. Select a batch grouping label (factor) then click "Remove batch effect". If multiple datasets were separately uploaded, the "dataset" factor can be used.'
          )
        ),
        fluidRow(
          id = 'batchEffectStep_vis_tray',
          selectInput(
            "batchEffectStep_select_col",
            "Batch factor:",
            choices = list('No column in the metadata' = NULL),
            selected = 'dataset',
            width = '50%'
          ),
          plotOutput('batchEffectStep_barchart', height = '450px')
        ),
        fluidRow(id = 'batchEffectStep_sampling_tray',
                 actionButton('batchEffectStep_resample', 'Re-plot random 96 cells')),
        fluidRow(
          id = 'batchEffectStep_combat_tray',
          hr(),
          radioButtons(
            'batchEffectStep_method',
            'Batch-effect removal methods',
            c(`ComBat` = 'combat',
              `Simple median alignment` = 'mid_align')
          ),
          actionButton('batchEffectStep_perform', 'Remove batch effect')
        ),
        fluidRow(
          id = 'batchEffectStep_confirm_tray',
          class = 'right-align',
          hr(),
          actionButton('batchEffectStep_reset', 'Reset', class = 'btn-primary'),
          actionButton('batchEffectStep_confirm', 'Submit', class = 'btn-success')
        )
      ),

      tabPanel(
        value = 'outlierRemovalStep',
        strong("Outlier removal"),
        actionButton('bm_button_oi', icon("bookmark-o", lib="font-awesome"), class = 'bm-success'),
        fluidRow(
          id = 'outlierRemovalStep_help_tray',
          p(h2(strong('Outlier removal'))),
          p(
            'Remove unusual cells, e.g., those damaged by capture. Select cells by clicking points in the plot and/or using "Auto-identify", then click "Remove selected".'
          )
        ),
        fluidRow(
          id = 'outlierRemovalStep_viz_tray',
          hr(),
          selectInput(
            "outlierRemovalStep_select_col",
            "Cell labels (from metadata)",
            choices = list('No column in the metadata' = NULL),
            width = '50%'
          ),
          fluidRow(column(6, plotlyOutput('outlierRemovalStep_vis_1')), column(6, plotlyOutput('outlierRemovalStep_vis_2'))),
          checkboxInput(
            'outlierRemovalStep_top_expressed_only',
            'Cluster using only top expressed genes (helps to identify outliers)',
            T
          ),
          hr(),
          actionButton('outlierRemovalStep_auto_outlier', 'Auto-identify'),
          actionButton('outlierRemovalStep_remove', 'Remove selected'),
          actionButton('outlierRemovalStep_clear_sel', 'De-select all')
        ),
        fluidRow(
          id = 'outlierRemovalStep_confirm_tray',
          class = 'right-align',
          hr(),
          actionButton('outlierRemovalStep_reset', 'Reset', class = 'btn-primary'),
          actionButton('outlierRemovalStep_confirm', 'Submit', class = 'btn-success')
        ),
        fluidRow(
          id = 'outlierRemovalStep_preview_tray',
          hr(),
          p('Selected cells:'),
          dataTableOutput('outlierRemovalStep_preview_tb')
        )
      ),

      tabPanel(
        value = 'normalizationStep',
        strong("Normalization"),
        actionButton('bm_button_nz', icon("bookmark-o", lib="font-awesome"), class = 'bm-success'),
        fluidRow(
          id = 'normalizationStep_help_tray',
          p(h2(strong('Normalization'))),
          p(
            'Adjust expression levels to correct for artificial differences between cells, e.g., differences in sequencing depth. When a rescaling/normalization button is clicked, the box plot (showing expression statistics for up to 96 randomly selected cells) will reflect changes. For example, clicking "Rescaling to geometric mean" will cause red dots (geometric means) to align. Note that clicking more than one rescaling/normalization button will apply adjustments on already adjusted values (use "Reset" to go back to unadjusted data).'
          )
        ),
        fluidRow(id = 'normalizationStep_vis_tray',
                 plotOutput('normalizationStep_barchart', height = '450px')),
        fluidRow(id = 'normalizationStep_sampling_tray',
                 actionButton('normalizationStep_resample', 'Re-plot random 96 cells')),
        fluidRow(
          id = 'normalizationStep_algo_tray',
          hr(),
          actionButton('normalizationStep_quantile', 'Quantile normalization'),
          actionButton('normalizationStep_scalecenter', 'Rescale to genometric mean'),
          actionButton('normalizationStep_sizefactor', 'Size-factor normalization'),
          actionButton('normalizationStep_voom', 'Voom')
        ),
        fluidRow(
          id = 'normalizationStep_confirm_tray',
          class = 'right-align',
          hr(),
          downloadButton('normalizationStep_download_mat', 'Download the normalized matrix'),
          actionButton('normalizationStep_reset', 'Reset', class = 'btn-primary'),
          actionButton('normalizationStep_confirm', 'Submit', class = 'btn-success')
        )
      ),

      tabPanel(
        value = 'imputationStep',
        strong("Imputation"),
        actionButton('bm_button_im', icon("bookmark-o", lib="font-awesome"), class = 'bm-success'), #TODO
        fluidRow(
          id = 'imputationStep_help_tray',
          p(h2(strong('Imputation'))),
          p(
            'The large number of drop-outs might pose potential problems for downstream analyses. It is thus often appropriate to try to infer whether a zero is in the dataset is a drop-out -- that is, a non-zero expression level incorrectly assayed as zero. And if it is a drop-out, to infer its original expression level.'
          )
        ),
        fluidRow(id = 'imputationStep_vis_tray',
                 plotOutput('imputationStep_distribution', height = '450px')),
        fluidRow(
          id = 'imputationStep_algo_tray',
          hr(),
          actionButton('imputationStep_scimpute', 'scImpute')
          ## actionButton('imputationStep_saver', 'SAVER')
        ),
        fluidRow(
          id = 'imputationStep_confirm_tray',
          class = 'right-align',
          hr(),
          actionButton('imputationStep_reset', 'Reset', class = 'btn-primary'), #TODO
          actionButton('imputationStep_confirm', 'Submit', class = 'btn-success') #TODO
        )
      ),

      tabPanel(
        value = 'filterStep',
        strong("Gene filtering"),
        actionButton('bm_button_filterStep', icon("bookmark-o", lib="font-awesome"), class = 'bm-success'),
        fluidRow(
          id = 'filterStep_help_tray',
          p(h2(strong('Gene filtering'))),
          p(
            'Remove genes having very low expression and/or those with little variation (dispersion) by moving the sliders. It is recommended to keep at least 2,000 genes.'
          )
        ),
        fluidRow(
          id = 'filterStep_vis_tray',
          hr(),
          plotOutput('filterStep_vis_2', height = '600px'),
          textOutput('filterStep_vis_des'),
          fluidRow(column(
            6,
            sliderInput(
              "filterStep_met",
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
              "filterStep_dft",
              "Dispersion Fit Threshold",
              width = '100%',
              min = 0,
              max = 5,
              value = 1,
              step = 0.01
            )
          ))
        ),
        fluidRow(id = 'filterStep_statistics_tray',
                 hr(),
                 fluidRow(
                   column(
                     6,
                     span('Starting number of genes: '),
                     textOutput('filterStep_stat_current')
                   ),
                   column(
                     6,
                     span('Post-filtering number of genes: '),
                     textOutput('filterStep_stat_after')
                   )
                 )),
        fluidRow(
          id = 'filterStep_confirm_tray',
          class = 'right-align',
          hr(),
          actionButton('filterStep_confirm', 'Submit', class = 'btn-success')
        )
      ),

      tabPanel(
        value = 'clusterStep',
        strong("Clustering"),
        actionButton('bm_button_clusterStep', icon("bookmark-o", lib="font-awesome"), class = 'bm-success'),
        fluidRow(
          id = 'clusterStep_help_tray',
          p(h2(strong('Clustering'))),
          p(
            'Select a clustering method and enter a number of clusters (or check the box for auto selection), then click "Run clustering".'
          )
        ),
        fluidRow(id = 'clusterStep_vis_tray',
                 hr(),
                 fluidRow(
                   column(
                     12,
                     selectInput(
                       "clusterStep_select_col",
                       "Cell labels",
                       choices = list('No column in the metadata' = NULL),
                       width = '50%'
                     )
                   ),
                   column(6, plotOutput('clusterStep_vis_1')),
                   column(6, plotOutput('clusterStep_vis_2'))
                 )),
        fluidRow(
          id = 'clusterStep_algos_tray',
          hr(),
          radioButtons(
            'clusterStep_algo',
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
            'clusterStep_auto_num_clusters',
            'Automatically choose the number of clusters (might take a long time)'
          ),
          numericInput('clusterStep_num_clusters', 'Number of clusters', 3L, 1L, 20L, 1L),
          h5(textOutput('clusterStep_num_clusters_auto') %>% hidden),
          actionButton('clusterStep_cluster', 'Run clustering')
        ),
        fluidRow(
          id = 'clusterStep_confirm_tray',
          class = 'right-align',
          hr(),
          downloadButton('clusterStep_download_mat', 'Download the filtered matrix'),
          downloadButton('clusterStep_download', 'Download CSV table'),
          actionButton('clusterStep_confirm', 'Submit', class = 'btn-success')
        ) %>% hidden
      ),

      tabPanel(
        value = 'diffExpStep',
        strong("Differential expression"),
        actionButton('bm_button_de', icon("bookmark-o", lib="font-awesome"), class = 'bm-success'),
        fluidRow(
          id = 'diffExpStep_help_tray',
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
          id = 'diffExpStep_start_tray',
          hr(),
          radioButtons(
            'diffExpStep_methods',
            'Method',
            c(
              `NODES (fast)` = 'diffExp_nodes',
              `limma (fast)` = 'diffExp_limma',
              `edgeR (fast)` = 'diffExp_edgeR',
              `SCDE (slow)` = 'diffExp_scde'
            )
          ),
          selectInput(
            "diffExpStep_select_vec",
            "Which factor to use for DE?",
            choices = list(Clusters = 'Clusters'),
            width = '50%'
          ),
          actionButton('diffExpStep_start', 'Start analysis')
        ) %>% hidden,
        fluidRow(id = 'diffExpStep_too_few_genes_tray',
                 hr(),
                 p(strong(
                   textOutput('diffExpStep_too_few_genes_message')
                 )),
                 actionButton('diffExpStep_skip', 'Skip')) %>% hidden,
        fluidRow(
          id = 'diffExpStep_vis_tray',
          hr(),
          textOutput('diffExpStep_warn_message'),
          column(
            12,
            selectInput(
              "diffExpStep_select_col",
              "Cell labels",
              choices = list('No column in the metadata' = NULL),
              width = '50%'
            )
          ),
          column(6, plotOutput('diffExpStep_vis_1')),
          column(6, plotOutput('diffExpStep_vis_2'))
        ) %>% hidden,
        fluidRow(
          id = 'diffExpStep_res_tray',
          hr(),
          p(
            'Tabs indicate cluster numbers. Genes are sorted by absolute Z-score.'
          ),
          uiOutput('diffExpStep_tables')
        ) %>% hidden,
        fluidRow(
          id = 'diffExpStep_confirm_tray',
          class = 'right-align',
          hr(),
          downloadButton('diffExpStep_download', 'Download CSV table'),
          actionButton('diffExpStep_confirm', 'Submit', class = 'btn-success')
        ) %>% hidden
      ),

      tabPanel(
        value = 'proteinNetworkStep',
        strong("Protein network"),
        actionButton('bm_button_ne', icon("bookmark-o", lib="font-awesome"), class = 'bm-success'),
        fluidRow(
          id = 'proteinNetworkStep_help_tray',
          p(h2(strong('Protein network'))),
          p(
            'Proteins from top differentially expressed genes are visualized with connecting lines indicating documented biochemical interactions. Go to the next step by clicking "Proceed" (bottom right of page).'
          )
        ),
        fluidRow(id = 'proteinNetworkStep_vis_tray',
                 hr(),
                 uiOutput('proteinNetworkStep_networks')),
        fluidRow(
          id = 'proteinNetworkStep_confirm_tray',
          class = 'right-align',
          hr(),
          actionButton('proteinNetworkStep_confirm', 'Proceed', class = 'btn-success')
        )
      ),

      tabPanel(
        value = 'psuedoTimeStep',
        strong("Pseudo-time construction"),
        actionButton('bm_button_pt', icon("bookmark-o", lib="font-awesome"), class = 'bm-success'),
        fluidRow(
          id = 'psuedoTimeStep_help_tray',
          p(h2(strong(
            'Pseudo-time construction'
          ))),
          p(
            'Cells are ordered in pseudo-time using similarities between their expression profiles.'
          )
        ),
        fluidRow(
          id = 'psuedoTimeStep_res_tray',
          hr(),
          selectInput(
            "psuedoTimeStep_select_col",
            "Cell labels",
            choices = list('No column in the metadata' = NULL),
            width = '50%'
          ) %>% hidden,
          plotOutput('psuedoTimeStep_plot', height = '800px')
        )
      )
    )
  )
#)
}
