################################################################################
# Copyright 2017 University of Hawaii
#
# Licensed for academic research, study, or teaching. A special agreement with
# the copyright holder must otherwise be made for commercial use.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
################################################################################

# Granatum

# options -------------------------------

options(shiny.maxRequestSize = 10000 * 1024 ^ 2)
addResourcePath('www', 'www')

# helpers -------------------------------

# sanity checks:
#   all integers
#   minimum number of genes (2000) and cell numbers (50)
#   gene name check

# number of samples in the metadata vs. number of cols in the count table
# every sample must have an assignment

# display selected column in the metadata as colors in the scatterplot
# display both labeling and clustering as overlapping different size dots

# 1 vs. 2 should be Red vs. Blue

# z-score outlier-detection

# normalization:
# three buttons :
#- first method: align the boxes
#- second method: align the means
#- third method: size factors
#- FPKM normalization

read_mat <-
  function(filename,
           format = 'auto',
           row_names = T,
           col_names = T,
           ...) {
    if (format == 'auto') {
      if (filename %>% str_detect('\\.csv$')) {
        format <- 'csv'
      } else {
        format <- 'tsv'
      }
    }

    if (format == 'csv') {
      tb <- suppressWarnings(read_csv(filename, col_names = col_names, ...))
    } else {
      tb <- suppressWarnings(read_tsv(filename, col_names = col_names, ...))
    }

    tb <- tb %>% as.data.frame

    if (row_names) {
      rownames(tb) <- tb[[1]]
      tb[[1]] <- NULL
    }

    rw <- tb %>% data.matrix

    rw
  }





# server ------------------------------------------------------------------
source("load_dependencies.R")

server <- function(input, output, session) {
  # User space global variables

  print('HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH')
  print('HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH')
  print('HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH')
  print('HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH')
  print(devtools::session_info())

  raw_df <- NULL
  raw_mat <- NULL
  pr_dat <- NULL
  tsne_dat <- NULL
  sample_meta <- NULL
  raw_mat_l <- NULL
  pr <- NULL
  tsne <- NULL
  de_res <- NULL

  br_sampling <- NULL
  br_raw_mat_l <- NULL

  oi_selection <- numeric(0)

  oi_raw_mat_l <- NULL

  nz_raw_mat_l <- NULL
  nz_sampling <- NULL

  ft_raw_mat_l <- NULL
  ft_avg_log_expr_threshold <- 0
  ft_filter_vec <- NULL

  ft_cds <- NULL

  clusters <- NULL
  monocle_data <- NULL
  grouping_col <- NULL

  datasets_overview <- data_frame( dataset = character(0), n_genes = character(0), n_samples = character(0))
  raw_mat_list <- list()
  sample_meta_list <- list()
  num_datasets <- 1
  all_genes <- NULL
  all_samples <- NULL
  species <- NULL

  restoring <- FALSE
  previous_dir <- NULL
  current_dir <- NULL

  globalvars <- c(
    "raw_df",
    "raw_mat",
    "pr_dat",
    "tsne_dat",
    "sample_meta",
    "raw_mat_l",
    "pr",
    "tsne",
    "de_res",
    "br_sampling",
    "oi_selection",
    "oi_raw_mat_l",
    "nz_raw_mat_l",
    "nz_sampling",
    "ft_raw_mat_l",
    "ft_avg_log_expr_threshold",
    "ft_filter_vec",
    "ft_cds",
    "clusters",
    "monocle_data",
    "grouping_col",
    "datasets_overview",
    "raw_mat_list",
    "sample_meta_list",
    "num_datasets",
    "all_genes",
    "all_samples",
    "br_raw_mat_l",
    "species"
  )

  save_var <- function(var) {
    if (!is.null(eval(parse(text=var))) && !is.null(current_dir)) { eval(parse(text=sprintf("saveRDS(%s, sprintf(\"%s/%s.rds\", current_dir, var))", var, current_dir, var))) }
  }

  load_var <- function(var) {
    if (!is.null(current_dir)) {
      eval(parse(text=sprintf("if(file.exists(\"%s/%s.rds\")) { %s <<- readRDS(\"%s/%s.rds\") }", current_dir, var, var, current_dir, var)))
    }
  }

  force_bookmark <- function() {
    reactiveValuesToList(input)
    session$doBookmark()
  }

  force_new_bookmark <- function() {
    if(restoring) return()
    old_dir <- current_dir
    force_bookmark()

    # Then copy everything back
    dir.create(old_dir)
    flist <- list.files(current_dir)
    file.copy(from = file.path(current_dir, flist), to = file.path(old_dir, flist))
    write(as.integer(Sys.time()), sprintf("%s/disconnected", old_dir))
    write(as.integer(Sys.time()), sprintf("%s/branched", old_dir))

    url <- sprintf("http://ilab.hawaii.edu:8100/?_state_id_=%s&tab=%s", basename(old_dir), input$steps_list)
    # Display for user
    showModal(modalDialog(sprintf(
      'Warning: state may be garbage collected after some time. You may now use the following URL to access this state (copy to a safe place): %s', url
    )))
  }

  observe({ force_bookmark() })

  observeEvent(input$bm_button_uep, { force_new_bookmark() })
  observeEvent(input$bm_button_br, { force_new_bookmark() })
  observeEvent(input$bm_button_oi, { force_new_bookmark() })
  observeEvent(input$bm_button_nz, { force_new_bookmark() })
  observeEvent(input$bm_button_ft, { force_new_bookmark() })
  observeEvent(input$bm_button_cl, { force_new_bookmark() })
  observeEvent(input$bm_button_de, { force_new_bookmark() })
  observeEvent(input$bm_button_ne, { force_new_bookmark() })
  observeEvent(input$bm_button_pt, { force_new_bookmark() })

  onBookmarked(
    function(url) {
      updateQueryString(sprintf("%s&tab=%s", url, isolate(input$steps_list)))
      state_id <- url %>% str_match('_state_id_=([a-z0-9]+)') %>% .[2]
      current_dir <<- sprintf("shiny_bookmarks/%s", state_id)
      if(!is.null(previous_dir)) {
        file.rename(from = file.path(current_dir, 'input.rds'), to = file.path(current_dir, 'input.back.rds'))
        flist <- list.files(previous_dir, pattern="*.rds")
        if(file.exists(sprintf("%s/branched", previous_dir))) {
          file.copy(from = file.path(previous_dir, flist), to = file.path(current_dir, flist))
        } else {
          file.rename(from = file.path(previous_dir, flist), to = file.path(current_dir, flist))
          unlink(previous_dir, recursive=TRUE)
        }
        file.rename(from = file.path(current_dir, 'input.back.rds'), to = file.path(current_dir, 'input.rds'))
      }
      if(file.exists(sprintf("%s/disconnected", current_dir))) file.remove(sprintf("%s/disconnected", current_dir))
      previous_dir <<- current_dir
    }
  )

  br_select_col <- NULL

  onRestore(
    function(state) {
      restoring <<- TRUE

      current_dir <<- sprintf("shiny_bookmarks/%s", basename(state$dir))
      previous_dir <<- current_dir

      print("Loading saved state (before restore)")
      print(state$input$br_select_col)
      br_select_col <<- state$input$br_select_col

      lapply(globalvars, load_var)

      # select_step(isolate(state$input$steps_list))
    }
  )

  onRestored(function(state) {
    restoring <<- FALSE

    select_step(isolate(state$input$steps_list))

    br_select_col <<- NULL
  })

  onSessionEnded(
    function() {
      message('Granatum message: session ended')
      write(as.integer(Sys.time()), sprintf("%s/disconnected", current_dir))
    }
  )

  observeEvent(input$uep_no_meta, {
    if(restoring) { return() }
    if (isolate(input$uep_no_meta)) {
      shinyjs::hide('uep_sample_meta_selector')
    } else {
      shinyjs::show('uep_sample_meta_selector')
    }
  })

  observeEvent(input$uep_expression_table_format_pop_btn, {
    if(restoring) { return() }
    showModal(modalDialog(div(
      class = 'centered',
      img(
        id = 'uep_logo_table_format',
        class = 'format-diagram',
        src = 'table_format.svg'
      )
    ), size = 'l'))
  })

  observeEvent(input$uep_sanity_check, {
    if(restoring) { return() }
    withProgress(message = 'Loading data ...', {
      # upload
      risky <- function() {
        raw_mat <<-
          read_mat(input$uep_expr_file_selector$datapath, format = 'csv')

        if (min(raw_mat) < 0) {
          stop(sprintf(
            'There are negative values found in your expresison table. We expect non-negative expression levels.',
            nrow(raw_mat)
          ))
        }

        if (nrow(raw_mat) < 51) {
          stop(sprintf(
            'The number of genes (%s) is smaller than the minimum requirement (50).',
            nrow(raw_mat)
          ))
        }

        if (nrow(raw_mat) < 51) {
          stop(sprintf(
            'The number of genes (%s) is smaller than the minimum requirement (50).',
            nrow(raw_mat)
          ))
        }

        if (ncol(raw_mat) < 2) {
          stop(
            sprintf(
              'The number of samples (%s) is smaller than the minimum requirement (2).',
              ncol(raw_mat)
            )
          )
        }

        if (isolate(input$uep_no_meta)) {
          sample_meta <<- data_frame(ID = colnames(raw_mat))
        } else {
          sample_meta <<- read_csv(input$uep_sample_meta_selector$datapath)
        }

        sample_tb_in_table <- data_frame(ID = colnames(raw_mat))
        colnames(sample_meta)[1] <<- 'ID'

        sample_meta <<-
          sample_tb_in_table %>% inner_join(sample_meta, by = 'ID')

        if (nrow(sample_meta) < ncol(raw_mat)) {
          stop(
            sprintf(
              'Looks like some of the samples in the count table is not annotated by any row in the sample
              metadata. Please check your uploaded files.'
            )
            )
        }

        if (nrow(sample_meta) == 0) {
          stop(
            sprintf(
              'Looks like entries in the first column of the metadata do not correspond to the column names
              of the count table. Please check your uploaded files.'
            )
            )
        }

        raw_mat <<-
          apply(raw_mat, 1, function(x)
            any(x > 0)) %>% raw_mat[.,]
        if (ncol(sample_meta) == 1) {
          sample_meta <<- sample_meta %>% mutate(batch = 'A')
        }

        species <<- input$uep_species
        }

      try <- risky %>% quietly %>% safely %>% {
        .()
      }

      if (is.null(try$error)) {
        if (length(try$result$warning) == 0) {
          try$result$result
        } else {
          showModal(modalDialog(sprintf(
            'Format check failed: %s', try$result$warnings
          )))
          return()
        }
      } else {
        showModal(modalDialog(sprintf(
          'Format check failed: %s', try$error$message
        )))
        return()
      }

      raw_df <<- raw_mat %>% as_data_frame %>%
        mutate(Gene = rownames(raw_mat)) %>%
        select(Gene, everything())

      if (is.null(all_samples)) {
        all_samples <<- colnames(raw_mat)
      } else {
        all_samples <<- union(all_samples, colnames(raw_mat))
      }

      if (is.null(all_genes)) {
        all_genes <<- rownames(raw_mat)
      } else {
        all_genes <<- union(all_genes, rownames(raw_mat))
      }

      datasets_overview <<- datasets_overview %>%
        add_row(
          dataset = as.character(num_datasets),
          n_genes = nrow(raw_mat),
          n_samples = ncol(raw_mat)
        )

      lapply(
        c(
          "datasets_overview",
          "all_genes",
          "all_samples",
          "raw_df",
          "species",
          "raw_mat",
          "sample_meta"
        ),
        save_var
      )

      uep_prep()

      })
  })

  observeEvent(input$uep_reset, {
    if(restoring) { return() }
    js$refresh()
  })

  observeEvent(input$uep_go_back, {
    if(restoring) { return() }
    shinyjs::hide('uep_preview_tray')
    shinyjs::hide('uep_confirm_tray')
    shinyjs::show('uep_upload_tray')
  })

  oi_refresh_plots <- function() {
    # for some reason plotly only supports keys when using ggplotly interface
    #pr_dat %>%
    #  mutate(selected=factor(ifelse(key %in% oi_selection, 'Remove', 'Remain'))) %>%
    #  {ggplot(.) + geom_point(aes(pc1, pc2, color=selected, key=key)) + theme(legend.position='none')} %>%
    #  print

    if(is.null(pr_dat)) return()
    if(is.null(tsne_dat)) return()
    if(restoring) return()

    plot_helper <-
      function(dat, x_name, y_name, x_title, y_title, hide_legend) {
        p <- ggplot(dat)
        if (!is.null(oi_selection) && length(oi_selection) > 0) {
          p <-
            p + geom_point(
              aes_string(x_name, y_name),
              data = dat %>% filter(key %in% oi_selection),
              size = 3,
              alpha = 0.2
            )
        }
        if (is.null(grouping_col) || grouping_col == '') {
          p <-
            p + geom_point(aes_string(x_name, y_name, key = 'key', sample_id = 'ID'))
        } else {
          p <-
            p + geom_point(aes_string(
              x_name,
              y_name,
              color = grouping_col,
              key = 'key',
              sample_id = 'ID'
            ))
        }
        p <- p + theme(legend.title = element_blank())
        if (hide_legend) {
          p <- p + theme(legend.position = 'none')
        }

        ggplotly(p, source = 'oi_vis') %>%
          layout(xaxis = list(title = x_title, fixedrange = T)) %>%
          layout(yaxis = list(title = y_title, fixedrange = T)) %>%
          config(displayModeBar = T)
      }

    p1 <- plot_helper(pr_dat, 'pc1', 'pc2', 'PC1', 'PC2', F)
    p2 <- plot_helper(tsne_dat, 'tsne1', 'tsne2', 'tSNE1', 'tSNE2', F)

    output$oi_preview_tb <-
      renderDataTable(
        sample_meta[as.integer(oi_selection), ],
        options = list(
          searching = F,
          lengthChange = F,
          scrollY = '150px',
          scrollCollapse = T,
          paging = F
        )
      )
    output$oi_vis_1 <- renderPlotly(p1)
    output$oi_vis_2 <- renderPlotly(p2)

    #output$oi_vis <- renderPlotly({ subplot(list(p1, p2), titleX=T, titleY=T, margin=0.05) })
  }

  calculate_dimrs <- function(matl) {
    withProgress(message = 'Computing visualization data ...', {
      raw_mat_local <- if (input$oi_top_expressed_only) {
        matl %>% apply(1, mean) %>% order %>% tail(50) %>% {
          matl[., ]
        }
      } else {
        matl
      }

      pr <<- subsampling_PCA(t(raw_mat_local))
      pr_dat <<-
        data_frame(
          ID = colnames(raw_mat_local),
          pc1 = pr[, 1],
          pc2 = pr[, 2],
          key = 1:nrow(pr)
        ) %>%
        left_join(sample_meta, by = 'ID')

      tsne <<-
        Rtsne((1 - cor(raw_mat_local)) ^ 2,
              dims = 2,
              is_distance = T,
              perplexity = min(30, floor((
                ncol(raw_mat_local) - 1
              ) / 3))
        )$Y
      tsne_dat <<-
        data_frame(
          ID = colnames(raw_mat_local),
          tsne1 = tsne[, 1],
          tsne2 = tsne[, 2],
          key = 1:nrow(tsne)
        ) %>%
        left_join(sample_meta, by = 'ID')

      lapply(
        c(
          "pr",
          "pr_dat",
          "tsne",
          "tsne_dat"
        ),
        save_var
      )

    })
  }

  observeEvent(input$uep_add_another_dataset, {
    if(restoring) { return() }
    raw_mat_list[[num_datasets]] <<- raw_mat
    sample_meta_list[[num_datasets]] <<-
      sample_meta %>% mutate(dataset = factor(num_datasets))
    num_datasets <<- num_datasets + 1

    lapply(
      c(
        "raw_mat_list",
        "sample_meta_list",
        "num_datasets"
      ),
      save_var
    )

    shinyjs::hide('uep_preview_tray')
    shinyjs::hide('uep_confirm_tray')
    shinyjs::show('uep_example_tray')
    shinyjs::show('uep_upload_tray')
  })

  br_refresh_plots <- function() {
    if (restoring) { return() }
    withProgress(message = 'Plotting the barchart ...', {
      if(is.null(br_raw_mat_l)) return()

      br_raw_mat_l_sampled <- br_raw_mat_l[, br_sampling]

      print("Replotting the barchart")
      print(input$br_select_col)

      if (isolate(input$br_select_col) == "") {
        if ('dataset' %in% colnames(sample_meta)) {
          br_select_col_default <- 'dataset'
        } else {
          br_select_col_default <- colnames(sample_meta)[2]
        }
        batch_vec <- sample_meta[[br_select_col_default]]
      } else {
        batch_vec <- sample_meta[[isolate(input$br_select_col)]]
      }

      names(batch_vec) <- sample_meta$ID

      batches <- batch_vec[colnames(br_raw_mat_l_sampled)] %>%
        enframe('sample', 'Batch')

      ggdat <- br_raw_mat_l_sampled %>%
        melt(c('gene', 'sample'), value.name = 'log_expr') %>%
        filter(log_expr > 0) %>%
        left_join(batches)

      ggdat_gmean <- ggdat %>%
        group_by(sample) %>%
        summarize(gmean = mean(log_expr))

      br_p <-
        ggplot(ggdat) + geom_boxplot(aes(sample, log_expr, fill = Batch)) +
        geom_point(
          aes(sample, gmean),
          data = ggdat_gmean,
          color = 'red',
          alpha = 0.5
        ) +
        labs(x = 'Sample', y = 'Log Expr. Lvl. (non-zero)') +
        theme(axis.text.x = element_text(angle = 90))

      output$br_barchart <- renderPlot({
        withProgress(message = 'Rendering the plot ...', {
          br_p
        })
      })
    })
  }

  observeEvent(input$info_next, {
    if(restoring) { return() }
    updateTabsetPanel(session, 'steps_list', 'uep')
  })

  # test change

  observeEvent(input$uep_submit, {
    if(restoring) { return() }
    if (num_datasets > 1) {
      raw_mat_list[[num_datasets]] <<- raw_mat
      sample_meta_list[[num_datasets]] <<-
        sample_meta %>% mutate(dataset = factor(num_datasets))

      sample_meta <<- bind_rows(sample_meta_list)

      raw_mat <<-
        matrix(
          0,
          nrow = length(all_genes),
          ncol = length(all_samples),
          dimnames = list(all_genes, all_samples)
        )

      for (m in raw_mat_list) {
        raw_mat[rownames(m), colnames(m)] <- m
      }
    }

    raw_mat <<- raw_mat[rowSums(raw_mat) > 0, ]

    raw_mat_l <<- log(raw_mat + 1)

    lapply(
      c(
        "raw_mat",
        "raw_mat_l",
        "sample_meta",
        "raw_mat_list",
        "sample_meta_list"
      ),
      save_var
    )

    updateTabsetPanel(session, 'steps_list', 'br')
  })

  observeEvent(input$br_resample, {
    if(restoring) { return() }
    br_resample_num <- min(ncol(br_raw_mat_l), 96)
    br_sampling <<-
      sample(1:ncol(br_raw_mat_l), br_resample_num) %>% sort
    br_refresh_plots()
    save_var("br_sampling")
  })

  observeEvent(input$br_select_col, { if(restoring) { return() }; br_refresh_plots() })

  observeEvent(input$br_reset, {
    if(restoring) { return() }
    br_raw_mat_l <<- raw_mat_l
    save_var("br_raw_mat_l")
    br_refresh_plots()
  })

  observeEvent(input$br_perform, {
    if(restoring) { return() }
    withProgress(message = 'Removing batch effect ...', {
      batch_vec <- sample_meta[[isolate(input$br_select_col)]]

      if (length(unique(batch_vec)) < 2) {
        showModal(modalDialog(sprintf('Need to have more than one different levels in the batch vector.')))
        return()
      }
      if (isolate(input$br_method) == 'mid_align') {
        names(batch_vec) <- sample_meta$ID

        sample_medians <-
          apply(raw_mat_l, 2, function(x)
            median(x[x > 0]))
        cross_batch_mean <- max(sample_medians)
        batch_levels <- unique(batch_vec)
        for (l in batch_levels) {
          one_batch_mean <- mean(sample_medians[batch_vec == l])
          print(batch_vec == l)
          print(one_batch_mean)
          print(cross_batch_mean)
          br_raw_mat_l[, batch_vec == l] <<-
            raw_mat_l[, batch_vec == l] - one_batch_mean + cross_batch_mean
          br_raw_mat_l[raw_mat_l == 0] <<- 0
        }
      } else if (isolate(input$br_method) == 'combat') {
        br_raw_mat_l <<- ComBat(raw_mat_l, batch_vec)
        br_raw_mat_l <<- log(exp(br_raw_mat_l) + 1)
      }

      save_var("br_raw_mat_l")

      br_refresh_plots()
    })
  })

  observeEvent(input$go_back_uep, {
    if(restoring) { return() }
    updateTabsetPanel(session, 'steps_list', 'uep')
    removeModal()
  })

  observeEvent(input$go_back_de, {
    if(restoring) { return() }
    updateTabsetPanel(session, 'steps_list', 'de')
    removeModal()
  })

  raw_data_modal <- function() {
    showModal(modalDialog(
      sprintf('Raw data is missing. Need to go to Upload step and upload data first.'),
      footer = tagList(actionButton("go_back_uep","Go back"))
    ))
  }

  sample_meta_modal <- function() {
    showModal(modalDialog(
      sprintf('Meta data is missing in Upload step. Please upload meta data first.'),
      footer = tagList(actionButton("go_back_uep","Go back"))
    ))
  }

  uep_prep <- function() {
    if(is.null(datasets_overview) || is.null(raw_df)) {
      return()
    }
    datasets_overview_formatted <- datasets_overview %>%
      mutate(n_genes = n_genes %>% as.character,
             n_samples = n_samples %>% as.character) %>%
      add_row(
        dataset = '',
        n_genes = length(all_genes) %>% {
          sprintf('Total num of distinct genes: %d', .)
        } %>% strong %>% as.character,
        n_samples = length(all_samples) %>% {
          sprintf('Total num of samples: %d', .)
        } %>% strong %>% as.character
      ) %>%
      rename(
        `Dataset` = dataset,
        `Number of genes` = n_genes,
        `Number of samples` = n_samples
      )

    output$uep_overview_tb <- renderDataTable(
      datasets_overview_formatted,
      escape = F,
      options = list(
        searching = F,
        lengthChange = F,
        scrollY = '150px',
        scrollCollapse = T,
        paging = F,
        info = F
      )
    )

    if (ncol(raw_df) - 1 > 500) {
      showModal(modalDialog(p(
        sprintf(
          'There are %d cells in the table and to reduce table rendering time we only show a random sample of 500 cells.',
          ncol(raw_df) - 1
        )
      ),
      footer = tagList(modalButton('OK'))))
      output$uep_preview_exp_profile_dt <-
        renderDataTable(raw_df[, c(1, sample(2:ncol(raw_df), 500))],
                        options = list(
                          pageLength = 10,
                          autoWidth = T,
                          scrollX = T
                        ))
    } else {
      output$uep_preview_exp_profile_dt <-
        renderDataTable(raw_df,
                        options = list(
                          pageLength = 10,
                          autoWidth = T,
                          scrollX = T
                        ))
    }

    output$uep_preview_metadata_dt <-
      renderDataTable(sample_meta,
                      options = list(
                        pageLength = 10,
                        autoWidth = T,
                        scrollX = T
                      ))

    shinyjs::hide('uep_upload_tray')
    shinyjs::show('uep_preview_tray')
    shinyjs::show('uep_confirm_tray')

  }

  br_prep <- function() {
    if(is.null(raw_mat_l)) { raw_data_modal(); return() }
    if(is.null(sample_meta)) { sample_meta_modal(); return() }

    if ('dataset' %in% colnames(sample_meta)) {
      br_select_col_default <- 'dataset'
    } else {
      br_select_col_default <- colnames(sample_meta)[2]
    }

    if(is.null(br_select_col)) {
      updateSelectInput( session, 'br_select_col', choices = colnames(sample_meta)[-1], selected = br_select_col_default )
    } else {
      updateSelectInput( session, 'br_select_col', choices = colnames(sample_meta)[-1], selected = br_select_col )
    }

    br_raw_mat_l <<- raw_mat_l

    if (ncol(br_raw_mat_l) > 96) {
      br_sampling <<- sample(1:ncol(br_raw_mat_l), 96) %>% sort
      shinyjs::show('br_sampling_tray')
    } else {
      br_sampling <<- 1:ncol(br_raw_mat_l)
    }

    save_var("br_raw_mat_l")
    save_var("br_sampling")

#    br_refresh_plots()
  }

  oi_prep <- function() {
    if(is.null(raw_mat_l)) { raw_data_modal(); return() }

    oi_raw_mat_l <<- raw_mat_l

    calculate_dimrs(oi_raw_mat_l)

    save_var("oi_raw_mat_l")
    save_var("oi_selection")

    if(is.null(grouping_col) || grouping_col == "") {
      updateSelectInput(session, 'oi_select_col', choices = colnames(sample_meta)[-1])
    } else {
      updateSelectInput(session, 'oi_select_col', choices = colnames(sample_meta)[-1], selected = grouping_col)
    }
  }

  nz_prep <- function() {
    if(is.null(raw_mat_l)) { raw_data_modal(); return() }

    nz_raw_mat_l <<- raw_mat_l

    if (ncol(nz_raw_mat_l) > 96) {
      nz_sampling <<- sample(1:ncol(nz_raw_mat_l), 96) %>% sort
      shinyjs::show('nz_sampling_tray')
    } else {
      nz_sampling <<- 1:ncol(nz_raw_mat_l)
    }

    save_var("nz_raw_mat_l")
    save_var("nz_sampling")

    nz_refresh_plots()
  }

  im_prep <- function() {
    if(is.null(raw_mat_l)) { raw_data_modal(); return() }

    im_raw_mat_l <<- raw_mat_l

    save_var("im_raw_mat_l")

    im_refresh_plots()
  }

  ft_prep <- function() {
    if(is.null(raw_mat_l)) { raw_data_modal(); return() }
    if(is.null(sample_meta)) { sample_meta_modal(); return() }

    if (!is.null(ft_cds)) {
      ft_cds <<- NULL
    }

    ft_raw_mat_l <<- raw_mat_l
    ft_refresh_plots_disp()
  }

  cl_prep <- function() {
    if(is.null(raw_mat_l)) { raw_data_modal(); return() }
    if(is.null(sample_meta)) { sample_meta_modal(); return() }

    clusters <<- rep('unclustered', ncol(raw_mat_l))
    save_var("clusters")

    #calculate_dimrs(raw_mat_l)

    #cl_refresh_plots(pr_dat, tsne_dat)
    updateSelectInput(session, 'cl_select_col', choices = colnames(sample_meta)[-1])
  }

  de_prep <- function() {
    if(is.null(raw_mat_l)) { raw_data_modal(); return() }
    if(is.null(sample_meta)) { sample_meta_modal(); return() }

    if(is.null(de_res)) {
      shinyjs::show('de_start_tray')
      shinyjs::hide('de_vis_tray')
      shinyjs::hide('de_res_tray')
      shinyjs::hide('de_confirm_tray')
    } else {
      shinyjs::hide('de_start_tray')
      shinyjs::show('de_vis_tray')
      shinyjs::show('de_res_tray')
      shinyjs::show('de_confirm_tray')
      de_refresh_plots()
    }

    updateSelectInput(session, 'de_select_col', choices = colnames(sample_meta)[-1])
    updateSelectInput(session, 'de_select_vec', choices = c(Clusters = "Clusters", colnames(sample_meta)[-1]))
  }

  ne_prep <- function() {
    if(is.null(raw_mat_l)) { raw_data_modal(); return() }
    if(is.null(sample_meta)) { sample_meta_modal(); return() }
    if(is.null(de_res)) {
      showModal(modalDialog(
        sprintf('Differential expression was not run for the dataset. It must be run first.'),
        footer = tagList(actionButton("go_back_de","Go back"))
      ))
      return()
    }

    tbps <- list()
    for (i in 1:length(de_res)) {
      name <- names(de_res)[i]
      tbps[[i]] <- tabPanel(
        name,
        plotOutput(sprintf('ne_network_color_bar_%d', i), height = '100px'),
        visNetworkOutput(sprintf('ne_network_pane_%d', i), height = '800px')
      )
    }
    tbset <- do.call(tabsetPanel, tbps)
    output$ne_networks <- renderUI(tbset)

    render_i <- function(i) {
      tbi <- de_res[[i]]

      ppi_graph <- readRDS(sprintf('ppi/%s.RDS', species))

      tbi <- tbi[rownames(tbi) %in% V(ppi_graph)$name, ]

      top_genes <- head(rownames(tbi), 300)
      z_scores <- head(tbi$Z, 300)

      cps <-
        ppi_graph %>% induced_subgraph(top_genes) %>% components

      top_genes_c <-
        names(cps$membership[cps$membership %in% which(cps$csize >= 2)])

      z_scores_c <-
        z_scores[top_genes %in% top_genes_c] %>% squish(c(-3, 3))

      ppi_graph_f <- ppi_graph %>% induced_subgraph(top_genes_c)

      V(ppi_graph_f)$color <-
        z_scores_c %>% rescale_mid %>% {
          colour_ramp(c('blue', 'white', 'red'))(.)
        }

      output[[sprintf('ne_network_pane_%d', i)]] <-
        renderVisNetwork(
          ppi_graph_f %>% visIgraph(physics = T) %>% visPhysics(
            solver = "forceAtlas2Based",
            forceAtlas2Based = list(gravitationalConstant = -500)
          )
        )

      limit <- max(max(z_scores_c),-min(z_scores_c))
      output[[sprintf('ne_network_color_bar_%d', i)]] <-
        renderPlot({
          par(oma = c(0, 0, 0, 0))
          par(mar = c(2, 1, 3, 1))
          image(
            matrix(1:1000, ncol = 1),
            xaxt = 'n',
            yaxt = 'n',
            col = colorRampPalette(c('blue', 'white', 'red'))(1000)
          )
          axis(1, c(0, 0.5, 1), c(sprintf('%0.3g',-limit), '0', sprintf('%0.3g', limit)))
          title('Z-scores (blue = Down-regulation, red = Up-regulation)')
        })
    }

    lapply(1:length(de_res), render_i)
  }

  pt_prep <- function() {
    if(is.null(raw_mat_l)) { raw_data_modal(); return() }
    if(is.null(sample_meta)) { sample_meta_modal(); return() }
    output$pt_plot <- renderPlot(withProgress(message = 'Running monocle ...', {
      print(sample_meta)
      monocle_sample_sheet <-
        sample_meta %>% as.data.frame %>% column_to_rownames('ID') %>% .[colnames(raw_mat_l), , drop =
                                                                           F]
      print(monocle_sample_sheet)
      print(dim(monocle_sample_sheet))
      print(dim(raw_mat))
      raw_mat <-
        raw_mat[rownames(raw_mat_l), colnames(raw_mat_l)]
      monocle_data <<- do_monocle(raw_mat, monocle_sample_sheet)
      updateSelectInput(
        session,
        'pt_select_col',
        choices = colnames(monocle_sample_sheet),
        selected = grouping_col
      )
      shinyjs::show('pt_select_col')
      plot_monocle(monocle_data)
    }))
  }

  select_step <- function(step) {
    switch(
      step,
      uep = uep_prep(),
      br = br_prep(),
      oi = oi_prep(),
      nz = nz_prep(),
      ft = ft_prep(),
      im = im_prep(),
      cl = cl_prep(),
      de = de_prep(),
      ne = ne_prep(),
      pt = pt_prep()
    )
  }

  observeEvent(input$steps_list, {
    if(restoring) {return()}
    select_step(isolate(input$steps_list))
  })

  observeEvent(input$br_confirm, {
    if(restoring) { return() }

    raw_mat_l <<- br_raw_mat_l
    save_var("raw_mat_l")

    updateTabsetPanel(session, 'steps_list', 'oi')
  })

  observeEvent(input$oi_select_col, {
    if(restoring) { return() }
    if(isolate(input$oi_select_col) == "") return()

    grouping_col <<- isolate(input$oi_select_col)
    save_var("grouping_col")

    oi_refresh_plots()
  })

  observe({
    selected_data <- event_data('plotly_selected', source = 'oi_vis')
    for (k in selected_data$key) {
      if (k %in% oi_selection) {
        oi_selection <<- setdiff(oi_selection, k)
      } else {
        oi_selection <<- union(oi_selection, k)
      }
    }
    save_var("oi_selection")
    oi_refresh_plots()
  })

  observe({
    click_data <- event_data('plotly_click', source = 'oi_vis')
    for (k in click_data$key) {
      if (k %in% oi_selection) {
        oi_selection <<- setdiff(oi_selection, k)
      } else {
        oi_selection <<- union(oi_selection, k)
      }
    }
    save_var("oi_selection")
    oi_refresh_plots()
  })

  observeEvent(input$oi_top_expressed_only, suspended = T, {
    if(restoring) return()
    if(is.null(oi_raw_mat_l)) return()

    calculate_dimrs(oi_raw_mat_l)

    oi_refresh_plots()
  })

  observeEvent(input$oi_clear_sel, {
    if(restoring) return()

    oi_selection <<- numeric(0)
    save_var("oi_selection")

    oi_refresh_plots()
  })

  observeEvent(input$oi_auto_outlier, {
    if(restoring) { return() }
    showModal(
      modalDialog(
        numericInput('oi_z_score_threshold', 'Z-score threshold', 4, 0, 10, step =
                       0.25),
        numericInput('oi_num_outlier', 'Number of Outliers', 1L, 1L, 1000L, step =
                       1L) %>% disabled,
        radioButtons(
          'oi_which_threshold',
          'Using',
          c(
            `Z-score threshold` = 'zscore',
            `Fixed number of samples` = 'num'
          )
        ),
        radioButtons(
          'oi_which_vis',
          'According to',
          c(PCA = 'pca', `Correlation t-SNE` = 'tsne')
        ),
        footer = tagList(
          modalButton('Cancel'),
          actionButton('oi_auto_outlier_ok', 'OK')
        )
      )
    )
  })

  observeEvent(input$oi_which_threshold, {
    if(restoring) { return() }
    if (isolate(input$oi_which_threshold) == 'num') {
      shinyjs::disable('oi_z_score_threshold')
      shinyjs::enable('oi_num_outlier')
    } else {
      shinyjs::disable('oi_num_outlier')
      shinyjs::enable('oi_z_score_threshold')
    }
  })

  observeEvent(input$oi_auto_outlier_ok, {
    if(restoring) { return() }
    mat <- switch(
      input$oi_which_vis,
      pca = cbind(pr_dat$pc1, pr_dat$pc2),
      tsne = cbind(tsne_dat$tsne1, tsne_dat$tsne2)
    )

    z_scores <- mat %>%
      scale %>% t %>% apply(2, function(x)
        sum(x ^ 2))

    message('z_scores = ')
    print(z_scores)

    if (isolate(input$oi_which_threshold) == 'num') {
      message('num was chosen')
      oi_selection <<-
        order(z_scores) %>% tail(isolate(input$oi_num_outlier))
    } else {
      print(z_scores)
      message('num was not chosen')
      threshold <- isolate(input$oi_z_score_threshold)
      message(sprintf('threshold is %s', threshold))
      num_outliers <- sum(z_scores > threshold)
      message(sprintf('num_outliers is %s', num_outliers))

      oi_selection <<- order(z_scores) %>% tail(num_outliers)
    }

    save_var("oi_selection")

    oi_refresh_plots()

    removeModal()
  })

  observeEvent(input$oi_remove, {
    if(restoring) { return() }

    if (!is.null(oi_selection) && length(oi_selection) > 0) {
      oi_raw_mat_l <<- oi_raw_mat_l[, -as.integer(oi_selection)]

      calculate_dimrs(oi_raw_mat_l)

      oi_selection <<- numeric(0)
      save_var("oi_selection")

      oi_refresh_plots()
    }
  })

  observeEvent(input$oi_reset, {
    if(restoring) { return() }

    oi_raw_mat_l <<- raw_mat_l
    save_var("oi_raw_mat_l")

    calculate_dimrs(oi_raw_mat_l)

    oi_refresh_plots()
  })

  nz_refresh_plots <- function() {
    withProgress(message = 'Plotting the barchart ...', value = 0, {
      ggdat <- nz_raw_mat_l[, nz_sampling] %>%
        melt(c('gene', 'sample'), value.name = 'log_expr') %>%
        filter(log_expr > 0)

      incProgress(amount = 0.2, message = "Copying data")

      ggdat_gmean <- ggdat %>%
        group_by(sample) %>%
        summarize(gmean = mean(log_expr))

      incProgress(amount = 0.2, message = "Calculating mean")

      nz_p <-
        ggplot(ggdat) + geom_boxplot(aes(sample, log_expr)) +
        geom_point(
          aes(sample, gmean),
          data = ggdat_gmean,
          color = 'red',
          alpha = 0.5
        ) +
        labs(x = 'Sample', y = 'Log Expr. Lvl. (non-zero)') +
        theme(axis.text.x = element_text(angle = 90))

      incProgress(amount = 0.2, message = "Creating boxplot")

      output$nz_barchart <- renderPlot(nz_p)

      incProgress(amount = 0.2, message = "May need to wait while Shiny renders plot")

      Sys.sleep(0.5)
    })
  }

  observeEvent(input$oi_confirm, {
    if(restoring) { return() }

    raw_mat_l <<- oi_raw_mat_l
    raw_mat <<- raw_mat[, colnames(raw_mat_l)]
    save_var("raw_mat_l")
    save_var("raw_mat")

    updateSelectInput(session, 'cl_select_col', selected = isolate(input$oi_select_col))
    updateTabsetPanel(session, 'steps_list', 'nz')
  })

  observeEvent(input$nz_resample, {
    if(restoring) { return() }
    nz_resample_num <- min(ncol(nz_raw_mat_l), 96)
    nz_sampling <<-
      sample(1:ncol(nz_raw_mat_l), nz_resample_num) %>% sort
    save_var("nz_sampling")
    nz_refresh_plots()
  })

  deseq_sizefactor_normalization <- function(counts) {
    withProgress(message = 'Calculating size-factors ...', {
      relative_factors <-
        (1 / apply(counts, 1, function(x) {
          x %>% {
            . + 1
          } %>% log %>% mean %>% exp %>% {
            . - 1
          }
        })) %>%
        map_if( ~ . == Inf, ~ 0) %>% (purrr::simplify)
      relative_counts <- counts * relative_factors

      size_factors <-
        apply(relative_counts, 2, function(x)
          median(x[x > 0]))

      t(t(counts) / size_factors)
    })
  }

  observeEvent(input$nz_quantile, {
    if(restoring) { return() }

    nz_raw_mat_l <<- normalizeQuantiles(nz_raw_mat_l, ties = F)
    save_var("nz_raw_mat_l")

    nz_refresh_plots()
  })

  observeEvent(input$nz_scalecenter, {
    if(restoring) { return() }
    raw_mat <- exp(nz_raw_mat_l) - 1

    means <-
      exp(apply(nz_raw_mat_l, 2, function(x)
        mean(x[x > 0]))) - 1
    raw_mat <- sweep(raw_mat, 2, means / mean(means), '/')

    nz_raw_mat_l <<- log(raw_mat + 1)

    # repeat

    raw_mat <- exp(nz_raw_mat_l) - 1

    means <-
      exp(apply(nz_raw_mat_l, 2, function(x)
        mean(x[x > 0]))) - 1
    raw_mat <- sweep(raw_mat, 2, means / mean(means), '/')

    nz_raw_mat_l <<- log(raw_mat + 1)

    save_var("nz_raw_mat_l")

    nz_refresh_plots()
  })

  observeEvent(input$nz_sizefactor, {
    if(restoring) { return() }

    nz_raw_mat_l <<- nz_raw_mat_l %>% exp %>% {
      . - 1
    } %>%
      deseq_sizefactor_normalization %>% {
        . + 1
      } %>% log

    save_var("nz_raw_mat_l")

    nz_refresh_plots()
  })

  observeEvent(input$nz_voom, {
    if (restoring) return()

    nz_raw_mat_l <<-
      nz_raw_mat_l %>% {
        exp(.) + 1
      } %>% voom(normalize.method = 'quantile') %>% .$E %>% {
        . - min(.)
      }
    save_var("nz_raw_mat_l")

    nz_refresh_plots()
  })

  observeEvent(input$nz_reset, {
    if(restoring) { return() }
    nz_raw_mat_l <<- raw_mat_l
    save_var("nz_raw_mat_l")
    nz_refresh_plots()
  })

  output$nz_download_mat <- downloadHandler(
    filename = function() {
      "matrix.csv"
    },
    content = function(file) {
      write('"Please cite: Zhu, Xun et al. “Granatum: A Graphical Single-Cell RNA-Seq Analysis Pipeline for Genomics Scientists.” Genome Medicine 9.1 (2017)"\n',file=file)
      nz_raw_mat_l %>% {
        exp(.) - 1
      } %>% as.data.frame %>% rownames_to_column('Gene') %>% write_csv(file,append=TRUE)
    }
  )

  im_refresh_plots <- function() {
    withProgress(message = 'Plotting the distribution ...', {
      ggdat <- im_raw_mat_l %>%
        melt(c('gene', 'sample'), value.name = 'log_expr')

      nz_p <-
        ggplot(ggdat) +
        geom_histogram(aes(log_expr)) +
        labs(x = 'Log Exp. Lvl.', y = 'Number of entries in the matrix')

      output$im_distribution <- renderPlot(nz_p)
    })
  }

  observeEvent(input$im_saver, {
    if (restoring) return()

    withProgress(message = 'Performing SAVER ...', {
      im_raw_mat_l <<- SAVER::saver(im_raw_mat_l)$estimate
    })

    save_var("im_raw_mat_l")

    im_refresh_plots()
  })

  observeEvent(input$im_scimpute, {
    if (restoring) return()

    tryCatch({
      withProgress(message = 'Performing scImpute ...', {
        im_raw_mat_l <<- scImpute::scimpute(im_raw_mat_l)
      })
    }, error = function(err) {
        stop(sprintf(
            'Something went wrong',
            err
          ))
    })

    save_var("im_raw_mat_l")

    im_refresh_plots()
  })

  observeEvent(input$im_reset, {
    if(restoring) { return() }
    im_raw_mat_l <<- raw_mat_l
    save_var("im_raw_mat_l")
    im_refresh_plots()
  })

  observeEvent(input$im_confirm, {
    if(restoring) { return() }

    if(is.null(im_raw_mat_l)) return()
    raw_mat_l <<- im_raw_mat_l
    save_var("raw_mat_l")

    updateTabsetPanel(session, 'steps_list', 'ft')
  })

  ft_refresh_plots_disp <- function() {
    withProgress(message = 'Calculating dispersion ... ', {
      if (is.null(ft_cds)) {
        mat <- ft_raw_mat_l

        cds <-
          newCellDataSet(mat,
                         lowerDetectionLimit = 0.0001,
                         expressionFamily = negbinomial.size())
        cds <- estimateSizeFactors(cds)
        cds <- estimateDispersions(cds)

        ft_cds <<- cds
        save_var("ft_cds")
      } else {
        cds <- ft_cds
      }

      d <- isolate(input$ft_dft)
      m <- exp(isolate(input$ft_met))

      mdt <- dispersionTable(cds)
      mog <-
        subset(mdt,
               mean_expression >= m &
                 dispersion_empirical >= d * dispersion_fit)$gene_id
      cds <- setOrderingFilter(cds, mog)

      disp_table <- dispersionTable(cds) %>%
        mutate(log_mean_expression      = log(mean_expression)) %>%
        mutate(log_dispersion_empirical = log(dispersion_empirical))

      ordering_genes <-
        row.names(subset(fData(cds), use_for_ordering == TRUE))

      g <-
        ggplot(disp_table,
               aes(log_mean_expression, log_dispersion_empirical)) +
        geom_point(color = 'darkgrey') +
        geom_line(aes(y = log(dispersion_fit)), color = 'red') +
        geom_point(data = disp_table %>% filter(gene_id %in% ordering_genes),
                   color = 'black') +
        labs(x = 'Log Mean Expression', y = 'Log Empirical Dispersion') +
        theme_bw()

      met_min <- min(disp_table$log_mean_expression) %>% round(2)
      met_max <- max(disp_table$log_mean_expression) %>% round(2)

      updateSliderInput(session, 'ft_met', min = met_min, max = met_max)

      output$ft_vis_2 <- renderPlot(g)
      ft_filter_vec <<-
        rownames(fData(cds))[fData(cds)$use_for_ordering]

      save_var("ft_filter_vec")

      output$ft_stat_current <- renderText(nrow(ft_raw_mat_l))
      output$ft_stat_after <- renderText(length(ft_filter_vec))
    })
  }

  observeEvent(input$ft_met, {
    if(restoring) return()
    if(is.null(ft_raw_mat_l)) return()
    ft_refresh_plots_disp()
  })

  observeEvent(input$ft_dft, {
    if(restoring) return()
    if(is.null(ft_raw_mat_l)) return()
    ft_refresh_plots_disp()
  })

  observeEvent(input$nz_confirm, {
    if(restoring) { return() }

    if(is.null(nz_raw_mat_l)) return()
    raw_mat_l <<- nz_raw_mat_l
    save_var("raw_mat_l")

    updateTabsetPanel(session, 'steps_list', 'im')
  })

  cl_refresh_plots <- function(pr_dat, tsne_dat) {
    # for some reason plotly only supports keys when using ggplotly interface
    plot_helper <-
      function(dat,
               x_name,
               y_name,
               x_title,
               y_title,
               hide_legend) {
        dat <- dat %>% mutate(clusters = clusters, grouping_col = grouping_col)

        p <- ggplot(dat)
        if (is.null(clusters) ||
            length(clusters) == 0 || clusters[1] == 'unclustered') {
          message(sprintf('x_name = %s', x_name))
          message(sprintf('y_name = %s', y_name))
          message(sprintf('grouping_col = %s', grouping_col))
          p <- p + geom_point(aes_string(x_name, y_name, color = grouping_col))
        } else {
          p <-
            p + geom_point(aes_string(x_name, y_name, color = grouping_col), size =
                             4.2)
          p <-
            p + geom_text(
              aes_string(x_name, y_name, label = 'clusters'),
              size = 3.9,
              color = 'white'
            )
        }
        p <- p + theme(legend.title = element_blank())
        if (hide_legend) {
          p <- p + theme(legend.position = 'none')
        }

        p <- p + labs(x = x_title, y = y_title)

        p
      }

    p1 <- plot_helper(pr_dat, 'pc1', 'pc2', 'PC1', 'PC2', F)
    p2 <-
      plot_helper(tsne_dat, 'tsne1', 'tsne2', 'tSNE1', 'tSNE2', F)

    output$cl_vis_1 <- renderPlot(p1)
    output$cl_vis_2 <- renderPlot(p2)
  }

  observeEvent(input$ft_confirm, {
    if(restoring) { return() }
    updateCheckboxInput(session, 'oi_top_expressed_only', value = F)

    ft_raw_mat_l <<- ft_raw_mat_l[ft_filter_vec, ]
    raw_mat_l <<- ft_raw_mat_l

    save_var("ft_raw_mat_l")
    save_var("raw_mat_l")

    updateTabsetPanel(session, 'steps_list', 'cl')

  })

  run_nmf <- function(mat, n) {
    matt <- apply(mat, 1, function(x)
      any(x > 0)) %>% mat[., ]
    ren <-
      nmf(
        matt,
        rank = n,
        nrun = 30,
        method = "brunet",
        .options = list(
          debug = F,
          verbose = 3,
          parallel = isolate(input$uep_num_cores)
        ),
        seed = 12345
      )
    predict(ren) %>% factor
  }

  run_hclust <- function(mat, n) {
    cutree(hclust(dist(t(mat))), n) %>% factor
  }

  run_kmeans <- function(mat, n) {
    kmeans(t(mat), n)$cluster %>% factor
  }

  observeEvent(input$cl_select_col, {
    if(restoring) return()
    if(isolate(input$cl_select_col) == "") return()
    grouping_col <<- isolate(input$cl_select_col)
    save_var("grouping_col")
    calculate_dimrs(raw_mat_l)
    cl_refresh_plots(pr_dat, tsne_dat)
  })

  observeEvent(input$cl_auto_num_clusters, {
    if(restoring) { return() }
    if (isolate(input$cl_auto_num_clusters)) {
      shinyjs::hide('cl_num_clusters')
      shinyjs::show('cl_num_clusters_auto')
    } else {
      shinyjs::show('cl_num_clusters')
      shinyjs::hide('cl_num_clusters_auto')
    }
  })

  do_clustering <- function(n) {
    message(sprintf('do_clustering(%s)', n))
    cls <- switch(
      isolate(input$cl_algo),
      nmf = run_nmf(raw_mat_l, n),
      kmeans = run_kmeans(raw_mat_l, n),
      kmeans_corr = run_kmeans(t(tsne), n),
      hclust = {
        showModal(modalDialog(size = 'l', plotOutput('cl_heatmap', height = '800px')))
        output$cl_heatmap <-
          renderPlot(withProgress(message = 'Generating heatmap ...', heatmap(raw_mat_l)))
        run_hclust(raw_mat_l, n)
      },
      hclust_corr = run_hclust(t(tsne), n)
    )

    cls
  }

  observeEvent(input$cl_cluster, {
    if(restoring) { return() }
    clusters <<- if (input$cl_auto_num_clusters) {
      withProgress(message = 'Computing clusters ...',
                   {
                     ss <- c(0)
                     for (i in 2:20) {
                       km <- kmeans(t(raw_mat_l), i)
                       ss[i] <- km$betweenss
                     }

                     rr <- c(0)
                     x <- c(1:20)

                     for (i in 2:20) {
                       x2 <- pmax(1, x - i + 2)
                       rr[i] <- sum(lm(ss ~ x + x2)$residuals ^ 2)
                     }
                     num_cl <- which.min(rr[-1]) + 1

                     output$cl_num_clusters_auto <-
                       renderText(sprintf('Automatically selected number of clusters: %s', num_cl))

                     do_clustering(num_cl)
                   })
    } else {
      withProgress(
        message = sprintf(
          'Computing clusters (with %s expected clusters) ...',
          isolate(input$cl_num_clusters)
        ),
        do_clustering(isolate(input$cl_num_clusters))
      )
    }
    save_var("clusters")

    cl_refresh_plots(pr_dat, tsne_dat)

    output$cl_download_mat <- downloadHandler(
      filename = function() {
        "matrix.csv"
      },
      content = function(file) {
      write('"Please cite: Zhu, Xun et al. “Granatum: A Graphical Single-Cell RNA-Seq Analysis Pipeline for Genomics Scientists.” Genome Medicine 9.1 (2017)"\n',file=file)
        raw_mat_l %>% {
          exp(.) - 1
        } %>% as.data.frame %>% rownames_to_column('Gene') %>% write_csv(file, append=T)
      }
    )

    output$cl_download <- downloadHandler(
      filename = function() {
        "clustering.csv"
      },
      content = function(file) {
      write('"Please cite: Zhu, Xun et al. “Granatum: A Graphical Single-Cell RNA-Seq Analysis Pipeline for Genomics Scientists.” Genome Medicine 9.1 (2017)"\n',file=file)
        cluster_df <-
          data_frame(sample = colnames(raw_mat_l), cluster = clusters)
        print(colnames(sample_meta))
        print(colnames(cluster_df))
        sample_meta %>% left_join(cluster_df, by = c(ID = 'sample')) %>% write_csv(file, append=T)
      }
    )

    shinyjs::show('cl_confirm_tray')
  })

  de_refresh_plots <- function() {
    # for some reason plotly only supports keys when using ggplotly interface
    plot_helper <-
      function(dat,
               x_name,
               y_name,
               x_title,
               y_title,
               hide_legend) {
        if(grouping_col == "" || is.null(grouping_col)) { return()  }
        p <- ggplot(dat)
        if (is.null(clusters) || length(clusters) == 0) {
          p <- p + geom_point(aes_string(x_name, y_name, color = grouping_col))
        } else {
          print(x_name)
          print(y_name)
          print(grouping_col)
          p <-
            p + geom_point(aes_string(x_name, y_name, color = grouping_col), size =
                             4.2)
          p <-
            p + geom_text(
              aes_string(x_name, y_name, label = 'clusters'),
              size = 3.9,
              color = 'white'
            )
        }
        p <- p + theme(legend.title = element_blank())
        if (hide_legend) {
          p <- p + theme(legend.position = 'none')
        }

        p <- p + labs(x = x_title, y = y_title)

        p
      }

    p1 <- plot_helper(pr_dat, 'pc1', 'pc2', 'PC1', 'PC2', F)
    p2 <- plot_helper(tsne_dat, 'tsne1', 'tsne2', 'tSNE1', 'tSNE2', F)

    output$de_vis_1 <- renderPlot(p1)
    output$de_vis_2 <- renderPlot(p2)
  }

  observeEvent(input$de_select_col, {
    if(restoring) return()
    if(isolate(input$de_select_col) == "") return()
    grouping_col <<- isolate(input$de_select_col)
    save_var("grouping_col")
    de_refresh_plots()
  })

  observeEvent(input$cl_confirm, {
    if(restoring) { return() }
    updateTabsetPanel(session, 'steps_list', 'de')
    updateSelectInput(session, 'de_select_col', selected = isolate(input$cl_select_col))
  })

  observeEvent(input$de_start, {
    if(restoring) { return() }
    shinyjs::hide('de_start_tray')

    withProgress(message = 'Computing differential expression ...', {
      mat <- raw_mat[rownames(raw_mat_l), colnames(raw_mat_l)]
      n_clusters <- clusters %>% unique %>% length
      if (ncol(mat) > n_clusters * 48) {
        idx <- sample(1:ncol(mat), n_clusters * 48)
        mat <- mat[, idx]
        clusters <- clusters[idx]
      }
      mode(mat) <- 'integer'

      if (isolate(input$de_select_vec) == 'Clusters') {
        de_res <<-
          do_diff_exp(
            mat,
            clusters,
            TRUE,
            isolate(input$uep_num_cores),
            isolate(input$de_methods)
          )
      } else {
        meta_col <- sample_meta[[isolate(input$de_select_vec)]]
        names(meta_col) <- sample_meta$ID
        vec <- meta_col[colnames(raw_mat_l)]
        de_res <<-
          do_diff_exp(mat,
                      vec,
                      TRUE,
                      isolate(input$uep_num_cores),
                      isolate(input$de_methods))
      }

      save_var("de_res")

      de_refresh_plots()
    })

    tbps <- list()
    for (i in 1:length(de_res)) {
      name <- names(de_res)[i]
      tbps[[i]] <- tabPanel(
        name,
        dataTableOutput(sprintf('de_res_tables_pane_%d', i)),
        actionButton(sprintf('de_kegg_analysis_%d', i), 'KEGG enrichment'),
        actionButton(
          sprintf('de_go_analysis_%d', i),
          'Gene Ontology enrichment'
        ),
        plotOutput(sprintf('de_plot_%d', i), height = '600px') %>% hidden
      )
    }
    tbset <- do.call(tabsetPanel, tbps)
    output$de_tables <- renderUI(tbset)


    render_i <- function(tbi) {
      renderDataTable(
        options = list(
          pageLength = 10,
          autoWidth = T,
          scrollX = T
        ),
        escape = F,
        isolate({
          tbi %>%
            mutate(
              gene = sprintf(
                '<a href="http://www.genecards.org/Search/Keyword?queryString=%s" target="_blank">%s</a>',
                rownames(tbi),
                rownames(tbi)
              )
            ) %>%
            select(gene, everything())
        })
      )
    }

    do_kegg_analysis <- function(genes_, scores_, species_, title_) {
      species_code <- switch(species_, mouse = 'Mm', human = 'Hs')
      species_code_long <-
        switch(species_, mouse = 'mmu', human = 'hsa')

      print('mappedkeys(KEGGPATHID2EXTID) = ')
      print(mappedkeys(KEGGPATHID2EXTID))
      kegg_pathways <-
        mappedkeys(KEGGPATHID2EXTID) %>% str_subset(species_code_long) %>% str_replace(sprintf('^%s(.*)$', species_code_long), '\\1')
      print('kegg_pathways = ')
      print(kegg_pathways)
      kegg_names <-
        KEGGPATHID2NAME[kegg_pathways] %>% as.list %>% simplify
      kegg_to_eg <-
        KEGGPATHID2EXTID[kegg_pathways %>% {
          sprintf('%s%s', species_code_long, .)
        }] %>% as.list
      names(kegg_to_eg) <- kegg_names

      print('kegg_to_eg = ')
      print(kegg_to_eg)

      SYMBOL2EG <-
        eval(parse(text = sprintf(
          'org.%s.egSYMBOL2EG', species_code
        )))

      names(scores_) <- genes_

      genes <- intersect(genes_, mappedkeys(SYMBOL2EG))

      scores_ <- scores_[genes]

      gene_entrez <-
        genes %>% SYMBOL2EG[.] %>% as.list %>% map( ~ .[1]) %>% simplify

      names(scores_) <- gene_entrez

      dput(genes)

      print('scores_ = ')
      print(scores_)

      fgseaRes <- fgsea(kegg_to_eg, scores_, nperm = 10000)

      ggdat <-
        fgseaRes %>% as.data.frame %>% as_data_frame %>% arrange(-abs(NES)) %>% head(20) %>% mutate(pathway =
                                                                                                      fct_inorder(pathway))
      print('ggdat = ')
      print(ggdat)

      ggplot(ggdat) +
        geom_point(aes(
          x = pathway,
          y = abs(NES),
          size = size
        )) +
        labs(title = paste("KEGG:", title_),
             x = 'Pathway',
             y = 'Absolute Normalized Enrichment Score') +
        scale_size_continuous(name = 'Size of\nthe pathway') +
        theme_grey(base_size = 20) +
        theme(axis.text.x = element_text(angle = -30, hjust = 0),
              plot.margin = margin(l = 50))
    }

    do_go_analysis <- function(genes_, scores_, species_, title_) {
      species_code <- switch(species_, mouse = 'Mm', human = 'Hs')

      SYMBOL2EG <-
        eval(parse(text = sprintf(
          'org.%s.egSYMBOL2EG', species_code
        )))
      GO2ALLEGS <-
        eval(parse(text = sprintf(
          'org.%s.egGO2ALLEGS', species_code
        )))

      names(scores_) <- genes_

      genes <- intersect(genes_, mappedkeys(SYMBOL2EG))

      scores_ <- scores_[genes]

      print(length(genes))

      gene_entrez <-
        genes %>% SYMBOL2EG[.] %>% as.list %>% map( ~ .[1]) %>% simplify

      print(length(gene_entrez))

      names(scores_) <- gene_entrez

      go_terms <-
        intersect(mappedkeys(GO2ALLEGS), mappedkeys(GOTERM))
      go_to_eg <- GO2ALLEGS[go_terms] %>% as.list
      names(go_to_eg) <-
        names(go_to_eg) %>% GOTERM[.] %>% as.list %>% map( ~ .@Term) %>% simplify
      fgseaRes <-
        fgsea(
          go_to_eg,
          scores_,
          nperm = 10000,
          minSize = 50,
          maxSize = 500
        )
      ggdat <-
        fgseaRes %>% as.data.frame %>% as_data_frame %>% arrange(-abs(NES)) %>% head(20) %>% mutate(pathway =
                                                                                                      fct_inorder(pathway))
      ggplot(ggdat) +
        geom_point(aes(
          x = pathway,
          y = abs(NES),
          size = size
        )) +
        labs(title = paste("GO:", title_),
             x = 'Gene set',
             y = 'Absolute Normalized Enrichment Score') +
        scale_size_continuous(name = 'Gene set\nsize') +
        theme_grey(base_size = 20) +
        theme(axis.text.x = element_text(angle = -30, hjust = 0),
              plot.margin = margin(l = 50))
    }

    lapply(1:length(de_res), function(i) {
      output[[sprintf('de_res_tables_pane_%d', i)]] <-
        render_i(de_res[[i]])
      observeEvent(input[[sprintf('de_kegg_analysis_%d', i)]], {
        output[[sprintf('de_plot_%d', i)]] <-
          renderPlot(withProgress(message =
                                    'Performing enrichment analysis ...', {
                                      do_kegg_analysis(rownames(de_res[[i]]),
                                                       de_res[[i]]$Z,
                                                       species,
                                                       names(de_res)[i])
                                    }))
        shinyjs::show(sprintf('de_plot_%d', i))
      })
      observeEvent(input[[sprintf('de_go_analysis_%d', i)]], {
        output[[sprintf('de_plot_%d', i)]] <- renderPlot(withProgress(message =
                                                                        'Performing enrichment analysis ...', {
                                                                          do_go_analysis(rownames(de_res[[i]]),
                                                                                         de_res[[i]]$Z,
                                                                                         species,
                                                                                         names(de_res)[i])
                                                                        }))
        shinyjs::show(sprintf('de_plot_%d', i))
      })
    })

    shinyjs::show('de_vis_tray')
    shinyjs::show('de_res_tray')
    shinyjs::show('de_confirm_tray')
  })

  output$de_download <- downloadHandler(
    filename = function() {
      "differential_expression.csv"
    },
    content = function(file) {
      write('"Please cite: Zhu, Xun et al. “Granatum: A Graphical Single-Cell RNA-Seq Analysis Pipeline for Genomics Scientists.” Genome Medicine 9.1 (2017)"\n',file=file)
      de_res %>%
        lapply(function(x)
          rownames_to_column(x)) %>%
        enframe('comparison', 'data') %>%
        unnest %>%
        write_csv(file, append=T)
    }
  )

  observeEvent(input$de_confirm, {
    if(restoring) { return() }
    updateTabsetPanel(session, 'steps_list', 'ne')
  })

  observeEvent(input$ne_confirm, {
    if(restoring) { return() }
    updateTabsetPanel(session, 'steps_list', 'pt')
  })

  observeEvent(input$pt_select_col, {
    if(is.null(monocle_data)) { return() }
    output$pt_plot <- renderPlot(withProgress(message = 'Replotting ...', {
      plot_monocle(monocle_data, isolate(input$pt_select_col))
    }))
  })

  # =============== Following are downloadHandlers for examples =======================

  output$uep_example_expr <- downloadHandler(
    filename = 'Combined_expression.csv',
    content = function(con) {
      data <-
        read_csv('doc/Kim_et_al_2016_datasets/Combined_expression.csv')
      write_csv(data, con)
    }
  )

  output$uep_example_expr_batch1 <- downloadHandler(
    filename = 'Expression_1.csv',
    content = function(con) {
      data <- read_csv('doc/Kim_et_al_2016_datasets/Expression_1.csv')
      write_csv(data, con)
    }
  )

  output$uep_example_expr_batch2 <- downloadHandler(
    filename = 'Expression_2.csv',
    content = function(con) {
      data <- read_csv('doc/Kim_et_al_2016_datasets/Expression_2.csv')
      write_csv(data, con)
    }
  )

  output$uep_example_expr_batch3 <- downloadHandler(
    filename = 'Expression_3.csv',
    content = function(con) {
      data <- read_csv('doc/Kim_et_al_2016_datasets/Expression_3.csv')
      write_csv(data, con)
    }
  )

  output$uep_example_meta <- downloadHandler(
    filename = 'Combined_metadata.csv',
    content = function(con) {
      data <-
        read_csv('doc/Kim_et_al_2016_datasets/Combined_metadata.csv')
      write_csv(data, con)
    }
  )

  output$uep_example_meta_batch1 <- downloadHandler(
    filename = 'Metadata_1.csv',
    content = function(con) {
      data <- read_csv('doc/Kim_et_al_2016_datasets/Metadata_1.csv')
      write_csv(data, con)
    }
  )

  output$uep_example_meta_batch2 <- downloadHandler(
    filename = 'Metadata_2.csv',
    content = function(con) {
      data <- read_csv('doc/Kim_et_al_2016_datasets/Metadata_2.csv')
      write_csv(data, con)
    }
  )

  output$uep_example_meta_batch3 <- downloadHandler(
    filename = 'Metadata_3.csv',
    content = function(con) {
      data <- read_csv('doc/Kim_et_al_2016_datasets/Metadata_3.csv')
      write_csv(data, con)
    }
  )
}
