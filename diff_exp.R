do_limma_for_two_groups <- function (mat, vec, n_cores)  {
  vec <- paste('group_', as.character(vec), sep='')
  design <- model.matrix(~0+vec)
  lvls <- vec %>% factor %>% levels
  colnames(design) <- lvls
  contrast <- makeContrasts(contrasts=sprintf('%s - %s', lvls[1], lvls[2]), levels=design)
  fit <- lmFit(mat, design)
  fit <- contrasts.fit(fit, contrast)
  fit <- eBayes(fit)
  res <- topTable(fit, number=Inf)
  res$Z <- res$logFC

  res
}

do_edgeR_for_two_groups <- function (mat, vec, n_cores)  {
  cds <- DGEList(mat, group=vec)
  cds <- cds[rowSums(1e+06 * cds$counts/expandAsMatrix(cds$samples$lib.size, dim(cds)) > 1) >= 3, ]
  cds <- calcNormFactors( cds )
                                        #plotMDS( cds , main = "MDS Plot for Count Data", labels = sub("^.*(.)..","\\1",colnames( cds$counts )) )
  de <- exactTest(cds, dispersion=0.2)

  res <- topTags(de, Inf)$table

  res$Z <- res$logFC

  res
}

do_nodes_for_two_groups <- function (mat, vec, n_cores)  {
  NODES(mat, as.character(vec))
}

do_scde_for_two_groups <- function (mat, vec, n_cores)  {

  scde_counts             <- data.frame(mat)
  scde_sg                 <- factor(vec)

  names(scde_sg)          <- colnames(scde_counts)

  scde_cd                 <- scde_counts
  scde_o_ifm              <- scde.error.models(counts = scde_cd,
                                               groups = scde_sg,
                                               n.cores = n_cores,
                                               min.size.entries = min(2000, nrow(mat)),
                                               threshold.segmentation = TRUE,
                                               save.model.plots = FALSE,
                                               verbose = 1)
  scde_valid_cells        <- scde_o_ifm$corr.a > 0
  scde_o_ifm              <- scde_o_ifm[scde_valid_cells,]
  scde_o_prior            <- scde.expression.prior(models = scde_o_ifm, counts = scde_cd, length.out = 400, show.plot = FALSE)
  scde_sg_valid           <- scde_sg[rownames(scde_o_ifm)]

  scde_ediff              <- scde.expression.difference(scde_o_ifm,
                                                        scde_cd, scde_o_prior,
                                                        groups = scde_sg_valid,
                                                        n.randomizations = 100,
                                                        n.cores = n_cores,
                                                        verbose = 0)

  p.values <- 2*pnorm(abs(scde_ediff$Z),lower.tail=F) # 2-tailed p-value
  p.values.adj <- 2*pnorm(abs(scde_ediff$cZ),lower.tail=F) # Adjusted to control for FDR

  scde_ediff %>% as.data.frame %>% rownames_to_column('Gene') %>%
    mutate(p.values = p.values) %>%
    mutate(p.values.adj = p.values.adj) %>%
    arrange(p.values.adj) %>%
    as.data.frame %>%
    column_to_rownames('Gene')
}

do_diff_exp <- function (mat, vec, pairwise=F, n_cores, method)  {
  vec <- factor(vec)
  lvls <- levels(vec)

  if (length(lvls) < 2) stop('Need at least two groups for DE.')

  output <- list()

  if (length(lvls) == 2 || pairwise) {
    pairs <- as.list(as.data.frame(combn(lvls, 2)))
    for (pair in pairs) {
      vec2 <- vec[vec %in% pair]
      mat2 <- mat[,vec %in% pair]

      output[[sprintf('%s vs. %s', pair[1], pair[2])]] <-
        switch(method,
               'nodes' = do_nodes_for_two_groups(mat2, vec2, n_cores),
               'scde' = do_scde_for_two_groups(mat2, vec2, n_cores),
               'edgeR' = do_edgeR_for_two_groups(mat2, vec2, n_cores),
               'limma' = do_limma_for_two_groups(mat2, vec2, n_cores))
    }
  } else {
    for (l in lvls) {
      lvls2        <- rep(2, length(lvls))
      names(lvls2) <- lvls
      lvls2[l]     <- 1
      vec2         <- lvls2[vec]

      output[[sprintf('%s vs. all other', l)]] <-
        switch(method,
               'nodes' = do_nodes_for_two_groups(mat, vec2, n_cores),
               'scde' = do_scde_for_two_groups(mat, vec2, n_cores),
               'edgeR' = do_edgeR_for_two_groups(mat, vec2, n_cores),
               'limma' = do_limma_for_two_groups(mat, vec2, n_cores))
    }
  }

  output
}
