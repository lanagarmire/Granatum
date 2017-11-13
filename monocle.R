library(monocle)


do_monocle <- function (monocle_expr_matrix, monocle_sample_sheet) { 

  monocle_gene_annotation <- data.frame(row.names=rownames(monocle_expr_matrix))

  # Setup
  monocle_pd <- new("AnnotatedDataFrame", data = monocle_sample_sheet)
  monocle_fd <- new("AnnotatedDataFrame", data = monocle_gene_annotation)
  
  # Expects read counts in matrix
  monocle_data <- newCellDataSet(as(as.matrix(monocle_expr_matrix), "sparseMatrix"),
                                 phenoData = monocle_pd,
                                 featureData = monocle_fd,
                                 lowerDetectionLimit=1,
                                 expressionFamily=negbinomial.size()) # "Slightly less accurate for differential expression than negbinomial(), but much, much faster."
  
  # Pre-calculate values for later
  monocle_data <- estimateSizeFactors(monocle_data)
  monocle_data <- estimateDispersions(monocle_data)
  
  monocle_expressed_genes <- row.names(monocle_data)
  
  monocle_disp_table <- dispersionTable(monocle_data)
  
  #monocle_ordering_genes <- subset(monocle_disp_table,
  #                                 mean_expression >= 0.1 &
  #                                 dispersion_empirical >= 1 * dispersion_fit)$gene_id
  monocle_ordering_genes <- monocle_disp_table$gene_id
  monocle_data <- setOrderingFilter(monocle_data, monocle_ordering_genes)
  
  monocle_data <- reduceDimension(monocle_data, max_components = 2)
  monocle_data <- orderCells(monocle_data, reverse=FALSE)

  monocle_data
}

plot_monocle <- function (monocle_data, color_by=NULL) { 
  plot_cell_trajectory(monocle_data, color_by=color_by, show_tree=T, show_backbone=T) +
    theme_grey(base_size=20)
}

