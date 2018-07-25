#' Nonparametric differential expression analysis for scRNA-seq data.
#' @param data \code{pQ} normalized single cell data.
#' @param group An array of strings containing unique group identifiers for cells. For example \code{c('A','A','B','B')}.
#' @param r A numeric value indicating number of permutations of outcomes to be done for
#' generting the empirical distribution for \code{D} statistic. Default value is \code{20}.
#' @param smooth_points A numeric value indicating number of bins for smoothing, default is \code{10000}.
#' @param zper Indicating quantile of pooled standard errors; default is \code{0.5}.
#' @return A table reporting \code{p}-values and \code{q}-values with original ordering of genes retained.

#' data(data_Trapnell) # load the Trapnell data
#' norm_Data <- pQ(data_Trapnell) # pQ normalize the data
#' grp <- c(rep('T0',75),rep('T24',71)) # Group assignment
#' Res <- NODES(norm_Data[1:10,],grp) # Compute p-value just for 10 genes




## NODES
NODES <- function(data, group, r = 20, smooth_points = 10000, zper = 0.5) {

  library(MetaDE)

  # This part is for identifying the groups
  indices <- list()

  U <- unique(group)

  for (i in 1:length(U)) {
    indices[[as.character(U[i])]] <- grep(U[i], group)
  }

  # length of group 1
  n1 <- length(indices[[U[1]]])
  n2 <- length(indices[[U[2]]])


  # Getting Noise distribution from NOISeq and estimate p values

  Zr <- NULL
  for (i in 1:r) {

    print(paste("Randomization run =", i))

    mipermu <- sample(1:(n1 + n2))  ## randomize labels

    mipermu <- data[, mipermu]  ## randomize matrix columns accordingly

    mean1 <- rowMeans(mipermu[, 1:n1])  ## get the means for random group 1
    mean2 <- rowMeans(mipermu[, (n1 + 1):(n1 + n2)])  ## get the means for random group 2

    sd1 <- apply(mipermu[, 1:n1], 1, sd)  ## sd for group 1
    sd2 <- apply(mipermu[, (n1 + 1):(n1 + n2)], 1, sd)  ## sd for group 2

    myparam <- list(n = c(n1, n2), sd = cbind(sd1, sd2))

    MDperm <- MDbio(dat = cbind(mean1, mean2), param = myparam, a0per = zper)

    Zr <- cbind(Zr, MDperm$D)

  }


  # Estimating noise density using Gaussian kernel
  cat("\nSmoothing the noise density...\n")
  dF <- approxfun(density(as.vector(Zr), n = smooth_points))

  # Getting stat for all genes

  mean1 <- rowMeans(as.matrix(data[, indices[[U[1]]]]))
  mean2 <- rowMeans(as.matrix(data[, indices[[U[2]]]]))

  sd1 <- apply(as.matrix(data[, indices[[U[1]]]]), 1, sd)
  sd2 <- apply(as.matrix(data[, indices[[U[2]]]]), 1, sd)

  myparam <- list(n = c(n1, n2), sd = cbind(sd1, sd2))

  Ds <- MDbio(dat = cbind(mean1, mean2), param = myparam, a0per = zper)

  Zs <- Ds$D
  Zrs <- Ds$Dr


  # Estimating p values only from noise distribution

  prob_1 <- apply(as.matrix(Zs), 1, function(x) den(dF, as.vector(Zr),
    x, smooth_points))



  # getting wilcoxon p values
  cat("\nComputing Wilcoxon p values...\n")
  prob_2 <- apply(data, 1, function(x) wilcox.test(x[indices[[U[1]]]],
    x[indices[[U[2]]]])$p.value)

  # Fisher's method to combine p values
  cat("\nCombining p values using Fisher's method...\n")

  together <- list()


  together[["p"]] <- as.matrix(cbind(prob_1, prob_2))
  META <- MetaDE::MetaDE.pvalue(together, meta.method = "Fisher")
  PVAL <- META$meta.analysis$pval
  # names(PVAL)<-rownames(data)


  # fdr
  fdr <- p.adjust(PVAL, method = "fdr")

  # prepare final result
  res <- data.frame(cbind(pvalues = PVAL, qvalues = fdr, Z = Zrs))

  rownames(res) <- rownames(data)

  res$abs_Z <- abs(res$Z)

  res <- res[order(-abs(res$Z)), ]

  cat("\nCompleted successfully.\n")

  # return
  return(res)
}


# Tail probability estimation
den <- function(approx, obs, val, points) {
  # if(val>=max(obs) || val <= min(obs))
  if (val >= max(obs)) {
    pF <- 1/(1 + points)
  } else {
    # pF <-
    # min(integrate(approx,val,max(obs))$value,integrate(approx,min(obs),val)$value)
    pF <- integrate(approx, val, max(obs), subdivisions=2000)$value
  }

  return(pF)  # 2*pF otherwise
}


## This code is a shadow of NOISeqbio
MDbio <- function(dat = dat, param = NULL, a0per = 0.5) {

  ddr <- (dat[,1]-dat[,2]) # this is for two tailed.
  dd <- abs(dat[, 1] - dat[, 2])


  # sd.D = sqrt(param$sd[,1]^2/sqrt(param$n[1]) +
  # param$sd[,2]^2/sqrt(param$n[2]))
  sd.D <- sqrt((param$sd[, 1]^2/param$n[1]) + (param$sd[, 2]^2/param$n[2]))

  a0per <- as.numeric(a0per)
  a0.D <- quantile(sd.D, probs = a0per, na.rm = TRUE)

  dd <- dd/(a0.D + sd.D)

  # Results
  list(D = dd, Dr=ddr)
}
