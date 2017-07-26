subsampling_PCA <- function(m, subsample_size=300) {
  if (nrow(m) > subsample_size) {
    rw_ss <- m[sample(1:nrow(m), subsample_size), ]
  } else {
    rw_ss <- m
  }

  p <- prcomp(rw_ss)

  (m - rep(p$center, nrow(m))) %*% p$rotation
}
