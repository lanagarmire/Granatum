context("test-nodesdiffexp.R")

test_that("test diff exp fun", {
  
  # Todo: create an rda or other file with test examples.
  # Below is just a dump of our test DF (dput(mat))
  data = structure(c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 63L, 2L, 0L, 0L, 
    0L, 245L, 0L, 0L, 0L, 0L, 7L, 123L, 0L, 0L, 0L, 0L, 58L),
    .Dim = c(5L, 5L), .Dimnames = list(c("ENSMUSG00000000037", "ENSMUSG00000000093", 
    "ENSMUSG00000000125", "ENSMUSG00000000127", "ENSMUSG00000000134"
    ), c("P1A8", "P1A9", "P1B12", "P1B6", "P1B7")))
  
  groups = c("1", "1", "1", "5", "1")
  
  tryCatch({
    nodesDiffExp(data, groups)  
    expect_equal("Success", "Failure")
  }, error = function (err) {
    expect_equal("Caught", "Caught")
  })
  
  data2 = structure(c(0L, 0L, 0L, 25L, 10L, 0L, 138L, 0L, 215L, 62L, 432L, 
    1092L, 7L, 351L, 0L, 0L, 4L, 29L, 0L, 0L, 0L, 131L, 13L, 0L, 
    0L, 0L, 0L, 15L, 0L, 8L, 0L, 2L, 4L, 5L, 7L, 0L, 3L, 14L, 0L, 
    2L, 0L, 20L, 0L, 802L, 5L, 0L, 0L, 0L, 0L, 0L, 103L, 0L, 12L, 
    0L, 0L, 232L, 37L, 0L, 0L, 0L, 0L, 0L, 0L, 62L, 27L, 0L, 101L, 
    0L, 0L, 66L, 0L, 0L, 312L, 10L, 20L, 0L, 0L, 0L, 58L, 0L, 0L, 
    0L, 0L, 63L, 123L, 0L, 18L, 0L, 0L, 5L, 0L, 1L, 0L, 6924L, 1L, 
    62L, 2L, 0L, 13L, 9L), .Dim = c(20L, 5L), .Dimnames = list(c("ENSMUSG00000000037", 
    "ENSMUSG00000000093", "ENSMUSG00000000125", "ENSMUSG00000000127", 
    "ENSMUSG00000000134", "ENSMUSG00000000142", "ENSMUSG00000000148", 
    "ENSMUSG00000000244", "ENSMUSG00000000247", "ENSMUSG00000000253", 
    "ENSMUSG00000000266", "ENSMUSG00000000275", "ENSMUSG00000000282", 
    "ENSMUSG00000000290", "ENSMUSG00000000325", "ENSMUSG00000000359", 
    "ENSMUSG00000000374", "ENSMUSG00000000409", "ENSMUSG00000000439", 
    "ENSMUSG00000000532"), c("P3B1", "P3C1", "P3D1", "P3E1", "P3F1"
    )))
  
  groups2 = structure(c(P3B1 = 2L, P3C1 = 1L, P3D1 = 2L, P3E1 = 1L, P3F1 = 1L
  ), .Label = c("1", "2", "3", "4", "5", "6"), class = "factor")
  
  result = nodesDiffExp(data2, groups2)
  
  expect_equal(nrow(result), 20)
  expect_equal(length(result), 4)
})
