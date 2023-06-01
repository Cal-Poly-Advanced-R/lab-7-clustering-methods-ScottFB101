test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("plot_clustering works", {
    empty_clustering <- data.frame(matrix(ncol = 3, nrow = 0))
    colnames(empty_clustering) <- c("Comp.1", "Comp.2", "clusters")
    expect_error(plot_clusterings(empty_clustering))
})
