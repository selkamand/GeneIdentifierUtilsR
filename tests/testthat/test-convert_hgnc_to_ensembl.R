test_that("hgnc_to_ensembl works", {
  expect_equal(convert_hgnc_to_ensembl("AATK"), "ENSG00000181409", ignore_attr = TRUE)
  expect_equal(convert_hgnc_to_ensembl("AATYK"), "ENSG00000181409", ignore_attr = TRUE)
  expect_equal(convert_hgnc_to_ensembl("ICR2B"), "ENSG00000144452", ignore_attr = TRUE)
  expect_equal(convert_hgnc_to_ensembl("LI2"), "ENSG00000144452", ignore_attr = TRUE)
  expect_equal(convert_hgnc_to_ensembl("DKFZP434G232"), "ENSG00000144452", ignore_attr = TRUE)
  expect_true(is.na(convert_hgnc_to_ensembl("ASJDLAKWJDLWA")))
  expect_true(is.na(convert_hgnc_to_ensembl(NA)))
  expect_true(is.na(convert_hgnc_to_ensembl(4)))

  expect_equal(
    convert_hgnc_to_ensembl(c("AATK", "ICR2B", "LI2")),
    c("ENSG00000181409", "ENSG00000144452", "ENSG00000144452"),
    ignore_attr = TRUE
  )

  expect_equal(
    convert_hgnc_to_ensembl(c("AATK", "ICR2B", "ASDWADASDWAD", "LI2")),
    c("ENSG00000181409", "ENSG00000144452", NA, "ENSG00000144452"),
    ignore_attr = TRUE
  )
})
