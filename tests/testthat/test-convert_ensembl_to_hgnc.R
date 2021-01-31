test_that("ensembl ID to hgnc conversion", {

  expect_equal(convert_ensembl_to_hgnc("ENSG00000268895"), "A1BG-AS1", ignore_attr=TRUE)
  expect_equal(convert_ensembl_to_hgnc("ENSG00000188984"), "AADACL3", ignore_attr=TRUE)
  expect_true(is.na(convert_ensembl_to_hgnc("")))
  expect_true(is.na(convert_ensembl_to_hgnc("ASDSAKD:LW")))
  expect_error(convert_ensembl_to_hgnc(4), "character")

})
