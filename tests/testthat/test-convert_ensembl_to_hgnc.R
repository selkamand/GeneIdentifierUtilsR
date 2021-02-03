test_that("ensembl ID to hgnc conversion", {

  expect_equal(convert_ensembl_to_hgnc("ENSG00000268895"), "A1BG-AS1", ignore_attr=TRUE)
  expect_equal(convert_ensembl_to_hgnc("ENSG00000188984"), "AADACL3", ignore_attr=TRUE)
  expect_true(is.na(convert_ensembl_to_hgnc("")))
  expect_true(is.na(convert_ensembl_to_hgnc("ASDSAKD:LW")))
  expect_error(convert_ensembl_to_hgnc(4), "character")

  #Multiple
  expect_equal(convert_ensembl_to_hgnc(c("ENSG00000188984", "ENSG00000118017")), c("AADACL3", "A4GNT"), ignore_attr=TRUE)
  expect_equal(convert_ensembl_to_hgnc(c("ENSG00000188984", "ENSG00000118017", "ENSG00000115657")), c("AADACL3", "A4GNT", "ABCB6"), ignore_attr=TRUE)

  #Multiple WITH NAs and "" and Incorrect IDs
  expect_equal(convert_ensembl_to_hgnc(c("ENSG00000188984", NA, "ENSG00000115657")), c("AADACL3", NA, "ABCB6"), ignore_attr=TRUE)
  expect_equal(convert_ensembl_to_hgnc(c("ENSG00000188984", "", "ENSG00000115657")), c("AADACL3", NA, "ABCB6"), ignore_attr=TRUE)
  expect_equal(convert_ensembl_to_hgnc(c("ENSG00000188984", "NOTAREALID", "ENSG00000115657")), c("AADACL3", NA, "ABCB6"), ignore_attr=TRUE)

  #Duplicates
  expect_equal(
    object = convert_ensembl_to_hgnc(c("ENSG00000188984", "ENSG00000188984", "ENSG00000188984", "ENSG00000115657")),
    expected = c("AADACL3", "AADACL3", "AADACL3", "ABCB6"),
    ignore_attr=TRUE
    )

  #Duplicates reshuffled
  expect_equal(
    object = convert_ensembl_to_hgnc(c("ENSG00000188984", "ENSG00000188984", "ENSG00000115657", "ENSG00000188984")),
    expected = c("AADACL3", "AADACL3", "ABCB6", "AADACL3"),
    ignore_attr=TRUE
  )

  #Duplicates reshuffled w/ N, "", and incorrect IDS
  expect_equal(
    object = convert_ensembl_to_hgnc(c("ENSG00000188984", "ENSG00000188984",NA, "ENSG00000115657" )),
    expected = c("AADACL3", "AADACL3", NA, "ABCB6"),
    ignore_attr=TRUE
  )

  expect_equal(
    object = convert_ensembl_to_hgnc(c("ENSG00000188984", "ENSG00000188984", "", "ENSG00000115657")),
    expected = c("AADACL3", "AADACL3", NA, "ABCB6"),
    ignore_attr=TRUE
  )

  expect_equal(
    object = convert_ensembl_to_hgnc(c("ENSG00000188984", "ENSG00000188984",  "", "ENSG00000115657")),
    expected = c("AADACL3", "AADACL3", NA, "ABCB6"),
    ignore_attr=TRUE
  )

  expect_equal(
    object = convert_ensembl_to_hgnc(c("ENSG00000188984", "ENSG00000188984",  "NOTAREALID", "ENSG00000115657")),
    expected = c("AADACL3", "AADACL3", NA, "ABCB6"),
    ignore_attr=TRUE
  )



})
