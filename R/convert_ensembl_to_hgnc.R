# Packages ----------------------------------------------------------------


# Paths --------------------------------------------------------------------
#hgnc_to_ensembl.path = system.file("hgnc_to_ensembl_or_refseq.tsv", package="GeneIdentifierUtilsR")
#gnc_to_ensembl.df = read.csv(hgnc_to_ensembl.path, header = TRUE, sep = "\t")
# Read Data ---------------------------------------------------------------


#Use this function for mapping hgnc to ensembl ids
#' Ensembl to HGNC Symbol
#'
#'  Converts a single ensembl id to HGNC approved symbol.
#'
#' @param ensembl_id ensembl id (string)
#'
#' @return HGNC approved symbols (string). NA if can't find a match.
#' @export
convert_single_ensembl_to_hgnc <- function(ensembl_id){
  if(is.na(ensembl_id)) return(NA)

  #assertthat::assert_that(assertthat::is.string(ensembl_id))
  assertthat::assert_that(is.character(ensembl_id))

  gene_symbol = ensembldb::select(x=EnsDb.Hsapiens.v86::EnsDb.Hsapiens.v86, keys = ensembl_id, keytype="GENEID", columns = c("SYMBOL","GENEID")) %>%
    dplyr::pull(.data$SYMBOL)

  if(length(gene_symbol) == 0)
    return(NA)
  else
    return(gene_symbol)
}

#Use this function for mapping hgnc to ensembl ids
#' Ensembl to HGNC Symbol
#'
#' Vectorized version of convert_single_ensembl_to_hgnc
#'
#' @param ensembl_id ensembl id (character)
#'
#' @return HGNC approved symbols (character vector). NA if can't find a match.
#' @export
convert_ensembl_to_hgnc <- function(ensembl_id) {
  # assertthat::assert_that(is.character(ensembl_id))
  # purrr::map_chr(ensembl_id, convert_single_ensembl_to_hgnc)
  convert_single_ensembl_to_hgnc(ensembl_id)
}



