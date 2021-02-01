# Packages ----------------------------------------------------------------


hgnc_to_ensemble_lookup.df = read.csv(
  system.file(package = "GeneIdentifierUtilsR","hgnc_to_ensembl_or_refseq.longform.tsv"),
  header = TRUE,
  sep = "\t"
  ) %>%
  dplyr::tibble()
#hgnc_to_ensemble_lookup.df %>% View()

#' Mapping HGNC to ensembl ids
#'
#' Maps a HGNC gene symbol to its ensembl_id
#'
#' @param hgnc_symbol A HGNC symbol. Can be the currently approved symbol, an alias, or a previous symbol (string)
#'
#' @return ensembl id (string)
#' @export
#'
#' @examples
#' convert_single_hgnc_to_ensembl("AATK")
convert_single_hgnc_to_ensembl <- function(hgnc_symbol){
  assertthat::assert_that(assertthat::is.string(hgnc_symbol))

  if(!nzchar(hgnc_symbol)) return(NA)
  if(is.na(hgnc_symbol)) return(NA)

  #browser()
  ensembl_id=hgnc_to_ensemble_lookup.df %>%
    dplyr::filter(.data$HGNC.SYMBOL==hgnc_symbol) %>%
    dplyr::filter(nzchar(.data$Ensembl.gene.ID)) %>%
    dplyr::filter(!is.na(.data$Ensembl.gene.ID)) %>%
    dplyr::pull(.data$Ensembl.gene.ID) %>%
    unique()

  assertthat::assert_that(length(ensembl_id) < 2, msg = paste0("hgnc_symbol: ", hgnc_symbol, " was associated with multiple, conflicting, ensembl_ids: ", paste0(ensembl_id, collapse = ", ")))

  if(length(ensembl_id)==0) return(NA)
  if(!nzchar(ensembl_id)) return(NA)

  return(ensembl_id)
}



#' Mapping HGNC to ensembl ids
#'
#' Maps HGNC gene symbols to ensembl_ids
#'
#' @param hgnc_symbols HGNC symbols. Can be the currently approved symbol, an alias, or a previous symbols (character or type that can be converted to a character)
#'
#' @return ensembl id (string)
#' @export
#'
#' @examples
#' hgnc_symbols = c("AATK", ""RNU6-1046P", "SUR-5")
#' convert_hgnc_to_ensembl()
convert_hgnc_to_ensembl <- function(hgnc_symbols){
  hgnc_symbols = as.character(hgnc_symbols)
  assertthat::assert_that(is.character(hgnc_symbols))

  purrr::map_chr(hgnc_symbols, convert_single_hgnc_to_ensembl)
}
