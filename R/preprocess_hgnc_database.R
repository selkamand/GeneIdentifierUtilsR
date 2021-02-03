
#' preprocess_hgnc_database
#'
#' Takes the HGNC database from:https://biomart.genenames.org/ and adds turns it into a redundant longform dataset with a HGNC.SYMBOL column that represents each and every HGNC symbol (approved, alternative, and previous)
#'
#' @return NULL. Run for its side effects
#' @importFrom rlang .data
preprocess_hgnc_database <- function(){
  hgnc_database.path = system.file("inst/hgnc_to_ensembl_or_refseq.tsv",package = "GeneIdentifierUtilsR")

  hgnc_database.df <- utils::read.csv(hgnc_database.path, header = TRUE, sep = "\t") %>% dplyr::tibble()
  # hgnc_database.df

  #Make a new column: HGNC.SYMBOL, which contains all HGNC symbols (current/previous/alias) all comma separated.
  hgnc_database_all_symbols_in_one_colum.df <- hgnc_database.df %>% dplyr::mutate(
    HGNC.SYMBOL = paste(Approved.symbol, Previous.symbols,  Alias.symbols, sep = ",")
    ) %>%
    dplyr::mutate(
      HGNC.SYMBOL.ALL=gsub(pattern = " ", replacement = "", x = .dawrta$HGNC.SYMBOL),
      HGNC.SYMBOL=gsub(pattern = ",,", replacement = ",", x = .data$HGNC.SYMBOL.ALL),
      HGNC.SYMBOL=gsub(pattern = ",$", replacement = "", x = .data$HGNC.SYMBOL.ALL)
    ) %>%
    dplyr::select(.data$HGNC.SYMBOL, tidyr::everything())

  #Separate the new comma delimited column into one row per HGNC.SYMBOL
  # hgnc_database_all_symbols_in_one_colum.df

  hgnc_database_long.df <- hgnc_database_all_symbols_in_one_colum.df %>%
    tidyr::separate_rows(.data$HGNC.SYMBOL, sep = ",") %>%
    dplyr::distinct(.data$HGNC.SYMBOL, .keep_all = TRUE) %>%
    dplyr::filter(nzchar(.data$HGNC.SYMBOL))

  #browser()
  # hgnc_database_long.df %>%
  #   dplyr::filter(Ensembl.gene.ID=="ENSG00000181409") %>% View()
  outfile=paste0(system.file(package = "GeneIdentifierUtilsR"), "/hgnc_to_ensembl_or_refseq.longform.tsv")

  utils::write.table(
    x = hgnc_database_long.df,
    file = outfile,
    sep = "\t",
    row.names = FALSE,
    col.names = TRUE
  )

}
