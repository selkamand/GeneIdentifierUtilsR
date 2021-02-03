# Packages ----------------------------------------------------------------


# Paths --------------------------------------------------------------------
#hgnc_to_ensembl.path = system.file("hgnc_to_ensembl_or_refseq.tsv", package="GeneIdentifierUtilsR")
#gnc_to_ensembl.df = read.csv(hgnc_to_ensembl.path, header = TRUE, sep = "\t")
# Read Data ---------------------------------------------------------------


#Use this function for mapping hgnc to ensembl ids
#' Ensembl to HGNC Symbol
#'
#'  Converts ensembl ids to HGNC approved symbol.
#'
#' @param ensembl_id ensembl id (character)
#'
#' @return HGNC approved symbols (character). NA if can't find a match.
#' @export
convert_ensembl_to_hgnc <- function(ensembl_id){
  GENEID=NULL; SYMBOL=NULL

  assertthat::assert_that(is.character(ensembl_id))
  ensembl_id <- ensembl_id %>%tidyr::replace_na("")

  unique_ensembl_id.v = unique(ensembl_id)

  findings_df <- ensembldb::select(x=EnsDb.Hsapiens.v86::EnsDb.Hsapiens.v86, keys = unique_ensembl_id.v, keytype="GENEID", columns = c("SYMBOL","GENEID")) %>% data.table::as.data.table()

  hgnc_symbols.v <- purrr::map_chr(unique_ensembl_id.v, function(id) {
    #browser()
    gene_symbol = findings_df[GENEID==id,SYMBOL] %>% dplyr::first();
    if(length(gene_symbol) == 0 || id == "")
      return(NA)
    else
      return(gene_symbol)
  } )

  #names(unique_ensembl_id.v) <- hgnc_symbols.v
  hgnc_symbols.v[match(ensembl_id, unique_ensembl_id.v)] %>%
    return()
}


