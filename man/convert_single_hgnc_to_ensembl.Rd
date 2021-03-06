% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_hgnc_to_ensembl.R
\name{convert_single_hgnc_to_ensembl}
\alias{convert_single_hgnc_to_ensembl}
\title{Mapping HGNC to ensembl ids}
\usage{
convert_single_hgnc_to_ensembl(hgnc_symbol)
}
\arguments{
\item{hgnc_symbol}{A HGNC symbol. Can be the currently approved symbol, an alias, or a previous symbol (string)}
}
\value{
ensembl id (string)
}
\description{
Maps a HGNC gene symbol to its ensembl_id
}
\examples{
convert_single_hgnc_to_ensembl("AATK")
}
