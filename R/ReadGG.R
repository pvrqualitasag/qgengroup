#' @title Reading statistics on missing parents
#'
#' @description
#' Read a table of statistics on missing parents
#'
#' @param psInputFile file including number of missing parents per year and breed
#' @export read_statGenGroupOutFile
read_statGenGroupOutFile <- function(psInputFile,
                                     pbLog = FALSE){

  d.gg <- read.table(file=psInputFile, sep=";", header = T, skip = 2)
  d.gg[is.na(d.gg)] <- 0
  d.gg <- d.gg[d.gg$GebJahr != "unbekannt",]
  d.gg$GebJahr <- as.numeric(as.character(d.gg$GebJahr))

  ### # Result
  return(d.gg)

}
