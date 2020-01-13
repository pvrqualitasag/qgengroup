#' @title Write output files with definition of genetic group
#'
#' @description
#' Write output files with definition of genetic group
#'
#' @param psInputFile List of genetic group
#' @param psOutputFile genetic group csv-file
#' @export output_GG
output_GG <- function(psInputFile,
                      psOutputFile,
                      pbLog = FALSE){

  outourder <- order(psInputFile$Code)
  gg_def_df <- psInputFile[outourder, ]

  write.table(gg_def_df, file = psOutputFile, sep = ";", row.names = F, quote = F)

}
