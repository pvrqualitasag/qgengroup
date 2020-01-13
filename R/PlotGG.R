#' @title Plot statistics on missing parents
#'
#' @description
#' Plot a table of statistics on missing parents
#'
#' @param psInputFile file including number of missing parents per year and breed
#' @param pBreed breed to plot (breed code of Qualitas)
#' @export plot_statGenGroupOutFile
plot_statGenGroupOutFile <- function(psInputFile,
                                     pBreed,
                                     pbLog = FALSE){

  ### # Load required package
  suppressPackageStartupMessages(if(! require("magrittr")) {
    install.packages("magrittr", repos="https://stat.ethz.ch/CRAN/")
    require("magrittr")
  })
  suppressPackageStartupMessages(if(! require("dplyr")) {
    install.packages("dplyr", repos="https://stat.ethz.ch/CRAN/")
    require("dplyr")
  })
  suppressPackageStartupMessages(if(! require("ggplot2")) {
    install.packages("ggplot2", repos="https://stat.ethz.ch/CRAN/")
    require("ggplot2")
  })
  suppressPackageStartupMessages(if(! require("reshape2")) {
    install.packages("reshape2", repos="https://stat.ethz.ch/CRAN/")
    require("reshape2")
  })

  #Read file
  tbl_file <- readr::read_delim(file = psInputFile, delim = ";", skip = 2)

  #Group by 2 variables: breed and birthyear
  tbl_rssXjhr <- tbl_file %>% filter(Rasse == pBreed) %>%
                                 group_by(GebJahr) %>%
                                 summarise(sum_SB = sum(SP_SB, na.rm = TRUE),
                                           sum_SC = sum(SP_SC, na.rm = TRUE),
                                           sum_DB = sum(SP_DB, na.rm = TRUE),
                                           sum_DC = sum(SP_DC, na.rm = TRUE))

  #Transform data to make the plotting easier
  tbl_2plot <- melt(tbl_rssXjhr, id.vars = "GebJahr")

  #Plot
  gg <- ggplot(tbl_2plot, aes(GebJahr, value, col = variable)) +
    facet_grid(rows = vars(variable)) +
    geom_point() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = "Jahren") +
    ggtitle("Analyse genetische Gruppen ",
            subtitle = paste("Rasse: ", pBreed, sep = ""))

  print(gg)

}
