#' @title Define genetic group for breeds
#'
#' @description
#' Identifying breeds that have to few versus enough missing sires.
#' This criteria is regulated with the option pminGroupSize.
#'
#' @param psInputFile data frame including number of missing parents per year and breed
#' @param pminGroupSize minimum number of animals required per genetic group
#' @param pnumberOfYears minimum number of birth years required per genetic group
#' @param pglobal_minyear global minimum year
#' @param pglobal_maxyear globar maximum year
#' @export define_GG_withoutCountry
define_GG_withoutCountry <- function(psInputFile,
                                     pminGroupSize = 250,
                                     pnumberOfYears = 6,
                                     pglobal_minyear = 1900,
                                     pglobal_maxyear = 2100,
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

  ### # Remove unused factor levels
  psInputFile[] <- lapply(psInputFile, function(x) if(is.factor(x)) factor(x) else x)

  ### # Reduce to sires
  ### # The deduction of the genetic groups will be done only on missing sire information
  t.gg.sires <- psInputFile[,c(1:5)]

  ### # Identify breeds that have to few missing sires for an own genetic group
  t.counts_per_brd <- aggregate(cbind(SP_SB, SP_SC) ~ Rasse, data = t.gg.sires, FUN=sum)
  n_missing_per_brd <- data.frame(breed=t.counts_per_brd$Rasse, nmiss=t.counts_per_brd$SP_SB + t.counts_per_brd$SP_SC)
  brd_to_small <- n_missing_per_brd[n_missing_per_brd$nmiss < pminGroupSize, "breed"]
  brd_big_enough <- n_missing_per_brd[! n_missing_per_brd$breed %in% brd_to_small, "breed"]

  ### # Define dummy genetic group for breeds that have to few missing sires
  n_dummy_gg_def <- 0
  dummy_gg_def_list = list()
  for(breed_l in brd_to_small) {
    gg_def <- data.frame(Rasse = breed_l, GebJahrUnten = pglobal_minyear, GebJahrOben = pglobal_maxyear)
    n_dummy_gg_def <- n_dummy_gg_def + 1
    dummy_gg_def_list[[n_dummy_gg_def]] <- gg_def
  }

  ### # Identify genetic groups for breeds with enough missing sires
  n_gg_def <- 0
  gg_code <- 0
  gg_def_list = list()


  for(breed_l in brd_big_enough) {

    n_gg_def_within_breed <- 0
    gg_def_within_breed_list = list()

    tbl_t.gg.sires <- t.gg.sires %>% filter(Rasse == breed_l)
    counts_of_this_brd <-  aggregate(cbind(SP_SB, SP_SC) ~ Rasse+GebJahr, data = tbl_t.gg.sires, FUN=sum)


    year <- pglobal_minyear
    minyear <- pglobal_minyear
    maxyear <- pglobal_minyear
    while (year < pglobal_maxyear) {
      nr_years <- 0
      nmissing <- 0
      while ((nmissing < pminGroupSize | nr_years < pnumberOfYears) & year < pglobal_maxyear){
        tmpyear <- counts_of_this_brd[counts_of_this_brd$GebJahr==year,]
        if (nrow(tmpyear) > 0) {
          nmissing <- nmissing + sum(tmpyear[,c(3:4)])
        }
        nr_years <- nr_years + 1
        year <- year + 1
        if (year > maxyear) maxyear <- year  # Save latest birth year of genetic group
      }

      n_gg_def_within_breed <- n_gg_def_within_breed + 1
      gg_def <- data.frame(Rasse = breed_l, GebJahrUnten = minyear, GebJahrOben = maxyear)
      gg_def_within_breed_list[[n_gg_def_within_breed]] <- gg_def
      minyear <- maxyear + 1       # Set new minimum year for new round
    }

    # Resolve internal grouping
    for (gg_def in gg_def_within_breed_list) {
      gg_code <- gg_code + 1
      gg_def$Code <- gg_code
      n_gg_def <- n_gg_def + 1
      gg_def_list[[n_gg_def]] <- gg_def
    }

  }


  ### # Add gg definitions of small groups/breeds
  gg_code <- gg_code + 1 # Zu kleine Rassen kommen alle in die gleiche Gruppe
  for (gg_def in dummy_gg_def_list) {
    gg_def$Code <- gg_code
    n_gg_def <- n_gg_def + 1
    gg_def_list[[n_gg_def]] <- gg_def
  }

  ### # Result
  return(gg_def_list)

}
