#' @title Define genetic group for breeds
#'
#' @description
#' Identifying breeds that have to few versus enough missing sires.
#' This criteria is regulated with the option pminGroupSize.
#'
#' @param psInputFile data frame including number of missing parents per year and breed
#' @param psBreedsFile breed grouping file
#' @param psCountryFile country grouping file
#' @param pminGroupSize minimum number of animals required per genetic group
#' @param pnumberOfYears minimum number of birth years required per genetic group
#' @param ppdiffYear difference in birth years between parents of bulls and parents of dams
#' @param pglobal_minyear global minimum year
#' @param pglobal_maxyear globar maximum year
#' @export define_GG
define_GG <- function(psInputFile,
                      psBreedsFile = NULL,
                      psCountryFile = NULL,
                      pminGroupSize = 250,
                      pnumberOfYears = 6,
                      pdiffYear = 3,
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


  ### # Read a breed grouping file if specified
  if (!is.null(psBreedsFile)) {
    d.breeds <- read.table(file=psBreedsFile, sep=";", header = T)
    d.breeds$Gruppe <- as.factor(paste("bg_",d.breeds$Gruppe,sep="")) # Make the groups to factor, prefix is added for readability

    # Check if every breed is only once in breedsFile
    if(length(unique(d.breeds$Rasse)) != nrow(d.breeds)){
      n_occur <- data.frame(table(d.breeds$Rasse))
      n_occur[n_occur$Freq > 1,1]
      cat(as.character(n_occur[n_occur$Freq > 1,1]))
      message("")
      stop("Breeds above appear more than once in ", psBreedsFile, " please delete the duplicates!")
    }

    d.gg <- psInputFile
    breeds2add <- levels(d.gg$Rasse)[! levels(d.gg$Rasse) %in% levels(d.breeds$Rasse)]
    df_breeds2add <- data.frame(Rasse=breeds2add, Gruppe=breeds2add)
    d.breeds <- rbind(d.breeds, df_breeds2add)
    d.gg$Rasse <- factor(d.gg$Rasse, levels=d.breeds$Rasse, labels=d.breeds$Gruppe)
  }else{
    d.gg <- psInputFile
  }



  ### # Read a country grouping file if specified
  if (!is.null(psCountryFile)) {
      d.countries <- read.table(file=psCountryFile, sep=";", header = T)
      d.countries$Gruppe <- as.factor(paste("cg_",d.countries$Gruppe,sep="")) # Make the groups to factor, prefix is added for readability

      # Check if every country is only mentioned once in file
      if(length(unique(d.countries$Land)) != nrow(d.countries)){
        n_occur <- data.frame(table(d.countries$Land))
        n_occur[n_occur$Freq > 1,1]
        cat(as.character(n_occur[n_occur$Freq > 1,1]))
        message("")
        stop("Countries above appear more than once in ", psCountryFile, " please delete the duplicates!")
      }

      ctry2add <- levels(d.gg$Land)[! levels(d.gg$Land) %in% levels(d.countries$Land)]
      df_ctry2add <- data.frame(Land=ctry2add, Gruppe=ctry2add)
      d.countries <- rbind(d.countries, df_ctry2add)
      d.gg$Land <- factor(d.gg$Land, levels=d.countries$Land, labels=d.countries$Gruppe)
  }else{
    d.gg <- psInputFile
  }

  ### # Remove unused factor levels
  d.gg[] <- lapply(d.gg, function(x) if(is.factor(x)) factor(x) else x)

  ### # Reduce to sires
  ### # The deduction of the genetic groups will be done only on missing sire information
  t.gg.sires <- d.gg[,c(1:5)]

  ### # Identify breeds that have to few missing sires for an own genetic group
  t.counts_per_brd <- aggregate(cbind(SP_SB, SP_SC) ~ Rasse, data = t.gg.sires, FUN=sum)
  n_missing_per_brd <- data.frame(breed=t.counts_per_brd$Rasse, nmiss=t.counts_per_brd$SP_SB + t.counts_per_brd$SP_SC)
  brd_to_small <- n_missing_per_brd[n_missing_per_brd$nmiss < pminGroupSize, "breed"]
  brd_big_enough <- n_missing_per_brd[! n_missing_per_brd$breed %in% brd_to_small, "breed"]

  ### # Define dummy genetic group for breeds that have to few missing sires
  n_dummy_gg_def <- 0
  dummy_gg_def_list = list()
  for(breed_l in brd_to_small) {
    counts_of_this_brd <- t.gg.sires[t.gg.sires$Rasse == breed_l,]
    for(country_l in unique(counts_of_this_brd$Land)) {
      n_dummy_gg_def <- n_dummy_gg_def + 1
      gg_def <- data.frame(Rasse = breed_l, Land = country_l, GebJahrUnten = pglobal_minyear, GebJahrOben = pglobal_maxyear)
      dummy_gg_def_list[[n_dummy_gg_def]] <- gg_def
    }
  }


  ### # Identify genetic groups for breeds with enough missing sires
  n_gg_def <- 0
  gg_code <- 0
  gg_def_list = list()

  for(breed_l in brd_big_enough) {
    counts_of_this_brd <- t.gg.sires[t.gg.sires$Rasse == breed_l,]

    # Count number of missing sires per country within breed
    # If the number is to low for a coutnry a groupting will be done
    t.counts_per_ctry <- aggregate(cbind(SP_SB, SP_SC) ~ Land, data = counts_of_this_brd, FUN=sum)
    n_missing_per_ctry <- data.frame(country=t.counts_per_ctry$Land, nmiss=t.counts_per_ctry$SP_SB + t.counts_per_ctry$SP_SC)
    ctry_to_small <- n_missing_per_ctry[n_missing_per_ctry$nmiss < pminGroupSize, "country"]
    ctry_big_enough <- n_missing_per_ctry[! n_missing_per_ctry$country %in% ctry_to_small, "country"]

    nmissing <- sum(n_missing_per_ctry[n_missing_per_ctry$country %in% ctry_to_small, "nmiss"])

    if (nmissing < 0) {
      stop("nmissing below 0")
    } else if (nmissing == 0) { # All countries (or countr groups) have enough missing sires
      ctry_in_icg <- NULL
    } else if (nmissing < pminGroupSize) {
      # Die zu kleinen Länder werden mit dem kleinsten genügend grossen Land zusammengefasst
      # Es ist meine Erwartung, dass bei mehreren genügend grossen Länder CHE nicht das kleinste
      # ist und CHE somit spearat bleibt --> war aber bei z.B SI nicht so

      n_missing_per_ctry_big_enough <- n_missing_per_ctry[n_missing_per_ctry$country %in% ctry_big_enough,]
      smallest_big_enough_ctry <- n_missing_per_ctry_big_enough$country[which.min(n_missing_per_ctry_big_enough$nmiss)]
      ctry_to_group <- unlist(list(ctry_to_small, smallest_big_enough_ctry))

      levels(counts_of_this_brd$Land) <- c(levels(counts_of_this_brd$Land), "icg")  # "icg" steht für internal country group
      counts_of_this_brd[counts_of_this_brd$Land %in% ctry_to_group, "Land"] <- "icg"
      ctry_in_icg <- ctry_to_group
    } else {
      # Alle zu kleinen Länder zusammenfassen
      levels(counts_of_this_brd$Land) <- c(levels(counts_of_this_brd$Land), "icg")  # "icg" steht für internal country group
      counts_of_this_brd[counts_of_this_brd$Land %in% ctry_to_small, "Land"] <- "icg"
      ctry_in_icg <- ctry_to_small
    }

    counts_of_this_brd[] <- lapply(counts_of_this_brd, function(x) if(is.factor(x)) factor(x) else x) # Remove unused factor levels

    n_gg_def_within_breed <- 0
    gg_def_within_breed_list = list()

    for (country_l in levels(counts_of_this_brd$Land)) {
      counts_of_this_brd_and_ctry <- counts_of_this_brd[counts_of_this_brd$Land == country_l,]

      year <- pglobal_minyear
      minyear <- pglobal_minyear
      maxyear <- pglobal_minyear
      while (year < pglobal_maxyear) {
        nr_years <- 0
        nmissing <- 0
        while ((nmissing < pminGroupSize | nr_years < pnumberOfYears) & year < pglobal_maxyear){
          tmpyear <- counts_of_this_brd_and_ctry[counts_of_this_brd_and_ctry$GebJahr==year,]
          if (nrow(tmpyear) > 0) {
            nmissing <- nmissing + sum(tmpyear[,c(4:5)])
          }
          nr_years <- nr_years + 1
          year <- year + 1
          if (year > maxyear) maxyear <- year  # Save latest birth year of genetic group
        }
        n_gg_def_within_breed <- n_gg_def_within_breed + 1
        gg_def <- data.frame(Rasse = breed_l, Land = country_l, GebJahrUnten = minyear, GebJahrOben = maxyear)
        if (nmissing < pminGroupSize & minyear == pglobal_minyear & maxyear == pglobal_maxyear) { # Es liegen total zu wenige missing sires for für die zu kleinen Länder
          gg_def_within_breed_list[[n_gg_def_within_breed]] <- gg_def
        } else if (nmissing < pminGroupSize) {
          # Last one: If not enough parents in last genetic group, assign those animals to previous genetic group
          # This is achieved by setting maxyear to pglobal_maxyear
          n_gg_def_within_breed <- n_gg_def_within_breed-1
          gg_def_within_breed_list[[n_gg_def_within_breed]]$GebJahrOben <- pglobal_maxyear
        } else {
          gg_def_within_breed_list[[n_gg_def_within_breed]] <- gg_def
        }
        #      print(gg_def_within_breed_list)
        minyear <- maxyear + 1       # Set new minimum year for new round
      }
    } # Ende country loop

    # Resolve internal country grouping
    if (is.null(ctry_in_icg)) {
      for (gg_def in gg_def_within_breed_list) {
        gg_code <- gg_code + 1
        gg_def$Code <- gg_code
        n_gg_def <- n_gg_def + 1
        gg_def_list[[n_gg_def]] <- gg_def
      }
    } else {
      for (gg_def in gg_def_within_breed_list) {
        gg_code <- gg_code + 1
        gg_def$Code <- gg_code # Wenn das hier gemacht wird, bekommen alle Länder in icg in die gleiche Gruppe.
        if (gg_def$Land == "icg") {
          for (country_l in ctry_in_icg) {
            this_gg_def <- gg_def
            this_gg_def$Land <- country_l
            n_gg_def <- n_gg_def + 1
            gg_def_list[[n_gg_def]] <- this_gg_def
          }
        } else {
          n_gg_def <- n_gg_def + 1
          gg_def_list[[n_gg_def]] <- gg_def
        }
      }
    }
  } # Ende breed loop


  # Add gg definitions of small groups/breeds
  gg_code <- gg_code + 1 # Zu kleine Rassen kommen alle in die gleiche Gruppe
  for (gg_def in dummy_gg_def_list) {
    gg_def$Code <- gg_code
    n_gg_def <- n_gg_def + 1
    gg_def_list[[n_gg_def]] <- gg_def
  }

  ### # Subtract pdiffYear for genetic groups of parents of bulls
  L_gg_def_list_diffYear <- substract_diffYear(psInputFile = gg_def_list,
                                               pdiffYear = pdiffYear,
                                               pglobal_minyear = pglobal_minyear,
                                               pglobal_maxyear = pglobal_maxyear)

  ### # Resolve breed grouping
  if (!is.null(psBreedsFile)) {
    brd_grp_lst <- split(d.breeds$Rasse, d.breeds$Gruppe)

    gg_def_brdgrp_list <- L_gg_def_list_diffYear

    n_gg_def <- 0
    gg_def_list = list()

    for (gg_def in gg_def_brdgrp_list) {
      for (brd in brd_grp_lst[[as.character(gg_def$Rasse)]]) {
        this_gg_def <- gg_def
        this_gg_def$Rasse <- brd
        n_gg_def <- n_gg_def + 1
        gg_def_list[[n_gg_def]] <- this_gg_def
      }
    }
  }else{
    gg_def_list <- L_gg_def_list_diffYear
  }

  ### # Resolve country grouping
  if (!is.null(psCountryFile)) {
      ctry_grp_lst <- split(d.countries$Land, d.countries$Gruppe)

      gg_def_ctrygrp_list <- gg_def_list

      n_gg_def <- 0
      gg_def_list = list()

      for (gg_def in gg_def_ctrygrp_list) {
        for (ctry in ctry_grp_lst[[as.character(gg_def$Land)]]) {
          this_gg_def <- gg_def
          this_gg_def$Land <- ctry
          n_gg_def <- n_gg_def + 1
          gg_def_list[[n_gg_def]] <- this_gg_def
        }
      }
  }else{
    gg_def_list <- L_gg_def_list_diffYear
  }

  ### # Result
  return(gg_def_list)


}
