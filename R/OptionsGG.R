#' @title Subtract pdiffYear for genetic groups of parents of bulls
#'
#' @description
#' Distinction between parents of bulls and parents of cows.
#' Because the parent of sires reach the same genetic level earlier than parents of cows.
#'
#' @param psInputFile List of genetic group
#' @param ppdiffYear difference in birth years between parents of bulls and parents of dams
#' @param pglobal_minyear global minimum year
#' @param pglobal_maxyear globar maximum year
#' @export substract_pdiffYear
substract_diffYear <- function(psInputFile,
                               pdiffYear = 3,
                               pglobal_minyear = 1900,
                               pglobal_maxyear = 2100,
                               pbLog = FALSE){

  gg_def_onlySC_list <- psInputFile

  n_gg_def <- 0
  gg_def_list = list()

  for (gg_def in gg_def_onlySC_list) {
    # Zuerst SC
    this_gg_def <- gg_def
    this_gg_def$Selektionspfad <- "SC"
    n_gg_def <- n_gg_def + 1
    gg_def_list[[n_gg_def]] <- this_gg_def
    # Dann SB
    this_gg_def <- gg_def
    this_gg_def$Selektionspfad <- "SB"

    if (this_gg_def$GebJahrUnten > pglobal_minyear) {
      this_gg_def$GebJahrUnten <- this_gg_def$GebJahrUnten - pdiffYear
    }
    if (this_gg_def$GebJahrOben < pglobal_maxyear) {
      this_gg_def$GebJahrOben <- this_gg_def$GebJahrOben - pdiffYear
    }

    n_gg_def <- n_gg_def + 1
    gg_def_list[[n_gg_def]] <- this_gg_def
  }

  ### # Result
  return(gg_def_list)


}



#' @title Create genetic groups for dams
#'
#' @description
#' Create genetic groups for dams
#'
#' @param psInputFile List of genetic group
#' @param pshiftYear shift between genetic groups of sire and genetic groups of dams; this is required so that they do not contain exactly the same animals
#' @param pglobal_minyear global minimum year
#' @param pglobal_maxyear globar maximum year
#' @export define_GG_dam
define_GG_dam <- function(psInputFile,
                          pshiftYear = 2,
                          pglobal_minyear = 1900,
                          pglobal_maxyear = 2100,
                          pbLog = FALSE){

  gg_def_df <- do.call("rbind", psInputFile)

  gg_def_df$Code <- gg_def_df$Code*2
  gg_def_df_dams <- gg_def_df

  gg_def_df_dams[gg_def_df_dams$Selektionspfad == "SC",]$Selektionspfad <- "DC"
  gg_def_df_dams[gg_def_df_dams$Selektionspfad == "SB",]$Selektionspfad <- "DB"
  gg_def_df_dams$Code <- gg_def_df_dams$Code - 1

  gg_def_df_dams[gg_def_df_dams$GebJahrOben < pglobal_maxyear,]$GebJahrOben <- gg_def_df_dams[gg_def_df_dams$GebJahrOben < pglobal_maxyear,]$GebJahrOben - pshiftYear
  gg_def_df_dams[gg_def_df_dams$GebJahrUnten > pglobal_minyear,]$GebJahrUnten <- gg_def_df_dams[gg_def_df_dams$GebJahrUnten > pglobal_minyear,]$GebJahrUnten - pshiftYear

  gg_def_df <- rbind(gg_def_df, gg_def_df_dams)

  ### # Result
  return(gg_def_df)


}
