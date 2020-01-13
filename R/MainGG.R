#' @title Create genetic groups without looking to countries
#'
#' @description
#' Overall function to create genetic groups without looking to countries
#'
#' @param psInputFile file including number of missing parents per year and breed
#' @param pminGroupSize minimum number of animals required per genetic group
#' @param pnumberOfYears minimum number of birth years required per genetic group
#' @param pglobal_minyear global minimum year
#' @param pglobal_maxyear globar maximum year
#' @param pdiffYear difference in birth years between parents of bulls and parents of dams
#' @param pshiftYear shift between genetic groups of sire and genetic groups of dams; this is required so that they do not contain exactly the same animals
#' @param psOutputFile genetic group csv-file
#' @export create_GG_withoutCountry
create_GG_withoutCountry <- function(psInputFile,
                                     pminGroupSize = 250,
                                     pnumberOfYears = 6,
                                     pglobal_minyear = 1900,
                                     pglobal_maxyear = 2100,
                                     pdiffYear = 3,
                                     pshiftYear = 2,
                                     psOutputFile,
                                     pbLog = FALSE){

  ### # check parameters
  if(is.null(psInputFile)) {
    stop("--missingParentsFile has not been specified")
  }

  ### # Reading statistics on missing parents
  df_d.gg <- read_statGenGroupOutFile(psInputFile)

  ### # Define genetic group for breeds
  L_gg_def_list <- define_GG_withoutCountry(psInputFile = df_d.gg,
                                            pminGroupSize = pminGroupSize,
                                            pnumberOfYears = pnumberOfYears,
                                            pglobal_minyear = pglobal_minyear,
                                            pglobal_maxyear = pglobal_maxyear)

  ### # Subtract pdiffYear for genetic groups of parents of bulls
  L_gg_def_list_diffYear <- substract_diffYear(psInputFile = L_gg_def_list,
                                               pdiffYear = pdiffYear,
                                               pglobal_minyear = pglobal_minyear,
                                               pglobal_maxyear = pglobal_maxyear)

  ### # Create genetic groups for dams
  L_gg_def_df_dam <- define_GG_dam(psInputFile = L_gg_def_list_diffYear,
                                   pshiftYear = pshiftYear,
                                   pglobal_minyear = pglobal_minyear,
                                   pglobal_maxyear = pglobal_maxyear)

  ### # Write output files with definition of genetic group
  output_GG(psInputFile = L_gg_def_df_dam,
            psOutputFile = psOutputFile)

}



#' @title Create genetic groups regarding countries
#'
#' @description
#' Overall function to create genetic groups regarding countries
#'
#' @param psInputFile file including number of missing parents per year and breed
#' @param psBreedsFile Read a breed grouping file
#' @param psCountryFile country grouping file
#' @param pminGroupSize minimum number of animals required per genetic group
#' @param pnumberOfYears minimum number of birth years required per genetic group
#' @param pglobal_minyear global minimum year
#' @param pglobal_maxyear globar maximum year
#' @param pdiffYear difference in birth years between parents of bulls and parents of dams
#' @param pshiftYear shift between genetic groups of sire and genetic groups of dams; this is required so that they do not contain exactly the same animals
#' @param psOutputFile genetic group csv-file
#' @export create_GG
create_GG <- function(psInputFile,
                      psBreedsFile = NULL,
                      psCountryFile = NULL,
                      pminGroupSize = 250,
                      pnumberOfYears = 6,
                      pglobal_minyear = 1900,
                      pglobal_maxyear = 2100,
                      pdiffYear = 3,
                      pshiftYear = 2,
                      psOutputFile,
                      pbLog = FALSE){

  ### # check parameters
  if(is.null(psInputFile)) {
    stop("--missingParentsFile has not been specified")
  }

  ### # Reading statistics on missing parents
  df_d.gg <- read_statGenGroupOutFile(psInputFile)

  ### # Define genetic group for breeds
  L_gg_def_list <- define_GG(psInputFile = df_d.gg,
                             psBreedsFile = psBreedsFile,
                             psCountryFile = psCountryFile,
                             pminGroupSize = pminGroupSize,
                             pnumberOfYears = pnumberOfYears,
                             pdiffYear = pdiffYear,
                             pglobal_minyear = pglobal_minyear,
                             pglobal_maxyear = pglobal_maxyear)

  ### # Create genetic groups for dams
  L_gg_def_df_dam <- define_GG_dam(psInputFile = L_gg_def_list,
                                   pshiftYear = pshiftYear,
                                   pglobal_minyear = pglobal_minyear,
                                   pglobal_maxyear = pglobal_maxyear)

  ### # Write output files with definition of genetic group
  output_GG(psInputFile = L_gg_def_df_dam,
            psOutputFile = psOutputFile)

}
