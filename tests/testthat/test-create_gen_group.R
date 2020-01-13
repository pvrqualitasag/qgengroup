context("Generate Genetic Groups")

test_that("create genetic groups", {
  s_input_path <- system.file("extdata", "statGenGroupOutFile", package = "qgengroup")
  s_expected_output_path <- system.file("extdata", "genGrDef_functionMain_Default_withCountry.csv", package = "qgengroup")
  s_output_fname <- "genGrDef_functionMain_Default_withCountry.csv"
  n_min_group_size <- 1500
  n_number_of_years <- 5
  create_GG(psInputFile = s_input_path,
            pminGroupSize = n_min_group_size,
            pnumberOfYears = n_number_of_years,
            psOutputFile = s_output_fname)

  tbl_gen_gr <- readr::read_csv2(file = s_output_fname)
  tbl_exp_out <- readr::read_csv2(file = s_expected_output_path)
  expect_equal(tbl_gen_gr, tbl_exp_out)
  unlink(s_output_fname)
})

test_that("create genetic groups without countries",{
  s_input_path <- system.file("extdata", "statGenGroupOutFile", package = "qgengroup")
  s_expected_output_path_woc <- system.file("extdata", "genGrDef_functionMain_Default_withoutCountry.csv", package = "qgengroup")
  s_output_fname_woc <- "genGrDef_functionMain_Default_withoutCountry.csv"
  n_min_group_size <- 1500
  n_number_of_years <- 5
  create_GG_withoutCountry(psInputFile = s_input_path,
            pminGroupSize = n_min_group_size,
            pnumberOfYears = n_number_of_years,
            psOutputFile = s_output_fname_woc)

  tbl_gen_gr_woc <- readr::read_csv2(file = s_output_fname_woc)
  tbl_exp_out_woc <- readr::read_csv2(file = s_expected_output_path_woc)
  expect_equal(tbl_gen_gr_woc, tbl_exp_out_woc)
  unlink(s_output_fname_woc)
})
