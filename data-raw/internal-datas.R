library(usethis)
library(data.table)
library(readxl)


# drug_bkdn1 --------------------------------------------------------------

drug_bkdn1 <- as.data.table(read_xlsx(
  "inst/extdata/Tableaux_de_references_criteres_selection_CS_2018-10-08.xlsx")
)
drug_bkdn1 <- drug_bkdn1[
  , .(Combn_drug_code = as.integer(COD_DENOM),
      Combn_act_code = as.integer(COD_DENOM_SEP))
]
setkey(drug_bkdn1, Combn_drug_code)


# cst_deliv_duration1 -----------------------------------------------------

cst_deliv_duration1 <- data.table(
  Cst_drug_code = as.integer(c(
    10530, 33634, 34180, 43124, 46288, 47914, 47916,
    13, 10218, 47653,
    44164, 44489, 45531, 47206, 47424, 47536, 47586, 47615, 47749
  )),
  Cst_duration = as.integer(c(
    rep(30, 7),
    rep(60, 3),
    rep(90, 9)
  ))
)
setkey(cst_deliv_duration1, Cst_drug_code)



# tests ---------------------------------------------------------------------------------------

test_ind_simult <- readRDS("tests/test-data/test_ind_simult.rds")
test_ind_stdcontinuous <- readRDS("tests/test-data/test_ind_stdcontinuous.rds")
test_ind_stdcumul_per1 <- readRDS("tests/test-data/test_ind_stdcumul_per1.rds")
test_ind_stdcumul_per3 <- readRDS("tests/test-data/test_ind_stdcumul_per3.rds")
test_ind_ucontinuous <- readRDS("tests/test-data/test_ind_ucontinuous.rds")
test_ind_wcumul <- readRDS("tests/test-data/test_ind_wcumul.rds")

use_data(cst_deliv_duration1,
         drug_bkdn1,
         test_ind_simult,
         test_ind_stdcontinuous,
         test_ind_stdcumul_per1,
         test_ind_stdcumul_per3,
         test_ind_ucontinuous,
         test_ind_wcumul,

         internal = TRUE,
         overwrite = TRUE)
