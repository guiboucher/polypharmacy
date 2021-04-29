library(testthat)
library(polypharmacy)
library(data.table)

test_that("ind_stdcumul", {
  rx1 <- data.frame(id = c(1, 1, 1, 2),
                    code = c("A", "B", "C", "A"),
                    date = c("2000-01-01", "2000-01-01", "2000-01-26", "2000-01-17"),
                    duration = c(30, 5, 5, 10))
  cohort1 <- data.frame(id = as.numeric(1:3),
                        age = c(45, 12, 89),
                        sex = c("F", "F", "M"))
  rx_proc1 <- data_process(Rx_deliv = rx1, Rx_id = "id", Rx_drug_code = "code",
                           Rx_drug_deliv = "date", Rx_deliv_dur = "duration",
                           Cohort = cohort1, Cohort_id = "id",
                           study_start = "2000-01-01", study_end = "2000-01-30",
                           cores = 1)
  expect_equal(ind_stdcumul(processed_tab = rx_proc1, nPeriod = 1),
               polypharmacy:::test_ind_stdcumul_per1,
               ignore_attr = TRUE)
  expect_equal(ind_stdcumul(processed_tab = rx_proc1, nPeriod = 3),
               polypharmacy:::test_ind_stdcumul_per3,
               ignore_attr = TRUE)
})
