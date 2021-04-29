library(testthat)
library(polypharmacy)
library(data.table)

test_that("ind_simult", {
  rx1 <- data.frame(id = c(1, 1, 2),
                    code = c("A", "B", "A"),
                    date = c("2000-01-01", "2000-01-04", "2000-01-08"),
                    duration = c(5, 7, 5))
  cohort1 <- data.frame(id = as.numeric(1:3),
                        age = c(45, 12, 89),
                        sex = c("F", "F", "M"))
  rx_proc1 <- data_process(Rx_deliv = rx1, Rx_id = "id", Rx_drug_code = "code",
                           Rx_drug_deliv = "date", Rx_deliv_dur = "duration",
                           Cohort = cohort1, Cohort_id = "id",
                           study_start = "2000-01-01", study_end = "2000-01-15",
                           cores = 1)
  Obj <- ind_simult(rx_proc1, calendar = TRUE, cores = 1)
  Exp <- polypharmacy:::test_ind_simult
  expect_equal(Obj, Exp, ignore_attr = TRUE)
})
