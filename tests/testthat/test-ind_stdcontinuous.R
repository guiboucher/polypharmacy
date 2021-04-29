test_that("ind_stdcontinuous", {
  rx1 <- data.frame(id = c(1, 1, 1, 2, 3),
                    code = c("A", "A", "B", "A", "A"),
                    date = c("2000-01-01", "2000-01-22", "2000-01-10", "2000-01-01", "2000-01-20"),
                    duration = c(10, 10, 22, 31, 12))
  cohort1 <- data.frame(id = as.numeric(1:4),
                        age = c(45, 12, 89, 31),
                        sex = c("F", "F", "M", "M"))
  rx_proc1 <- data_process(Rx_deliv = rx1, Rx_id = "id", Rx_drug_code = "code",
                           Rx_drug_deliv = "date", Rx_deliv_dur = "duration",
                           Cohort = cohort1, Cohort_id = "id",
                           study_start = "2000-01-01", study_end = "2000-01-31",
                           cores = 1)
  Obj <- ind_stdcontinuous(processed_tab = rx_proc1, pdays = 10)
  Exp <- polypharmacy:::test_ind_stdcontinuous
  expect_equal(Obj, Exp, ignore_attr = TRUE)
})
