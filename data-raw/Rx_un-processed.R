library(usethis)
library(data.table)
library(lubridate)
library(polypharmacy)

set.seed(23)
Rx_unprocessed <- data.table()
for (i in 1:100) {  # id number
  for (j in sample(LETTERS, sample(1:20, 1))) {  # code number
    Rx_unprocessed <- rbind(
      Rx_unprocessed, data.table(
        id = i,
        code = j,
        start = as_date(sample(10957:11687, sample(1:30), 1)),
        duration = sample(c(1, 7, 15, 30, 45, 60, 90), 1)
      )
    )
  }
}
setkey(Rx_unprocessed, id, code, start)

Rx_processed <- data_process(
  Rx_deliv = Rx_unprocessed,
  Rx_id = "id", Rx_drug_code = "code",
  Rx_drug_deliv = "start", Rx_deliv_dur = "duration"
)

use_data(Rx_unprocessed,
         Rx_processed,

         overwrite = TRUE)
