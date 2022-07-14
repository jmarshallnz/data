#' source of files is:
#' 
#' http://www.nzpho.org.nz/NotifiableDisease.aspx
#' 
#' and for more recent years https://surv.esr.cri.nz/PDF_surveillance/MthSurvRpt/

library(tidyverse)

counts <- read_csv("~/data/data/campylobacteriosis/Total_data.csv") %>%
  janitor::clean_names()

counts %>% count(measure_names)

counts %>% pivot_wider(names_from = measure_names, values_from = measure_values) # Doesn't have month :(

library(tabular)

foo <- extract_tables("~/data/data/campylobacteriosis/2018/201801JanDHB.pdf")

test <- locate_areas("~/data/data/campylobacteriosis/2018/201801JanDHB.pdf")

extract_data <- function(file, base_path = "~/data/data/campylobacteriosis/2018") {
  cat("reading file:", file, "\n")
  test <- locate_areas(file.path(base_path, file))
  ans <- extract_tables(file.path(base_path, file), area=test)[[1]]
  mat <- sub(".*?([0-9\\.]+)", "\\1", ans)
  data.frame(counts = as.numeric(mat[1,]), rates = as.numeric(mat[2,]), file=file)
}

pdfs_to_read <- list.files("~/data/data/campylobacteriosis/2018", "*.pdf")

out <- map_dfr(pdfs_to_read, extract_data)
write_csv(out, file.path(base_path, "processed_pdfs.csv"))

# try 2017?
base_path <- "~/data/data/campylobacteriosis/2017"
pdfs_to_read <- list.files(base_path, "*.pdf")
out <- map_dfr(pdfs_to_read, extract_data, base_path=base_path)
write_csv(out, file.path(base_path, "processed_pdfs.csv"))
