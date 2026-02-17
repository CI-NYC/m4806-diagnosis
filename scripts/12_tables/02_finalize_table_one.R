library(xtable)

path <- "~/medicaid/M4806-diagnosis-cohort/scripts/12_tables"

source(file.path(path, "01_table_one_function.R"))

overall <- table_one_function(data)[1:52,]

write.csv(overall, file.path(path, "table_one.csv"), row.names=F)

dim(data)

overall[overall == "0 (NaN\\%)"] <- ""
print(
  xtable(
    caption = "",
    overall,
  ),
  include.rownames = FALSE,
  sanitize.text.function = identity,
  booktabs  = TRUE,
  caption.placement      = "top",
)

