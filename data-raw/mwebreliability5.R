library(data.table)

# Manually copied from
# https://repository.upenn.edu/cgi/viewcontent.cgi?article=1043&context=asc_papers # nolint
# TODO(jucor) cite document properly
mwebreliability5_matrix <- read.table(text = "1 2 3 3 2 1 4 1 2 . . .
1 2 3 3 2 2 4 1 2 5 . 3
. 3 3 3 2 3 4 2 2 5 1 .
1 2 3 3 2 4 4 1 2 5 1 .", na.strings = ".")

mwebreliability5_wide <- data.table(t(mwebreliability5_matrix))
mwebreliability5_wide$unit <- 1:nrow(mwebreliability5_wide)
mwebreliability5 <- melt(
  mwebreliability5_wide,
  id.vars = "unit",
  variable.name = "observer",
  value.name = "measurement",
  na.rm = TRUE
)

usethis::use_data(mwebreliability5, overwrite = TRUE)
usethis::use_data(mwebreliability5_wide, overwrite = TRUE)
