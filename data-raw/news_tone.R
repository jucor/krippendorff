# Manually extracted from http://afhayes.com/public/cmm2007.pdf , Table 1, p. 84
library(data.table)

news_tone_wide <- fread(
  "data-raw/news_tone_hayes2007.csv",
  na.strings = ".",
  header = TRUE
)
news_tone <- melt(news_tone_wide,
  id.vars = "unit",
  variable.name = "observer",
  value.name = "tone",
  na.rm = TRUE
)


usethis::use_data(news_tone_wide, overwrite = TRUE)
usethis::use_data(news_tone, overwrite = TRUE)
