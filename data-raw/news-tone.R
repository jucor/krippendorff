# Manually extracted from http://afhayes.com/public/cmm2007.pdf , Table 1, p. 84
library(data.table)

news.tone.wide <- fread('data-raw/news.tone.hayes2007.csv', na.strings=".", header=TRUE)
news.tone <- melt(news.tone.wide,
                  id.vars='unit',
                  variable.name="observer",
                  value.name="tone",
                  na.rm = TRUE)


usethis::use_data(news.tone.wide, overwrite = TRUE)
usethis::use_data(news.tone, overwrite = TRUE)
