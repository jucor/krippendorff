# Manually extracted from http://afhayes.com/public/cmm2007.pdf , Table 1, p. 84
library(data.table)

news.tone.wide <- fread('data-raw/news.tone.hayes2007.csv', na.strings=".", header=TRUE)
save(news.tone.wide,
       file='data/news.tone.wide.rda', compress="bzip2")

news.tone <- melt(news.tone.wide,
                  id.vars='unit',
                  variable.name="observer",
                  value.name="tone",
                  na.rm = TRUE)
save(news.tone,
       file='data/news.tone.rda', compress="bzip2")
