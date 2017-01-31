# Manually extracted from http://afhayes.com/public/cmm2007.pdf , Table 1, p. 84
hayes2007 <- read.csv('data-raw/hayes2007table1.csv', na.strings=".", header=TRUE)
save(hayes2007, file='data/hayes2007.rda', compress="bzip2")
