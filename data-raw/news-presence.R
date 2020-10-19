library(data.table)

# Manually read from Krippendorff 2004, section 11.3.1, page 224
raw.matrix <- read.table('data-raw/news.presence.krippendorff2004.11.3.1.csv', stringsAsFactors = FALSE)
news.presence.wide <- data.table(t(raw.matrix[,-1]))
colnames(news.presence.wide) <- raw.matrix[,1]
rownames(news.presence.wide) <- NULL

news.presence <- melt(news.presence.wide,
                           id.vars='article',
                           variable.name='observer',
                           value.name='presence')
usethis::use_data(news.presence, overwrite=TRUE)
usethis::use_data(news.presence.wide, overwrite=TRUE)
