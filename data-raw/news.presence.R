library(data.table)

# Manually read from Krippendorff 2004, section 11.3.1, page 224
raw.matrix <- read.table('data-raw/news.presence.krippendorff2004.11.3.1.csv', stringsAsFactors = FALSE)
news.presence.wide <- data.table(t(raw.matrix[,-1]))
colnames(news.presence.wide) <- raw.matrix[,1]
rownames(news.presence.wide) <- NULL
save(news.presence.wide,
     file="data/news.presence.wide.rda",
     compress="bzip2")

news.presence <- melt(news.presence.wide,
                           id.vars='article',
                           variable.name='observer',
                           value.name='presence')
save(news.presence,
     file="data/news.presence.rda",
     compress="bzip2")
