library(data.table)

# Manually read from Krippendorff 2004, section 11.3.1, page 224
raw_matrix <- read.table(
  "data-raw/news_presence_krippendorff2004_11_3_1.csv",
  stringsAsFactors = FALSE
)
news_presence_wide <- data.table(t(raw_matrix[, -1]))
colnames(news_presence_wide) <- raw_matrix[, 1]
rownames(news_presence_wide) <- NULL

news_presence <- melt(news_presence_wide,
  id.vars = "article",
  variable.name = "observer",
  value.name = "presence"
)
usethis::use_data(news_presence, overwrite = TRUE)
usethis::use_data(news_presence_wide, overwrite = TRUE)
