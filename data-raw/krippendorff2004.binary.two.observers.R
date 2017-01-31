# Manually read from Krippendorff 2004, section 11.3.1, page 224
raw.matrix <- read.table('data-raw/krippendorff2004.binary.two.observers.csv')
df <- data.frame(t(raw.matrix[,-1]))
colnames(df) <- raw.matrix[,1]
rownames(df) <- NULL
krippendorff2004.binary.two.observers <- df
save(krippendorff2004.binary.two.observers,
     file="data/krippendorff2004.binary.two.observers.rda",
     compress="bzip2")
