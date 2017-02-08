library(data.table)
# Manually copied from  mwebreliability5.pdf
# TODO(jucor) cite Krippendorff properly
mwebreliability5.matrix <- read.table(text="1 2 3 3 2 1 4 1 2 . . .
1 2 3 3 2 2 4 1 2 5 . 3
. 3 3 3 2 3 4 2 2 5 1 .
1 2 3 3 2 4 4 1 2 5 1 .", na.string=".")

mwebreliability5.wide <- data.table(t(mwebreliability5.matrix))
mwebreliability5.wide$unit <- 1:nrow(mwebreliability5.wide)
mwebreliability5 <- melt(mwebreliability5.wide, id.vars="unit", variable.name="observer", value.name="measurement", na.rm = TRUE)

save(mwebreliability5, file="data/mwebreliability5.rda")
save(mwebreliability5.wide, file="data/mwebreliability5.wide.rda")

