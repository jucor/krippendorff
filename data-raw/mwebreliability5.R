library(data.table)
# Manually copied from  mwebreliability5.pdf
# TODO(jucor) cite Krippendorff properly
nominal.matrix <- read.table(text="1 2 3 3 2 1 4 1 2 . . .
1 2 3 3 2 2 4 1 2 5 . 3
. 3 3 3 2 3 4 2 2 5 1 .
1 2 3 3 2 4 4 1 2 5 1 .", na.string=".")

nominal.wide <- data.table(t(nominal.matrix))
nominal.wide$unit <- 1:nrow(nominal.wide)
nominal <- melt(nominal.wide, id.vars="unit", variable.name="observer", value.name="measurement", na.rm = TRUE)

save(nominal, file="data/mwebreliability5.R")
save(nominal.wide, file="data/mwebreliability5.wide.R")

