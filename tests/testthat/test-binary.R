library(data.table)

context("K-alpha without bootstrapping")

test_that("Melt works as expected",
        {
          expected = read.csv(text='Article,observer,measurement
1,Jon,1
2,Jon,1
3,Jon,0
4,Jon,0
5,Jon,0
6,Jon,0
7,Jon,0
8,Jon,0
9,Jon,0
10,Jon,0
1,Han,0
2,Han,1
3,Han,1
4,Han,0
5,Han,0
6,Han,1
7,Han,0
8,Han,1
9,Han,0
10,Han,0', stringsAsFactors = FALSE)
          obtained = to.long(krippendorff2004.binary.two.observers, unit="Article", observers=c("Jon", "Han"))
          obtained$observer <- as.character(obtained$observer)
          expect_equal(obtained, expected)
          })

test_that("K-alpha on book section 11.3.1", {
          long <- data.table(to.long(krippendorff2004.binary.two.observers, unit="Article", observers=c("Jon", "Han")))
          expect_equal(kalpha(long,
                              unit = "Article",
                              measurement = "measurement",
                              level = "nominal"),
                       0.095)
          })
