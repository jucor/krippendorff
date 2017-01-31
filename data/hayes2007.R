#' News coverage reliability data set, Table 1 from Hayes & Krippendorff 2007
#'
#' An example reliability data set from Hayes & Krippendorf (2007).
#' From the original article:
#' \dQuote{The data for this example come from five observers who were asked to evaluate the local
#' news coverage given to the challenger running against an incumbent for a political office in one of several
#' political races. The observers were given 40 news- paper articles describing the race published by the
#' largest circulation newspaper in the incumbent’s district within 1 week prior to the election.
#' These 40 articles were randomly selected from the pool of all articles published during that 1-week period.
#' Observers rated whether the tone of the article suggested the challenger was a sure loser (0),
#' somewhat competitive (1), competitive (2), or a likely winner (3). After training, observers read and
#' judged the articles independently. The data were entered into SPSS such that each article was represented
#' with a row in the data file, and each observer’s evaluation of the articles was located in the columns,
#' with the columns labeled “obs1,” “obs2,” “obs3,” “obs4,” and “obs5.” Thus, in this example, the data
#' occupied a 40 (articles) × 5 (observers) matrix, with each cell in the matrix containing a 0, 1, 2, 3,
#' or a period character (“.”) for missing judgments.}
#' TODO(jucor): Find how to cite Hayes 2007 properly.
#'
#' @format A data frame with 40 rows and 6 variables:
#' \describe{
#' \item{unit}{unit observed}
#' \item{obs1}{ordinal judgement from observer 1}
#' \item{obs2}{ordinal judgement from observer 2}
#' \item{obs3}{ordinal judgement from observer 3}
#' \item{obs4}{ordinal judgement from observer 4}
#' \item{obs5}{ordinal judgement from observer 5}
#' }
