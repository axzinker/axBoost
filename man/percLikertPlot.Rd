% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/percLikertPlot.R
\name{percLikertPlot}
\alias{percLikertPlot}
\title{Plots the percentage of answers of likert scale answer options. Alternative
could be likert::likert()}
\usage{
percLikertPlot(dat, itemLabels = NULL, refItem = dat[, 1])
}
\arguments{
\item{dat}{A dataframe with items in columns.}

\item{itemLabels}{Labels of the anwer options. If empty, no legend is added.}

\item{refItem}{Name of the column of the Item which should be used for
reference. It is used for number of categories and should contain all
possible values. Default is the first item.}
}
\value{
Returns a plot and the percentage values on console
}
\description{
Plots the percentage of answers of likert scale answer options. Alternative
could be likert::likert()
}
\examples{
\dontrun{
itemNames <- c("Item1", "Item2", "Item3")
df <- matrix(c(4, 2, 4,
            1, 0, 2,
            4, 4, 4,
            3, 3, 2,
            3, 3, 2,
            0, 4, 2,
            2, 4, 2),
          ncol = 3, byrow = TRUE)
df <- as.data.frame(df)
names(df) <- itemNames
# itemLabels <- attr(attr(df[,"Item1"], "value.labels"), "names")
itemLabels <- c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree")

percLikertPlot(df)
percLikertPlot(df, itemLabels = itemLabels)
percLikertPlot(df, refItem = df[,"Item3"], itemLabels = itemLabels) # leads to an error
}
}
\author{
Axel Zinkernagel \email{zinkernagel@uni-wuppertal.de}
}
