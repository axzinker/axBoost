#' Plots the percentage of answers of likert scale answer options. Alternative
#' could be likert::likert()
#'
#' @param dat A dataframe with items in columns.
#' @param itemLabels Labels of the anwer options. If empty, no legend is added.
#' @param refItem Name of the column of the Item which should be used for
#' reference. It is used for number of categories and should contain all
#' possible values. Default is the first item.
#'
#' @return Returns a plot and the percentage values on console
#'
#' @author Axel Zinkernagel \email{zinkernagel@uni-wuppertal.de}
#'
#' @examples
#' \dontrun{
#' itemNames <- c("Item1", "Item2", "Item3")
#' df <- matrix(c(4, 2, 4,
#'             1, 0, 2,
#'             4, 4, 4,
#'             3, 3, 2,
#'             3, 3, 2,
#'             0, 4, 2,
#'             2, 4, 2),
#'           ncol = 3, byrow = TRUE)
#' df <- as.data.frame(df)
#' names(df) <- itemNames
#' # itemLabels <- attr(attr(df[,"Item1"], "value.labels"), "names")
#' itemLabels <- c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree")
#'
#' percLikertPlot(df)
#' percLikertPlot(df, itemLabels = itemLabels)
#' percLikertPlot(df, refItem = df[,"Item3"], itemLabels = itemLabels) # leads to an error
#' }
#' @export
percLikertPlot <- function(dat, itemLabels = NULL, refItem = dat[,1]) {
  dat <- na.omit(dat) # missings rausnehmen
  itemRange <- unique(refItem)
  colors <- c("red", "orange", "yellow", "green", "blue","darkblue", "brown", "black")
  n <- nrow(dat)
  itemNames <- names(dat)
  #itemLabels <- attr(attr(dat[,1], "value.labels"), "names") # first variable is used for labelling
  relFreqMat <- matrix(, nrow =  ncol(dat), ncol = (length(itemRange)))
  relFreqMat <- as.data.frame(relFreqMat)
  names(relFreqMat) <- itemRange

  for (i in 1:ncol(dat)) {
    tmp <- factor(dat[,i], levels = itemRange) # also include unused categories!
    tmp <- as.numeric(table(tmp))
    relFreqMat[i,] <- tmp
  }

  relFreqMat <- (relFreqMat / n) * 100
  relFreqMat <- as.data.frame(relFreqMat)
  rownames(relFreqMat) <- itemNames
  colnames(relFreqMat) <- itemLabels

  barplot(t(relFreqMat),
          horiz = TRUE,
          col = colors,
          legend.text = itemLabels,
          las = 1,
          cex.names = 0.75,
          args.legend = list(x = "topright", bty = "n"),
          xlab = "Percentage",
          main = "Relative frequency of Likert-item values")

  return(round(relFreqMat,2)) # for Debugging purposes
}
