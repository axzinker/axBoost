#' Corrgram-like correlation table plot
#'
#' In the upper right triangle the correlations are plotted as numbers, and in the
#' lower left triangle a scatterplot with smoothed line is plotted.
#' In the diagonal histogramms are plotted
#'
#' @param data Matrix / DataFrame with items in columns.
#' @param method A character string indicating which correlation coefficient
#' (or covariance) is to be computed. One of "pearson" (default), "kendall",
#' or "spearman".
#' @param fontsize Base font size of the correlations in cex
#' @return Returns a data frame containing the parcels.
#'
#' @author Axel Zinkernagel \email{zinkernagel@uni-landau.de}
#'
#' @examples
#' \dontrun{
#' axCorrgram(axBoost::df1, fontsize = .7)
#' }
#'
#' @export
axCorrgram <- function(data, method = "pearson", fontsize = 3){

  # Helper Functions

  panel.cor <- function(x,y,digits=2, prefix = "", cex.cor) {
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- round(cor(x, y, method = method, use = "complete.obs"),2)
    sig <- cor.test(x, y, use = "complete.obs")
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste(prefix, txt, sep = "")
    cex.cor <- fontsize / strwidth(txt)
    if (r >= 0) {
      if ((sig$p.value < .1) & (sig$p.value >= .05)) {text(0.5, 0.5, paste0(txt,"+"), cex = cex.cor * (1 + abs(r)) / 2, col = "darkred")}
      if (sig$p.value < .05) {text(0.5, 0.5, paste0(txt,"*"), cex = cex.cor * (1 + abs(r)) / 2, col = "red")}
    } else {
      if ((sig$p.value < .1) & (sig$p.value >= .05)) {text(0.5, 0.5, paste0(txt,"+"), cex = cex.cor * (1 + abs(r)) / 2, col = "darkblue")}
      if (sig$p.value < .05) {text(0.5, 0.5, paste0(txt,"*"), cex = cex.cor * (1 + abs(r)) / 2, col = "blue")}
    }
    if (sig$p.value >= .1) {text(0.5, 0.5, txt, cex = cex.cor * (1 + abs(r)) / 2, col = "black")}
  }

  panel.hist <- function(x){
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5))
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks
    nB <- length(breaks)
    y <- h$counts
    y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col = "white")
  }

  panel.lm <- function(x, y, col = par("col"), bg = NA, pch = par("pch"), cex = 1, col.smooth = "black") {
    points(x, y, pch = pch, col = col, bg = bg, cex = cex)
    abline(stats::lm(y~x),col = col.smooth)
  }

  # Main function
  # generate Correlogram- like plot
  pairs(data, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)
}
