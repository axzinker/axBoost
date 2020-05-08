#' Plot a moderated regression interaction diagram and return graphical coordinates
#'
#' Function plots a moderated regression interaction diagramm and returns the
#' coordinates of the points pred_H_mod_H pred_H_mod_L pred_L_mod_H pred_L_mod_L
#' to be used elswhere (e.g., in another plot).
#'
#' @param betas Coefficients (Betas) of the moderated regression
#' @param pred String with the name of the predictor
#' @param mod String with the name of the moderator
#' @param crit String with the name of the criterium
#' @param modRangeL Lower end of the moderator to be plottet (default: -1 SD)
#' @param modRangeH Upper end of the moderator to be plottet (default: 1 SD)
#' @param modLabSeq Moderation label sequence (1 or 2). Cope with the lm() function,
#' which puts out the labels in 2 different orders pred:mod (which is 1), or mod:pred
#' (which is 2). Default is 1
#' @param title Title of the plot
#' @param plot Logical value if a plot should be printed (default is TRUE)
#'
#' @return Named vector with the four coordinates (pred_H_mod_H, pred_H_mod_L,
#' pred_L_mod_H, pred_L_mod_L) on the y-axis for predictor at -1/1 SD
#' and moderator at modRangeL/modRangeH SD
#'
#' @author Axel Zinkernagel \email{zinkernagel@uni-landau.de}
#'
#' @examples
#' # Axel: fix me
#'
#' @import graphics
#' @import stats
#'
#' @export
modRegGraph <- function(betas, pred, mod, crit, modRangeL = -1, modRangeH = 1, modLabSeq = 1, title = "", plot = TRUE) {

  xAchse <- c(-1.2,1.2)
  yAchse <- c(-1.2,1.2)
  moderatorlabels <- c(paste0("low ",mod,"(",modRangeL," SD)"),paste0("high ",mod,"(",modRangeH," SD)"))
  legendposition <- c("topleft")

  # sequence HH, etc is: pred, mod
  if (modLabSeq == 1) {
    HH <- (1 * betas[pred]) + (modRangeH * betas[mod]) + (1 * modRangeH * betas[paste0(pred, ":", mod)])
    HL <- (1 * betas[pred]) + (modRangeL * betas[mod]) + (1 * modRangeL * betas[paste0(pred, ":", mod)])
    LH <- (-1 * betas[pred]) + (modRangeH * betas[mod]) + (-1 * modRangeH * betas[paste0(pred, ":", mod)])
    LL <- (-1 * betas[pred]) + (modRangeL * betas[mod]) + (-1 * modRangeL * betas[paste0(pred, ":", mod)])
  } else {
    HH <- (1 * betas[pred]) + (modRangeH * betas[mod]) + (1 * modRangeH * betas[paste0(mod, ":", pred)])
    HL <- (1 * betas[pred]) + (modRangeL * betas[mod]) + (1 * modRangeL * betas[paste0(mod, ":", pred)])
    LH <- (-1 * betas[pred]) + (modRangeH * betas[mod]) + (-1 * modRangeH * betas[paste0(mod, ":", pred)])
    LL <- (-1 * betas[pred]) + (modRangeL * betas[mod]) + (-1 * modRangeL * betas[paste0(mod, ":", pred)])
  }
  Grafikpunkte <- c(HH,HL,LH,LL)
  names(Grafikpunkte) <- c("pred_H_mod_H","pred_H_mod_L","pred_L_mod_H","pred_L_mod_L")

  mLow <- matrix(c(-1,1,LL,HL), nrow = 2)
  mHigh <- matrix(c(-1,1,LH,HH), nrow = 2)

  if (plot) {
    plot(mLow, xlim = xAchse, ylim = yAchse, type = "n", xlab = pred, ylab = crit, title = title)

    for (k in seq(yAchse[1], yAchse[2], by = .1)) {
      if (round(k %% .2,2) == 0) {
        abline(h = k, col = "lightgrey")
      }
    }

    points(mLow, pch = 22, col = "black")
    points(mHigh, pch = 19, col = "black")
    lines(mLow, col = "black", lty = 2)
    lines(mHigh, col = "black")

    # Legende erstellen
    if (nchar(moderatorlabels[1]) > nchar(moderatorlabels[2])) {
      moderatorlabels2 = moderatorlabels[1]
    } else {
      moderatorlabels2 = moderatorlabels[2]
    }
    legend(legendposition, legend = moderatorlabels, text.width = strwidth(moderatorlabels2),
           lty = c(2,1), pch = c(22,19), xjust = 1, yjust = 1)
  }
  return(Grafikpunkte)
}
