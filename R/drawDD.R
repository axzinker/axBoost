#' Draw a double dissociation based on regression coefficients
#'
#' Draws a double dissociation based on regression coefficients.
#'
#' Assuming a double dissociation, the function computes two regressions:
#'
#' critUpper ~ predUpper + predLower
#'
#' critLower ~ predLower + predUpper
#'
#' If a moderator is given, the regressions are computed as:
#'
#' critUpper ~ predUpper * moderator + predLower
#'
#' critLower ~ predLower * moderator + predUpper
#'
#' If modBothPaths = TRUE, the regressions are computed as:
#'
#' critUpper ~ predUpper * moderator + predLower * moderator
#'
#' critLower ~ predLower * moderator + predUpper * moderator
#'
#' All variables are scaled before computing the regressions to cope with
#' multicolinearity in the moderated case.
#'
#' @param data A data frame containing the numeric columns predUpper,
#' predLower, critUpper, critLower, and optionally moderator.
#' @param predUpper String with the name of the Predictor of the upper double dissociation path.
#' @param predLower String with the name of the Predictor of the lower double dissociation path.
#' @param critUpper String with the name of the Criterion of the upper double dissociation path.
#' @param critLower String with the name of the Criterion of the lower double dissociation path.
#' @param moderator String with the name of the Moderator of the upper and lower direct path from
#' predictor to criterion
#' @param moderatorSD Defines the standard deviations for which the slope
#' lines are plotted. Default is -1 and 1. For example, for experimetal group
#' / control group design use c(0,1).
#' @param modBothPaths If TRUE, a moderator effect is computed for both paths
#' @param printReg Print the summary () of the regression coefficients.
#' @param title Title of plot
#'
#' @return Returns a plot with a (moderated) double dissociation.
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
drawDD <- function(data, predUpper, predLower, critUpper, critLower,
                   moderator = "", moderatorSD = c(-1,1), modBothPaths = FALSE,
                   printReg = FALSE, title = ""){

  # Define constants
  # Colors for sig., marginal sig. and not sig. paths in the plot
  colNotSig <- "black"
  colMargSig <- "darkred"
  colSig <- "red"

  # compute the regressions
  if (moderator != "") {
    if (modBothPaths == FALSE) {
      upper <- lm(scale(data[[critUpper]]) ~ scale(data[[predUpper]]) * scale(data[[moderator]]) + scale(data[[predLower]]))
      lower <- lm(scale(data[[critLower]]) ~ scale(data[[predLower]]) * scale(data[[moderator]]) + scale(data[[predUpper]]))
      # Rename variables
      names(upper$coefficients) <- c("(Intercept)", predUpper, moderator, predLower, paste0(predUpper,":",moderator))
      names(lower$coefficients) <- c("(Intercept)", predLower, moderator, predUpper, paste0(predLower,":",moderator))
    } else {# modBothPaths == TRUE
      upper <- lm(scale(data[[critUpper]]) ~ scale(data[[predUpper]]) * scale(data[[moderator]]) + scale(data[[predLower]]) * scale(data[[moderator]]))
      lower <- lm(scale(data[[critLower]]) ~ scale(data[[predLower]]) * scale(data[[moderator]]) + scale(data[[predUpper]]) * scale(data[[moderator]]))
      # Rename variables
      names(upper$coefficients) <- c("(Intercept)", predUpper, moderator, predLower, paste0(predUpper,":",moderator), paste0(moderator,":",predLower))
      names(lower$coefficients) <- c("(Intercept)", predLower, moderator, predUpper, paste0(predLower,":",moderator), paste0(moderator,":",predUpper))
    }
  } else {
    upper <- lm(scale(data[[critUpper]]) ~ scale(data[[predUpper]]) + scale(data[[predLower]]))
    lower <- lm(scale(data[[critLower]]) ~ scale(data[[predLower]]) + scale(data[[predUpper]]))
    # Rename variables
    names(upper$coefficients) <- c("(Intercept)", predUpper, predLower)
    names(lower$coefficients) <- c("(Intercept)", predLower, predUpper)
  }

  # compute correlations
  predCor <- cor.test(data[[predUpper]], data[[predLower]], use = "pairwise")
  critCor <- cor.test(data[[critUpper]], data[[critLower]], use = "pairwise")

  # rewrite the formula call
  if (printReg) {
    formulaCallUpper <- as.character(upper$call[2])
    formulaCallLower <- as.character(lower$call[2])
    formulaCallUpper <- gsub("scale(data[[critUpper]])", critUpper, formulaCallUpper, fixed = TRUE)
    formulaCallUpper <- gsub("scale(data[[predUpper]])", predUpper, formulaCallUpper, fixed = TRUE)
    formulaCallUpper <- gsub("scale(data[[predLower]])", predLower, formulaCallUpper, fixed = TRUE)
    formulaCallLower <- sub("scale(data[[critLower]])", critLower, formulaCallLower, fixed = TRUE)
    formulaCallLower <- sub("scale(data[[predLower]])", predLower, formulaCallLower, fixed = TRUE)
    formulaCallLower <- sub("scale(data[[predUpper]])", predUpper, formulaCallLower, fixed = TRUE)
    if (moderator != "") {
      formulaCallUpper <- gsub("scale(data[[moderator]])", moderator, formulaCallUpper, fixed = TRUE)
      formulaCallLower <- gsub("scale(data[[moderator]])", moderator, formulaCallLower, fixed = TRUE)
    }
    upper$call[2] <- formulaCallUpper
    lower$call[2] <- formulaCallLower
    print(summary(upper))
    print(summary(lower))
    print(predCor)
    print(critCor)
  }

  # make basic plot
  plot(NULL, main = title, type = "n", xlim = c(-10,10), ylim = c(-7,7),
       xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '')
  rect(-10, 1, -5, 4, density = 0, border = "black")
  rect(-10, -1, -5, -4, density = 0, border = "black")
  rect(10, 1, 5, 4, density = 0, border = "black")
  rect(10, -1, 5, -4, density = 0, border = "black")

  # add variable labels
  text(5, 2.5, critUpper, col = "black", cex = .7, pos = 4) # AVoben
  text(5, -2.5, critLower, col = "black", cex = .7, pos = 4) # AVunten
  text(-10,2.5, predUpper, col = "black", cex = .7, pos = 4) # Pred1
  text(-10,-2.5, predLower, col = "black", cex = .7, pos = 4) # Pred2

  # mark significant paths
  upper_p_col <- c(colNotSig,colNotSig,colNotSig,colNotSig,colNotSig,colNotSig)
  upper_p_sign <- c("","","","","","")
  upper_p_col <- replace(upper_p_col, 2*pt(abs(coef(upper) / sqrt(diag(vcov(upper)))) * -1, df = upper$df.residual) < .1, colMargSig)
  upper_p_sign <- replace(upper_p_sign, 2*pt(abs(coef(upper) / sqrt(diag(vcov(upper)))) * -1, df = upper$df.residual) < .1, "+")
  upper_p_col <- replace(upper_p_col,2*pt(abs(coef(upper) / sqrt(diag(vcov(upper)))) * -1, df = upper$df.residual) < .05, colSig)
  upper_p_sign <- replace(upper_p_sign,2*pt(abs(coef(upper) / sqrt(diag(vcov(upper)))) * -1, df = upper$df.residual) < .05, "*")

  lower_p_col <- c(colNotSig,colNotSig,colNotSig,colNotSig,colNotSig,colNotSig)
  lower_p_sign <- c("","","","","","")
  lower_p_col <- replace(lower_p_col, 2*pt(abs(coef(lower) / sqrt(diag(vcov(lower)))) * -1, df = lower$df.residual) < .1, colMargSig)
  lower_p_sign <- replace(lower_p_sign, 2*pt(abs(coef(lower) / sqrt(diag(vcov(lower)))) * -1, df = lower$df.residual) < .1, "+")
  lower_p_col <- replace(lower_p_col,2*pt(abs(coef(lower) / sqrt(diag(vcov(lower)))) * -1, df = lower$df.residual) < .05, colSig)
  lower_p_sign <- replace(lower_p_sign,2*pt(abs(coef(lower) / sqrt(diag(vcov(lower)))) * -1, df = lower$df.residual) < .05, "*")

  # mark significant correlations
  predCor_p_col <- c(colNotSig)
  predCor_p_sign <- c("")
  predCor_p_col <- replace(predCor_p_col, predCor$p.value < .1, colMargSig)
  predCor_p_sign <- replace(predCor_p_sign, predCor$p.value < .1, "+")
  predCor_p_col <- replace(predCor_p_col, predCor$p.value < .05, colSig)
  predCor_p_sign <- replace(predCor_p_sign, predCor$p.value < .05, "*")

  critCor_p_col <- c(colNotSig)
  critCor_p_sign <- c("")
  critCor_p_col <- replace(critCor_p_col, critCor$p.value < .1, colMargSig)
  critCor_p_sign <- replace(critCor_p_sign, critCor$p.value < .1, "+")
  critCor_p_col <- replace(critCor_p_col, critCor$p.value < .05, colSig)
  critCor_p_sign <- replace(critCor_p_sign, critCor$p.value < .05, "*")

  # Plot correlations (equal for all regression models)
  text(-10.3, 0, paste0(round(predCor$estimate, digits = 2),predCor_p_sign), col = predCor_p_col, cex = 1, pos = 4)
  text(8.3, 0, paste0(round(critCor$estimate, digits = 2),critCor_p_sign), col = critCor_p_col, cex = 1, pos = 4)

  # add correlation arcs
  plotrix::draw.arc(-7.5, 0, 3, deg1 = 213, deg2 = 147, col = predCor_p_col)
  plotrix::draw.arc(7.5, 0, 3, deg1 = 33, deg2 = 0, col = critCor_p_col)
  plotrix::draw.arc(7.5, 0, 3, deg1 = 360, deg2 = 327, col = critCor_p_col)

  # Plots must be defined differently for each model, because the sequence of predictors is different for each lm() model
  ###########
  # Plot simple double dissociation
  if (moderator == "") {
    # add colored beta weights
    text(2.5, 1, paste0(round(coef(upper)[3], digits = 2),upper_p_sign[3]), col = upper_p_col[3], cex = 1, pos = 4) # PredLower -> CritUpper
    text(2.5, 3.5, paste0(round(coef(upper)[2], digits = 2),upper_p_sign[2]), col = upper_p_col[2], cex = 1, pos = 4) # PredUpper -> CritUpper
    text(2.5,-1, paste0(round(coef(lower)[3], digits = 2),lower_p_sign[3]), col = lower_p_col[3], cex = 1, pos = 4) # PredUper -> CritLower
    text(2.5,-3.5, paste0(round(coef(lower)[2], digits = 2),lower_p_sign[2]), col = lower_p_col[2], cex = 1, pos = 4) # PredLower -> CritLower

    # add colored arrows
    arrows(-5,2.5,5,2.5,col = upper_p_col[2], angle = 10) # predUpper -> critUpper
    arrows(-5,-2.5,5,2.5, col = upper_p_col[3], angle = 10) # preLower -> critUpper
    arrows(-5,-2.5,5,-2.5, col = lower_p_col[2], angle = 10) # predLower -> critLower
    arrows(-5,2.5,5,-2.5, col = lower_p_col[3], angle = 10) # predUpper -> critLower
  }
  ###########
  # Plot moderated double dissociation
  if (moderator != "" && modBothPaths == FALSE) {
    # add colored beta weights
    text(2.5, 1, paste0(round(coef(upper)[4], digits = 2),upper_p_sign[4]), col = upper_p_col[4], cex = 1, pos = 4) # PredLower -> CritUpper
    text(2.5, 3.5, paste0(round(coef(upper)[2], digits = 2),upper_p_sign[2]), col = upper_p_col[2], cex = 1, pos = 4) # PredUpper -> CritUpper
    text(2.5,-1, paste0(round(coef(lower)[4], digits = 2),lower_p_sign[4]), col = lower_p_col[4], cex = 1, pos = 4) # PredUper -> CritLower
    text(2.5,-3.5, paste0(round(coef(lower)[2], digits = 2),lower_p_sign[2]), col = lower_p_col[2], cex = 1, pos = 4) # PredLower -> CritLower

    # add colored arrows
    arrows(-5,2.5,5,2.5,col = upper_p_col[2], angle = 10) # predUpper -> critUpper
    arrows(-5,-2.5,5,2.5, col = upper_p_col[4], angle = 10) # preLower -> critUpper
    arrows(-5,-2.5,5,-2.5, col = lower_p_col[2], angle = 10) # predLower -> critLower
    arrows(-5,2.5,5,-2.5, col = lower_p_col[4], angle = 10) # predUpper -> critLower

    # add moderator boxes
    rect(-2.5, 4, 2.5, 7, density = 0, border = "black")
    rect(-2.5,-4, 2.5,-7, density = 0, border = "black")
    # add moderator label
    text(-2.5, 5.5, moderator, col = "black", cex = .7, pos = 4) # upper moderator
    text(-2.5,-5.5, moderator, col = "black", cex = .7, pos = 4) # lower moderator
    # add colored arrows
    arrows(2.5,5.5,7.5,4,col = upper_p_col[3], angle = 10) # moderator -> critUpper
    arrows(-2,4,-2,2.5,col = upper_p_col[5], angle = 10) # moderator -> pathUpper
    arrows(2.5,-5.5,7.5,-4,col = lower_p_col[3], angle = 10) # moderator -> critLower
    arrows(-2,-4,-2,-2.5,col = lower_p_col[5], angle = 10) # moderator -> pathLower
    # add colored beta weights
    text(6, 5, paste0(round(coef(upper)[3], digits = 2),upper_p_sign[3]), col = upper_p_col[3], cex = 1, pos = 4) # moderator -> critUpper
    text(-2, 3, paste0(round(coef(upper)[5], digits = 2),upper_p_sign[5]), col = upper_p_col[5], cex = 1, pos = 4) # moderator -> critUpper
    text(6, -5.3, paste0(round(coef(lower)[3], digits = 2),lower_p_sign[3]), col = lower_p_col[3], cex = 1, pos = 4) # moderator -> critUpper
    text(-2, -3.3, paste0(round(coef(lower)[5], digits = 2),lower_p_sign[5]), col = lower_p_col[5], cex = 1, pos = 4) # moderator -> critUpper
  }
  ###########
  # Plot moderatored on both paths double dissociation
  if (moderator != "" && modBothPaths == TRUE) {
    # add colored beta weights
    text(2.5, 1, paste0(round(coef(upper)[4], digits = 2),upper_p_sign[4]), col = upper_p_col[4], cex = 1, pos = 4) # PredLower -> CritUpper
    text(2.5, 3.5, paste0(round(coef(upper)[2], digits = 2),upper_p_sign[2]), col = upper_p_col[2], cex = 1, pos = 4) # PredUpper -> CritUpper
    text(2.5,-1, paste0(round(coef(lower)[4], digits = 2),lower_p_sign[4]), col = lower_p_col[4], cex = 1, pos = 4) # PredUper -> CritLower
    text(2.5,-3.5, paste0(round(coef(lower)[2], digits = 2),lower_p_sign[2]), col = lower_p_col[2], cex = 1, pos = 4) # PredLower -> CritLower

    # add colored arrows
    arrows(-5,2.5,5,2.5,col = upper_p_col[2], angle = 10) # predUpper -> critUpper
    arrows(-5,-2.5,5,2.5, col = upper_p_col[4], angle = 10) # preLower -> critUpper
    arrows(-5,-2.5,5,-2.5, col = lower_p_col[2], angle = 10) # predLower -> critLower
    arrows(-5,2.5,5,-2.5, col = lower_p_col[4], angle = 10) # predUpper -> critLower

    # add moderator boxes
    rect(-2.5, 4, 2.5, 7, density = 0, border = "black")
    rect(-2.5,-4, 2.5,-7, density = 0, border = "black")
    # add moderator label
    text(-2.5, 5.5, moderator, col = "black", cex = .7, pos = 4) # upper moderator
    text(-2.5,-5.5, moderator, col = "black", cex = .7, pos = 4) # lower moderator
    # add colored arrows
    arrows(2.5,5.5,7.5,4,col = upper_p_col[3], angle = 10) # moderator -> critUpper
    arrows(-2,4,-2,2.5,col = upper_p_col[5], angle = 10) # moderator -> pathUpper
    arrows(2.5,-5.5,7.5,-4,col = lower_p_col[3], angle = 10) # moderator -> critLower
    arrows(-2,-4,-2,-2.5,col = lower_p_col[5], angle = 10) # moderator -> pathLower
    # add colored beta weights
    text(6, 5, paste0(round(coef(upper)[3], digits = 2),upper_p_sign[3]), col = upper_p_col[3], cex = 1, pos = 4) # moderator -> critUpper
    text(-2, 3, paste0(round(coef(upper)[5], digits = 2),upper_p_sign[5]), col = upper_p_col[5], cex = 1, pos = 4) # moderator -> critUpper
    text(6, -5.3, paste0(round(coef(lower)[3], digits = 2),lower_p_sign[3]), col = lower_p_col[3], cex = 1, pos = 4) # moderator -> critUpper
    text(-2, -3.3, paste0(round(coef(lower)[5], digits = 2),lower_p_sign[5]), col = lower_p_col[5], cex = 1, pos = 4) # moderator -> critUpper
    # add second moderator path
    arrows(-2.2,4,-2.2,1.1,col = upper_p_col[6], angle = 10) # moderator -> path2Upper
    arrows(-2.2,-4,-2.2,-1.1,col = lower_p_col[6], angle = 10) # moderator -> path2Lower
    text(-2.2, 1.5, paste0(round(coef(upper)[6], digits = 2),upper_p_sign[6]), col = upper_p_col[6], cex = 1, pos = 4) # moderator -> critUpper
    text(-2.2, -1.5, paste0(round(coef(lower)[6], digits = 2),lower_p_sign[6]), col = lower_p_col[6], cex = 1, pos = 4) # moderator -> critUpper
  }
}
# Function drawDD ends
################################################################################

# # Ab hier kommen Beispiele
#
# # Datensatz laden
# # OS Pfadanpassung
# if (R.version$os == "mingw32") {prePath = "C:/users"}
# if (R.version$os == "linux-gnu") {prePath = "/home"}
# midPath <- c("/axel")
# #midPath <- c("/nita")
#
# # Testdaten laden (die Pfade musst du natürlich anpassen)
# inputDirectory <- paste0(prePath,midPath,"/Nextcloud_Landau/20_Untersuchungen/Projekt_Ekel/02_Studien/05_Studie2_Dep_EkelAV_SoSe16/03_Daten/")
# outputDirectory <- paste0(prePath,midPath,"/Nextcloud_Landau/45_Methoden/R/drawDD/")
# load(paste0(inputDirectory,"Studie_1und2.rda"))
# # Sinnvoll umbenennen
# Study1Disgust <- Studie_1und2
# rm(Studie_1und2)

################################################################################
# Einfache Beispiele:

# Beispiel 1: mache einen DD-Plot mit Moderator und gib die Regressionen aus
# drawDD(data = Study1Disgust, predUpper = "Ekel_SBSTIAT", predLower = "BR_Kakerl_ekelig",
#        critUpper = "mean.Gesicht", critLower = "Kverhalten.ges",
#        moderator = "SKS", modBothPaths = TRUE, printReg = TRUE, title = "Beispiel 1")

# Beispiel 1: mache einen DD-Plot mit Moderator und gib die Regressionen aus
# drawDD(data = Study1Disgust, predUpper = "Ekel_SBSTIAT", predLower = "BR_Kakerl_ekelig",
#        critUpper = "mean.Gesicht", critLower = "Kverhalten.ges",
#        printReg = TRUE, title = "Beispiel 1")

# # Beispiel 2: selbes Beispiel, ohne Moderator und Ausgabe der Regression
# drawDD(data = Study1Disgust, predUpper = "Ekel_SBSTIAT", predLower = "BR_Kakerl_ekelig",
#        critUpper = "mean.Gesicht", critLower = "Kverhalten.ges",
#        title = "Beispiel 2")
#
# # Beispiel 3: Regressionen so wie auf deinem Poster, mit geschachtelten Schleifen
# # über explizite Praediktoren und Moderatoren
# dataEG <- subset(Study1Disgust, subset = (Study1Disgust$cond_label == "EG"))
# dataKG <- subset(Study1Disgust, subset = (Study1Disgust$cond_label == "KG"))
#
# pred <- c("TDDS","TDDS_path","BR_Kakerl_ekelig")
# moder <- c("PCRS","ESA","SKS") # weiß nicht, ob das die richtigen sind ;-)
#
# k = 0
# for (i in 1:length(pred)) {
#   for (j in 1:length(moder)) {
#     k = k + 1
#     # EG plot
#     drawDD(data = dataEG, predUpper = "Ekel_SBSTIAT", predLower = pred[i],
#            critUpper = "mean.Gesicht", critLower = "Kverhalten.ges",
#            moderator = moder[j],
#            title = paste0("Beisp. ",k,"/",length(pred)*length(moder),", EG, expl_P: ",pred[i],", mod: ",moder[j]))
#     # KG plot
#     drawDD(data = dataKG, predUpper = "Ekel_SBSTIAT", predLower = pred[i],
#            critUpper = "mean.Gesicht", critLower = "Kverhalten.ges",
#            moderator = moder[j],
#            title = paste0("Beisp. ",k,"/",length(pred)*length(moder),", KG, expl_P: ",pred[i],", mod: ",moder[j]))
#       }
# }

