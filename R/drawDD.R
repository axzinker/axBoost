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
#' If covariates are givem, the function is computed as:
#'
#' critUpper ~ covUpper + predUpper + predLower
#'
#' critLower ~ covLower + predLower + predUpper
#'
#' If covResidAsDep is false, this is done as a multiple regression. If it is
#' true, this is done for the residuals of the covUpper/covLower regressions
#' as dependent variable. It works also in combination with moderators.
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
#' predLower, critUpper, critLower, and optionally covariates and a moderator.
#' @param predUpper String with the name of the predictor of the upper double dissociation path.
#' @param predLower String with the name of the predictor of the lower double dissociation path.
#' @param critUpper String with the name of the criterion of the upper double dissociation path.
#' @param critLower String with the name of the criterion of the lower double dissociation path.
#' @param covUpper Optional string vector with the names of the covariates for the criterion of the upper double dissociation path.
#' @param covLower Optional string vector with the names of the covariates for the criterion of the lower double dissociation path.
#' @param covResidAsDep If FALSE (the default), a multiple regression is computed. If TRUE, the residuals of the covariate regression are used as dependent variable.
#' @param moderator String with the name of the Moderator of the upper and lower direct path from
#' predictor to criterion
#' @param moderatorSD Defines the standard deviations for which the slope
#' lines are plotted. Default is -1 and 1. For example, for experimetal group
#' / control group design use c(0,1).
#' @param modLabSeq Moderation label sequence (1 or 2). Cope with the lm() function,
#' which puts out the labels in 2 different orders pred:mod (which is 1), or mod:pred
#' (which is 2). Default is 1.
#' @param plotRange Plot range of the square plot of a (marginally) significant interaction (only works with default of 1 SD at the moment).
#' @param modBothPaths If TRUE, a moderator effect is computed for both paths. Default is FALSE.
#' @param robust Logical value if robust parameter estimation is used (robust::lmRob() and WRS2::pbcor()). Default is FALSE.
#' @param printReg Print the summary () of the regression coefficients. Default is FALSE.
#' @param title Title of plot
#'
#' @return Returns a plot with a (moderated) (robust) double dissociation.
#'
#' @author Axel Zinkernagel \email{zinkernagel@uni-landau.de}
#'
#' @examples
#' # See vignette
#'
#' @import graphics
#' @import stats
#'
#' @export
drawDD <- function(data, predUpper, predLower, critUpper, critLower,
                   covUpper = NULL, covLower = NULL, covResidAsDep = FALSE,
                   moderator = NULL, moderatorSD = c(-1,1),
                   modLabSeq = 1, plotRange = 1, modBothPaths = FALSE,
                   robust = FALSE, printReg = FALSE, title = ""){

  # Define constants ####
  # Colors / Symbols for sig., marginal sig. and not sig. paths in the plot
  colNotSig <- "black"
  colMargSig <- "darkred"
  colSig <- "red"
  symNotSig <- ""
  symMargSig <- "+"
  symSig <- "*"
  # Scaling input variables, scale() function
  scale = TRUE
  center = TRUE
  # Variable to add some additional text into the plot (e.g., robust estimation, )
  addText <- NULL

  # Scale input data -> full standaradized regressions, no multicolinearity in moderated regression
  data[[predUpper]] <- scale(data[[predUpper]], scale = scale, center = center)
  data[[predLower]] <- scale(data[[predLower]], scale = scale, center = center)
  data[[critUpper]] <- scale(data[[critUpper]], scale = scale, center = center)
  data[[critLower]] <- scale(data[[critLower]], scale = scale, center = center)
  if (!is.null(moderator)) {
    data[[moderator]] <- scale(data[[moderator]], scale = scale, center = center)
  }
  if (!is.null(covUpper)) {
    for (i in 1:length(covUpper)) {
      data[[covUpper[i]]] <- scale(data[[covUpper[i]]], scale = scale, center = center)
    }
  }
  if (!is.null(covLower)) {
    for (i in 1:length(covLower)) {
      data[[covLower[i]]] <- scale(data[[covLower[i]]], scale = scale, center = center)
    }
  }

  # Rewrite rownames of input dataframe to handle missings (N observations
  # deleted due to missingness) in the output of regression computations
  rownames(data) <- 1:nrow(data)

  # Recompute DVs as redsiduals, if requested (covResidAsDep = TRUE) ####
  if (covResidAsDep == TRUE) {
    # Generate formula call for residual regressions

    # Upper regression
    if (!is.null(covUpper)) {
      functionCall <- paste0(paste0(critUpper," ~ ", paste0(covUpper, collapse = " + ")))

      if (robust == TRUE) {
        covResidUpper <- eval(parse(text = paste0("robust::lmRob(",functionCall,", data = data)")))
      } else {
        covResidUpper <- eval(parse(text = paste0("lm(",functionCall,", data = data)")))
      }

      if (printReg) {
        print(summary(covResidUpper))
      }

      # Correct missing residual computations (= check for consecutive line numbers)
      # and replace original DV with residuals
      rowNumbReg <- as.numeric(attr(covResidUpper$residuals, "names"))
      if (length(rowNumbReg) < nrow(data)) {
        rowNumbFill <- axBoost::fillVec(rowNumbReg,covResidUpper$residuals)
        data[critUpper] <- as.numeric(rowNumbFill[,2])
      } else {
        data[critUpper] <- covResidUpper$residuals
      }

      # prepare regression output for plotting (round, add col and sign info)
      covResidUpper <- as.data.frame(t(summary(covResidUpper)$coefficients))
      covResidUpper <- covResidUpper[c(1,4),] # delete Std. Error and t value
      rownames(covResidUpper) <- c("beta","pval")
      covResidUpper["beta",] <- round(covResidUpper["beta",], digits = 2)
      covResidUpper["pval",] <- round(covResidUpper["pval",], digits = 4)
      covResidUpper <- rbind(covResidUpper, col = colNotSig, sym = symNotSig)
      covResidUpper["col",] <- replace(covResidUpper["col",], covResidUpper["pval",] < .1, colMargSig)
      covResidUpper["col",] <- replace(covResidUpper["col",], covResidUpper["pval",] < .05, colSig)
      covResidUpper["sym",] <- replace(covResidUpper["sym",], covResidUpper["pval",] < .1, symMargSig)
      covResidUpper["sym",] <- replace(covResidUpper["sym",], covResidUpper["pval",] < .05, symSig)
      covResidUpper["(Intercept)"] <- NULL

      # Prepare cov upper output text
      covUpperText <- "Cov upper (resid. as DV): "
      for (i in 1:length(covUpper)) {
        covUpperText <- paste0(covUpperText, covUpper[i], " (", covResidUpper["beta",covUpper[i]], covResidUpper["sym",covUpper[i]], ")") # color does not work with mtext
        if (i < length(covUpper)) {
          covUpperText <- paste0(covUpperText,", ")
        }
      }
    }
    # Lower regression
    if (!is.null(covLower)) {
      functionCall <- paste0(paste0(critLower," ~ ", paste0(covLower, collapse = " + ")))

      if (robust == TRUE) {
        covResidLower <- eval(parse(text = paste0("robust::lmRob(",functionCall,", data = data)")))
      } else {
        covResidLower <- eval(parse(text = paste0("lm(",functionCall,", data = data)")))
      }

      if (printReg) {
        print(summary(covResidLower))
      }

      # Correct missing residual computations (= check for consecutive line numbers)
      # and replace original DV with residuals
      rowNumbReg <- as.numeric(attr(covResidLower$residuals, "names"))
      if (length(rowNumbReg) < nrow(data)) {
        rowNumbFill <- axBoost::fillVec(rowNumbReg,covResidLower$residuals)
        data[critLower] <- as.numeric(rowNumbFill[,2])
      } else {
        data[critLower] <- covResidLower$residuals
      }

      # prepare regression output for plotting (round, add col and sign info)
      covResidLower <- as.data.frame(t(summary(covResidLower)$coefficients))
      covResidLower <- covResidLower[c(1,4),] # delete Std. Error and t value
      rownames(covResidLower) <- c("beta","pval")
      covResidLower["beta",] <- round(covResidLower["beta",], digits = 2)
      covResidLower["pval",] <- round(covResidLower["pval",], digits = 4)
      covResidLower <- rbind(covResidLower, col = colNotSig, sym = symNotSig)
      covResidLower["col",] <- replace(covResidLower["col",], covResidLower["pval",] < .1, colMargSig)
      covResidLower["col",] <- replace(covResidLower["col",], covResidLower["pval",] < .05, colSig)
      covResidLower["sym",] <- replace(covResidLower["sym",], covResidLower["pval",] < .1, symMargSig)
      covResidLower["sym",] <- replace(covResidLower["sym",], covResidLower["pval",] < .05, symSig)
      covResidLower["(Intercept)"] <- NULL

      # Prepare cov lower output text
      covLowerText <- "Cov lower (resid. as DV): "
      for (i in 1:length(covLower)) {
        covLowerText <- paste0(covLowerText, covLower[i], " (", covResidLower["beta",covLower[i]], covResidLower["sym",covLower[i]], ")") # color does not work with mtext
        if (i < length(covLower)) {
          covLowerText <- paste0(covLowerText,", ")
        }
      }
    }
  }

  # Generate formula call for regressions ####
  formulaCallUpper <- paste0(critUpper, " ~ ")
  formulaCallLower <- paste0(critLower, " ~ ")

  # Include covariates, if not requested of residuals to be DV
  if (!is.null(covUpper) & covResidAsDep == FALSE) {
    formulaCallUpper <- paste0(formulaCallUpper, paste0(covUpper, collapse = " + ")," + ")
  }
  if (!is.null(covLower) & covResidAsDep == FALSE) {
    formulaCallLower <- paste0(formulaCallLower, paste0(covLower, collapse = " + ")," + ")
  }

  # Include predictor
  formulaCallUpper <- paste0(formulaCallUpper, predUpper)
  formulaCallLower <- paste0(formulaCallLower, predLower)

  # Include moderator, if available
  if (!is.null(moderator)) {
    formulaCallUpper <- paste0(formulaCallUpper, " * ", moderator)
    formulaCallLower <- paste0(formulaCallLower, " * ", moderator)
  }

  # Include other predictor
  formulaCallUpper <- paste0(formulaCallUpper, " + ", predLower)
  formulaCallLower <- paste0(formulaCallLower, " + ", predUpper)

  # Include moderator, if available and both paths should be moderated
  if ((!is.null(moderator)) & (modBothPaths == TRUE)) {
    formulaCallUpper <- paste0(formulaCallUpper, " * ", moderator)
    formulaCallLower <- paste0(formulaCallLower, " * ", moderator)
  }

  # Compute the regressions and correlations ####
  if (robust == TRUE) {
    upper <- eval(parse(text = paste0("robust::lmRob(",formulaCallUpper,", data = data)")))
    lower <- eval(parse(text = paste0("robust::lmRob(",formulaCallLower,", data = data)")))

    predCor <- WRS2::pbcor(cbind(data[[predUpper]], data[[predLower]]))
    critCor <- WRS2::pbcor(cbind(data[[critUpper]], data[[critLower]]))
  } else {
    upper <- eval(parse(text = paste0("lm(",formulaCallUpper,", data = data)")))
    lower <- eval(parse(text = paste0("lm(",formulaCallLower,", data = data)")))

    predCor <- cor.test(data[[predUpper]], data[[predLower]], use = "pairwise")
    critCor <- cor.test(data[[critUpper]], data[[critLower]], use = "pairwise")
  }

  # Print regressions and correlations, if requested ####
  if (printReg) {
    print(summary(upper))
    print(summary(lower))
    print(predCor)
    print(critCor)
  }

  # prepare regression output for plotting (round, add col and sign info)
  upper <- as.data.frame(t(summary(upper)$coefficients))
  lower <- as.data.frame(t(summary(lower)$coefficients))
  upper <- upper[c(1,4),] # delete Std. Error and t value
  lower <- lower[c(1,4),] # delete Std. Error and t value
  rownames(upper) <- c("beta","pval")
  rownames(lower) <- c("beta","pval")
  upper["beta",] <- round(upper["beta",], digits = 2)
  lower["beta",] <- round(lower["beta",], digits = 2)
  upper["pval",] <- round(upper["pval",], digits = 4)
  lower["pval",] <- round(lower["pval",], digits = 4)
  upper <- rbind(upper, col = colNotSig, sym = symNotSig)
  lower <- rbind(lower, col = colNotSig, sym = symNotSig)
  upper["col",] <- replace(upper["col",], as.numeric(upper["pval",]) < .1, colMargSig)
  upper["col",] <- replace(upper["col",], as.numeric(upper["pval",]) < .05, colSig)
  upper["sym",] <- replace(upper["sym",], as.numeric(upper["pval",]) < .1, symMargSig)
  upper["sym",] <- replace(upper["sym",], as.numeric(upper["pval",]) < .05, symSig)
  lower["col",] <- replace(lower["col",], as.numeric(lower["pval",]) < .1, colMargSig)
  lower["col",] <- replace(lower["col",], as.numeric(lower["pval",]) < .05, colSig)
  lower["sym",] <- replace(lower["sym",], as.numeric(lower["pval",]) < .1, symMargSig)
  lower["sym",] <- replace(lower["sym",], as.numeric(lower["pval",]) < .05, symSig)
  upper["(Intercept)"] <- NULL
  lower["(Intercept)"] <- NULL

  # prepare correlation output for plotting (round, add col and sign info)
  if (robust == TRUE) {
    predCor <- data.frame(predCor = c(round(predCor$cor, digits = 2),predCor$p.value, colNotSig, symNotSig), stringsAsFactors = FALSE)
    critCor <- data.frame(critCor = c(round(critCor$cor, digits = 2),critCor$p.value, colNotSig, symNotSig), stringsAsFactors = FALSE)
  } else {
    predCor <- data.frame(predCor = c(round(predCor$estimate, digits = 2),predCor$p.value, colNotSig, symNotSig), stringsAsFactors = FALSE)
    critCor <- data.frame(critCor = c(round(critCor$estimate, digits = 2),critCor$p.value, colNotSig, symNotSig), stringsAsFactors = FALSE)
  }
  rownames(predCor) <- c("r","pval","col","sym")
  rownames(critCor) <- c("r","pval","col","sym")
  predCor["col",] <- replace(predCor["col",], predCor["pval",] < .1, colMargSig)
  predCor["col",] <- replace(predCor["col",], predCor["pval",] < .05, colSig)
  predCor["sym",] <- replace(predCor["sym",], predCor["pval",] < .1, symMargSig)
  predCor["sym",] <- replace(predCor["sym",], predCor["pval",] < .05, symSig)
  critCor["col",] <- replace(critCor["col",], critCor["pval",] < .1, colMargSig)
  critCor["col",] <- replace(critCor["col",], critCor["pval",] < .05, colSig)
  critCor["sym",] <- replace(critCor["sym",], critCor["pval",] < .1, symMargSig)
  critCor["sym",] <- replace(critCor["sym",], critCor["pval",] < .05, symSig)

  ###
  # Computing is done, now plotting
  ###

  # Plot basic plot ####
  plot(NULL, main = title, type = "n", xlim = c(-10,10), ylim = c(-7,7),
       xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '')
  rect(-10, 1, -5, 4, density = 0, border = "black")
  rect(-10, -1, -5, -4, density = 0, border = "black")
  rect(10, 1, 5, 4, density = 0, border = "black")
  rect(10, -1, 5, -4, density = 0, border = "black")

  # Add variable labels
  text(5, 2.5, critUpper, col = "black", cex = .7, pos = 4) # AVoben
  text(5, -2.5, critLower, col = "black", cex = .7, pos = 4) # AVunten
  text(-10,2.5, predUpper, col = "black", cex = .7, pos = 4) # Pred1
  text(-10,-2.5, predLower, col = "black", cex = .7, pos = 4) # Pred2

  # Plot correlations (equal for all regression models)
  text(-10.3, 0, paste0(predCor["r",],predCor["sym",]), col = predCor["col",], cex = 1, pos = 4)
  text(8.3, 0, paste0(critCor["r",],critCor["sym",]), col = critCor["col",], cex = 1, pos = 4)

  # Add correlation arcs
  plotrix::draw.arc(-7.5, 0, 3, deg1 = 213, deg2 = 147, col = predCor["col",])
  plotrix::draw.arc(7.5, 0, 3, deg1 = 33, deg2 = 0, col = critCor["col",])
  plotrix::draw.arc(7.5, 0, 3, deg1 = 360, deg2 = 327, col = critCor["col",])

  # Add regression paths and colored arrows
  arrows(-5,2.5,5,2.5,col = upper["col",predUpper], angle = 10) # predUpper -> critUpper
  text(2.5, 3.5, paste0(upper["beta",predUpper],upper["sym",predUpper]), col = upper["col",predUpper], cex = 1, pos = 4) # PredUpper -> CritUpper
  arrows(-5,-2.5,5,2.5, col = upper["col",predLower], angle = 10) # preLower -> critUpper
  text(2.5, 1, paste0(upper["beta",predLower],upper["sym",predLower]), col = upper["col",predLower], cex = 1, pos = 4) # PredLower -> CritUpper
  arrows(-5,-2.5,5,-2.5, col = lower["col",predLower], angle = 10) # predLower -> critLower
  text(2.5,-3.5, paste0(lower["beta",predLower],lower["sym",predLower]), col = lower["col",predLower], cex = 1, pos = 4) # PredLower -> CritLower
  arrows(-5,2.5,5,-2.5, col = lower["col",predUpper], angle = 10) # predUpper -> critLower
  text(2.5,-1, paste0(lower["beta",predUpper],lower["sym",predUpper]), col = lower["col",predUpper], cex = 1, pos = 4) # PredUper -> CritLower

  # Add moderator paths, if available (single moderator) ####
  if (!is.null(moderator)) {
    # Add moderator boxes
    rect(-2.5, 4, 2.5, 7, density = 0, border = "black")
    rect(-2.5,-4, 2.5,-7, density = 0, border = "black")
    # Add moderator label
    text(-2.5, 5.5, moderator, col = "black", cex = .7, pos = 4) # upper moderator
    text(-2.5,-5.5, moderator, col = "black", cex = .7, pos = 4) # lower moderator
    # Add colored arrows
    arrows(2.5,5.5,7.5,4,col = upper["col",moderator], angle = 10) # moderator -> critUpper
    text(6, 5, paste0(upper["beta", moderator], upper["sym", moderator]), col = upper["col", moderator], cex = 1, pos = 4) # moderator -> critUpper
    arrows(-2,4,-2,2.5,col = upper["col", paste0(predUpper,":",moderator)], angle = 10) # moderator -> pathUpper
    text(-2, 3, paste0(upper["beta", paste0(predUpper,":",moderator)], upper["sym", paste0(predUpper,":",moderator)]),
         col = upper["col", paste0(predUpper,":",moderator)], cex = 1, pos = 4) # moderator -> pathUpper
    arrows(2.5,-5.5,7.5,-4,col = lower["col",moderator], angle = 10) # moderator -> critLower
    text(6, -5.3, paste0(lower["beta", moderator], lower["sym", moderator]), col = lower["col", moderator], cex = 1, pos = 4) # moderator -> critLower
    arrows(-2,-4,-2,-2.5,col = lower["col",paste0(predLower,":",moderator)], angle = 10) # moderator -> pathLower
    text(-2, -3.3, paste0(lower["beta", paste0(predLower,":",moderator)], lower["sym", paste0(predLower,":",moderator)]),
         col = lower["col", paste0(predLower,":",moderator)], cex = 1, pos = 4) # moderator -> pathLower

    # Add interaction diagramm if interaction is at least marg sig.
    if (as.numeric(upper["pval", paste0(predUpper,":",moderator)]) < .1) {
      rect(-6, 7.2, -3, 4.2, density = 0, border = "black")
      segments(-4.5, 4.2,-2, 3.25,lty = "dashed", col = "black")
      upperCoef <- as.numeric(upper["beta",])
      names(upperCoef) <- names(upper["beta",])
      intPoints <- axBoost::modRegGraph(betas = upperCoef, pred = predUpper, mod = moderator, crit = critUpper,
                                        modRangeL = moderatorSD[1], modRangeH = moderatorSD[2],
                                        modLabSeq = modLabSeq, plotRange = plotRange, plot = FALSE)
      # scale parameters into box; box is 3x3, see as -1SD, 1SD
      segments(-6, as.numeric(5.7 + 1.5 * intPoints["pred_L_mod_H"]), -3, as.numeric(5.7 + 1.5 * intPoints["pred_H_mod_H"]), lty = "dashed", col = "blue") # high moderator (1SD)
      segments(-6, as.numeric(5.7 + 1.5 * intPoints["pred_L_mod_L"]), -3, as.numeric(5.7 + 1.5 * intPoints["pred_H_mod_L"]), lty = "solid", col = "black") # low moderator (-1SD)
      addText <- c(addText, paste0("Int.: Dashed blue ", moderatorSD[2], "SD, solid black ", moderatorSD[1], "SD"))
    }

    if (as.numeric(lower["pval", paste0(predLower,":",moderator)]) < .1) {
      rect(-6, -7.2, -3, -4.2, density = 0, border = "black")
      segments(-4.5,-4.2,-2,-3.25,lty = "dashed", col = "black")
      lowerCoef <- as.numeric(lower["beta",])
      names(lowerCoef) <- names(lower["beta",])
      intPoints <- axBoost::modRegGraph(betas = lowerCoef, pred = predLower, mod = moderator, crit = critLower,
                                          modRangeL = moderatorSD[1], modRangeH = moderatorSD[2], plot = FALSE)
      # scale parameters into box; box is 3x3, see as -1SD, 1SD
      segments(-6, as.numeric(-5.7 + 1.5 * intPoints["pred_L_mod_H"]), -3, as.numeric(-5.7 + 1.5 * intPoints["pred_H_mod_H"]), lty = "dashed", col = "blue") # high moderator (1SD)
      segments(-6, as.numeric(-5.7 + 1.5 * intPoints["pred_L_mod_L"]), -3, as.numeric(-5.7 + 1.5 * intPoints["pred_H_mod_L"]), lty = "solid", col = "black") # low moderator (-1SD)
      addText <- c(addText, paste0("Int.: Dashed blue ", moderatorSD[2], "SD, solid black ", moderatorSD[1], "SD"))
    }
  }

  # Add moderator paths (double moderator) ####
  if ((!is.null(moderator)) && modBothPaths == TRUE) {
    arrows(-2.3,4,-2.3,-1.1,col = upper["col",paste0(moderator,":",predLower)], angle = 10) # moderator -> path2UpperfromLower
    text(-5, -1, paste0(upper["beta",paste0(moderator,":",predLower)], upper["sym",paste0(moderator,":",predLower)]),
         col =  upper["col",paste0(moderator,":",predLower)], cex = 1, pos = 4) # moderator -> path2UpperfromLower
    arrows(-2.1,-4,-2.2,1.1,col = lower["col",paste0(moderator,":",predUpper)], angle = 10) # moderator -> path2LowerfromUpper
    text(-5, 1, paste0(lower["beta",paste0(moderator,":",predUpper)], lower["sym",paste0(moderator,":",predUpper)]),
         col =  lower["col",paste0(moderator,":",predUpper)], cex = 1, pos = 4) # moderator -> path2LowerfromUpper

    if (as.numeric(upper["pval", paste0(moderator,":",predLower)]) < .1) {
      rect(-9.2, 7.2, -6.2, 4.2, density = 0, border = "black")
      segments(-7.7,4.2,-2.3,1.45,lty = "dashed", col = "black")
      upperCoef <- as.numeric(upper["beta",])
      names(upperCoef) <- names(upper["beta",])
      intPoints <- axBoost::modRegGraph(betas = upperCoef, pred = predLower, mod = moderator, crit = critUpper,
                                        modRangeL = moderatorSD[1], modRangeH = moderatorSD[2], modLabSeq = 2,
                                        plot = FALSE)
      # scale parameters into box; box is 3x3, see as -1SD, 1SD
      segments(-9.2, as.numeric(5.7 + 1.5 * intPoints["pred_L_mod_H"]), -6.2, as.numeric(5.7 + 1.5 * intPoints["pred_H_mod_H"]), lty = "dashed", col = "blue") # high moderator (1SD)
      segments(-9.2, as.numeric(5.7 + 1.5 * intPoints["pred_L_mod_L"]), -6.2, as.numeric(5.7 + 1.5 * intPoints["pred_H_mod_L"]), lty = "solid", col = "black") # low moderator (-1SD)
      addText <- c(addText, paste0("Int.: Dashed blue ", moderatorSD[2], "SD, solid black ", moderatorSD[1], "SD"))
    }

    if (as.numeric(lower["pval", paste0(moderator,":",predUpper)]) < .1) {
      rect(-9.2, -7.2, -6.2, -4.2, density = 0, border = "black")
      segments(-7.7,-4.2,-2.2,-1.45,lty = "dashed", col = "black")
      lowerCoef <- as.numeric(lower["beta",])
      names(lowerCoef) <- names(lower["beta",])
      intPoints <- axBoost::modRegGraph(betas = lowerCoef, pred = predUpper, mod = moderator, crit = critLower,
                                        modRangeL = moderatorSD[1], modRangeH = moderatorSD[2], modLabSeq = 2,
                                        plot = FALSE)
      # scale parameters into box; box is 3x3, see as -1SD, 1SD
      segments(-9.2, as.numeric(-5.7 + 1.5 * intPoints["pred_L_mod_H"]), -6.2, as.numeric(-5.7 + 1.5 * intPoints["pred_H_mod_H"]), lty = "dashed", col = "blue") # high moderator (1SD)
      segments(-9.2, as.numeric(-5.7 + 1.5 * intPoints["pred_L_mod_L"]), -6.2, as.numeric(-5.7 + 1.5 * intPoints["pred_H_mod_L"]), lty = "solid", col = "black") # low moderator (-1SD)
      addText <- c(addText, paste0("Int.: Dashed blue ", moderatorSD[2], "SD, solid black ", moderatorSD[1], "SD"))
    }
  }

  # Compute covariates (if covResidAsDep == FALSE)
  if (covResidAsDep == FALSE) {
    if (!is.null(covUpper)) {
      covUpperText <- "Cov upper (multiv. regr.): "
      for (i in 1:length(covUpper)) {
        covUpperText <- paste0(covUpperText, covUpper[i], " (", upper["beta",covUpper[i]], upper["sym",covUpper[i]], ")") # color does not work with mtext
        if (i < length(covUpper)) {
          covUpperText <- paste0(covUpperText,", ")
        }
      }
    }

    if (!is.null(covLower)) {
      covLowerText <- "Cov lower (multiv. regr.): "
      for (i in 1:length(covLower)) {
        covLowerText <- paste0(covLowerText, covLower[i], " (", lower["beta",covLower[i]], upper["sym",covLower[i]], ")") # color does not work with mtext
        if (i < length(covLower)) {
          covLowerText <- paste0(covLowerText,", ")
        }
      }
    }
  }

  # Add covariates, if available ####
  # Upper regression
  if (!is.null(covUpper)) {
    mtext(covUpperText, side = 1, line = 0, adj = 0, cex = 1)
  }
  # Lower regression
  if (!is.null(covLower)) {
    mtext(covLowerText, side = 1, line = 1, adj = 0, cex = 1)
  }

  if (robust == TRUE) {
    addText <- c(addText,"Robust regr. and cor.")
  }

  # Add sampel size
  addText <- c(addText, paste0("N = ", nrow(data)))

  if (length(addText) > 0) {
    mtext(paste0("Note: ", paste0(unique(addText), collapse = "; ")), side = 1, line = 2, adj = 0, cex = 1)
  }
}

