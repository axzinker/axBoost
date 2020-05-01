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
#' # Axel: fix me
#'
#' @import graphics
#' @import stats
#'
#' @export
drawDD <- function(data, predUpper, predLower, critUpper, critLower,
                   covUpper = NULL, covLower = NULL, covResidAsDep = FALSE,
                   moderator = NULL, moderatorSD = c(-1,1), modBothPaths = FALSE,
                   robust = FALSE, printReg = FALSE, title = ""){

  # Define constants ####
  # Colors for sig., marginal sig. and not sig. paths in the plot
  colNotSig <- "black"
  colMargSig <- "darkred"
  colSig <- "red"
  # Scaling input variables, scale() function
  scale = TRUE
  center = TRUE

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

  # Helper functions ####
  # Get p values of regression variables
  getRegPval <- function(RegObj, robust = robust) {
    if (robust == TRUE) {
      result <- (2*pt(abs(coef(RegObj) / sqrt(diag(RegObj$cov))) * -1, df = RegObj$df.residual))
    } else {
      result <- (2*pt(abs(coef(RegObj) / sqrt(diag(vcov(RegObj)))) * -1, df = RegObj$df.residual))
    }
    return(result)
  }

  # Recompute DVs as redsiduals, if requested (covResidAsDep = TRUE) ####
  if (covResidAsDep == TRUE) {

    # Upper regression
    if (!is.null(covUpper)) {
      functionCall <- paste0(paste0(critUpper," ~ ", paste0(covUpper, collapse = " + ")))

      # Mark significant betas (red, darkred, + or *)
      covUpper_p_col <- rep(colNotSig,length(covUpper) + 1) # (add intercept)
      covUpper_p_sign <- rep("",length(covUpper) + 1) # (add intercept)

      if (robust == TRUE) {
        covResidUpper <- eval(parse(text = paste0("robust::lmRob(",functionCall,", data = data)")))
      } else {
        covResidUpper <- eval(parse(text = paste0("lm(",functionCall,", data = data)")))
      }

      if (printReg) {
        print(summary(covResidUpper))
      }

      covUpper_p_col <- replace(covUpper_p_col, getRegPval(covResidUpper, robust = robust) < .1, colMargSig)
      covUpper_p_sign <- replace(covUpper_p_sign, getRegPval(covResidUpper, robust = robust) < .1, "+")
      covUpper_p_col <- replace(covUpper_p_col, getRegPval(covResidUpper, robust = robust)  < .05, colSig)
      covUpper_p_sign <- replace(covUpper_p_sign, getRegPval(covResidUpper, robust = robust)  < .05, "*")
      covCoeffUpper <- covResidUpper$coefficients[2:(length(covUpper) + 1)]

      # Correct missing residual computations (= check for consecutive line numbers)
      # and replace original DV with residuals
      rowNumbReg <- as.numeric(attr(covResidUpper$residuals, "names"))
      if (length(rowNumbReg) < nrow(data)) {
        rowNumbFill <- axBoost::fillVec(rowNumbReg,covResidUpper$residuals)
        data[critUpper] <- as.numeric(rowNumbFill[,2])
      } else {
        data[critUpper] <- covResidUpper$residuals
      }
    }
    # Lower regression
    if (!is.null(covLower)) {
      functionCall <- paste0(paste0(critLower," ~ ", paste0(covLower, collapse = " + ")))

      # Mark significant betas (red, darkred, + or *)
      covLower_p_col <- rep(colNotSig,length(covLower) + 1) # (add intercept)
      covLower_p_sign <- rep("",length(covLower) + 1) # (add intercept)

      if (robust == TRUE) {
        covResidLower <- eval(parse(text = paste0("robust::lmRob(",functionCall,", data = data)")))
      } else{
        covResidLower <- eval(parse(text = paste0("lm(",functionCall,", data = data)")))
      }

      if (printReg) {
        print(summary(covResidLower))
      }

      covLower_p_col <- replace(covLower_p_col, getRegPval(covResidLower, robust = robust) < .1, colMargSig)
      covLower_p_sign <- replace(covLower_p_sign, getRegPval(covResidLower, robust = robust) < .1, "+")
      covLower_p_col <- replace(covLower_p_col, getRegPval(covResidLower, robust = robust)  < .05, colSig)
      covLower_p_sign <- replace(covLower_p_sign, getRegPval(covResidLower, robust = robust)  < .05, "*")
      covCoeffLower <- covResidLower$coefficients[2:(length(covLower) + 1)]

      # Correct missing residual computations (= check for consecutive line numbers)
      # and replace original DV with residuals
      rowNumbReg <- as.numeric(attr(covResidLower$residuals, "names"))
      if (length(rowNumbReg) < nrow(data)) {
        rowNumbFill <- axBoost::fillVec(rowNumbReg,covResidLower$residuals)
        data[critLower] <- as.numeric(rowNumbFill[,2])
      } else {
        data[critLower] <- covResidLower$residuals
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

  # Compute covariates (if covResidAsDep == FALSE)
  if (covResidAsDep == FALSE) {
    # Mark significant betas (red, darkred, + or *)
    covUpper_p_col <- rep(colNotSig,length(upper$coefficients))
    covUpper_p_sign <- rep("",length(upper$coefficients))
    covUpper_p_col <- replace(covUpper_p_col, getRegPval(upper, robust = robust) < .1, colMargSig)
    covUpper_p_sign <- replace(covUpper_p_sign, getRegPval(upper, robust = robust) < .1, "+")
    covUpper_p_col <- replace(covUpper_p_col, getRegPval(upper, robust = robust)  < .05, colSig)
    covUpper_p_sign <- replace(covUpper_p_sign, getRegPval(upper, robust = robust)  < .05, "*")
    covCoeffUpper <- upper$coefficients[2:(length(covUpper) + 1)]

    covLower_p_col <- rep(colNotSig,length(lower$coefficients))
    covLower_p_sign <- rep("",length(lower$coefficients))
    covLower_p_col <- replace(covLower_p_col, getRegPval(lower, robust = robust) < .1, colMargSig)
    covLower_p_sign <- replace(covLower_p_sign, getRegPval(lower, robust = robust) < .1, "+")
    covLower_p_col <- replace(covLower_p_col, getRegPval(lower, robust = robust)  < .05, colSig)
    covLower_p_sign <- replace(covLower_p_sign, getRegPval(lower, robust = robust)  < .05, "*")
    covCoeffLower <- upper$coefficients[2:(length(covLower) + 1)]
  }

  # Print basic plot ####
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

  # Mark significant paths (darkred, red; + or *)
  upper_p_col <- c(colNotSig,colNotSig,colNotSig,colNotSig,colNotSig,colNotSig)
  upper_p_sign <- c("","","","","","")
  lower_p_col <- c(colNotSig,colNotSig,colNotSig,colNotSig,colNotSig,colNotSig)
  lower_p_sign <- c("","","","","","")

  if (robust == TRUE) {
    upper_p_col <- replace(upper_p_col, getRegPval(upper, robust = robust) < .1, colMargSig)
    upper_p_sign <- replace(upper_p_sign, getRegPval(upper, robust = robust) < .1, "+")
    upper_p_col <- replace(upper_p_col, getRegPval(upper, robust = robust) < .05, colSig)
    upper_p_sign <- replace(upper_p_sign, getRegPval(upper, robust = robust) < .05, "*")

    lower_p_col <- replace(lower_p_col, getRegPval(lower, robust = robust) < .1, colMargSig)
    lower_p_sign <- replace(lower_p_sign, getRegPval(lower, robust = robust) < .1, "+")
    lower_p_col <- replace(lower_p_col, getRegPval(lower, robust = robust) < .05, colSig)
    lower_p_sign <- replace(lower_p_sign, getRegPval(lower, robust = robust) < .05, "*")
  } else {
    upper_p_col <- replace(upper_p_col, getRegPval(upper, robust = robust) < .1, colMargSig)
    upper_p_sign <- replace(upper_p_sign, getRegPval(upper, robust = robust) < .1, "+")
    upper_p_col <- replace(upper_p_col, getRegPval(upper, robust = robust) < .05, colSig)
    upper_p_sign <- replace(upper_p_sign, getRegPval(upper, robust = robust) < .05, "*")

    lower_p_col <- replace(lower_p_col, getRegPval(lower, robust = robust) < .1, colMargSig)
    lower_p_sign <- replace(lower_p_sign, getRegPval(lower, robust = robust) < .1, "+")
    lower_p_col <- replace(lower_p_col, getRegPval(lower, robust = robust) < .05, colSig)
    lower_p_sign <- replace(lower_p_sign, getRegPval(lower, robust = robust) < .05, "*")
  }

  # Mark significant correlations
  predCor_p_col <- c(colNotSig)
  predCor_p_sign <- c("")
  critCor_p_col <- c(colNotSig)
  critCor_p_sign <- c("")

  predCor_p_col <- replace(predCor_p_col, predCor$p.value < .1, colMargSig)
  predCor_p_sign <- replace(predCor_p_sign, predCor$p.value < .1, "+")
  predCor_p_col <- replace(predCor_p_col, predCor$p.value < .05, colSig)
  predCor_p_sign <- replace(predCor_p_sign, predCor$p.value < .05, "*")

  critCor_p_col <- replace(critCor_p_col, critCor$p.value < .1, colMargSig)
  critCor_p_sign <- replace(critCor_p_sign, critCor$p.value < .1, "+")
  critCor_p_col <- replace(critCor_p_col, critCor$p.value < .05, colSig)
  critCor_p_sign <- replace(critCor_p_sign, critCor$p.value < .05, "*")

  # Plot correlations (equal for all regression models)
  if (robust == TRUE) {
    text(-10.3, 0, paste0(round(predCor$cor, digits = 2),predCor_p_sign), col = predCor_p_col, cex = 1, pos = 4)
    text(8.3, 0, paste0(round(critCor$cor, digits = 2),critCor_p_sign), col = critCor_p_col, cex = 1, pos = 4)
  } else {
    text(-10.3, 0, paste0(round(predCor$estimate, digits = 2),predCor_p_sign), col = predCor_p_col, cex = 1, pos = 4)
    text(8.3, 0, paste0(round(critCor$estimate, digits = 2),critCor_p_sign), col = critCor_p_col, cex = 1, pos = 4)
  }

  # Add correlation arcs
  plotrix::draw.arc(-7.5, 0, 3, deg1 = 213, deg2 = 147, col = predCor_p_col)
  plotrix::draw.arc(7.5, 0, 3, deg1 = 33, deg2 = 0, col = critCor_p_col)
  plotrix::draw.arc(7.5, 0, 3, deg1 = 360, deg2 = 327, col = critCor_p_col)

  # Add regression paths (if no moderator is present)
  if (is.null(moderator)) {
    # Add colored beta weights
    text(2.5, 1, paste0(round(coef(upper)[3], digits = 2),upper_p_sign[3]), col = upper_p_col[3], cex = 1, pos = 4) # PredLower -> CritUpper
    text(2.5, 3.5, paste0(round(coef(upper)[2], digits = 2),upper_p_sign[2]), col = upper_p_col[2], cex = 1, pos = 4) # PredUpper -> CritUpper
    text(2.5,-1, paste0(round(coef(lower)[3], digits = 2),lower_p_sign[3]), col = lower_p_col[3], cex = 1, pos = 4) # PredUper -> CritLower
    text(2.5,-3.5, paste0(round(coef(lower)[2], digits = 2),lower_p_sign[2]), col = lower_p_col[2], cex = 1, pos = 4) # PredLower -> CritLower

    # Add colored arrows
    arrows(-5,2.5,5,2.5,col = upper_p_col[2], angle = 10) # predUpper -> critUpper
    arrows(-5,-2.5,5,2.5, col = upper_p_col[3], angle = 10) # preLower -> critUpper
    arrows(-5,-2.5,5,-2.5, col = lower_p_col[2], angle = 10) # predLower -> critLower
    arrows(-5,2.5,5,-2.5, col = lower_p_col[3], angle = 10) # predUpper -> critLower
  }

  # Add moderator paths (single moderator) ####
  if ((!is.null(moderator)) && modBothPaths == FALSE) {
    # Add colored beta weights
    text(2.5, 1, paste0(round(coef(upper)[4], digits = 2),upper_p_sign[4]), col = upper_p_col[4], cex = 1, pos = 4) # PredLower -> CritUpper
    text(2.5, 3.5, paste0(round(coef(upper)[2], digits = 2),upper_p_sign[2]), col = upper_p_col[2], cex = 1, pos = 4) # PredUpper -> CritUpper
    text(2.5,-1, paste0(round(coef(lower)[4], digits = 2),lower_p_sign[4]), col = lower_p_col[4], cex = 1, pos = 4) # PredUper -> CritLower
    text(2.5,-3.5, paste0(round(coef(lower)[2], digits = 2),lower_p_sign[2]), col = lower_p_col[2], cex = 1, pos = 4) # PredLower -> CritLower

    # Add colored arrows
    arrows(-5,2.5,5,2.5,col = upper_p_col[2], angle = 10) # predUpper -> critUpper
    arrows(-5,-2.5,5,2.5, col = upper_p_col[4], angle = 10) # preLower -> critUpper
    arrows(-5,-2.5,5,-2.5, col = lower_p_col[2], angle = 10) # predLower -> critLower
    arrows(-5,2.5,5,-2.5, col = lower_p_col[4], angle = 10) # predUpper -> critLower

    # Add moderator boxes
    rect(-2.5, 4, 2.5, 7, density = 0, border = "black")
    rect(-2.5,-4, 2.5,-7, density = 0, border = "black")
    # Add moderator label
    text(-2.5, 5.5, moderator, col = "black", cex = .7, pos = 4) # upper moderator
    text(-2.5,-5.5, moderator, col = "black", cex = .7, pos = 4) # lower moderator
    # Add colored arrows
    arrows(2.5,5.5,7.5,4,col = upper_p_col[3], angle = 10) # moderator -> critUpper
    arrows(-2,4,-2,2.5,col = upper_p_col[5], angle = 10) # moderator -> pathUpper
    arrows(2.5,-5.5,7.5,-4,col = lower_p_col[3], angle = 10) # moderator -> critLower
    arrows(-2,-4,-2,-2.5,col = lower_p_col[5], angle = 10) # moderator -> pathLower
    # Add colored beta weights
    text(6, 5, paste0(round(coef(upper)[3], digits = 2),upper_p_sign[3]), col = upper_p_col[3], cex = 1, pos = 4) # moderator -> critUpper
    text(-2, 3, paste0(round(coef(upper)[5], digits = 2),upper_p_sign[5]), col = upper_p_col[5], cex = 1, pos = 4) # moderator -> critUpper
    text(6, -5.3, paste0(round(coef(lower)[3], digits = 2),lower_p_sign[3]), col = lower_p_col[3], cex = 1, pos = 4) # moderator -> critUpper
    text(-2, -3.3, paste0(round(coef(lower)[5], digits = 2),lower_p_sign[5]), col = lower_p_col[5], cex = 1, pos = 4) # moderator -> critUpper
  }

  # Add moderator paths (double moderator) ####
  if ((!is.null(moderator)) && modBothPaths == TRUE) {
    # Add colored beta weights
    text(2.5, 1, paste0(round(coef(upper)[4], digits = 2),upper_p_sign[4]), col = upper_p_col[4], cex = 1, pos = 4) # PredLower -> CritUpper
    text(2.5, 3.5, paste0(round(coef(upper)[2], digits = 2),upper_p_sign[2]), col = upper_p_col[2], cex = 1, pos = 4) # PredUpper -> CritUpper
    text(2.5,-1, paste0(round(coef(lower)[4], digits = 2),lower_p_sign[4]), col = lower_p_col[4], cex = 1, pos = 4) # PredUper -> CritLower
    text(2.5,-3.5, paste0(round(coef(lower)[2], digits = 2),lower_p_sign[2]), col = lower_p_col[2], cex = 1, pos = 4) # PredLower -> CritLower

    # Add colored arrows
    arrows(-5,2.5,5,2.5,col = upper_p_col[2], angle = 10) # predUpper -> critUpper
    arrows(-5,-2.5,5,2.5, col = upper_p_col[4], angle = 10) # preLower -> critUpper
    arrows(-5,-2.5,5,-2.5, col = lower_p_col[2], angle = 10) # predLower -> critLower
    arrows(-5,2.5,5,-2.5, col = lower_p_col[4], angle = 10) # predUpper -> critLower

    # Add moderator boxes
    rect(-2.5, 4, 2.5, 7, density = 0, border = "black")
    rect(-2.5,-4, 2.5,-7, density = 0, border = "black")
    # Add moderator label
    text(-2.5, 5.5, moderator, col = "black", cex = .7, pos = 4) # upper moderator
    text(-2.5,-5.5, moderator, col = "black", cex = .7, pos = 4) # lower moderator
    # Add colored arrows
    arrows(2.5,5.5,7.5,4,col = upper_p_col[3], angle = 10) # moderator -> critUpper
    arrows(-2,4,-2,2.5,col = upper_p_col[5], angle = 10) # moderator -> pathUpper
    arrows(2.5,-5.5,7.5,-4,col = lower_p_col[3], angle = 10) # moderator -> critLower
    arrows(-2,-4,-2,-2.5,col = lower_p_col[5], angle = 10) # moderator -> pathLower
    # Add colored beta weights
    text(6, 5, paste0(round(coef(upper)[3], digits = 2),upper_p_sign[3]), col = upper_p_col[3], cex = 1, pos = 4) # moderator -> critUpper
    text(-2, 3, paste0(round(coef(upper)[5], digits = 2),upper_p_sign[5]), col = upper_p_col[5], cex = 1, pos = 4) # moderator -> critUpper
    text(6, -5.3, paste0(round(coef(lower)[3], digits = 2),lower_p_sign[3]), col = lower_p_col[3], cex = 1, pos = 4) # moderator -> critUpper
    text(-2, -3.3, paste0(round(coef(lower)[5], digits = 2),lower_p_sign[5]), col = lower_p_col[5], cex = 1, pos = 4) # moderator -> critUpper
    # Add second moderator path
    arrows(-2.3,4,-2.3,-1.1,col = upper_p_col[6], angle = 10) # moderator -> path2Upper
    arrows(-2.1,-4,-2.2,1.1,col = lower_p_col[6], angle = 10) # moderator -> path2Lower
    text(-2.3, 1.5, paste0(round(coef(upper)[6], digits = 2),upper_p_sign[6]), col = upper_p_col[6], cex = 1, pos = 4) # moderator -> critUpper
    text(-2.1, -1.5, paste0(round(coef(lower)[6], digits = 2),lower_p_sign[6]), col = lower_p_col[6], cex = 1, pos = 4) # moderator -> critUpper
  }

  # Add covariates, if available ####
  # Upper regression
  if (!is.null(covUpper)) {
    covUpperText <- "Cov upper"
    if (covResidAsDep == TRUE) {
      covUpperText <- paste0(covUpperText," (Resid. as DV): ")
    } else {
      covUpperText <- paste0(covUpperText," (multiv. Reg.): ")
    }
    for (i in 1:length(covUpper)) {
      covUpperText <- paste0(covUpperText, covUpper[i], " (",
                             paste0(round(as.numeric(covCoeffUpper[i]),
                                          digits = 2), covUpper_p_sign[i + 1]),")") # , col = covUpper_p_col[i + 1] # color only within text() function
      if (i < length(covUpper)) {
        covUpperText <- paste0(covUpperText,", ")
      }
    }
    mtext(covUpperText, side = 1, line = 0, adj = 0, cex = 1)
  }
  # Lower regression
  if (!is.null(covLower)) {
    covLowerText <- "Cov lower"
    if (covResidAsDep == TRUE) {
      covLowerText <- paste0(covLowerText," (Resid. as DV): ")
    } else {
      covLowerText <- paste0(covLowerText," (multiv. Reg.): ")
    }
    for (i in 1:length(covLower)) {
      covLowerText <- paste0(covLowerText, covLower[i], " (",
                             paste0(round(as.numeric(covCoeffLower[i]),
                                          digits = 2), covLower_p_sign[i + 1]),")") # , col = covLower_p_col[i + 1] # color only within text() function
      if (i < length(covLower)) {
        covLowerText <- paste0(covLowerText,", ")
      }
    }
    mtext(covLowerText, side = 1, line = 1, adj = 0, cex = 1)
  }
}

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
#


