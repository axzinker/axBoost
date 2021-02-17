#' Get the (apa formatted) output of a linear (moderated) regression in a variable list
#'
#' Get the (apa formatted) output of a (moderated) regression in a variable list
#' to be used in further computations or table arrangements.
#'
#' @param data A data frame containing the numeric columns to be used in the
#' regression.
#' @param pred Character with the column name which should be used as
#' predictors of the linear regression
#' @param crit Character cointaining the column name of the criteria
#' @param cov Character vector with the column names which should be used as
#' covariates in the linear regression
#' @param mod Character with the column name which sould be used as a moderator
#' (pred * mod) in the linear regression
#' @param symMargSig Symbol to be used as indicator of marginal significance in
#' formatted (apa) output. Default is dagger
#' @param symSig Symbol to be used as indicator of significance in
#' formatted (apa) output. Default is *
#' @param sorting Character vector containing the sequence of variables which
#' should be used in the output
#' @param formatted Should the variable list contain apa formatted output
#' ("apa", e.g., rounded, no leading zero, stars and daggers of significance)
#' or unformatted ("raw") output. Default is "apa".
#'
#' @return Returns a list with variables of the regression output, either apa
#' formatted or raw to be used for further computations or table output.
#'
#' @author Axel Zinkernagel \email{zinkernagel@uni-landau.de}
#'
#' @examples
#' axFormatTableDat(axBoost::df1, pred = "IndMeasure", crit = "AutoBehav",
#' cov = c("DirMeasure"), mod = "mod", formatted = "apa")
#'
#'
#' @export
axFormatTableDat <- function(data, pred, crit, cov = NULL, mod = NULL,
                             symMargSig = "+", symSig = "*", sorting = "",
                             formatted = "apa"){

  # Define constants
  # Symbols for sig., marginal sig. and not sig.
  symNotSig <- ""
  # Scaling input variables, scale() function -> betas instead of B-values
  scale = TRUE
  center = TRUE

  # Scale input data -> full standaradized regressions, no multicolinearity in moderated regression
  data[[pred]] <- scale(data[[pred]], scale = scale, center = center)
  data[[crit]] <- scale(data[[crit]], scale = scale, center = center)
  if (!is.null(mod)) {
    data[[mod]] <- scale(data[[mod]], scale = scale, center = center)
  }
  if (!is.null(cov)) {
    for (i in 1:length(cov)) {
      data[[cov[i]]] <- scale(data[[cov[i]]], scale = scale, center = center)
    }
  }

  # Rewrite rownames of input dataframe to handle missings (N observations
  # deleted due to missingness) in the output of regression computations
  rownames(data) <- 1:nrow(data)

  # Generate formula call for regressions
  formulaCall <- paste0(crit, " ~ ")
  if (!is.null(cov)) {
    formulaCall <- paste0(formulaCall, paste0(cov, collapse = " + ")," + ")
  }
  formulaCall <- paste0(formulaCall, pred)

  # Include moderator, if available
  if (!is.null(mod)) {
    formulaCall <- paste0(formulaCall, " * ", mod)
  }

  reg <- eval(parse(text = paste0("lm(",formulaCall,", data = data)")))
  reg <- summary(reg)

  if (length(sorting) > 1) {
    sorting <- c("(Intercept)", sorting)
    reg$coefficients <- reg$coefficients[match(sorting,rownames(reg$coefficients)),]
  }

  if (formatted == "apa") {
    formula <- as.character(reg$call)[2]
    DV <- trimws(strsplit(formula,"~")[[1]][1])
    labels <- as.character(attr(reg$coefficients, "dimnames")[[1]])
    betas <- format(round(as.numeric(reg$coefficients[,1]), digits = 2), nsmall = 2)
    sigAttach <- NULL # initialize Variable
    sigAttach[1:nrow(reg$coefficients)] <- symNotSig
    sigAttach[reg$coefficients[,4] < .1] <- symMargSig
    sigAttach[reg$coefficients[,4] < .05] <- symSig
    betas <- remLead0(betas)
    betas <- paste0(betas,"",sigAttach)

    SE <- remLead0(format(round(as.numeric(reg$coefficients[,2]), digits = 2), nsmall = 2))
    t <- format(round(as.numeric(reg$coefficients[,3]), digits = 2), nsmall = 2)
    p <- remLead0(format(round(as.numeric(reg$coefficients[,4]), digits = 3), nsmall = 3))
    R2 <- remLead0(as.character(format(round(reg$r.squared[1], digits = 2), nsmall = 2)))
    F <- format(as.numeric(round(reg$fstatistic[1], digits = 2)), nsmall = 2)
    df <- as.numeric(c(reg$fstatistic[2], reg$fstatistic[3]))
    pF <- format(as.numeric(round((1 - pf(reg$fstatistic[1],reg$fstatistic[2],reg$fstatistic[3])), digits = 3)), nsmall = 3)
    sigAttach <- symNotSig
    if (pF < .1) { sigAttach <- symMargSig }
    if (pF < .05) sigAttach <- symSig
    R2 <- paste0(R2,"",sigAttach)
    pF <- remLead0(pF)
    dat <- list(formula = formula, DV = DV, labels = labels, betas = betas, SE = SE, t = t, p = p, R2 = R2, F = F, df = df, pF = pF)
  }

  if (formatted == "raw") {
    formula <- as.character(reg$call)[2]
    DV <- trimws(strsplit(formula,"~")[[1]][1])
    labels <- as.character(attr(reg$coefficients, "dimnames")[[1]])
    betas <- as.numeric(reg$coefficients[,1])
    SE <- as.numeric(reg$coefficients[,2])
    t <- as.numeric(reg$coefficients[,3])
    p <- as.numeric(reg$coefficients[,4])
    R2 <- as.numeric(reg$r.squared[1])
    F <- as.numeric(reg$fstatistic[1])
    df <- as.numeric(c(reg$fstatistic[2], reg$fstatistic[3]))
    pF <- as.numeric((1 - pf(reg$fstatistic[1],reg$fstatistic[2],reg$fstatistic[3])))
    dat <- list(formula = formula, DV = DV, labels = labels, betas = betas, SE = SE, t = t, p = p, R2 = R2, F = F, df = df, pF = pF)
  }

  return(dat)
}

remLead0 <- function(dstring) # Axel: fix me using of regular expressions
  # which may be more flexible and robust
{
  dstring <- trimws(as.character(dstring))
  for (i in 1:length(dstring)) {
    if (substring(dstring[i], 1, 1) == "-") {
      dstring[i] <- paste0("-",substr(dstring[i], 3, nchar(dstring[i])))
    } else {
      dstring[i] <- (substr(dstring[i], 2, nchar(dstring[i])))
    }
  }
  return(dstring)
}
