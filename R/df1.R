#' Example data frame for the axBoost package.
#'
#' This constructed data set is aimed to test double dissociations (e.g., Asendorpf,
#' Banse, and Mücke, 2002) and contains an indirect measurement (e.g., Implicit
#' Association Test, IAT, Greenwald, McGhee, & Schwartz, 1998) as
#' a predictor and an automatic behavior as corresponding criteria, as well as
#' a direct measurement as predictor and controlled behavior as criteria.
#'
#' Asendorpf, J. B., Banse, R., & Mücke, D. (2002). Double dissociation
#' between implicit and explicit personality self-concept: The case of shy
#' behavior. Journal of Personality and Social Psychology, 83(2), 380–393.
#'
#' Greenwald, A. G., McGhee, D. E., & Schwartz, J. L. K. (1998). Measuring
#' Individual Differences in Implicit Cognition: The Implicit Association Test.
#' Journal of Personality and Social Psychology, 74, 1464–1480.
#'
#'
#' @format A data frame with 20 rows and 5 variables:
#' \describe{
#'   \item{IndMeasure}{Indirect measurement, e.g., an IAT}
#'   \item{AutoBehav}{Automatic behavior, e.g., facial expression, speech
#'   illustrators}
#'   \item{DirMeasure}{Direct measurement, e.g., questionnaire data}
#'   \item{ContrBehav}{Controlled behavior, e.g., behavior based on controlled
#'   decisions}
#'   \item{mod}{Moderator, e.g., cognitive control ressources}
#' }
"df1"
