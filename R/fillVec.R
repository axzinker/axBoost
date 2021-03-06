#' Add missing elements to a vector of consecutive numbers
#'
#' Fill a vector, which should be of consecutive numbers, but due to missings
#' is not containing only consecutive numbers (e.g., the residuals of a lm())
#' with values (default is NA). Optionally fill a second parallel vector also
#' with replValue
#'
#' @param x A numeric vector which should contain consecutive numbers,
#' but isn't.
#' @param y An optional second parallel vector which is also filled with replValue.
#' @param replValue A numeric atomic vector which is used for replacement
#' (default is NA).
#' @param verbose Logical value, if verbose output should be given (default is
#' FALSE).
#'
#' @return Returns a numeric vector in which missing consecutive values are
#' replaced by replValue (e.g., NA).
#'
#' @author Axel Zinkernagel \email{zinkernagel@uni-landau.de}
#'
#' @examples
#' data <- c(1,2,3,4,5,6,7,8,10,11,12,13,16,17,18,20,25,30)
#' dataMod <- fillVec(data, verbose = TRUE)
#'
#' y <- rnorm(18, -1, 1)
#' dataMod2 <- fillVec(data, y, verbose = TRUE)
#'
#' @import stats
#'
#' @export
fillVec <- function(x, y = NULL, replValue = NA, verbose = FALSE) {

  result <- rle(diff(x))
  result$values <- result$values - 1
  tmpPos <- 1

  if (verbose) {
    cat(paste0("Original data: ", paste(x, collapse = " "),"\n"))
    cat(paste0("Sequence differences: ", paste(diff(x) - 1, collapse = " "),"\n"))
    print(result)
  }

  for (i in 1:length(result$lengths)) {
    if (result$values[i] == 0) {
      tmpPos <- tmpPos + result$lengths[i]
    } else {
      for (j in 1:result$lengths[i]) {
        if (verbose) {
          cat(paste0("Adding ", paste(rep(replValue, result$values[i]), collapse = " "), " after element ", x[tmpPos], "\n"))
        }
        x <- append(x, values = rep(replValue, result$values[i]), after = tmpPos)
        if (!is.null(y)) {
          y <- append(y, values = rep(replValue, result$values[i]), after = tmpPos)
        }
        tmpPos <- tmpPos + result$values[i] + 1
      }
    }
  }

  if (verbose) {
    cat(paste0("Modified data: ", paste(x, collapse = " "),"\n"))
  }

  if (!is.null(y)) {
    return(cbind(x,y))
  } else {
    return(x)
  }
}
