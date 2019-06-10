#' Parcels a dataframe with item columns
#'
#' The function accepts a data frame containing items and generates parcels,
#' e.g. for Structural Equation Modelling, according to the parameters
#' specified below.
#'
#' @param data Matrix / DataFrame with items in columns. For
#' parallelizedBy =="IAT", data must be a logfile prepared for the cleanIAT()
#' function of the IAT package.
#' @param nParcels Number of parcels to build. Default is 3.
#' @param parcelName Name stem of parcel names to which the parcelnumber
#' will be added. Default is "parcel".
#' @param parallelizedBy Method to build parallel parcels. Can be "loadings",
#' "discrimination" or "IAT". Factor loadings are computed by a PAF with oblique
#' rotation. Default is "loadings". For "IAT" the data must be a logfile prepeared
#' for the use of the cleanIAT() function of the IAT package.
#' @return Returns a data frame containing the parcels.
#'
#' @author Axel Zinkernagel \email{zinkernagel@uni-landau.de}
#'
#' @examples
#' # Axel: fix me
#'
#' @import psych
#'
#' @export
parcelMe <- function(data, nParcels = 3, parcelName = "parcel",
                     parallelizedBy = "loadings"){

  # catch errors
  stopifnot(nParcels > 1) # build at least two parcels
  stopifnot(nParcels < ncol(data)) # Number of parcels must be lower than number of items
  stopifnot(is.data.frame(data))
  stopifnot(parallelizedBy == "loadings" | parallelizedBy == "discrimination" | parallelizedBy == "IAT")

  if ((ncol(data) %% nParcels) != 0) {
    warning("Unequal number of items per parcel have be generated")
  }

  if (parallelizedBy == "loadings") {
    # compute PFA
    faTemp <- psych::fa(data, fa = "pa", fm = "pa")
    # build DF containig only names and loadings
    faTemp <- data.frame(names = attr(faTemp$loadings,"dimnames")[[1]],
                         loadings = as.numeric(faTemp$loadings),
                         stringsAsFactors = FALSE)
    # order DF by loadings
    faTemp <- faTemp[order(faTemp$loadings, decreasing = TRUE),]
    # build equal parcels
    faTemp <- cbind(faTemp,items = seq(1:nrow(faTemp)), stringsAsFactors = FALSE)
    faTemp <- cbind(faTemp,parcel = (faTemp$items %% nParcels), stringsAsFactors = FALSE)
    faTemp$parcel[faTemp$parcel == 0] <- nParcels
    itemnamesPerParcel <- NULL
    for (i in 1:nParcels) {
      itemnamesPerParcel[i] <- list(subset(faTemp$names, subset = (faTemp$parcel == i)))
    }
  }

  if (parallelizedBy == "discrimination") {
    # compute item discrimination
    discTemp <- psych::alpha(data)
    # build DF containig only names and discrimination
    discTemp <- data.frame(names = attr(discTemp$keys,"names"),
                           discrim = (discTemp$item.stats$r.drop),
                           stringsAsFactors = FALSE)
    # order DF by loadings
    discTemp <- discTemp[order(discTemp$discrim, decreasing = TRUE),]
    # build equal parcels
    discTemp <- cbind(discTemp,items = seq(1:nrow(discTemp)), stringsAsFactors = FALSE)
    discTemp <- cbind(discTemp,parcel = (discTemp$items %% nParcels), stringsAsFactors = FALSE)
    discTemp$parcel[discTemp$parcel == 0] <- nParcels
    itemnamesPerParcel <- NULL
    for (i in 1:nParcels) {
      itemnamesPerParcel[i] <- list(subset(discTemp$names, subset = (discTemp$parcel == i)))
    }
  }

  if (parallelizedBy == "loadings" | parallelizedBy == "discrimination") {
    # initialize variables
    parcels <- data.frame(matrix(, nrow = nrow(data), ncol = 0))

    # attach parcel rowMeans to dataframe
    for (i in 1:nParcels) {
      if (length(unlist(itemnamesPerParcel[i])) == 1) {
        parcels <- cbind(parcels, data[,unlist(itemnamesPerParcel[i])], stringsAsFactors = FALSE)
      } else {
        parcels <- cbind(parcels, rowMeans(data[,unlist(itemnamesPerParcel[i])]), stringsAsFactors = FALSE)
      }
      tempParcelName <- paste0(parcelName,"_",i)
      names(parcels)[i] <- tempParcelName
      attr(parcels[,tempParcelName], "items") <- unlist(itemnamesPerParcel[i])
    }
  }

  if (parallelizedBy == "IAT") {
    parcels <- data.frame(matrix(, nrow = length(unique(data$subject)), ncol = 0))
    for (i in 1:nParcels) {
      IATtemp <- subset(data, subset = ((data$trialnum %% nParcels) == (i - 1)))
      parcels[i] <- IAT::cleanIAT(my_data = IATtemp,
                                block_name = "blocknum",
                                trial_blocks = c("3","4","6","7"),
                                session_id = "subject",
                                trial_latency = "latency",
                                trial_error = "correct",
                                v_error = 1,
                                v_extreme = 2,
                                v_std = 1)["IAT"]
      tempParcelName <- paste0(parcelName,"_",i)
      names(parcels)[i] <- tempParcelName
    }
  }

return(parcels)
}
