#' Generate Suitability Index (SI) Curve
#'
#' Generates SI curves for a given environmental variable.
#'
#'
#' @return A dataframe of SI curve bins, SI values and SI breaks.
#' @export
#' @examples
#' SIData=SIcurve(surveydata = Lobster_SI_9020,
#' envVariable = c("Latitude","Longitude","Depth", "SST"),
#' int_n = 20,
#' Seasons =c("Spring","Fall"))


SIcurve <- function(surveydata, envVariable, Seasons,int_n){
  library(classInt)
  library(mgcv)

  results <- list()

  for (Season in Seasons){
    # subset seasons
    Data_Set <- surveydata[surveydata$Season == Season, ]

    si.df.bins <- list()
    si.df.SI <- list()
    si.df.breaks <- list()

    # build SI curves
    for (i in seq_along(envVariable)){
      envData <- as.numeric(as.character(Data_Set[[envVariable[i]]]))
      envData <- na.omit(envData)

      if (length(envData) < int_n + 1){
        warning(paste("Not enough data for", envVariable[i], "in", Season, "season, skip."))
        next
      }

      envData_int <- classIntervals(envData, int_n, style = "fisher", largeN = nrow(Data_Set))
      envData_int[[2]][1] <- envData_int[[2]][1] - 0.1

      Data_Set$envData_bins <- cut(envData, breaks = envData_int$brks)

      SI <- aggregate(Data_Set$abundance ~ envData_bins, data = Data_Set, FUN = "mean")
      colnames(SI)[1] <- paste0(envVariable[i], "_bins")
      colnames(SI)[2] <- "abundance"

      SI$SI_value <- (SI$abundance - min(SI$abundance)) / (max(SI$abundance) - min(SI$abundance))
      SI$bin <- 1:nrow(SI)

      if (nrow(SI) > 3){
        g <- gam(abundance ~ s(bin), data = SI)
        SI$abundance <- predict(g, newdata = SI, type = "response")
        SI$SI_value <- (SI$abundance - min(SI$abundance)) / (max(SI$abundance) - min(SI$abundance))
      }

      #plot(SI[[1]], SI$SI_value, pch = int_n, main = paste(envVariable[i], Season))

      si.df.bins[[ paste(envVariable[i], Season, sep="_") ]] <- SI[[1]]
      si.df.SI[[   paste(envVariable[i], Season, sep="_") ]] <- SI$SI_value
      si.df.breaks[[paste(envVariable[i], Season, sep="_") ]] <-envData_int$brks
    }

    results[[Season]] <- list(bins = si.df.bins,
                              SI = si.df.SI,
                              breaks =si.df.breaks)
  }

  return(results)
}
