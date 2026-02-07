
#' Calculate Habitat Suitability Index (HSI)
#'
#' This function calculates habitat suitability indices (HSI)
#' based on survey data and environmental variables.
#'
#' @param surveydata A dataframe containing abundance and environmental data.
#' @param MapData A dataframe with spatial environmental variable information (latitude, longitude, depth, SST, etc.).
#' @param envVariable A character vector of environmental variable names.
#' @param Seasons Character vector of seasons (e.g. "Spring", "Fall").
#' @param int_n Number of intervals/bins for SI curve.
#' @param weights Numeric vector of weights for each environmental variable (must sum to 1).
#' @param locVariable Define location variable used for mapping.
#'
#' @return A dataframe containing calculated HSI values.
#' @export
#' @examples
#' data(Lobster_SI_9020)
#' data(LobsterHSI_mapData)
#' get_HSI(
#'   surveydata = Lobster_SI_9020,
#'   MapData = LobsterHSI_mapData,
#'   envVariable = c("Latitude","Longitude","Depth","SST"),
#'   Seasons = c("Spring","Fall"),
#'   int_n = 20,
#'   weights = c(0.25,0.25,0.25,0.25),
#'   locVariable=c("Longitude","Latitude")
#' )

get_HSI <- function(surveydata, MapData, envVariable, Seasons, int_n, weights,locVariable) {

  SIData=SIcurve(surveydata,
                 envVariable,
                 Seasons,
                 int_n)

  MapData_out <- MapData

  for (Season in Seasons) {
    for (var in envVariable) {

      name <- paste0(var, "_", Season)

      bins   <- SIData[[Season]]$bins[[name]]
      si     <- SIData[[Season]]$SI[[name]]
      breaks <- SIData[[Season]]$breaks[[name]]

      # change the boundaries of breaks to include all data
      breaks[1] <- min(MapData_out[[var]], na.rm = TRUE) - 1
      breaks[length(breaks)] <- max(MapData_out[[var]], na.rm = TRUE) + 1

      lookup <- data.frame(
        bin = levels(bins),
        si  = si
      )

      #create colume
      col_name <- paste0(var, "_si")
      if (!col_name %in% names(MapData_out)) {
        MapData_out[[col_name]] <- NA_real_
      }

      #subset season
      idx <- MapData_out$Season == Season
      MapData_out[[col_name]][idx] <- lookup$si[
        as.numeric(cut(MapData_out[[var]][idx], breaks = breaks, include.lowest = TRUE))
      ]
    }
  }
  MapData_out <- MapData_out[complete.cases(MapData_out), ]

  #Weighted HSI
  MapData_out$AMM_HSI <- rowSums(
    sapply(seq_along(envVariable), function(i) {
      MapData_out[[paste0(envVariable[i], "_si")]] * weights[i]
    }),
    na.rm = TRUE
  )

  MapData_out = MapData_out[,c("Season",locVariable[1], locVariable[2], "AMM_HSI")]

  return(MapData_out)
}
