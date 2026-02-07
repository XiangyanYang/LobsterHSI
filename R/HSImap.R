#' Plot HSI Map
#'
#' Creates spatial maps of habitat suitability indices.
#'
#' @param filename save the plotted HSI map with defined name.
#' @export
#' @examples
#' HSImap=HSImap(surveydata = Lobster_SI_9020,
#' MapData = LobsterHSI_mapData,
#' envVariable = c("Latitude","Longitude","Depth", "SST"),
#' Seasons =c("Spring","Fall"),
#' int_n = 20,
#' weights=c(0.25,0.25,0.25,0.25),
#' filename="HSIplot.jpg")
#'
#' for project users, define months 1-6 as spring, define months 7-12 as fall
#' LobsterHSI_mapData <- LobsterHSI_monthData %>%
#'    dplyr::mutate(Season = case_when(
#'      Month %in% c(1:6) ~ "Spring",
#'      Month %in% c(7:12)   ~ "Fall",
#'      TRUE               ~ NA_character_))%>%
#'    dplyr::select(-Month)



HSImap <- function(surveydata, MapData, envVariable, Seasons, int_n, weights,locVariable, filename) {

  HSI = get_HSI(surveydata, MapData,envVariable, Seasons, int_n, weights,locVariable)

  library(sp)
  library(gstat)
  library(rworldmap)
  library(rworldxtra)
  library(colorRamps)
  library(gridExtra); data("countriesHigh")
  library(automap)


  map_list <- list()

  for (Season in Seasons) {
    Map_data <- HSI[HSI$Season == Season, ]
    ts <- Map_data[, c(locVariable[1], locVariable[2], "AMM_HSI")]
    # remove na
    ts <- ts[stats::complete.cases(ts[, c(locVariable[1], locVariable[2], "AMM_HSI")]), ]
    if (nrow(ts) < 2) {
      stop("Not enough valid points (after removing NA) to build variogram/grid for this Season.")
    }
    coordinates(ts) <- stats::as.formula(paste("~", locVariable[1], "+", locVariable[2]))

    auto = autofitVariogram(AMM_HSI ~ 1, ts)
    g = gstat(formula = AMM_HSI ~ 1, model = auto$var_model, data = ts, maxdist = 0.2)

    xy <- sp::coordinates(ts)
    xrange <- range(xy[, 1], na.rm = TRUE)
    yrange <- range(xy[, 2], na.rm = TRUE)
    grid <- expand.grid(x = seq(from = xrange[1], to = xrange[2], by = 0.01),
                        y = seq(from = yrange[1], to = yrange[2], by = 0.01))
    names(grid) <- c(locVariable[1], locVariable[2])
    sp::gridded(grid) <- stats::as.formula(paste("~", locVariable[1], "+", locVariable[2]))

    p = predict(g, newdata = grid)
    proj4string(p) <- "+proj=longlat"

    col = colorRamps::matlab.like(100)
    map_list[[Season]] <- spplot(
      p,
      sp.layout = list(list("sp.polygons", countriesHigh, first = FALSE, lwd = 0.1, fill="grey")),
      at = (0:100)/100, # HSI scale 0-1
      par.settings = list(fontsize = list(text = 12)),
      col.regions = col,
      zcol = "var1.pred",
      main = list(label=Season),
      scales=list(draw=TRUE),
      xlab="", ylab="", colorkey=TRUE
    )
  }

  jpeg(filename, res = 300, units = "in", width = 6*length(Seasons), height = 6)
  grid.arrange(grobs = map_list, ncol = length(Seasons))
  dev.off()

}
