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



HSImap <- function(surveydata, MapData, envVariable, Seasons, int_n, weights, filename) {

  HSI = get_HSI(surveydata, MapData,envVariable, Seasons, int_n, weights)

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
    ts <- Map_data[,c("Longitude","Latitude","AMM_HSI")]
    coordinates(ts) = ~Longitude + Latitude

    auto = autofitVariogram(AMM_HSI ~ 1, ts)
    g = gstat(formula = AMM_HSI ~ 1, model = auto$var_model, data = ts, maxdist = 0.2)

    xrange = range(ts$Longitude)
    yrange = range(ts$Latitude)
    grid = expand.grid(Longitude = seq(from = xrange[1], to = xrange[2], by = 0.01),
                       Latitude = seq(from = yrange[1], to = yrange[2], by = 0.01))
    gridded(grid) = ~Longitude + Latitude

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
