#' Plot Suitability Index (SI) Curves
#'
#' Plots SI curves for one or multiple environmental variables.
#' Currently only capable of plotting spring and fall
#' @export
#' @examples
#' plots=plot_SI_curves(surveydata = Lobster_SI_9020,
#' envVariable = c("Latitude","Longitude","Depth", "SST"),
#' int_n = 20,
#' Seasons =c("Spring","Fall"),
#' filename = "SI_curvrs.jpg")

plot_SI_curves <- function(surveydata, envVariable, Seasons,int_n,
                           filename){
  SIData=SIcurve(surveydata,
                 envVariable,
                 Seasons,
                 int_n)

  jpeg(filename, res = 600, units = "in", width = 4, height = 3*length(envVariable))
  par(mfrow=c(length(envVariable),1))

  ###
  cols <- c("blue", "red", "darkgreen", "orange","purple") #assuming there won't be more than five seasons in someone's data
  names(cols) <- Seasons

  for (var in envVariable) {
    mids_list <- list()
    SI_list   <- list()
    # extract all seasons
    for (s in Seasons) {
      bins <- SIData[[s]]$bins[[paste0(var, "_", s)]]
      mids <- get_midpoints(bins)
      SI   <- SIData[[s]]$SI[[paste0(var, "_", s)]]

      mids_list[[s]] <- mids
      SI_list[[s]]   <- SI
    }
    # fix x lim
    all_mid <- unlist(mids_list)

    plot(mids_list[[Seasons[1]]], SI_list[[Seasons[1]]],type = "l", col  = cols[Seasons[1]],
      lwd  = 3,xlab = var,ylab = "SI",main = var,
      ylim = c(0, 1),
      xlim = c(min(all_mid), max(all_mid) + 1.5))
    # other seasons
    if (length(Seasons) > 1) {
      for (s in Seasons[-1]) {
        lines(mids_list[[s]],SI_list[[s]],col = cols[s],lwd = 3)
      }
    }
    #legend
    legend("topright",legend = Seasons, col    = cols[Seasons],lwd= 3,bty    = "n")
  }
  dev.off()
}
