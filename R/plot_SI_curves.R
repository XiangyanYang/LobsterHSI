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

  for (var in envVariable){
    spring_bins <- SIData$Spring$bins[[paste0(var,"_Spring")]]
    spring_mid  <- get_midpoints(spring_bins)
    spring_SI   <- SIData$Spring$SI[[paste0(var,"_Spring")]]

    fall_bins <- SIData$Fall$bins[[paste0(var,"_Fall")]]
    fall_mid  <- get_midpoints(fall_bins)
    fall_SI   <- SIData$Fall$SI[[paste0(var,"_Fall")]]

    plot(spring_mid, spring_SI, type="l", col="blue", lwd=3,
         xlab=var, ylab="SI", main=var,
         ylim=c(0,1),
         xlim=c(min(c(spring_mid, fall_mid)), max(c(spring_mid, fall_mid))+1.5)
    )
    lines(fall_mid, fall_SI, col="red", lwd=3)

    legend("topright", legend=c("Spring","Fall"), col=c("blue","red"),
           lwd=3, bty="n")
  }
  dev.off()
}
