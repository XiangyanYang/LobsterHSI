#' Calculate Midpoints
#'
#' Computes midpoints of suitability index (SI) bins.
#'
#' use in the plot_SI_curve function for plotting
#' @export

get_midpoints <- function(bin_factor){
  bins <- as.character(bin_factor)
  splits <- strsplit(bins, "\\(|,|\\]")
  mids <- sapply(splits, function(x){
    mean(as.numeric(x[2:3]))
  })
  return(mids)
}
