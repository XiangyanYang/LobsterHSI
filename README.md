
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LobsterHSI

<!-- badges: start -->
<!-- badges: end -->

The goal of LobsterHSI is to calculate Suitability Index (SI) values and
plot Habitat Suitability Index (HSI) maps.

## Installation

You can install the development version of LobsterHSI from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("XiangyanYang/LobsterHSI")
```

## Example

This is a basic example which shows you how to use code:

``` r
devtools::load_all()
#> ℹ Loading LobsterHSI
library(LobsterHSI)
## basic example code
data("Lobster_SI_9020")
data("data/LobsterHSI_mapData.rda")
#> Warning in data("data/LobsterHSI_mapData.rda"): data set
#> 'data/LobsterHSI_mapData.rda' not found
SIData=SIcurve(surveydata = Lobster_SI_9020, 
            envVariable = c("Latitude","Longitude","Depth", "SST"), 
            int_n = 20, 
            Seasons =c("Spring","Fall"))
#> Loading required package: nlme
#> This is mgcv 1.9-0. For overview type 'help("mgcv-package")'.

plots=plot_SI_curves(surveydata = Lobster_SI_9020,
                     envVariable = c("Latitude","Longitude","Depth", "SST"),
                     int_n = 20, 
                     Seasons =c("Spring","Fall"),
                     filename = "SI_curves.jpg")
# for project users, define months 1-6 as spring, define months 7-12 as fall
# data("LobsterHSI_monthData")
# LobsterHSI_mapData <- LobsterHSI_monthData %>%
#   dplyr::mutate(Season = case_when(
#     Month %in% c(1:6) ~ "Spring",
#     Month %in% c(7:12)   ~ "Fall",
#     TRUE               ~ NA_character_))%>%
#   dplyr::select(-Month)


HSI <- get_HSI(surveydata = Lobster_SI_9020, 
               MapData = LobsterHSI_mapData,
               envVariable = c("Latitude","Longitude","Depth", "SST"), 
               int_n = 20, 
               Seasons =c("Spring","Fall"),
               weights=c(0.25,0.25,0.25,0.25))



HSImap=HSImap(surveydata = Lobster_SI_9020, 
        MapData = LobsterHSI_mapData,
        envVariable = c("Latitude","Longitude","Depth", "SST"), 
        Seasons =c("Spring","Fall"),
        int_n = 20, 
        weights=c(0.25,0.25,0.25,0.25), 
        filename="HSIplot.jpg")
#> ### Welcome to rworldmap ###
#> For a short introduction type :   vignette('rworldmap')
#> [using ordinary kriging]
#> [using ordinary kriging]
```

example plots:

    #> Warning in data("data/LobsterHSI_mapData.rda"): data set
    #> 'data/LobsterHSI_mapData.rda' not found
    #> png 
    #>   2
    #> [using ordinary kriging]
    #> [using ordinary kriging]
    #> png 
    #>   2
