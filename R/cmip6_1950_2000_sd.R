#' Model-Averaging Decadal Standard deviation of Seafloor Climatic Data during 1950 to 2000
#'
#' Emsemble average of the decadal Standard deviation of seafloor (bottom-most) projections from 1950 to 2000.
#' @details
#' Yearly mean projections from 1950 to 2000 were calculated from each models.
#' Decadal standard deviation was calculated from the yearly mean and then averaged across all models.
#' @docType data
#' @keywords datasets
#' @format A RasterBrick object of 11 raster layers:
#' \describe{
#'   \item{epc_sd_1950_to_2000}{Export POC flux to seafloor (mg C m^-2 d^-1)}
#'   \item{o2_sd_1950_to_2000}{Dissolved oxygen concentration at seafloor (mol m^-3)}
#'   \item{ph_sd_1950_to_2000}{pH at seafloor}
#'   \item{thetao_sd_1950_to_2000}{Potential temperature at seafllor (C)}
#'   \item{arag_sd_1950_to_2000}{Aragonite Concentration (mol m-3)}
#'   \item{calc_sd_1950_to_2000}{Calcite Concentration (mol m-3)}
#'   \item{co3_sd_1950_to_2000}{Mole Concentration of Carbonate expressed as Carbon in Sea Water (mol m-3)}
#'   \item{co3satarag_sd_1950_to_2000}{Carbonate ion concentration for seawater in equilibrium with pure aragonite (mol m-3)}
#'   \item{co3satcalc_sd_1950_to_2000}{Carbonate ion concentration for seawater in equilibrium with pure calcite (mol m-3)}
#'   \item{aragsat_sd_1950_to_2000}{Aragonite Saturation State (omega)}
#'   \item{calcsat_sd_1950_to_2000}{Calcite Saturation State (omega)}
#' }
#' @source \url{https://esgf-node.llnl.gov/search/esgf-llnl/}
#' @name cmip6_1950_2000_sd
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' library(patchwork)
#' library(tidyr)
#'
#' # Convert raster to data frame and then to list
#' cmip6_list <- subset(cmip6_1950_2000_sd, 1:4) %>%
#'   as.data.frame(xy = TRUE) %>%
#'   gather(-x, -y, key = "var", value = "value") %>%
#'   group_split(var)
#'
#' # Depth
#' bathy <- etopo2022%>% as.data.frame(xy = TRUE) %>% na.omit
#'
#' # ggolot list
#' gg_list = lapply(cmip6_list, function(dat) {
#'   ggplot(dat) +
#'     geom_raster(aes(x=x, y=y, fill=value))+
#'     geom_polygon(data=arg, aes(x=X, y=Y, group=PID), fill="bisque2", colour="black")+
#'     geom_contour(data=bathy, aes(x=x, y=y, z=layer), breaks=-200, linetype=2, colour="gray50")+
#'     geom_contour(data=bathy, aes(x=x, y=y, z=layer), breaks=seq(-6000, -1000, by=1000), linetype=1, colour="gray50")+
#'     scale_fill_gradientn(colours = jet.colors(10))+
#'     scale_x_continuous(expand = expansion(mult = 0))+
#'     scale_y_continuous(expand = expansion(mult = 0))+
#'     labs(x="Longitude", y="Latitude", fill="")+
#'     coord_fixed(1.52)+
#'     facet_wrap(~ var) +
#'     theme_bw()
#'     })
#'
#' # Wrap ggplot list
#' wrap_plots(gg_list)
NULL
