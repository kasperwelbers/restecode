#' Given longitude and latitude, return region id as used by ggmap
#'
#' Finds nearest nabour of lon/lat for regions in the world map data in ggmap
#'
#' @param lon longitude
#' @param lat lattitude
#' @param country If country names are available, and these match regions in the
#'                world map data, give these matches preferences over nearest neigbour matches
#' @param radius max radius in which to look
#'
#' @return A character vector with the region names as used by ggmap
#' @export
#'
#' @examples
#'
get_region_id <- function(lon, lat, country=NULL, radius=10) {
  isna = is.na(lon) | is.na(lat)
  region = rep(character(), length(lon))
  map.world <- ggplot2::map_data("world")

  closest <- RANN::nn2(map.world[,c('long','lat')], data.frame(lon=lon,lat=lat)[!isna,],
                 k = 1, searchtype = "radius", radius = radius)
  region[!isna] = map.world$region[closest$nn.idx]
  if (!is.null(country)) {
    region = ifelse(country %in% unique(map.world$region), country, region)
  }
  region
}

#' Plot values on worldmap for given regions
#'
#' @param region The region id (see get_region_id function)
#' @param value  A numeric value
#' @param scale  if TRUE, scale data so that middle is zero
#' @param sqrt   If TRUE, perform square root transformation. This is usefull for
#'               pulling back outliers that would otherwise make the color differences
#'               difficult to see
#' @return
#' @export
#'
#' @examples
plot_worldmap <- function(region, value, scale=T, sqrt=T, only_europe=F) {
  d = data.frame(region=region, value=value)
  if (scale) d$value = scale(d$value)
  if (sqrt) d$value = sign(d$value) * sqrt(abs(d$value))
  par(mar=c(0,0,0,0))

  WorldData = ggplot2::fortify(ggplot2::map_data('world'))

  if (only_europe) {
    europeanUnion <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
                       "Czech Rep.","Denmark","Estonia","Finland","France",
                       "Germany","Greece","Hungary","Ireland","Italy","Latvia",
                       "Lithuania","Luxembourg","Malta","Netherlands","Poland",
                       "Portugal","Romania","Slovakia","Slovenia","Spain",
                       "Sweden","United Kingdom")
    WorldData = WorldData[WorldData$region %in% europeanUnion,]
    xlim = c(-15,35)
    ylim = c(30,75)
  } else {
    xlim = c(-180,180)
    ylim = c(-60,90)
  }
  p <- ggplot2::ggplot() +
    ggplot2::geom_map(data = WorldData, map = WorldData,
             ggplot2::aes(x = long, y = lat, group = group, map_id=region),
             fill = "white", colour = "#7f7f7f", size=0.3, show.legend = F) +
    ggplot2::geom_map(data = d, map=WorldData,
             ggplot2::aes(fill=value, map_id=region),
             colour="#7f7f7f", size=0.2, show.legend = F) +
    ggplot2::coord_map("rectangular", lat0=0, xlim=xlim, ylim=ylim) +
    ggplot2::scale_fill_gradient2(low = 'darkblue', high = 'darkred') +
    ggplot2::scale_y_continuous(breaks=c()) +
    ggplot2::scale_x_continuous(breaks=c()) +
    ggplot2::xlab('') + ggplot2::ylab('') +
    ggplot2::theme_bw()
  p
}


