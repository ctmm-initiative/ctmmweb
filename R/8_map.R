# map ----
GRID_GROUP <- "_graticule_"
# tiles_info hold selected map layer name, here api key, which are used here and other places in server code.
init_base_maps <- function(tiles_info) {
  leaf <- leaflet::leaflet(options = leaflet::leafletOptions(
    attributionControl = FALSE))
  for (prov in tiles_info$here) {
    leaf <- leaf %>%
      leaflet::addProviderTiles(leaflet::providers[[prov]],
                                group = prov,
                                options = leaflet::providerTileOptions(
                                  detectRetina = TRUE,
                                  app_id = tiles_info$here_app_id,
                                  app_code = tiles_info$here_app_code))
  }
  for (prov in tiles_info$open) {
    leaf <- leaf %>%
      leaflet::addProviderTiles(leaflet::providers[[prov]], group = prov)
  }
  return(leaf)
}
add_measure <- function(leaf) {
  leaf %>%
    leaflet::addMeasure(
      position = "bottomright",
      primaryLengthUnit = "meters",
      secondaryLengthUnit = "kilometers",
      primaryAreaUnit = "sqmeters",
      secondaryAreaUnit = "hectares",
      activeColor = "#3D535D",
      completedColor = "#e74c3c")
}
# the layer control need to wait home range, so not added here.
add_points <- function(leaf, dt, info, id_pal) {
  leaf <- leaf %>%
    leaflet::addSimpleGraticule(interval = 1, showOriginLabel = FALSE,
                                redraw = "moveend", group = GRID_GROUP)
  # add each individual as a layer
  # for loop is better than lapply since we don't need to use <<-
  for (current_id in info$identity) {
    leaf <- leaf %>%
      leaflet::addCircles(data = dt[identity == current_id], group = current_id,
                          lng = ~longitude, lat = ~latitude, radius = 0.3, weight = 2,
                          color = ~id_pal(id), opacity = 0.4, fillOpacity = 0.05)
  }
  leaf %>%
    leaflet::addLegend(pal = id_pal, values = info$identity,
                       position = "topleft") %>%
    leaflet::addScaleBar(position = "bottomleft") %>%
    # draw with measure, but it show measure on markers
    # addDrawToolbar(targetGroup = draw_group,
    #                editOptions = editToolbarOptions(
    #                  selectedPathOptions = selectedPathOptions())) %>%
    # addMeasurePathToolbar(options =
    #                         measurePathOptions(showOnHover = FALSE,
    #                                            minPixelDistance = 100))
    # simple measure
    add_measure()
}
# check if a reactive value is valid yet
reactive_validated <- function(reactive_value) {
  res <- try(reactive_value, silent = TRUE)
  return(!("try-error" %in% class(res)))
}
add_home_range <- function(leaf, hrange, hr_levels, hr_color, group_name){
  hrange_spdf <- sp::spTransform(
    ctmm::SpatialPolygonsDataFrame.UD(hrange, level.UD = hr_levels),
    sp::CRS("+proj=longlat +datum=WGS84"))
  ML_indice <- seq(2, length(hrange_spdf), by = 3)
  hrange_spdf_ML <- hrange_spdf[ML_indice, ]
  hrange_spdf_other <- hrange_spdf[-ML_indice, ]
  leaf %>%
    leaflet::addPolygons(data = hrange_spdf_ML, weight = 2.2, opacity = 0.7,
                         fillOpacity = 0.05, color = hr_color, group = group_name) %>%
    leaflet::addPolygons(data = hrange_spdf_other, weight = 1.2, opacity = 0.4,
                         fillOpacity = 0.05, color = hr_color, group = group_name)
}
# given a map object, add layers and return the map object
add_home_range_list <- function(leaf, hrange_list, hr_levels,
                                color_list, group_vec) {
  for (i in seq_along(hrange_list)) {
    leaf <- leaf %>% add_home_range(hrange_list[[i]], hr_levels,
                                    color_list[[i]], group_vec[i])
  }
  return(leaf)
}
# take and return rgb strings. given a base color, create variations in different values, ordered from bright to dark.
vary_color <- function(base_color, count) {
  if (count == 1) {
    return(base_color)
  } else {
    hsv_vec <- grDevices::rgb2hsv(grDevices::col2rgb(base_color))[, 1]
    return(grDevices::hsv(hsv_vec[1], hsv_vec[2], seq(1, 0.5, length.out = count)))
  }
}
# base map layer control added here
add_heat <- function(leaf, dt, tiles_info) {
  leaf %>%
    leaflet::addSimpleGraticule(interval = 1, showOriginLabel = FALSE,
                                redraw = "moveend", group = GRID_GROUP) %>%
    leaflet.extras::addHeatmap(data = dt, lng = ~longitude, lat = ~latitude,
                               blur = 8, max = 1, radius = 5, group = "Heatmap") %>%
    leaflet::addScaleBar(position = "bottomleft") %>%
    add_measure() %>%
    leaflet::addLayersControl(
      baseGroups = c(tiles_info$here, tiles_info$open),
      overlayGroups = c(GRID_GROUP, "Heatmap"),
      options = leaflet::layersControlOptions(collapsed = FALSE))
}
get_bounds <- function(dt) {
  return(list(lng1 = min(dt$longitude), lat1 = min(dt$latitude),
              lng2 = max(dt$longitude), lat2 = max(dt$latitude)))
}
apply_bounds <- function(leaf, bounds) {
  leaflet::fitBounds(leaf, bounds$east, bounds$north, bounds$west, bounds$south)
}
# export map, just a wrapper around htmlwidget. the app save_map have more details.
