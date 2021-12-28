#' @title Process polygon data
#' @name processPolygons
#' @description takes an dataframe of points and produces polygons
#' @param polyData  Data frame containing all the measured polygon features.
#' @param overWrite  should this rewrite any existing line files. TRUE/FALSE
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom dplyr select
#' @importFrom sf st_polygon st_sfc st_set_crs st_write
#'

processPolygons <- function(polyData, overWrite) {
  # split polygons and lines
  p1 <- polyData
  p1[nrow(p1) + 1, ] <- p1[1, ]
  # create polygon
  coords <- p1 %>%
    as.data.frame() %>%
    select("Longitude", "Latitude") %>%
    as.matrix()
  p2 <- st_polygon(
    list(coords)
  ) %>%
    st_sfc() %>%
    st_set_crs(4326)
  # export
  st_write(p2, dsn = paste0(fileLocation, "/building.geojson"), delete_dsn = overWrite)
}
