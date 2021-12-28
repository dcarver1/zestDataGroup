#' @title Generate Maps
#' @name  Produce maps
#' @description generates a map using google imagery
#' @param line : sf object containing specific lines
#' @param polygons : sf object containing specific polygons
#' @param zoomLevel : zoom level for the map
#' @param fileLocation : where output will be saved too
#' @param apiKey : key for google account
#'
#' @return
#' @export
#' @importFrom sf
#' @importFrom stringr str_split
#' @examples
produceMapsGoogle<-function(line, polygons, zoomLevel, fileLocation, apiKey){

  mapFile <- paste0(fileLocation,"/my_mapG_",zoomLevel)
  mapRdata <- paste0(fileLocation,"/my_mapG_",zoomLevel,".Rdata")
  if(file.exists(mapRdata)&file.info(mapRdata)$size>0){
    load(mapRdata)
  }else{
    # grab function
    source("src/utils/ggmap_bbox.R")
    # split the file path to grab site name
    siteName <- str_split(fileLocation, pattern = "/")[[1]][2]

    ###
    # grab spatial file
    f1 <- list.files(path = fileLocation, pattern = ".geojson",full.names = TRUE)
    # read in spatial data, condition for evaluating buildings verse lines
    b1 <- try(f1[grepl(pattern = "building", x = f1)]%>%
                st_read())
    l1 <- f1[grepl(pattern = "lines", x = f1)]%>%
      sf::st_read()

    #calcualte centroid
    c1 <- sf::st_centroid(sp1)%>%
      sf::st_bbox()
    x <- mean(c1[1],c1[3])
    y <- mean(c1[2],c1[4])

    # grab imagery for the location
    map1 <- get_map(location = c(lon = x, lat = y),  #coordiantes of the middle of the map
                    zoom = zoomLevel,
                    source = "google",#higher numbers = closer zoom in
                    maptype = "satellite")



    # Convert spatial object to projection of ggmap 3857
    sp1_3857 <- st_transform(sp1, 3857)
    bbox <- unname(st_bbox(sp1))

    # Use the function to align map
    map1a <- ggmap_bbox(map1)

    # construct palette based on the utility type
    list1 <- c("eletric","sewer","water","propane" )
    list2 <- c("yellow", "green", "blue", "red")
    sp1_3857$color <- NA
    for(i in seq_along(list1)){
      sp1_3857[sp1_3857$group == list1[i], "color"] <- list2[i]
    }
    sp1_3857


    map_final <- ggmap(map1a, extent = "device") +
      coord_sf(crs = st_crs(3857))
    for(i in seq_along(unique(sp1$group))){
      cat <- unique(sp1$group)[i]
      d1 <- sp1_3857[sp1$group == cat, ]

      map_final <- map_final +
        geom_sf(data = d1, inherit.aes = FALSE,
                color = d1$color)
    }
    # add building if it exists
    if(class(b1) == "sf"){
      map_final <- map_final +
        geom_sf(data = b1, inherit.aes = FALSE)
    }
    map_final <- map_final +
      ggtitle(paste0('Utility map of ', siteName, ' produced on ', Sys.Date()))

  }
  # save the map as an RData file so you don't have to call api secondtime
  save(map_final, file = mapRdata)
  # save as an image
  ggplot2::ggsave(filename= paste0(fileLocation,"/",mapFile,".png"),
                  plot=map_final, width=6,
                  height=6, units = "in",
                  dpi = 320)

}
