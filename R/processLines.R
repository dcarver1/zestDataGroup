#' @title Process Line data
#' @name processLines
#' @description
#' @param lineData : Data frame containing all the measured line features.
#' @param overWrite : should this rewrite any existing line files. TRUE/FALSE
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom dplyr filter mutate case_when group_by summarise
#' @importFrom stringr str_replace_all str_starts
#' @importFrom sf st_cast st_write
#'



processLines <- function(lineData, overWrite) {

  # create point object from data
  sp1 <- lineData

  # remove all numeric values to get group names
  n1 <- unique(str_replace_all(sp1$Name, "[:digit:]", ""))

  # convert points to lines
  sp1$group <- "temp"
  for (i in n1) {
    sp1$group[str_starts(sp1$Name, i)] <- i
  }

  # create line group
  l1 <- filter(sp1, group %in% c("s", "w", "e", "p"))
  # convert points to lines
  l1 <- l1 %>%
    dplyr::mutate(
      group = case_when(
        str_starts(Name, "s") ~ "sewer",
        str_starts(Name, "w") ~ "water",
        str_starts(Name, "e") ~ "eletric",
        str_starts(Name, "p") ~ "gas"
      )
    ) %>%
    group_by(group) %>%
    summarise(do_union = FALSE) %>%
    st_cast("LINESTRING")
  # export
  st_write(l1, dsn = paste0(fileLocation, "/lines.geojson"), delete_dsn = overWrite)
}
