# LOAD PACKAGES -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman") # install pacman if not yet

pacman::p_load(
  tidyverse,
  DT,
  leaflet
)


# LOAD DATA ---------------------------------------------------------------

census_tracker_data <- readxl::read_xlsx("data/UNFPA_global_census_2022_round.xlsx")
admin0 <- sf::read_sf("data/UN_Geodata_simplified/admin0.shp")

unfpa_reg <- "Arab States"

# FUNCTIONS ---------------------------------------------------------------

#' Title
#'
#' @param data Input data to be converted into DT object
#'
#' @returns DT Object
#' @export
#'
#' @examples


interactive_table <- function(data){
  DT::datatable(data)
}

#interactive_table(census_tracker_data)


interactive_map <- function(admin0) {
  admin0 <- sf::st_transform(admin0, 4326)  # Ensure CRS is correct
  
  leaflet::leaflet(admin0) |>
    setView(lat = 11.5166646, lng = 3.8666632, zoom = 3) |>
    addTiles(group = "OSM") |>
    addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark") |>
    addProviderTiles(providers$CartoDB.Positron, group = "Light") |>
    addPolygons(
      fillColor = "steelblue",
      weight = 1,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      group = "Countries",  # Required for layer toggle
      highlightOptions = highlightOptions(
        weight = 2,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE
      ),
      label = ~as.character(nam_en),  # Refer directly to the column name here
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "13px",
        direction = "auto"
      )
    ) |>
    addLayersControl(
      baseGroups = c("OSM", "Dark", "Light"),
      overlayGroups = c("Countries"),
      options = layersControlOptions(collapsed = TRUE)
    )
}

