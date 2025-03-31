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


interactive_map <- function(census_data, admin0) {
  admin0 <- sf::st_transform(admin0, 4326) |> # Ensure CRS is correct
    dplyr::filter(ENDDATE == "9999/12/31 00:00:00.000") |>
    dplyr::left_join(census_data,
                     by = c("ISO_3_CODE" = "UNFPA ISO3 Codes"))
  
  #factpal <- colorFactor(topo.colors(5), admin0$`Enumeration Category`)
  factpal <- colorFactor("Paired", admin0$`Enumeration Category`)
  #qpal <- colorQuantile("Blues",  admin0$`Enumeration Category`, n = 7)
  
  admin0_unfpa <- admin0 |>
    dplyr::filter(!is.na(`UNFPA Region`), `Enumeration Category` != "No census conducted")
  
  admin0_no_census <- admin0 |>
    dplyr::filter(`Enumeration Category` == "No census conducted", !is.na(`UNFPA Region`))
  
  leaflet::leaflet() |>
    setView(lat = 11.5166646, lng = 3.8666632, zoom = 3) |>
    addTiles(group = "OSM") |>
    addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark") |>
    addProviderTiles(providers$CartoDB.Positron, group = "Light") |>
    addPolygons(
      data = admin0,
      fillColor = "#C0C0C0",
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "",
      #fillOpacity = 0.4,
      group = "Excluded",  # Required for layer toggle
      highlightOptions = highlightOptions(
        weight = 2,
        color = "#666",
        dashArray = "",
        fillOpacity = 0,
        bringToFront = TRUE
      ),
      label = ~as.character(ADM0_NAME),  # Refer directly to the column name here
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "13px",
        direction = "auto"
      )
    )  |>
    addPolygons(
      data = admin0_no_census,
      fillColor = "#666",
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "",
      fillOpacity = 1,
      group = "Excluded",  # Required for layer toggle
      highlightOptions = highlightOptions(
        weight = 2,
        color = "#666",
        dashArray = "",
        fillOpacity = 0,
        bringToFront = TRUE
      ),
      label = ~as.character(ADM0_NAME),  # Refer directly to the column name here
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "13px",
        direction = "auto"
      )
    )  |>
    addPolygons(
      data = admin0_unfpa,
      fillColor = ~factpal(`Enumeration Category`),
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "",
      fillOpacity = 1,
      group = "UNFPA Countries",  # Required for layer toggle
      highlightOptions = highlightOptions(
        weight = 2,
        color = "#666",
        dashArray = "",
        fillOpacity = 1,
        bringToFront = TRUE
      ),
      label = ~as.character(ADM0_NAME),  # Refer directly to the column name here
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "13px",
        direction = "auto"
      )
    ) |>
    addLayersControl(
      baseGroups = c("OSM", "Dark", "Light"),
      overlayGroups = c("UNFPA Countries", "Excluded"),
      options = layersControlOptions(collapsed = TRUE)
    )  |>
    addLegend(
      colors = c("#666", "#C0C0C0"),
      labels = c("No Census Conducted", "Non-UNFPA"),
      opacity = 1,
      position = "bottomleft"
    ) |>
    addLegend(pal = factpal, 
              values = admin0_unfpa$`Enumeration Category`, 
              opacity = 1, 
              position = "bottomleft",
              title = "Census Year"
              )
}

