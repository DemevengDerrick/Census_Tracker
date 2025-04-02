# LOAD PACKAGES -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman") # install pacman if not yet

pacman::p_load(
  tidyverse,
  DT,
  leaflet,
  treemap,
  ggplot2
)


# LOAD DATA ---------------------------------------------------------------

census_tracker_data <- read_rds("data/clean_census_data.rds")
admin0 <- sf::read_sf("data/UN_Geodata_simplified/admin0.shp")

unfpa_reg <- "Arab States"

# FUNCTIONS ---------------------------------------------------------------

# 1) Interactive Table

#' Interactive table
#'
#' @param census_data Input data to be converted into DT object
#'
#' @returns DT Object
#' @export
#'
#' @examples


interactive_table <- function(census_data){
  DT::datatable(census_data)
}

#interactive_table(census_tracker_data)

# 2) Interactive Map

#' Interactive Census Map
#'
#' @param census_data Input Census tracker file
#' @param admin0 Admin 0 boundaries
#'
#' @returns
#' @export
#'
#' @examples
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
      fillOpacity = 1,
      group = "Excluded",  # Required for layer toggle
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
    )  |>
    addPolygons(
      data = admin0_no_census,
      fillColor = "#666",
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "",
      fillOpacity = 1,
      group = "No Census Conducted",  # Required for layer toggle
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
    )  |>
    addPolygons(
      data = admin0_unfpa,
      fillColor = ~factpal(`Enumeration Category`),
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "",
      fillOpacity = 1,
      group = "Census Conducted",  # Required for layer toggle
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
      overlayGroups = c("Census Conducted", "No Census Conducted", "Excluded"),
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

stack_bar_census <- function(census_data){
  census_data <- census_data |>
    dplyr::group_by(`UNFPA Region`, `Enumeration Category`) |>
    count(name = "count") |>
    dplyr::mutate(
      `UNFPA Region` = case_when(
        `UNFPA Region` == "NA" ~ NA,
        T ~ `UNFPA Region`
      )
    ) |>
    dplyr::filter(!is.na(`UNFPA Region`))
  
  my_colors <- RColorBrewer::brewer.pal(n = 12, name = "Paired")
  
  ggplot2::ggplot(census_data, aes(fill = `Enumeration Category`, y = count, x = `UNFPA Region`)) +
    ggplot2::geom_bar(stat = "identity", position = "fill", color = "white") +
    ggplot2::scale_fill_manual(values = my_colors) +
    ggplot2::theme_void() +
    ggplot2::labs(
      title = "Census Conducted per UNFPA Region",
      xlab = "UNFPA Region",
      fill = "Census Year"
    ) +
    ggplot2::theme(
      axis.title.x = element_text(inherit.blank = F, size = 16, face = "bold"),
      plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
      axis.text.x = element_text(inherit.blank = F, angle = 45, size = 14, face = "bold")
    )
}


census_data <- census_tracker_data

census_heatmap <- function(census_data){
  census_data <- census_data |>
    dplyr::select(`UNFPA Region`, Country, `Actual Enumeration Date (Year)`, `Originally Planned Census Year`, `Actual Enumeration Date (Month)`, `Originally Planned Census Month`) |>
    dplyr::mutate(
      `UNFPA Region` = case_when(
        `UNFPA Region` == "NA" ~ NA,
        T ~ `UNFPA Region`
      ),
      `Actual Enumeration Date (Year)` = as.integer(`Actual Enumeration Date (Year)`),
      `Originally Planned Census Year` = as.integer(`Originally Planned Census Year`),
      `UNFPA Region` = factor(`UNFPA Region`, levels = rev(sort(unique(`UNFPA Region`))))
    ) |>
    dplyr::filter(!is.na(`UNFPA Region`)) #`UNFPA Region` == "West & Central Africa"
  
  df_long <- census_data %>%
    pivot_longer(
      cols = c(`Actual Enumeration Date (Year)`, `Originally Planned Census Year`),
      names_to = "Type",
      values_to = "Year"
    )
  
  ggplot2::ggplot(df_long) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.5, width = 0.5, height = 0.5, aes(x = Year, y = Country, fill = Type)) +
    ggplot2::labs(
      xlab = "Year",
      fill = "Enumeration Year"
    ) +
    ggplot2::theme(
      axis.title.x = element_text(size = 16, face = "bold"),
      axis.title.y = element_text(size = 16, face = "bold"),
      plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
      axis.text.x = element_text(inherit.blank = F, size = 14, face = "bold"),
      axis.text.y = element_text(inherit.blank = F, size = 14, face = "bold")
    ) +
    ggplot2::coord_fixed(ratio = 1) +
    ggplot2::facet_grid(rows = vars(`UNFPA Region`)) +
    ggtitle("Census Implementation by Country, Year and UNFPA Regon")
  
}
