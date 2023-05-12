#' Get grocery stores from all of Utah
#' 
#' @param grocery_sourcedata Path to file of all grocery stores
#' @param nems_groceries 
#' 
get_all_groceries <- function(grocery_sourcedata, nems_groceries, bg_acs, this_crs) {
  
  # read in the file and adjust the variables ========================
  groceries <- sf::st_read(grocery_sourcedata) |> 
    sf::st_transform(this_crs) |> 
    dplyr::select(id = OBJECTID, Name = NAME, type = TYPE, ethnic) |> 
    dplyr::mutate(
      id = as.character(id),
      pharmacy = NA,
      ethnic = ifelse(grepl("Specialty", type) | !is.na(ethnic), TRUE, FALSE),
      type = dplyr::case_when(
        Name %in% c("DOLLAR TREE", "FAMILY DOLLAR") ~ "Dollar Store",
        type %in%  c("Health Food", "Supermarket", "Retail") ~ "Grocery Store",
        type == "Convenience Grocery" ~ "Convenience",
        type %in% c("Salvage Grocery", "Specialty Grocery") ~ "Other",
        TRUE ~ type,
      ),
      merch = NA, 
      registers = NA,
      selfchecko = NA,
      total_registers = NA,
      availability = NA,
      cost = NA,
      market = NA, 
      brand = dplyr::case_when(
        grepl("DOLLAR TREE", Name) ~ "Dollar Tree",
        grepl("FAMILY DOLLAR", Name) ~ "Family Dollar",
        grepl("WALMART|WAL-MART", Name) ~ "Walmart",
        grepl("MACEY", Name) ~ "Macey's",
        grepl("HARMON", Name) ~ "Harmons",
        grepl("SMITH", Name) ~ "Smith's",
        grepl("TARGET", Name)~  "Target",
        grepl("WINCO", Name) ~ "Winco",
        TRUE ~ "Other"
      )
    )
  
  # get county of grocery stores
  counties <- tigris::counties("utah") |> 
    sf::st_transform(this_crs) |> 
    dplyr::transmute(
      county = NAME
    )
    
  groceries <- groceries |> 
    sf::st_join(counties)
  
  # exclude groceries that we surveyed ====================
  buf <- nems_groceries |> 
    sf::st_buffer(dist = 500) 
  
  discard <- sapply(sf::st_within(groceries, buf), function(x) length(x) > 0)
  groceries <- groceries[!discard, ]
  
  # reconcile and bind ===================
  bind_rows(
    nems_groceries |> 
      dplyr::mutate(
        type = dplyr::case_when(
          type == "Convenience Store" ~ "Convenience",
          type == "Dollar Store" ~ "Dollar Store",
          type == "Grocery Store" ~ "Grocery Store", 
          TRUE ~ "Other"
        )
      ),
    groceries
  ) |> 
    sf::st_transform(4326)
  
}



#' Get grocery stores with NEMS data
#' 
#' @param nems_utah
#' @param nems_saltlake
#' @param nems_sanjuan
#' 
get_nems_groceries <- function(nems_saltlake, nems_sanjuan, nems_utah, this_crs) {
  
  nems <- dplyr::bind_rows(
    list(
      "Salt Lake" = readr::read_csv(nems_saltlake),
      "San Juan"  = readr::read_csv(nems_sanjuan),
      "Utah"      = readr::read_csv(nems_utah)
    ),
    .id = "county"
  )  |> 
    select(-Latitude, -Longitude)
  
  
  geojson <- lapply(
    list("Salt Lake" = "data/groceries_saltlake.geojson", 
         "San Juan"  = "data/groceries_sanjuan.geojson",
         "Utah" = "data/groceries_utah.geojson"),
    function(x) sf::st_read(x) 
  ) |> 
    dplyr::bind_rows() |> 
    dplyr::filter(!is.na(ID_1)) |> 
    dplyr::select(id = ID_1)
  
  
  dplyr::left_join(nems, geojson, by = "id") |> 
    sf::st_as_sf() |> 
    sf::st_transform(this_crs) |> 
    sf::st_make_valid() |> 
    sf::st_centroid()
}


#' Get American Community Survey data for the study.
#' 
#' @param state Which state to pull for
#' @param county Which county(ies) to pull
#' 
get_acsdata <- function(bgcentroids) {
  
  # the list of variables to get from the ACS data
  variables <- c(
    "population" = "B02001_001", # TOTAL: RACE
    "housing_units" = "B25001_001", # HOUSING UNITS
    "households" = "B19001_001", #HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2017 INFLATION-ADJUSTED DOLLARS)
    # Hispanic or Latino Origin by Race
    "white" = "B03002_003",
    "black" = "B03002_004",
    "asian" = "B03002_006",
    "hispanic" = "B03002_012",
    #MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2017 INFLATION-ADJUSTED DOLLARS)
    "income" = "B19013_001",
    # FAMILY TYPE BY PRESENCE AND AGE OF RELATED CHILDREN
    "children_c06" = "B11004_004", # married under 6 only
    "children_c6+" = "B11004_005", # married under 6 and older
    "children_m06" = "B11004_011", # male under 6 only
    "children_m6+" = "B11004_012", # male under 6 and older
    "children_f06" = "B11004_017", # female under 6 only
    "children_f6+" = "B11004_018", # female under 6 and older
    #HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2017 INFLATION-ADJUSTED DOLLARS)
    "inc_0010" = "B19001_002",  "inc_1015" = "B19001_003", "inc_1520" = "B19001_004",
    "inc_2025" = "B19001_005", "inc_2530" = "B19001_006", "inc_3035" = "B19001_007",
    "inc_125"  = "B19001_015", "inc_150"  = "B19001_016", "inc_200"  = "B19001_017"
  )
  
  states <- unique(substr(bgcentroids$id, 1, 2))
  counties <- unique(substr(bgcentroids$id, 3, 5))
  
  tidycensus::get_acs(
    geography = "block group", 
    variables = variables, 
    geometry = TRUE,
    state = states,
    county = counties
    
  ) |>
    dplyr::select(-moe) |>
    tidyr::spread(variable, estimate) |>
    # area is in m^2, change to km^2
    dplyr::mutate(area = as.numeric(st_area(geometry) * 1e-6)) |>
    dplyr::transmute(
      geoid = GEOID,
      group = 1,
      population, households, housing_units, 
      density = households / area,
      income,
      # many of the variables come in raw counts, but we want to consider
      # them as shares of a relevant denominator.
      children = 100 * ( children_c06 + `children_c6+` + 
                           children_m06 + `children_m6+` + 
                           children_f06 + `children_f6+`) / households,
      lowincome    = 100 * (inc_0010 + inc_1015 + inc_1520 + inc_2530 +
                              inc_3035) / households,
      highincome   = 100 * (inc_125 + inc_150 + inc_200) / households,
      black        = 100 * black / population,
      asian        = 100 * asian / population,
      hispanic     = 100 * hispanic / population,
      white        = 100 * white / population
    ) |>
    dplyr::filter(population > 0) |>
    sf::st_set_geometry(NULL) |>
    tibble::as_tibble()
}


#' Get block group centroids
#' 
#' @details Gets the population-weighted block group centroids
#' for all block groups in Utah.
#' 
get_bgcentroids <- function(){
  bgcentroid <- readr::read_csv("https://www2.census.gov/geo/docs/reference/cenpop2020/blkgrp/CenPop2020_Mean_BG49.txt")
  
  sf::st_as_sf(bgcentroid, coords = c("LONGITUDE", "LATITUDE"), crs = 4326) |> 
    dplyr::mutate(id = stringr::str_c(STATEFP, COUNTYFP, TRACTCE, BLKGRPCE)) |> 
    dplyr::select(id, POPULATION)
}