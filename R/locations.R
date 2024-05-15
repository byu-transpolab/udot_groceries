#' Impute store data
#' 
#' @param all_groceries grocery stores
#' @param bgcentroids population-weighted block group centroids
#' @param bg_acs ACS data for block groups
#' 
#' 
impute_store_data <- function(all_groceries, bgcentroids, bg_acs) {
  
  
  # make a coherent block group matcher ==============
  bg <- left_join(bgcentroids, bg_acs, by = c("id" = "geoid")) |> 
    filter(population > 0, !is.na(population))
  
  # compute statistics for knn data list =====================
  knn <- nngeo::st_nn( all_groceries, bg, k = 9, returnDist = TRUE, 
                       progress = FALSE)
  
  neighborstats  <- lapply(seq_along(knn$nn), function(i){
    bgs <- knn$nn[[i]]
    dists <- knn$dist[[i]] 
    
    # get block groups within the range
    bg[bgs,] |> 
      sf::st_set_geometry(NULL) |> 
      dplyr::summarise(
        population = sum(population / (dists * 0.1)),
        households = sum(households / (dists * 0.1)),
        density = weighted.mean(density, w = 1 / dists, na.rm = TRUE),
        income = weighted.mean(income, w = 1 / dists, na.rm = TRUE)
      ) 
  }) |> 
    dplyr::bind_rows()
  
  knn_groceries <- dplyr::bind_cols(all_groceries, neighborstats) |> 
      dplyr::select(id, county, type, pharmacy:brand, population:income) |> 
      sf::st_set_geometry(NULL)
  
  
  # impute missing data ==================
  imp <- mice::mice(knn_groceries, maxit = 30, printFlag = FALSE, m = 10)
  
  imp
}


#' Make new store locations
#' 
#' @param nems_groceries attribute data
#' 
make_new_stores <- function(nems_groceries){
  
  # get some average values
  new_attrs <- nems_groceries |> 
    filter(type == "Grocery Store") |> 
    filter(county == "Salt Lake") |> 
    ungroup() |> 
    st_set_geometry(NULL) |> 
    summarise(
      .by = county,
      type = "Grocery Store",
      pharmacy = TRUE,
      ethnic = FALSE,
      merch = FALSE,
      total_registers = mean(total_registers),
      availability = quantile(availability, probs = 0.75),
      cost = quantile(cost, probs = 0.75),
      market = quantile(market, probs = 0.25)
    )
  
  
  # location of new stores
  tibble(
    id = str_c("new-", nrow(new_attrs)),
    county = new_attrs$county,
    lon = c(-111.95852668416485),
    lat = c(40.68272535460591)
  ) |> 
    left_join(new_attrs, by = c("county")) |> 
    st_as_sf(coords = c("lon", "lat"), crs = 4326)
}

#' Make improved stores 
#' 
make_improved_stores <- function(nems_groceries){
  
  # get the stores 
  ng_pulled <- nems_groceries |> 
    filter(
      id %in% c("SL-013", "UT-002", "SJ-006")
    )
  
  # get some average values
  new_attrs <- nems_groceries |> 
    ungroup() |> 
    st_set_geometry(NULL) |> 
    summarise(
      type = "Grocery Store",
      pharmacy = TRUE,
      ethnic = FALSE,
      merch = FALSE,
      total_registers = mean(total_registers),
      availability = quantile(availability, probs = 0.85),
      cost = quantile(cost, probs = 0.85),
      market = quantile(market, probs = 0.15),
      .by = county
    )
  
  # make sure these values are better than the old ones
  new_attrs$total_registers = pmax(new_attrs$total_registers, ng_pulled$total_registers)
  
  
  # remove the three stores from the database
  ng_removed <- nems_groceries |> 
    filter(
      !id %in% c("SL-013", "UT-002", "SJ-006")
    )
  
  # put the new attributes back into the data frame
  nems_groceries |> 
    filter(
      id %in% c("SL-013", "UT-002", "SJ-006")
    ) |> 
    select(id, county, Name, brand) |> 
    left_join(new_attrs, by = "county") 
  
  
  
}

#' Make stores with increased costs but that will have 
#' no cost of travel
#' 
make_delivery_stores <- function(nems_groceries){
  nems_groceries |> 
    ungroup() |> 
    
    # get 30% of full-service grocery stores
    filter(type == "Grocery Store") |> 
    filter(county == "Salt Lake") |> 
    filter(!ethnic) |> 
    sample_frac(0.3) |> 
    
    
    # add 10 dollars to the cost of the market basket
    mutate(
      market = market + 10
    )
}


#' Get WV block groups
#' 
#' @param bgcentroids
#' @param wvfile
#' 
get_wv_bgs <- function(bgcentroids, wvfile){
  shp <- st_read(wvfile, crs = 4326)
  
  bgcentroids |> 
    st_crop(shp) |> 
    pull(id)
  
}

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
    sf::st_transform(this_crs) |> 
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
#' @param nems_list A list of NEMS files
#' 
#' 
get_nems_groceries <- function(nems_list, brands, this_crs) {
  
  # get raw data from Qualtrics surveys and do NEMS-r stuff to it
  nems <- lapply(nems_list, function(n) {
    stores <- nemsr::read_nemss(n) 
    nems <- nemsr::calculate_nems_score(stores)
    mkt <- nemsr::calculate_market_basket(stores)
    
    stores |> 
      transmute(
        id = STORE_ID, 
        type = store_type,
        pharmacy = as.logical(pharmacy), 
        ethnic = as.logical(ethnic), 
        merch = as.logical(merch), 
        total_registers = as.numeric(register) + as.numeric(self_checkout),
        availability = nems$Total_Availability_Score,
        cost = nems$Total_Cost_Score,
        market = mkt$total
      )
  }) |> 
    dplyr::bind_rows() |> 
    dplyr::group_by(id) |> 
    dplyr::slice(1) |> 
    dplyr::mutate(
      county = dplyr::case_when(
        substr(id, 1, 2) == "SL" ~ "Salt Lake",
        substr(id, 1, 2) == "UT" ~ "Utah",
        substr(id, 1, 2) == "SJ" ~ "San Juan"
      )
    )
    
  # brands were stored separately  
  brands_df <- readr::read_csv(brands)
  
  nems <- dplyr::left_join(nems, brands_df, by = "id")
  
  geojson <- lapply(
    list("Salt Lake" = "data/groceries_saltlake.geojson", 
         "San Juan"  = "data/groceries_sanjuan.geojson",
         "Utah" = "data/groceries_utah.geojson"),
    function(x) sf::st_read(x) 
  ) |> 
    dplyr::bind_rows() |> 
    dplyr::filter(sf::st_geometry_type(geometry) == "MULTIPOLYGON") |> 
    dplyr::select(id = ID_1) 
  
  geojson <- geojson[!duplicated(geojson),]
  
  dplyr::inner_join(nems, geojson, by = "id") |> 
    sf::st_as_sf() |> 
    sf::st_transform(this_crs) |> 
    sf::st_make_valid() |> 
    sf::st_centroid()
}

#' Get the ACS data for block groups in the vicinity of NEMS-S collected stores
#' 
#' @param bg_acs
#' @param nems_groceries
#' 
get_neighbor_acs <- function(bg_acs, bgcentroids, nems_groceries){
  a <- nems_groceries |> 
    sf::st_buffer(1.5 * 5280) |> 
    dplyr::group_by(county) |> 
    dplyr::summarise()
  
  b <- bgcentroids |> 
    st_transform(st_crs(a)) |> 
    st_join(a) |> 
    filter(!is.na(county))
  
  inner_join(bg_acs, 
             b |> select(geoid = id, county) |> st_set_geometry(NULL))
  
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
  
  # HOUSEHOLD SIZE BY VEHICLES AVAILABLE at tract level
  tv_acs <- tidycensus::get_acs(
    geography = "tract", variables = c("zero_vehicle" = "B08201_002", "households" = "B19001_001" ),
    geometry = TRUE, state = states, year = 2019, county = counties) |> 
    dplyr::select(-moe) |>
    tidyr::spread(variable, estimate) |> 
    dplyr::transmute(
      tract = GEOID, zero_vehicle = 100 * zero_vehicle/households
    ) |>    
    sf::st_set_geometry(NULL) 
  
  
  bg_acs <- tidycensus::get_acs(
    geography = "block group", 
    variables = variables, 
    geometry = TRUE,
    state = states,
    year = 2019,
    county = counties
    
  ) |>
    dplyr::select(-moe) |>
    tidyr::spread(variable, estimate) |>
    # area is in m^2, change to km^2
    dplyr::mutate(area = as.numeric(st_area(geometry) * 1e-6)) |>
    dplyr::transmute(
      geoid = GEOID,
      tract = substr(geoid, 1, 11),
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
      white        = 100 * white / population,
    ) |> 
    sf::st_set_geometry(NULL) 
  
  
  
  left_join(bg_acs, tv_acs, by = "tract") |>
    dplyr::filter(population > 0) |>
    tibble::as_tibble()
}


#' Get block group centroids
#' 
#' @details Gets the population-weighted block group centroids
#' for all block groups in Utah.
#' 
get_bgcentroids <- function(){
  bgcentroid <- readr::read_csv("https://www2.census.gov/geo/docs/reference/cenpop2010/blkgrp/CenPop2010_Mean_BG49.txt")
  
  
  
  sf::st_as_sf(bgcentroid, coords = c("LONGITUDE", "LATITUDE"), crs = 4326) |> 
    dplyr::mutate(id = stringr::str_c(STATEFP, COUNTYFP, TRACTCE, BLKGRPCE)) |> 
    dplyr::select(id, POPULATION)
}