#' Calculate costs for a scenario
#' 
#' @param base Base scenario DCLS
#' @param altd Alternative scenario DCLS
#' 
make_costs <- function(base, altd){
  
  return(NULL)
}


get_costbetas <- function(sl_models, ut_models, sj_models){
  lapply(list(sl_models, ut_models, sj_models), function(x){
    coefficients(x$All)["market"]
  }) |> 
    set_names(c("SaltLake", "Utah", "SanJuan")) |> 
    unlist()
}


#' Make access database for utah county
#' 
#' @param access
#' @param bg
#' @param bg_acs
#' 
#' 
make_utbgaccess <- function(access, bg, bg_acs) {
  utbgaccess <-  bg |> 
    filter(substr(GEOID, 3, 5) == "049") |> 
    select(id = GEOID) |> 
    left_join(bg_acs, by = c("id" = "geoid")) |> 
    left_join(access, by = c("id" )) |> 
    filter(dclogsum > -5) |> 
    mutate(access = dclogsum)
  
  utbgaccess
}


make_nocaraccess <- function(access, slnocar_access, bg, bg_acs){
  # limit to salt lake valley
  sl_limits <- tibble(
    lon = c(-112.11, -111.7),
    lat = c(40.5, 40.8)
  ) |> 
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |> 
    sf::st_bbox()
  
  nocar_access <- bg |> 
    select(geoid = GEOID) |> 
    left_join(bg_acs, by = "geoid") |> 
    left_join(access, by = c("geoid" = "id")) |> 
    filter(!is.na(dclogsum)) |> 
    left_join(slnocar_access |> rename(geoid = id, nocarlogsum = dclogsum),
              by = "geoid") |> 
    sf::st_transform(4326) |> 
    sf::st_crop(sl_limits)
  
  
  nocar_access
}

#' Compute the destination choice logsum
#' 
#' @param access_data A data frame created by make_access_data()
#' @param model An estimated model
#' 
compute_dclogsum <- function(access_data, model){
  
  if (!any(grepl("Convenience", names(coefficients(model))))) {
    access_data <- access_data |> 
      mutate(type = ifelse(type == "Convenience", "Other", type))
  }
  
  adidx <- dfidx::dfidx(
    access_data |> 
      mutate(chosen = FALSE),
    idx = c("blockgroup", "resource"), idnames = c("obs_id", "alt") ) 
    
  
  
  require(mlogit)
  
  # This requires the version of broom at gregmacfarlane/broom
  adf <- broom::augment(model, newdata = adidx)
  
  adf |> 
    mutate(id, alternative, expu = exp(.fitted)) |> 
    group_by(id) |> 
    filter(!is.na(expu)) |> 
    summarise(dclogsum = log(sum(expu)))
}



#' Assemble an accessibility dataset
#' 
#' @param bg_acs A dataset with block group information
#' @param imputed_groceries A dataset with imputed grocery store information
#' @param mcls A dataset with travel times and mode choice values
#' @param geoids A list of geoids to include in the dataset
#' @param max_car The maximum car duration to be included in the choice set
#' @param completed_id Which of the multiple imputed datasets to use. If NULL, will
#'   pick a random frame.
#' 
make_access_data <- function(bg_acs, imputed_groceries, mcls, geoids, max_car = 50,
                             completed_id = NULL, new_store = NULL, improved_stores = NULL){
  
  # select a multiply imputed dataset to join
  if ( is.null(completed_id) ) {
    completed_id <- sample(1:length(imputed_groceries$imp$id), 1)
  }
  
  dests <- mice::complete(imputed_groceries, completed_id) |> 
    dplyr::select(id, type:brand) 
  
  if(!is.null(new_store)){
    dests <- bind_rows(dests, new_store)
  }
  
  if(!is.null(improved_stores)){
    dests <- dests |> 
      filter(!id %in% improved_stores$id) |> 
      bind_rows(improved_stores |> st_set_geometry(NULL))
  }
  
  # Build up the choice set dataframe ============
  # start from the mcls data frame because it has origins and destinations to
  # bind to
  access_data <- mcls |> 
    # only keep block groups in the list of geoids
    dplyr::filter(substr(blockgroup, 1, 5) %in% geoids) |> 
    
    # only keep destinations in the maximum range of car travel
    # this will be different in Utah County vs San Juan County
    dplyr::filter(duration_CAR <= max_car) |> 
    dplyr::select(resource, blockgroup, mclogsum) |> 
    
    # join the block groups to the mode choice logsum frame, but use an inner join
    # so that you discard all the origin-destination pairs that are not
    # a part of this analysis
    dplyr::inner_join(bg_acs, by = c("blockgroup" = "geoid")) |> 
  
    # join the groceries data to th 
    dplyr::left_join(dests, by = c("resource" = "id"), 
                     relationship = "many-to-many")
  
  
  access_data
}




#' Estimate a model for accessibility
#' 
#' @param estdata
#' 
estimate_model <- function(estdata) {
  
  df <- dfidx::dfidx(estdata, idx = c("obs_id", "alt")) |> 
    mutate(type = ifelse(type == "Trading Post", "Other", type),
           type = forcats::as_factor(type), 
           type = forcats::fct_relevel(type, "Grocery Store"))
  
  m <- list( 
    "Access" = mlogit(chosen ~ mclogsum | -1, data = df),
    "NEMS" = mlogit(chosen ~ availability + cost | -1, data = df),
    "Attributes" = mlogit(chosen ~ market + total_registers + type | -1, data = df),
    "All" = mlogit(chosen ~ mclogsum + availability + cost + market + total_registers + type | -1, data = df)
  )
  
  m
}


#' Construct an estimation dataset
#' 
#' @param flows tibble with flows from block groups to destination zones
#' @param lsums tibble with multimodal times and logsums between origins and destinations. see calculate_logsums()
#' @param ludata tibble with land use data
#' @param n_obs Number of simulated agents
#' @param n_alts Number of non-chosen alternatives
#' 
#' @return A tibble with simulated choice makers and their chosen alternative
#' 
#' 
make_estdata <- function(flows, lsums, ludata, acsdata, n_obs = 50, n_alts = 5,
                         day = "0: All Days (M-Su)", time = "All Day (12am-12am)") {
  
  # Get a list of chosen destinations  ----
  mydata <- flows %>%
    dplyr::ungroup() %>%
    dplyr::filter(dest %in% lsums$resource, geoid %in% lsums$blockgroup) %>%
    dplyr::filter(time %in% time, day %in% day) %>%
    dplyr::mutate(weight = flow / sum(flow)) %>%
    dplyr::sample_n(n_obs, replace = TRUE, weight = weight) %>%
    dplyr::transmute(obs_id = as.character(row_number()), geoid, dest,
              validation = sample(c(TRUE,FALSE), n(), TRUE, prob = c(0.2, 0.8))) %>%
    dplyr::rename(alt_0 = dest) %>%
    dplyr::filter(alt_0 %in% ludata$id)
  
  
  
  # Get a list of non-chosen alternatives ---------
  sampled_dests <- lapply(1:nrow(mydata), function(i){
    sample(ludata$id[which(mydata$obs_id[i] != ludata$id)], n_alts)
  }) %>%
    unlist() %>%
    matrix(ncol = n_alts) %>%
    as_tibble(.name_repair = ~ stringr::str_c("alt", 1:n_alts, sep = "_"))
  
  # Create dataset ----
  attributes <- ludata %>% st_set_geometry(NULL)  %>% as_tibble()
  
  
  logitdata <- mydata %>%
    dplyr::bind_cols(sampled_dests) %>%
    tidyr::gather(key = "alt", value = "dest", -obs_id, -geoid, -validation) %>%
    dplyr::mutate(chosen = alt == "alt_0") %>%
    dplyr::arrange(obs_id, alt) %>%
    
    # append attributes
    dplyr::left_join(attributes, by = c("dest" = "id")) %>%
    
    # append distances
    dplyr::left_join(lsums, by = c("geoid" = "blockgroup", "dest" = "resource")) %>%
    
    # append block group attributes
    dplyr::left_join(acsdata, by = c("geoid")
              
    )
  
  logitdata
}


#' Read and clean up Streetlight data file
#' 
#' @param path to a streetlight data file
#' @param mcls modechoice logsums matrix
#' 
#' @return a tibble with cleaned and organized data
read_sl_data <- function(path, mcls, edit_dest = ""){
  
  # available flows ----
  # don't keep flows that OSM has no path between
  available <- mcls |> dplyr::select(resource, blockgroup)
  
  readr::read_csv(
    path, 
    col_types = list(
      `Zone ID` = col_character(),
      `Average Daily Zone Traffic (StL Volume)` = col_number(),
      `Percent by Home Location` = col_double()
    ))   %>%
    dplyr::transmute(
      geoid = as.character(ifelse(`Block Group ID` == "N/A", NA, `Block Group ID`)),
      dest = str_c(edit_dest, `Zone Name`, sep = ""),
      dest_id = `Zone ID`,
      home_work = `Home and Work Filter`,
      intersection = `Intersection Type`,
      day  = `Day Type`,
      time = `Day Part`,
      volume = `Average Daily Zone Traffic (StL Volume)`,
      percent = `Percent by Home Location`,
      flow = volume * percent,
      flow = ifelse(is.na(flow), 0, flow)
    )   %>%
    dplyr::filter(!is.na(geoid)) |> 
    dplyr::inner_join(available, by = c("geoid" = "blockgroup", "dest" = "resource"))
}

