#' @import RosyUtils
#' @import magrittr
#' @title update_RosyRun
#' @return DB object
#' @export
update_RosyRun<-function(DB,default_check_hours = 24,recent_only = T){
  try_connection <- T
  if(is_something(DB$internals$last_data_update))try_connection<-age(DB$internals$last_data_update,Sys.time(),units = "hours",floor = F)>default_check_hours
  if(!try_connection){
    bullet_in_console(paste0("Last check was less than ",default_check_hours," hours ago."), bullet_type = "i")
  }else{
    CONNECTED <- get_sum_activities(DB = DB,recent_only = recent_only)
    if(!CONNECTED){
      bullet_in_console(paste0("Unable to connect to API."), bullet_type = "x")
    }
    if(CONNECTED){
      get_det_activities(DB = DB)
      DB$internals$last_data_update<-Sys.time()
    }
  }
  IDs<-check_RosyRun_dir(DB,F)
  IDs<-IDs$ID[which(IDs$det)]
  IDs<-IDs[which(!IDs%in%DB$data$ACTIVITIES$activity_id)]
  if(length(IDs)>0){
    message(length(IDs), ' IDs to add/update')
    pb <- progress::progress_bar$new(
      format = "  adding to DB [:bar] :percent ETA: :eta",
      total = length(IDs), clear = FALSE, width= 60)
    for(ID in IDs){#ID <- IDs %>% sample1()
      LIST_IN <- jsonlite::fromJSON(file.path(DB$dir_path,'input','json',paste0("det_",ID,'.json'))) %>% strava_list_ETL()
      for(i in seq_along(LIST_IN)){
        if (!names(LIST_IN[i])%in% names(DB$data)){
          DB$data[[names(LIST_IN[i])]] <- LIST_IN[[i]][0,]
        }
        row_check <- which(DB$data[[names(LIST_IN[i])]]$activity_id != LIST_IN[[i]]$activity_id)
        if(length(row_check)>0){
          DB$data[[names(LIST_IN[i])]] <- DB$data[[names(LIST_IN[i])]][row_check,]
        }
        DB$data[[names(LIST_IN[i])]] <- DB$data[[names(LIST_IN[i])]] %>% dplyr::bind_rows(LIST_IN[[i]])
      }
      pb$tick()
    }
    for(TABLE in c(
      "ACTIVITIES",
      "LAPS_U",
      "LAPS_I",
      "LAPS_M"
    )){
      if(TABLE %in% names(DB$data)){
        rownames(DB$data[[TABLE]]) <- NULL
        if(TABLE =="ACTIVITIES"){
          DB$data[[TABLE]] <- DB$data[[TABLE]][order(DB$data[[TABLE]]$datetime,decreasing = T),]
        }else{
          DB$data[[TABLE]] <- DB$data[[TABLE]][order(DB$data[["ACTIVITIES"]]$datetime[match(DB$data[[TABLE]]$activity_id, DB$data[["ACTIVITIES"]]$activity_id)],decreasing = T),]
        }
      }
    }
    save_DB(DB)
  }else{
    message('Nothing to update!')
  }
  # save_DB_raw(DB_raw,ask=F)
  return(DB)
}
strava_list_ETL <- function(LIST_IN){
  ##ACTIVITIES ----
  ACTIVITIES<-data.frame(
    # resource_state = LIST_IN[['resource_state']] %>% na_if_empty() %>% as.character(),
    activity_id = LIST_IN[['id']] %>% na_if_empty() %>% as.character(),
    athlete_id = LIST_IN[['athlete']][[1]] %>% na_if_empty() %>% as.character(),
    name = LIST_IN[['name']] %>% na_if_empty() %>% as.character(),
    distance_km = (LIST_IN[['distance']] %>% na_if_empty() %>% as.numeric()) %>% divide_by(1000),
    distance_mi = (LIST_IN[['distance']] %>% na_if_empty() %>% as.numeric()) %>% divide_by(1609.344),
    moving_time_mins = LIST_IN[['moving_time']] %>% na_if_empty() %>% as.numeric() %>% divide_by(60),
    moving_time_secs = LIST_IN[['moving_time']] %>% na_if_empty() %>% as.numeric(),
    moving_time_hms = LIST_IN[['moving_time']] %>% na_if_empty() %>% as.numeric() %>% hms::as_hms(),
    elapsed_time_mins = LIST_IN[['elapsed_time']] %>% na_if_empty() %>% as.numeric() %>% divide_by(60),
    elapsed_time_secs = LIST_IN[['elapsed_time']] %>% na_if_empty() %>% as.numeric(),
    elapsed_time_hms = LIST_IN[['elapsed_time']] %>% na_if_empty() %>% as.numeric() %>% hms::as_hms(),
    total_elevation_gain = LIST_IN[['total_elevation_gain']] %>% na_if_empty() %>% as.numeric(),
    type = LIST_IN[['type']] %>% na_if_empty() %>% as.character(),
    sport_type = LIST_IN[['sport_type']] %>% na_if_empty() %>% as.character(),
    workout_type = LIST_IN[['workout_type']] %>% na_if_empty() %>% as.character(),
    start_date = LIST_IN[['start_date']] %>% na_if_empty() %>% as.character(),
    start_date_local = LIST_IN[['start_date_local']] %>% na_if_empty() %>% as.character(),
    timezone = LIST_IN[['timezone']] %>% na_if_empty() %>% as.character(),
    utc_offset = LIST_IN[['utc_offset']] %>% na_if_empty() %>% as.character(),
    location_city = LIST_IN[['location_city']] %>% na_if_empty() %>% as.character(),
    location_state = LIST_IN[['location_state']] %>% na_if_empty() %>% as.character(),
    location_country = LIST_IN[['location_country']] %>% na_if_empty() %>% as.character(),
    city = as.character(NA),
    county = as.character(NA),
    state = as.character(NA),
    achievement_count = LIST_IN[['achievement_count']] %>% na_if_empty() %>% as.character(),
    kudos_count = LIST_IN[['kudos_count']] %>% na_if_empty() %>% as.character(),
    comment_count = LIST_IN[['comment_count']] %>% na_if_empty() %>% as.character(),
    athlete_count = LIST_IN[['athlete_count']] %>% na_if_empty() %>% as.character(),
    photo_count = LIST_IN[['photo_count']] %>% na_if_empty() %>% as.character(),
    trainer = LIST_IN[['trainer']] %>% na_if_empty() %>% as.character(),
    commute = LIST_IN[['commute']] %>% na_if_empty() %>% as.character(),
    manual = LIST_IN[['manual']] %>% na_if_empty() %>% as.character(),
    private = LIST_IN[['private']] %>% na_if_empty() %>% as.character(),
    visibility = LIST_IN[['visibility']] %>% na_if_empty() %>% as.character(),
    flagged = LIST_IN[['flagged']] %>% na_if_empty() %>% as.character(),
    gear_id = LIST_IN[['gear_id']] %>% na_if_empty() %>% as.character(),
    start_lat = LIST_IN[['start_latlng']][1] %>% unlist() %>% na_if_empty() %>% as.numeric(),
    start_lat_round = LIST_IN[['start_latlng']][1] %>% unlist() %>% na_if_empty() %>% as.numeric() %>% round(2),
    start_lon = LIST_IN[['start_latlng']][2] %>% unlist() %>% na_if_empty() %>% as.numeric(),
    start_lon_round = LIST_IN[['start_latlng']][2] %>% unlist() %>% na_if_empty() %>% as.numeric() %>% round(2),
    end_lat = LIST_IN[['end_latlng']][1] %>% unlist() %>% na_if_empty() %>% as.numeric(),
    end_lon = LIST_IN[['end_latlng']][2] %>% unlist() %>% na_if_empty() %>% as.numeric(),
    average_speed = LIST_IN[['average_speed']] %>% na_if_empty() %>% as.numeric(),
    max_speed = LIST_IN[['max_speed']] %>% na_if_empty() %>% as.numeric(),
    has_heartrate = LIST_IN[['has_heartrate']] %>% na_if_empty() %>% as.character() %>% as.logical(),
    average_heartrate = LIST_IN[['average_heartrate']] %>% na_if_empty() %>% as.numeric(),
    # BP_mi = LIST_IN[['average_heartrate']] %>% na_if_empty() %>% as.numeric() %>% magrittr::multiply_by(
    #
    # )
    max_heartrate = LIST_IN[['max_heartrate']] %>% na_if_empty() %>% as.numeric(),
    heartrate_opt_out = LIST_IN[['heartrate_opt_out']] %>% na_if_empty() %>% as.character(),
    display_hide_heartrate_option = LIST_IN[['display_hide_heartrate_option']] %>% na_if_empty() %>% as.character(),
    elev_high = LIST_IN[['elev_high']] %>% na_if_empty() %>% as.numeric(),
    elev_low = LIST_IN[['elev_low']] %>% na_if_empty() %>% as.numeric(),
    upload_id = LIST_IN[['upload_id']] %>% na_if_empty() %>% as.character(),
    upload_id_str = LIST_IN[['upload_id_str']] %>% na_if_empty() %>% as.character(),
    external_id = LIST_IN[['external_id']] %>% na_if_empty() %>% as.character(),
    from_accepted_tag = LIST_IN[['from_accepted_tag']] %>% na_if_empty() %>% as.character(),
    pr_count = LIST_IN[['pr_count']] %>% na_if_empty() %>% as.character(),
    total_photo_count = LIST_IN[['total_photo_count']] %>% na_if_empty() %>% as.character(),
    has_kudoed = LIST_IN[['has_kudoed']] %>% na_if_empty() %>% as.character(),
    description = LIST_IN[['description']] %>% na_if_empty() %>% as.character(),
    calories = LIST_IN[['calories']] %>% na_if_empty() %>% as.numeric(),
    perceived_exertion = LIST_IN[['perceived_exertion']] %>% na_if_empty() %>% as.character(),
    prefer_perceived_exertion = LIST_IN[['prefer_perceived_exertion']] %>% na_if_empty() %>% as.character(),
    hide_from_home = LIST_IN[['hide_from_home']] %>% na_if_empty() %>% as.character(),
    device_name = LIST_IN[['device_name']] %>% na_if_empty() %>% as.character(),
    embed_token = LIST_IN[['embed_token']] %>% na_if_empty() %>% as.character(),
    available_zones = LIST_IN[['available_zones']] %>% unlist() %>% paste0(collapse = " | ") %>% na_if_empty() %>% as.character(),
    average_watts = LIST_IN[['average_watts']] %>% na_if_empty() %>% as.character(),
    kilojoules = LIST_IN[['kilojoules']] %>% na_if_empty() %>% as.character(),
    device_watts = LIST_IN[['device_watts']] %>% na_if_empty() %>% as.character(),
    average_cadence = LIST_IN[['average_cadence']] %>% na_if_empty() %>% as.numeric() %>% multiply_by(2),
    average_temp = LIST_IN[['average_temp']] %>% na_if_empty() %>% as.numeric() %>% multiply_by(9/5) %>% add(32),
    gear_primary = LIST_IN[['gear']][['primary']] %>% na_if_empty() %>% as.character(),
    gear_name = LIST_IN[['gear']][['name']] %>% na_if_empty() %>% as.character(),
    gear_nickname = LIST_IN[['gear']][['nickname']] %>% na_if_empty() %>% as.character(),
    gear_distance = LIST_IN[['gear']][['distance']] %>% na_if_empty() %>% as.character(),
    gear_converted_distance = LIST_IN[['gear']][['converted_distance']] %>% na_if_empty() %>% as.character(),
    private_note = LIST_IN[['private_note']] %>% na_if_empty() %>% as.character()
  )
  if(nrow(ACTIVITIES)>1)stop("Something went very wrong; activity > 1 -> ",NAME)
  ACTIVITIES$datetime <- lubridate::as_datetime(ACTIVITIES$start_date_local)
  ACTIVITIES$date <- as.Date(strtrim(ACTIVITIES$datetime,10))
  # make groups of different distances using ifelse
  ACTIVITIES$activity_type_distance <- ifelse(ACTIVITIES$distance_mi < 5.2, "0-5 mi", ifelse(ACTIVITIES$distance_mi < 10.2, "5-10 mi",">10 mi"))
  # make factors for these so that they're in the right order when we make the plot
  ACTIVITIES$activity_type_distance = factor(ACTIVITIES$activity_type_distance, levels=c("0-5 mi","5-10 mi",">10 mi"),ordered = T)
  # ACTIVITIES <- ACTIVITIES[which(!grepl("2013|2014|2015",ACTIVITIES$date)),] # remove these years from the analysis
  # ACTIVITIES$average_heartrate[which(ACTIVITIES$average_heartrate<145)]=NA
  ACTIVITIES$moving_pace_mins_mi <- ifelse(
    ACTIVITIES$moving_time_mins!=0&
      !is.na(ACTIVITIES$moving_time_mins)&
      ACTIVITIES$distance_mi!=0&
      !is.na(ACTIVITIES$distance_mi),
    ACTIVITIES$moving_time_mins/ACTIVITIES$distance_mi,
    NA
  )
  ACTIVITIES$moving_pace_mins_mi_hms <- ifelse(
    ACTIVITIES$moving_time_secs!=0&
      !is.na(ACTIVITIES$moving_time_secs)&
      ACTIVITIES$distance_mi!=0&
      !is.na(ACTIVITIES$distance_mi),
    ACTIVITIES$moving_time_secs %>% divide_by(ACTIVITIES$distance_mi)%>% as.integer() %>% add(1),
    NA
  )  %>% hms::as_hms()
  ACTIVITIES$moving_pace_mins_km <- ifelse(
    ACTIVITIES$moving_time_mins!=0&
      !is.na(ACTIVITIES$moving_time_mins)&
      ACTIVITIES$distance_km!=0&
      !is.na(ACTIVITIES$distance_km),
    ACTIVITIES$moving_time_mins/ACTIVITIES$distance_km,
    NA
  )
  ACTIVITIES$moving_pace_mins_km_hms <- ifelse(
    ACTIVITIES$moving_time_secs!=0&
      !is.na(ACTIVITIES$moving_time_secs)&
      ACTIVITIES$distance_km!=0&
      !is.na(ACTIVITIES$distance_km),
    ACTIVITIES$moving_time_secs %>% divide_by(ACTIVITIES$distance_km)%>% as.integer() %>% add(1),
    NA
  )  %>% hms::as_hms()
  ACTIVITIES$elapsed_pace_mins_mi <- ifelse(
    ACTIVITIES$elapsed_time_mins!=0&
      !is.na(ACTIVITIES$elapsed_time_mins)&
      ACTIVITIES$distance_mi!=0&
      !is.na(ACTIVITIES$distance_mi),
    ACTIVITIES$elapsed_time_mins/ACTIVITIES$distance_mi,
    NA
  )
  ACTIVITIES$elapsed_pace_mins_mi_hms <- ifelse(
    ACTIVITIES$elapsed_time_secs!=0&
      !is.na(ACTIVITIES$elapsed_time_secs)&
      ACTIVITIES$distance_mi!=0&
      !is.na(ACTIVITIES$distance_mi),
    ACTIVITIES$elapsed_time_secs %>% divide_by(ACTIVITIES$distance_mi)%>% as.integer() %>% add(1),
    NA
  )  %>% hms::as_hms()
  ACTIVITIES$elapsed_pace_mins_km <- ifelse(
    ACTIVITIES$elapsed_time_mins!=0&
      !is.na(ACTIVITIES$elapsed_time_mins)&
      ACTIVITIES$distance_km!=0&
      !is.na(ACTIVITIES$distance_km),
    ACTIVITIES$elapsed_time_mins/ACTIVITIES$distance_km,
    NA
  )
  ACTIVITIES$elapsed_pace_mins_km_hms <- ifelse(
    ACTIVITIES$elapsed_time_secs!=0&
      !is.na(ACTIVITIES$elapsed_time_secs)&
      ACTIVITIES$distance_km!=0&
      !is.na(ACTIVITIES$distance_km),
    ACTIVITIES$elapsed_time_secs %>% divide_by(ACTIVITIES$distance_km)%>% as.integer() %>% add(1),
    NA
  )  %>% hms::as_hms()
  ACTIVITIES$BP_mi <- ACTIVITIES$average_heartrate %>% multiply_by(ACTIVITIES$moving_pace_mins_mi) %>% as.integer() %>% add(1)
  ACTIVITIES$BP_km <- ACTIVITIES$average_heartrate %>% multiply_by(ACTIVITIES$moving_pace_mins_km) %>% as.integer() %>% add(1)
  ACTIVITIES$average_heartrate_type <- NA
  ACTIVITIES$average_heartrate_type[which((ACTIVITIES$average_heartrate>=145)&(ACTIVITIES$average_heartrate<=160))] <- "Low HR"
  ACTIVITIES$average_heartrate_type[which((ACTIVITIES$average_heartrate>160)&(ACTIVITIES$average_heartrate<=174))] <- "Aerobic"
  ACTIVITIES$average_heartrate_type[which(ACTIVITIES$average_heartrate>174)] <- "Anaerobic"
  ACTIVITIES$average_heartrate_type <- ACTIVITIES$average_heartrate_type %>% factor(levels = c("Low HR","Aerobic","Anaerobic"),ordered = T)
  ACTIVITIES$weekday <- weekdays(ACTIVITIES$date)
  ACTIVITIES$week_start<-weekstart(ACTIVITIES$date)  %>% as.Date()
  ACTIVITIES$month <- months(ACTIVITIES$date) %>% factor(levels=sapply(1:12, function(x){paste0("2020-",stringr::str_pad(x,2,pad=0),"-01") %>% as.Date() %>% months()}))
  ACTIVITIES$month_day<- lubridate::mday(ACTIVITIES$date)
  ACTIVITIES$month_start<-ACTIVITIES$date %>% strtrim(7) %>% paste0("-01") %>% as.Date()
  ACTIVITIES$year <- lubridate::year(ACTIVITIES$date)
  ACTIVITIES$year_day<- lubridate::yday(ACTIVITIES$date)
  ACTIVITIES$station_name <- NA
  ACTIVITIES$location_state <- NA
  ACTIVITIES$distance_rnoaa <- NA
  ACTIVITIES$temperature <- NA
  ACTIVITIES$got_temp <- "FALSE"
  ACTIVITIES$lastupdated<-NA
  ACTIVITIES$weight_ext <- NA
  ACTIVITIES$BMI_ext <- NA
  ##LAPS_U-----
  # name check
  # DB_raw$raw %>% map_brose1("laps") %>% map_brose1("activity") %>% lapply(names) %>% unlist() %>% unique()
  if(LIST_IN[['laps']][['id']] %>% unlist() %>% length() %>% is_greater_than(0)){
    LAPS_U<-data.frame(
      activity_id = LIST_IN[['id']] %>% na_if_empty() %>% as.character(),
      activity_type = LIST_IN[['type']] %>% na_if_empty() %>% as.character(),
      lap_id = LIST_IN[['laps']][['id']] %>% na_if_empty() %>% as.character(),
      # resource_state = LIST_IN[['laps']][['resource_state']] %>% na_if_empty() %>% as.character(),
      name = LIST_IN[['laps']][['name']] %>% na_if_empty() %>% as.character(),
      athlete_id = LIST_IN[['laps']][['athlete']][['id']] %>% na_if_empty() %>% as.character(),
      moving_time_mins = LIST_IN[['laps']][['moving_time']] %>% na_if_empty() %>% as.numeric() %>% divide_by(60),
      moving_time_secs = LIST_IN[['laps']][['moving_time']] %>% na_if_empty() %>% as.numeric(),
      moving_time_hms = LIST_IN[['laps']][['moving_time']] %>% na_if_empty() %>% as.numeric() %>% hms::as_hms(),
      elapsed_time_mins = LIST_IN[['laps']][['elapsed_time']] %>% na_if_empty() %>% as.numeric() %>% divide_by(60),
      elapsed_time_secs = LIST_IN[['laps']][['elapsed_time']] %>% na_if_empty() %>% as.numeric(),
      elapsed_time_hms = LIST_IN[['laps']][['elapsed_time']] %>% na_if_empty() %>% as.numeric() %>% hms::as_hms(),
      start_date = LIST_IN[['laps']][['start_date']] %>% na_if_empty() %>% as.character(),
      start_date_local = LIST_IN[['laps']][['start_date_local']] %>% na_if_empty() %>% as.character(),
      distance_km = (LIST_IN[['laps']][['distance']] %>% na_if_empty() %>% as.numeric()) %>% divide_by(1000),
      distance_mi = (LIST_IN[['laps']][['distance']] %>% na_if_empty() %>% as.numeric()) %>% divide_by(1609.344),
      start_index = LIST_IN[['laps']][['start_index']] %>% na_if_empty() %>% as.character(),
      end_index = LIST_IN[['laps']][['end_index']] %>% na_if_empty() %>% as.character(),
      total_elevation_gain = LIST_IN[['laps']][['total_elevation_gain']] %>% na_if_empty() %>% as.numeric(),
      average_speed = LIST_IN[['laps']][['average_speed']] %>% na_if_empty() %>% as.numeric(),
      max_speed = LIST_IN[['laps']][['max_speed']] %>% na_if_empty() %>% as.numeric(),
      device_watts = LIST_IN[['laps']][['device_watts']] %>% na_if_empty() %>% as.character(),
      average_heartrate = LIST_IN[['laps']][['average_heartrate']] %>% na_if_empty() %>% as.numeric(),
      max_heartrate = LIST_IN[['laps']][['max_heartrate']] %>% na_if_empty() %>% as.numeric(),
      lap_index = LIST_IN[['laps']][['lap_index']] %>% na_if_empty() %>% as.character(),
      split = LIST_IN[['laps']][['split']] %>% na_if_empty() %>% as.numeric(),
      pace_zone = LIST_IN[['laps']][['pace_zone']] %>% na_if_empty() %>% as.character(),
      average_watts = LIST_IN[['laps']][['average_watts']] %>% na_if_empty() %>% as.numeric(),
      average_cadence = LIST_IN[['laps']][['average_cadence']] %>% na_if_empty() %>% as.numeric() %>% multiply_by(2)
    )
    LAPS_U$activity_type_distance <- ifelse(LAPS_U$distance_mi >= 0.5, "~1 mi", "<0.5")
    # make factors for these so that they're in the right order when we make the plot
    # LAPS_U <- LAPS_U[which(!grepl("2013|2014|2015",LAPS_U$date)),] # remove these years from the analysis
    # LAPS_U$average_heartrate[which(LAPS_U$average_heartrate<145)]=NA
    LAPS_U$moving_pace_mins_mi <- ifelse(
      LAPS_U$moving_time_mins!=0&
        !is.na(LAPS_U$moving_time_mins)&
        LAPS_U$distance_mi!=0&
        !is.na(LAPS_U$distance_mi),
      LAPS_U$moving_time_mins/LAPS_U$distance_mi,
      NA
    )
    LAPS_U$moving_pace_mins_mi_hms <- ifelse(
      LAPS_U$moving_time_secs!=0&
        !is.na(LAPS_U$moving_time_secs)&
        LAPS_U$distance_mi!=0&
        !is.na(LAPS_U$distance_mi),
      LAPS_U$moving_time_secs %>% divide_by(LAPS_U$distance_mi)%>% as.integer() %>% add(1),
      NA
    )  %>% hms::as_hms()
    LAPS_U$moving_pace_mins_km <- ifelse(
      LAPS_U$moving_time_mins!=0&
        !is.na(LAPS_U$moving_time_mins)&
        LAPS_U$distance_km!=0&
        !is.na(LAPS_U$distance_km),
      LAPS_U$moving_time_mins/LAPS_U$distance_km,
      NA
    )
    LAPS_U$moving_pace_mins_km_hms <- ifelse(
      LAPS_U$moving_time_secs!=0&
        !is.na(LAPS_U$moving_time_secs)&
        LAPS_U$distance_km!=0&
        !is.na(LAPS_U$distance_km),
      LAPS_U$moving_time_secs %>% divide_by(LAPS_U$distance_km)%>% as.integer() %>% add(1),
      NA
    )  %>% hms::as_hms()
    LAPS_U$elapsed_pace_mins_mi <- ifelse(
      LAPS_U$elapsed_time_mins!=0&
        !is.na(LAPS_U$elapsed_time_mins)&
        LAPS_U$distance_mi!=0&
        !is.na(LAPS_U$distance_mi),
      LAPS_U$elapsed_time_mins/LAPS_U$distance_mi,
      NA
    )
    LAPS_U$elapsed_pace_mins_mi_hms <- ifelse(
      LAPS_U$elapsed_time_secs!=0&
        !is.na(LAPS_U$elapsed_time_secs)&
        LAPS_U$distance_mi!=0&
        !is.na(LAPS_U$distance_mi),
      LAPS_U$elapsed_time_secs %>% divide_by(LAPS_U$distance_mi)%>% as.integer() %>% add(1),
      NA
    )  %>% hms::as_hms()
    LAPS_U$elapsed_pace_mins_km <- ifelse(
      LAPS_U$elapsed_time_mins!=0&
        !is.na(LAPS_U$elapsed_time_mins)&
        LAPS_U$distance_km!=0&
        !is.na(LAPS_U$distance_km),
      LAPS_U$elapsed_time_mins/LAPS_U$distance_km,
      NA
    )
    LAPS_U$elapsed_pace_mins_km_hms <- ifelse(
      LAPS_U$elapsed_time_secs!=0&
        !is.na(LAPS_U$elapsed_time_secs)&
        LAPS_U$distance_km!=0&
        !is.na(LAPS_U$distance_km),
      LAPS_U$elapsed_time_secs %>% divide_by(LAPS_U$distance_km)%>% as.integer() %>% add(1),
      NA
    )  %>% hms::as_hms()
    LAPS_U$BP_mi <- LAPS_U$average_heartrate %>% multiply_by(LAPS_U$moving_pace_mins_mi) %>% as.integer() %>% add(1)
    LAPS_U$BP_km <- LAPS_U$average_heartrate %>% multiply_by(LAPS_U$moving_pace_mins_km) %>% as.integer() %>% add(1)
  }else{
    LAPS_U<-NULL
  }
  ##LAPS_I----
  # DB_raw$raw %>% map_brose1("splits_standard") %>% lapply(names) %>% unlist() %>% unique()
  # SEGMENTS<-data.frame( LIST_IN[['segment_efforts']]) #need to fix lat long
  if(LIST_IN[['splits_standard']][['distance']] %>% unlist() %>% length() %>% is_greater_than(0)){
    LAPS_I<-data.frame(
      activity_id = LIST_IN[['id']] %>% na_if_empty() %>% as.character(),
      activity_type = LIST_IN[['type']] %>% na_if_empty() %>% as.character(),
      distance_km = (LIST_IN[['splits_standard']][['distance']] %>% na_if_empty() %>% as.numeric()) %>% divide_by(1000),
      distance_mi = (LIST_IN[['splits_standard']][['distance']] %>% na_if_empty() %>% as.numeric()) %>% divide_by(1609.344),
      moving_time_mins = LIST_IN[['splits_standard']][['moving_time']] %>% na_if_empty() %>% as.numeric() %>% divide_by(60),
      moving_time_secs = LIST_IN[['splits_standard']][['moving_time']] %>% na_if_empty() %>% as.numeric(),
      moving_time_hms = LIST_IN[['splits_standard']][['moving_time']] %>% na_if_empty() %>% as.numeric() %>% hms::as_hms(),
      elapsed_time_mins = LIST_IN[['splits_standard']][['elapsed_time']] %>% na_if_empty() %>% as.numeric() %>% divide_by(60),
      elapsed_time_secs = LIST_IN[['splits_standard']][['elapsed_time']] %>% na_if_empty() %>% as.numeric(),
      elapsed_time_hms = LIST_IN[['splits_standard']][['elapsed_time']] %>% na_if_empty() %>% as.numeric() %>% hms::as_hms(),
      elevation_difference = LIST_IN[['splits_standard']][['elevation_difference']] %>% na_if_empty() %>% as.character(),
      split = LIST_IN[['splits_standard']][['split']] %>% na_if_empty() %>% as.numeric(),
      average_speed = LIST_IN[['splits_standard']][['average_speed']] %>% na_if_empty() %>% as.character(),
      average_grade_adjusted_speed = LIST_IN[['splits_standard']][['average_grade_adjusted_speed']] %>% na_if_empty() %>% as.numeric(),
      average_heartrate = LIST_IN[['splits_standard']][['average_heartrate']] %>% na_if_empty() %>% as.numeric(),
      pace_zone = LIST_IN[['splits_standard']][['pace_zone']] %>% na_if_empty() %>% as.numeric()
    )
    LAPS_I$activity_type_distance <- ifelse(LAPS_I$distance_mi >= 0.7, "~1 mi", "<0.7")
    # make factors for these so that they're in the right order when we make the plot
    # LAPS_I <- LAPS_I[which(!grepl("2013|2014|2015",LAPS_I$date)),] # remove these years from the analysis
    # LAPS_I$average_heartrate[which(LAPS_I$average_heartrate<145)]=NA
    LAPS_I$moving_pace_mins_mi <- ifelse(
      LAPS_I$moving_time_mins!=0&
        !is.na(LAPS_I$moving_time_mins)&
        LAPS_I$distance_mi!=0&
        !is.na(LAPS_I$distance_mi),
      LAPS_I$moving_time_mins/LAPS_I$distance_mi,
      NA
    )
    LAPS_I$moving_pace_mins_mi_hms <- ifelse(
      LAPS_I$moving_time_secs!=0&
        !is.na(LAPS_I$moving_time_secs)&
        LAPS_I$distance_mi!=0&
        !is.na(LAPS_I$distance_mi),
      LAPS_I$moving_time_secs %>% divide_by(LAPS_I$distance_mi)%>% as.integer() %>% add(1),
      NA
    )  %>% hms::as_hms()
    LAPS_I$moving_pace_mins_km <- ifelse(
      LAPS_I$moving_time_mins!=0&
        !is.na(LAPS_I$moving_time_mins)&
        LAPS_I$distance_km!=0&
        !is.na(LAPS_I$distance_km),
      LAPS_I$moving_time_mins/LAPS_I$distance_km,
      NA
    )
    LAPS_I$moving_pace_mins_km_hms <- ifelse(
      LAPS_I$moving_time_secs!=0&
        !is.na(LAPS_I$moving_time_secs)&
        LAPS_I$distance_km!=0&
        !is.na(LAPS_I$distance_km),
      LAPS_I$moving_time_secs %>% divide_by(LAPS_I$distance_km)%>% as.integer() %>% add(1),
      NA
    )  %>% hms::as_hms()
    LAPS_I$elapsed_pace_mins_mi <- ifelse(
      LAPS_I$elapsed_time_mins!=0&
        !is.na(LAPS_I$elapsed_time_mins)&
        LAPS_I$distance_mi!=0&
        !is.na(LAPS_I$distance_mi),
      LAPS_I$elapsed_time_mins/LAPS_I$distance_mi,
      NA
    )
    LAPS_I$elapsed_pace_mins_mi_hms <- ifelse(
      LAPS_I$elapsed_time_secs!=0&
        !is.na(LAPS_I$elapsed_time_secs)&
        LAPS_I$distance_mi!=0&
        !is.na(LAPS_I$distance_mi),
      LAPS_I$elapsed_time_secs %>% divide_by(LAPS_I$distance_mi)%>% as.integer() %>% add(1),
      NA
    )  %>% hms::as_hms()
    LAPS_I$elapsed_pace_mins_km <- ifelse(
      LAPS_I$elapsed_time_mins!=0&
        !is.na(LAPS_I$elapsed_time_mins)&
        LAPS_I$distance_km!=0&
        !is.na(LAPS_I$distance_km),
      LAPS_I$elapsed_time_mins/LAPS_I$distance_km,
      NA
    )
    LAPS_I$elapsed_pace_mins_km_hms <- ifelse(
      LAPS_I$elapsed_time_secs!=0&
        !is.na(LAPS_I$elapsed_time_secs)&
        LAPS_I$distance_km!=0&
        !is.na(LAPS_I$distance_km),
      LAPS_I$elapsed_time_secs %>% divide_by(LAPS_I$distance_km)%>% as.integer() %>% add(1),
      NA
    )  %>% hms::as_hms()
    LAPS_I$BP_mi <- LAPS_I$average_heartrate %>% multiply_by(LAPS_I$moving_pace_mins_mi) %>% as.integer() %>% add(1)
    LAPS_I$BP_km <- LAPS_I$average_heartrate %>% multiply_by(LAPS_I$moving_pace_mins_km) %>% as.integer() %>% add(1)
  }else{
    LAPS_I<-NULL
  }
  # DB_raw$raw %>% map_brose1("splits_metric") %>% lapply(names) %>% unlist() %>% unique()
  ##LAPS_M----
  # DB_raw$raw %>% map_brose1("splits_metric") %>% lapply(names) %>% unlist() %>% unique()
  # SEGMENTS<-data.frame( LIST_IN[['segment_efforts']]) #need to fix lat long
  if(LIST_IN[['splits_metric']][['distance']] %>% unlist() %>% length() %>% is_greater_than(0)){
    LAPS_M<-data.frame(
      activity_id = LIST_IN[['id']] %>% na_if_empty() %>% as.character(),
      activity_type = LIST_IN[['type']] %>% na_if_empty() %>% as.character(),
      distance_km = (LIST_IN[['splits_metric']][['distance']] %>% na_if_empty() %>% as.numeric()) %>% divide_by(1000),
      distance_mi = (LIST_IN[['splits_metric']][['distance']] %>% na_if_empty() %>% as.numeric()) %>% divide_by(1609.344),
      moving_time_mins = LIST_IN[['splits_metric']][['moving_time']] %>% na_if_empty() %>% as.numeric() %>% divide_by(60),
      moving_time_secs = LIST_IN[['splits_metric']][['moving_time']] %>% na_if_empty() %>% as.numeric(),
      moving_time_hms = LIST_IN[['splits_metric']][['moving_time']] %>% na_if_empty() %>% as.numeric() %>% hms::as_hms(),
      elapsed_time_mins = LIST_IN[['splits_metric']][['elapsed_time']] %>% na_if_empty() %>% as.numeric() %>% divide_by(60),
      elapsed_time_secs = LIST_IN[['splits_metric']][['elapsed_time']] %>% na_if_empty() %>% as.numeric(),
      elapsed_time_hms = LIST_IN[['splits_metric']][['elapsed_time']] %>% na_if_empty() %>% as.numeric() %>% hms::as_hms(),
      elevation_difference = LIST_IN[['splits_metric']][['elevation_difference']] %>% na_if_empty() %>% as.character(),
      split = LIST_IN[['splits_metric']][['split']] %>% na_if_empty() %>% as.numeric(),
      average_speed = LIST_IN[['splits_metric']][['average_speed']] %>% na_if_empty() %>% as.character(),
      average_grade_adjusted_speed = LIST_IN[['splits_metric']][['average_grade_adjusted_speed']] %>% na_if_empty() %>% as.numeric(),
      average_heartrate = LIST_IN[['splits_metric']][['average_heartrate']] %>% na_if_empty() %>% as.numeric(),
      pace_zone = LIST_IN[['splits_metric']][['pace_zone']] %>% na_if_empty() %>% as.numeric()
    )
    LAPS_M$activity_type_distance <- ifelse(LAPS_M$distance_mi >= 0.5, "~1 mi", "<0.5")
    # make factors for these so that they're in the right order when we make the plot
    # LAPS_M <- LAPS_M[which(!grepl("2013|2014|2015",LAPS_M$date)),] # remove these years from the analysis
    # LAPS_M$average_heartrate[which(LAPS_M$average_heartrate<145)]=NA
    LAPS_M$moving_pace_mins_mi <- ifelse(
      LAPS_M$moving_time_mins!=0&
        !is.na(LAPS_M$moving_time_mins)&
        LAPS_M$distance_mi!=0&
        !is.na(LAPS_M$distance_mi),
      LAPS_M$moving_time_mins/LAPS_M$distance_mi,
      NA
    )
    LAPS_M$moving_pace_mins_mi_hms <- ifelse(
      LAPS_M$moving_time_secs!=0&
        !is.na(LAPS_M$moving_time_secs)&
        LAPS_M$distance_mi!=0&
        !is.na(LAPS_M$distance_mi),
      LAPS_M$moving_time_secs %>% divide_by(LAPS_M$distance_mi)%>% as.integer() %>% add(1),
      NA
    )  %>% hms::as_hms()
    LAPS_M$moving_pace_mins_km <- ifelse(
      LAPS_M$moving_time_mins!=0&
        !is.na(LAPS_M$moving_time_mins)&
        LAPS_M$distance_km!=0&
        !is.na(LAPS_M$distance_km),
      LAPS_M$moving_time_mins/LAPS_M$distance_km,
      NA
    )
    LAPS_M$moving_pace_mins_km_hms <- ifelse(
      LAPS_M$moving_time_secs!=0&
        !is.na(LAPS_M$moving_time_secs)&
        LAPS_M$distance_km!=0&
        !is.na(LAPS_M$distance_km),
      LAPS_M$moving_time_secs %>% divide_by(LAPS_M$distance_km)%>% as.integer() %>% add(1),
      NA
    )  %>% hms::as_hms()
    LAPS_M$elapsed_pace_mins_mi <- ifelse(
      LAPS_M$elapsed_time_mins!=0&
        !is.na(LAPS_M$elapsed_time_mins)&
        LAPS_M$distance_mi!=0&
        !is.na(LAPS_M$distance_mi),
      LAPS_M$elapsed_time_mins/LAPS_M$distance_mi,
      NA
    )
    LAPS_M$elapsed_pace_mins_mi_hms <- ifelse(
      LAPS_M$elapsed_time_secs!=0&
        !is.na(LAPS_M$elapsed_time_secs)&
        LAPS_M$distance_mi!=0&
        !is.na(LAPS_M$distance_mi),
      LAPS_M$elapsed_time_secs %>% divide_by(LAPS_M$distance_mi)%>% as.integer() %>% add(1),
      NA
    )  %>% hms::as_hms()
    LAPS_M$elapsed_pace_mins_km <- ifelse(
      LAPS_M$elapsed_time_mins!=0&
        !is.na(LAPS_M$elapsed_time_mins)&
        LAPS_M$distance_km!=0&
        !is.na(LAPS_M$distance_km),
      LAPS_M$elapsed_time_mins/LAPS_M$distance_km,
      NA
    )
    LAPS_M$elapsed_pace_mins_km_hms <- ifelse(
      LAPS_M$elapsed_time_secs!=0&
        !is.na(LAPS_M$elapsed_time_secs)&
        LAPS_M$distance_km!=0&
        !is.na(LAPS_M$distance_km),
      LAPS_M$elapsed_time_secs %>% divide_by(LAPS_M$distance_km)%>% as.integer() %>% add(1),
      NA
    )  %>% hms::as_hms()
    LAPS_M$BP_mi <- LAPS_M$average_heartrate %>% multiply_by(LAPS_M$moving_pace_mins_mi) %>% as.integer() %>% add(1)
    LAPS_M$BP_km <- LAPS_M$average_heartrate %>% multiply_by(LAPS_M$moving_pace_mins_km) %>% as.integer() %>% add(1)
  }else{
    LAPS_M<-NULL
  }
  # DB_raw$raw %>% map_brose1("splits_metric") %>% lapply(names) %>% unlist() %>% unique()
  OUT <- list(
    ACTIVITIES = ACTIVITIES,
    LAPS_U = LAPS_U,
    LAPS_I = LAPS_I,
    LAPS_M = LAPS_M
  )
  return(OUT)
  ##add to tables -------
  # message("added ",NAME)
  # DB_raw$tables$ACTIVITIES <- DB_raw$tables$ACTIVITIES[which(DB_raw$tables$ACTIVITIES$activity_id!=NAME),] %>% dplyr::bind_rows(ACTIVITIES)
  # DB_raw$tables$ACTIVITIES <- DB_raw$tables$ACTIVITIES[order(DB_raw$tables$ACTIVITIES$datetime ,decreasing=T),]
  # rownames(DB_raw$tables$ACTIVITIES)<-NULL
  # DB_raw$tables$LAPS_U <- DB_raw$tables$LAPS_U[which(DB_raw$tables$LAPS_U$activity_id!=NAME),] %>% dplyr::bind_rows(LAPS_U)
  # DB_raw$tables$LAPS_U<-DB_raw$tables$LAPS_U[order(DB_raw$tables$LAPS_U$start_date_local ,decreasing=T),]
  # rownames(DB_raw$tables$LAPS_U)<-NULL
  # DB_raw$tables$LAPS_I <- DB_raw$tables$LAPS_I[which(DB_raw$tables$LAPS_I$activity_id!=NAME),] %>% dplyr::bind_rows(LAPS_I)
  # DB_raw$tables$LAPS_M <- DB_raw$tables$LAPS_M[which(DB_raw$tables$LAPS_M$activity_id!=NAME),] %>% dplyr::bind_rows(LAPS_M)
  # if(ACTIVITIES$type=="Run"){
  #   DB_raw$tables$RUNS_DAILY <- DB_raw$tables$RUNS_DAILY[which(DB_raw$tables$RUNS_DAILY$date!=ACTIVITIES$date),] %>% dplyr::bind_rows(
  #     data.frame(
  #       date= ACTIVITIES$date,
  #       distance_mi= ACTIVITIES$date %>% sapply(function(x){DB_raw$tables$ACTIVITIES$distance_mi[which(DB_raw$tables$ACTIVITIES$date==x)] %>% sum()}),
  #       distance_km= ACTIVITIES$date %>% sapply(function(x){DB_raw$tables$ACTIVITIES$distance_km[which(DB_raw$tables$ACTIVITIES$date==x)] %>% sum()}),
  #       count= ACTIVITIES$date %>% sapply(function(x){which(DB_raw$tables$ACTIVITIES$date==x) %>% length()}),
  #       average_cadence= ACTIVITIES$date %>% sapply(function(x){DB_raw$tables$ACTIVITIES$average_cadence[which(DB_raw$tables$ACTIVITIES$date==x)] %>% weighted.mean(DB_raw$tables$ACTIVITIES$distance_mi[which(DB_raw$tables$ACTIVITIES$date==x)],na.rm = T)}),
  #       average_BP_mi = ACTIVITIES$date %>% sapply(function(x){DB_raw$tables$ACTIVITIES$BP_mi[which(DB_raw$tables$ACTIVITIES$date==x)] %>% weighted.mean(DB_raw$tables$ACTIVITIES$distance_mi[which(DB_raw$tables$ACTIVITIES$date==x)],na.rm = T)}),
  #       year = ACTIVITIES$year,
  #       year_day = ACTIVITIES$year_day,
  #       month = ACTIVITIES$month,
  #       month_day = ACTIVITIES$month_day,
  #       week_start = ACTIVITIES$week_start,
  #       weekday = ACTIVITIES$weekday
  #     )
  #   )
  #   DB_raw$tables$RUNS_DAILY<-DB_raw$tables$RUNS_DAILY[order(DB_raw$tables$RUNS_DAILY$date ,decreasing=T),]
  #   # DB_raw$tables$RUNS_DAILY$year<-factor(DB_raw$tables$RUNS_DAILY$year,levels = max(DB_raw$tables$RUNS_DAILY$year):min(DB_raw$tables$RUNS_DAILY$year))
  #   YEARS<-data.frame(YEAR=DB_raw$tables$RUNS_DAILY$year %>% unique())
  #   YEARS$value<-YEARS$YEAR %>% sapply(function(YEAR){
  #     (DB_raw$tables$RUNS_DAILY$distance_mi[which(DB_raw$tables$RUNS_DAILY$year==YEAR)] %>% sum(na.rm = T))
  #   })
  #   DB_raw$tables$RUNS_DAILY$year_sum <- NA
  #   DB_raw$tables$RUNS_DAILY$total_yearly_miles <- NA
  #   for(YEAR in YEARS$YEAR){
  #     DB_raw$tables$RUNS_DAILY$year_sum[which(DB_raw$tables$RUNS_DAILY$year==YEAR)] <- YEARS$value[which(YEARS$YEAR==YEAR)]
  #     DB_raw$tables$RUNS_DAILY$total_yearly_miles[which(DB_raw$tables$RUNS_DAILY$year==YEAR)] <- DB_raw$tables$RUNS_DAILY$distance_mi[which(DB_raw$tables$RUNS_DAILY$year==YEAR)]%>% rev() %>% cumsum() %>%  rev()
  #   }
  #   DB_raw$tables$RUNS_DAILY$total_miles_ever<-DB_raw$tables$RUNS_DAILY$distance_mi %>% rev() %>% cumsum() %>%  rev()
  #   DB_raw$tables$RUNS_WEEKLY <- DB_raw$tables$RUNS_WEEKLY[which(DB_raw$tables$RUNS_WEEKLY$week_start!=ACTIVITIES$week_start),] %>% dplyr::bind_rows(
  #     data.frame(
  #       week_start=ACTIVITIES$week_start,
  #       weekday = ACTIVITIES$weekday,
  #       distance_mi= ACTIVITIES$week_start %>% sapply(function(x){DB_raw$tables$ACTIVITIES$distance_mi[which(DB_raw$tables$ACTIVITIES$week_start==x)] %>% sum()}),
  #       distance_km= ACTIVITIES$week_start %>% sapply(function(x){DB_raw$tables$ACTIVITIES$distance_km[which(DB_raw$tables$ACTIVITIES$week_start==x)] %>% sum()}),
  #       count= ACTIVITIES$week_start %>% sapply(function(x){which(DB_raw$tables$ACTIVITIES$week_start==x) %>% length()}),
  #       average_moving_pace_mins_mi= ACTIVITIES$week_start %>% sapply(function(x){DB_raw$tables$ACTIVITIES$moving_pace_mins_mi[which(DB_raw$tables$ACTIVITIES$week_start==x)] %>% weighted.mean(DB_raw$tables$ACTIVITIES$distance_mi[which(DB_raw$tables$ACTIVITIES$week_start==x)],na.rm = T)}),
  #       average_HR= ACTIVITIES$week_start %>% sapply(function(x){DB_raw$tables$ACTIVITIES$average_heartrate[which(DB_raw$tables$ACTIVITIES$week_start==x)] %>% weighted.mean(DB_raw$tables$ACTIVITIES$distance_mi[which(DB_raw$tables$ACTIVITIES$week_start==x)],na.rm = T)}),
  #       average_cadence= ACTIVITIES$week_start %>% sapply(function(x){DB_raw$tables$ACTIVITIES$average_cadence[which(DB_raw$tables$ACTIVITIES$week_start==x)] %>% weighted.mean(DB_raw$tables$ACTIVITIES$distance_mi[which(DB_raw$tables$ACTIVITIES$week_start==x)],na.rm = T)}),
  #       average_BP_mi= ACTIVITIES$week_start %>% sapply(function(x){DB_raw$tables$ACTIVITIES$BP_mi[which(DB_raw$tables$ACTIVITIES$week_start==x)] %>% weighted.mean(DB_raw$tables$ACTIVITIES$distance_mi[which(DB_raw$tables$ACTIVITIES$week_start==x)],na.rm = T)})
  #     )
  #   )
  #   DB_raw$tables$RUNS_WEEKLY<-DB_raw$tables$RUNS_WEEKLY[order(DB_raw$tables$RUNS_WEEKLY$week_start ,decreasing=T),]
  #   DB_raw$tables$RUNS_MONTHLY <- DB_raw$tables$RUNS_MONTHLY[which(DB_raw$tables$RUNS_MONTHLY$month_start!=ACTIVITIES$month_start),] %>% dplyr::bind_rows(
  #     data.frame(
  #       month_start=ACTIVITIES$month_start,
  #       month = ACTIVITIES$month,
  #       month_day = ACTIVITIES$month_day,
  #       distance_mi= ACTIVITIES$month_start %>% sapply(function(x){DB_raw$tables$ACTIVITIES$distance_mi[which(DB_raw$tables$ACTIVITIES$month_start==x)] %>% sum()}),
  #       distance_km= ACTIVITIES$month_start %>% sapply(function(x){DB_raw$tables$ACTIVITIES$distance_km[which(DB_raw$tables$ACTIVITIES$month_start==x)] %>% sum()}),
  #       count= ACTIVITIES$month_start %>% sapply(function(x){which(DB_raw$tables$ACTIVITIES$month_start==x) %>% length()}),
  #       average_moving_pace_mins_mi= ACTIVITIES$month_start %>% sapply(function(x){DB_raw$tables$ACTIVITIES$moving_pace_mins_mi[which(DB_raw$tables$ACTIVITIES$month_start==x)] %>% weighted.mean(DB_raw$tables$ACTIVITIES$distance_mi[which(DB_raw$tables$ACTIVITIES$month_start==x)],na.rm = T)}),
  #       average_HR= ACTIVITIES$month_start %>% sapply(function(x){DB_raw$tables$ACTIVITIES$average_heartrate[which(DB_raw$tables$ACTIVITIES$month_start==x)] %>% weighted.mean(DB_raw$tables$ACTIVITIES$distance_mi[which(DB_raw$tables$ACTIVITIES$month_start==x)],na.rm = T)}),
  #       average_cadence= ACTIVITIES$month_start %>% sapply(function(x){DB_raw$tables$ACTIVITIES$average_cadence[which(DB_raw$tables$ACTIVITIES$month_start==x)] %>% weighted.mean(DB_raw$tables$ACTIVITIES$distance_mi[which(DB_raw$tables$ACTIVITIES$month_start==x)],na.rm = T)}),
  #       average_BP_mi= ACTIVITIES$month_start %>% sapply(function(x){DB_raw$tables$ACTIVITIES$BP_mi[which(DB_raw$tables$ACTIVITIES$month_start==x)] %>% weighted.mean(DB_raw$tables$ACTIVITIES$distance_mi[which(DB_raw$tables$ACTIVITIES$month_start==x)],na.rm = T)}),
  #       year = ACTIVITIES$year,
  #       year_day = ACTIVITIES$year_day
  #     )
  #   )
  #   DB_raw$tables$RUNS_MONTHLY<-DB_raw$tables$RUNS_MONTHLY[order(DB_raw$tables$RUNS_MONTHLY$month_start ,decreasing=T),]
  #   DB_raw$tables$RUNS_YEARLY <- DB_raw$tables$RUNS_YEARLY[which(DB_raw$tables$RUNS_YEARLY$year!=ACTIVITIES$year),] %>% dplyr::bind_rows(
  #     data.frame(
  #       year=ACTIVITIES$year,
  #       distance_mi= ACTIVITIES$year %>% sapply(function(x){DB_raw$tables$ACTIVITIES$distance_mi[which(DB_raw$tables$ACTIVITIES$year==x)] %>% sum()}),
  #       distance_km= ACTIVITIES$year %>% sapply(function(x){DB_raw$tables$ACTIVITIES$distance_km[which(DB_raw$tables$ACTIVITIES$year==x)] %>% sum()}),
  #       count= ACTIVITIES$year %>% sapply(function(x){which(DB_raw$tables$ACTIVITIES$year==x) %>% length()}),
  #       average_moving_pace_mins_mi= ACTIVITIES$year %>% sapply(function(x){DB_raw$tables$ACTIVITIES$moving_pace_mins_mi[which(DB_raw$tables$ACTIVITIES$year==x)] %>% weighted.mean(DB_raw$tables$ACTIVITIES$distance_mi[which(DB_raw$tables$ACTIVITIES$year==x)],na.rm = T)}),
  #       average_HR= ACTIVITIES$year %>% sapply(function(x){DB_raw$tables$ACTIVITIES$average_heartrate[which(DB_raw$tables$ACTIVITIES$year==x)] %>% weighted.mean(DB_raw$tables$ACTIVITIES$distance_mi[which(DB_raw$tables$ACTIVITIES$year==x)],na.rm = T)}),
  #       average_cadence= ACTIVITIES$year %>% sapply(function(x){DB_raw$tables$ACTIVITIES$average_cadence[which(DB_raw$tables$ACTIVITIES$year==x)] %>% weighted.mean(DB_raw$tables$ACTIVITIES$distance_mi[which(DB_raw$tables$ACTIVITIES$year==x)],na.rm = T)}),
  #       average_BP_mi= ACTIVITIES$year %>% sapply(function(x){DB_raw$tables$ACTIVITIES$BP_mi[which(DB_raw$tables$ACTIVITIES$year==x)] %>% weighted.mean(DB_raw$tables$ACTIVITIES$distance_mi[which(DB_raw$tables$ACTIVITIES$year==x)],na.rm = T)})
  #     )
  #   )
  #   DB_raw$tables$RUNS_YEARLY<-DB_raw$tables$RUNS_YEARLY[order(DB_raw$tables$RUNS_YEARLY$year ,decreasing=T),]
  # }
  # if(ACTIVITIES$type=="Ride"){
  #   DB_raw$tables$BIKES_WEEKLY <- DB_raw$tables$BIKES_WEEKLY[which(DB_raw$tables$BIKES_WEEKLY$week_start!=ACTIVITIES$week_start),] %>% dplyr::bind_rows(
  #     data.frame(
  #       week_start=ACTIVITIES$week_start,
  #       distance_mi= ACTIVITIES$week_start %>% sapply(function(x){DB_raw$tables$ACTIVITIES$distance_mi[which(DB_raw$tables$ACTIVITIES$week_start==x)] %>% sum()}),
  #       distance_km= ACTIVITIES$week_start %>% sapply(function(x){DB_raw$tables$ACTIVITIES$distance_km[which(DB_raw$tables$ACTIVITIES$week_start==x)] %>% sum()}),
  #       count= ACTIVITIES$week_start %>% sapply(function(x){which(DB_raw$tables$ACTIVITIES$week_start==x) %>% length()}),
  #       average_moving_pace_mins_mi= ACTIVITIES$week_start %>% sapply(function(x){DB_raw$tables$ACTIVITIES$moving_pace_mins_mi[which(DB_raw$tables$ACTIVITIES$week_start==x)] %>% weighted.mean(DB_raw$tables$ACTIVITIES$distance_mi[which(DB_raw$tables$ACTIVITIES$week_start==x)],na.rm = T)}),
  #       average_HR= ACTIVITIES$week_start %>% sapply(function(x){DB_raw$tables$ACTIVITIES$average_heartrate[which(DB_raw$tables$ACTIVITIES$week_start==x)] %>% weighted.mean(DB_raw$tables$ACTIVITIES$distance_mi[which(DB_raw$tables$ACTIVITIES$week_start==x)],na.rm = T)}),
  #       average_cadence= ACTIVITIES$week_start %>% sapply(function(x){DB_raw$tables$ACTIVITIES$average_cadence[which(DB_raw$tables$ACTIVITIES$week_start==x)] %>% weighted.mean(DB_raw$tables$ACTIVITIES$distance_mi[which(DB_raw$tables$ACTIVITIES$week_start==x)],na.rm = T)}),
  #       average_BP_mi= ACTIVITIES$week_start %>% sapply(function(x){DB_raw$tables$ACTIVITIES$BP_mi[which(DB_raw$tables$ACTIVITIES$week_start==x)] %>% weighted.mean(DB_raw$tables$ACTIVITIES$distance_mi[which(DB_raw$tables$ACTIVITIES$week_start==x)],na.rm = T)})
  #     )
  #   )
  #   DB_raw$tables$BIKES_MONTHLY <- DB_raw$tables$BIKES_MONTHLY[which(DB_raw$tables$BIKES_MONTHLY$month_start!=ACTIVITIES$month_start),] %>% dplyr::bind_rows(
  #     data.frame(
  #       month_start=ACTIVITIES$month_start,
  #       distance_mi= ACTIVITIES$month_start %>% sapply(function(x){DB_raw$tables$ACTIVITIES$distance_mi[which(DB_raw$tables$ACTIVITIES$month_start==x)] %>% sum()}),
  #       distance_km= ACTIVITIES$month_start %>% sapply(function(x){DB_raw$tables$ACTIVITIES$distance_km[which(DB_raw$tables$ACTIVITIES$month_start==x)] %>% sum()}),
  #       count= ACTIVITIES$month_start %>% sapply(function(x){which(DB_raw$tables$ACTIVITIES$month_start==x) %>% length()}),
  #       average_moving_pace_mins_mi= ACTIVITIES$month_start %>% sapply(function(x){DB_raw$tables$ACTIVITIES$moving_pace_mins_mi[which(DB_raw$tables$ACTIVITIES$month_start==x)] %>% weighted.mean(DB_raw$tables$ACTIVITIES$distance_mi[which(DB_raw$tables$ACTIVITIES$month_start==x)],na.rm = T)}),
  #       average_HR= ACTIVITIES$month_start %>% sapply(function(x){DB_raw$tables$ACTIVITIES$average_heartrate[which(DB_raw$tables$ACTIVITIES$month_start==x)] %>% weighted.mean(DB_raw$tables$ACTIVITIES$distance_mi[which(DB_raw$tables$ACTIVITIES$month_start==x)],na.rm = T)}),
  #       average_cadence= ACTIVITIES$month_start %>% sapply(function(x){DB_raw$tables$ACTIVITIES$average_cadence[which(DB_raw$tables$ACTIVITIES$month_start==x)] %>% weighted.mean(DB_raw$tables$ACTIVITIES$distance_mi[which(DB_raw$tables$ACTIVITIES$month_start==x)],na.rm = T)}),
  #       average_BP_mi= ACTIVITIES$month_start %>% sapply(function(x){DB_raw$tables$ACTIVITIES$BP_mi[which(DB_raw$tables$ACTIVITIES$month_start==x)] %>% weighted.mean(DB_raw$tables$ACTIVITIES$distance_mi[which(DB_raw$tables$ACTIVITIES$month_start==x)],na.rm = T)})
  #     )
  #   )
  #   DB_raw$tables$BIKES_YEARLY <- DB_raw$tables$BIKES_YEARLY[which(DB_raw$tables$BIKES_YEARLY$year!=ACTIVITIES$year),] %>% dplyr::bind_rows(
  #     data.frame(
  #       year=ACTIVITIES$year,
  #       distance_mi= ACTIVITIES$year %>% sapply(function(x){DB_raw$tables$ACTIVITIES$distance_mi[which(DB_raw$tables$ACTIVITIES$year==x)] %>% sum()}),
  #       distance_km= ACTIVITIES$year %>% sapply(function(x){DB_raw$tables$ACTIVITIES$distance_km[which(DB_raw$tables$ACTIVITIES$year==x)] %>% sum()}),
  #       count= ACTIVITIES$year %>% sapply(function(x){which(DB_raw$tables$ACTIVITIES$year==x) %>% length()}),
  #       average_moving_pace_mins_mi= ACTIVITIES$year %>% sapply(function(x){DB_raw$tables$ACTIVITIES$moving_pace_mins_mi[which(DB_raw$tables$ACTIVITIES$year==x)] %>% weighted.mean(DB_raw$tables$ACTIVITIES$distance_mi[which(DB_raw$tables$ACTIVITIES$year==x)],na.rm = T)}),
  #       average_HR= ACTIVITIES$year %>% sapply(function(x){DB_raw$tables$ACTIVITIES$average_heartrate[which(DB_raw$tables$ACTIVITIES$year==x)] %>% weighted.mean(DB_raw$tables$ACTIVITIES$distance_mi[which(DB_raw$tables$ACTIVITIES$year==x)],na.rm = T)}),
  #       average_cadence= ACTIVITIES$year %>% sapply(function(x){DB_raw$tables$ACTIVITIES$average_cadence[which(DB_raw$tables$ACTIVITIES$year==x)] %>% weighted.mean(DB_raw$tables$ACTIVITIES$distance_mi[which(DB_raw$tables$ACTIVITIES$year==x)],na.rm = T)}),
  #       average_BP_mi= ACTIVITIES$year %>% sapply(function(x){DB_raw$tables$ACTIVITIES$BP_mi[which(DB_raw$tables$ACTIVITIES$year==x)] %>% weighted.mean(DB_raw$tables$ACTIVITIES$distance_mi[which(DB_raw$tables$ACTIVITIES$year==x)],na.rm = T)})
  #     )
  #   )
  # }
  #not used -----
  # DB_raw$raw %>% map_brose1("map") %>% lapply(names) %>% unlist() %>% unique()
  # map = LIST_IN[['map']] %>% na_if_empty() %>% as.character(),
  # best_efforts = LIST_IN[['best_efforts']] %>% na_if_empty() %>% as.character(),
  # photos = LIST_IN[['photos']] %>% na_if_empty() %>% as.character(),
  # stats_visibility = LIST_IN[['stats_visibility']] %>% na_if_empty() %>% as.character(),
  # similar_activities = LIST_IN[['similar_activities']] %>% na_if_empty() %>% as.character(),
}
log_growth<-function(t){
  K=40/7
  N0=mean(c(DB_raw$tables$RUNS_WEEKLY$distance_mi[which(DB_raw$tables$RUNS_WEEKLY$week_start==weekprev(as.Date(Sys.Date())))],DB_raw$tables$RUNS_WEEKLY$distance_mi[which(DB_raw$tables$RUNS_WEEKLY$week_start==weekprev(weekprev(as.Date(Sys.Date()))))]),na.rm = T)/7
  DB_raw$tables$RUNS_WEEKLY$distance_mi[which(DB_raw$tables$RUNS_WEEKLY$week_start==weekprev(as.Date(Sys.Date())))]
  r=0.13/7
  (K*N0)/(N0+(K-N0)*exp(1)^(-r*(t)))
}
