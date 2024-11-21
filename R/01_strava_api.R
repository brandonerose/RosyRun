#' @import RosyUtils
strava_token<-function(key=Sys.getenv("strava_key"),secret=Sys.getenv("strava_secret")){
  if(key=="")stop("Set you strava key in the argument of the function or even better in your environment with `Sys.setenv(strava_key = 'YoUr_never_shaRe_key_FroM_stava_website')` or using `usethis::edit_r_environ()`")
  if(secret=="")stop("Set you strava key in the argument of the function or even better in your environment with `Sys.setenv(strava_secret = 'YoUr_never_shaRe_secret_FroM_stava_website')` or using `usethis::edit_r_environ()`")
  httr::oauth2.0_token(
    endpoint=httr::oauth_endpoint(
      request = NULL,
      authorize = "https://www.strava.com/oauth/authorize",
      access = "https://www.strava.com/oauth/token"
    ),
    app = httr::oauth_app(
      appname = "strava",
      key = as.numeric(key),
      secret = as.character(secret)
    ),
    as_header = FALSE,
    scope = "activity:read_all"
  )
}
get_sum_activities<-function(DB,key=Sys.getenv("strava_key"),secret=Sys.getenv("strava_secret"),recent_only = T){
  i <- 1
  done <- FALSE
  nrow(check_RosyRun_dir(DB)) %>% message(" IDs before...")
  CONNECTED<-F
  while ( ! done) {
    req <- httr::GET(
      url = "https://www.strava.com/api/v3/activities",
      config = strava_token(key = key,secret = secret),
      query = list(per_page = 200, page = i)
    )
    if( ! httr::http_error(req)){
      CONNECTED<-T #bounce up message that we connected
      OUT<-httr::content(req, as = "parsed")
      if (length(OUT)>0) {
        IDs<-OUT %>% purrr::map('id') %>% unlist()
        message(length(IDs),' IDs checked')
        IDs<-IDs[which(!file.exists(file.path(DB$dir_path,'input','json',paste0("sum_",IDs,'.json'))))]
        message(length(IDs),' ID(s) new')
        for(ID in IDs){
          OUT[[which(IDs==ID)]] %>% jsonlite::toJSON() %>% write(file.path(DB$dir_path,'input','json',paste0("sum_",ID,'.json')))
        }
      }
      if( ! recent_only){
        i <- i +1
      }else{
        done<-T
      }
    }else{
      done <- TRUE
      message("Incomplete. Try again in 15 mins or tomorrow if you've exceeded 1,000 calls today.")
      message(httr::http_status(req)$message)
    }
  }
  message('...',nrow(check_RosyRun_dir(DB))," IDs after!")
  return(CONNECTED)
}
get_det_activities<-function(DB,key=Sys.getenv("strava_key"),secret=Sys.getenv("strava_secret")){
  IDs <- check_RosyRun_dir(DB,silent = F)
  error <- F
  IDs_get<-IDs$ID[which(!IDs$det)]
  pb <- progress::progress_bar$new(
    format = "  downloading from Strava [:bar] :percent ETA: :eta",
    total = length(IDs_get), clear = FALSE, width= 60)
  while(length(IDs_get)>0 & ! error){
    ID<-IDs_get[1]
    done <- FALSE
    req <- httr::GET(
      url = paste0("https://www.strava.com/api/v3/activities/",ID),
      config = strava_token(key = key,secret = secret)
    )
    if( ! httr::http_error(req)){
      OUT<-httr::content(req, as = "parsed")
      OUT %>% jsonlite::toJSON() %>% write(file.path(DB$dir_path,'input','json',paste0("det_",ID,'.json')))
      IDs_get<-IDs_get[-1]
      pb$tick()
    }else{
      error<-T
      message(httr::http_status(req)$message)
      message("Incomplete. Try again in 15 mins or tomorrow if you've exceeded 1,000 calls today.")
      message(length(IDs_get),' activities missing')
    }
  }
  if(! error){
    message("Complete! Up to date!")
    check_RosyRun_dir(DB,silent = F)
  }
}
