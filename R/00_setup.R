#' @import RosyUtils
#' @title setup_RosyRun
#' @return DB object
#' @export
setup_RosyRun <- function(short_name = "run" ,dir_path = getwd()){
  DB <- RosyDB::setup_DB(
    short_name = short_name,
    dir_path = dir_path
  )
  dir.create(file.path(dir_path,"input","json"),showWarnings = F)
  dir.create(file.path(dir_path,"output"),showWarnings = F)
  return(DB)
}
check_RosyRun_dir<-function(DB,silent=T){
  IDs<-list.files(file.path(DB$dir_path,'input','json'))
  IDs<-IDs[which(grepl('sum_|det_',IDs))]
  IDs_sum<-gsub("sum_|\\.json","",IDs[which(grepl('sum_',IDs))])
  IDs_det<-gsub("det_|\\.json","",IDs[which(grepl('det_',IDs))])
  IDs<-gsub("det_|sum_|\\.json","",IDs)%>% unique()
  IDs<-data.frame(ID= IDs,sum=IDs%in%IDs_sum,det=IDs%in%IDs_det)
  if(any(!IDs$sum))warning("detailed record present without a summary file. Was something deleted? Re-run get_sum")
  if( ! silent){
    message(nrow(IDs), ' Activity IDs on Strava')
    message(sum(!IDs$det), ' Activities to download ...')
  }
  return(IDs)
}
