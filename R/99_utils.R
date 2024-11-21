map_brose0 <- function(LIST,FUN){purrr::map_depth(.x=LIST,.depth = 0,.f=FUN,.null=NA)}
map_brose1 <- function(LIST,FUN){purrr::map_depth(.x=LIST,.depth = 1,.f=FUN,.null=NA)}
map_brose2 <- function(LIST,FUN){purrr::map_depth(.x=LIST,.depth = 2,.f=FUN,.null=NA)}
map_brose3 <- function(LIST,FUN){purrr::map_depth(.x=LIST,.depth = 3,.f=FUN,.null=NA)}
map_brose4 <- function(LIST,FUN){purrr::map_depth(.x=LIST,.depth = 4,.f=FUN,.null=NA)}
na_if_empty<-function(x){if(length(x)>0){x}else{NA}}
weekstart <- function(x) {7 * floor(as.numeric(x-1+4)/7) + as.Date(1-4, origin="1970-01-01")}
weekend <- function(x) {7 * floor(as.numeric(x-1+4)/7) + as.Date(1-4, origin="1970-01-01")+6}
weekprev <- function(x) {weekstart(x)-lubridate::days(7)}
monthstart <- function(x) {x %>% strtrim(8) %>% paste0("01") %>% as.Date()}
monthend <- function(x) {ceiling_date(x, "month") - lubridate::days(1)}
monthprev <- function(x) {monthstart(x)-months(1)}
yearstart <- function(x) {x %>% strtrim(4) %>% paste0("-01-01") %>% as.Date()}
yearend <- function(x) {x %>% strtrim(4) %>% paste0("-01-01") %>% as.Date()+lubridate::years(1)-1}
yearprev <- function(x) {yearstart(x)-lubridate::years(1)}
yeardays <- function(x) {sapply(1:12, function(z){yearstart(x) %>% strtrim(4) %>% paste0("-",stringr::str_pad(z,2,pad=0),"-01") %>% lubridate::days_in_month()}) %>% sum()}
pace_character<-function(pace){
  pace %>% hms::as_hms() %>% as.character() %>% strsplit(":") %>% sapply(function(ROW){
    paste0(as.integer(ROW[2]),":",stringr::str_pad(as.integer(ROW[3]),side = "left",width = 2,pad = 0)," min/mi")
  }) %>% as.character()
}
