#hobo-summary.R
#script used to process raw hobo data and make summary table including
#heating duration and temperature integration 
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)

TZ = "CST6CDT"

# need the hobo case number and measuremnet location

read_hobo_file <- function(filename) {
  label <- str_sub(filename, 14, -9)
  hobo <- read.csv(filename, skip=2, header=FALSE)
  
  names(hobo)[1:3] <- c("row", "time", "temp")
  hobo <- hobo %>% select(time, temp) %>%
    # we use floor_date() below to round to seconds so we can line up our
    # measurements across HOBOs
    mutate(time = floor_date(mdy_hms(time, tz=TZ), "second"))
  hobo$hobo_ID <- str_sub(label, 1, -2)
  hobo$location <- str_sub(label, -1)
  return(hobo) #change timezone
}

concat_hobo_files <- function(filelist){
  l <- lapply(filelist, read_hobo_file)
  r <- bind_rows(l)
  return(r)
}

all_hobo <- concat_hobo_files(list.files("../data/hobo", full.names=TRUE))

#get tree-hobo matching file, fire start and end time 

match_id <- read.csv("../data/tree-hobo-id.csv", stringsAsFactors = FALSE, 
                     na.strings = c(" "))
#convert hobo_ID to character
match_id <- match_id %>% mutate(hobo_ID = as.character(hobo_ID))

# fire start time stored in weather file
weather <- read.csv("../data/weather.csv", stringsAsFactors = FALSE, 
                    na.strings = c(" "))

fire_time <- weather %>% select(date, unit)


#function to extract start and end time of each fire
get_time <- function(fire.unit, password) {
  if (password == 1) {
    return(weather$time[which(weather$unit == fire.unit)[1]])
  }
  else if (password == 2) {
    return(weather$time[last(which(weather$unit == fire.unit))])
  }
}

fire_time$start <- sapply(fire_time$unit, get_time, 1)
fire_time$end <- sapply(fire_time$unit, get_time, 2)
fire_time <- fire_time[!duplicated(fire_time), ]

#combine id and fire time by unit and date
time_id <- match_id %>% left_join(fire_time, by = "unit") %>% 
  mutate(start = mdy_hm(str_c(date, " ", start), tz = TZ)) %>% 
  mutate(end = mdy_hm(str_c(date, " ", end), tz = TZ))
                        
#add half hour buffer to the end time 
time_id <- time_id %>% mutate(end = end + minutes(30))
time_id$interval <- interval(time_id$start, time_id$end)

##use interval and hobo_id to match hobo measurement to the correct tree_id that
## it was measuring 
get_tree_id <- function(time, hobo) {
  t.matches <- time %within% time_id$interval
  if(!any(t.matches)) return(NA)
  else{ hobo.match <- time_id$hobo_ID[t.matches]
        tree_id <- time_id$tree_ID[t.matches]
        return(tree_id[match(hobo, hobo.match)]) }
  }


all_hobo$tree_ID <- mapply(get_tree_id, time = all_hobo$time, 
                           hobo = all_hobo$hobo_ID)

#throw away data outside the fire
all_hobo <- all_hobo %>% filter(!is.na(tree_ID))

#summary
threshold <- 60
tempsec.sum <- all_hobo %>% group_by(tree_ID, location) %>%
  summarise(dur = sum(temp > threshold),
            degsec = sum(temp[temp > threshold]),
            peak.temp = max(temp, na.rm=TRUE),
            peak.time = time[which(peak.temp == temp)[1]],
            num.NA = sum(is.na(temp)))


#clean env
rm("fire_time", "match_id", "contact_hobo_files", "get_time", "get_tree_id",
   "read_hobo_file")
