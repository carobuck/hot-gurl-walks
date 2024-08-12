#install.packages('rStrava')
library(rStrava)
library(tidyverse)

# Try rStrava package, because other method was not working with API access tokens (docs written before strava changed their APIso I might've been doing something wrong. or out of date)

app_name <- 'r_api' # chosen by user
app_client_id  <- Sys.getenv("app_client_id")
app_secret <- Sys.getenv("app_secret")

# create the authentication token
stoken <- httr::config(token = strava_oauth(app_name, app_client_id, app_secret, app_scope="activity:read_all"))

# Pull my data
# get activities by date range, then filter based on name ('hot gurl' in title), then get more detailed stream info for each activity
my_acts <- get_activity_list(stoken, after = as.Date('2024-07-15'))
act_data <- compile_activities(my_acts) 

# streams give moment by moment (by second??) level of data; use the og activity df for some walk info
act_data %>% 
  filter(str_like(name,'%ot g%')) -> walk_data

act_data %>% 
  #filter(type == 'Walk') %>%
  # Could've used activity type, but more exciting to use new func for searching for 'like' strings lol
  filter(str_like(name,'%ot g%')) %>% 
  get_activity_streams(stoken) %>%
  inner_join(walk_data %>% select(name,id)) -> walk_data_streams



# Visualize/play w/ the data ----
## does ruby walk faster at the beginning? ----
# play around w/ cadence, altitude, velocity_smooth
walk_data_streams %>% 
  filter(cadence > 0) %>%
  ggplot(aes(x=time,y=velocity_smooth,color=name)) +
  geom_line(aes(y=altitude),color='black') +
  #geom_line() +
  geom_smooth(se=FALSE) +
  facet_wrap(name~.)
  #facet_grid(name~.,scales='free')
  
## How far across Death Valley have we walked? ----
# Using Death Valley as the hottest place in the world
# geojson source: https://public-nps.opendata.arcgis.com/datasets/nps-boundary-1/explore?location=38.411995%2C-113.002501%2C6.16
my_sf <- sf::read_sf('NPS.geojson') %>%
  filter(UNIT_NAME == 'Death Valley National Park')

ggplot(my_sf) +
  geom_sf(fill='green') +
  geom_hline(yintercept = 4299999)
  
# TODO: maybe make a Line geometry with points for x miles we've walked?? or add a line on top?? idk. 
# https://docs.mapbox.com/android/java/api/libjava-geojson/5.8.0/com/mapbox/geojson/LineString.html
# https://www.nps.gov/deva/faqs.htm#:~:text=Death%20Valley%20is%20about%20140,Saratoga%20Springs%20in%20the%20south.
# Death valley approx 140 miles long (225.308 km)
death_valley_length <- 225.308
sum(walk_data$distance) # 52.8397km (distance is in km)
 
pct_done <- sum(walk_data$distance)/death_valley_length
# how to color this % of the shapefile? like a goal to be filled? 

# try getting horiz lat that would be x% of the way up? approx (using min and max lat)
# (got these values from geometry bounding box)
# Used chatGPT to convert the non-degree lat/long to degree version so maybe will plot??
lat_min <- 35.5910 # -13135820
lat_max <- 37.3495 # -12943850

# og values from bb with the og projection
lat_min <-  -13135820
lat_max <-  -12943850
#lat <- 
lat_min*(1+pct_done)
  abs(lat_max-lat_min)*pct_done + lat_min # I still think this is correct?? but projection is making it weird?? 

# get something close to this # 4299999 for the nps mapping (took a lot of guess and check to see where to get line at a reasonable place)
# there is something very VERY weird going on under the hood for projections I am guessing
4299999
36.00341
## TRASH
my_sf$geometry

bbox_list <- lapply(st_geometry(my_sf), st_bbox)

#To df
maxmin <- as.data.frame(matrix(unlist(bbox_list),nrow=nrow(myshape)))
