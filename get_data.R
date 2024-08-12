# NOTE: THIS CODE DOES NOT FULLY WORK AS-IS. 

# Using some strava api code from here: https://rviews.rstudio.com/2021/11/22/strava-data/
# secrets/renv fyi: https://cran.r-project.org/web/packages/httr/vignettes/secrets.html
file.edit("~/.Renviron")

#library(tarchetypes)
#library(conflicted)
library(tidyverse)
library(jsonlite)
#library(targets)
#library(httpuv)
library(httr)
#library(pins)
#library(fs)

# Define funcs to set up Strava connection and get all activities ----
define_strava_app <- function() {
  oauth_app(
    appname = "r_api",
    key = Sys.getenv("STRAVA_KEY"),
    secret = Sys.getenv("STRAVA_SECRET")
  )
}
define_strava_endpoint <- function() {
  oauth_endpoint(request = NULL,
                 authorize = "https://www.strava.com/oauth/authorize",
                 access = "https://www.strava.com/oauth/token")
}
define_strava_sig <- function(endpoint, app) {
  oauth2.0_token(
    endpoint,
    app,
    scope = "activity:read_all,activity:read,profile:read_all",
    type = NULL,
    use_oob = FALSE,
    as_header = FALSE,
    use_basic_auth = FALSE,
    cache = FALSE
  )
}

read_all_activities <- function(sig) {
  activities_url <- parse_url("https://www.strava.com/api/v3/athlete/activities")
  
  act_vec <- vector(mode = "list")
  df_act <- tibble::tibble(init = "init")
  i <- 1L
  
  while (nrow(df_act) != 0) {
    r <- activities_url %>%
      modify_url(query = list(
        access_token = sig$credentials$access_token[[1]],
        page = i
      )) %>%
      GET()
    
    df_act <- content(r, as = "text") %>%
      fromJSON(flatten = TRUE) %>%
      as_tibble()
    if (nrow(df_act) != 0)
      act_vec[[i]] <- df_act
    i <- i + 1L
  }
  
  df_activities <- act_vec %>%
    bind_rows() %>%
    mutate(start_date = ymd_hms(start_date))
}

# Call the funcs ----
app <- define_strava_app()
endpoint <- define_strava_endpoint()
my_sig <- define_strava_sig(endpoint,app)
# TODO: i think close(??) to figuring out why this auth code doesn't work?? maybe?? bit unclear for why it is 401 error. got it resolved on rStrava package, so I suspect some token/id is wrong somewhere? I get the GUI auth screen to pop up now so idk. 

# Get all activities and wrangle ----

read_all_activities(my_sig)	
