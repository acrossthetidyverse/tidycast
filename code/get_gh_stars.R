library("gh")
library("lubridate")
library("dplyr")
library("purrr")

############################################################
#                                                          #
#                    get packages list                     #
#                                                          #
############################################################


source("code/packages_list.R")
tidypacks <- gsub("\\,", "", tidypacks)


############################################################
#                                                          #
#                  function to get stars                   #
#                                                          #
############################################################
astronaut <- function(packlink){
    print(packlink)
    # get data, loop because pagination
    stardates <- NULL
    
    geht <- TRUE
    page <- 1
    while(geht){
        print(page)
        stars <- try(gh(paste0("/repos/", packlink, "/stargazers"),
                        .token = Sys.getenv("GITHUB_PAT"),
                        .send_headers = c("Accept" = 'application/vnd.github.v3.star+json'),
                        page = page))
        
        geht <- stars != ""
        
        if(geht){
            stardates <- c(stardates, vapply(stars, "[[", "", "starred_at"))
            page <- page + 1
        }
        
        
    }
    
    stardates <- lubridate::ymd_hms(stardates)
    
    # make table of counts per day
    star_table <- data.frame(time = stardates)
    star_table <- mutate_(star_table, date = lazyeval::interp(~as.Date(time)))
    star_table <- group_by_(star_table, "date") %>%
        summarize_(n_stars = ~n()) 
    star_table <- mutate_(star_table, package = ~packlink)
}

############################################################
#                                                          #
#                    get and save stars                    #
#                                                          #
############################################################


allstars <- map(tidypacks, astronaut)
allstars <- bind_rows(allstars)

readr::write_csv(allstars, path = "data/tidyverse_gh_stars.csv")
