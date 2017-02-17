library("cranlogs")
library("lubridate")
library("dplyr")

############################################################
#                                                          #
#       minimal date from Github stars as start date       #
#                                                          #
############################################################

allstars <- readr::read_csv("data/tidyverse_gh_stars.csv")
min_date <- min(allstars$date)
rm(allstars)

############################################################
#                                                          #
#                    get packages list                     #
#                                                          #
############################################################


source("code/packages_list.R")
tidypacks <- gsub("\\,", "", tidypacks)
tidypacks <- lapply(tidypacks, strsplit, "\\/")
tidypacks <- lapply(tidypacks, "[[", 1)
tidypacks <- lapply(tidypacks, "[", 2)
tidypacks <- unlist(tidypacks)
tidypacks <- c(tidypacks)
############################################################
#                                                          #
#                    get CRAN downloads                    #
#                                                          #
############################################################
crandl <- cran_downloads(tidypacks, 
                         from = min_date,
                         to = as.character(Sys.Date()))

crandl <- cran_downloads(NULL, 
                         from = min_date,
                         to = as.character(Sys.Date())) %>%
    bind_rows(crandl)

readr::write_csv(crandl, path = "data/tidyverse_cran_downloads.csv")
