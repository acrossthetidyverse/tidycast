library(tidyverse)
library(ggrepel)

#github stars
tidy_stars <- read_csv("./data/tidyverse_gh_stars.csv")
tidy_stars_growth <-
    tidy_stars %>%
    group_by(package) %>%
    mutate(cummulative_stars = cumsum(n_stars),
           days_alive = date - min(date))

tidy_stars_growth %>% 
    ggplot(aes(x = days_alive, y = cummulative_stars)) +
        geom_path() + 
        facet_wrap(~package)

#first year
tidy_stars_growth %>% 
    filter(days_alive <= 365) %>%
    ggplot(aes(x = days_alive, y = cummulative_stars)) +
    geom_path() + 
    facet_wrap(~package)

#CRAN downloads
tidy_cran_dls <- read_csv("data/tidyverse_cran_downloads.csv")
tidy_dl_growth <-
    tidy_cran_dls %>% 
    group_by(package) %>%
    filter(count >= 1) %>%
    mutate(cummulative_dls = cumsum(count),
           days_alive = date - min(date))

tidy_dl_growth %>% 
    ggplot(aes(x = days_alive, y = cummulative_dls)) +
    geom_path() + 
    facet_wrap(~package)

    