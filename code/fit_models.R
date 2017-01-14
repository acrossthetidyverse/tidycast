library("dplyr")
library("ggplot2")
library("viridis")
library("broom")
library("lazyeval")
library("purrr")
library("tidyr")

allstars <- readr::read_csv("data/tidyverse_gh_stars.csv")
allstars <- group_by(allstars, package)
allstars <- mutate(allstars, age = date - min(date))

# start with a quasi Poisson glm
model_stars <- function(df){
    glm(as.formula(n_stars ~ age), data = df,
        family = "quasipoisson")
}

# maybe not use early youth
allstars <- filter(allstars, age > 50)
allstars_models <- by_slice(allstars, model_stars,
                            .to = "model")
allstars_fitted <- by_row(allstars_models, 
                          function(df){augment(df$model[[1]])},
                          .to = "augment")
allstars_fitted <- unnest(allstars_fitted, augment)

quick_look <- function(packagename){
    pkgdata <- filter_(allstars, interp(~package == packagename))
    pkgpred <- filter_(allstars_fitted,  interp(~package == packagename))
    print(ggplot() +
        geom_point(data = pkgdata,
                   aes(age, n_stars)) +
        geom_line(data = pkgpred,
                  aes(age, exp(.fitted)), col = "red")+
        geom_line(data = pkgpred,
                  aes(age, exp(.fitted + 1.76 * .fitted)), col = "red") +
            ggtitle(packagename))
}

map(unique(allstars$package), quick_look)
