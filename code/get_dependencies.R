#tidyverse depenencies
library(devtools)
library(igraph)
library(tidyverse)
library(forcats)
library(ggraph)
library(ggrepel)
source("./code/packages_list.R")

tidypacks_df <- 
    as_tibble(tidypacks) %>%
    separate(value, into = c("user","package"), sep = "/")
    
#How many packages are dependent on the tidyverse, exluding the tidyverse?
tidy_revdeps <- map(tidypacks_df$package, revdep)
all_revdeps <- 
    reduce(tidy_revdeps, union) %>%
    setdiff(tidypacks_df$package)
n_deps <- length(all_revdeps)

#How are these dependencies ditributed?
tidy_revdep_count <- 
    map2(tidy_revdeps, tidypacks_df$package, setdiff) %>%
    map(length)
names(tidy_revdep_count) <- tidypacks_df$package

#plot depenency distribution
tidy_revdep_count_df <- 
    tidy_revdep_count %>%
    as_tibble() %>%
    gather(key = "package", value = "dependencies", matches("*"))

tidy_revdep_count_df %>%    
    ggplot( aes( x = fct_reorder(factor(package), dependencies, .desc = TRUE),
                 y = dependencies)) +
        xlab("package") +
        ylab("Number of CRAN dependencies") +
        geom_bar(stat = "identity") +
        theme_minimal()

#CRAN depenencies vs github stars
tidy_stars <- read_csv("./data/tidyverse_gh_stars.csv")
tidy_stars %>% 
    group_by(package) %>%
    summarise(n_stars = sum(n_stars)) %>%
    separate(package, into = c("user","package"), sep = "/") %>%
    left_join(tidy_revdep_count_df) %>%
    ggplot(aes(x = n_stars, y = dependencies)) +
        geom_point() +
        geom_text_repel(aes(label = package)) +
        theme_minimal() +
        geom_abline(slope = 1)

#How do dependencies flow within the tidyverse?
tidy_deps <- map(tidypacks_df$package, package_deps)
tidy_deps_list <- 
    map(tidy_deps, `[[`, "package") %>%
    map(intersect, tidypacks_df$package) %>%
    map2(tidypacks_df$package, setdiff)  #remove self dependency
names(tidy_deps_list) <- tidypacks_df$package

#create a graph
tidy_dep_graph <-
    tidy_deps_list %>%
    map(match, tidypacks_df$package) %>%
    graph_from_adj_list()
V(tidy_dep_graph)$name <- tidypacks_df$package    
    
#Plot the graph
ggraph(tidy_dep_graph, layout = 'fr', circular = FALSE) +
    geom_edge_fan(aes(start_cap = label_rect(node1.name),
                      end_cap = label_rect(node2.name)),
                      arrow = arrow(length = unit(2, 'mm'))) +
    geom_node_label(aes(label = name, fill = degree(tidy_dep_graph, mode = "in"))) +
    scale_fill_distiller(name = "Reverse Dependencies", 
                         palette="YlOrBr", 
                         breaks = seq(0,50,2)) +    
    ggforce::theme_no_axes()
    





