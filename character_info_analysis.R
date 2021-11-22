### Code Summary: ########################################################################
#
#
#
### Basic packages: ######################################################################
repo <- "http://cran.us.r-project.org"
github <- "https://raw.githubusercontent.com/hcasellato/"

# Required packages:
if(!require(data.table)) install.packages("data.table", repos = repo, dependencies = TRUE)
if(!require(tidyverse))  install.packages("tidyverse",  repos = repo, dependencies = TRUE)
if(!require(caret))      install.packages("caret",      repos = repo, dependencies = TRUE)
if(!require(dplyr))      install.packages("dplyr",      repos = repo, dependencies = TRUE)

library(data.table)
library(tidyverse)
library(caret)
library(dplyr)

rm(repo)

### Data Cleaning: #######################################################################
# Download file
urla <- "dnd5e_intelligence_distribution/main/character_info"
dl <- tempfile()

character_info_raw <- download.file(url = paste(github,urla, sep = ""), destfile = dl)

setwd(getwd())
character_info <- read.csv("character_info")[2:21]

rm(github, urla, dl, character_info_raw)

# Raw division by race, background, and class
raw_race <- character_info[,c(2,18,19,20)]

raw_back <- character_info[,c(7,18,19,20)]

raw_class <- character_info[,c(8,18,19,20)]

# Summarized division by race, background, and class
su_race <- character_info[,c(2,18,19,20)] %>% 
           group_by(Race) %>% 
           mutate(int_score   = mean(int_score),
                  cha_score   = mean(cha_score),
                  full_score  = mean(full_score)) %>%
           distinct()

su_back <- character_info[,c(7,18,19,20)] %>% 
           group_by(Background) %>% 
           mutate(int_score   = mean(int_score),
                  cha_score   = mean(cha_score),
                  full_score  = mean(full_score)) %>%
           distinct()

su_class <- character_info[,c(8,18,19,20)] %>% 
            group_by(Class) %>% 
            mutate(int_score   = mean(int_score),
                   cha_score   = mean(cha_score),
                   full_score  = mean(full_score)) %>%
            distinct()

### Data Analysis: #######################################################################
group <- c("Race", "Background", "Class")
test <- c("int_score", "cha_score", "full_score")

## Density and quantiles
mean_ci <- mean(character_info$full_score)
sd_ci   <- sd(character_info$full_score)
xval    <- round(sapply(c(-3:3), function(x){mean_ci + x*sd_ci}),2)
qu_ci   <- quantile(character_info$full_score)

character_info %>% ggplot(aes(full_score)) + geom_density() + 
                                             scale_x_continuous(breaks = xval)

## Boxplot
box_func <- function(x,y){
  character_info %>% ggplot(aes(reorder(character_info[[x]],character_info[[y]]), character_info[[y]])) + geom_boxplot() +
    labs(x = str_to_title(x) , y = str_to_title(y))
}

# Full score by group
lapply(c(1:3), function(x){box_func(group[x],test[3])})

##########################################################################################