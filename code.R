### Code Summary: ##############################################################
# 
# 
# 
# 
### Basic packages: ############################################################
repo <- "http://cran.us.r-project.org"

# Required packages:
if(!require(data.table))    install.packages("data.table",   repos = repo)
if(!require(tidyverse))     install.packages("tidyverse",    repos = repo)
if(!require(caret))         install.packages("caret",        repos = repo)
if(!require(dplyr))         install.packages("dplyr",        repos = repo)

library(data.table)
library(tidyverse)
library(caret)
library(dplyr)

### Basic data sets: ###########################################################

B <- 10000

# Dice simulation
set.seed(2021, sample.kind = "Rounding")
raw_intelligence <- replicate(B, {
                    dice <- sample(c(1:6), 4, replace = TRUE)
                    dice <- dice[order(dice)][2:4]
                    sum(dice)
})

# Races with intelligence modifiers according to the Player's Handbook races
# (+2) = Gnome 
# (+1) = High-Elf, Human, Tiefling
#
# Obs.: The distribution of races in the world is solely based on the total 
# number of races.

# Adding race modifiers to the list
set.seed(2021, sample.kind = "Rounding")
race <- sample(c(0,1,2), B, replace = TRUE, prob = c(5/9, 3/9, 1/9))

### Random Choice: #############################################################
# Race increment is assigned randomly with the dice 

# Final distribution of the intelligence raw score 
race_int_raw <- as.data.frame(race + raw_intelligence)
colnames(race_int_raw) <- c("Raw_Score")

mean(race_int_raw$Raw_Score)
sd(race_int_raw$Raw_Score)

race_int_raw %>% ggplot(aes(x = Raw_Score)) + 
                 geom_histogram(bins = 18) + 
                 scale_x_continuous(breaks = 3:20) +
                 theme_bw()

quantile(race_int_raw$Raw_Score, probs = c(.001, .02, .16, .50,
                                                        .84, .98, .999))

# Final distribution of the intelligence modifiers 
race_int_mod <- as.data.frame(floor((race + raw_intelligence - 10)/2))
colnames(race_int_mod) <- c("Modifier")

mean(race_int_mod$Modifier)
sd(race_int_mod$Modifier)

race_int_mod %>% ggplot(aes(x = Modifier)) + 
                  geom_histogram(bins = 10) + 
                  scale_x_continuous(breaks = -4:5) +
                  theme_bw()

quantile(race_int_mod$Modifier, probs = c(.001, .02, .16, .50,
                                           .84, .98, .999))

### Biased Choice: #############################################################
# Better race increments are assigned to the best dice

b_raw_intelligence <- raw_intelligence[order(raw_intelligence)]
b_race             <- race[order(race)]

b_race_int_raw <- as.data.frame(b_race + b_raw_intelligence)
colnames(b_race_int_raw) <- c("Raw_Score")

mean(b_race_int_raw$Raw_Score)
sd(b_race_int_raw$Raw_Score)

b_race_int_raw %>% ggplot(aes(x = Raw_Score)) + 
  geom_histogram(bins = 18) + 
  scale_x_continuous(breaks = 3:20) +
  theme_bw()

quantile(b_race_int_raw$Raw_Score, probs = c(.001, .02, .16, .50,
                                           .84, .98, .999))



