### Code Summary: ##############################################################
# This code creates a dataset of D&D characters and their exam scores.
# Also has other data such as: background, class, proficiencies, etc.
#
### Basic packages: ############################################################
repo <- "http://cran.us.r-project.org"

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
### Basic data sets: ###########################################################
# When analyzing various IQ tests and subtests, mainly the WAIS-IV, it can be 
# implied that some abilities can be related to them.
#
# Intelligence can be related to general reasoning and memory; and
# Charisma can be related to vocabulary reasoning.
#

# Number of observations:
A <- 50000

# Dice simulation
set.seed(2021, sample.kind = "Rounding")
raw_die <- replicate(2*A, {
        dice <- sample(c(1:6), 4, replace = TRUE)
        dice <- dice[order(dice)][2:4]
        sum(dice)
})

set.seed(2021, sample.kind = "Rounding")
dice_index <- createDataPartition(raw_die, list = FALSE)
int_die   <- as.numeric(raw_die[dice_index])
cha_die   <- as.numeric(raw_die[-dice_index])

rm(raw_die, dice_index)

### Races with intelligence modifiers according to the Player's Handbook races
# (+2) = Gnome 
# (+1) = High-Elf, Human, Tiefling
#
### Races with charisma modifiers according to the Player's Handbook races
# (+2) = Half-Elf, Tiefling
# (+1) = Dragonborn, Drow, Human, Lightfoot Halfling
#
# Obs.: The distribution of races in the world is solely based on the total 
# number of races, not in regional division.
# Obs.: There are 9 races, but when considering their subraces, the number grows
# to 14 "total races".
#
### Groups of races and modifiers:
# Groups:   | INT | CHA | Races:                        | Probability:
# Gnome     | +2  | +0  | "                             | 1/14
# High-Elf  | +1  | +0  | "                             | 1/14
# Human     | +1  | +1  | "                             | 1/14
# Half-Elf  | +0  | +2  | "                             | 1/14
# Tiefling  | +1  | +2  | "                             | 1/14
# Group I   | +0  | +1  | Dragonborn, Drow, L. Halfling | 3/14
# Others    | +0  | +0  | ...                           | 6/14
#

# Race ability increase table
races <- c("Gnome", "High-Elf", "Human", "Half-Elf", "Tiefling", "Group I", "Others")
race_prob <- c(1/14, 1/14, 1/14, 1/14, 1/14, 3/14, 6/14)

set.seed(2021, sample.kind = "Rounding")
race <- sample(races, A, replace = TRUE, prob = race_prob)

race_table <- data.table(Race    = races,
                         Int_inc = c(2,1,1,0,1,0,0),
                         Cha_inc = c(0,0,1,2,2,1,0))

# Race with dice and modifiers after increase
race_die <- bind_cols(race, int_die, cha_die)
colnames(race_die) <- c("Race", "Int_die", "Cha_die")
race_die <- inner_join(race_die, race_table, by = "Race") %>%
            rowwise() %>%
            mutate(Int_total = sum(c(Int_die, Int_inc)),
                   Cha_total = sum(c(Cha_die, Cha_inc)),
                   Int_mod   = floor((Int_total - 10)/2),
                   Cha_mod   = floor((Cha_total - 10)/2))
character_die <- race_die[,c(1,6,7,8,9)]

rm(race_table, race_prob, race_die)

### Backgrounds
backgrounds <- c("Acolyte", "Charlatan", "Criminal / Spy", "Entertainer",
                 "Folk Hero", "Gladiator", "Guild Artisan / Guild Merchant",
                 "Hermit", "Knight", "Noble", "Outlander", "Pirate", "Sage",
                 "Sailor", "Soldier", "Urchin")

background_table <- data.table(Background    = backgrounds,
                               Arcana        = c(0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0),
                               Deception     = c(0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0),
                               History       = c(0,0,0,0,0,0,0,0,1,1,0,0,1,0,0,0),
                               Intimidation  = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0),
                               Investigation = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
                               Nature        = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
                               Performance   = c(0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0),
                               Persuasion    = c(0,0,0,0,0,0,1,0,1,1,0,0,0,0,0,0),
                               Religion      = c(1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0))

set.seed(2021, sample.kind = "Rounding")
background_list <- sample(backgrounds, A, replace = TRUE)

# Bind the background to the character_die table
# Adding an ID column
character_id <- seq(1,A,1)
character_die <- bind_cols(character_id, character_die, background_list)
colnames(character_die) <- c("Character_ID", "Race", "Int_total", "Cha_total", 
                              "Int_mod", "Cha_mod", "Background")



### Classes
# Create class table with proficiencies
classes <- c("Barbarian", "Bard", "Cleric", "Druid", "Fighter", "Monk", "Paladin",
             "Ranger", "Rogue", "Sorcerer", "Warlock", "Wizard")

class_table <- data.table(Class         = classes,
                          Arcana        = c(0,1,0,1,0,0,0,0,0,1,1,1),
                          Deception     = c(0,1,0,0,0,0,0,0,1,1,1,0),
                          History       = c(0,1,1,0,1,1,0,0,0,0,1,1),
                          Intimidation  = c(1,1,0,0,1,0,1,0,1,1,1,0),
                          Investigation = c(0,1,0,0,0,0,0,1,1,0,1,1),
                          Nature        = c(1,1,0,1,0,0,0,1,0,0,1,0),
                          Performance   = c(0,1,0,0,0,0,0,0,1,0,0,0),
                          Persuasion    = c(0,1,1,0,0,0,1,0,1,1,0,0),
                          Religion      = c(0,1,1,1,0,1,1,0,0,1,1,1),
                          # This will be useful for probability calculations:
                          Other         = c(4,9,2,5,6,4,3,7,6,1,0,2), 
                          Choose        = c(2,3,2,2,2,2,2,3,4,2,2,2))
# Class list
set.seed(2021, sample.kind = "Rounding")
class_list <- sample(classes, A, replace = TRUE)

# Append classes to the character table
character_die <- bind_cols(character_die, class_list)
colnames(character_die)[8] <- c("Class")

# Create a table of only class and background
bc_table <- character_die[,c(7:8)]

# Separate tables in:
# Background
b_table  <- inner_join(bc_table, background_table, by = "Background")[,3:11]

# Class
c_table  <- inner_join(bc_table, class_table, by = "Class")[,3:11] 

# "Choose" and "Other" columns from class_table
co_table <- inner_join(bc_table, class_table, by = "Class")[,12:13] 

# This next chunk is a way of discovering available proficiencies for a character
# negating the intersection between background prof. and class prof.
bc_f <- c_table - b_table
bc_f[bc_f < 0] <- 0

bc_f <- bind_cols(bc_f, co_table)

prof <- colnames(bc_f)[2:11]

prof_choice <- function(x){
  s_prof <- colnames(bc_f[x,which(bc_f[x,-11] > 0)])
  s_prof <- s_prof[-length(s_prof)]
  other_c <- rep("Other", times = bc_f[x,"Other"])
  l_prof <- c(s_prof, other_c)
  
  sample(l_prof,bc_f[x,"Choose"])
}

set.seed(2021, sample.kind = "Rounding")
sel_prof <- sapply(seq(1,A), prof_choice)
sel_prof <- sapply(seq(1,A), function(x){table(sel_prof[[x]])})
sel_prof <- as.data.frame(bind_rows(sel_prof))[,-1]

sel_prof[is.na(sel_prof)] <- 0
sel_prof <- sel_prof[,order(names(sel_prof))]


bc_table <- as.data.frame(sapply(seq(1,A), function(x){sel_prof[x,] + b_table[x,]}))
bc_table <- as.data.frame(t(bc_table))
bc_table <- bind_cols(character_id,bc_table)
colnames(bc_table)[1] <- c("Character_ID")

# All proficiencies added based on background and class:
character_die <-  inner_join(character_die, bc_table, by = "Character_ID") %>%
                  rowwise() %>%
                  mutate(Arcana        = Int_mod + (2*Arcana),
                         Deception     = Cha_mod + (2*Deception),
                         History       = Int_mod + (2*History),
                         Intimidation  = Cha_mod + (2*Intimidation),
                         Investigation = Int_mod + (2*Investigation),
                         Nature        = Int_mod + (2*Nature),
                         Performance   = Cha_mod + (2*Performance),
                         Persuasion    = Cha_mod + (2*Persuasion),
                         Religion      = Int_mod + (2*Religion))

rm(b_table,background_table,bc_f,bc_table,c_table,class_table,co_table,sel_prof)
rm(prof_choice)
rm(background_list,backgrounds,cha_die,character_id,class_list,classes,int_die,
   prof,race,races,A)
### "Exam": ####################################################################
# The general intelligence exam in the D&D world, related to IQ, could be divided
# by proficiency subtests.
# 
# Hypothetically, the characters would have an exam with questions assigned to 
# each proficiency:
# Obs.: To mitigate the bias to the intelligence ability, the number of questions
# assigned to charisma will be higher
# 
# Int: 20 questions per proficiency
# Cha: 25 questions per proficiency
#
# In other words, the characters would roll, for each question, a d20 die with
# the appropriate modifier
#

int_ex <- character_die[,c(1,9,11,13,14,17)]
cha_ex <- character_die[,c(1,10,12,15,16)]

# Intelligence subtest function
int_test <- function(proficiency){
  dice_int <- sample(1:20, 20, replace = TRUE) + proficiency
  mean(dice_int)
}

# Charisma subtest function
cha_test <- function(proficiency){
  dice_cha <- sample(1:20, 25, replace = TRUE) + proficiency
  mean(dice_cha)
}

# Intelligence exam
set.seed(2021, sample.kind = "Rounding")
int_ex <- int_ex %>% mutate(Arcana        = int_test(Arcana),
                            History       = int_test(History),
                            Investigation = int_test(Investigation),
                            Nature        = int_test(Nature),
                            Religion      = int_test(Religion))

int_mean <- as.data.table(round(rowMeans(int_ex[,2:6]),2))
colnames(int_mean) <- c("int_score")
int_ex <- bind_cols(int_ex, int_mean)

# Charisma exam
set.seed(2021, sample.kind = "Rounding")
cha_ex <- cha_ex %>% mutate(Deception    = int_test(Deception),
                            Intimidation = int_test(Intimidation),
                            Performance  = int_test(Performance),
                            Persuasion   = int_test(Persuasion))

cha_mean <- as.data.table(round(rowMeans(cha_ex[,2:5]),2))
colnames(cha_mean) <- c("cha_score")
cha_ex <- bind_cols(cha_ex, cha_mean)

# Final exam table
set.seed(2021, sample.kind = "Rounding")
full_exam <- inner_join(int_ex, cha_ex, by = "Character_ID")[,c(1,7,12)] %>%
             mutate(full_score = round(mean(c(int_score, cha_score)),2))

# Appending exam scores to character_die table
character_die <- inner_join(character_die, full_exam, by = "Character_ID")
rm(full_exam, cha_ex, int_ex, cha_mean, int_mean)
rm(cha_test,int_test)

# Full Exam Histogram
character_die %>% ggplot() +
                  geom_histogram(aes(full_score), fill = "#64242e") +
                  scale_x_continuous(breaks = 7:16) +
                  theme_bw()

# write.csv(character_die, file="character_info")