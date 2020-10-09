
## Assignment 1: Descriptive data

### Part 1: Reproduce a table (from Hunt et.al. (2019))


#### load packages ####
library(tidyverse)


#### Import online data sets ####

download.file(url = "https://ndownloader.figstatic.com/files/14702420", 
              destfile = "./data/hypertrophy.csv")

hypertrophy <- read_csv("./data/hypertrophy.csv")

#### Explore the data set ####

# View the full data set
View(hypertrophy)

# Show summary statistics
summary(hypertrophy)

# Glimpse the data
glimpse(hypertrophy)

#### make a variable containing the variables in the data set that we are interested in ####

var_interest <- c("SUB_ID", "GROUP", "CLUSTER", "AGE", "T1_BODY_MASS",
                  "PERCENT_TYPE_II_T1", "Squat_3RM_kg", "DXA_LBM_1", 
                  "DXA_FM_T1", "SQUAT_VOLUME")

hyp1 <- hypertrophy %>%
  select(all_of(var_interest)) %>%
  print()
# Specify all_of(var_interest) to select variables correctly

# Check the new data set
glimpse(hyp1)

#### Group by and summarise ####

hyp1 %>%
  group_by(CLUSTER) %>%
  print()

# The data contains NA

# Use the function is.na to test if we have any NA

var <- c("LOW", "HIGH", NA)

# Test if any are NA

is.na(var)

# Reverse the test with !is.na

!is.na(var)

hyp1 %>%
  filter(is.na(CLUSTER)) %>%
  print()

hyp1 %>%
  filter(!is.na(CLUSTER)) %>%
  print()

# Now the data set only contains participants from the
# two groups (CLUSTER)

# Find the mean age in the two groups

hyp1 %>%
  filter(!is.na(CLUSTER)) %>%
  group_by(CLUSTER) %>%
  summarise(age.m = mean(AGE)) %>%
  print()

# Find the sd for this variable (both mean and sd for CLUSTER)

hyp1 %>%
  filter(!is.na(CLUSTER)) %>%
  group_by(CLUSTER) %>%
  summarise(age.m = mean(AGE),
            age.st = sd(AGE)) %>%
  print()


# unselect the GROUP variable

hyp1 %>%
  filter(!is.na(CLUSTER)) %>%
  group_by(CLUSTER) %>%
  select(-GROUP) %>%
  print()

# change the data set from a wide format to a long format

hyp1 %>%
  filter(!is.na(CLUSTER)) %>%
  group_by(CLUSTER) %>%
  select(-GROUP) %>%
  pivot_longer(names_to = "variable",
               values_to = "value",
               cols = AGE:SQUAT_VOLUME) %>%
  print()


# group the data set by CLUSTER, and find mean and sd in these to groups
# from all the variables

hyp1 %>%
  filter(!is.na(CLUSTER)) %>%
  
  select(-GROUP) %>%
  pivot_longer(names_to = "variable",
               values_to = "value",
               cols = AGE:SQUAT_VOLUME) %>%
  
  group_by(CLUSTER) %>%
  summarise(m = mean(value),
            s = sd(value)) %>%
  print()

# this was a high mean and sd, because of the variable SQUAT_VOLUME

# group the data set by CLUSTER and variable, and find the mean and sd

hyp1 %>%
  filter(!is.na(CLUSTER)) %>%
  
  select(-GROUP) %>%
  pivot_longer(names_to = "variable",
               values_to = "value",
               cols = AGE:SQUAT_VOLUME) %>%
  
  group_by(CLUSTER, variable) %>%
  summarise(m = mean(value),
            s = sd(value)) %>%
  print()

# make a new variable with m and s together with the function mutate and paste
# (the function paste converts numbers to text)

hyp1 %>%
  filter(!is.na(CLUSTER)) %>%
  
  select(-GROUP) %>%
  pivot_longer(names_to = "variable",
               values_to = "value",
               cols = AGE:SQUAT_VOLUME) %>%
  
  group_by(CLUSTER, variable) %>%
  summarise(m = mean(value),
            s = sd(value)) %>%
  mutate(ms = paste(round(m, 1), 
                    " (",
                    round(s, 1), 
                    ")", sep = "")) %>%
  print()

# Change the variable names

hyp1 %>%
  filter(!is.na(CLUSTER)) %>%
  
  select(-GROUP) %>%
  pivot_longer(names_to = "variable",
               values_to = "value",
               cols = AGE:SQUAT_VOLUME) %>%
  
  group_by(CLUSTER, variable) %>%
  summarise(m = mean(value),
            s = sd(value)) %>%
  mutate(ms = paste(round(m, 1), 
                    " (",
                    round(s, 1), 
                    ")", sep = ""),
         variable = factor(variable, 
                           levels = c("AGE", 
                                      "T1_BODY_MASS",
                                      "DXA_LBM_1",
                                      "DXA_FM_T1",
                                      "PERCENT_TYPE_II_T1",
                                      "Squat_3RM_kg", 
                                      "SQUAT_VOLUME"),
                           labels = c("Age (years)", 
                                      "Body mass (kg)",
                                      "LBM (kg)",
                                      "FM (kg)",
                                      "Type II fiber (%)",
                                      "3RM back squat (kg)",
                                      "Total back squat training volume (kg) from weeks 1 to 6"))) %>%
  print()

# unselect the variable m and s

hyp1 %>%
  filter(!is.na(CLUSTER)) %>%
  
  select(-GROUP) %>%
  pivot_longer(names_to = "variable",
               values_to = "value",
               cols = AGE:SQUAT_VOLUME) %>%
  
  group_by(CLUSTER, variable) %>%
  summarise(m = mean(value),
            s = sd(value)) %>%
  mutate(ms = paste(round(m, 1), 
                    " (",
                    round(s, 1), 
                    ")", sep = ""),
         variable = factor(variable, 
                           levels = c("AGE", 
                                      "T1_BODY_MASS",
                                      "DXA_LBM_1",
                                      "DXA_FM_T1",
                                      "PERCENT_TYPE_II_T1",
                                      "Squat_3RM_kg", 
                                      "SQUAT_VOLUME"),
                           labels = c("Age (years)", 
                                      "Body mass (kg)",
                                      "LBM (kg)",
                                      "FM (kg)",
                                      "Type II fiber (%)",
                                      "3RM back squat (kg)",
                                      "Total back squat training volume (kg) from weeks 1 to 6"))) %>%
  select(-m, -s) %>%
  print()

# now we want to make a wider data set, with one column with HIGH, 
# and one column with LOW from the CLUSTER group

hyp1 %>%
  filter(!is.na(CLUSTER)) %>%
  
  select(-GROUP) %>%
  pivot_longer(names_to = "variable",
               values_to = "value",
               cols = AGE:SQUAT_VOLUME) %>%
  
  group_by(CLUSTER, variable) %>%
  summarise(m = mean(value),
            s = sd(value)) %>%
  mutate(ms = paste(round(m, 1), 
                    " (",
                    round(s, 1), 
                    ")", sep = ""),
         variable = factor(variable, 
                           levels = c("AGE", 
                                      "T1_BODY_MASS",
                                      "DXA_LBM_1",
                                      "DXA_FM_T1",
                                      "PERCENT_TYPE_II_T1",
                                      "Squat_3RM_kg", 
                                      "SQUAT_VOLUME"),
                           labels = c("Age (years)", 
                                      "Body mass (kg)",
                                      "LBM (kg)",
                                      "FM (kg)",
                                      "Type II fiber (%)",
                                      "3RM back squat (kg)",
                                      "Total back squat training volume (kg) from weeks 1 to 6"))) %>%
  select(-m, -s) %>%
  
  pivot_wider(names_from = CLUSTER, 
              values_from = ms) %>%
  
  print()

# arrange the variables from the factor variable

hyp1 %>%
  filter(!is.na(CLUSTER)) %>%
  
  select(-GROUP) %>%
  pivot_longer(names_to = "variable",
               values_to = "value",
               cols = AGE:SQUAT_VOLUME) %>%
  
  group_by(CLUSTER, variable) %>%
  summarise(m = mean(value),
            s = sd(value)) %>%
  mutate(ms = paste(round(m, 1), 
                    " (",
                    round(s, 1), 
                    ")", sep = ""),
         variable = factor(variable, 
                           levels = c("AGE", 
                                      "T1_BODY_MASS",
                                      "DXA_LBM_1",
                                      "DXA_FM_T1",
                                      "PERCENT_TYPE_II_T1",
                                      "Squat_3RM_kg", 
                                      "SQUAT_VOLUME"),
                           labels = c("Age (years)", 
                                      "Body mass (kg)",
                                      "LBM (kg)",
                                      "FM (kg)",
                                      "Type II fiber (%)",
                                      "3RM back squat (kg)",
                                      "Total back squat training volume (kg) from weeks 1 to 6"))) %>%
  select(-m, -s) %>%
  
  pivot_wider(names_from = CLUSTER, 
              values_from = ms) %>%
  arrange(variable) %>%
  
  print()

# change the names of HIGH and LOW 

hyp1 %>%
  filter(!is.na(CLUSTER)) %>%
  
  select(-GROUP) %>%
  pivot_longer(names_to = "variable",
               values_to = "value",
               cols = AGE:SQUAT_VOLUME) %>%
  
  group_by(CLUSTER, variable) %>%
  summarise(m = mean(value),
            s = sd(value)) %>%
  mutate(ms = paste(round(m, 1), 
                    " (",
                    round(s, 1), 
                    ")", sep = ""),
         
         CLUSTER = factor(CLUSTER, levels = c("LOW", "HIGH"),
                          labels = c("LOW (n=10)",
                                     "HIGH (n=10)")),
         
         variable = factor(variable, 
                           levels = c("AGE", 
                                      "T1_BODY_MASS",
                                      "DXA_LBM_1",
                                      "DXA_FM_T1",
                                      "PERCENT_TYPE_II_T1",
                                      "Squat_3RM_kg", 
                                      "SQUAT_VOLUME"),
                           labels = c("Age (years)", 
                                      "Body mass (kg)",
                                      "LBM (kg)",
                                      "FM (kg)",
                                      "Type II fiber (%)",
                                      "3RM back squat (kg)",
                                      "Total back squat training volume (kg) from weeks 1 to 6"))) %>%
  select(-m, -s) %>%
  
  pivot_wider(names_from = CLUSTER, 
              values_from = ms) %>%
  arrange(variable) %>%
  
  
  print()


# change the order of HIGH and LOW

hyp1 %>%
  filter(!is.na(CLUSTER)) %>%
  
  select(-GROUP) %>%
  pivot_longer(names_to = "variable",
               values_to = "value",
               cols = AGE:SQUAT_VOLUME) %>%
  
  group_by(CLUSTER, variable) %>%
  summarise(m = mean(value),
            s = sd(value)) %>%
  mutate(ms = paste(round(m, 1), 
                    " (",
                    round(s, 1), 
                    ")", sep = ""),
         
         CLUSTER = factor(CLUSTER, levels = c("LOW", "HIGH"),
                          labels = c("LOW (n=10)",
                                     "HIGH (n=10)")),
         
         variable = factor(variable, 
                           levels = c("AGE", 
                                      "T1_BODY_MASS",
                                      "DXA_LBM_1",
                                      "DXA_FM_T1",
                                      "PERCENT_TYPE_II_T1",
                                      "Squat_3RM_kg", 
                                      "SQUAT_VOLUME"),
                           labels = c("Age (years)", 
                                      "Body mass (kg)",
                                      "LBM (kg)",
                                      "FM (kg)",
                                      "Type II fiber (%)",
                                      "3RM back squat (kg)",
                                      "Total back squat training volume (kg) from weeks 1 to 6"))) %>%
  select(-m, -s) %>%
  
  pivot_wider(names_from = CLUSTER, 
              values_from = ms) %>%
  arrange(variable) %>%
  select(variable, `LOW (n=10)`, `HIGH (n=10)`) %>%
  
  
  print()

