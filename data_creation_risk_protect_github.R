rm(list=ls())
options(scipen=999)

library(survey)
library(dplyr)
library(data.table)
library(haven)
library(lubridate)
library(tidyr)
library(gtsummary)
library(sandwich)
library(tibble)
library(caret)


# Binary Data 
df_binary <- read.csv("YOUR PATH/Data/2023_National_SADCQN_transformed.csv") %>%  filter(year == "2023")

binary_base <- df_binary %>% 
  filter(sex == 1) %>% # Females only
  select(
    -condensed_sexuality, -condensed_sexual_partners, -race7,
     -sitecode, -sitename, -sitetype,
    -year, -age,-plansuicide_qn28,-suicideattinjury_qn30,-sex,
    -sad_qn26, 
  ) %>%
  mutate(
    ideation = case_when(
      
      thinksuicide_qn27 == 1 & suicideatt_qn29 == 0 ~ 1,
      
      thinksuicide_qn27 == 1 & suicideatt_qn29 == 1 ~ 0,
      
      thinksuicide_qn27 == 0 ~ 0
    )
  ) %>%
  select(
    -thinksuicide_qn27
  ) 


var_to_keep <- binary_base %>% 
  select(
    -weight
  ) %>%
  gather(., "var","val") %>%
  group_by(var) %>%
  summarise(
    prop_missing = mean(is.na(val))
  ) %>%
  arrange(desc(prop_missing)) %>%
  filter(prop_missing <= 0.50) %>%
  pull(var) 

binary_modeling_data <- binary_base %>%
  mutate(
    sexual_partners = ifelse(sexual_partners == "", NA, sexual_partners), 
    sexuality = ifelse(sexuality == "", NA, sexuality), 
    race4 = ifelse(race4 == "", NA, race4)
  ) %>%
  select(
    var_to_keep , weight, stratum, PSU, -mentalhealth_qn84
  ) %>%
  mutate(
    grade = as.factor(grade)
  )


write.csv(binary_modeling_data, 'YOUR PATH/Data/yrbs2023_female_only_binary.csv')


























































