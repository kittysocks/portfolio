library(tidyverse)
library(flextable)

format_output <- function(output_tab, caption, autofit = TRUE) {
  # Build flextable object for pretty presentation
  
  # Isolate header rows, to align and bold them
  rows <- grep('Information', output_tab[,1], ignore.case = TRUE)
  
  # Build and format table
  tab <- flextable(output_tab)
  tab <- theme_box(tab)
  tab <- bold(tab, i = rows, bold = TRUE)
  if (autofit) tab <- autofit(tab)
  tab <- align(tab, i = rows, align = 'left')
  tab <- align(tab, j = 2:ncol(output_tab), align = 'center', part = 'all')
  tab <- set_caption(tab, caption = caption)
  return(tab)
}

FitFlextableToPage <- function(ft, pgwidth = 6) {
  
  ft_out <- ft %>% autofit()
  
  ft_out <- width(ft_out, width = dim(ft_out)$widths * pgwidth / (flextable_dim(ft_out)$widths))
  return(ft_out)
}


get_demo_tbl <- function() {
# Read in data from CYP-GUIDES Kaggle Dataset
df <- read.csv("Dataset.csv")

# Summary Statistics 
race <- df %>%
  rename(race = RACE.ETHNICITY) %>%
  mutate(Category = case_when(
    (race == "W") ~ ("White"),
    (race == "B") ~ ("Black"),
    (race == "L") ~ ("Hispanic/Latino"),
    TRUE ~ ("Other/Unknown")
  )) %>%
  group_by(Category) %>%
  summarize(Count = n()) %>%
  mutate(Variable = "Race")

sex <- df %>%
  rename(sex = GENDER) %>%
  mutate(Category = case_when(
    (sex == "F") ~ ("Female"),
    (sex == "M") ~ ("Male")
  )) %>%
  group_by(Category) %>%
  summarize(Count = n()) %>%
  mutate(Variable = "Sex")

age <- df %>%
  rename(age = AGE) %>%
  mutate(age = as.integer(age)) %>%
  mutate(Category = case_when(
    (age >= 18 & age <= 24) ~ ("Young Adults (18-24)"),
    (age >= 25 & age <= 39) ~ ("Early Mid-Adulthood (25-39)"),
    (age >= 40 & age <= 64) ~ ("Late Adulthood (40-64)"),
    (age >= 65) ~ ("Elderly Adults (65+)")
  )) %>%
  group_by(Category) %>%
  summarize(Count = n()) %>%
  mutate(Variable = "Age") 

demo.tbl <- bind_rows(race, sex, age ) %>%
  select(Variable, Category, Count) %>%
  arrange(factor(Category, levels = c("Female","Male","Young Adults (18-24)","Early Mid-Adulthood (25-39)","Late Adulthood (40-64)", "Elderly Adults (65+)","White","Black","Hispanic/Latino","Other/Unknown") ))

return(demo.tbl)
}