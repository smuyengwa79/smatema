library(gtsummary)
library(tidyverse)
library(readxl)
library(janitor)
library(openxlsx)
library(gt)
library(arsenal)

# Load the dataset, reading sheet 1 of the file to create basic ta --------


sam_data_001 <- read_excel("sam_data_001.xlsx") %>%
  clean_names()
labels(sam_data_001) = c(
  age = "Age of respondent, yrs",
  agehousehold = "HH Age, yrs",
  size = "Size of household",
  female = "No. of females",
  male = "No. of males",
  members_under5 = "No. of childern under 5 yrs",
  members5to17 = "No. of hh members 5-17 yrs",
  members18to65 = "No. of hh members 18-65 yrs",
  members66 = "No. of hh members 66+ yrs",
  genderhousehold = "Gender of HH",
  marital_status = " Marital status of HH",
  sec4b1 = "Religion"
)

pt <- sam_data_001 %>%
  select(genderhousehold, index)

# Demographic summary -----------------------------------------------------

dem <- sam_data_001 %>%
  select(7:18, sec4b1)

dem <- dem %>%
  replace_na(
    list(
      agehousehold = 0,
      size = 0,
      female = 0,
      male = 0,
      members_under5 = 0,
      members5to17 = 0,
      members18to65 = 0,
      members66 = 0
    )
  )


# Household Roster --------------------------------------------------------


sam_data_002 <- read_excel("sam_data_001.xlsx", sheet = 2) %>%
  clean_names()

names(sam_data_002) <- gsub("section1_", "", names(sam_data_002))
roster <- sam_data_002 %>%
  select(2:6, 8) %>%
  replace_na(list (sec1q3 = 0))

# SECTION 2: HOUSEHOLD FOOD SECURITY --------------------------------------

fs <- sam_data_001 %>%
  select(genderhousehold, contains("sec2"))
fs <- fs %>%
  select(-3, -11)
names(fs) <- gsub("sec2q2_", "", names(fs))
labels(fs) <-
  c(sec2q1 = "In the past 5 years, where there years you did not have enough food to meet your family needs ",
    sec2q3 = "How many meals excluding snacks do you normally have in a day",
    sec2q4 = "Compared to 5 years ago, your households is: ")



# SECTION 3A: MAJOR SHOCKS AND RISKS --------------------------------------


sam_data_003 <- read_excel("sam_data_001.xlsx", sheet = 3) %>%
  clean_names()
#Join table 3 with the primary table (to link table with gender of household)

shocks <-
  left_join(pt, sam_data_003, by = c("index" = "parent_index")) %>%
  select(genderhousehold, sec3a_sec3a1)

labels(shocks) <-
  c(genderhousehold = "Gender of the head of household",
    sec3a_sec3a1 = "Which shocks did you experience")

# SECTION 3B: SHOCKS AND SOCIAL NETWORKS (CTD) ----------------------------
sam_data_004 <- read_excel("sam_data_001.xlsx", sheet = 4)
names(sam_data_004) <- gsub("sec3b/", "", names(sam_data_004))
adaptation <- sam_data_004 %>% 
  select(sec3b1, 5:18)

land <- read_excel("sam_data_001.xlsx", sheet = 8) %>% 
  clean_names()
land <- land %>% 
  select(c(-3, -11:-17, -20:-39))

labels(land) = c(
  plot_questions_plot_id = "Plot Number",
  plot_questions_sec6a2	= "Type of Land",
  plot_questions_sec6a4	= "Irrigated",
  plot_questions_sec6a5	= "Tenure",
  plot_questions_sec6a6	= "Land Acquired Through",
  plot_questions_sec6a7 = "Principal Use",
  plot_questions_sec6a8 = "Crops Rotated",
  plot_questions_sec6a9a = "Degradation",	
  plot_questions_sec6a9b = "Form of Degradation",
  plot_questions_sec6a10 = "Extent of Degradation",
  plot_questions_sec6a11 = "Manager"
)

land <- land %>% 
  rename(`Plot_Number` = plot_questions_plot_id,
         `Type_of_Land` = plot_questions_sec6a2,
         `Irrigated` = plot_questions_sec6a4,
         `Tenure` = plot_questions_sec6a5,
         `Land_Acquired_Through` = plot_questions_sec6a6,
         `Principal_Use` = plot_questions_sec6a7,
         `Crops_Rotated` = plot_questions_sec6a8,
         `Degradation` = plot_questions_sec6a9a,
         `Form_of_Degradation` = plot_questions_sec6a9b,
         `Extent_of_Degradation` = plot_questions_sec6a10,
         `Manage` = plot_questions_sec6a11)

programs <- read_excel("sam_data_001.xlsx", sheet = 7) %>% 
  clean_names()
