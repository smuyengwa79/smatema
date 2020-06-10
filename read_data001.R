library(gtsummary)
library(tidyverse)
library(readxl)
library(janitor)
library(openxlsx)
library(gt)
library(arsenal)

# LOAD DATA --------


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

# DEMOGRAPHIC SUMMARY -----------------------------------------------------

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


# HOUSEHOLD ROSTER --------------------------------------------------------


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


# SECTION 4A: SHOCKS AND LOCAL KNOWLEDGE SYSTEMS --------------------------

sam_data_005 <- read_excel("sam_data_001.xlsx", sheet = 5) %>%
  clean_names()
names(sam_data_005) <- gsub("sec4a_", "", names(sam_data_005))
iks <- sam_data_005 %>%
  select(1:2)
labels(iks) <- c(sec4a1 = "Changes in weather pattern",
                 sec4a2 = "Are the ways that local people predict or know about weather other than through radio")

## Part B

iks2 <- sam_data_001 %>% 
  select(genderhousehold, sec4a4, sec4a6)
labels(iks2) <- c(genderhousehold = "Gender of household head", 
                  sec4a4 = "In your view, are the local weather prediction systems useful?", 
                  sec4a6 = "Have you used this informatoon to plan your agricutural activities?")

# SECTION 4B:SACRED SITES -------------------------------------------------
sacred_sites <- sam_data_001 %>% 
  select(sec4b1, sec4b2)


# SECTION 4C: SACRED ANIMALS, BIRDS AND TREES -----------------------------

sam_data_006 <- read_excel("sam_data_001.xlsx", sheet = 6) %>% 
  clean_names()

# SECTION 5: PROGRAMS -----------------------------------------------------

sam_data_007 <- read_excel("sam_data_001.xlsx", sheet = 7) %>% 
  clean_names()

abt <-sam_data_007 %>% 
  select(1:2)
labels(abt) <- c(section4c_sec4c1a = "Is there a sacred bird, tree, animal?", 
                 section4c_sec4c1b = "Indicate the name")

# SECTION 6: LAND  --------------------------------------------------------

sam_data_008 <- read_excel("sam_data_001.xlsx", sheet = 8) %>% 
  clean_names()
land <- sam_data_008 %>% 
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

programs <- left_join(pt, programs, by = c("index" = "parent_index"))%>% 
  select(genderhousehold, section5_sec5q1, section5_sec5q2, section5_sec5q3,
         section5_sec5q4, section5_sec5q5, section5_sec5q6, section5_sec5q7,
         section5_sec5q8, section5_sec5q9)

programs <- programs %>% 
  rename(`Programmes` = section5_sec5q1,
         `Source` = section5_sec5q2,
         `Did_HH_Travel` = section5_sec5q3,
         `HH_Member_Travelled` = section5_sec5q4,
         `Where_travelled` = section5_sec5q5,
         `Form_of_Transport` = section5_sec5q6,
         `Distance_in_km` = section5_sec5q7,
         `Duration_in_minutes` = section5_sec5q8,
         `Cost_of_Transport` = section5_sec5q9)

labels(programs) = c(
  Programmes = "Services and programmes",
  Source = "Who provided support?",
  Did_HH_Travel = "Did hh member travelled to get suppot?",
  HH_Member_Travelled = "Who travelled?",
  Where_travelled = "Where did he/she travelled?",
  Form_of_Transport = "Form of transport used",
  Distance_in_km = "Distance in kilometres",
  Duration_in_minutes = "Time taken in minutes",
  Cost_of_Transport = "Cost of transport"
  
)

