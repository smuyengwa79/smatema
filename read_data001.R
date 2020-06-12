library(gtsummary)
library(tidyverse)
library(readxl)
library(janitor)
library(openxlsx)
library(gt)
library(arsenal)
library(questionr)
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
abt <-sam_data_006 %>% 
  select(1:2)
labels(abt) <- c(section4c_sec4c1a = "Is there a sacred bird, tree, animal?", 
                 section4c_sec4c1b = "Indicate the name")

# Section 5: Access to services and programmes ----------------------------

sam_data_007 <- read_excel("sam_data_001.xlsx", sheet = 7) %>% 
  clean_names()

sam_data_007 <- left_join(pt, sam_data_007, by = c("index" = "parent_index"))%>% 
  select(genderhousehold, section5_sec5q1, section5_sec5q2, section5_sec5q3,
         section5_sec5q4, section5_sec5q5, section5_sec5q6, section5_sec5q7,
         section5_sec5q8, section5_sec5q9)

sam_data_007$section5_sec5q2 <- fct_recode(sam_data_007$section5_sec5q2,
                                               "Social Welfare" = "Social walfare",
                                               "Government" = "Gvt",
                                               "BEAM" = "Beam",
                                               "Social Welfare" = "Social welfare",
                                               "Agritex" = "Agritex Officers",
                                               "Vet Services" = "Veterinary Services",
                                               "Vet Services" = "Vetinary services",
                                               "Agritex" = "Agritex Officer",
                                               "Vet Services" = "Veterinary Officers",
                                               "Vet Services" = "Veterinary",
                                               "Donors" = "USAID",
                                               "Agritex" = "Agritex officers",
                                               "Donors" = "NAC",
                                               "Agritex" = "Arex",
                                               "Donors" = "NGO",
                                               "Agritex" = "Agritex offices",
                                               "Vet Services" = "Verterinary services",
                                               "Donors" = "Usaid",
                                               "Donors" = "WfP",
                                               "Donors" = "Donor",
                                               "Government" = "GVT",
                                               "Donors" = "Dor",
                                               "Vet Services" = "Vertinary"
)

sam_data_007 <- sam_data_007 %>% 
  rename(`Programmes` = section5_sec5q1,
         `Source` = section5_sec5q2,
         `Did_HH_Travel` = section5_sec5q3,
         `HH_Member_Travelled` = section5_sec5q4,
         `Where_travelled` = section5_sec5q5,
         `Form_of_Transport` = section5_sec5q6,
         `Distance_in_km` = section5_sec5q7,
         `Duration_in_minutes` = section5_sec5q8,
         `Cost_of_Transport` = section5_sec5q9)

labels(sam_data_007) = c(
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




# SECTION 6A: LAND  --------------------------------------------------------

sam_data_008 <- read_excel("sam_data_001.xlsx", sheet = 8) %>% 
  clean_names()
sam_data_008 <- sam_data_008 %>% 
  select(c(-3, -11:-17, -20:-39))

labels(sam_data_008) = c(
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

sam_data_008 <- sam_data_008 %>% 
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
         `Who_Manage_the_field` = plot_questions_sec6a11)



# Section 6B: Crop Production - Last Rainy Season (2018/2019) -------------

sam_data_009 <- read_excel("sam_data_001.xlsx", sheet = 9) %>% 
  clean_names() %>% select(sec6b_sec6b2,	sec6b_sec6b3,
                           sec6b_sec6b4,	sec6b_sec6b5,
                           sec6b_sec6b6,	sec6b_sec6b7,
                           sec6b_sec6b8,	sec6b_sec6b8a,
                           sec6b_sec6b8b,	sec6b_sec6b9,
                           sec6b_sec6b9a,	sec6b_sec6b9b
                           )

sam_data_009 <- sam_data_009 %>% 
  rename(`Plot_Id` = sec6b_sec6b2,
         `Crop` = sec6b_sec6b3,
         `Type_of_Crop_Stand` = sec6b_sec6b4,
         `Entire_Plot` = sec6b_sec6b5,
         `Area_Under_Crop` = sec6b_sec6b6,
         `Seed_Variety` = sec6b_sec6b7
         )
## Recoding sam_data_009$sec6b_sec6b8 into sam_data_009$Total_Harvest_in_kgs
sam_data_009$Total_Harvest_in_kgs <- fct_recode(sam_data_009$sec6b_sec6b8,
   "90" = "90Kg Bag",
   "25" = "25 Kg Bag",
   "1" = "Kilogram",
   "50" = "50 Kg Bag",
   "75" = "75Kg Bag"
 )



# SECTION 6C: CROP PRODUCTION-MAJOR CROP & PRODUCTION ---------------------
sam_data_010 <- read_excel("sam_data_001.xlsx", sheet = 10) %>% 
  clean_names()
names(sam_data_010) <- gsub("sec6c_", "", names(sam_data_010))
crops <- sam_data_010 %>%  select(1:3, 29:30)
labels(crops) <- c(sec6c1 = "Name of crop grown", 
                   sec6c2 = "Trend in proportion under this croop", 
                   sec6c3 =  "Explain the change in land size", 
                   sec6c4 = "Production levels in 2009", 
                   sec6c5 = "Explain the change in production")


# SECTION D: CROP PRODUCTION -CHANGING PREFERENCES ------------------------

sam_data_011 <- read_excel("sam_data_001.xlsx", sheet = 11) %>% 
  clean_names()
crops_1980  <- sam_data_011 %>% 
  select(1:2)
labels(crops_1980) <- c(sec6d_sec6d1 = "Crop dropped since 1980", 
                        sec6d_sec6d2 = "Why was the crop dropped" )


# SECTION 6D: CROP PRODUCTION AND PREFERENCES ADOPTED ---------------------
sam_data_012 <- read_excel("sam_data_001.xlsx", sheet = 12) %>% 
  clean_names()
crops_1980a  <- sam_data_012 %>% 
  select(1:2)
labels(crops_1980a) <- c(sec6dd_sec6d3 = "Crop dropped since 1980", 
                        sec6dd_sec6d4 = "Why was the crop dropped" )




# SECTION 7A: WATERSHED CONDITION AND MANAGEMENT --------------------------

sam_data_013 <- read_excel("sam_data_001.xlsx", sheet = 13) %>% 
  clean_names() %>% select(sec7a_sec7a1,
                           sec7a_sec7a2,
                           sec7a_sec7a3,
                           sec7a_sec7a4
)

sam_data_013 <- sam_data_013 %>% 
  rename(`Type_of_change` = sec7a_sec7a1,
         `Observed_in_comm` = sec7a_sec7a2,
         `Observed_in_own_field` = sec7a_sec7a3,
         `Prevention_measure` = sec7a_sec7a4)

labels(sam_data_013) = c(
  Type_of_change = "Type of change",
  Observed_in_comm = "Change observed in community",
  Observed_in_own_field = "Change observed in own field",
  Prevention_measures = "Prevention measure"
)


# SECTION 7B: WATERSHED CONDITION AND MANAGEMENT - INSTITUTIONS -----------

sam_data_014 <- read_excel("sam_data_001.xlsx", sheet = 14) %>% 
  clean_names() %>% select(sec7b_sec7b2,
                           sec7b_sec7b3,
                           sec7b_sec7b4,
                           sec7b_sec7b5,
                           sec7b_sec7b6

)

sam_data_014 <- sam_data_014 %>% 
  rename(`Rule` = sec7b_sec7b2,
         `Awareness` = sec7b_sec7b3,
         `Enforcer` = sec7b_sec7b4,
         `Compliance` = sec7b_sec7b5,
         `Non_compliance_rsn` = sec7b_sec7b6)

labels(sam_data_014) = c(
  Rule = "Rule",
  Awareness = "Are you aware of the rule?",
  Enforcer = "Who enforces the rule?",
  Compliance = "Do you comply?",
  Non_compliance_rsn = "Reason for not complying"
)


# SECTION 9: HOUSEHOLD LIVELIHOOD STRATEGIES ------------------------------


sam_data_016 <- read_excel("sam_data_001.xlsx", sheet = 16) %>% 
  clean_names()

sam_data_016 <-
  left_join(pt, sam_data_016, by = c("index" = "parent_index")) %>%
  select(genderhousehold, sec9_sec9q1,	sec9_sec9q2,
         sec9_sec9q3,	sec9_sec9q4)
 sam_data_016 <- sam_data_016 %>% 
   rename(`Livelihood_source` = sec9_sec9q1,
          `Is_it_a_source` = sec9_sec9q2,
          `No_of_hh_members` = sec9_sec9q3,
          `Viability` = sec9_sec9q4)
labels(sam_data_016) = c(
  Livelihood_source = "Source of livelihood",
  Is_it_a_source = "Is this a source for the hh",
  No_of_hh_members = "No. of hh members involved",
  Viability = "Viability in the past 5 years"
)


# SECTION 10: FOREST RESOURCES DEPENDENCE ---------------------------------

sam_data_017 <- read_excel("sam_data_001.xlsx", sheet = 17) %>% 
  clean_names() %>% select(section10_forestproduct,	section10_sec10a1,
                           section10_sec10a2,	section10_sec10a3,
                           section10_sec10a4,	section10_sec10a5,
                           section10_sec10a6)

sam_data_017 <- sam_data_017 %>% 
  rename(`Product` = section10_forestproduct,
         `Does_hh_consume` = section10_sec10a1,
         `Time_of_year` = section10_sec10a2,
         `Does_hh_sell` = section10_sec10a3,
         `Time_of_year_s` = section10_sec10a4,
         `Who_sells` = section10_sec10a5,
         `How_much` = section10_sec10a6)

labels(sam_data_017) = c(
  Product = "Forest Product",
  Does_hh_consume = "Does your hh consume the product",
  Time_of_year = "Time of main consumption",
  Does_hh_sell = "Does your hh sell the product",
  Time_of_year_s = "Time of main sales",
  Who_sells = "Who sells the product"
)





