
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  #@        Here the conatct matrix will be one year age group  @
  #@                                                            @
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                         I. Data sources
#  1.measles cases by country and month from the WHO website:
#  https://www.who.int/teams/immunizationvaccines-and-biologicals/immunization-analysis-and-insights/surveillance/monitoring/provisional-monthly-measles-and-rubella-data
#  2.Catch-up vaccination
#  https://immunizationdata.who.int/global/wiise-detail-page/catch-up-vaccination?ISO_3_CODE=&YEAR=
#  3.Country-specific information on assessments and strategies to drive demand for immunization
#  https://immunizationdata.who.int/global/wiise-detail-page/demand-for-immunization?YEAR=
#  
#  .4.EpiSignalDetection
#  https://www.ecdc.europa.eu/en/publications-data/episignaldetection-tool
#  5.EMRO health indicators :https://iris.who.int/bitstream/handle/10665/348133/9789292744403-eng.pdf?sequence=4&isAllowed=y
#  6.EMRO Data and statistics :https://www.emro.who.int/data-and-statistics.html
#  7.Long-term Dynamics of Measles Virus–Specific Neutralizing Antibodies in Children Vaccinated Before 12 Months of Age(https://academic.oup.com/cid/article/80/4/904/7874423?login=false)
#  Age group(5 years): World Population Prospects 2024
#------------------>#https://population.un.org/wpp/downloads?folder=Standard%20Projections&group=Population
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#Clean workspace
#rm(list=ls())
#Set work directoryhttp://127.0.0.1:36857/graphics/647e0d49-61eb-4cfb-810f-7e69672770a2.png
getwd()
setwd("C:/Courses/Placement project")
getwd()
setwd("D:/Placement project disk")
#Packages
library(readxl)
age<-read_excel("3.U.1.WPP2024_POP_F02_1_POPULATION_5-YEAR_AGE_GROUPS_BOTH_SEXES .xlsx",sheet = "Estimates")
print(age)
# List of all EMRO country codes
emro_countries_code <- c("AFG", "BHR", "DJI", "EGY", "IRN", "IRQ", "JOR", "KWT", 
                         "LBN", "LBY", "MAR", "OMN", "PAK", "PSE", "QAT", "SAU", 
                         "SDN", "SOM", "SYR", "TUN", "ARE", "YEM")
pacman::p_load(dplyr)
#age_str <- age%>% slice(-(1:16))# 
age_str <- age[-(2:17), ]
names(age_str)
age_str
age_str1<-age_str|>
  filter(Year>=2015)|>
  filter(`ISO3 Alpha-code` %in% emro_countries_code)
print(age_str1)
names(age_str1)
#
str(age_str1)
#I covert it into numeric
age_str2 <- age_str1 %>%
  mutate(across(`0-4`:`100+`, ~ as.numeric(.)))
names(age_str2)

#Total for each column
age_str3 <- age_str2 %>%
  bind_rows(
    summarise(.,
              across(`0-4`:`100+`, sum, na.rm = TRUE),
              `Region, subregion, country or area *` = "TOTAL")
  )
#Checking
Tot<-age_str3|>
  filter(`Region, subregion, country or area *`=="TOTAL")
Tot1<-Tot[,-(1:2)]
Tot1
Tot2<-Tot1[,-(4:9)]
Tot2
#Longformat
pacman::p_load(tidyr,dplyr)
age_str_long <- age_str3%>%
  pivot_longer(
    cols = `0-4`:`100+`,  # Columns to pivot into long format
    names_to = "Age_Category",  # New column for age categories
    values_to = "Population_Value"  # New column for population values
  ) %>%
  select(
    Country = `Region, subregion, country or area *`,  # Rename country column
    Country_Code = `ISO3 Alpha-code`,                  # Select ISO3 code
    Year=`Year`,
    Age_Category,
    Population_Value
  )
head(age_str_long,21)
#Here i will need to merge some age groups
age_str_long1 <- age_str_long%>%
  mutate(
    Merged_Age_Group = case_when(
      Age_Category %in% c( "5-9", "10-14") ~ "5-14",
      Age_Category %in% c("15-19", "20-24", "25-29","30-34", "35-39", "40-44") ~ "15-44",
      Age_Category %in% c("45-49","40-54","55-59","60-64") ~ "45-64",
      Age_Category %in% c(
        "65-69",                               
        "70-74",                               
        "75-79",
        "80-84",
        "85-89",
        "90-94",
        "95-99",
        "100+") ~ "65+",
      TRUE ~ Age_Category
    )
  )

#After merging age group, i will need to merge population in new age groups
age_str_long2 <- age_str_long1%>%
  group_by(Country,Country_Code, Year,Age_Category,Merged_Age_Group) %>%
  summarise(Total_Population = sum(Population_Value), .groups = "drop")
# View 
head(age_str_long2,21)
#Population in thousands
age_str_long3 <- age_str_long2|>
  mutate(Pop_Value = as.numeric(Total_Population)) |>
  mutate(Pop_in_thousands = round(Pop_Value * 1e3, 2)) #|>View()
head(age_str_long3)
tail(age_str_long3,21)
str(age_str_long3)

head(age_str3)
names(age_str3)

#i will need to add proportion in each age group by country
age_str_long4 <- age_str_long3 %>%
  group_by(Country, Year) %>%
  mutate(
    Country_Population = sum(Pop_in_thousands,na.rm = TRUE),
    Proportion = Pop_in_thousands / Country_Population
  ) %>% 
  ungroup()   #|>View()

age_str_long4 # I will need to change the Year in Period to match with dataset for cases+vaccine
colnames(age_str_long4)<-
  c("Country","CODE","Period",  "Age_Category", "Merged_Age_Group", "Total_Population", "Pop_Value", "Pop_in_thousands", "Country_Population", "Proportion")
Bahrain<-age_str_long4|>
  filter(Country=="Bahrain")|>
  filter(Period==2023)
Bahrain
dim(Bahrain) # 21 ages floors
B<-21
t<-table(Bahrain$Age_Category, Bahrain$Merged_Age_Group)
#plot(t)
t<-table(age_str_long4$Age_Category)

#Reorder age groups
age_levels <- c("0-4", "5-9", "10-14", "15-19", "20-24", 
                "25-29", "30-34", "35-39", "40-44", "45-49",
                "50-54", "55-59", "60-64", "65-69", "70-74",
                "75-79", "80-84", "85-89", "90-94", "95-99", "100+")
age_str_long5 <- age_str_long4 %>%
  mutate(Age = factor(Age_Category, 
                      levels = age_levels,
                      ordered = TRUE))
#Remove total
pacman::p_load(ggplot2)
age_str_long6<-age_str_long5|>
  filter(Country != "TOTAL")
age_str_long6
ggplot(age_str_long6,aes(x=Age,y=Proportion,by=Country))+
  geom_bar(stat="identity",position="dodge",aes(fill=Country))

age_str_long7<-age_str_long6|>
  filter(Country=="TOTAL")#|>View()
age_str_long6#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                   Mortality (in %) by age group               #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(readxl)
deaths_percent<- read_excel("3.U.1.WPP2024_MORT_F01_1_DEATHS_SINGLE_AGE_BOTH_SEXES.xlsx",sheet="Estimates") 
deaths_percent1<-deaths_percent[-(2:17),]
deaths_percent1
deaths_percent2<-deaths_percent1|>
  filter(Year>=2015)|>
  filter(`ISO3 Alpha-code` %in% emro_countries_code)
print(deaths_percent2)
names(deaths_percent2)
#
str(deaths_percent2)
#I covert it into numeric
deaths_percent3 <-deaths_percent2%>%
  mutate(across(`0`:`100+`, ~ as.numeric(.)))
deaths_percent3

deaths_percent4<-deaths_percent3[,-(1:2)]
deaths_percent4
deaths_percent5<-deaths_percent4[,-2]
deaths_percent5
deaths_percent6<-deaths_percent5[,-(4:7)]
deaths_percent6
names(deaths_percent6)
#Change the 100+ into 100
names(deaths_percent6)[names(deaths_percent6) == "100+"] <- "100"
names(deaths_percent6)
#Longformat
pacman::p_load(tidyr,dplyr)
deaths_percent_long <-deaths_percent6%>%
  pivot_longer(
    cols = `0`:`100`,  # Columns to pivot into long format
    names_to = "Age",  # New column for age categories
    values_to = "Percentage"  # New column for population values
  ) %>%
  select(
    Country = `Region, subregion, country or area *`,  # Rename country column
    Country_Code = `ISO3 Alpha-code`,                  # Select ISO3 code
    Year=`Year`,
    Age,
    Percentage
  )
str(deaths_percent_long)
deaths_percent_long1<-deaths_percent_long|>
  mutate(Year=as.numeric(Year))
deaths_percent_long1
names(deaths_percent_long1)

emro_countries_code <- c("AFG", "BHR", "DJI", "EGY", "IRN", "IRQ", "JOR", "KWT", 
                         "LBN", "LBY", "MAR", "OMN", "PAK", "PSE", "QAT", "SAU", 
                         "SDN", "SOM", "SYR", "TUN", "ARE", "YEM")

names(deaths_percent_long1)
(deaths_percent_long1.a<-deaths_percent_long1|>
    filter(Year>=2015)|>
    filter(`Country_Code` %in% emro_countries_code))
print(deaths_percent_long1.a)
#library(readr)
write.csv(deaths_percent_long1.a,"3.U.1.EMRO_mortality_by_age_group_1yearage.csv",row.names = FALSE)

# Here i will need to merge some ages groups
deaths_percent_long2<- deaths_percent_long1 %>%
  #filter(grepl("^[0-9]+$", Age_Category)) %>%  # keep rows where Age_Category is all digits
  mutate(
    Age = as.numeric(Age),
    Age_group = case_when(
      Age == 0 ~ "0-1",
      Age >= 1 & Age <= 4 ~ "1-4",
      Age >= 5 & Age <= 9 ~ "5-9",
      Age >= 10 & Age <= 14 ~ "10-14",
      Age >= 15 & Age <= 19 ~ "15-19",
      Age >= 20 & Age <= 24 ~ "20-24",
      Age >= 25 & Age <= 29 ~ "25-29",
      Age >= 30 & Age <= 34 ~ "30-34",
      Age >= 35 & Age <= 39 ~ "35-39",
      Age >= 40 & Age <= 44 ~ "40-44",
      Age >= 45 & Age <= 49 ~ "45-49",
      Age >= 50 & Age <= 54 ~ "50-54",
      Age >= 55 & Age <= 59 ~ "55-59",
      Age >= 60 & Age <= 64 ~ "60-64",
      Age >= 65 & Age <= 69 ~ "65-69",
      Age >= 70 & Age <= 74 ~ "70-74",
      Age >= 75 & Age <= 79 ~ "75-79",
      Age >= 80 & Age <= 84 ~ "80-84",
      Age >= 85 & Age <= 89 ~ "85-89",
      Age >= 90 & Age <= 94 ~ "90-94",
      Age >= 95 & Age <= 99 ~ "95-99",
      Age > 99~ "100+"
    )
  ) %>%
  group_by(Country,Country_Code,Year, Age_group) %>%
  summarise(Percentage_Agegroup = sum(Percentage, na.rm = TRUE), .groups = 'drop') %>% 
  rename(Age_category = Age_group)
deaths_percent_long2
#Ordered
age_levels <- c(
  "0-1",
  "1-4",
  paste0(seq(5, 95, by = 5), "-", seq(9, 99, by = 5)),
  "100+"
)
# 2. Re-factor and re-order
deaths_percent_long3 <- deaths_percent_long2 %>%
  mutate(
    Age_category = factor(Age_category, levels = age_levels, ordered = TRUE)
  ) %>%
  arrange(Country, Country_Code, Year, Age_category)

# 3 Check
levels(deaths_percent_long3$Age_category)
#Viualization
names(deaths_percent_long3)
(pyramyd<-ggplot(deaths_percent_long3|>
                   filter(Country=="Pakistan",Year==2023), aes(x = Age_category, y = Percentage_Agegroup)) +
    geom_col(fill = "red") +
    labs(title = "Mortality in Pakistan (2023)",
         x = "Age", y = "Deaths per 1000 pop") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
)
#EMRO countries
emro_countries_code <- c("AFG", "BHR", "DJI", "EGY", "IRN", "IRQ", "JOR", "KWT", 
                         "LBN", "LBY", "MAR", "OMN", "PAK", "PSE", "QAT", "SAU", 
                         "SDN", "SOM", "SYR", "TUN", "ARE", "YEM")

names(deaths_percent_long3)
(deaths_percent_long4<-deaths_percent_long3|>
    filter(Year>=2015)|>
    filter(`Country_Code` %in% emro_countries_code))
print(deaths_percent_long4)
#library(readr)
write.csv(deaths_percent_long4,"3.U.1.EMRO_mortality_by_age_group.csv",row.names = FALSE)
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                 Women population  by age group               #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(readxl)
pop_femal <- read_excel("3.U.1.WPP2024_POP_F01_3_POPULATION_SINGLE_AGE_FEMALE.xlsx",sheet = "Estimates")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Contact matrix: Construction or extraction|--------------------------------> 
#1.-------------|--->POLYMOD study 2008 (Prof.Ricardo courses):Mossong et al. (2008). Social Contacts and Mixing Patterns Relevant to the Spread of Infectious Diseases.
#2.             |--->POLYMOD:DOI: 10.1371/journal.pmed.0050074
#3.             |--->Measles-Specific Matrices:Adam J. Kucharski(2014),sierra Leone:DOI: 10.1073/pnas.1508814112 
#4.             |--->LMIC-Specific Matrices:Mark Jit ,,Prem et al. (2017). Projecting Social Contact Matrices in 152 Countries
#5                 
#~~~~~~~~~~~~~~~~~~~~~~~~~~(1.POLYMOD)~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.Polymod study 2008
pacman::p_load(socialmixr)
data(polymod)   #Contact data
print(polymod)
#contact_matrix <- contact_matrix(polymod, age.limits = c(0, 5, 10, 15, 20, 30, 40, 50, 60))
#N.B: With POLYMOD data, we can change age.limits in contact_matrix() to match our disease model or policy question.
#     1.The final group always includes everyone greater than or equal to the last cutoff.
#     2.Make sure your population data (if you use survey.pop) matches the age groups you define.
contact_matrix <- contact_matrix(polymod, age.limits = c(0, 5, 15, 45, 65))
#contact_matrix <- contact_matrix(polymod, age.limits = c(1:101))
#Measles-Specific Contacts
#Measles is highly contagious; i will use all locations (households + schools + workplaces) or weight by transmission intensity:
measles_matrix <- contact_matrix$matrix
measles_matrix
colnames(measles_matrix)<-c("0-4","5-14","15-44","45-64","65+")
rownames(measles_matrix)<-c("0-4","5-14","15-44","45-64","65+")
measles_matrix
measles_matrix<-as.matrix(measles_matrix)
#Filters for measles specificity :1. phys = TRUE
#                                 2. intensity = "close"
#                                 3. duration = ">=15 minutes"
#                                 4. location = "home", "school", "work", "leisure", "transport", etc.
mcm<-contact_matrix(
  polymod,
  age.limits = c(0, 5, 10, 15, 20, 30, 40, 50, 60),
  filter = list(phys = TRUE, duration = ">=15 minutes", location = "home",location = "school"),
  symmetric = TRUE
)

#school_contacts <- contact_matrix(polymod, age.limits = c(0, 5, 10), settings = "school")

#Visualization
pacman::p_load(ggplot2,reshape2)

df <- melt(measles_matrix)
colnames(df) <- c("Contactee", "Contactor", "Contacts")

ggplot(df, aes(x = Contactor, y = Contactee, fill = Contacts)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "purple") +
  theme_minimal() +
  labs(title = "Contact Matrix Heatmap")

#ggplot(df, aes(x = Contactor, y = Contactee, fill = Contacts)) +
#  geom_tile() +
# scale_fill_viridis_c() +
#theme_minimal() # +

#ggplot(df, aes(x =Contactor, y = Contactee, fill = Contacts)) + 
  #geom_bar(stat = "identity", position = "dodge")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Mark Jit ,,Prem et al. (2017). Projecting Social Contact Matrices in 152 Countries 
#                   |---------------| S1 Data sets
#-------------------|2.152 Countries|----------------------------------------------
#                   |---------------|
library(readxl) 
#all_locations_1:a to m 
#all_locations_2:m to Z
#Emro countries
# My assumptions
#1.The transmissions weighted by setting 
#2.For school age children,schools contribute more to the measles :
#3.Schools should be targeted in SIAs
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#The conatact 16*16 (5 years ) will be coverted in to 80*80 (1 years) Assumming :1) uniform mixing in the same age group
# Dummy 5-year matrix (16 x 16)                                                  2) average of contacts
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#set.seed(42)
#C_5y <- matrix(runif(16 * 16, 0, 10), nrow = 16, ncol = 16)
# Initialize 1-year matrix (80 x 80)
#C_1y <- matrix(0, nrow = 80, ncol = 80)

# Expand each 5-year group into 5 x 5 block
#for (i in 1:16) {
  #for (j in 1:16) {
   # row_idx <- ((i - 1) * 5 + 1):(i * 5)
   # col_idx <- ((j - 1) * 5 + 1):(j * 5)
   # C_1y[row_idx, col_idx] <- C_5y[i, j]
  #}
#}

##1.

Bahrain
dim(Bahrain)
# I will first  initialize 1-year matrix (80 x 80)
Bahrain_1y <- matrix(0, nrow = 80, ncol = 80)

# Expand each 5-year group into 5 x 5 block
for (i in 1:16) {
  for (j in 1:16) {
    row_idx <- ((i - 1) * 5 + 1):(i * 5)
    col_idx <- ((j - 1) * 5 + 1):(j * 5)
    Bahrain_1y[row_idx, col_idx] <- Bahrain[i, j]
  }
}
Bahrain_1y
dim(Bahrain_1y)
#

#1.
Bahrain<- as.matrix(read_excel("Contact matrix.152 countries/pcbi.1005697.s002/contact_matrices_152_countries/MUestimates_all_locations_1.xlsx", 
                               sheet = "Bahrain"))
Bahrain
dim(Bahrain)
# I will first  initialize 1-year matrix (80 x 80)
Bahrain_1y <- matrix(0, nrow = 80, ncol = 80)

# Expand each 5-year group into 5 x 5 block
for (i in 1:16) {
  for (j in 1:16) {
    row_idx <- ((i - 1) * 5 + 1):(i * 5)
    col_idx <- ((j - 1) * 5 + 1):(j * 5)
    Bahrain_1y[row_idx, col_idx] <- Bahrain[i, j]
  }
}

Bahrain_1y
dim(Bahrain_1y)
#2.
Egypt<- as.matrix(read_excel("Contact matrix.152 countries/pcbi.1005697.s002/contact_matrices_152_countries/MUestimates_all_locations_1.xlsx", 
                             sheet = "Egypt"))
Egypt
dim(Egypt)
Egypt_1y <- matrix(0, nrow = 80, ncol = 80)
# Expand each 5-year group into 5 x 5 block
for (i in 1:16) {
  for (j in 1:16) {
    row_idx <- ((i - 1) * 5 + 1):(i * 5)
    col_idx <- ((j - 1) * 5 + 1):(j * 5)
    Egypt_1y[row_idx, col_idx] <- Egypt[i, j]
  }
}
Egypt_1y
dim(Egypt_1y)

#3.
Iran<-as.matrix(read_excel("Contact matrix.152 countries/pcbi.1005697.s002/contact_matrices_152_countries/MUestimates_all_locations_1.xlsx", 
                           sheet = "Iran (Islamic Republic of)"))
Iran
Iran_1y <- matrix(0, nrow = 80, ncol = 80)
# Expand each 5-year group into 5 x 5 block
for (i in 1:16) {
  for (j in 1:16) {
    row_idx <- ((i - 1) * 5 + 1):(i * 5)
    col_idx <- ((j - 1) * 5 + 1):(j * 5)
    Iran_1y[row_idx, col_idx] <- Iran[i, j]
  }
}
Iran_1y
dim(Iran_1y)
#4.
Iraq<-as.matrix(read_excel("Contact matrix.152 countries/pcbi.1005697.s002/contact_matrices_152_countries/MUestimates_all_locations_1.xlsx", 
                           sheet = "Iraq"))
Iraq
Iraq_1y <- matrix(0, nrow = 80, ncol = 80)
# Expand each 5-year group into 5 x 5 block
for (i in 1:16) {
  for (j in 1:16) {
    row_idx <- ((i - 1) * 5 + 1):(i * 5)
    col_idx <- ((j - 1) * 5 + 1):(j * 5)
    Iraq_1y[row_idx, col_idx] <- Iraq[i, j]
  }
}
Iraq_1y
dim(Iraq_1y)
#5
Jordan <- as.matrix(read_excel("Contact matrix.152 countries/pcbi.1005697.s002/contact_matrices_152_countries/MUestimates_all_locations_1.xlsx", 
                               sheet = "Jordan"))
Jordan
Jordan_1y <- matrix(0, nrow = 80, ncol = 80)
# Expand each 5-year group into 5 x 5 block
for (i in 1:16) {
  for (j in 1:16) {
    row_idx <- ((i - 1) * 5 + 1):(i * 5)
    col_idx <- ((j - 1) * 5 + 1):(j * 5)
    Jordan_1y[row_idx, col_idx] <- Jordan[i, j]
  }
}
Jordan_1y
dim(Jordan_1y)

#6.
Kuwait <- as.matrix(read_excel("Contact matrix.152 countries/pcbi.1005697.s002/contact_matrices_152_countries/MUestimates_all_locations_1.xlsx", 
                               sheet = "Kuwait"))
Kuwait
Kuwait_1y <- matrix(0, nrow = 80, ncol = 80)
# Expand each 5-year group into 5 x 5 block
for (i in 1:16) {
  for (j in 1:16) {
    row_idx <- ((i - 1) * 5 + 1):(i * 5)
    col_idx <- ((j - 1) * 5 + 1):(j * 5)
    Kuwait_1y[row_idx, col_idx] <- Kuwait[i, j]
  }
}
Kuwait_1y
dim(Kuwait_1y)
    
#7
Lebanon <-as.matrix(read_excel("Contact matrix.152 countries/pcbi.1005697.s002/contact_matrices_152_countries/MUestimates_all_locations_1.xlsx", 
                               sheet = "Lebanon"))
Lebanon 
Lebanon_1y <- matrix(0, nrow = 80, ncol = 80)
# Expand each 5-year group into 5 x 5 block
for (i in 1:16) {
  for (j in 1:16) {
    row_idx <- ((i - 1) * 5 + 1):(i * 5)
    col_idx <- ((j - 1) * 5 + 1):(j * 5)
    Lebanon_1y[row_idx, col_idx] <-Lebanon[i, j]
  }
}
Lebanon_1y
dim(Lebanon_1y)

#8
Morocco <-as.matrix(read_excel("Contact matrix.152 countries/pcbi.1005697.s002/contact_matrices_152_countries/MUestimates_all_locations_1.xlsx", 
                               sheet = "Morocco"))
Morocco
Morocco_1y <- matrix(0, nrow = 80, ncol = 80)
# Expand each 5-year group into 5 x 5 block
for (i in 1:16) {
  for (j in 1:16) {
    row_idx <- ((i - 1) * 5 + 1):(i * 5)
    col_idx <- ((j - 1) * 5 + 1):(j * 5)
    Morocco_1y[row_idx, col_idx] <-Morocco[i, j]
  }
}
Morocco_1y
dim(Morocco_1y)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#9.
Oman<- as.matrix(read_excel("Contact matrix.152 countries/pcbi.1005697.s002/contact_matrices_152_countries/MUestimates_all_locations_2.xlsx", 
                            sheet = "Pakistan"))
Oman
Oman_1y <- matrix(0, nrow = 80, ncol = 80)
# Expand each 5-year group into 5 x 5 block
for (i in 1:16) {
  for (j in 1:16) {
    row_idx <- ((i - 1) * 5 + 1):(i * 5)
    col_idx <- ((j - 1) * 5 + 1):(j * 5)
    Oman_1y[row_idx, col_idx] <-Oman[i, j]
  }
}
Oman_1y
dim(Oman_1y)

#10.
Pakistan <- as.matrix(read_excel("Contact matrix.152 countries/pcbi.1005697.s002/contact_matrices_152_countries/MUestimates_all_locations_2.xlsx", 
                                 sheet = "Pakistan"))
Pakistan
Pakistan_1y <- matrix(0, nrow = 80, ncol = 80)
# Expand each 5-year group into 5 x 5 block
for (i in 1:16) {
  for (j in 1:16) {
    row_idx <- ((i - 1) * 5 + 1):(i * 5)
    col_idx <- ((j - 1) * 5 + 1):(j * 5)
    Pakistan_1y[row_idx, col_idx] <-Pakistan[i, j]
  }
}
Pakistan_1y
dim(Pakistan_1y)

#11
Qatar<-as.matrix(read_excel("Contact matrix.152 countries/pcbi.1005697.s002/contact_matrices_152_countries/MUestimates_all_locations_2.xlsx", 
                            sheet = "Qatar"))
Qatar
Qatar_1y <- matrix(0, nrow = 80, ncol = 80)
# Expand each 5-year group into 5 x 5 block
for (i in 1:16) {
  for (j in 1:16) {
    row_idx <- ((i - 1) * 5 + 1):(i * 5)
    col_idx <- ((j - 1) * 5 + 1):(j * 5)
    Qatar_1y[row_idx, col_idx] <-Qatar[i, j]
  }
}
Qatar_1y
dim(Qatar_1y)

#12
Syrian<- as.matrix(read_excel("Contact matrix.152 countries/pcbi.1005697.s002/contact_matrices_152_countries/MUestimates_all_locations_2.xlsx", 
                              sheet = "Syrian Arab Republic"))
Syrian
Syrian_1y <- matrix(0, nrow = 80, ncol = 80)
# Expand each 5-year group into 5 x 5 block
for (i in 1:16) {
  for (j in 1:16) {
    row_idx <- ((i - 1) * 5 + 1):(i * 5)
    col_idx <- ((j - 1) * 5 + 1):(j * 5)
    Syrian_1y[row_idx, col_idx] <-Syrian[i, j]
  }
}
Syrian_1y
dim(Syrian_1y)

#13.
Saudi_Arabia<-as.matrix(read_excel("Contact matrix.152 countries/pcbi.1005697.s002/contact_matrices_152_countries/MUestimates_all_locations_2.xlsx", 
                                   sheet = "Saudi Arabia"))
Saudi_Arabia
Saudi_Arabia_1y <- matrix(0, nrow = 80, ncol = 80)
# Expand each 5-year group into 5 x 5 block
for (i in 1:16) {
  for (j in 1:16) {
    row_idx <- ((i - 1) * 5 + 1):(i * 5)
    col_idx <- ((j - 1) * 5 + 1):(j * 5)
    Saudi_Arabia_1y[row_idx, col_idx] <-Syrian[i, j]
  }
}
Saudi_Arabia_1y
dim(Saudi_Arabia_1y)

#14
United_Arab_Emirates<-as.matrix(read_excel("Contact matrix.152 countries/pcbi.1005697.s002/contact_matrices_152_countries/MUestimates_all_locations_2.xlsx", 
                                           sheet = "United Arab Emirates"))
United_Arab_Emirates
United_Arab_Emirates_1y <- matrix(0, nrow = 80, ncol = 80)
# Expand each 5-year group into 5 x 5 block
for (i in 1:16) {
  for (j in 1:16) {
    row_idx <- ((i - 1) * 5 + 1):(i * 5)
    col_idx <- ((j - 1) * 5 + 1):(j * 5)
    United_Arab_Emirates_1y[row_idx, col_idx] <-United_Arab_Emirates[i, j]
  }
}
United_Arab_Emirates_1y
dim(United_Arab_Emirates_1y)

#15
Tunisia<-as.matrix(read_excel("Contact matrix.152 countries/pcbi.1005697.s002/contact_matrices_152_countries/MUestimates_all_locations_2.xlsx", 
                              sheet = "Tunisia"))
Tunisia
Tunisia_1y <- matrix(0, nrow = 80, ncol = 80)
# Expand each 5-year group into 5 x 5 block
for (i in 1:16) {
  for (j in 1:16) {
    row_idx <- ((i - 1) * 5 + 1):(i * 5)
    col_idx <- ((j - 1) * 5 + 1):(j * 5)
    Tunisia_1y[row_idx, col_idx] <-Tunisia[i, j]
  }
}
Tunisia_1y
dim(Tunisia_1y)
#16
Yemen<-as.matrix(read_excel("Contact matrix.152 countries/pcbi.1005697.s002/contact_matrices_152_countries/MUestimates_all_locations_2.xlsx", 
                            sheet = "Yemen"))
Yemen
Yemen_1y <- matrix(0, nrow = 80, ncol = 80)
# Expand each 5-year group into 5 x 5 block
for (i in 1:16) {
  for (j in 1:16) {
    row_idx <- ((i - 1) * 5 + 1):(i * 5)
    col_idx <- ((j - 1) * 5 + 1):(j * 5)
    Yemen_1y[row_idx, col_idx] <-Yemen[i, j]
  }
}
Yemen_1y
dim(Yemen_1y)

#Afghanistan,Palestine/Djibouti/Libya/Somalia/ and Sudan are not available (should we replace it by polymod? or syntetic??)
#Their codes are:codes<-c("PSE", "DJI", "LBY", YEMS, "SOM", "SDN")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The most recent updated contact matrix 2021 have  177 countries
# Kiesha Prem et al.2021.https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1009098
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~--
synthetic<-load("C:/Courses/Placement project/Contact matrix.177 countries/8383778/kieshaprem/synthetic-contact-matrices-v2.0/kieshaprem-synthetic-contact-matrices-eea4d02/output/syntheticmatrices/contact_all.rdata")
str(synthetic)
#
get
synthetic<-load("3.U.1.synthetic_contacts.rdata")
synthetic



library(readr)
synthetic_contacts_2021<- read_csv("Contact matrix.177 countries/8383778/kieshaprem/synthetic-contact-matrices-v2.0/kieshaprem-synthetic-contact-matrices-eea4d02/generate_synthetic_matrices/output/syntheticmatrices/synthetic_contacts_2021.csv")
synthetic_contacts_2021$iso3c
any(synthetic_contacts_2021$iso3c == "SOM")  # Should return FALSE if missing
any(synthetic_contacts_2021$iso3c == "YEM")  # Likewise for Yemen

#First ,i will check the countries that we do not have contact matrix
#countries <- c("Palestine", "Djibouti", "Libya", "Afghanistan", "Somalia", "Sudan")
codes<-c("AFG","PSE", "DJI", "LBY", "SOM", "SDN","YEM")
synthetic_contacts<-synthetic_contacts_2021|>
  filter(iso3c %in% codes)
synthetic_contacts
table(synthetic_contacts_2021$iso3c)
# Here i will need to rebuild the contact matrix for each country
age_groups <- c("0 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24", 
                "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49",
                "50 to 54", "55 to 59", "60 to 64", "65 to 69", "70 to 74",
                "75 to 79", "80 to 84", "85 to 89", "90 to 94", "95 to 99", "100+")

pacman::p_load(tidyr,dplyr)
rebuild_contact_matrix <- function(synthetic_contacts, country_code, contact_setting = "overall", contact_location = "all") {
  
  # Here, i will filter data and handle duplicates by taking mean (average contact)
  filtered <- synthetic_contacts%>%
    filter(iso3c == country_code,
           setting == contact_setting,
           location_contact == contact_location | contact_location == "all") %>%
    group_by(age_contactor, age_cotactee) %>%
    summarise(mean_contacts = mean(mean_number_of_contacts), .groups = "drop")
  
  # Create matrix
  contact_matrix_1 <- filtered %>%
    pivot_wider(names_from = age_cotactee, 
                values_from = mean_contacts,
                values_fill = 0) %>%  # Here i will fill missing combinations with 0
    arrange(factor(age_contactor, levels = age_groups)) %>%
    select(-age_contactor) %>%
    as.matrix()
  
  # Set row/col names
  rownames(contact_matrix_1) <- colnames(contact_matrix_1) <- age_groups[1:nrow(contact_matrix_1)]
  
  return(contact_matrix_1)
}
#17
Djibouti<- rebuild_contact_matrix(synthetic_contacts, "DJI")
Djibouti
dim(Djibouti)
Djibouti_1y <- matrix(0, nrow = 80, ncol = 80)
# Expand each 5-year group into 5 x 5 block
for (i in 1:16) {
  for (j in 1:16) {
    row_idx <- ((i - 1) * 5 + 1):(i * 5)
    col_idx <- ((j - 1) * 5 + 1):(j * 5)
    Djibouti_1y[row_idx, col_idx] <-Djibouti[i, j]
  }
}
Djibouti_1y
dim(Djibouti_1y)

#18
Palestine<- rebuild_contact_matrix(synthetic_contacts, "PSE")
Palestine
Palestine_1y <- matrix(0, nrow = 80, ncol = 80)
# Expand each 5-year group into 5 x 5 block
for (i in 1:16) {
  for (j in 1:16) {
    row_idx <- ((i - 1) * 5 + 1):(i * 5)
    col_idx <- ((j - 1) * 5 + 1):(j * 5)
    Palestine_1y[row_idx, col_idx] <-Palestine[i, j]
  }
}
Palestine_1y
dim(Palestine_1y)
#19
Lybia<- rebuild_contact_matrix(synthetic_contacts, "LBY")
Lybia
Lybia_1y <- matrix(0, nrow = 80, ncol = 80)
# Expand each 5-year group into 5 x 5 block
for (i in 1:16) {
  for (j in 1:16) {
    row_idx <- ((i - 1) * 5 + 1):(i * 5)
    col_idx <- ((j - 1) * 5 + 1):(j * 5)
    Lybia_1y[row_idx, col_idx] <-Lybia[i, j]
  }
}
Lybia_1y
dim(Lybia_1y)
#20
Sudan<- rebuild_contact_matrix(synthetic_contacts, "SDN")
Sudan
Sudan_1y <- matrix(0, nrow = 80, ncol = 80)
# Expand each 5-year group into 5 x 5 block
for (i in 1:16) {
  for (j in 1:16) {
    row_idx <- ((i - 1) * 5 + 1):(i * 5)
    col_idx <- ((j - 1) * 5 + 1):(j * 5)
    Sudan_1y[row_idx, col_idx] <-Sudan[i, j]
  }
}
Sudan_1y
dim(Sudan_1y)
#Additional July 7,2025
Afghanistan<- rebuild_contact_matrix(synthetic_contacts, "AFG")
Afghanistan
Afghanistan_1y <- matrix(0, nrow = 80, ncol = 80)
# Expand each 5-year group into 5 x 5 block
for (i in 1:16) {
  for (j in 1:16) {
    row_idx <- ((i - 1) * 5 + 1):(i * 5)
    col_idx <- ((j - 1) * 5 + 1):(j * 5)
    Afghanistan_1y[row_idx, col_idx] <-Afghanistan[i, j]
  }
}
Afghanistan_1y
dim(Afghanistan_1y)

#YEMEN
Yemen<- rebuild_contact_matrix(synthetic_contacts, "YEM")
Yemen
Yemen_1y <- matrix(0, nrow = 80, ncol = 80)
# Expand each 5-year group into 5 x 5 block
for (i in 1:16) {
  for (j in 1:16) {
    row_idx <- ((i - 1) * 5 + 1):(i * 5)
    col_idx <- ((j - 1) * 5 + 1):(j * 5)
    Yemen_1y[row_idx, col_idx] <-Yemen[i, j]
  }
}
Yemen_1y
dim(Yemen_1y)


#Yemen<- rebuild_contact_matrix(synthetic_contacts, "YEM")#Issues  to be fixed
#Yemen
#Somalia<- rebuild_contact_matrix(synthetic_contacts, "SOM") #Issues wih somalia to be fixed
#Sudan
Sudan<- rebuild_contact_matrix(synthetic_contacts, "SDN")


Sudan
Sudan_1y <- matrix(0, nrow = 80, ncol = 80)
# Expand each 5-year group into 5 x 5 block
for (i in 1:16) {
  for (j in 1:16) {
    row_idx <- ((i - 1) * 5 + 1):(i * 5)
    col_idx <- ((j - 1) * 5 + 1):(j * 5)
    Sudan_1y[row_idx, col_idx] <-Sudan[i, j]
  }
}
Sudan_1y
dim(Sudan_1y)


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@    Now contacts matrix are 80 x 80  of 1 years , we need to complete the remaining part 80 to 101 @                  @
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#A<-21 # Number of age groups/age floors
A<-101 # Number of age groups/age floors including <1 of the matrix that we want
C<-80        # Number of age groups for contact matrix that we have
(nce <- A - C) # Number of age groups for which we do not have contacts
# filling in 4 higher age groups 75-80, 80-85, 85-90, 95-100, 100+ (using one year age groups)
#From Prem et al 2017
contact_Afghanistan <- matrix(0, nrow = A, ncol = A) 
contact_Afghanistan                                            #?
dim(contact_Afghanistan)
contact_Bahrain <- matrix(0, nrow = A, ncol = A)           #1
contact_Bahrain                                            #?
dim(contact_Bahrain)
contact_Egypt<- matrix(0, nrow = A, ncol = A)              #2
contact_Iran <- matrix(0, nrow = A, ncol = A)              #3
contact_Iraq <- matrix(0, nrow = A, ncol = A)              #4
contact_Jordan<- matrix(0, nrow = A, ncol = A)             #5
contact_Kuwait<- matrix(0, nrow = A, ncol = A)             #6
contact_Lebanon<- matrix(0, nrow = A, ncol = A)            #7
contact_Morocco <- matrix(0, nrow = A, ncol = A)           #8
contact_Oman<- matrix(0, nrow = A, ncol = A)               #9
contact_Pakistan<- matrix(0, nrow = A, ncol = A)           #10
contact_Qatar<- matrix(0, nrow = A, ncol = A)              #11
contact_Syrian<- matrix(0, nrow = A, ncol = A)              #12
contact_Saudi_Arabia<- matrix(0, nrow = A, ncol = A)        #13
contact_United_Arab_Emirates<- matrix(0, nrow = A, ncol = A) #14
contact_Tunisia<- matrix(0, nrow = A, ncol = A)              #15
contact_Yemen<- matrix(0, nrow = A, ncol = A)                #16
#From prem et al.2021
contact_Djibouti<- matrix(0, nrow = A, ncol = A) #17
contact_Palestine<- matrix(0, nrow = A, ncol = A) #18
contact_Lybia<- matrix(0, nrow = A, ncol = A) #19
contact_Sudan<- matrix(0, nrow = A, ncol = A) #20
contact_somalia<- matrix(0, nrow = A, ncol = A) #21
dim(contact_somalia)
#This code below fill the 80*80 by letting the 80-101*81-101 by zero (initial value)
for (i in 1:(A - nce)){                                 #From 1st row 80th row 
  for (j in 1:(A - nce)){                               #From 1st col to 80th column
    contact_Afghanistan[i, j] <-Afghanistan_1y[i, j]
    contact_Bahrain[i, j] <-Bahrain_1y[i, j]            #i is row and j is  a column
    contact_Egypt[i, j] <-Egypt_1y[i, j]
    contact_Iran[i,j] <- Iran_1y[i, j]
    contact_Iraq[i, j] <- Iraq_1y[i, j]
    contact_Jordan[i,j] <-Jordan_1y[i, j]
    contact_Kuwait[i,j] <- Kuwait_1y[i, j]
    contact_Lebanon[i,j] <- Lebanon_1y[i, j]
    contact_Morocco[i,j] <- Morocco_1y[i, j]
    contact_Oman[i,j] <- Oman_1y[i, j]
    contact_Pakistan[i,j] <-Pakistan_1y[i, j]
    contact_Qatar[i,j] <-Qatar_1y[i, j]
    contact_Syrian[i,j] <-Syrian_1y[i, j]
    contact_Saudi_Arabia[i,j] <- Saudi_Arabia_1y[i, j]
    contact_United_Arab_Emirates[i,j] <- United_Arab_Emirates_1y[i, j]
    contact_Tunisia[i,j] <- Tunisia_1y[i, j]
    #
    contact_Yemen[i,j] <-Yemen_1y[i, j]
    contact_Djibouti[i,j] <- Djibouti_1y[i, j]
    contact_Palestine[i,j] <-Palestine_1y[i, j]
    contact_Lybia[i,j] <- Lybia_1y[i, j]
    contact_Sudan[i,j] <- Sudan_1y[i, j]
    
  }
}
contact_Afghanistan

#This code below fills the row 17 to 21 by letting the colum at initial value(zero)
for (i in (A + 1 - nce):A){                         # From 17th row  to 21th row
  for (j in 1:(A - nce)){                           # from 1st column to 16th column
    contact_Afghanistan[i, j] <- Afghanistan_1y[(A - nce), j]
    contact_Bahrain[i, j] <- Bahrain_1y[(A - nce), j]  #Value of J in 17th row  to 21th row(A-nce)
    contact_Egypt[i, j] <-Egypt_1y[(A - nce), j]
    contact_Iran[i, j] <- Iran_1y[(A - nce), j]
    contact_Iraq[i, j] <- Iraq_1y[(A - nce), j]
    contact_Jordan[i, j] <-Jordan_1y[(A - nce), j]
    contact_Kuwait[i, j] <- Kuwait_1y[(A - nce), j]
    contact_Lebanon[i, j] <- Lebanon_1y[(A - nce), j]
    contact_Morocco[i, j] <- Morocco_1y[(A - nce), j]
    contact_Oman[i, j] <- Oman_1y[(A - nce), j]
    contact_Pakistan[i, j] <-Pakistan_1y[(A - nce), j]
    contact_Qatar[i, j] <-Qatar_1y[(A - nce), j]
    contact_Syrian[i, j] <-Syrian_1y[(A - nce), j]
    contact_Saudi_Arabia[i, j] <- Saudi_Arabia_1y[(A - nce), j]
    contact_United_Arab_Emirates[i, j] <- United_Arab_Emirates_1y[(A - nce), j] 
    contact_Tunisia[i, j] <-Tunisia_1y[(A - nce), j]
    #
    contact_Yemen[i, j] <-Yemen_1y[(A - nce), j]
    contact_Djibouti[i, j] <- Djibouti_1y[(A - nce), j]
    contact_Palestine[i, j] <-Palestine_1y[(A - nce), j]
    contact_Lybia[i, j] <- Lybia_1y[(A - nce), j]
    contact_Sudan[i, j] <- Sudan_1y[(A - nce), j]
  }
}

#This code below fills the remain columns by value instead initial values 
for (i in 1:(A - nce)){                             #From 1th row to 16th row
  for (j in (A + 1 - nce):A){                       #From 16th  colum to 21th colum
    contact_Afghanistan[i, j]<-Afghanistan_1y[i, (A - nce)]
    contact_Bahrain[i, j]<-Bahrain_1y[i, (A - nce)]       #Value of i in (A-nce) columns
    contact_Egypt[i, j] <-Egypt_1y[i, (A - nce)]
    contact_Iran[i, j] <- Iran_1y[i, (A - nce)]
    contact_Iraq[i, j] <- Iraq_1y[i, (A - nce)]
    contact_Jordan[i, j] <-Jordan_1y[i, (A - nce)]
    contact_Kuwait[i, j] <- Kuwait_1y[i, (A - nce)]
    contact_Lebanon[i, j] <- Lebanon_1y[i, (A - nce)]
    contact_Morocco[i, j] <- Morocco_1y[i, (A - nce)]
    contact_Oman[i, j] <- Oman_1y[i, (A - nce)]
    contact_Pakistan[i, j] <-Pakistan_1y[i, (A - nce)]
    contact_Qatar[i, j] <-Qatar_1y[i, (A - nce)]
    contact_Syrian[i, j] <-Syrian_1y[i, (A - nce)]
    contact_Saudi_Arabia[i, j] <- Saudi_Arabia_1y[i, (A - nce)]
    contact_United_Arab_Emirates[i, j] <- United_Arab_Emirates_1y[i, (A - nce)]
    contact_Tunisia[i, j] <- Tunisia_1y[i, (A - nce)]
    #
    contact_Yemen[i, j] <-Yemen_1y[i, (A - nce)]
    contact_Djibouti[i, j] <- Djibouti_1y[i, (A - nce)]
    contact_Palestine[i, j] <-Palestine_1y[i, (A - nce)]
    contact_Lybia[i, j] <- Lybia_1y[i, (A - nce)]
    contact_Sudan[i, j] <- Sudan_1y[i, (A - nce)]
  }
}

#This code combine the previous ones
for (i in (A + 1 - nce):A){
  for (j in (A + 1 - nce):A){
    contact_Afghanistan[i, j] <- Afghanistan_1y[(A - nce),(A - nce)]
    contact_Bahrain[i, j] <- Bahrain_1y[(A - nce),(A - nce)] #values  in A-nce row  and columns
    contact_Egypt[i, j] <-Egypt_1y[(A - nce),(A - nce)]
    contact_Iran[i, j] <- Iran_1y[(A - nce),(A - nce)]
    contact_Iraq[i, j] <- Iraq_1y[(A - nce),(A - nce)]
    contact_Jordan[i, j] <-Jordan_1y[(A - nce),(A - nce)]
    contact_Kuwait[i, j] <- Kuwait_1y[(A - nce),(A - nce)]
    contact_Lebanon[i, j] <- Lebanon_1y[(A - nce),(A - nce)]
    contact_Morocco[i, j] <- Morocco_1y[(A - nce),(A - nce)]
    contact_Oman[i, j] <- Oman_1y[(A - nce),(A - nce)]
    contact_Pakistan[i, j] <-Pakistan_1y[(A - nce),(A - nce)]
    contact_Qatar[i, j] <-Qatar_1y[(A - nce),(A - nce)]
    contact_Syrian[i, j] <-Syrian_1y[(A - nce),(A - nce)]
    contact_Saudi_Arabia[i, j] <- Saudi_Arabia_1y[(A - nce),(A - nce)]
    contact_United_Arab_Emirates[i, j] <-United_Arab_Emirates_1y[(A - nce),(A - nce)]
    contact_Tunisia[i, j] <-Tunisia_1y[(A - nce),(A - nce)]
    #
    contact_Yemen[i, j] <-Yemen_1y[(A - nce), (A - nce)]
    contact_Djibouti[i, j] <- Djibouti_1y[(A - nce), (A - nce)]
    contact_Palestine[i, j] <-Palestine_1y[(A - nce), (A - nce)]
    contact_Lybia[i, j] <- Lybia_1y[(A - nce), (A - nce)]
    contact_Sudan[i, j] <- Sudan_1y[(A - nce), (A - nce)]
  }
}
contact_Afghanistan
dim(contact_Afghanistan)
contact_Afghanistan_1y<-contact_Afghanistan
write.csv(contact_Afghanistan_1y, file = "3.U.1.contact_Afghanistan_1y.csv", row.names = FALSE)

contact_Bahrain_1y<-contact_Bahrain
write.csv(contact_Bahrain_1y, file = "3.U.1.contact_Bahrain_1y.csv", row.names = FALSE)

(contact_Egypt_1y<-contact_Egypt)
write.csv(contact_Egypt_1y, file = "3.U.1.contact_Egypt_1y.csv", row.names = FALSE)
(contact_Iran_1y<-contact_Iran)
write.csv(contact_Iran_1y, file = "3.U.1.contact_Iran_1y.csv", row.names = FALSE)
contact_Iraq_1y<-contact_Iraq
write.csv(contact_Iraq_1y, file = "3.U.1.contact_Iraq_1y.csv", row.names = FALSE)
contact_Jordan_1y<-contact_Jordan
write.csv(contact_Jordan_1y, file = "3.U.1.contact_Jordan_1y.csv", row.names = FALSE)
contact_Kuwait_1y<-contact_Kuwait
write.csv(contact_Kuwait_1y, file = "3.U.1.contact_Kuwait_1y.csv", row.names = FALSE)
contact_Lebanon_1y<-contact_Lebanon
write.csv(contact_Lebanon_1y, file = "3.U.1.contact_Lebanon_1y.csv", row.names = FALSE)
contact_Morocco_1y<-contact_Morocco
write.csv(contact_Morocco_1y, file = "3.U.1.contact_Morocco_1y.csv", row.names = FALSE)
contact_Oman_1y<-contact_Oman
write.csv(contact_Oman_1y, file = "3.U.1.contact_Oman_1y.csv", row.names = FALSE)
contact_Pakistan_1y<-contact_Pakistan
write.csv(contact_Pakistan_1y, file = "3.U.1.contact_Pakistan_1y.csv", row.names = FALSE)
contact_Qatar_1y<-contact_Qatar
write.csv(contact_Qatar_1y, file = "3.U.1.contact_Qatar_1y.csv", row.names = FALSE)
contact_Syrian_1y<-contact_Syrian
write.csv(contact_Syrian_1y, file = "3.U.1.contact_Syrian_1y.csv", row.names = FALSE)
contact_Saudi_Arabia_1y<-contact_Saudi_Arabia
write.csv(contact_Saudi_Arabia_1y, file = "3.U.1.contact_Saudi_Arabia_1y.csv", row.names = FALSE)
contact_United_Arab_Emirates_1y<-contact_United_Arab_Emirates
write.csv(contact_United_Arab_Emirates_1y, file = "3.U.1.contact_United_Arab_Emirates_1y.csv", row.names = FALSE)
contact_Tunisia_1y<-contact_Tunisia
write.csv(contact_Tunisia_1y, file = "3.U.1.contact_Tunisia_1y.csv", row.names = FALSE)
#
contact_Djibouti_1y<-contact_Djibouti
write.csv(contact_Djibouti_1y, file = "3.U.1.contact_Djibouti_1y.csv", row.names = FALSE)
contact_Palestine_1y<-contact_Palestine
write.csv(contact_Palestine_1y, file = "3.U.1.contact_Palestine_1y.csv", row.names = FALSE)
contact_Lybia_1y<-contact_Lybia
write.csv(contact_Lybia_1y, file = "3.U.1.contact_Lybia_1y.csv", row.names = FALSE)
(contact_Yemen_1y<-contact_Yemen)
write.csv(contact_Yemen_1y, file = "3.U.1.contact_Yemen_1y.csv", row.names = FALSE)
(contact_Sudan_1y<-contact_Sudan)
write.csv(contact_Sudan_1y, file = "3.U.1.contact_Sudan_1y.csv", row.names = FALSE)

#For somalia which do not have contact matrix (empirically or sythetically)
#I will use polymod#
pacman::p_load(socialmixr)
data(polymod)   #Contact data
print(polymod)
#contact_matrix <- contact_matrix(polymod, age.limits = c(0, 5, 10, 15, 20, 30, 40, 50, 60))
#N.B: With POLYMOD data, we can change age.limits in contact_matrix() to match our disease model or policy question.
#     1.The final group always includes everyone greater than or equal to the last cutoff.
#     2.Make sure your population data (if you use survey.pop) matches the age groups you define.
age_breaks <- 0:100
(contact_somalia_1y <- contact_matrix(polymod, age.limits = age_breaks))
colnames(contact_Pakistan_1y) <-c(as.character(0:99), "100+")
rownames(contact_Pakistan_1y) <- c(as.character(0:99), "100+")
write.csv(contact_somalia_1y, file = "3.U.1.contact_somalia_1y.csv", row.names = FALSE)

#For each country
colnames(contact_Pakistan_1y) <-c(as.character(0:99), "100+")

rownames(contact_Pakistan_1y) <- c(as.character(0:99), "100+")
contact_Pakistan_1y

#Visualization
pacman::p_load(ggplot2,reshape2)

df <- melt(contact_Pakistan_1y)
colnames(df) <- c("Contactee", "Contactor", "Contacts")

ggplot(df, aes(x = Contactor, y = Contactee, fill = Contacts)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "purple") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=1))+
  labs(title = "Contact Matrix Heatmap for Pakistan")
ggplot(df, aes(x = Contactor, y = Contactee, fill = Contacts)) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme_minimal()  +
  labs(title = "Contact Matrix Heatmap for Pakistan")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Regional level
#Prem et al. (2017)
emro_countries <- c(
  "Afghanistan", "Bahrain", "Djibouti", "Egypt", "Iran", "Iraq", "Jordan",
  "Kuwait", "Lebanon", "Libya", "Morocco", "Oman", "Pakistan", "Palestine",
  "Qatar", "SaudiArabia", "Somalia", "Sudan", "Syria", "Tunisia",
  "UnitedArabEmirates", "Yemen"
)

library(readr)
library(boot)
library(table1)

#table1(~as.factor(Country_Code)+Population_Value+Age_Category,data=age_str_long1)
#table1(~ Population_Value + Country_Code|Age_Category, data = age_str_long1)

#Mortality/ Cases Fatlity Rate
#https://www.un.org/development/desa/pd/data-landing-page
#https://www.un.org/development/desa/pd/sites/www.un.org.development.desa.pd/files/files/documents/2020/Jan/un_2019_worldmortality_databooklet.pdf
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Descriptive analysis
pacman::p_load(EpiSignalDetection,tidyverse,rio,here,broom,plm,ggrepel,ggplot2,ggmap,gganimate)
# Simulate weekly flu case data
observed_cases <- c(10, 15, 20, 45, 35, 25, 30, 50, 55, 45)
#data<-Measles_cases_all_8
library(readxl)
expected_cases <- rep(mean(observed_cases), length(observed_cases))

pacman::p_load(readxl)
data<- read_excel("3.1.Measles cases all_8.xlsx") #
#View(data)
data<-as.data.frame(data)
attach(data)
names(data)
summary(data)
Africa.cases<-data$Africa
expected_cases<-as.numeric(Africa.cases)

# Calculate a basic signal detection based on mean and standard deviation
threshold <- mean(expected_cases) + 2 * sd(expected_cases)  # 2 standard deviations above the mean
outbreak_signal <- observed_cases > threshold
Week<-1:10
data<-cbind(observed_cases,expected_cases,threshold,outbreak_signal)
data<-as.data.frame(data)
#table(data[,1],data[,5])

if (any(outbreak_signal)) {
  print("Outbreak signal detected!")
} else {
  print("No outbreak signal detected.")
}

# Simulate Weekly Disease Case Counts (e.g., Influenza cases)
set.seed(123)  # For reproducibility
weeks <- 1:30  # Weeks of data
base_cases <- rep(10, 20)  # First 20 weeks of normal disease cases
outbreak_cases <- rep(10, 10) + rnorm(10, mean=15, sd=5)  # Outbreak in the last 10 weeks
cases <- c(base_cases, outbreak_cases)  # Combine base and outbreak data

# Moving Average for Smoothing (3-week moving average)
#moving_avg <- filter(cases, rep(1/3, 3), sides=2)
moving_avg <- stats::filter(cases, rep(1/3, 3), sides = 1)
# Calculate Threshold for Outbreak Detection (2 standard deviations above moving average)
threshold <- moving_avg #+ 2 * sd(cases)

# Plot the Data and Detection Signal
plot(weeks, cases, type='b', pch=19, col='blue', main="Disease Cases with Signal Detection", 
     xlab="Week", ylab="Cases", ylim=c(0, max(cases) + 10))
lines(weeks, moving_avg, col='green', lwd=2)  # Plot moving average
lines(weeks, threshold, col='red', lwd=2, lty=2)  # Plot threshold

# Highlight where the signal is triggered (cases above threshold)
outbreak_signal <- cases > threshold
points(weeks[outbreak_signal], cases[outbreak_signal], col='red', pch=19, cex=1.5)  # Red dots for signals

legend("bottomright", legend=c("Cases", "Moving Average", "Threshold", "Outbreak Signal"),
       col=c("blue", "green", "red", "red"), pch=c(19, NA, NA, 19), lty=c(NA, 1, 2, NA), 
       lwd=c(NA, 2, 2, NA), cex=0.8)

#Using ggplot2
# Between-country inequality (coefficient of variation)

#Packages
if (!require("pacman"))
  install.packages("pacman")
pacman::p_load(tidyverse, rio, here, broom, plm, ggrepel)
options(scipen = 999)

#data<-Measles_cases_all_8
print(data)
attach(data)
names(data)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   Cases at the regional level                                               
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pacman::p_load(readxl)
data<- read_excel("3.1.Measles cases all_8.xlsx") #
#View(data)
data<-as.data.frame(data)
attach(data)
names(data)
summary(data)
historical_cases<-data|> 
  ggplot(aes(x = Period)) +
  geom_point(aes(y = data$`Western Pacific`, color = "Western Pacific")) +
  geom_line(aes(y = data$`Western Pacific`, color = "Western Pacific")) +
  geom_point(aes(y = data$`South East Asia`, color = "South East Asia")) +
  geom_line(aes(y = data$`South East Asia`, color = "South East Asia")) +
  geom_point(aes(y = data$Europe, color = "Europe")) +
  geom_line(aes(y = data$Europe, color = "Europe")) +
  geom_point(aes(y = data$`Eastern Mediterranean`, color = "Eastern Mediterranean")) +
  geom_line(aes(y = data$`Eastern Mediterranean`, color = "Eastern Mediterranean")) +
  geom_point(aes(y = data$Americas, color = "Americas")) +
  geom_line(aes(y = data$Americas, color = "Americas")) +
  geom_point(aes(y = data$Africa, color = "Africa")) +
  geom_line(aes(y = data$Africa, color = "Africa")) +
  scale_color_manual(values = c(
    "Western Pacific"= "green",
    "South East Asia" = "navy",
    "Europe" = "pink",
    "Eastern Mediterranean" = "red",
    "Americas" = "blue",
    "Africa" = "purple"
  )) +
  
  scale_x_continuous(breaks = 1980:2023) +
  scale_y_continuous(breaks = seq(0, 1500000, by = 250000), 
                     limits = c(1.29, 1500000)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.caption = element_text(hjust = 0)) +
  labs(
    x = NULL,
    y = "Measles cases",
    title = " History of reported Measles cases in World Health Organization Regions ",
    caption = "Source: WHO (2024) The Global Health Observatory(2024) .WHO and UNICEF immunization data portal.N = Six WHO regions with available data between 1980 and 2023.\nA.Iradukunda et al.(April,2025)"
  ) +
  theme(
    legend.position = "bottom",  # Ensures the legend is at the bottom of the plot
    legend.title = element_blank(),  # Optionally remove the legend title
    legend.box.spacing = unit(0.5, "cm"),  # Adds spacing between the plot and the legend
    axis.text.x = element_text(angle = 80, hjust = 1)
  )
historical_cases
ggsave(historical_cases,filename="3.3.Cases_historical reported cases.png",width = 12, height = 8)

# Long format(from 2015)
pacman::p_load(tidyr,dplyr)
data_long <- data %>%
  filter(Period>=2015)%>%
  pivot_longer(cols = -Period, names_to = "Region", values_to = "Cases")
library(ggplot2)
ggplot(data_long, aes(x = Period, y = Cases, color = Region)) +
  geom_line(linewidth = 1) +
  scale_x_continuous(breaks = 2015:2023) + 
  labs(title = "Measles Cases by WHO Region (1980–2023)",
       x = "Year",
       y = "Number of Cases",
       color = "Region") +
  theme_minimal()

# Stacked bar chart
# Period to numeric to ensure proper x-axis spacing
data_long$Period <- as.numeric(data_long$Period)

# Color palette
region_colors <- c(
  "Africa" = "#1f77b4",
  "Americas" = "#aec7e8",
  "Eastern Mediterranean" = "#ffbb78",
  "Europe" = "#ff0000",
  "South-East Asia" = "#2ca02c",
  "Western Pacific" = "#8c1b1b"
)
# Plot: Stacked bar chart
Region_cases<-ggplot(data_long, aes(x = Period, y = Cases, fill = Region)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = 2015:2023) +
  scale_fill_manual(values = region_colors) +
  labs(title = "Measles cases in WHO Region (2015–2023)",
       x = "Year ",
       y = "Measles cases",
       fill = "Region") +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.title.y = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold")
  )
Region_cases
ggsave(Region_cases,filename="3.3.Cases_in_regions.png",width=12,height = 8)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   II.Cases at the country level:--> 3.3.data.cases_allcountries_3.xlsx
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data <- read_excel("3.3.data.cases_allcountries_2.xlsx",sheet = "All cases_all countries")
table(data$Code_region)
print(data)

data_filtered <-data %>%
  filter(Period >= 2015 & Code_region == "EMR")

Countries_cases<-ggplot(data_filtered, aes(x = Period, y = Cases, color = Country)) +
  geom_line() +
  scale_x_continuous(breaks = 2015:2023) + 
  labs(title = "Measles Cases in EMR Region (2015–2023)",
       x = "Year",
       y = "Number of Cases",
       color = "Country") +
  theme_minimal(base_size = 13) +
  theme(    legend.position = "bottom",               # Legend below the plot
            axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(face = "bold"),
            legend.key.size = unit(0.2, "cm"),      # Adjust legend key size
            legend.title = element_text(face = "bold")
  )
Countries_cases

# Stacked bar chart for all countries
# Period to numeric to ensure proper x-axis spacing
data$Period <- as.numeric(data$Period)

# Set WHO color palette (you can adjust as needed)
table(data_filtered$Country)
country_colors <- c(
  "Afghanistan" = "#ffbb78",
  "Bahrain" = "#1f77b4",
  "Djibouti" = "#aec7e8",
  "Egypt" = "#2ca02c",
  "Iran (Islamic Republic of)" = "#ff7f0e",
  "Iraq" = "#9467bd",
  "Jordan" = "#8c564b",
  "Kuwait" = "#e377c2",
  "Lebanon" = "#7f7f7f",
  "Libya" = "#bcbd22",
  "Morocco" = "#17becf",
  "occupied Palestinian territory, including east Jerusalem" = "#000000",
  "Oman" = "#ff9896",
  "Pakistan" = "#98df8a",
  "Qatar" = "#ffeda0",
  "Saudi Arabia" = "#c5b0d5",
  "Somalia" = "#f7b6d2",
  "Sudan" = "#c49c94",
  "Syrian Arab Republic" = "#f4a582",
  "Tunisia" = "#fdd0a2",
  "United Arab Emirates" = "#ff9896",
  "Yemen" = "#d62728"
)

# Plot: Stacked bar chart for cases at country level
table(data_filtered$Country)
Cases_country<-ggplot(data_filtered, aes(x = Period, y = Cases, fill = Country)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = 2015:2023) +
  scale_fill_manual(values = country_colors) +
  labs(title = "Measles cases in EMRO countries",
       x = "Year ",
       y = "Measles cases",
       fill = "Country") +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.title.y = element_text(face = "bold"),
    plot.title = element_text(face = "bold",hjust = 0.5),
    #legend.position = "bottom", 
    legend.box = "horizontal",   # Arranges legend items horizontally
    legend.title = element_text(face = "bold")
  )
Cases_country
ggsave(Cases_country, filename="3.3.Cases_in_emro_countries.png",width = 12,height=8, dpi=300)

#top five
# Filter for 2023 EMRO data and get top 5 countries
top5_2023 <- data_filtered %>%
  filter(Period == 2023) %>%
  arrange(desc(Cases)) %>%
  head(5)
#Bar plot for top 5 countries
ggplot(top5_2023, aes(x = reorder(Country, +Cases), y = Cases, fill = Country)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = country_colors) +
  labs(title = "5 EMRO Countries with Highest Measles Cases (2023)",
       x = "Country",
       y = "Number of Measles Cases",
       caption = "Source: Your Data Source") +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.title.y = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none" # Remove legend since x-axis shows countries
  ) +
  geom_text(aes(label = Cases), vjust = -0.5, size = 4, fontface = "bold") # Add value labels

# Display the top 5 table
top5_table <- top5_2023 %>%
  select(Country, Cases) %>%
  rename(`Measles Cases (2023)` = Cases)

print(top5_table)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#     Measles cases in WHO regions and countries
#     https://www.who.int/data/gho/data/indicators/indicator-details/GHO/measles---number-of-reported-cases
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#II.Vaccine coverage#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#cov<-X3_2_Measles_vaccine_coverage_V12
library(readxl)
cov <- read_excel("3.2.Measles vaccine coverage_V12.xlsx")
library(dplyr)
cov<-data.frame(cov)
names(cov)
table(cov$NAME)
attach(cov)
colnames(cov)<-c("GROUP",                        
                 "CODE",                         
                 "Region" ,                        
                 "YEAR"  ,                       
                 "ANTIGEN",                      
                 "ANTIGEN_DESCRIPTION",          
                 "COVERAGE_CATEGORY"   ,         
                 "COVERAGE_CATEGORY_DESCRIPTION",
                 "Code",                         
                 "TARGET_NUMBER",                
                 "DOSES",               
                 "MCV1",                         
                 "MCV2")

#dev.off()
#.rs.restartR()#

cov<-data.frame(cov)
str(cov) 
summary(cov)
pacman::p_load(gridExtra,ggplot2)
g1<-cov|>
  ggplot(aes(x = YEAR)) +
  geom_point(aes(y=MCV2 , color = Region)) +
  labs(title = "MCV2", 
       x = "Period", y = "Coverage")
g1
g2<-cov|>
  ggplot(aes(x = YEAR)) +
  geom_point(aes(y=MCV1 , color = Region)) +
  labs(title = "MCV1", 
       x = "Period", y = "Coverage")
g2
grid.arrange( g2,g1, nrow = 2)

covdata<-cov
attach(covdata)
names(covdata)
covdata_filtered <- covdata %>% filter(YEAR >= 2015)
ggplot(covdata_filtered,aes(x=YEAR,y=MCV1,colour = ANTIGEN))+
  geom_area(aes(y = MCV1, fill = "MCV1"), alpha = 0.2, position = 'identity') +
  # Area under MCV2
  geom_area(aes(y = MCV2, fill = "MCV2"), alpha = 0.2, position = 'identity') +
  geom_point() +
  geom_line() +
  geom_point(aes(y = MCV2, colour = "MCV2")) +
  geom_line(aes(y = MCV2, colour = "MCV2")) +
  #geom_smooth(aes(y = MCV2, colour = "MCV2"), se = FALSE, method = "loess", linetype = "dashed") +
  facet_wrap(~ Region) +
  labs(title = "Gap in measles vaccine coverage in WHO Regions",
       x = "Period",
       y = "Cases") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
# Vaccine coverage in EMRO Countries
#covdata<-X3_2_Measles_vaccine_coverage_V12
attach(covdata)
names(covdata)
print(covdata)
covdata_filtered1 <- covdata %>% filter((YEAR >= 2015)&(CODE=="EMR"))
dim(covdata_filtered1)
covdata_filtered1
#add cases
data<- read_excel("3.1.Measles cases all_8.xlsx") #
names(data)
colnames(data)<-c("YEAR",
                  "Cases_WP",
                  "Cases_SEA",
                  "Cases_Europe",
                  "Cases_EMRO",
                  "Cases_Americas","Cases_Africa")
case_emro<-data|>
  filter(Period>=2015)
case_emro
dim(case_emro)
cov_case<-merge(covdata_filtered1,case_emro, by=c("YEAR"))
cov_case
names(cov_case)
ggplot(cov_case, aes(x = YEAR)) +
  # Bars for reported cases (right axis)
  geom_col(aes(y = Cases_EMRO / 100, fill = "Reported Cases"), 
           alpha = 0.6, width = 1.5) +
  # Lines for vaccine coverage (left axis)
  
  geom_line(data = cov_case,#data_long, 
            aes(y = Coverage, color = Vaccine_Type, group = Vaccine_Type), 
            linewidth = 1.2) +
  geom_point(data =cov_case ,# data_long
             aes(y = Coverage, color = Vaccine_Type), 
             size = 3) +
  # Adjust scales for dual axes
  scale_y_continuous(
    name = "Vaccine Coverage (%)",
    sec.axis = sec_axis(~ . * 100, name = "Reported Measles Cases")
  ) +
  # Customize colors and labels
  scale_fill_manual(values = c("Reported Cases" = "red")) +
  scale_color_manual(values = c("MCV1_Coverage" = "blue", "MCV2_Coverage" = "green")) +
  labs(
    title = "Measles Vaccine Coverage vs. Reported Cases in EMRO (2016-2022)",
    x = "Year",
    color = "Vaccine Type",
    fill = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )








library(dplyr)
library(ggplot2)
ggplot(covdata_filtered1, aes(x = YEAR)) +
  geom_point(aes(y = MCV1, colour = "MCV1")) +
  geom_line(aes(y = MCV1, colour = "MCV1")) +
  geom_point(aes(y = MCV2, colour = "MCV2")) +
  geom_line(aes(y = MCV2, colour = "MCV2")) +
  facet_wrap(~Region) +
  # Axis and title
  labs(title = "Gaps in Measles Vaccine Coverage in EMRO",
       #subtitle = "Eastern Mediterranean Region",
       x = "Period",
       y = "Coverage (%)",
       colour = "ANTIGEN") +
  scale_y_continuous(limits = c(70, 90), 
                     breaks = seq(70, 90, 5),
                     labels = function(x) paste0(x, "%")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
#With WHO recommendations
vaccine_coverage_in_emro_region<- ggplot(covdata_filtered1, aes(x = YEAR)) +
  geom_point(aes(y = MCV1, colour = "MCV1"),size=3) +
  geom_line(aes(y = MCV1, colour = "MCV1"),size = 1) +
  geom_point(aes(y = MCV2, colour = "MCV2"),size=3) +
  geom_line(aes(y = MCV2, colour = "MCV2"),size = 1) +
  # WHO recommended horizontal lines
  geom_hline(aes(yintercept = 95, colour = "MCV1"), linetype = "dashed") +
  geom_hline(aes(yintercept = 90, colour = "MCV2"), linetype = "dashed") +
  # Faceting
  facet_wrap(~Region) +
  # Axis and title
  labs(title = "Gaps in Measles Vaccine Coverage in EMRO",
       #subtitle = "Eastern Mediterranean Region",
       x = "Period",
       y = "Coverage (%)",
       colour = "ANTIGEN") +
  scale_y_continuous(limits = c(70, 100), 
                     breaks = seq(70, 100, 5),
                     labels = function(x) paste0(x, "%")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
vaccine_coverage_in_emro_region
vaccine_coverage_in_emro_region1<-grid.arrange(vaccine_coverage_in_emro_region,Cases_country,ncol=2)
ggsave(vaccine_coverage_in_emro_region1, filename="3.3.Vaccine_coverage_in_emro_region.png",width=12,height=8,dpi = 300)

#################################################################################
cases_region<- read_excel("3.1.Measles cases all_8.xlsx") #
cov_region <- read_excel("3.2.Measles vaccine coverage_V12.xlsx")
cov_region1<- cov_region|>
  filter(YEAR>=2015)|>
  filter(CODE=="EMR")
cases_region1<-cases_region|>
  filter(Period>=2015)
cases_region1
both_data<-cbind(cases_region1$`Eastern Mediterranean`,cov_region1$MCV1,cov_region1$MCV2)
both_data<-data.frame(both_data)
colnames(both_data)<-c("Cases", "MCV1","MCV2")
g3<-ggplot(both_data, aes(x = MCV1, y=Cases )) +
  geom_point(alpha = 0.6, color = "darkblue") +
  geom_smooth(method = "loess", se = TRUE, color = "red", span = 0.75) +
  labs(
    title = "Cases and MCV1 coverage in EMRO",
    x = "MCV1 coverage (%)",
    y = "Measles cases"
  ) +
  theme_minimal()
g4<-ggplot(both_data, aes(x = MCV2, y=Cases )) +
  geom_point(alpha = 0.6, color = "darkblue") +
  geom_smooth(method = "loess", se = TRUE, color = "red", span = 0.75) +
  labs(
    title = "Cases and MCV2 coverage in EMRO",
    x = "MCV2 coverage (%)",
    y = "Measles cases"
  ) +
  theme_minimal()

grid.arrange(g3,g4,ncol=2)
plot.grid(g3,g4)

pacman::p_load("ggExtra")
# Let me add marginal histograms
g3 <- ggplot(both_data, aes(x =MCV1, y = MCV2, color =Cases )) +
  geom_point(alpha = 0.7, size = 2) +
  scale_color_viridis_c(option = "plasma") +  # colorful gradient
  labs(
    title = "Scatter Plot with Marginal Histograms",
    x = "MCV1 coverage (%)",
    y = "MCV2 coverage (%)",
    color = "Cases"
  ) +
  theme_minimal()
g3
ggMarginal(g3, type = "histogram", margins = "both", fill = "gray", color = "black")

m1 <- ggMarginal(g3, type = "histogram", margins = "both")
m2 <- ggMarginal(g3, type = "histogram", margins = "both")
# cowplot::plot_grid to combine them
#dev.off()
cowplot::plot_grid(immunity1, immunity2, labels = c("A", "B"), ncol = 2, align = "hv")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

############################################################### 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Vaccine coverage at the country level
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
vaccine_countries<- read_excel("3.3.Measles vaccination coverage V12 countries.xlsx", sheet = "Vaccine coverage 2")
table(vaccine_countries$Country)
emro_country_names <- c(
  "Afghanistan",
  "Bahrain",
  "Djibouti",
  "Egypt",
  "Iran",       #Iran (Islamic Republic of)
  "Iraq",
  "Jordan",
  "Kuwait",
  "Lebanon",
  "Libya",
  "Morocco",
  "Oman",
  "Pakistan",
  "Palestine*",  #occupied Palestinian territory, including east Jerusalem
  "Qatar",
  "Saudi Arabia",
  "Somalia",
  "Sudan",
  "Syrian Arab Republic",
  "Tunisia",
  "United Arab Emirates",
  "Yemen"
)

emro_countries_vax_coverage <- vaccine_countries %>%
  filter(Country %in% emro_country_names)
MCV1_2023<-emro_countries_vax_coverage|>
  filter(Period == 2023, ANTIGEN == "MCV1") 
MCV1_2023
print(MCV1_2023)
MCV2_2023<-emro_countries_vax_coverage|>
  filter(Period == 2023, ANTIGEN == "MCV2")
MCV2_2023


#1.Lowest MCV1 coverage
top5_EMROcountries_low_MCV1 <- emro_countries_vax_coverage %>%
  filter(Period == 2023, ANTIGEN == "MCV1") %>%
  arrange(COVERAGE) %>%
  head(5)
# Select top 5 EMRO countries with lowest MCV1 coverage in 2023
top5_EMROcountries_low_MCV1 <- emro_countries_vax_coverage %>%
  filter(Period == 2023, ANTIGEN == "MCV1") %>%
  arrange(COVERAGE) %>%
  head(5)

#Country colors
country_colors1 <- c("#FF0000", "#E69F00", "#CC79A7","#009E73", "#56B4E9")
names(country_colors1) <- top5_EMROcountries_low_MCV1$Country

# Create bar plot
lowest_MCV1<-ggplot(top5_EMROcountries_low_MCV1, aes(x = reorder(Country, -COVERAGE), y = COVERAGE, fill = Country)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = country_colors1) +
  labs(title = "EMRO Countries with Lowest MCV1 Coverage (2023)",
       x = "Country",
       y = "Coverage (%)",
       caption = "Source: WHO Vaccine Coverage Data") +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.title.y = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none"
  ) +
  geom_text(aes(label = COVERAGE), vjust = -0.5, size = 4, fontface = "bold")
lowest_MCV1
# Display top 5 table
top5_table_MCV1 <- top5_EMROcountries_low_MCV1 %>%
  select(Country, COVERAGE) %>%
  rename(`MCV1 Coverage (%) in 2023` = COVERAGE)
print(top5_table_MCV1)

#2.Lowest MCV2 coverage
top5_EMROcountries_low_MCV2 <- emro_countries_vax_coverage %>%
  filter(Period == 2023, ANTIGEN == "MCV2") %>%
  arrange(COVERAGE) %>%
  head(5)


#Country colors
country_colors2 <- c("#FF0000", "#E69F00", "#CC79A7","#009E73", "#56B4E9")
names(country_colors2) <- top5_EMROcountries_low_MCV2$Country

# Create bar plot
lowest_MCV2<-ggplot(top5_EMROcountries_low_MCV2, aes(x = reorder(Country, -COVERAGE), y = COVERAGE, fill = Country)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = country_colors2) +
  labs(title = "EMRO Countries with Lowest MCV2 Coverage (2023)",
       x = "Country",
       y = "Coverage (%)",
       caption = "Source: WHO Vaccine Coverage Data") +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.title.y = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none"
  ) +
  geom_text(aes(label = COVERAGE), vjust = -0.5, size = 4, fontface = "bold")
lowest_MCV2
# Display top 5 table
top5_table_MCV2<- top5_EMROcountries_low_MCV2 %>%
  select(Country, COVERAGE) %>%
  rename(`MCV2 Coverage (%) in 2023` = COVERAGE)
print(top5_table_MCV2)
grid.arrange(lowest_MCV1,lowest_MCV2,ncol=2)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#              Spatial analysis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Installation and required packages
pacman::p_load(sf,
               rnaturalearth,
               rnaturalearthdata,
               ggplot2,dplyr
)

#
library(readr)
data <- read_csv("3.3.Coverage_all_countries.csv")
data<-data[,-1]
data<-data[,-5]
names(data)
table(data$YEAR)
table(data$COVERAGE_CATEGORY)
str(data)
data<-as.data.frame(data)
str(data$YEAR)
data<-data|>
  filter(COVERAGE_CATEGORY=="WUENIC")|>
  filter(YEAR>2020)|>
  filter(YEAR<2024)
  
           
data<-data[-c(5,6)]
data<-data[-c(5,6,8)]
emro_iso3 <- c("AFG", "BHR", "DJI", "EGY", "IRN", "IRQ", "JOR", "KWT", "LBN", "LBY",
               "MAR", "OMN", "PAK", "QAT", "SAU", "SOM", "SDN", "SYR", "TUN", "ARE",
               "YEM", "PSE")
data<-data|>
  filter(CODE %in% emro_iso3)

library(sf)
library(ggplot2)
library(rnaturalearth)
library(dplyr)
table(data$YEAR)
# Your 5 EMRO countries ISO3 codes (replace as needed)
emro_codes <- c("AFG", "BHR", "DJI", "EGY", "IRN")  

# Load world country polygons
world <- ne_countries(scale = "medium", returnclass = "sf")

# Join your data to the spatial polygons by ISO3 code
world_coverage <- world %>%
  left_join(data, by = c("iso_a3" = "CODE")) %>%
  mutate(COVERAGE = ifelse(iso_a3 %in% emro_codes, COVERAGE, NA))  # NA outside EMRO

# Plot: only EMRO countries colored, others white
ggplot(world_coverage) +
  geom_sf(aes(fill = COVERAGE), color = "black", size = 0.1) +
  scale_fill_viridis_c(option = "plasma", na.value = "white", limits = c(0, 100)) +
  facet_grid(ANTIGEN ~ YEAR) +
  labs(title = "Vaccine Coverage for 5 EMRO Countries by Year and Antigen",
       fill = "Coverage (%)") +
  theme_minimal()

ggplot(world_coverage) +
  geom_sf(aes(fill = COVERAGE), color = "black", size = 0.1) +
  scale_fill_viridis_c(option = "plasma", na.value = "white", limits = c(0, 100)) +
  facet_grid(ANTIGEN ~ YEAR) +
  labs(title = "Vaccine Coverage for EMRO Countries by Year and Antigen",
       fill = "Coverage (%)") +
  theme_minimal() +
  coord_sf(xlim = c(30, 70), ylim = c(10, 45))  # <-- zoom: adjust as needed
#
ggplot(world_coverage) +
  geom_sf(aes(fill = COVERAGE), color = "black", size = 0.1) +
  scale_fill_viridis_c(option = "plasma", na.value = "white", limits = c(0, 100)) +
  facet_grid(ANTIGEN ~ YEAR) +
  labs(title = "Vaccine Coverage for EMRO Countries by Year and Antigen",
       fill = "Coverage (%)") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  coord_sf(xlim = c(30, 70), ylim = c(10, 45))

#
library(sf)
library(ggplot2)
library(rnaturalearth)
library(dplyr)

# Your EMRO country codes
emro_codes <- c("AFG", "BHR", "DJI", "EGY", "IRN")

# Load world polygons
world <- ne_countries(scale = "medium", returnclass = "sf")

# Filter EMRO countries
emro <- world %>% filter(iso_a3 %in% emro_codes)

# Find neighbors: countries that touch EMRO countries
neighbors <- st_filter(world, emro, .predicate = st_touches)

# Create a new column "group" to classify polygons
world <- world %>%
  mutate(group = case_when(
    iso_a3 %in% emro_codes ~ "EMRO",
    iso_a3 %in% neighbors$iso_a3 ~ "Neighbor",
    TRUE ~ "Other"
  ))

# Join your data to world for coverage info
world <- world %>%
  left_join(data, by = c("iso_a3" = "CODE"))

# Set coverage only for EMRO countries, NA for others (neighbors & others)
world <- world %>%
  mutate(COVERAGE = ifelse(group == "EMRO", COVERAGE, NA))

# Ocean bounding box
xlim <- c(30, 70)
ylim <- c(10, 45)
ocean <- st_as_sfc(st_bbox(c(xmin = xlim[1], xmax = xlim[2], ymin = ylim[1], ymax = ylim[2]), crs = st_crs(world)))

# Plot
ggplot() +
  geom_sf(data = ocean, fill = "#a6cee3", color = NA) +               # Ocean
  geom_sf(data = world %>% filter(group == "Neighbor"), fill = "white", color = "black", size = 0.1) +  # Neighbors in white with borders
  geom_sf(data = world %>% filter(group == "EMRO"), aes(fill = COVERAGE), color = "black", size = 0.3) + # EMRO colored
  scale_fill_viridis_c(option = "plasma", na.value = "white", limits = c(0, 100)) +
  facet_grid(ANTIGEN ~ YEAR) +
  labs(title = "Vaccine Coverage for EMRO Countries with Neighbors in White",
       fill = "Coverage (%)") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  coord_sf(xlim = xlim, ylim = ylim)

# World map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# EMRO country ISO codes based on WHO
emro_iso3 <- c("AFG", "BHR", "DJI", "EGY", "IRN", "IRQ", "JOR", "KWT", "LBN", "LBY",
               "MAR", "OMN", "PAK", "QAT", "SAU", "SOM", "SDN", "SYR", "TUN", "ARE",
               "YEM", "PSE")

# EMRO countries only
emro_map <- world %>% filter(iso_a3 %in% emro_iso3)
Cases_for_map <- read_excel("3.3.data.cases_allcountries_2.xlsx",sheet = "All cases_all countries")
Cases_for_map1<-Cases_for_map|>
  filter(Period==2023)%>%
  filter(Code_region=="EMR")%>%
  select(Country,Code_region,Cases)
table(data$Code_region)
print(data)
#EMRO Countries codes 
emro_country_codes <- tribble(
  ~Country,                        ~ISO3_Code,
  "Afghanistan",                  "AFG",
  "Bahrain",                      "BHR",
  "Djibouti",                     "DJI",
  "Egypt",                        "EGY",
  "Iran (Islamic Republic of)",   "IRN",
  "Iraq",                         "IRQ",
  "Jordan",                       "JOR",
  "Kuwait",                       "KWT",
  "Lebanon",                      "LBN",
  "Libya",                        "LBY",
  "Morocco",                      "MAR",
  "Oman",                         "OMN",
  "Pakistan",                     "PAK",
  "Qatar",                        "QAT",
  "Saudi Arabia",                "SAU",
  "Somalia",                      "SOM",
  "Sudan",                        "SDN",
  "Syrian Arab Republic",         "SYR",
  "Tunisia",                      "TUN",
  "United Arab Emirates",         "ARE",
  "Yemen",                        "YEM",
  "occupied Palestinian territory, including east Jerusalem",                    "PSE"
)

# View the data
print(emro_country_codes)
Cases_for_map2<-Cases_for_map1%>%
  left_join(emro_country_codes,by="Country")
#Merge cases data with world map data

#EMRO countries map with cases data
# Load required packages
pacman::p_load(sf, rnaturalearth, rnaturalearthdata, ggplot2, dplyr)
# Load world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# EMRO ISO3 country codes
emro_iso3 <- c("AFG", "BHR", "DJI", "EGY", "IRN", "IRQ", "JOR", "KWT", "LBN", "LBY",
               "MAR", "OMN", "PAK", "QAT", "SAU", "SOM", "SDN", "SYR", "TUN", "ARE",
               "YEM", "PSE")

# Only EMRO countries from the world map
emro_map <- world %>% filter(iso_a3 %in% emro_iso3)

# Your cases data (replace this if needed)
Cases_for_map2 <- tibble::tibble(
  Country = c("Afghanistan", "United Arab Emirates", "Bahrain", "Djibouti", "Egypt",
              "Iran (Islamic Republic of)", "Iraq", "Kuwait", "Lebanon", "Libya",
              "Oman", "Pakistan", "occupied Palestinian territory, including east Jerusalem",
              "Qatar", "Saudi Arabia", "Sudan", "Somalia", "Syrian Arab Republic",
              "Tunisia", "Yemen"),
  Code_region = rep("EMR", 20),
  Cases = c(2792, 484, 3, 446, 267, 648, 12331, 6, 346, 257,
            25, 17722, 3, 127, 2254, 1377, 3167, 744, 10, 49755),
  ISO3_Code = c("AFG", "ARE", "BHR", "DJI", "EGY", "IRN", "IRQ", "KWT", "LBN", "LBY",
                "OMN", "PAK", "PSE", "QAT", "SAU", "SDN", "SOM", "SYR", "TUN", "YEM")
)

# Merge map with measles cases data using ISO3 country code
emro_map_colored <- emro_map %>%
  left_join(Cases_for_map2, by = c("iso_a3" = "ISO3_Code"))

# Plot the EMRO map with color fill by measles cases
ggplot(emro_map_colored) +
  geom_sf(aes(fill = Cases), color = "black") +
  scale_fill_gradient(low = "#FFF5F0", high = "#B10026", na.value = "gray90",
                      name = "Measles Cases") +
  geom_sf_text(aes(label = iso_a3), size = 3, color = "black") +
  labs(title = "Measles Cases in EMRO Countries (2023)",
       caption = "Source: WHO / Natural Earth") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

#MCV1
# Coverage data for ALL EMRO countries
coverage<-covdata

coverage <- covdata %>%
  pivot_longer(cols = c(MCV1, MCV2),
               names_to = "MCV",
               values_to = "coverage")
coverage

coverage_data_MCV1 <- tibble::tribble(
  ~CODE, ~Country, ~Period, ~ANTIGEN, ~COVERAGE,
  "AFG", "Afghanistan", 2023, "MCV1", 55,
  "BHR", "Bahrain", 2023, "MCV1", 99,
  "DJI", "Djibouti", 2023, "MCV1", 76,
  "EGY", "Egypt", 2023, "MCV1", 96, 
  "IRN", "Iran ", 2023, "MCV1", 99, #Iran=(Islamic Republic of)
  "IRQ", "Iraq", 2023, "MCV1", 97,
  "JOR", "Jordan", 2023, "MCV1", 95,
  "KWT", "Kuwait", 2023, "MCV1", 99,
  "LBN", "Lebanon", 2023, "MCV1", 73,
  "LBY", "Libya", 2023, "MCV1", 73,
  "MAR", "Morocco", 2023, "MCV1", 99,
  "PSE", "Palestine", 2023, "MCV1", 89,#Palestine:occupied Palestinian territory, including east Jerusalem
  "OMN", "Oman", 2023, "MCV1", 99,
  "PAK", "Pakistan", 2023, "MCV1", 84,
  "QAT", "Qatar", 2023, "MCV1", 99,
  "SAU", "Saudi Arabia", 2023, "MCV1", 97,
  "SOM", "Somalia", 2023, "MCV1", 46,
  "SDN", "Sudan", 2023, "MCV1", 51,
  "SYR", "Syrian Arab Republic", 2023, "MCV1", 74,
  "TUN", "Tunisia", 2023, "MCV1", 96,
  "ARE", "United Arab Emirates", 2023, "MCV1", 98,
  "YEM", "Yemen", 2023, "MCV1", 45
)

# Map CODEs to match first map style
coverage_data1 <- coverage_data_MCV1 %>%
  mutate(CODE_MAP = case_when(
    CODE == "BHR" ~ "BMF",
    CODE == "DJI" ~ "DIN",
    CODE == "JOR" ~ "PSSR",
    CODE == "KWT" ~ "KWIT",
    CODE == "ARE" ~ "SAN",
    TRUE ~ CODE
  ))

# Get world map data
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  mutate(CODE_MAP = case_when(
    iso_a3 == "BHR" ~ "BMF",
    iso_a3 == "DJI" ~ "DIN",
    iso_a3 == "JOR" ~ "PSSR",
    iso_a3 == "KWT" ~ "KWIT",
    iso_a3 == "ARE" ~ "SAN",
    TRUE ~ iso_a3
  ))

# Get ocean data
ocean <- ne_download(scale = 50, type = "ocean", category = "physical", returnclass = "sf")

# Separate EMRO and non-EMRO countries
emro_countries <- world %>% filter(CODE_MAP %in% coverage_data1$CODE_MAP)
other_countries <- world %>% filter(!CODE_MAP %in% coverage_data1$CODE_MAP)

# Merge coverage data with EMRO countries
emro_map <- emro_countries %>%
  left_join(coverage_data1, by = "CODE_MAP")

# Creation of the map with water and neighbors
map_MCV1<-ggplot() +
  # Ocean layer
  geom_sf(data = ocean, fill = "#b3cde3", color = NA) +
  
  # Non-EMRO countries (neighbors)
  geom_sf(data = other_countries, fill = "#f0f0f0", color = "#f0f0f0", size = 0.7) +
  
  # EMRO countries with data
  geom_sf(data = emro_map, aes(fill = COVERAGE), color = "white", size = 0.2) +
  
  # Labels for EMRO countries
  geom_sf_text(data = emro_map, aes(label = paste0(CODE_MAP, "\n", COVERAGE, "%")), 
               size = 3, color = "black", fontface = "bold") +
  scale_fill_viridis(
    option = "plasma",
    na.value = "gray90",
    limits = c(0, 100),
    name = "MCV1 Coverage (%)"
  ) +
  labs(
    title = "Measles Vaccination Coverage (MCV1) in EMRO Countries (2023)",
    subtitle = "Eastern Mediterranean Region",
    caption = "Source: WHO/UNICEF data.\nArnaud: April 2025"
  ) +
  coord_sf(xlim = c(-20, 80), ylim = c(-5, 45), expand = FALSE) +  # Zoom to EMRO region
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(hjust = 0.5, size = 10),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )
map_MCV1



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                               MCV2 Map                #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load required libraries
pacman::p_load(tidyverse,
               sf,
               rnaturalearthdata,
               rnaturalearth,
               viridis,
               ggrepel,
               countrycode
)

# Define the coverage data for MCV2
coverage_data_MCV2 <- tibble::tribble(
  ~CODE, ~Country, ~Period, ~ANTIGEN, ~COVERAGE,
  "AFG", "Afghanistan", 2023, "MCV2", 42,
  "BHR", "Bahrain", 2023, "MCV2", 99,
  "DJI", "Djibouti", 2023, "MCV2", 64,
  "EGY", "Egypt", 2023, "MCV2", 95,
  "IRN", "Irab", 2023, "MCV2", 99, #Iran (Islamic Republic of)
  "IRQ", "Iraq", 2023, "MCV2", 83,
  "JOR", "Jordan", 2023, "MCV2", 89,
  "KWT", "Kuwait", 2023, "MCV2", 91,
  "LBN", "Lebanon", 2023, "MCV2", 53,
  "LBY", "Libya", 2023, "MCV2", 72,
  "MAR", "Morocco", 2023, "MCV2", 99,
  "PSE", "Palestine", 2023, "MCV2", 89,#occupied Palestinian territory, including east Jerusalem
  "OMN", "Oman", 2023, "MCV2", 99,
  "PAK", "Pakistan", 2023, "MCV2", 80,
  "QAT", "Qatar", 2023, "MCV2", 99,
  "SAU", "Saudi Arabia", 2023, "MCV2", 96,
  "SOM", "Somalia", 2023, "MCV2", 13,
  "SDN", "Sudan", 2023, "MCV2", 38,
  "SYR", "Syrian Arab Republic", 2023, "MCV2", 64,
  "TUN", "Tunisia", 2023, "MCV2", 97,
  "ARE", "United Arab Emirates", 2023, "MCV2", 94,
  "YEM", "Yemen", 2023, "MCV2", 45
)

#
# Map CODEs to match first map style
coverage_data2 <- coverage_data_MCV2 %>%
  mutate(CODE_MAP = case_when(
    CODE == "BHR" ~ "BMF",
    CODE == "DJI" ~ "DIN",
    CODE == "JOR" ~ "PSSR",
    CODE == "KWT" ~ "KWIT",
    CODE == "ARE" ~ "SAN",
    TRUE ~ CODE
  ))

# Get world map data
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  mutate(CODE_MAP = case_when(
    iso_a3 == "BHR" ~ "BMF",
    iso_a3 == "DJI" ~ "DIN",
    iso_a3 == "JOR" ~ "PSSR",
    iso_a3 == "KWT" ~ "KWIT",
    iso_a3 == "ARE" ~ "SAN",
    TRUE ~ iso_a3
  ))

# Get ocean data
ocean <- ne_download(scale = 50, type = "ocean", category = "physical", returnclass = "sf")

# Separate EMRO and non-EMRO countries
emro_countries <- world %>% filter(CODE_MAP %in% coverage_data2$CODE_MAP)
other_countries <- world %>% filter(!CODE_MAP %in% coverage_data2$CODE_MAP)

# Merge coverage data with EMRO countries
emro_map <- emro_countries %>%
  left_join(coverage_data2, by = "CODE_MAP")

# Creation of the map with water and neighbors
map_MCV2<-ggplot() +
  # Ocean layer
  geom_sf(data = ocean, fill = "#b3cde3", color = NA) +
  
  # Non-EMRO countries (neighbors)
  geom_sf(data = other_countries, fill = "#f0f0f0", color = "#f0f0f0", size = 0.7) +
  
  # EMRO countries with data
  geom_sf(data = emro_map, aes(fill = COVERAGE), color = "white", size = 0.2) +
  
  # Labels for EMRO countries
  geom_sf_text(data = emro_map, aes(label = paste0(CODE_MAP, "\n", COVERAGE, "%")), 
               size = 3, color = "black", fontface = "bold") +
  
  scale_fill_viridis(
    option = "plasma",
    na.value = "gray90",
    limits = c(0, 100),
    name = "MCV1 Coverage (%)"
  ) +
  labs(
    title = "MCV2 in EMRO Countries (2023)",
    subtitle = "Eastern Mediterranean Region",
    caption = "Source: WHO/UNICEF data.\nArnaud: April 2025"
  ) +
  coord_sf(xlim = c(-20, 80), ylim = c(-5, 45), expand = FALSE) +  # Zoom to EMRO region
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(hjust = 0.5, size = 10),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )
map_MCV2

# Save the plot
ggsave("3.3.emro_mcv2_coverage_2023.png", 
       plot = map_MCV2,
       width = 10, height = 8, dpi = 300, bg = "white")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Vaccine started in 2000
#Cases started in 1974
vaccine_countries<- read_excel("3.3.Measles vaccination coverage V12 countries.xlsx", sheet = "Vaccine coverage 2")
vaccine_countries
cases<- read_excel("3.3.data.cases_allcountries_2.xlsx",sheet = "All cases_all countries")
cases
table(cases$Code_region)
cases_countries<-cases|>
  filter(Period>=2000)
cases_countries

dim(cases_countries)
dim(vaccine_countries)
#Short format
vaccine_countries_MCV1<-vaccine_countries|>
  filter(ANTIGEN=="MCV1")
vaccine_countries_MCV1
vaccine_countries_MCV2<-vaccine_countries|>
  filter(ANTIGEN=="MCV2")
vaccine_countries_MCV2
dim(vaccine_countries_MCV2)
#dim(vaccine_countries_MCV2)
#dim(cases_countries)
#vaccine_countries_MCV12<-cbind(vaccine_countries_MCV1,vaccine_countries_MCV2$ANTIGEN,vaccine_countries_MCV2$COVERAGE)
#vaccine_countries_MCV12<-merge(vaccine_countries_MCV1,vaccine_countries_MCV2,by=c("Country","Period"))
#vaccine_countries_MCV12_cases<-merge(vaccine_countries_MCV12,cases_countries,by=c("Country","Period"))
#vaccine_countries_MCV12_cases


names(cases_countries)
names(vaccine_countries)
combined_data<-merge(vaccine_countries,cases_countries,by=c("Country","Period"))
combined_data1<-combined_data|>
  mutate(Covid19=ifelse(Period>=2000,"Yes","No"))|>
  filter(Code_region=="EMR")
combined_data1
dim(combined_data1)
names(combined_data1)

names(combined_data1)

ggplot(combined_data1, aes(x = Period, y = COVERAGE, color = ANTIGEN)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 95, linetype = "dashed", color = "gray50") +  # First horizontal line
  geom_hline(yintercept = 80, linetype = "dashed", color = "gray50") +
  facet_wrap(~ Country) +
  labs(title = "Gap in measles vaccine coverage in WHO Regions",
       x = "Period",
       y = "Coverage (%)",
       color = "Vaccine") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
#Rescale CASES to match COVERAGE range
pacman::p_load(scales)
library(ggplot2)
library(scales)   #For rescaling functions
max_cases <- max(combined_data1$Cases, na.rm = TRUE)
combined_data1$CASES_rescaled <- rescale(combined_data1$Cases, to = c(0, 100))

ggplot(combined_data1) +
  # Primary axis (COVERAGE)
  geom_point(aes(x = Period, y = COVERAGE, color = ANTIGEN)) +
  geom_line(aes(x = Period, y = COVERAGE, color = ANTIGEN)) +
  
  # Secondary axis (CASES, rescaled)
  geom_line(aes(x = Period, y = CASES_rescaled, group = ANTIGEN), 
            color = "purple", linetype = "solid", size = 1) +
  
  # Horizontal reference lines
  geom_hline(yintercept = 95, linetype = "dashed", color = "gray50") +
  geom_hline(yintercept = 80, linetype = "dashed", color = "gray50") +
  
  # Facets and labels
  facet_wrap(~ Country) +
  labs(
    title = "Measles vaccine coverage and cases in WHO Regions",
    x = "Period",
    y = "Coverage (%)",
    color = "Vaccine"
  ) +
  
  # Secondary axis (reverse the rescaling for labels)
  scale_y_continuous(
    sec.axis = sec_axis(
      ~ (. * max_cases / 100),  # Convert back to original CASES scale
      name = "Cases "
    )
  ) +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


library(ggplot2)
library(dplyr)

# 1. Rescale cases and add label for max cases
combined_data1 <- combined_data1 %>%
  group_by(Country) %>%
  mutate(
    max_cases = max(Cases, na.rm = TRUE),
    CASES_rescaled = (Cases/ max_cases) * 100,
    max_label = paste0("Max cases: ", max_cases)
  ) %>%
  ungroup()

# 2. Plot
ggplot(combined_data1) +
  # Coverage points and lines
  geom_point(aes(x = Period, y = COVERAGE, color = ANTIGEN)) +
  geom_line(aes(x = Period, y = COVERAGE, color = ANTIGEN)) +
  
  # Rescaled case bars
  geom_col(aes(x = Period, y = CASES_rescaled),
           fill = "purple", alpha = 0.3) +
  
  # Reference lines for targets
  geom_hline(yintercept = 95, linetype = "dashed", color = "gray50") +
  geom_hline(yintercept = 80, linetype = "dashed", color = "gray50") +
  
  # Max case label per country
  geom_text(data = combined_data1 %>% distinct(Country, max_label),
            aes(x = -Inf, y = 100, label = max_label),
            hjust = -0.1, vjust = 1.3, inherit.aes = FALSE, size = 3) +
  
  # Facet per country
  facet_wrap(~ Country) +
  
  # Titles and labels
  labs(
    title = "Measles vaccine coverage and cases by country",
    x = "Period",
    y = "Coverage (%)",
    color = "Vaccine"
  ) +
  
  # Adjust y-axis and add secondary y-axis for cases
  scale_y_continuous(
    limits = c(0, 100),
    sec.axis = sec_axis(~ ., name = "Cases (% of max)")
  ) +
  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "bottom"
  )












