#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# This code visualize the WHO EMRO region  and  the structure of some of its demographic characteristics    #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Packages
pacman::p_load( sf,rnaturalearth,ggplot2,dplyr)
# Countries
coverage_data_MCV1 <- data.frame(
  CODE = c("AFG", "BHR", "DJI", "EGY", "IRN", "IRQ", "JOR", "KWT", "LBN", "LBY", 
           "MAR", "PSE", "OMN", "PAK", "QAT", "SAU", "SOM", "SDN", "SYR", "TUN", "ARE", "YEM"),
  Country = c("Afghanistan", "Bahrain", "Djibouti", "Egypt", "Iran", "Iraq", "Jordan", 
              "Kuwait", "Lebanon", "Libya", "Morocco", "Palestine", "Oman", "Pakistan", 
              "Qatar", "Saudi Arabia", "Somalia", "Sudan", "Syrian Arab Republic", 
              "Tunisia", "United Arab Emirates", "Yemen")
  
)

# Map codes
coverage_data1 <- coverage_data_MCV1 %>%
  mutate(CODE_MAP = case_when(
    CODE == "BHR" ~ "BMF", CODE == "DJI" ~ "DIN", CODE == "JOR" ~ "PSSR",
    CODE == "KWT" ~ "KWIT", CODE == "ARE" ~ "SAN", TRUE ~ CODE
  ))

# world map
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  mutate(CODE_MAP = case_when(
    iso_a3 == "BHR" ~ "BMF", iso_a3 == "DJI" ~ "DIN", iso_a3 == "JOR" ~ "PSSR",
    iso_a3 == "KWT" ~ "KWIT", iso_a3 == "ARE" ~ "SAN", TRUE ~ iso_a3
  ))

# Ocean data
ocean <- ne_download(scale = 50, type = "ocean", category = "physical", returnclass = "sf")

# Split countries
emro_countries <- world %>% filter(CODE_MAP %in% coverage_data1$CODE_MAP)
other_countries <- world %>% filter(!CODE_MAP %in% coverage_data1$CODE_MAP)

# Merge data
emro_map <- emro_countries %>% left_join(coverage_data1, by = "CODE_MAP")

# Here , i will  create map with YELLOW color for all EMRO countries
#A.Short codes for countries names. He i used ISO
map_1 <- ggplot() +
  geom_sf(data = ocean, fill = "#b3cde3", color = NA) +
  geom_sf(data = other_countries, fill = "#f0f0f0", color = "#f0f0f0", size = 0.7) +
  geom_sf(data = emro_map, fill = "#FFD700", color = "white", size = 0.2) +
  geom_sf_text(data = emro_map, aes(label = CODE_MAP), 
               size = 3, color = "black", fontface = "bold") +
  labs(title = "World Health Organization Eastern Mediterranean Region (WHO EMRO)",
       subtitle = "WHO Regional Office")+#,
       #caption = "") +#Source: WHO/UNICEF data.\nAI: April 2025
  coord_sf(xlim = c(-20, 80), ylim = c(-5, 45), expand = FALSE) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption = element_text(hjust = 0.5, size = 10))

#Herei will display and save the map
print(map_1)
ggsave("study_area_map_EMRO_1.png", plot = map_1, width = 12, height = 8, dpi = 300)


#B. Full name for countries
map_2 <- ggplot() +
  geom_sf(data = ocean, fill = "#b3cde3", color = NA) +
  geom_sf(data = other_countries, fill = "#f0f0f0", color = "#f0f0f0", size = 0.7) +
  geom_sf(data = emro_map, fill = "#FFD700", color = "white", size = 0.2) +
  geom_sf_text(
    data = emro_map,
    aes(label = Country),  # <--- changed here
    size = 3,
    color = "black",
    fontface = "bold"
  ) +
  labs(
    title = "World Health Organization Eastern Mediterranean Region",
    subtitle = "",
    caption = ""
  ) +
  coord_sf(xlim = c(-20, 80), ylim = c(-5, 45), expand = FALSE) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.1, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.001, size = 12),
    plot.caption = element_text(hjust = 0.001, size = 10)
  )

print(map_2)
ggsave("study_area_map_EMRO_2.png", plot = map_2, width = 12, height = 10, dpi = 300)

# 1. Demographic
getwd()
setwd("D:/Placement project disk")
getwd()
pacman::p_load(
  tidyverse,  # For data manipulation and visualization
  ggplot2,    # For plotting
  dplyr,      # For data manipulations
  patchwork,  # For combining plots
  plotly,     # For inter-actives plots
  bigmemory,  # For big data computation
  dtplyr,     # For Speed 
  fastmap,    # Ultra-fast key-value storage
  collapse    # Faster Data transformation
)

# 1.a.Age structure (101 groups: 0,1, 100+)
(age_groups <- c(as.character(0:99), "100+"))
# Number of age groups
(n_age <- length(age_groups))

#1.b.Population structure  1 year age group
Population_emro_2023 <- read_csv("Population_emro_2023_1yearage.csv")
table(Population_emro_2023$Country)
(Population_emro_2023<-as.data.frame(Population_emro_2023|>
                                  filter( Year=="2023")))
#Population structure
Population_emro_2023
names(Population_emro_2023)
#Population structure in thousands 

(Population_emro_2023_in_thousands<-as.data.frame(Population_emro_2023|>
                                               mutate(Population_age=Population_age*1000)))
#1.c. Visualization of Afghanistan's population structure
pacman::p_load(ggplot2,dplyr,plotly)
names(Population_emro_2023_in_thousands)
(pyramyd<-ggplot(Population_emro_2023_in_thousands, aes(x = Age_Category, y = Population_age)) +
    geom_col(fill = "steelblue") +
    scale_y_continuous(labels = scales::label_number(scale = 1e-6, accuracy = 1, suffix = "M")) +
    facet_wrap(~ Country)+
    labs(title = "Population structure (2023)",
         x = "Age", y = "Population") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
)
#save
ggsave("Population_structure_EMRO_2023.png", plot= pyramyd, width = 12, height = 8, dpi = 300)

#ggplotly(pyramyd)
# Shorten  the name
popstruc<-Population_emro_2023_in_thousands
#1.e.Number of age groups 
(A<-n_age<-length(Population_emro_2023_in_thousands[,6]))
#1.f. births by age of mother
(popbirth <- read.csv("3.U.1.Birth_1year_emro.csv", header = TRUE))
names(popbirth)
table(popbirth$Country)
popbirth<-popbirth|>
filter(Year==2023)
#popbirth <- popbirth[ , !(names(popbirth) %in% "X")]
popbirth <- popbirth|>
  mutate(Country = if_else(Country == "UnitedArabEmirates", "United Arab Emirates", Country))
(pyramyd2<-ggplot(popbirth, aes(x = Age, y = Birth)) +
    geom_col(fill = "blue") +
    facet_wrap(~ Country)+
    labs(title = "Births in EMRO (2023)",
         x = "Age", y = "Births") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
)
ggsave("Birth_structure_EMRO_2023.png", plot= pyramyd2, width = 12, height = 8, dpi = 300)
names(popbirth)

# convert from 1000s per 1 year period to per person per day
group_durations_years <- c(rep(1, n_age))  # total of 22 groups
group_durations_days <- group_durations_years * 365
(popbirth[, 6] <-  1000*popbirth[, 6] / (group_durations_days * popstruc[, 6] * 365))# I will need to make sure based on the two first age groups(not 5 years)
#1.g.natural mortality per person per year
(mortality <- read.csv("3.U.1.EMRO_mortality_by_age_group_1yearage.csv", header = TRUE))
names(mortality)
table(mortality$Country)
(popmort<-as.data.frame(mortality|>
                  filter(Year==2023)))
(pyramyd3<-ggplot(popmort , aes(x =Age , y = Percentage)) +
    geom_col(fill = "red") +
    facet_wrap(~ Country, scales = "free_y")+
    #facet_wrap(~ Country)+
    labs(title = "Mortality in EMRO (2023)",
         x = "Age", y = "Deaths") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
)
ggsave("Mortality_structure_EMRO_2023.png",plot= pyramyd3, width = 10, height = 8, dpi=300)
