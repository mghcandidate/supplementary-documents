#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                   I.Gaps in EMRO Region
# This code provide overview of measles vaccination coverage and cases in the EMRO Region
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Work directory
getwd()
setwd("C:/Courses/Placement project")
getwd()
setwd("D:/Placement project disk")
pacman::p_load(ggplot2,dplyr,tidyr,readxl)
cov <- read_excel("3.2.Measles vaccine coverage_V12.xlsx")
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
cov<-data.frame(cov)
covdata<-cov
covdata
covdata_filtered1<-covdata|>
  filter(YEAR>=2015)|>
  filter(CODE=="EMR")
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
  filter(YEAR>=2015)
case_emro
dim(case_emro)
cov_case<-merge(covdata_filtered1,case_emro, by=c("YEAR"))
cov_case
names(cov_case)
# Max cases to normalize
max_cases <- max(cov_case$Cases_EMRO, na.rm = TRUE)
# Pivot coverage data
data_long <- cov_case%>%
  select(YEAR, MCV1, MCV2) %>%
  pivot_longer(cols = c(MCV1, MCV2), names_to = "Vaccine_Type", values_to = "Coverage")
# Create the plot
gaps<-ggplot(cov_case, aes(x = YEAR)) +
  # Bar for measles cases (rescaled to match %)
  geom_col(aes(y = Cases_EMRO / max_cases * 100, fill = "Measles Cases"), 
           alpha = 0.7, width = 0.7) +
  geom_smooth(aes(y = Cases_EMRO / max_cases * 100), 
              method = "loess", se = FALSE, color = "#BF6B6B", linetype = "solid") +
  
  # Coverage lines and points
  geom_line(data = data_long, 
            aes(y = Coverage, color = Vaccine_Type, group = Vaccine_Type), 
            linewidth = 1.2) +
  geom_point(data = data_long, 
             aes(y = Coverage, color = Vaccine_Type), 
             size = 2.5) +
  # WHO and national targets
  geom_hline(yintercept = 95, linetype = "dashed", color = "#0072B2") +
  geom_hline(yintercept = 90, linetype = "dashed", color = "darkgreen") +
  
  # Text labels for target lines
  annotate("text", x = 2016.2, y = 97.5, label = "WHO Target (95%)", 
           color = "#0072B2", hjust = 0, size = 2.5) +
  annotate("text", x = 2016.2, y = 92.5, label = "National Target (90%)", 
           color = "darkgreen", hjust = 0, size = 2.5) +
  
  # Y axes
  scale_y_continuous(
    name = "Vaccine Coverage (%)",
    sec.axis = sec_axis(~ . * max_cases / 100, name = "Number of Measles Cases", 
                        labels = scales::comma),
    limits = c(0, 100)
  ) +
  # X axis with specified range
 scale_x_continuous(
    name = "Year",
    breaks = 2015:2023,  # Shows every year
    limits = c(2014, 2024)  # Sets the range
  ) +
  
  # Colors
  scale_fill_manual(values = c("Measles Cases" = "deeppink4")) +
  scale_color_manual(values = c("MCV1" = "#0072B2", "MCV2" = "#E69F00")) +
  
  # Labels
  labs(
    title = " ",#Measles's vaccination gaps and cases trend in EMRO
    subtitle = "",#Eastern Mediterranean Region
    x = "Year",
    fill = "",
    color = "Vaccine coverage"
  ) +
# Theme
  theme_minimal(base_size = 12) +
  theme(
    axis.title.y.left = element_text(color = "blue"),
    #axis.title.y.right = element_text(color = "#FF6B6B"),
    axis.title.y.right = element_text(color = "deeppink4"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )
gaps <- gaps + theme(text = element_text(size = 14))
gaps 
ggsave("gaps.png", plot=gaps, height = 10, width = 12, dpi=300)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                      II.Country level
#                       a.cases and vaccine
#                       b.Population (added from EpiSignal Script)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#                            |-------|
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~|a.Cases|~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                            |-------|
  
data_cases <- read_excel("3.3.data.cases_allcountries_2.xlsx",sheet = "All cases_all countries")
table(data_cases$Code_region)
print(data_cases)
data_cases_filtered <-data_cases%>%
  filter(Period >= 2000 & Code_region == "EMR")
Iran<-data_cases_filtered|>
  filter(Country=="Iran")
Iran
#Period,Code_region,Region,Code_country,Country,Cases

colnames(data_cases_filtered)<-c("Period","Code_region","Region","CODE","Country","Cases")

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
  filter(Country %in% emro_country_names)|>
  filter(Period>=2000)
#emro_countries_vax_coverage|>
# filter(Country=="Ira1")
  
#merged<-merge(emro_countries_vax_coverage,data_cases_filtered,b)
merged_dat <- left_join(
  data_cases_filtered,
  emro_countries_vax_coverage,
  by = c("Period", "CODE", "Country")
)
#
merged_dat$COVERAGE[is.na(merged_dat$COVERAGE)] <- 0
merged_dat$Cases[is.na(merged_dat$Cases)] <- 0

mcv_cases<-ggplot(merged_dat, aes(x = COVERAGE, y=Cases )) +
  geom_point(alpha = 0.6, color = "darkblue") +
  geom_smooth(method = "loess", se = TRUE, color = "red", span = 0.75) +
  labs(
    title = "",#Cases and MCV coverage in EMRO
    x = "MCV coverage (%)",
    y = "Measles cases"
  ) +
facet_wrap(~ ANTIGEN) +   
  theme_minimal()
mcv_cases
ggsave("mcv_cases_correlation.png", plot=mcv_cases, height = 10, width = 12, dpi=300)
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)


# Time points as years
years <- 2000:2023
n_timepoints <- length(years)

#Plot
p1<- ggplot(merged_dat, aes(x = Period, y =Country)) +
  geom_tile(aes(fill =COVERAGE)) +
  scale_x_continuous(breaks = years, labels = years) +
  scale_fill_gradientn(
    colors = rev(c("#0000FF","#00FF00","#FF0000", "#00FFFF","darkgreen","#FFFF00","tomato" )),
    values = c(0, 0.05, 0.1,0.25, 0.5,0.75, 1),
    limits = c(0, 100),
    name = "Coverage"
  ) +
  facet_grid(Code_region ~ ANTIGEN, scales = "free_y", space = "free_y") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    axis.ticks.x = element_line(size = 0.3),
    panel.grid.major = element_line(color = "white", size = 0.3),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "right",
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )
# plot
print(p1)
# To save the plot
ggsave("vaccination_heatmap.png", p1, width = 12, height = 10, dpi = 300)

p2<- ggplot(merged_dat, aes(x = Period, y =Country)) +
  geom_tile(aes(fill =Cases)) +
  scale_x_continuous(breaks = years, labels = years) +
  scale_fill_gradientn(
    colors = rev(c("#0000FF","#00FF00","pink","#FF0000","purple","#00FFFF","darkgreen","#FFFF00" )),
    values = c(0, 0.05, 0.1, 0.5, 1),
    limits = c(0, 5000),
    name = "Cases"
  ) +
  facet_grid(Code_region ~ ANTIGEN, scales = "free_y", space = "free_y") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    axis.ticks.x = element_line(size = 0.3),
    panel.grid.major = element_line(color = "white", size = 0.3),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "right",
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )
# plot
print(p2)
# To save the plot
ggsave("cases_heatmap.png", p2, width = 12, height = 10, dpi = 300)


Iran<-merged_dat|>
  filter(Country=="Iran")
Iran
summary(Iran$Cases)

Population<-age_str_long4#From 3.1.EpiSignalDetetion_1_OneYearContact
str(Population)# I need to covert period in to numeric
Pop<-Population|>
  mutate(Period = as.numeric(Period))
Pop
#I can use pop summary instead to avoid many to many...warning 
pop_summary <- Pop %>%
  group_by(Country, CODE, Period) %>% # Here i group Pop by the variables Country, CODE, and Period. i.e any summarization that follows will happen within each group
  summarize(Country_Population = unique(Country_Population), .groups = 'drop')  #For each group, i will extracts the unique value of Country_Population.
                                                                                #assumes there's only one unique value of Country_Population per group
                                                                                #.groups = 'drop' tells dplyr not to retain the grouping structure after summarization.
#I will group pop and cases.coverage by = c("Period", "CODE","Country") but the coutry names are differents
pop_summary$Country
pop_summary1 <- pop_summary %>%
  mutate(Country = case_when(
    Country == "State of Palestine" ~ "Palestine*",
    Country == "Iran (Islamic Republic of)" ~ "Iran",
    TRUE ~ Country  # in order keep other names unchanged
  ))

merged_dat1<-left_join(
  merged_dat,
  pop_summary1,
  by = c("Period", "CODE","Country")
)
merged_dat1

#Now i am going to calculate cases per 100 000 pop : Cases = cases/Popcountry*1000
library(dplyr)
merged_dat2<-merged_dat1|>
   mutate(Cases = (Cases / Country_Population) * 1000000,
          Cases = ifelse(is.na(Cases), 0, Cases),
          ) # I assume that NA cases is zero cases
merged_dat2


merged_dat3 <- merged_dat2 %>% # (%>%)is a pipe operator from the dplyr package.
  group_by(Country) %>%#This groups my data by the Country column:-->any calculations i do next will be performed within each country separately.
  mutate(Max_Cases_1 = max(Cases)) %>% #add a new column
  ungroup()   #removes the grouping created earlier.
merged_dat3
#

Iran<-merged_dat3|>
  filter(Country=="Iran")

max(Iran$Cases)
summary(Iran$Cases)*1000
merged_data<-merged_dat3|>
mutate(Cases_scaled=(Cases/Max_Cases_1)*100)
merged_data

#Cases at 100%
Max_Cases_1 <- max(merged_data$Cases, na.rm = TRUE)
merged_data$Cases_scaled <- (merged_data$Cases / Max_Cases_1) * 100


cases_cov_plot<-ggplot(merged_data, aes(x = Period, y =COVERAGE, color =ANTIGEN)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  ylim(0, 100) +
  
  geom_hline(yintercept = 90, linetype = "dashed", color = "darkred") +
  geom_hline(yintercept = 95, linetype = "dashed", color = "darkgreen") +
  facet_wrap(~ Country, scales = "free_y")+
  geom_col(data = merged_data, aes(x = Period, y =Cases_scaled, fill = "Measles Cases"), alpha = 0.6)+
  theme(
    axis.title.y.left = element_text(color = "black"),
    axis.title.y.right = element_text(color = "deeppink4"),
    legend.position = "bottom"
  )
#
ggsave("cases_cov_plot.png",plot=cases_cov_plot, height=10, width = 10,dpi=300)
#Cases at 1500

Max_Cases_1 <- max(merged_data$Cases, na.rm = TRUE)
merged_data$Cases_scaled <- (merged_data$Cases / Max_Cases_1) * 100

cases_cov_plot <- ggplot(merged_data, aes(x = Period, y = COVERAGE, color = ANTIGEN)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 90, linetype = "dashed", color = "darkred") +
  geom_hline(yintercept = 95, linetype = "dashed", color = "darkgreen") +
  facet_wrap(~ Country, scales = "free_y") +
  
  # Bars for scaled cases
  geom_col(
    data = merged_data,
    aes(x = Period, y = Cases_scaled, fill = "Measles Cases"),
    alpha = 0.6,
    inherit.aes = FALSE
  ) +
  scale_fill_manual(
    name = "",
    values = c("Measles Cases" = "deeppink4")
  )+
  # Dual y-axis
  scale_y_continuous(
    name = "Coverage (%)",
    limits = c(0, 100),
    sec.axis = sec_axis(~ . * Max_Cases_1 / 100, name = "Cases")
  ) +
  
  theme(
    axis.title.y.left = element_text(color = "black"),
    axis.title.y.right = element_text(color = "deeppink4"),
    legend.position = "bottom"
  )

ggsave("cases_cov_plot.png", plot = cases_cov_plot, height = 12, width = 14, dpi = 300)

library(ggplot2)
#
cases_min <- min(merged_data$Cases_scaled, na.rm = TRUE)
cases_max <- max(merged_data$Cases_scaled, na.rm = TRUE)

ggplot(merged_data, aes(x = Period, y = COVERAGE, color = ANTIGEN)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  ylim(0, 100) +
  geom_hline(yintercept = 90, linetype = "dashed", color = "darkred") +
  geom_hline(yintercept = 95, linetype = "dashed", color = "darkgreen") +
  facet_wrap(~ Country, scales = "free_y") +
  geom_col(aes(y = Cases_scaled, fill = "Cases per 1000"), alpha = 0.6, position = position_identity()) +
  scale_y_continuous(
    name = "Coverage (%)",                   # axe Y droite (par défaut)
    sec.axis = sec_axis(
      trans = ~ .,                          # transformation identique (pas de changement)
      name = "Cases per 1000(scaled)",               # nom axe Y gauche secondaire
      breaks = scales::pretty_breaks(n = 5)(seq(cases_min, cases_max, length.out = 100)), # graduations personnalisées
      labels = scales::comma_format()(scales::pretty_breaks(n = 5)(seq(cases_min, cases_max, length.out = 100)))
    ),
    limits = c(0, 100)
  ) +
  theme(
    axis.title.y.left = element_text(color = "deeppink4"),   # axe Y gauche secondaire
    axis.title.y.right = element_text(color = "black"),      # axe Y droite principale
    legend.position = "bottom"
  )






# Packages  to be used 
pacman::p_load(ggplot2,dplyr)

#1. Data for Afghanistan
dat <- merged_data %>%
  filter(CODE == "AFG", ANTIGEN %in% c("MCV1", "MCV2")) %>%
  mutate(ANTIGEN = factor(ANTIGEN, levels = c("MCV1", "MCV2")))

# Max cases
max_cases <- max(dat$Cases, na.rm = TRUE)

# Mmeasles cases: scale to 0–100 for plotting
case <- dat %>%
  distinct(Period, Cases) %>%
  mutate(Cases_scaled = Cases / max_cases * 100)

# Plot
plot1<-ggplot() +
  # Line and points: vaccine coverage (left axis)
  geom_line(data = dat, aes(x = Period, y = COVERAGE, color = ANTIGEN), size = 1.2) +
  geom_point(data = dat, aes(x = Period, y = COVERAGE, color = ANTIGEN), size = 2.5) +
  
  # Bar plot: scaled measles cases (mapped to right axis)
  geom_col(data = case, aes(x = Period, y = Cases_scaled, fill = "Measles Cases"), alpha = 0.6) +
  geom_smooth(
    data = case,
    aes(x = Period, y = Cases_scaled),
    method = "loess", 
    se = FALSE, 
    color = "#BF6B6B", 
    linetype = "solid")+
  # Reference lines for WHO/National targets
  geom_hline(yintercept = 95, linetype = "dashed", color = "dodgerblue3", linewidth = 0.7) +
  geom_hline(yintercept = 90, linetype = "dashed", color = "orange", linewidth = 0.7) +
  annotate("text", x = 2016.3, y = 97.9, label = "WHO Target (95%)", color = "dodgerblue3", size = 4) +
  annotate("text", x = 2016.5, y = 93.0, label = "National Target (90%)", color = "orange", size = 4) +
  
  # Axes: primary for vaccine coverage, secondary for absolute case count
  scale_y_continuous(
    name = "Vaccine Coverage (%)",
    limits = c(0, 100),
    sec.axis = sec_axis(~ . * max_cases / 100, name = "Measles Cases")
  ) +
  scale_x_continuous(name = "Year", limits = c(2014, 2024), breaks = 2015:2023) +
  scale_color_manual(values = c("MCV1" = "darkblue", "MCV2" = "goldenrod")) +
  scale_fill_manual(values = c("Measles Cases" = "pink"), name = NULL) +
  
  # Labels and theme
  labs(
    title = " Afghanistan",
    #subtitle = "Coverage (MCV1, MCV2) and absolute measles cases (2015–2023)",
    color = "Vaccine coverage"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.y.left = element_text(color = "black"),
    axis.title.y.right = element_text(color = "deeppink4"),
    legend.position = "bottom"
  )
plot1
#2. Data for United Arab Emirates 
table(merged_data$CODE)
dat <- merged_data %>%
  filter(CODE == "ARE", ANTIGEN %in% c("MCV1", "MCV2")) %>%
  mutate(ANTIGEN = factor(ANTIGEN, levels = c("MCV1", "MCV2")))

# Max cases
max_cases <- max(dat$Cases, na.rm = TRUE)

# Mmeasles cases: scale to 0–100 for plotting
case <- dat %>%
  distinct(Period, Cases) %>%
  mutate(Cases_scaled = Cases / max_cases * 100)

# Plot
plot2<-ggplot() +
  # Line and points: vaccine coverage (left axis)
  geom_line(data = dat, aes(x = Period, y = COVERAGE, color = ANTIGEN), size = 1.2) +
  geom_point(data = dat, aes(x = Period, y = COVERAGE, color = ANTIGEN), size = 2.5) +
  
  # Bar plot: scaled measles cases (mapped to right axis)
  geom_col(data = case, aes(x = Period, y = Cases_scaled, fill = "Measles Cases"), alpha = 0.6) +
  
  # Reference lines for WHO/National targets
  geom_hline(yintercept = 95, linetype = "dashed", color = "dodgerblue3", linewidth = 0.7) +
  geom_hline(yintercept = 90, linetype = "dashed", color = "orange", linewidth = 0.7) +
  annotate("text", x = 2015.5, y = 96.5, label = "WHO Target (95%)", color = "dodgerblue3", size = 4) +
  annotate("text", x = 2015.5, y = 91.5, label = "National Target (90%)", color = "orange", size = 4) +
  
  # Axes: primary for vaccine coverage, secondary for absolute case count
  scale_y_continuous(
    name = "Vaccine Coverage (%)",
    limits = c(0, 100),
    sec.axis = sec_axis(~ . * max_cases / 100, name = "Measles Cases")
  ) +
  scale_x_continuous(name = "Year", limits = c(2014, 2024), breaks = 2015:2023) +
  scale_color_manual(values = c("MCV1" = "darkblue", "MCV2" = "goldenrod")) +
  scale_fill_manual(values = c("Measles Cases" = "pink"), name = NULL) +
  
  # Labels and theme
  labs(
    title = " United Arab Emirates",
    #subtitle = "Coverage (MCV1, MCV2) and absolute measles cases (2015–2023)",
    color = "Vaccine coverage"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.y.left = element_text(color = "black"),
    axis.title.y.right = element_text(color = "deeppink4"),
    legend.position = "bottom"
  )
plot2

#3,Data for BHR
table(merged_data$CODE)
dat <- merged_data %>%
  filter(CODE == "BHR", ANTIGEN %in% c("MCV1", "MCV2")) %>%
  mutate(ANTIGEN = factor(ANTIGEN, levels = c("MCV1", "MCV2")))

# Max cases
max_cases <- max(dat$Cases, na.rm = TRUE)

# Mmeasles cases: scale to 0–100 for plotting
case <- dat %>%
  distinct(Period, Cases) %>%
  mutate(Cases_scaled = Cases / max_cases * 100)

# Plot
plot3<-ggplot() +
  # Line and points: vaccine coverage 
  geom_line(data = dat, aes(x = Period, y = COVERAGE, color = ANTIGEN), size = 1.2) +
  geom_point(data = dat, aes(x = Period, y = COVERAGE, color = ANTIGEN), size = 2.5) +
  
  # Bar plot: scaled measles cases 
  geom_col(data = case, aes(x = Period, y = Cases_scaled, fill = "Measles Cases"), alpha = 0.6) +
  
  # Reference lines for WHO/National targets
  geom_hline(yintercept = 95, linetype = "dashed", color = "dodgerblue3", linewidth = 0.7) +
  geom_hline(yintercept = 90, linetype = "dashed", color = "orange", linewidth = 0.7) +
  annotate("text", x = 2016.5, y = 96.5, label = "WHO Target (95%)", color = "dodgerblue3", size = 4) +
  annotate("text", x = 2016.5, y = 91.5, label = "National Target (90%)", color = "orange", size = 4) +
  
  # Axes: primary for vaccine coverage, secondary for absolute case count
  scale_y_continuous(
    name = "Vaccine Coverage (%)",
    limits = c(0, 100),
    breaks = seq(0, 100, by = 10),# to better visualize
    sec.axis = sec_axis(~ . * max_cases / 100, name = "Measles Cases")
  ) +
  scale_x_continuous(name = "Year", limits = c(2014, 2024), breaks = 2015:2023) +
  scale_color_manual(values = c("MCV1" = "darkblue", "MCV2" = "goldenrod")) +
  scale_fill_manual(values = c("Measles Cases" = "pink"), name = NULL) +
  
  # Labels and theme
  labs(
    title = "Bahrain",
    #subtitle = "Coverage (MCV1, MCV2) and absolute measles cases (2015–2023)",
    color = "Vaccine coverage"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.y.left = element_text(color = "black"),
    axis.title.y.right = element_text(color = "deeppink4"),
    legend.position = "bottom"
  )
plot3

#4.Data for DJI
table(merged_data$Country)
dat <- merged_data %>%
  filter(CODE == "DJI", ANTIGEN %in% c("MCV1", "MCV2")) %>%
  mutate(ANTIGEN = factor(ANTIGEN, levels = c("MCV1", "MCV2")))

# Max cases
max_cases <- max(dat$Cases, na.rm = TRUE)

# Mmeasles cases: scale to 0–100 for plotting
case <- dat %>%
  distinct(Period, Cases) %>%
  mutate(Cases_scaled = Cases / max_cases * 100)

# Plot
plot4<-ggplot() +
  # Line and points: vaccine coverage (left axis)
  geom_line(data = dat, aes(x = Period, y = COVERAGE, color = ANTIGEN), size = 1.2) +
  geom_point(data = dat, aes(x = Period, y = COVERAGE, color = ANTIGEN), size = 2.5) +
  
  # Bar plot: scaled measles cases (mapped to right axis)
  geom_col(data = case, aes(x = Period, y = Cases_scaled, fill = "Measles Cases"), alpha = 0.6) +
  
  # Reference lines for WHO/National targets
  geom_hline(yintercept = 95, linetype = "dashed", color = "dodgerblue3", linewidth = 0.7) +
  geom_hline(yintercept = 90, linetype = "dashed", color = "orange", linewidth = 0.7) +
  annotate("text", x = 2016.3, y = 96.5, label = "WHO Target (95%)", color = "dodgerblue3", size = 4) +
  annotate("text", x = 2016.5, y = 91.5, label = "National Target (90%)", color = "orange", size = 4) +
  
  # Axes: primary for vaccine coverage, secondary for absolute case count
  scale_y_continuous(
    name = "Vaccine Coverage (%)",
    breaks = seq(0, 100, by = 10),# to better visualize
    limits = c(0, 100),
    sec.axis = sec_axis(~ . * max_cases / 100, name = "Measles Cases")
  ) +
  scale_x_continuous(name = "Year", limits = c(2014, 2024), breaks = 2015:2023) +
  scale_color_manual(values = c("MCV1" = "darkblue", "MCV2" = "goldenrod")) +
  scale_fill_manual(values = c("Measles Cases" = "pink"), name = NULL) +
  
  # Labels and theme
  labs(
    title = " Djibouti",
    #subtitle = "Coverage (MCV1, MCV2) and absolute measles cases (2015–2023)",
    color = "Vaccine coverage"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.y.left = element_text(color = "black"),
    axis.title.y.right = element_text(color = "deeppink4"),
    legend.position = "bottom"
  )
plot4

#5.Data for EGY
table(merged_data$CODE)
dat <- merged_data %>%
  filter(CODE == "EGY", ANTIGEN %in% c("MCV1", "MCV2")) %>%
  mutate(ANTIGEN = factor(ANTIGEN, levels = c("MCV1", "MCV2")))

# Max cases
max_cases <- max(dat$Cases, na.rm = TRUE)

# Mmeasles cases: scale to 0–100 for plotting
case <- dat %>%
  distinct(Period, Cases) %>%
  mutate(Cases_scaled = Cases / max_cases * 100)

# Plot
plot5<-ggplot() +
  # Line and points: vaccine coverage (left axis)
  geom_line(data = dat, aes(x = Period, y = COVERAGE, color = ANTIGEN), size = 1.2) +
  geom_point(data = dat, aes(x = Period, y = COVERAGE, color = ANTIGEN), size = 2.5) +
  
  # Bar plot: scaled measles cases (mapped to right axis)
  geom_col(data = case, aes(x = Period, y = Cases_scaled, fill = "Measles Cases"), alpha = 0.6) +
  
  # Reference lines for WHO/National targets
  geom_hline(yintercept = 95, linetype = "dashed", color = "dodgerblue3", linewidth = 0.7) +
  geom_hline(yintercept = 90, linetype = "dashed", color = "orange", linewidth = 0.7) +
  annotate("text", x = 2016.3, y = 96.5, label = "WHO Target (95%)", color = "dodgerblue3", size = 4) +
  annotate("text", x = 2016.5, y = 91.5, label = "National Target (90%)", color = "orange", size = 4) +
  
  # Axes: primary for vaccine coverage, secondary for absolute case count
  scale_y_continuous(
    name = "Vaccine Coverage (%)",
    breaks = seq(0, 100, by = 10),# to better visualize
    limits = c(0, 100),
    sec.axis = sec_axis(~ . * max_cases / 100, name = "Measles Cases")
  ) +
  scale_x_continuous(name = "Year", limits = c(2014, 2024), breaks = 2015:2023) +
  scale_color_manual(values = c("MCV1" = "darkblue", "MCV2" = "goldenrod")) +
  scale_fill_manual(values = c("Measles Cases" = "pink"), name = NULL) +
  
  # Labels and theme
  labs(
    title = " Egypt",
    #subtitle = "Coverage (MCV1, MCV2) and absolute measles cases (2015–2023)",
    color = "Vaccine coverage"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.y.left = element_text(color = "black"),
    axis.title.y.right = element_text(color = "deeppink4"),
    legend.position = "bottom"
  )
plot5

#6.Data for IRN
table(merged_data$CODE)
dat <- merged_data %>%
  filter(CODE == "IRN", ANTIGEN %in% c("MCV1", "MCV2")) %>%
  mutate(ANTIGEN = factor(ANTIGEN, levels = c("MCV1", "MCV2")))

# Max cases
max_cases <- max(dat$Cases, na.rm = TRUE)

# Mmeasles cases: scale to 0–100 for plotting
case <- dat %>%
  distinct(Period, Cases) %>%
  mutate(Cases_scaled = Cases / max_cases * 100)

# Plot
plot6<-ggplot() +
  # Line and points: vaccine coverage (left axis)
  geom_line(data = dat, aes(x = Period, y = COVERAGE, color = ANTIGEN), size = 1.2) +
  geom_point(data = dat, aes(x = Period, y = COVERAGE, color = ANTIGEN), size = 2.5) +
  
  # Bar plot: scaled measles cases (mapped to right axis)
  geom_col(data = case, aes(x = Period, y = Cases_scaled, fill = "Measles Cases"), alpha = 0.6) +
  
  # Reference lines for WHO/National targets
  geom_hline(yintercept = 95, linetype = "dashed", color = "dodgerblue3", linewidth = 0.7) +
  geom_hline(yintercept = 90, linetype = "dashed", color = "orange", linewidth = 0.7) +
  annotate("text", x = 2016.3, y = 96.5, label = "WHO Target (95%)", color = "dodgerblue3", size = 4) +
  annotate("text", x = 2016.5, y = 91.5, label = "National Target (90%)", color = "orange", size = 4) +
  
  # Axes: primary for vaccine coverage, secondary for absolute case count
  scale_y_continuous(
    name = "Vaccine Coverage (%)",
    breaks = seq(0, 100, by = 10),# to better visualize
    limits = c(0, 100),
    sec.axis = sec_axis(~ . * max_cases / 100, name = "Measles Cases")
  ) +
  scale_x_continuous(name = "Year", limits = c(2014, 2024), breaks = 2015:2023) +
  scale_color_manual(values = c("MCV1" = "darkblue", "MCV2" = "goldenrod")) +
  scale_fill_manual(values = c("Measles Cases" = "pink"), name = NULL) +
  
  # Labels and theme
  labs(
    title = " Iran",
    #subtitle = "Coverage (MCV1, MCV2) and absolute measles cases (2015–2023)",
    color = "Vaccine coverage"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.y.left = element_text(color = "black"),
    axis.title.y.right = element_text(color = "deeppink4"),
    legend.position = "bottom"
  )
plot6

#7.Data for IRQ
table(merged_data$CODE)
dat <- merged_data %>%
  filter(CODE == "IRQ", ANTIGEN %in% c("MCV1", "MCV2")) %>%
  mutate(ANTIGEN = factor(ANTIGEN, levels = c("MCV1", "MCV2")))

# Max cases
max_cases <- max(dat$Cases, na.rm = TRUE)

# Mmeasles cases: scale to 0–100 for plotting
case <- dat %>%
  distinct(Period, Cases) %>%
  mutate(Cases_scaled = Cases / max_cases * 100)

# Plot
plot7<-ggplot() +
  # Line and points: vaccine coverage (left axis)
  geom_line(data = dat, aes(x = Period, y = COVERAGE, color = ANTIGEN), size = 1.2) +
  geom_point(data = dat, aes(x = Period, y = COVERAGE, color = ANTIGEN), size = 2.5) +
  
  # Bar plot: scaled measles cases (mapped to right axis)
  geom_col(data = case, aes(x = Period, y = Cases_scaled, fill = "Measles Cases"), alpha = 0.6) +
  
  # Reference lines for WHO/National targets
  geom_hline(yintercept = 95, linetype = "dashed", color = "dodgerblue3", linewidth = 0.7) +
  geom_hline(yintercept = 90, linetype = "dashed", color = "orange", linewidth = 0.7) +
  annotate("text", x = 2016.3, y = 96.5, label = "WHO Target (95%)", color = "dodgerblue3", size = 4) +
  annotate("text", x = 2016.5, y = 91.5, label = "National Target (90%)", color = "orange", size = 4) +
  
  # Axes: primary for vaccine coverage, secondary for absolute case count
  scale_y_continuous(
    name = "Vaccine Coverage (%)",
    breaks = seq(0, 100, by = 10),# to better visualize
    limits = c(0, 100),
    sec.axis = sec_axis(~ . * max_cases / 100, name = "Measles Cases")
  ) +
  scale_x_continuous(name = "Year", limits = c(2014, 2024), breaks = 2015:2023) +
  scale_color_manual(values = c("MCV1" = "darkblue", "MCV2" = "goldenrod")) +
  scale_fill_manual(values = c("Measles Cases" = "pink"), name = NULL) +
  
  # Labels and theme
  labs(
    title = " Iraq",
    #subtitle = "Coverage (MCV1, MCV2) and absolute measles cases (2015–2023)",
    color = "Vaccine coverage"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.y.left = element_text(color = "black"),
    axis.title.y.right = element_text(color = "deeppink4"),
    legend.position = "bottom"
  )
plot7


#8.Data for JOR
table(merged_data$CODE)
dat <- merged_data %>%
  filter(CODE == "JOR", ANTIGEN %in% c("MCV1", "MCV2")) %>%
  mutate(ANTIGEN = factor(ANTIGEN, levels = c("MCV1", "MCV2")))

# Max cases
max_cases <- max(dat$Cases, na.rm = TRUE)

# Mmeasles cases: scale to 0–100 for plotting
case <- dat %>%
  distinct(Period, Cases) %>%
  mutate(Cases_scaled = Cases / max_cases * 100)

# Plot
plot8<-ggplot() +
  # Line and points: vaccine coverage (left axis)
  geom_line(data = dat, aes(x = Period, y = COVERAGE, color = ANTIGEN), size = 1.2) +
  geom_point(data = dat, aes(x = Period, y = COVERAGE, color = ANTIGEN), size = 2.5) +
  
  # Bar plot: scaled measles cases (mapped to right axis)
  geom_col(data = case, aes(x = Period, y = Cases_scaled, fill = "Measles Cases"), alpha = 0.6) +
  
  # Reference lines for WHO/National targets
  geom_hline(yintercept = 95, linetype = "dashed", color = "dodgerblue3", linewidth = 0.7) +
  geom_hline(yintercept = 90, linetype = "dashed", color = "orange", linewidth = 0.7) +
  annotate("text", x = 2016.3, y = 96.5, label = "WHO Target (95%)", color = "dodgerblue3", size = 4) +
  annotate("text", x = 2016.5, y = 91.5, label = "National Target (90%)", color = "orange", size = 4) +
  
  # Axes: primary for vaccine coverage, secondary for absolute case count
  scale_y_continuous(
    name = "Vaccine Coverage (%)",
    breaks = seq(0, 100, by = 10),# to better visualize
    limits = c(0, 100),
    sec.axis = sec_axis(~ . * max_cases / 100, name = "Measles Cases")
  ) +
  scale_x_continuous(name = "Year", limits = c(2014, 2024), breaks = 2015:2023) +
  scale_color_manual(values = c("MCV1" = "darkblue", "MCV2" = "goldenrod")) +
  scale_fill_manual(values = c("Measles Cases" = "pink"), name = NULL) +
  
  # Labels and theme
  labs(
    title = " Jordany",
    #subtitle = "Coverage (MCV1, MCV2) and absolute measles cases (2015–2023)",
    color = "Vaccine coverage"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.y.left = element_text(color = "black"),
    axis.title.y.right = element_text(color = "deeppink4"),
    legend.position = "bottom"
  )
plot8
#9.Data for JOR
table(merged_data$CODE)
dat <- merged_data %>%
  filter(CODE == "KWT", ANTIGEN %in% c("MCV1", "MCV2")) %>%
  mutate(ANTIGEN = factor(ANTIGEN, levels = c("MCV1", "MCV2")))

# Max cases
max_cases <- max(dat$Cases, na.rm = TRUE)

# Mmeasles cases: scale to 0–100 for plotting
case <- dat %>%
  distinct(Period, Cases) %>%
  mutate(Cases_scaled = Cases / max_cases * 100)

# Plot
plot9<-ggplot() +
  # Line and points: vaccine coverage (left axis)
  geom_line(data = dat, aes(x = Period, y = COVERAGE, color = ANTIGEN), size = 1.2) +
  geom_point(data = dat, aes(x = Period, y = COVERAGE, color = ANTIGEN), size = 2.5) +
  
  # Bar plot: scaled measles cases (mapped to right axis)
  geom_col(data = case, aes(x = Period, y = Cases_scaled, fill = "Measles Cases"), alpha = 0.6) +
  
  # Reference lines for WHO/National targets
  geom_hline(yintercept = 95, linetype = "dashed", color = "dodgerblue3", linewidth = 0.7) +
  geom_hline(yintercept = 90, linetype = "dashed", color = "orange", linewidth = 0.7) +
  annotate("text", x = 2016.3, y = 96.5, label = "WHO Target (95%)", color = "dodgerblue3", size = 4) +
  annotate("text", x = 2016.5, y = 91.5, label = "National Target (90%)", color = "orange", size = 4) +
  
  # Axes: primary for vaccine coverage, secondary for absolute case count
  scale_y_continuous(
    name = "Vaccine Coverage (%)",
    breaks = seq(0, 100, by = 10),# to better visualize
    limits = c(0, 100),
    sec.axis = sec_axis(~ . * max_cases / 100, name = "Measles Cases")
  ) +
  scale_x_continuous(name = "Year", limits = c(2014, 2024), breaks = 2015:2023) +
  scale_color_manual(values = c("MCV1" = "darkblue", "MCV2" = "goldenrod")) +
  scale_fill_manual(values = c("Measles Cases" = "pink"), name = NULL) +
  
  # Labels and theme
  labs(
    title = " Kuwait ",
    #subtitle = "Coverage (MCV1, MCV2) and absolute measles cases (2015–2023)",
    color = "Vaccine coverage"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.y.left = element_text(color = "black"),
    axis.title.y.right = element_text(color = "deeppink4"),
    legend.position = "bottom"
  )
plot9
#10.Data for LBN
table(merged_data$CODE)
dat <- merged_data %>%
  filter(CODE == "LBN", ANTIGEN %in% c("MCV1", "MCV2")) %>%
  mutate(ANTIGEN = factor(ANTIGEN, levels = c("MCV1", "MCV2")))

# Max cases
max_cases <- max(dat$Cases, na.rm = TRUE)

# Mmeasles cases: scale to 0–100 for plotting
case <- dat %>%
  distinct(Period, Cases) %>%
  mutate(Cases_scaled = Cases / max_cases * 100)

# Plot
plot10<-ggplot() +
  # Line and points: vaccine coverage (left axis)
  geom_line(data = dat, aes(x = Period, y = COVERAGE, color = ANTIGEN), size = 1.2) +
  geom_point(data = dat, aes(x = Period, y = COVERAGE, color = ANTIGEN), size = 2.5) +
  
  # Bar plot: scaled measles cases (mapped to right axis)
  geom_col(data = case, aes(x = Period, y = Cases_scaled, fill = "Measles Cases"), alpha = 0.6) +
  
  # Reference lines for WHO/National targets
  geom_hline(yintercept = 95, linetype = "dashed", color = "dodgerblue3", linewidth = 0.7) +
  geom_hline(yintercept = 90, linetype = "dashed", color = "orange", linewidth = 0.7) +
  annotate("text", x = 2016.3, y = 96.5, label = "WHO Target (95%)", color = "dodgerblue3", size = 4) +
  annotate("text", x = 2016.5, y = 91.5, label = "National Target (90%)", color = "orange", size = 4) +
  
  # Axes: primary for vaccine coverage, secondary for absolute case count
  scale_y_continuous(
    name = "Vaccine Coverage (%)",
    breaks = seq(0, 100, by = 10),# to better visualize
    limits = c(0, 100),
    sec.axis = sec_axis(~ . * max_cases / 100, name = "Measles Cases")
  ) +
  scale_x_continuous(name = "Year", limits = c(2014, 2024), breaks = 2015:2023) +
  scale_color_manual(values = c("MCV1" = "darkblue", "MCV2" = "goldenrod")) +
  scale_fill_manual(values = c("Measles Cases" = "pink"), name = NULL) +
  
  # Labels and theme
  labs(
    title = " Lebanon ",
    #subtitle = "Coverage (MCV1, MCV2) and absolute measles cases (2015–2023)",
    color = "Vaccine coverage"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.y.left = element_text(color = "black"),
    axis.title.y.right = element_text(color = "deeppink4"),
    legend.position = "bottom"
  )
plot10

#11.Data for LBY 
table(merged_data$CODE)
dat <- merged_data %>%
  filter(CODE == "LBY", ANTIGEN %in% c("MCV1", "MCV2")) %>%
  mutate(ANTIGEN = factor(ANTIGEN, levels = c("MCV1", "MCV2")))

# Max cases
max_cases <- max(dat$Cases, na.rm = TRUE)

# Mmeasles cases: scale to 0–100 for plotting
case <- dat %>%
  distinct(Period, Cases) %>%
  mutate(Cases_scaled = Cases / max_cases * 100)

# Plot
plot11<-ggplot() +
  # Line and points: vaccine coverage (left axis)
  geom_line(data = dat, aes(x = Period, y = COVERAGE, color = ANTIGEN), size = 1.2) +
  geom_point(data = dat, aes(x = Period, y = COVERAGE, color = ANTIGEN), size = 2.5) +
  
  # Bar plot: scaled measles cases (mapped to right axis)
  geom_col(data = case, aes(x = Period, y = Cases_scaled, fill = "Measles Cases"), alpha = 0.6) +
  
  # Reference lines for WHO/National targets
  geom_hline(yintercept = 95, linetype = "dashed", color = "dodgerblue3", linewidth = 0.7) +
  geom_hline(yintercept = 90, linetype = "dashed", color = "orange", linewidth = 0.7) +
  annotate("text", x = 2016.3, y = 96.5, label = "WHO Target (95%)", color = "dodgerblue3", size = 4) +
  annotate("text", x = 2016.5, y = 91.5, label = "National Target (90%)", color = "orange", size = 4) +
  
  # Axes: primary for vaccine coverage, secondary for absolute case count
  scale_y_continuous(
    name = "Vaccine Coverage (%)",
    breaks = seq(0, 100, by = 10),# to better visualize
    limits = c(0, 100),
    sec.axis = sec_axis(~ . * max_cases / 100, name = "Measles Cases")
  ) +
  scale_x_continuous(name = "Year", limits = c(2014, 2024), breaks = 2015:2023) +
  scale_color_manual(values = c("MCV1" = "darkblue", "MCV2" = "goldenrod")) +
  scale_fill_manual(values = c("Measles Cases" = "pink"), name = NULL) +
  
  # Labels and theme
  labs(
    title = " Lybia ",
    #subtitle = "Coverage (MCV1, MCV2) and absolute measles cases (2015–2023)",
    color = "Vaccine coverage"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.y.left = element_text(color = "black"),
    axis.title.y.right = element_text(color = "deeppink4"),
    legend.position = "bottom"
  )
plot11

#12.Data for MAR 
table(merged_data$CODE)
dat <- merged_data %>%
  filter(CODE =="MAR", ANTIGEN %in% c("MCV1", "MCV2")) %>%
  mutate(ANTIGEN = factor(ANTIGEN, levels = c("MCV1", "MCV2")))

# Max cases
max_cases <- max(dat$Cases, na.rm = TRUE)

# Mmeasles cases: scale to 0–100 for plotting
case <- dat %>%
  distinct(Period, Cases) %>%
  mutate(Cases_scaled = Cases / max_cases * 100)

# Plot
plot12<-ggplot() +
  # Line and points: vaccine coverage (left axis)
  geom_line(data = dat, aes(x = Period, y = COVERAGE, color = ANTIGEN), size = 1.2) +
  geom_point(data = dat, aes(x = Period, y = COVERAGE, color = ANTIGEN), size = 2.5) +
  
  # Bar plot: scaled measles cases (mapped to right axis)
  geom_col(data = case, aes(x = Period, y = Cases_scaled, fill = "Measles Cases"), alpha = 0.6) +
  
  # Reference lines for WHO/National targets
  geom_hline(yintercept = 95, linetype = "dashed", color = "dodgerblue3", linewidth = 0.7) +
  geom_hline(yintercept = 90, linetype = "dashed", color = "orange", linewidth = 0.7) +
  annotate("text", x = 2016.3, y = 96.5, label = "WHO Target (95%)", color = "dodgerblue3", size = 4) +
  annotate("text", x = 2016.5, y = 91.5, label = "National Target (90%)", color = "orange", size = 4) +
  
  # Axes: primary for vaccine coverage, secondary for absolute case count
  scale_y_continuous(
    name = "Vaccine Coverage (%)",
    breaks = seq(0, 100, by = 10),# to better visualize
    limits = c(0, 100),
    sec.axis = sec_axis(~ . * max_cases / 100, name = "Measles Cases")
  ) +
  scale_x_continuous(name = "Year", limits = c(2014, 2024), breaks = 2015:2023) +
  scale_color_manual(values = c("MCV1" = "darkblue", "MCV2" = "goldenrod")) +
  scale_fill_manual(values = c("Measles Cases" = "pink"), name = NULL) +
  
  # Labels and theme
  labs(
    title = " Morocco ",
    #subtitle = "Coverage (MCV1, MCV2) and absolute measles cases (2015–2023)",
    color = "Vaccine coverage"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.y.left = element_text(color = "black"),
    axis.title.y.right = element_text(color = "deeppink4"),
    legend.position = "bottom"
  )
plot12
#13.Data for OMN
table(merged_data$CODE)
dat <- merged_data %>%
  filter(CODE =="OMN", ANTIGEN %in% c("MCV1", "MCV2")) %>%
  mutate(ANTIGEN = factor(ANTIGEN, levels = c("MCV1", "MCV2")))

# Max cases
max_cases <- max(dat$Cases, na.rm = TRUE)

# Mmeasles cases: scale to 0–100 for plotting
case <- dat %>%
  distinct(Period, Cases) %>%
  mutate(Cases_scaled = Cases / max_cases * 100)

# Plot
plot13<-ggplot() +
  # Line and points: vaccine coverage (left axis)
  geom_line(data = dat, aes(x = Period, y = COVERAGE, color = ANTIGEN), size = 1.2) +
  geom_point(data = dat, aes(x = Period, y = COVERAGE, color = ANTIGEN), size = 2.5) +
  
  # Bar plot: scaled measles cases (mapped to right axis)
  geom_col(data = case, aes(x = Period, y = Cases_scaled, fill = "Measles Cases"), alpha = 0.6) +
  
  # Reference lines for WHO/National targets
  geom_hline(yintercept = 95, linetype = "dashed", color = "dodgerblue3", linewidth = 0.7) +
  geom_hline(yintercept = 90, linetype = "dashed", color = "orange", linewidth = 0.7) +
  annotate("text", x = 2016.3, y = 96.5, label = "WHO Target (95%)", color = "dodgerblue3", size = 4) +
  annotate("text", x = 2016.5, y = 91.5, label = "National Target (90%)", color = "orange", size = 4) +
  
  # Axes: primary for vaccine coverage, secondary for absolute case count
  scale_y_continuous(
    name = "Vaccine Coverage (%)",
    breaks = seq(0, 100, by = 10),# to better visualize
    limits = c(0, 100),
    sec.axis = sec_axis(~ . * max_cases / 100, name = "Measles Cases")
  ) +
  scale_x_continuous(name = "Year", limits = c(2014, 2024), breaks = 2015:2023) +
  scale_color_manual(values = c("MCV1" = "darkblue", "MCV2" = "goldenrod")) +
  scale_fill_manual(values = c("Measles Cases" = "pink"), name = NULL) +
  
  # Labels and theme
  labs(
    title = " Oman ",
    #subtitle = "Coverage (MCV1, MCV2) and absolute measles cases (2015–2023)",
    color = "Vaccine coverage"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.y.left = element_text(color = "black"),
    axis.title.y.right = element_text(color = "deeppink4"),
    legend.position = "bottom"
  )
plot13
#14.Data for PAK
table(merged_data$CODE)
dat <- merged_data %>%
  filter(CODE =="PAK", ANTIGEN %in% c("MCV1", "MCV2")) %>%
  mutate(ANTIGEN = factor(ANTIGEN, levels = c("MCV1", "MCV2")))

# Max cases
max_cases <- max(dat$Cases, na.rm = TRUE)

# Mmeasles cases: scale to 0–100 for plotting
case <- dat %>%
  distinct(Period, Cases) %>%
  mutate(Cases_scaled = Cases / max_cases * 100)

# Plot
plot14<-ggplot() +
  # Line and points: vaccine coverage (left axis)
  geom_line(data = dat, aes(x = Period, y = COVERAGE, color = ANTIGEN), size = 1.2) +
  geom_point(data = dat, aes(x = Period, y = COVERAGE, color = ANTIGEN), size = 2.5) +
  
  # Bar plot: scaled measles cases (mapped to right axis)
  geom_col(data = case, aes(x = Period, y = Cases_scaled, fill = "Measles Cases"), alpha = 0.6) +
  
  # Reference lines for WHO/National targets
  geom_hline(yintercept = 95, linetype = "dashed", color = "dodgerblue3", linewidth = 0.7) +
  geom_hline(yintercept = 90, linetype = "dashed", color = "orange", linewidth = 0.7) +
  annotate("text", x = 2016.3, y = 96.5, label = "WHO Target (95%)", color = "dodgerblue3", size = 4) +
  annotate("text", x = 2016.5, y = 91.5, label = "National Target (90%)", color = "orange", size = 4) +
  
  # Axes: primary for vaccine coverage, secondary for absolute case count
  scale_y_continuous(
    name = "Vaccine Coverage (%)",
    breaks = seq(0, 100, by = 10),# to better visualize
    limits = c(0, 100),
    sec.axis = sec_axis(~ . * max_cases / 100, name = "Measles Cases")
  ) +
  scale_x_continuous(name = "Year", limits = c(2014, 2024), breaks = 2015:2023) +
  scale_color_manual(values = c("MCV1" = "darkblue", "MCV2" = "goldenrod")) +
  scale_fill_manual(values = c("Measles Cases" = "pink"), name = NULL) +
  
  # Labels and theme
  labs(
    title = " Pakistan ",
    #subtitle = "Coverage (MCV1, MCV2) and absolute measles cases (2015–2023)",
    color = "Vaccine coverage"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.y.left = element_text(color = "black"),
    axis.title.y.right = element_text(color = "deeppink4"),
    legend.position = "bottom"
  )
plot14
#15.Data for Palestine
table(merged_data$CODE)
dat <- merged_data %>%
  filter(CODE =="PSE", ANTIGEN %in% c("MCV1", "MCV2")) %>%
  mutate(ANTIGEN = factor(ANTIGEN, levels = c("MCV1", "MCV2")))

# Max cases
max_cases <- max(dat$Cases, na.rm = TRUE)

# Mmeasles cases: scale to 0–100 for plotting
case <- dat %>%
  distinct(Period, Cases) %>%
  mutate(Cases_scaled = Cases / max_cases * 100)

# Plot
plot15<-ggplot() +
  # Line and points: vaccine coverage (left axis)
  geom_line(data = dat, aes(x = Period, y = COVERAGE, color = ANTIGEN), size = 1.2) +
  geom_point(data = dat, aes(x = Period, y = COVERAGE, color = ANTIGEN), size = 2.5) +
  
  # Bar plot: scaled measles cases (mapped to right axis)
  geom_col(data = case, aes(x = Period, y = Cases_scaled, fill = "Measles Cases"), alpha = 0.6) +
  
  # Reference lines for WHO/National targets
  geom_hline(yintercept = 95, linetype = "dashed", color = "dodgerblue3", linewidth = 0.7) +
  geom_hline(yintercept = 90, linetype = "dashed", color = "orange", linewidth = 0.7) +
  annotate("text", x = 2016.3, y = 96.5, label = "WHO Target (95%)", color = "dodgerblue3", size = 4) +
  annotate("text", x = 2016.5, y = 91.5, label = "National Target (90%)", color = "orange", size = 4) +
  
  # Axes: primary for vaccine coverage, secondary for absolute case count
  scale_y_continuous(
    name = "Vaccine Coverage (%)",
    breaks = seq(0, 100, by = 10),# to better visualize
    limits = c(0, 100),
    sec.axis = sec_axis(~ . * max_cases / 100, name = "Measles Cases")
  ) +
  scale_x_continuous(name = "Year", limits = c(2014, 2024), breaks = 2015:2023) +
  scale_color_manual(values = c("MCV1" = "darkblue", "MCV2" = "goldenrod")) +
  scale_fill_manual(values = c("Measles Cases" = "pink"), name = NULL) +
  
  # Labels and theme
  labs(
    title = " Palestine ",
    #subtitle = "Coverage (MCV1, MCV2) and absolute measles cases (2015–2023)",
    color = "Vaccine coverage"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.y.left = element_text(color = "black"),
    axis.title.y.right = element_text(color = "deeppink4"),
    legend.position = "bottom"
  )
plot15
#16.Data for QAT
table(merged_data$CODE)
dat <- merged_data %>%
  filter(CODE =="QAT", ANTIGEN %in% c("MCV1", "MCV2")) %>%
  mutate(ANTIGEN = factor(ANTIGEN, levels = c("MCV1", "MCV2")))

# Max cases
max_cases <- max(dat$Cases, na.rm = TRUE)

# Mmeasles cases: scale to 0–100 for plotting
case <- dat %>%
  distinct(Period, Cases) %>%
  mutate(Cases_scaled = Cases / max_cases * 100)

# Plot
plot16<-ggplot() +
  # Line and points: vaccine coverage (left axis)
  geom_line(data = dat, aes(x = Period, y = COVERAGE, color = ANTIGEN), size = 1.2) +
  geom_point(data = dat, aes(x = Period, y = COVERAGE, color = ANTIGEN), size = 2.5) +
  
  # Bar plot: scaled measles cases (mapped to right axis)
  geom_col(data = case, aes(x = Period, y = Cases_scaled, fill = "Measles Cases"), alpha = 0.6) +
  
  # Reference lines for WHO/National targets
  geom_hline(yintercept = 95, linetype = "dashed", color = "dodgerblue3", linewidth = 0.7) +
  geom_hline(yintercept = 90, linetype = "dashed", color = "orange", linewidth = 0.7) +
  annotate("text", x = 2016.3, y = 96.5, label = "WHO Target (95%)", color = "dodgerblue3", size = 4) +
  annotate("text", x = 2016.5, y = 91.5, label = "National Target (90%)", color = "orange", size = 4) +
  
  # Axes: primary for vaccine coverage, secondary for absolute case count
  scale_y_continuous(
    name = "Vaccine Coverage (%)",
    breaks = seq(0, 100, by = 10),# to better visualize
    limits = c(0, 100),
    sec.axis = sec_axis(~ . * max_cases / 100, name = "Measles Cases")
  ) +
  scale_x_continuous(name = "Year", limits = c(2014, 2024), breaks = 2015:2023) +
  scale_color_manual(values = c("MCV1" = "darkblue", "MCV2" = "goldenrod")) +
  scale_fill_manual(values = c("Measles Cases" = "pink"), name = NULL) +
  
  # Labels and theme
  labs(
    title = " Quatar ",
    #subtitle = "Coverage (MCV1, MCV2) and absolute measles cases (2015–2023)",
    color = "Vaccine coverage"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.y.left = element_text(color = "black"),
    axis.title.y.right = element_text(color = "deeppink4"),
    legend.position = "bottom"
  )
plot16
#17.Data for SAU
table(merged_data$CODE)
dat <- merged_data %>%
  filter(CODE =="SAU", ANTIGEN %in% c("MCV1", "MCV2")) %>%
  mutate(ANTIGEN = factor(ANTIGEN, levels = c("MCV1", "MCV2")))

# Max cases
max_cases <- max(dat$Cases, na.rm = TRUE)

# Mmeasles cases: scale to 0–100 for plotting
case <- dat %>%
  distinct(Period, Cases) %>%
  mutate(Cases_scaled = Cases / max_cases * 100)

# Plot
plot17<-ggplot() +
  # Line and points: vaccine coverage (left axis)
  geom_line(data = dat, aes(x = Period, y = COVERAGE, color = ANTIGEN), size = 1.2) +
  geom_point(data = dat, aes(x = Period, y = COVERAGE, color = ANTIGEN), size = 2.5) +
  
  # Bar plot: scaled measles cases (mapped to right axis)
  geom_col(data = case, aes(x = Period, y = Cases_scaled, fill = "Measles Cases"), alpha = 0.6) +
  
  # Reference lines for WHO/National targets
  geom_hline(yintercept = 95, linetype = "dashed", color = "dodgerblue3", linewidth = 0.7) +
  geom_hline(yintercept = 90, linetype = "dashed", color = "orange", linewidth = 0.7) +
  annotate("text", x = 2016.3, y = 96.5, label = "WHO Target (95%)", color = "dodgerblue3", size = 4) +
  annotate("text", x = 2016.5, y = 91.5, label = "National Target (90%)", color = "orange", size = 4) +
  
  # Axes: primary for vaccine coverage, secondary for absolute case count
  scale_y_continuous(
    name = "Vaccine Coverage (%)",
    breaks = seq(0, 100, by = 10),# to better visualize
    limits = c(0, 100),
    sec.axis = sec_axis(~ . * max_cases / 100, name = "Measles Cases")
  ) +
  scale_x_continuous(name = "Year", limits = c(2014, 2024), breaks = 2015:2023) +
  scale_color_manual(values = c("MCV1" = "darkblue", "MCV2" = "goldenrod")) +
  scale_fill_manual(values = c("Measles Cases" = "pink"), name = NULL) +
  
  # Labels and theme
  labs(
    title = "Saudi Arabia ",
    #subtitle = "Coverage (MCV1, MCV2) and absolute measles cases (2015–2023)",
    color = "Vaccine coverage"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.y.left = element_text(color = "black"),
    axis.title.y.right = element_text(color = "deeppink4"),
    legend.position = "bottom"
  )
plot17
#18.Data for SDN
table(merged_data$CODE)
dat <- merged_data %>%
  filter(CODE =="SDN", ANTIGEN %in% c("MCV1", "MCV2")) %>%
  mutate(ANTIGEN = factor(ANTIGEN, levels = c("MCV1", "MCV2")))

# Max cases
max_cases <- max(dat$Cases, na.rm = TRUE)

# Mmeasles cases: scale to 0–100 for plotting
case <- dat %>%
  distinct(Period, Cases) %>%
  mutate(Cases_scaled = Cases / max_cases * 100)

# Plot
plot18<-ggplot() +
  # Line and points: vaccine coverage (left axis)
  geom_line(data = dat, aes(x = Period, y = COVERAGE, color = ANTIGEN), size = 1.2) +
  geom_point(data = dat, aes(x = Period, y = COVERAGE, color = ANTIGEN), size = 2.5) +
  
  # Bar plot: scaled measles cases (mapped to right axis)
  geom_col(data = case, aes(x = Period, y = Cases_scaled, fill = "Measles Cases"), alpha = 0.6) +
  
  # Reference lines for WHO/National targets
  geom_hline(yintercept = 95, linetype = "dashed", color = "dodgerblue3", linewidth = 0.7) +
  geom_hline(yintercept = 90, linetype = "dashed", color = "orange", linewidth = 0.7) +
  annotate("text", x = 2016.3, y = 96.5, label = "WHO Target (95%)", color = "dodgerblue3", size = 4) +
  annotate("text", x = 2016.5, y = 91.5, label = "National Target (90%)", color = "orange", size = 4) +
  
  # Axes: primary for vaccine coverage, secondary for absolute case count
  scale_y_continuous(
    name = "Vaccine Coverage (%)",
    breaks = seq(0, 100, by = 10),# to better visualize
    limits = c(0, 100),
    sec.axis = sec_axis(~ . * max_cases / 100, name = "Measles Cases")
  ) +
  scale_x_continuous(name = "Year", limits = c(2014, 2024), breaks = 2015:2023) +
  scale_color_manual(values = c("MCV1" = "darkblue", "MCV2" = "goldenrod")) +
  scale_fill_manual(values = c("Measles Cases" = "pink"), name = NULL) +
  
  # Labels and theme
  labs(
    title = " Sudan ",
    #subtitle = "Coverage (MCV1, MCV2) and absolute measles cases (2015–2023)",
    color = "Vaccine coverage"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.y.left = element_text(color = "black"),
    axis.title.y.right = element_text(color = "deeppink4"),
    legend.position = "bottom"
  )
plot18
#19.Data for SDN
table(merged_data$CODE)
dat <- merged_data %>%
  filter(CODE =="SOM", ANTIGEN %in% c("MCV1", "MCV2")) %>%
  mutate(ANTIGEN = factor(ANTIGEN, levels = c("MCV1", "MCV2")))

# Max cases
max_cases <- max(dat$Cases, na.rm = TRUE)

# Mmeasles cases: scale to 0–100 for plotting
case <- dat %>%
  distinct(Period, Cases) %>%
  mutate(Cases_scaled = Cases / max_cases * 100)

# Plot
plot19<-ggplot() +
  # Line and points: vaccine coverage (left axis)
  geom_line(data = dat, aes(x = Period, y = COVERAGE, color = ANTIGEN), size = 1.2) +
  geom_point(data = dat, aes(x = Period, y = COVERAGE, color = ANTIGEN), size = 2.5) +
  
  # Bar plot: scaled measles cases (mapped to right axis)
  geom_col(data = case, aes(x = Period, y = Cases_scaled, fill = "Measles Cases"), alpha = 0.6) +
  
  # Reference lines for WHO/National targets
  geom_hline(yintercept = 95, linetype = "dashed", color = "dodgerblue3", linewidth = 0.7) +
  geom_hline(yintercept = 90, linetype = "dashed", color = "orange", linewidth = 0.7) +
  annotate("text", x = 2016.3, y = 96.5, label = "WHO Target (95%)", color = "dodgerblue3", size = 4) +
  annotate("text", x = 2016.5, y = 91.5, label = "National Target (90%)", color = "orange", size = 4) +
  
  # Axes: primary for vaccine coverage, secondary for absolute case count
  scale_y_continuous(
    name = "Vaccine Coverage (%)",
    breaks = seq(0, 100, by = 10),# to better visualize
    limits = c(0, 100),
    sec.axis = sec_axis(~ . * max_cases / 100, name = "Measles Cases")
  ) +
  scale_x_continuous(name = "Year", limits = c(2014, 2024), breaks = 2015:2023) +
  scale_color_manual(values = c("MCV1" = "darkblue", "MCV2" = "goldenrod")) +
  scale_fill_manual(values = c("Measles Cases" = "pink"), name = NULL) +
  
  # Labels and theme
  labs(
    title = " Somalia ",
    #subtitle = "Coverage (MCV1, MCV2) and absolute measles cases (2015–2023)",
    color = "Vaccine coverage"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.y.left = element_text(color = "black"),
    axis.title.y.right = element_text(color = "deeppink4"),
    legend.position = "bottom"
  )
plot19
#20.Data for SYR
table(merged_data$CODE)
dat <- merged_data %>%
  filter(CODE =="SYR", ANTIGEN %in% c("MCV1", "MCV2")) %>%
  mutate(ANTIGEN = factor(ANTIGEN, levels = c("MCV1", "MCV2")))

# Max cases
max_cases <- max(dat$Cases, na.rm = TRUE)

# Mmeasles cases: scale to 0–100 for plotting
case <- dat %>%
  distinct(Period, Cases) %>%
  mutate(Cases_scaled = Cases / max_cases * 100)

# Plot
plot20<-ggplot() +
  # Line and points: vaccine coverage (left axis)
  geom_line(data = dat, aes(x = Period, y = COVERAGE, color = ANTIGEN), size = 1.2) +
  geom_point(data = dat, aes(x = Period, y = COVERAGE, color = ANTIGEN), size = 2.5) +
  
  # Bar plot: scaled measles cases (mapped to right axis)
  geom_col(data = case, aes(x = Period, y = Cases_scaled, fill = "Measles Cases"), alpha = 0.6) +
  
  # Reference lines for WHO/National targets
  geom_hline(yintercept = 95, linetype = "dashed", color = "dodgerblue3", linewidth = 0.7) +
  geom_hline(yintercept = 90, linetype = "dashed", color = "orange", linewidth = 0.7) +
  annotate("text", x = 2016.3, y = 96.5, label = "WHO Target (95%)", color = "dodgerblue3", size = 4) +
  annotate("text", x = 2016.5, y = 91.5, label = "National Target (90%)", color = "orange", size = 4) +
  
  # Axes: primary for vaccine coverage, secondary for absolute case count
  scale_y_continuous(
    name = "Vaccine Coverage (%)",
    breaks = seq(0, 100, by = 10),# to better visualize
    limits = c(0, 100),
    sec.axis = sec_axis(~ . * max_cases / 100, name = "Measles Cases")
  ) +
  scale_x_continuous(name = "Year", limits = c(2014, 2024), breaks = 2015:2023) +
  scale_color_manual(values = c("MCV1" = "darkblue", "MCV2" = "goldenrod")) +
  scale_fill_manual(values = c("Measles Cases" = "pink"), name = NULL) +
  
  # Labels and theme
  labs(
    title = " Syria ",
    #subtitle = "Coverage (MCV1, MCV2) and absolute measles cases (2015–2023)",
    color = "Vaccine coverage"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.y.left = element_text(color = "black"),
    axis.title.y.right = element_text(color = "deeppink4"),
    legend.position = "bottom"
  )
plot20

#21.Data for TUN
table(merged_data$CODE)
dat <- merged_data %>%
  filter(CODE =="TUN", ANTIGEN %in% c("MCV1", "MCV2")) %>%
  mutate(ANTIGEN = factor(ANTIGEN, levels = c("MCV1", "MCV2")))

# Max cases
max_cases <- max(dat$Cases, na.rm = TRUE)

# Mmeasles cases: scale to 0–100 for plotting
case <- dat %>%
  distinct(Period, Cases) %>%
  mutate(Cases_scaled = Cases / max_cases * 100)

# Plot
plot21<-ggplot() +
  # Line and points: vaccine coverage (left axis)
  geom_line(data = dat, aes(x = Period, y = COVERAGE, color = ANTIGEN), size = 1.2) +
  geom_point(data = dat, aes(x = Period, y = COVERAGE, color = ANTIGEN), size = 2.5) +
  
  # Bar plot: scaled measles cases (mapped to right axis)
  geom_col(data = case, aes(x = Period, y = Cases_scaled, fill = "Measles Cases"), alpha = 0.6) +
  
  # Reference lines for WHO/National targets
  geom_hline(yintercept = 95, linetype = "dashed", color = "dodgerblue3", linewidth = 0.7) +
  geom_hline(yintercept = 90, linetype = "dashed", color = "orange", linewidth = 0.7) +
  annotate("text", x = 2016.3, y = 96.5, label = "WHO Target (95%)", color = "dodgerblue3", size = 4) +
  annotate("text", x = 2016.5, y = 91.5, label = "National Target (90%)", color = "orange", size = 4) +
  
  # Axes: primary for vaccine coverage, secondary for absolute case count
  scale_y_continuous(
    name = "Vaccine Coverage (%)",
    breaks = seq(0, 100, by = 10),# to better visualize
    limits = c(0, 100),
    sec.axis = sec_axis(~ . * max_cases / 100, name = "Measles Cases")
  ) +
  scale_x_continuous(name = "Year", limits = c(2014, 2024), breaks = 2015:2023) +
  scale_color_manual(values = c("MCV1" = "darkblue", "MCV2" = "goldenrod")) +
  scale_fill_manual(values = c("Measles Cases" = "pink"), name = NULL) +
  
  # Labels and theme
  labs(
    title = " Tunisia ",
    #subtitle = "Coverage (MCV1, MCV2) and absolute measles cases (2015–2023)",
    color = "Vaccine coverage"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.y.left = element_text(color = "black"),
    axis.title.y.right = element_text(color = "deeppink4"),
    legend.position = "bottom"
  )
plot21
#22.Data for YEM
table(merged_data$CODE)
dat <- merged_data %>%
  filter(CODE =="YEM", ANTIGEN %in% c("MCV1", "MCV2")) %>%
  mutate(ANTIGEN = factor(ANTIGEN, levels = c("MCV1", "MCV2")))

# Max cases
max_cases <- max(dat$Cases, na.rm = TRUE)

# Mmeasles cases: scale to 0–100 for plotting
case <- dat %>%
  distinct(Period, Cases) %>%
  mutate(Cases_scaled = Cases / max_cases * 100)

# Plot
plot22<-ggplot() +
  # Line and points: vaccine coverage (left axis)
  geom_line(data = dat, aes(x = Period, y = COVERAGE, color = ANTIGEN), size = 1.2) +
  geom_point(data = dat, aes(x = Period, y = COVERAGE, color = ANTIGEN), size = 2.5) +
  
  # Bar plot: scaled measles cases (mapped to right axis)
  geom_col(data = case, aes(x = Period, y = Cases_scaled, fill = "Measles Cases"), alpha = 0.6) +
  
  # Reference lines for WHO/National targets
  geom_hline(yintercept = 95, linetype = "dashed", color = "dodgerblue3", linewidth = 0.7) +
  geom_hline(yintercept = 90, linetype = "dashed", color = "orange", linewidth = 0.7) +
  annotate("text", x = 2016.3, y = 96.5, label = "WHO Target (95%)", color = "dodgerblue3", size = 4) +
  annotate("text", x = 2016.5, y = 91.5, label = "National Target (90%)", color = "orange", size = 4) +
  
  # Axes: primary for vaccine coverage, secondary for absolute case count
  scale_y_continuous(
    name = "Vaccine Coverage (%)",
    breaks = seq(0, 100, by = 10),# to better visualize
    limits = c(0, 100),
    sec.axis = sec_axis(~ . * max_cases / 100, name = "Measles Cases")
  ) +
  scale_x_continuous(name = "Year", limits = c(2014, 2024), breaks = 2015:2023) +
  scale_color_manual(values = c("MCV1" = "darkblue", "MCV2" = "goldenrod")) +
  scale_fill_manual(values = c("Measles Cases" = "pink"), name = NULL) +
  
  # Labels and theme
  labs(
    title = " Yemen",
    #subtitle = "Coverage (MCV1, MCV2) and absolute measles cases (2015–2023)",
    color = "Vaccine coverage"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.y.left = element_text(color = "black"),
    axis.title.y.right = element_text(color = "deeppink4"),
    legend.position = "bottom"
  )
plot22
par(mfrow=c(6,4))


g1<-gridExtra::grid.arrange(plot1,plot2,
                        plot3,plot4
                        )
g2<-gridExtra::grid.arrange(plot5,plot6,
                            plot7,plot8
)
g3<-gridExtra::grid.arrange(plot9,plot10,
                            plot11,plot12
)
g4<-gridExtra::grid.arrange(plot13,plot14,
                            plot15,plot16
)

g5<-gridExtra::grid.arrange(plot17,plot18,
                            plot19,plot20
)

g6<-gridExtra::grid.arrange(plot21,plot22,ncol=2
)


#All these graphs combined 
merged_data1 <- merged_data %>%
  group_by(CODE) %>%
  mutate(Max_cases_country = max(Cases, na.rm = TRUE)) %>%
  ungroup()
merged_data1 

# List of all EMRO country codes
emro_countries <- c("AFG", "BHR", "DJI", "EGY", "IRN", "IRQ", "JOR", "KWT", 
                    "LBN", "LBY", "MAR", "OMN", "PAK", "PSE", "QAT", "SAU", 
                    "SDN", "SOM", "SYR", "TUN", "ARE", "YEM")

# Filter data for all EMRO countries and MCV1/MCV2
dat <- merged_data1 %>%
  filter(CODE %in% emro_countries,
         ANTIGEN %in% c("MCV1", "MCV2")) %>%
  mutate(ANTIGEN = factor(ANTIGEN, levels = c("MCV1", "MCV2"))) %>%
  group_by(CODE)|>
  ungroup()

# Measles cases: scale to 0-100 for plotting using Max_cases_country
case <- dat %>%
  distinct(CODE, Period, Cases, Max_cases_country) %>%
  mutate(Cases_scaled = Cases / Max_cases_country * 100)

case <- dat %>%
  distinct(CODE, Period, Cases, Max_cases_country) %>%
  mutate(Cases_scaled = Cases)

# Plot with facets
plot_emro <- ggplot() +
  # Line and points: vaccine coverage (left axis)
  geom_line(data = dat, aes(x = Period, y = COVERAGE, color = ANTIGEN), size = 1.2) +
  geom_point(data = dat, aes(x = Period, y = COVERAGE, color = ANTIGEN), size = 2.5) +
  
  # Bar plot: scaled measles cases (mapped to right axis)
  geom_col(data = case, aes(x = Period, y = Cases_scaled, fill = "Measles Cases"), alpha = 0.6) +
  
  # Reference lines for WHO/National targets
  geom_hline(yintercept = 95, linetype = "dashed", color = "dodgerblue3", linewidth = 0.7) +
  geom_hline(yintercept = 90, linetype = "dashed", color = "orange", linewidth = 0.7) +
  
  # Axes: primary for vaccine coverage, secondary for absolute case count
  scale_y_continuous(
    name = "Vaccine Coverage (%)",
    breaks = seq(0, 100, by = 20),
    limits = c(0, 100),
    sec.axis = sec_axis(
      ~.,
      name = "Measles Cases (scaled)" #,
      #breaks = seq(0, 100, by = 20),
      #labels = function(x) paste0(round(x), "")
    )
  ) +
  scale_x_continuous(name = "Year", limits = c(2014, 2024), breaks = seq(2015, 2023, by = 2)) +
  scale_color_manual(values = c("MCV1" = "darkblue", "MCV2" = "goldenrod")) +
  scale_fill_manual(values = c("Measles Cases" = "red"), name = NULL) +
  
  # Facet by country with fixed scales for comparison
  facet_wrap(~ CODE, ncol = 4) +  # 4 columns for better display of many countries
  
  # Labels and theme
  labs(
    title = "EMRO Countries: MCV Coverage and Measles Cases (2015-2023)",
    subtitle = "Left axis:coverage (line), Right axis:cases, scaled 0-100% of country maximum)",
    color = "Vaccine",
    caption = "Dashed lines: Blue = WHO Target (95%), Orange = National Target (90%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title.y.left = element_text(color = "black"),
    axis.title.y.right = element_text(color = "deeppink4"),
    legend.position = "bottom",
    strip.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10),
    panel.spacing = unit(1, "lines")
  )

# Display the plot
print(plot_emro)

#In two groups 

