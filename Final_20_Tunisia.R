
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                             SEIR Model for Measles for Tunisia
# Age-stratified model with : a.routine vaccination compartments (S, I, R,V1S, V1I, V1R, V2S, V2I, V2R)
#                             b.SIA flows : S->SIAS, V1S->SIAS, S->SIAR, V1S->SIAR, R->SIAR, V1R->SIAR
#
# This Script is country specific. The Codes be slow , however, most of them are optimized to make the computation very faster
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#NB this model is a one year structured model, and contact matrix are in :3.1.EpiSignalDetetion_1_OneYearContact
aa <- Sys.time()# start timer
getwd()
setwd("D:/Placement project disk")
getwd()
#rm(list=ls())
# gc()
#dev.off()
# Packages
#save.image(file = "Final_1_Tunisia.RData")-->This is the saved enviroment to be more fast
#load("Final_1_Tunisia.RData")

pacman::p_load(
  deSolve,    # For ODE
  tidyverse,  # For data manipulation and visualization
  ggplot2,    # For plotting
  dplyr,      # For data manipulations
  patchwork,  # For combining plots
  plotly,     # For inter-actives plots
  httr,       # For API requests
  foreach,    # For parallel computation
  doParallel, # For parallel computation
  future,     # For parallel computation
  furrr,      # For parallel computation
  Matrix,     # For parallel computation
  bigmemory,  # For big data computation
  dtplyr,     # For Speed 
  fastmap,    # Ultra-fast key-value storage
  collapse    # Faster Data transformation
)

# 1. Demographic parameters~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1.a.Age structure (101 groups: 0,1, 100+)
(age_groups <- c(as.character(0:99), "100+"))
# Number of age groups
(n_age <- length(age_groups))

#1.b.Population structure (Tunisia) / 1 year age group
Population_emro_2023 <- read_csv("Population_emro_2023_1yearage.csv")
table(Population_emro_2023$Country)
(Tunisia_pop<-as.data.frame(Population_emro_2023|>
                            filter( Year=="2023")|>
                            filter(Country=="Tunisia")))
#Population structure
Tunisia_pop 
names(Tunisia_pop)
#Population structure in thousands 

(Tunisia_pop_in_thousands<-as.data.frame(Tunisia_pop|>
                                         mutate(Population_age=Population_age*1000)))
#1.c. Visualization of Tunisia's population structure
pacman::p_load(ggplot2,dplyr,plotly)
names(Tunisia_pop_in_thousands)
(pyramyd<-ggplot(Tunisia_pop_in_thousands, aes(x = Age_Category, y = Population_age)) +
    geom_col(fill = "steelblue") +
    labs(title = "Tunisia's population structure (2023)",
         x = "Age", y = "Population") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
)
#ggplotly(pyramyd)
# Shorten  the name
popstruc<-Tunisia_pop_in_thousands
#1.e.Number of age groups 
(A<-n_age<-length(Tunisia_pop_in_thousands[,6]))
#1.f. births by age of mother
(popbirth <- read.csv("3.U.1.Birth_1year_emro.csv", header = TRUE))
names(popbirth)
table(popbirth$Country)
popbirth<-popbirth|>
  filter(Country=="Tunisia")|>
  filter(Year==2023)
#popbirth <- popbirth[ , !(names(popbirth) %in% "X")]
(pyramyd<-ggplot(popbirth, aes(x = Age, y = Birth)) +
    geom_col(fill = "blue") +
    labs(title = "Birth in Tunisia (2023)",
         x = "Age", y = "Births") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
)
names(popbirth)

# convert from 1000s per 1 year period to per person per day
group_durations_years <- c(rep(1, n_age))  # total of 22 groups
group_durations_days <- group_durations_years * 365
(popbirth[, 6] <-  1000*popbirth[, 6] / (group_durations_days * popstruc[, 6] * 365))# I will need to make sure based on the two first age groups(not 5 years)
#1.g.natural mortality per person per year
(mortality <- read.csv("3.U.1.EMRO_mortality_by_age_group_1yearage.csv", header = TRUE))
names(mortality)
table(mortality$Country)
(popmort<-as.data.frame(mortality|>filter(Country=="Tunisia")|>
                          filter(Year==2023)))
(pyramyd<-ggplot(popmort , aes(x =Age , y = Percentage)) +
    geom_col(fill = "red") +
    labs(title = "Mortality in Tunisia (2023)",
         x = "Age", y = "Deaths per 1000 pop") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
)
popmort
# I convert pop mort from 1000s per 1 year period to per person per day,(taking account of the first two age groups)
(group_durations_years <- rep(1, n_age))  # total of 101 groups
(group_durations_days <- group_durations_years * 365)
names(popmort)
names(popstruc)
popmort[, 5] <- 1000 * popmort[, 5] / (group_durations_days * popstruc[, 6])
mort <- popmort[, 5]
#1.h.Contact matrix  and visualization
(m_contact_1y <- as.matrix(read.csv("3.U.1.contact_Tunisia_1y.csv")))
dim(m_contact_1y)

for(i in 1:n_age){
  for(j in 1:n_age){
    m_contact_1y[i,j]<-m_contact_1y[i,j]/25
  }
}
colnames(m_contact_1y ) <-c(as.character(0:99), "100+")
rownames(m_contact_1y ) <- c(as.character(0:99), "100+")
m_contact_1y 
#Visualization of my contact matrix
pacman::p_load(ggplot2,reshape2)
#data frame
#df <- melt(m_contact_1y )
df <- reshape2::melt(m_contact_1y)
colnames(df) <- c("Contactee", "Contactor", "Contacts")
#plot contact matrix
(p<-ggplot(df, aes(x = Contactor, y = Contactee, fill = Contacts)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "red") +
    theme_minimal() +
    labs(title = "Contact Matrix Heatmap for Tunisia")
)
#(p_plotly <- ggplotly(p))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#           1.i. Aging
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Number of age groups
A <-n_age<- 101 # (0,...100+)
# Duration in days for each age 
#(duration_years)
(duration_years <- rep(1, A ))  # Here i change because  i want to use one year age group
(duration_days <- duration_years * 365)   # in days

#ageing vector: 1/duration for each group
(ageing_rates <- 1 / duration_days)
# Ageing matrix
ageing <- matrix(0, nrow = A, ncol = A)
for (i in 1:(A - 1)) {
  ageing[i, i] <- -ageing_rates[i]       # Leaving rate from current group
  ageing[i + 1, i] <- ageing_rates[i]    # Entering rate to next group
}
# Last age (100+) does not age out, stays at 0
ageing[A, A] <- 0
# Ageing matrix
ageing

#Aging (ricardo course)
(dd <- seq(1:A) / seq(1:A))# A=n_age
(ageing <- t(diff(diag(dd), lag = 1) / (1 * 365.25)))
(ageing <- cbind(ageing, 0 * seq(1:A)))    # no ageing from last compartment

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   2.Vaccination coverage 2000-2023 and 2023-2030
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#MCV coverage :2000-2023
pacman::p_load(readxl,dplyr,tidyverse)
cov<- as.data.frame(read_excel("3.3.Measles vaccination coverage countries 2025-28-03 12-18 UTC.xlsx"))
write.csv(cov,"3.3.Coverage_all_countries.csv",row.names=FALSE)
coverage<-read.csv("3.3.Coverage_all_countries.csv",header=TRUE)
#Let me select Tunisia and WHO/UNICEF estimated coverage
table(coverage$NAME)
coverage<-coverage|>
  filter(COVERAGE_CATEGORY=="WUENIC")|>
  filter(NAME=="Tunisia")
#I will need to select antigens 
MCV1_Tunisia<-coverage|>
  filter(ANTIGEN=="MCV1")
MCV2_Tunisia<-coverage|>
  filter(ANTIGEN=="MCV2")
print(MCV2_Tunisia)
table(MCV2_Tunisia$YEAR)
#I will need the coverage only  
#Secondly,  i will reverse the order in order to have 2000-2023 instead 2023-2000 
cov_MCV1_Tunisia<-rev(MCV1_Tunisia[,11])
cov_MCV2_Tunisia<-rev(MCV2_Tunisia[,11])
cov_MCV1_Tunisia
cov_MCV2_Tunisia
length(cov_MCV1_Tunisia)
length(cov_MCV2_Tunisia)
#We assumed that NA coverage  correspond to 0 coverage
cov_MCV1_Tunisia[is.na(cov_MCV1_Tunisia)] <- 0
cov_MCV2_Tunisia[is.na(cov_MCV2_Tunisia)] <- 0
#In % 
cov_MCV1<-cov_MCV1_Tunisia/100
cov_MCV2<-cov_MCV2_Tunisia/100
cov_MCV1
cov_MCV2
#Most recent coverage (2023)
last_MCV1 <- tail(cov_MCV1, 1)
last_MCV2<- tail(cov_MCV2, 1)
last_MCV1
last_MCV2
#3.Vaccination and measles parameters
parameters <- list(
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #  A.Vaccination parameters                                                  #
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Vaccine efficacy for each dose
  # MCV1 
  cov_MCV1= cov_MCV1,# annual coverage first dose 
  eff1_a = 0.9,           # Efficacy in susceptible
  eff1_b = 0.2,           # Efficacy in infected
  eff1_c = 1,             # Efficacy in recovered
  #MCV1 schedule
  start_MCV1 = 9*30,       # 9 months in days   (0*365: if we want annual rate)
  end_MCV1   = 12*30,      # 12 months in days  (1*365: if we want annual rate)
  # MCV2 
  cov_MCV2= cov_MCV2,# annual coverage second dose 
  eff2_a = 0.98,          # Efficacy in susceptible
  eff2_b = 0.3,           # Efficacy in infected
  eff2_c = 1.00,          # Efficacy in recovered
  #Vaccine schedule
  start_MCV2 = 15*30,      # 15 months in days (0*365: if we want annual rate)
  end_MCV2   = 18*30,      # 18 months in days (1*365: if we want annual rate)
  # SIA  
  eff3_a = 0.9,           # Efficacy in susceptible
  eff3_b = 0.3,           # Efficacy in infected
  eff3_c = 1.00,          # Efficacy in recovered
  # SIA parameters
  sia_cov= 0,
  sia_cycle = 3*365,      # Period between 2 SIAs
  sia_duration = 2*30,    # SIA campaign duration in days
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #     3.B. Measles's parameters                                              @
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Transmission parameters
  p = 0.094,#0.047*2 ,          # Probability of infection given contact
  sigma = 1/21,                 # Incubation rate (1/days)
  gamma = 1/21,                 # Recovery rate (1/days)
  amp  = 8,                      # amplitude of seasonal variation                                                    # amplitude of seasonal variation
  phi = 2,                      # month of peak in seasonal variation
  RO = 16,                      #  Basic Reproduction Number
  report = 8/10,                #Proportion of reported cases
  ph = 10/100 ,                  # 10% hospitalization rate (assumed)
  gamma_h = 1/20,              # 5 days average hospital stay but 21 days to develop immunity
  CFR_h <- 2.9/100,               # CFR at hospital
  mort_h = c(rep(2*CFR_h, 6), rep(CFR_h, n_age - 6)),  # Case Fatality Rate per day at hospital
  # Model structure
  age_groups = age_groups,
  m_contact_1y = m_contact_1y ,
  n_age = n_age
)
#checks
parameters$cov_MCV1
parameters$cov_MCV2
#lengths
length(parameters$cov_MCV1)
length(parameters$cov_MCV2)
# All additional parameters
# 2023 coverage
parameters$last_MCV1<-tail(parameters$cov_MCV1,1)
parameters$last_MCV2<-tail(parameters$cov_MCV2,1)
parameters$last_MCV1
parameters$last_MCV2
# Time horizon
(parameters$horizon<-2035-2023) # 12 years
# Coverage for new coming 12 years (2023-2035): Baseline maintain the 2023 coverage
(new_MCV1_values <- rep(parameters$last_MCV1, parameters$horizon))
(new_MCV2_values <- rep(parameters$last_MCV2, parameters$horizon))
# New cov_MCV1
parameters$cov_MCV1 <- c(cov_MCV1,new_MCV1_values)
parameters$cov_MCV2 <- c(cov_MCV2,new_MCV2_values)
#Checks
parameters$cov_MCV1
parameters$cov_MCV1
length(parameters$cov_MCV1)# This need to be 36 :i.e 2000-2035
length(parameters$cov_MCV2)
#Parameters on covid 19
parameters$covid_19_declaration_year<-2020
parameters$covid_19_end_year<-2023
parameters$simulation_start<-2000
(parameters$covid_19_start<-(as.numeric(parameters$covid_19_declaration_year)-as.numeric(parameters$simulation_start)))
(parameters$covid_19_end<- (as.numeric(parameters$covid_19_end_year)-as.numeric(parameters$simulation_start)))
parameters$social_distancing_start <- 20*365
parameters$social_distancing_end <-   23*365
parameters$social_distancing_effect <- 0.20
#Let's  assume the coverage diminished during covid-19 by 40%
#MCV1
parameters$cov_MCV1
parameters$cov_MCV1[21:24]<-parameters$cov_MCV1[21:24]*0.6# 21 is 2020
parameters$cov_MCV1
#MCV2
parameters$cov_MCV2
parameters$cov_MCV2[21:24]<-parameters$cov_MCV2[21:24]*0.6# 21 is 2020
parameters$cov_MCV2
str(parameters$cov_MCV2)
# Let me have a vector of routine vaccination rate
#routine vaccination rates (per day) |----->continuous for MCV1 and MCV2
(parameters$rate1_vec<-rep(0,length(parameters$cov_MCV1)))
(parameters$rate2_vec<-rep(0,length(parameters$cov_MCV2)))

(parameters$rate1_vec <- -log(1 - parameters$cov_MCV1)/365)
(parameters$rate2_vec <- -log(1 - parameters$cov_MCV2)/365)
#Rate of receiving SIA
(parameters$r_3<--log(1-parameters$sia_cov)/parameters$sia_duration)
#checks
#Indices for compartments 
Sindex    <- 1:n_age                         # Susceptible
Eindex    <- (1*n_age+1):(2*n_age)           # Exposed  unvaccinated
Iindex    <- (2*n_age+1):(3*n_age)           # Infectious
Rindex    <- (3*n_age+1):(4*n_age)           # Recovered

V1Sindex  <- (4*n_age+1):(5*n_age)           # 1 dose - Susceptible
V1Eindex  <- (5*n_age+1):(6*n_age)           # 1 dose - Infectious
V1Iindex  <- (6*n_age+1):(7*n_age)           # 1 dose:- Exposed after 1 dose  failure
V1Rindex  <- (7*n_age+1):(8*n_age)           # 1 dose - Recovered

V2Sindex  <- (8*n_age+1):(9*n_age)           # 2 doses - Susceptible
V2Eindex  <- (9*n_age+1):(10*n_age)           # 2 doses  - Exposed after second failure
V2Iindex  <- (10*n_age+1):(11*n_age)           # 2 doses - Infectious
V2Rindex  <- (11*n_age+1):(12*n_age)           # 2 doses - Recovered

SIASindex <- (12*n_age+1):(13*n_age)          # SIA - Susceptible
SIAEindex <- (13*n_age+1):(14*n_age)          # SIA - Exposed after SIA failure
SIAIindex <- (14*n_age+1):(15*n_age)         # SIA - Infectious
SIARindex <- (15*n_age+1):(16*n_age)         # SIA - Recovered
#Hospitalization compartments
H0index   <- (16*n_age+1):(17*n_age)         # Hospitalized unvaccinated
H1index   <- (17*n_age+1):(18*n_age)         # Hospitalized 1-dose
H2index   <- (18*n_age+1):(19*n_age)         # Hospitalized 2-dose
H3index   <- (19*n_age+1):(20*n_age)         # Hospitalized SIA
# Cumulative mortality counters
D0index   <- (20*n_age+1):(21*n_age)         # Deaths from H0 (unvaccinated)
D1index   <- (21*n_age+1):(22*n_age)         # Deaths from H1 (1-dose)
D2index   <- (22*n_age+1):(23*n_age)         # Deaths from H2 (2-dose)
D3index   <- (23*n_age+1):(24*n_age)         # Deaths from H3 (SIA)
DTotalindex <- (24*n_age+1):(25*n_age)       # Total deaths from all compartments
# Wasted dose counters indices
W1index   <- (25*n_age+1):(26*n_age)         # Wasted MCV1 doses
W2index   <- (26*n_age+1):(27*n_age)         # Wasted MCV2 doses  
W3index   <- (27*n_age+1):(28*n_age)         # Wasted SIA doses
WTotalindex <-(28*n_age+1):(29*n_age)        # Total wasted doses
#Initial conditions
ageindcase <- 20

aci <- floor((ageindcase / 5) + 2)
initP <- Tunisia_pop_in_thousands[,6]
sum(initP)
#initS <- Tunisia_pop_in_thousands[,6]
initE <-   0* Tunisia_pop_in_thousands[,6]
initI <-   0* Tunisia_pop_in_thousands[,6]
cases_allcountries_1 <- read_excel("3.3.data.cases_allcountries_2.xlsx")
names(cases_allcountries_1)
table(cases_allcountries_1$Location)
(cases_allcountries_1<-cases_allcountries_1|>
    filter(Location=="Tunisia")|>
    filter(Period==2000)) #Initial cases in 2000 ( if 2000 cases were not reported)
(cases_2000<-as.numeric(cases_allcountries_1$Value)*0.1)# 10% are infectious
initI[aci] <- 1
#Distribution of cases accross age groups (2000)
initI[1]<-0.1*cases_2000
initI[3]<-0.3*cases_2000
initI[4]<-0.2*cases_2000
initI[5]<-0.1*cases_2000
initI[6]<-0.05*cases_2000
initI[7]<-0.05*cases_2000
sum(initI)
Pop<-Tunisia_pop_in_thousands[,6]
initR<-0 * Tunisia_pop_in_thousands[,6]
initR[31:101] <- 0.5 * Pop[31:101]#People born before 1970 may have natural immunity only (no vaccine) 
initR[7:30] <-  0.1 * Pop[7:30]   #People born after 1970 may have both (natural and vaccine induced immunity):~10% of natural immunity. The average age of infection is ~ 6 years old
initV1S <- 0 * Tunisia_pop_in_thousands[,6]
initV1E <- 0 * Tunisia_pop_in_thousands[,6] # added here for my SEIR structure
initV1I <- 0 * Tunisia_pop_in_thousands[,6]
initV1R<-0 * Tunisia_pop_in_thousands[,6]
initV1R[2:30] <- 0.02* Pop[2:30]  # 2% of the population had a vaccine induced immunity( Here we assumed a low vaccination coverage before 2000)
initV2S <- 0 * Tunisia_pop_in_thousands[,6]
initV2E <- 0 * Tunisia_pop_in_thousands[,6] # added
initV2I <- 0 * Tunisia_pop_in_thousands[,6]
initV2R <- 0 * Tunisia_pop_in_thousands[,6]
initV2R[2:30] <- 0.01* Pop[2:30] #Here we assumed a low coverage of MCV2 than MCV1
initSIAS <-0 * Tunisia_pop_in_thousands[,6]
initSIAE <-0 * Tunisia_pop_in_thousands[,6] # added
initSIAI <-0 * Tunisia_pop_in_thousands[,6]
initSIAR <-0 * Tunisia_pop_in_thousands[,6]
#Hospitalization initial conditions 
initH0 <- 0 * Tunisia_pop_in_thousands[,6]
initH1 <- 0 * Tunisia_pop_in_thousands[,6]
initH2 <- 0 * Tunisia_pop_in_thousands[,6]
initH3 <- 0 * Tunisia_pop_in_thousands[,6]
#Cumulative mortality initial conditions
initD0 <- 0 * Tunisia_pop_in_thousands[,6]
initD1 <- 0 * Tunisia_pop_in_thousands[,6]
initD2 <- 0 * Tunisia_pop_in_thousands[,6]
initD3 <- 0 * Tunisia_pop_in_thousands[,6]
initDTotal <- 0 * Tunisia_pop_in_thousands[,6]
# Wasted dose initial conditions 
initW1 <- 0 * Tunisia_pop_in_thousands[,6]      #  MCV1 doses
initW2 <- 0 * Tunisia_pop_in_thousands[,6]      #  MCV2 doses  
initW3 <- 0 * Tunisia_pop_in_thousands[,6]      #  SIA doses
initWTotal <- 0 * Tunisia_pop_in_thousands[,6]  #  Total wasted doses
initS <- Tunisia_pop_in_thousands[,6] - (initE+initI + initR + initV1S + initV1E + initV1I + initV1R + 
                                         initV2S + initV2E + initV2I + initV2R + initSIAS + initSIAE + initSIAI + initSIAR +
                                         initH0 + initH1 + initH2 + initH3)
# Note: Wasted doses and Deaths are cumulative counters, so they don't reduce the susceptible population

#Y vector: Initial conditions
Y <- c(initS,initE, initI, initR, initV1S, initV1E,initV1I, initV1R, initV2S,initV2E, initV2I, initV2R, 
       initSIAS,initSIAE, initSIAI, initSIAR, initH0, initH1, initH2, initH3,
       initD0, initD1, initD2, initD3, initDTotal,
       initW1, initW2, initW3, initWTotal)
#Check initial conditions
sum(Y)
sum(popstruc[,6])
#
(p_susceptible<-sum(Y)/sum(popstruc[,6]))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Model function
Tunisia_model <- function(t, Y, parameters) {
  with(as.list(c(Y, parameters)),
       {
         S <- Y[Sindex]
         E <- Y[Eindex]
         I <- Y[Iindex]
         R <- Y[Rindex]
         
         #Vaccination: first dose
         V1S <- Y[V1Sindex]
         V1E <- Y[V1Eindex]
         V1I <- Y[V1Iindex]
         V1R <- Y[V1Rindex]
         
         #Vaccination: second dose
         V2S <- Y[V2Sindex]
         V2E <- Y[V2Eindex]
         V2I <- Y[V2Iindex]
         V2R <- Y[V2Rindex]
         
         #Vaccination : Catch-up dose (SIA)
         SIAS <- Y[SIASindex]
         SIAE <- Y[SIAEindex]
         SIAI <- Y[SIAIindex]
         SIAR <- Y[SIARindex]
         
         #Hospitalization 
         H0 <- Y[H0index]
         H1 <- Y[H1index]
         H2 <- Y[H2index]
         H3 <- Y[H3index]
         
         #Deaths by vaccination status
         D0 <- Y[D0index]
         D1 <- Y[D1index]
         D2 <- Y[D2index]
         D3 <- Y[D3index]
         DTotal <- Y[DTotalindex]
         
         #Wasted doses 
         W1 <- Y[W1index]
         W2 <- Y[W2index]
         W3 <- Y[W3index]
         WTotal <- Y[WTotalindex]
         
         # Total population by age (excluding death counters since they're cumulative)
         N <- S + E + I + R + V1S + V1E + V1I + V1R + V2S + V2E + V2I + V2R + 
           SIAS + SIAE + SIAI + SIAR + H0 + H1 + H2 + H3
         
         # Force of infection by age group
         # Each compartment should be a vector of length n_age
         total_infectious <- I + V1I + V2I + SIAI + H0 + H1 + H2 + H3
         seas <- 1 # +amp*cos(2*pi*(t-phi/12))  # Seasonal forcing (litterature review is controversall on seasolity reason why we diced to remove it)
         
         social_distancing <- ifelse(t > social_distancing_start &
                                       t < social_distancing_end,
                                     social_distancing_effect, 1)
         
         # This handle division by zero
         infectious_proportion <- ifelse(N > 0, total_infectious/N, 0)
         lambda <- social_distancing*seas * p * (m_contact_1y %*% infectious_proportion)
         
         # Births
         b1 <- sum(popbirth[, 6] * N)
         births <- rep(0, n_age)
         births[1] <- b1
         
         # Vaccination parameters
         year_index <- floor(t / 365) + 1  # Year index starting from 1
         idx <- pmin(year_index, length(parameters$rate1_vec))  # Keep within bounds
         rate1 <- parameters$rate1_vec[idx]
         rate2 <- parameters$rate2_vec[idx]
         
         # Age eligibility for routine vaccination
         p1 <- rep(0, n_age)
         p1[1] <- 1  # MCV1 for first year [9-12 months]
         p2 <- rep(0, n_age)
         p2[2] <- 1  # MCV2 for second year [15-18 months]
         
         # SIA implementation
         sia_target_ages <- 6:15
         pc_1 <- pc_2 <- pc_3 <- rep(0, n_age)
         pc_1[sia_target_ages] <- 1
         pc_2[sia_target_ages] <- 1  
         pc_3[sia_target_ages] <- 1
         
         # Schedule for SIA (Pulse)
         time_in_cycle <- t %% sia_cycle
         sia_active <- (time_in_cycle < sia_duration) * 1
         # SIA starting after 2025 (25 years from start year 2000)
         rate3 <- ifelse(((t > 25*365) & sia_active), r_3, 0)
         
         #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         #                   SEIR ODE System 
         #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         
         # Susceptible (S)
         dS <- births - 
           lambda * S -                                       # S to E (infection)
           p1 * rate1 * S * (1 - eff1_a) -                   # S to V1S
           p1 * rate1 * S * eff1_a -                          # S to V1R
           pc_1 * rate3 * S * (1 - eff3_a) -                 # S to SIAS
           pc_1 * rate3 * S * eff3_a -                       # S to SIAR
           mort * S + ageing %*% S
         
         # Exposed (E) - unvaccinated
         dE <- lambda * S -                                       # S to E (infection)
           sigma * E -                                        # E to I (becoming infectious)
           mort * E + ageing %*% E
         
         # Infected (I) - unvaccinated
         dI <- sigma * E -                                        # E to I (becoming infectious)
           gamma * I * (1 - ph) -                            # I to R
           ph * I -                                          # I to H0
           mort * I + ageing %*% I
         
         # Recovered (R) - unvaccinated
         dR <- gamma * I * (1 - ph) +                            # I to R
           gamma_h * H0 -                                    # H0 to R
           p1 * rate1 * R * eff1_c -                         # R to V1R
           pc_1 * rate3 * R * eff3_c -                       # R to SIAR
           mort * R + ageing %*% R
         
         # V1S (Susceptible after 1 dose)
         dV1S <- p1 * rate1 * S * (1 - eff1_a) -                # vaccination from S
           lambda * V1S -                                  # V1S to V1E (infection)
           p2 * rate2 * V1S * (1 - eff2_a) -              # V1S to V2S
           p2 * rate2 * V1S * eff2_a -                    # V1S to V2R
           pc_2 * rate3 * V1S * (1 - eff3_a) -            # V1S to SIAS
           pc_2 * rate3 * V1S * eff3_a -                  # V1S to SIAR
           mort * V1S + ageing %*% V1S
         
         # V1E (Exposed after 1 dose)
         dV1E <- lambda * V1S -                                  # infection from V1S
           sigma * V1E -                                   # V1E to V1I (becoming infectious)
           p2 * rate2 * V1E * (1 - eff2_a) -              # V1E to V2E
           p2 * rate2 * V1E * eff2_a -                    # V1E to V2R
           pc_2 * rate3 * V1E * (1 - eff3_a) -            # V1E to SIAE
           pc_2 * rate3 * V1E * eff3_a -                  # V1E to SIAR
           mort * V1E + ageing %*% V1E
         
         # V1I (Infected after 1 dose)
         dV1I <- sigma * V1E -                                   # V1E to V1I (becoming infectious)
           gamma * V1I * (1 - ph) -                       # V1I to V1R
           ph * V1I -                                     # V1I to H1
           mort * V1I + ageing %*% V1I
         
         # V1R (Recovered/Immune after 1 dose)
         dV1R <- p1 * rate1 * S * eff1_a +                      # vaccination from S
           p1 * rate1 * R * eff1_c +                      # vaccination from R
           gamma * V1I * (1 - ph) +                       # recovery from V1I
           gamma_h * H1 -                                 # recovery from H1
           p2 * rate2 * V1R * eff2_c -                    # V1R to V2R
           pc_2 * rate3 * V1R * eff3_c -                  # V1R to SIAR
           mort * V1R + ageing %*% V1R
         
         # V2S (Susceptible after 2 doses)
         dV2S <- p2 * rate2 * V1S * (1 - eff2_a) -              # vaccination from V1S
           lambda * V2S -                                  # V2S to V2E (infection)
           pc_3 * rate3 * V2S * (1 - eff3_a) -            # V2S to SIAS
           pc_3 * rate3 * V2S * eff3_a -                  # V2S to SIAR
           mort * V2S + ageing %*% V2S
         
         # V2E (Exposed after 2 doses)
         dV2E <- p2 * rate2 * V1E * (1 - eff2_a) +              # vaccination from V1E
           lambda * V2S -                                  # infection from V2S
           sigma * V2E -                                   # V2E to V2I (becoming infectious)
           pc_3 * rate3 * V2E * (1 - eff3_a) -            # V2E to SIAE
           pc_3 * rate3 * V2E * eff3_a -                  # V2E to SIAR
           mort * V2E + ageing %*% V2E
         
         # V2I (Infected after 2 doses)
         dV2I <- sigma * V2E -                                   # V2E to V2I (becoming infectious)
           gamma * V2I * (1 - ph) -                       # V2I to V2R
           ph * V2I -                                     # V2I to H2
           mort * V2I + ageing %*% V2I
         
         # V2R (Recovered/Immune after 2 doses)
         dV2R <- gamma * V2I * (1 - ph) +                       # recovery from V2I
           gamma_h * H2 +                                 # recovery from H2
           p2 * rate2 * V1S * eff2_a +                    # vaccination from V1S
           p2 * rate2 * V1E * eff2_a +                    # vaccination from V1E
           p2 * rate2 * V1R * eff2_c -                    # vaccination from V1R
           pc_3 * rate3 * V2R * eff3_c -                  # V2R to SIAR
           mort * V2R + ageing %*% V2R
         
         # SIAS (Susceptible after SIA)
         dSIAS <- pc_1 * rate3 * S * (1 - eff3_a) +             # SIA from S
           pc_2 * rate3 * V1S * (1 - eff3_a) +           # SIA from V1S
           pc_3 * rate3 * V2S * (1 - eff3_a) -           # SIA from V2S
           lambda * SIAS -                                # SIAS to SIAE (infection)
           mort * SIAS + ageing %*% SIAS
         
         # SIAE (Exposed after SIA)
         dSIAE <- pc_2 * rate3 * V1E * (1 - eff3_a) +           # SIA from V1E
           pc_3 * rate3 * V2E * (1 - eff3_a) +           # SIA from V2E
           lambda * SIAS -                                # infection from SIAS
           sigma * SIAE -                                 # SIAE to SIAI (becoming infectious)
           mort * SIAE + ageing %*% SIAE
         
         # SIAI (Infected after SIA)
         dSIAI <- sigma * SIAE -                                 # SIAE to SIAI (becoming infectious)
           gamma * SIAI * (1 - ph) -                     # SIAI to SIAR
           ph * SIAI -                                    # SIAI to H3
           mort * SIAI + ageing %*% SIAI
         
         # SIAR (Recovered/Immune after SIA)
         dSIAR <- gamma * SIAI * (1 - ph) +                     # recovery from SIAI
           gamma_h * H3 +                                # recovery from H3
           pc_1 * rate3 * S * eff3_a +                   # S to SIAR
           pc_2 * rate3 * V1S * eff3_a +                 # V1S to SIAR
           pc_2 * rate3 * V1E * eff3_a +                 # V1E to SIAR
           pc_3 * rate3 * V2S * eff3_a +                 # V2S to SIAR
           pc_3 * rate3 * V2E * eff3_a +                 # V2E to SIAR
           pc_1 * rate3 * R * eff3_c +                   # R to SIAR
           pc_2 * rate3 * V1R * eff3_c +                 # V1R to SIAR
           pc_3 * rate3 * V2R * eff3_c -                 # V2R to SIAR
           mort * SIAR + ageing %*% SIAR
         
         # Hospitalization compartments
         # H0 (Hospitalized unvaccinated)
         dH0 <- ph * I -                                         # I to H0
           gamma_h * H0 -                                   # H0 to R
           mort_h * H0 -                                    # death (goes to D0)
           mort * H0 + ageing %*% H0                        # general mortality + aging
         
         # H1 (Hospitalized 1-dose)
         dH1 <- ph * V1I -                                       # V1I to H1
           gamma_h * H1 -                                   # H1 to V1R
           mort_h * H1 -                                    # death (goes to D1)
           mort * H1 + ageing %*% H1                        # general mortality + aging
         
         # H2 (Hospitalized 2-dose)
         dH2 <- ph * V2I -                                       # V2I to H2
           gamma_h * H2 -                                   # H2 to V2R
           mort_h * H2 -                                    # death (goes to D2)
           mort * H2 + ageing %*% H2                        # general mortality + aging
         
         # H3 (Hospitalized SIA)
         dH3 <- ph * SIAI -                                      # SIAI to H3
           gamma_h * H3 -                                   # H3 to SIAR
           mort_h * H3 -                                    # death (goes to D3)
           mort * H3 + ageing %*% H3                        # general mortality + aging
         
         # Cumulative mortality counters 
         # D0 (Deaths from unvaccinated)
         dD0 <- mort_h * H0 +                                    # hospital deaths from H0
           mort * S +                                       # general deaths from S
           mort * E +                                       # general deaths from E
           mort * I +                                       # general deaths from I
           mort * R -                                       # general deaths from R
           0 * ageing %*% D0                                # no aging for death counters
         
         # D1 (Deaths from 1-dose vaccinated)
         dD1 <- mort_h * H1 +                                    # hospital deaths from H1
           mort * V1S +                                     # general deaths from V1S
           mort * V1E +                                     # general deaths from V1E
           mort * V1I +                                     # general deaths from V1I
           mort * V1R -                                     # general deaths from V1R
           0 * ageing %*% D1                                # no aging for death counters
         
         # D2 (Deaths from 2-dose vaccinated)
         dD2 <- mort_h * H2 +                                    # hospital deaths from H2
           mort * V2S +                                     # general deaths from V2S
           mort * V2E +                                     # general deaths from V2E
           mort * V2I +                                     # general deaths from V2I
           mort * V2R -                                     # general deaths from V2R
           0 * ageing %*% D2                                # no aging for death counters
         
         # D3 (Deaths from SIA vaccinated)
         dD3 <- mort_h * H3 +                                    # hospital deaths from H3
           mort * SIAS +                                    # general deaths from SIAS
           mort * SIAE +                                    # general deaths from SIAE
           mort * SIAI +                                    # general deaths from SIAI
           mort * SIAR -                                    # general deaths from SIAR
           0 * ageing %*% D3                                # no aging for death counters
         
         # Total deaths 
         dDTotal <- mort_h * (H0 + H1 + H2 + H3) +                    # all hospital deaths
           mort * (S + E + I + R + V1S + V1E + V1I + V1R + V2S + V2E + V2I + V2R + 
                     SIAS + SIAE + SIAI + SIAR + H0 + H1 + H2 + H3) - # all general deaths
           0 * ageing %*% DTotal                                      # no aging for death counters
         
         # Wasted dose counters
         # W1 (Wasted MCV1 doses)
         dW1 <- p1 * rate1 * R * eff1_c +                       # MCV1 given to R (already immune)
           0 * ageing %*% W1                                    # no aging for counters
         
         # W2 (Wasted MCV2 doses)
         dW2 <- p2 * rate2 * V1R * eff2_c +                     # MCV2 given to V1R (already immune)
           0 * ageing %*% W2                                    # no aging for counters
         
         # W3 (Wasted SIA doses)
         dW3 <- pc_1 * rate3 * R * eff3_c +                     # SIA given to R (already immune)
           pc_2 * rate3 * V1R * eff3_c +                   # SIA given to V1R (already immune)
           pc_3 * rate3 * V2R * eff3_c +                   # SIA given to V2R (already immune)
           0 * ageing %*% W3                                # no aging for counters
         
         # WTotal (Total wasted doses)
         dWTotal <- p1 * rate1 * R * eff1_c +                   # All MCV1 wasted doses
           p2 * rate2 * V1R * eff2_c +                 # All MCV2 wasted doses
           pc_1 * rate3 * R * eff3_c +                 # All SIA wasted doses (R)
           pc_2 * rate3 * V1R * eff3_c +               # All SIA wasted doses (V1R)
           pc_3 * rate3 * V2R * eff3_c +               # All SIA wasted doses (V2R)
           0 * ageing %*% WTotal                        # no aging for counters
         
         # Return the rate of change
         list(c(dS, dE, dI, dR, dV1S, dV1E, dV1I, dV1R, dV2S, dV2E, dV2I, dV2R, 
                dSIAS, dSIAE, dSIAI, dSIAR, dH0, dH1, dH2, dH3,
                dD0, dD1, dD2, dD3, dDTotal,
                dW1, dW2, dW3, dWTotal))
         
       }
  )
}
# Model simulation parameters
start <- 2000
end <- 2035
period <- end - start
times <- seq(0, 35*365, by = 1)
#Run the model
start_time <- Sys.time()# start timer
output_Tunisia <- ode(y = Y,times = times,func = Tunisia_model,parms = parameters,method = "lsoda")
end_time <- Sys.time()# End timer
(duration <- end_time - start_time)
# I.Process results_Tunisia
results_Tunisia <- as.data.frame(output_Tunisia)
# Column names (including hospitalization and wasted doses compartments and death counters
compartment_names <- c("S", "E", "I", "R", "V1S", "V1E", "V1I", "V1R", "V2S", "V2E", "V2I", "V2R", 
                       "SIAS", "SIAE", "SIAI", "SIAR", "H0", "H1", "H2", "H3",
                       "D0", "D1", "D2", "D3", "DTotal",
                       "W1", "W2", "W3", "WTotal")

col_names <- c("time")
for(comp in compartment_names) {
  for(age in age_groups) {
    col_names <- c(col_names, paste0(comp, "_", age))
  }
}
colnames(results_Tunisia) <- col_names
names(results_Tunisia)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# II.IMMUNITY PROFIL: FUNCTIONAL IMMUNITY /IMMUITY GAPS ASSESSMENT
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#6.Verification of the population over time
#Total population at each time point
total_pop<- rowSums(results_Tunisia[, 2:(20*n_age)])  # This will exclude time column
# Check conservation: Here the population is conserved if the birth=deaths
#                   : The population is not conserved as births and deaths  are not equal(we used realistic country and age specific births and deaths) 
#Initial population
(initial_pop <- sum(Y))
# Time 
tim<-results_Tunisia[,1]
#Data frame for plotting
P_t<-as.data.frame(cbind(tim,total_pop,initial_pop))
#Population variations
(a<-round(((tail(P_t$total_pop,1)-initial_pop)/initial_pop)*1000,2))# Checks
#Packages for visualization
pacman::p_load(ggplot2,ggtext)
#Visualization of the initial population and total population over time
# Plot 
#Population dynamic (with annotation)
ggplot(P_t, aes(x = tim, y = total_pop)) +
  geom_hline(aes(yintercept = initial_pop), linetype = "dashed", color = "red") +
  geom_line(color = "blue") +
  labs(
    title = "Total population over time in Tunisia",
    subtitle = "<span style='color:red;'>Red dashed</span>: Initial population, <span style='color:blue;'>Blue line</span>: Total population over time",
    x = "Time (days)",
    y = "Total population (M)"
  ) +
  scale_y_continuous(
    labels = scales::label_number(scale = 1e-6, accuracy = 0.1, suffix = " M")
  ) +
  annotate(
    "text",
    x = max(P_t$tim) * 0.5,
    y = max(P_t$initial_pop) * 0.1,
    label = paste0("Relative change: ", a, "%"),
    size = 3,
    color = "darkgreen"
  ) +
  theme_minimal() +
  theme(
    plot.subtitle = ggtext::element_markdown(size = 11)
  )
#Prob.susceptible
#Susceptible
results_Tunisia$susceptible <- rowSums( 
  results_Tunisia[, (Sindex + 1)])+
  rowSums(results_Tunisia[, (V1Sindex + 1)])+
  rowSums(results_Tunisia[, (V2Sindex + 1)])+
  rowSums(results_Tunisia[, (SIASindex + 1)])
prop_sus_t<-(results_Tunisia$susceptible/total_pop)
P_t<-as.data.frame(cbind(tim,total_pop,initial_pop,prop_sus_t))
P_t$prop_initial_sus<-sum(initS)/initial_pop

pacman::p_load(ggplot2,ggtext)
ggplot(P_t, aes(x = tim, y = prop_sus_t)) +
  geom_hline(aes(yintercept = prop_initial_sus), linetype = "dashed", color = "red") +
  geom_line(color = "blue") +
  labs(
    title = "Total susceptible over time in Tunisia",
    subtitle = "<span style='color:red;'>Red dashed</span>: Initial susceptible, <span style='color:blue;'>Blue line</span>: susceptible population over time",
    x = "Time (days)",
    y = "Prop. of susceptible"
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.1),
    labels = scales::label_number(scale = 1, accuracy = 0.1, suffix = " ")
  ) +
  theme_minimal() +
  theme(
    plot.subtitle = ggtext::element_markdown(size = 11)
  )
#
# 7.Reshape results_Tunisia for data analysis
names(results_Tunisia)
time_index <- which(names(results_Tunisia) == "time")
# Select "time" and next columns 20*101
20*101
selected <- results_Tunisia[, c(time_index, (time_index + 1):(time_index + 20*n_age))]
column_names <- names(selected)
results_Tunisia_0<-results_Tunisia[column_names]
#8.Long format of model results_Tunisia
#results_Tunisia<-results_Tunisia[,1:((16*n_age)+1)] #12 are SIR, 4 others are H0,H1,H2,H3
pacman::p_load(data.table) # This package will allow us to reshape dataset faster
results_Tunisia_0<- as.data.table(results_Tunisia_0)
#Long format using melt to be faster
results_Tunisia_long <- melt(
  results_Tunisia_0,
  id.vars = "time",
  variable.name = "variable",
  value.name = "value"
)[, c("compartment", "age_group") := tstrsplit(variable, "_", fixed = TRUE)
][, `:=`(
  age_group = factor(age_group, levels = age_groups),
  time_years = time / 365,
  time_days = time
)][, variable := NULL]
#check the structure of the long format
names(results_Tunisia_long)
head(results_Tunisia_long)
table(results_Tunisia_long$compartment)
#Here i will need to speed the visualization 
pacman::p_load(data.table,ggplot2,scales)
# Data.table 
setDT(results_Tunisia_long)
#Transformations in one data.table 
results_Tunisia_long_optimized <- results_Tunisia_long[, `:=`(
  # Status column using fcase 
  Status = fcase(
    compartment %in% c("I", "V1I", "V2I", "SIAI","H0", "H1", "H2","H3"), "Infectious",
    compartment %in% c("E", "V1E", "V2E", "SIAE"), "Exposed",
    compartment %in% c("R", "V1R", "V2R", "SIAR"), "Immune",
    compartment %in% c("S", "V1S", "V2S", "SIAS"), "Susceptible",
    default = compartment
  ),
  # Vaccine column
  Vaccine = fcase(
    compartment %in% c("S","E", "I", "R"), "0-Dose",
    compartment %in% c("V1S","V1E", "V1I", "V1R"), "1-Dose",
    compartment %in% c("V2S","V2E", "V2I", "V2R"), "2-Doses",
    compartment %in% c("SIAS","SIAE", "SIAI", "SIAR"), "SIA-Dose",
    compartment %in% c("H0", "H1", "H2","H3"), "Hospital",
    default = compartment
  )
)]
# Proportions in one step
results_Tunisia_long_optimized[as.integer(age_group) <= 101, `:=`(
  total_by_age = sum(value), 
  proportion = value / sum(value) * 100
), by = age_group]

#Immunity column 
results_Tunisia_long_optimized[, Immunity := fcase(
  Status == "Immune" & Vaccine == "0-Dose", "Natural",
  Status == "Immune" & Vaccine == "1-Dose", "MCV1", 
  Status == "Immune" & Vaccine == "2-Doses", "MCV2",
  Status == "Immune" & Vaccine == "SIA-Dose", "SIA",
  default = "Non-immune"
)]
# Levels efficiently
results_Tunisia_long_optimized[, Vaccine := factor(Vaccine, levels = c("0-Dose", "1-Dose", "2-Doses", "SIA-Dose"))]
# FUNCTIONAL IMMUNITY VISUALIZATION

# Pre-aggregate first
Final_Tunisia_immunity_summary <- results_Tunisia_long_optimized[
  as.integer(age_group) <= 100,
  .(total_value = sum(value)), 
  by = .(age_group, Immunity)
][, proportion := total_value / sum(total_value) * 100, by = age_group]

# Plot-aggregated data
names(Final_Tunisia_immunity_summary)
dim(Final_Tunisia_immunity_summary)
immunity1 <- ggplot(Final_Tunisia_immunity_summary, 
                    aes(x = age_group, y = proportion, fill = Immunity)) +
  geom_col(position = "stack") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8)) +
  labs(title = "Functional Immunity in Tunisia", x = "Age", y = "Percent of the population") +
  scale_fill_manual(values = c("MCV1" = "#00c4aa", "MCV2" = "#e573f3", 
                               "Natural" = "#00b3f4", "SIA" = "#9b9602", "Non-immune" = "#fc726c"))
print(immunity1)
# Save
ggsave("Final_immunity_Tunisia_stacked_plot.png", plot = immunity1, width = 10, height = 10)
#Data to be used to compare countries
write.csv(Final_Tunisia_immunity_summary, "Final_Tunisia_immunity_summary.csv", row.names = FALSE)
# Fast file writing
fwrite(results_Tunisia_long_optimized, "Final_FI_Tunisia.csv")
# Fast image saving
ggsave("3.U.1.Immunity_stack_Tunisia_0.png", plot = immunity1, 
       width = 10, height = 10, dpi = 300)

# Pre-aggregate data  (for dodged plot)
immunity2_summary <- results_Tunisia_long_optimized[
  as.integer(age_group) <= 101,
  .(total_value = sum(value)), 
  by = .(age_group, Immunity)
][, proportion := total_value / sum(total_value) * 100, by = age_group]

# Dodged plot
immunity2 <- ggplot(immunity2_summary, 
                    aes(x = age_group, y = proportion, fill = Immunity)) +
  geom_col(position = "dodge", width = 0.8) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8)) +
  labs(title = "Functional Immunity in Tunisia", x = "Age", y = "Percent of the population") +
  scale_fill_manual(values = c("MCV1" = "#00c4aa", "MCV2" = "#e573f3", 
                               "Natural" = "#00b3f4", "SIA" = "#9b9602", "Non-immune" = "#fc726c"))
print(immunity2)
# Save
ggsave("Final_immunity_Tunisia_dodged_plot.png", plot = immunity2, width = 10, height = 10)

#
#Age
table(results_Tunisia_long$age_group)
#Compartments
table(results_Tunisia_long$compartment)
(results_Tunisia_long1 <- results_Tunisia_long %>%
    mutate(
      Status = case_when(
        compartment %in% c("I", "V1I", "V2I", "SIAI","H0", "H1", "H2","H3") ~ "Infectious",
        compartment %in% c("E", "V1E", "V2E", "SIAE") ~ "Exposed",
        compartment %in% c("R", "V1R", "V2R", "SIAR") ~ "Immune",
        compartment %in% c("S", "V1S", "V2S", "SIAS") ~ "Susceptible",
        TRUE ~ compartment
      ),
      Vaccine = case_when(
        compartment %in% c("S","E", "I", "R") ~ "0-Dose",
        compartment %in% c("V1S","V1E", "V1I", "V1R") ~ "1-Dose",
        compartment %in% c("V2S","V2E", "V2I", "V2R") ~ "2-Doses",
        compartment %in% c("SIAS","SIAE", "SIAI", "SIAR") ~ "SIA-Dose",
        compartment %in% c("H0", "H1", "H2","H3") ~ "Hospital",
        TRUE ~ compartment
      )
    ))

head(results_Tunisia_long1)
results_Tunisia_long1
names(results_Tunisia_long1)
head(results_Tunisia_long1)
#Vaccination status 
vaccination_status<-ggplot(results_Tunisia_long1 |>
                             filter(age_group %in% levels(age_group)[1:50]),
                           aes(x = age_group, y = value, fill = Vaccine)) +
  geom_col(position = "stack") +
  labs(
    title = "Vaccination Status of Population in Tunisia",
    x = "Age Group",
    y = "Individuals (in Millions)",
    fill = "Status"
  ) +
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, accuracy = 1, suffix = " M")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#print(vaccination_status)
#Vaccination status at the end of simulation
end <- max(results_Tunisia_long1$time_days)
#
final_vaccination <- results_Tunisia_long1 %>%
  filter(time_days == end) %>%
  filter(age_group %in% levels(age_group)[1:100])|>
  group_by(Vaccine) %>%
  summarise(total = sum(value), .groups = "drop") %>%
  mutate(percentage = total / sum(total) * 100)
#print(final_vaccination)
#Visualisation of vaccination status at the end of simulation
bar_positions <- barplot(final_vaccination$percentage,
                         names.arg = final_vaccination$Vaccine,
                         col = rainbow(nrow(final_vaccination)),
                         main = "Vaccination status at the end of simmulation",
                         xlab = "",
                         ylab = "Percentage",
                         las = 2,  # make x labels vertical if long
                         ylim = c(0, max(final_vaccination$percentage) * 1.1))

#Percentage labels above bars
text(x = bar_positions,
     y = final_vaccination$percentage,
     labels = paste0(round(final_vaccination$percentage, 2), "%"),
     pos = 3,   # above the bar
     cex = 0.8,
     col = "black")
parameters
#Checks
table(results_Tunisia_long1$Status)
table(results_Tunisia_long1$Vaccine)
#Packages for visualization
pacman::p_load(ggplot2,cowplot,ggExtra)
#gridExtra::grid.arrange(immunity1, immunity2)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 6. Visualization  of infectious 
# Color palette for age 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
age_colors <- colorRampPalette(c("#FF6B6B", "#06D6A0","#FFD166", "#118AB2", "#073B4C"))(n_age)
names(age_colors) <- age_groups

# Measles cases across age groups
infectious_bar_plot1_a <- ggplot(results_Tunisia_long1 %>%
                                   filter(Status == "Infectious" & age_group %in% age_groups[1:21])) +  # Note: limited to first 3 groups
  geom_bar(aes(x = age_group, y = value, fill =age_group), stat = "identity") +
  scale_fill_manual(values = age_colors) +
  labs(x = "Age Group", y = "Number Infectious",
       title = "Measles cases by age group in Tunisia") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#print(infectious_bar_plot1_a)
#ggsave("3.U.1.infectious_bar_plot1_a.png",infectious_bar_plot1_a, width=10, height=6)
#By vaccination status
infectious_bar_plot1_b <- ggplot(results_Tunisia_long1 %>%
                                   filter(Status == "Infectious",
                                          Vaccine != "Hospital",  # Exclude "Hospitalized"
                                          age_group %in% age_groups[1:10])) +  
  geom_bar(aes(x = age_group, y = value, fill = age_group), stat = "identity") +
  scale_fill_manual(values = age_colors) +
  facet_wrap(~Vaccine, scales = "free_y", ncol = 2) +
  labs(x = "Age group", y = "Number Infectious",
       title = "Measles cases and vaccination status in Tunisia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(infectious_bar_plot1_b)
#ggsave("3.U.1.infectious_bar_plot1_b.png",infectious_bar_plot1_b, width=10, height=6)
pacman::p_load(gridExtra)
#grid.arrange(infectious_bar_plot1_a,infectious_bar_plot1_b,ncol=2)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#         III.SENARIOS ANALYSIS : OUTBREAK CONTROL ASSESSMENT
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#              1. scenario_0 : Baseline (Routine immunization maintained at 2023 level)                                         @
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#Rt-0
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     RO and Next Generation Matrix approach. 
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#O. Diekmann1, J. A. P. Heesterbeek2,*, and M. G. Roberts3
#The construction of next-generation matrices for compartmental epidemic models
results_Tunisia<-as.data.frame(output_Tunisia)

calculate_effective_Rt_parallel <- function(results_Tunisia, parameters) {
  library(parallel)
  
  # Parameters
  p <- parameters$p
  gamma <- parameters$gamma
  sigma <- parameters$sigma
  m_contact_1y <- parameters$m_contact_1y
  n_age <- parameters$n_age
  seas <- 1
  
  idx <- function(offset) ((offset * n_age + 2):((offset + 1) * n_age + 1))#if offset = 0 ,--> 2:102 = S
  #Here is the f(x) to extract compartments
  get_compartment <- function(offset) {
    as.matrix(results_Tunisia[, idx(offset)])
  }
  
  # Extraction of  compartments
  S    <- get_compartment(0)
  E    <- get_compartment(1)
  I    <- get_compartment(2)
  R    <- get_compartment(3)
  V1S  <- get_compartment(4)
  V1E  <- get_compartment(5)
  V1I  <- get_compartment(6)
  V1R  <- get_compartment(7)
  V2S  <- get_compartment(8)
  V2E  <- get_compartment(9)
  V2I  <- get_compartment(10)
  V2R  <- get_compartment(11)
  SIAS <- get_compartment(12)
  SIAE <- get_compartment(13)
  SIAI <- get_compartment(14)
  SIAR <- get_compartment(15)
  H0   <- get_compartment(16)
  H1   <- get_compartment(17)
  H2   <- get_compartment(18)
  H3   <- get_compartment(19)
  
  # Susceptible and total
  N_total <- S + E + I + R + V1S + V1E + V1I + V1R + V2S + V2E + V2I + V2R + SIAS +SIAE + SIAI + SIAR + H0 + H1 + H2 + H3
  susceptible <- S + V1S + V2S + SIAS
  prop_sus <- ifelse(N_total > 0, susceptible / N_total, 0)
  
  n_time <- nrow(prop_sus)
  
  #  NGM matrice precomputation
  NGM_list <- lapply(1:n_time, function(t) {
    p * seas * (1 / gamma) * m_contact_1y %*% diag(prop_sus[t, ])
  })
  
  # Cluster
  n_cores <- max(1, parallel::detectCores() - 1)
  cl <- makeCluster(n_cores)
  clusterExport(cl, varlist = c("NGM_list"), envir = environment())
  
  # Parallel eigenvalue computation
  R_eff <- parLapply(cl, NGM_list, function(mat) {
    max(Re(eigen(mat, only.values = TRUE)$values))
  })
  
  stopCluster(cl)
  
  # Return result
  data.frame(
    time_days = results_Tunisia$time,
    time_years = results_Tunisia$time / 365,
    R_eff = unlist(R_eff)
  )
}
start_time <- Sys.time()
Rt_data_Tunisia <- calculate_effective_Rt_parallel(results_Tunisia, parameters)
end_time <- Sys.time()
end_time - start_time
# Plot 1: Basic R_eff over time
(Rt_basic_plot <- ggplot(Rt_data_Tunisia, aes(x = time_years, y = R_eff)) +
    geom_line(color = "#E63946", linewidth = 1.2) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "black", alpha = 0.7, linewidth = 1) +
    geom_vline(xintercept = seq(0, 3, by = 2), linetype = "dotted", color = "blue", alpha = 0.5) +
    annotate("text", x = 2, y = max(Rt_data_Tunisia$R_eff) * 0.9, 
             label = "Rt = 1 (Epidemic threshold)", color = "black", size = 3.5) +
    annotate("text", x = 1, y = max(Rt_data_Tunisia$R_eff) * 0.8, 
             label = "SIA campaigns", color = "blue", size = 3) +
    labs(
      x = "Time (Years)",
      y = "Rt",
      title = "Rt over time : Baseline",
      #subtitle = "Impact of continuous and pulse vaccination on transmission potential"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12)
    ))
(max_Reff<-max(Rt_data_Tunisia$R_eff))
# Adjusted: time_years to actual calendar years starting at 2000
Rt_data_Tunisia$year <- 2000 + Rt_data_Tunisia$time_years
# Plot 2: R_eff with epidemic threshold zones
(Rt_zone_plot <- ggplot(Rt_data_Tunisia, aes(x = time_years, y = R_eff)) +
    # Colored background zones
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 1, ymax = Inf, 
             fill = "#ffcccc", alpha = 0.3) +  # Epidemic zone (R > 1)
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 1, 
             fill = "#ccffcc", alpha = 0.3) +  # Control zone (R < 1)
    
    geom_line(color = "#E63946", size = 1.3) +
    geom_point(data = Rt_data_Tunisia[seq(1, nrow(Rt_data_Tunisia), by = 50), ], 
               color = "#E63946", size = 1) +
    geom_hline(yintercept = 1, linetype = "solid", color = "black", size = 1) +
    geom_vline(xintercept = seq(0, 2, by = 2), linetype = "dashed", color = "blue", alpha = 0.6) +
    
    # Text annotations
    annotate("text", x = max(Rt_data_Tunisia$time_years) * 0.8, y = max(Rt_data_Tunisia$R_eff) * 0.95, 
             label = "Epidemic Zone:(Rt > 1)", color = "#8B0000", size = 4, fontface = "bold") +
    annotate("text", x = max(Rt_data_Tunisia$time_years) * 0.8, y = 0.5, 
             label = "Control Zone:(Rt < 1)", color = "#006400", size = 4, fontface = "bold") +
    annotate("text", x = 4, y = max(Rt_data_Tunisia$R_eff) * 0.1, 
             label = "SIA campaigns every 2 years", color = "blue", size = 3, angle = 0) +
    
    labs(
      x = "Time (Years)",
      y = "Rt",
      title = "Epidemic control assessment in Tunisia :Baseline",
      #subtitle = "Green:disease control (Rt < 1)|Red :Potential epidemic spread (Rt > 1)"
    ) +
    scale_y_continuous(limits = c(0, max(Rt_data_Tunisia$R_eff) * 1.1)) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11),
      panel.grid.minor = element_blank()
    ))
#Year in thousand
Rt_data_Tunisia$year <- as.numeric(2000 + Rt_data_Tunisia$time_years)

Rt_zone_plot <- ggplot(Rt_data_Tunisia, aes(x = year, y = R_eff)) +
  scale_x_continuous(breaks = seq(2000, max(Rt_data_Tunisia$year), by = 5)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 1, ymax = Inf, 
           fill = "#ffcccc", alpha = 0.3) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 1, 
           fill = "#ccffcc", alpha = 0.3) +
  geom_line(color = "#E63946", size = 1.3) +
  geom_point(data = Rt_data_Tunisia[seq(1, nrow(Rt_data_Tunisia), by = 50), ], 
             aes(x = year, y = R_eff), color = "#E63946", size = 1) +
  geom_hline(yintercept = 1, linetype = "solid", color = "black", size = 1) +
  geom_vline(xintercept = seq(2023, max(Rt_data_Tunisia$year), by = 3), 
             linetype = "dashed", color = "blue", alpha = 0.6) +
  annotate("text", x = 2020, y = max(Rt_data_Tunisia$R_eff) * 0.95, 
           label = "Epidemic Zone: (Rt > 1)", color = "#8B0000", size = 4, fontface = "bold") +
  annotate("text", x = 2020, y = 0.5, 
           label = "Control Zone: (Rt < 1)", color = "#006400", size = 4, fontface = "bold") +
  annotate("text", x = 2004, y = max(Rt_data_Tunisia$R_eff) * 0.1, 
           label = "SIA campaigns every 2 years", color = "blue", size = 3) +
  labs(
    x = "Year",
    y = "Rt",
    title = "Epidemic Control Assessment in Tunisia: Baseline"
  ) +
  scale_y_continuous(limits = c(0, max(Rt_data_Tunisia$R_eff) * 1.1)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    panel.grid.minor = element_blank()
  )

Rt_zone_plot

#plotly::ggplotly(Rt_zone_plot)
# Plot 3: R_eff distribution histogram
(Rt_hist_plot <- ggplot(Rt_data_Tunisia, aes(x = R_eff)) +
    geom_histogram(bins = 30, fill = "#118AB2", alpha = 0.7, color = "white") +
    geom_vline(xintercept = 1, linetype = "dashed", color = "#E63946", size = 1.2) +
    geom_vline(xintercept = mean(Rt_data_Tunisia$R_eff), linetype = "solid", color = "orange", size = 1) +
    annotate("text", x = 1.1, y = max(table(cut(Rt_data_Tunisia$R_eff, breaks = 30))) * 0.8,
             label = "Epidemic\nthreshold", color = "#E63946", size = 3.5, hjust = 0) +
    annotate("text", x = mean(Rt_data_Tunisia$R_eff) + 0.1, y = max(table(cut(Rt_data_Tunisia$R_eff, breaks = 30))) * 0.6,
             label = paste("Mean =", round(mean(Rt_data_Tunisia$R_eff), 2)), 
             color = "orange", size = 3.5, hjust = 0) +
    labs(
      x = "Rt",
      y = "Frequency",
      title = "Distribution of Rt values : Baseline",
      subtitle = paste("Proportion of time with Rt < 1:", 
                       round(100 * sum(Rt_data_Tunisia$R_eff < 1) / nrow(Rt_data_Tunisia), 1), "%")
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12)
    ))

# Plot 4: Combined plot showing R_eff and vaccination coverage
# a.vaccination coverage : Population level
vaccine_coverage_over_time <- results_Tunisia_long1 %>%
  filter(compartment %in% c("V1S","V1E","V1I", "V1R", "V2S","V2E", "V2I", "V2R", "SIAS","SIAE", "SIAI", "SIAR")) %>%
  group_by(time_days) %>%
  summarise(total_vaccinated = sum(value), .groups = "drop") %>%
  left_join(
    results_Tunisia_long1 %>%
      group_by(time_days) %>%
      summarise(total_population = sum(value), .groups = "drop"),
    by = "time_days"
  ) %>%
  mutate(
    vaccination_coverage = (total_vaccinated / total_population) * 100,
    time_years = time_days / 365
  )
names(vaccine_coverage_over_time)
#Coverage at specific ages 
target_ages <- c(0, 1, 6:15)# Vaccination targeted these ages
vaccine_coverage_over_time <- results_Tunisia_long1 %>%
  filter(age %in% target_ages,
         compartment %in% c("V1S","V1E" ,"V1I", "V1R", "V2S","V2E", "V2I", "V2R", "SIAS","SIAE", "SIAI", "SIAR")) %>%
  group_by(time_days) %>%
  summarise(total_vaccinated = sum(value), .groups = "drop") %>%
  
  # Total population in same age groups
  left_join(
    results_Tunisia_long1 %>%
      filter(age %in% target_ages) %>%
      group_by(time_days) %>%
      summarise(total_population = sum(value), .groups = "drop"),
    by = "time_days"
  ) %>%
  
  mutate(
    vaccination_coverage = (total_vaccinated / total_population) * 100,
    time_years = time_days / 365
  )
#Rt and vaccination data
combined_data <- Rt_data_Tunisia %>%
  left_join(vaccine_coverage_over_time, by = c("time_days", "time_years"))

# Dual-axis plot

# Fixed maximums
max_Rt <- max_Reff
max_coverage <- 100
scaling_factor <- max_Rt / max_coverage  # 0.2

# Plot with proper scaling
(Rt_coverage_plot <- ggplot(combined_data, aes(x = time_years)) +
    geom_line(aes(y = R_eff), color = "#E63946", size = 1.2) +
    geom_line(aes(y = vaccination_coverage * scaling_factor), color = "#118AB2", size = 1.2) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "black", alpha = 0.7) +
    geom_vline(xintercept = seq(1,3)*2, linetype = "dashed", color = "purple", alpha = 0.7) +
    scale_y_continuous(
      name = "Rt",
      limits = c(0, max_Rt),  # Primary axis limits
      sec.axis = sec_axis(
        transform = ~./scaling_factor,
        name = "Vaccination Coverage (%)"
      )
    ) +
    coord_cartesian(ylim = c(0, max_Rt)) +  # Ensures both scales match
    labs(
      x = "Time (Years)",
      title = "Rt and vaccination coverage over time in Tunisia"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.title.y.left = element_text(color = "#E63946"),
      axis.title.y.right = element_text(color = "#118AB2"),
      axis.text.y.left = element_text(color = "#E63946"),
      axis.text.y.right = element_text(color = "#118AB2")
    ))

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#  scenario-1 : Routine Immunization + Supplemental Activities (RI+SIA)
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
parameters_1<-parameters
parameters_1$sia_cov<-0.9
#SIA rate
parameters_1$r_3<--log(1-parameters_1$sia_cov)/parameters_1$sia_duration
start_time <- Sys.time()
output_Tunisia_scenario_1 <- ode(y = Y,times = times,func = Tunisia_model,parms = parameters_1,method = "lsoda")
end_time <- Sys.time()
(duration <- end_time - start_time)
# Process results_Tunisia
results_Tunisia_scenario_1 <- as.data.frame(output_Tunisia_scenario_1)
# Column names (including hospitalization and wasted doses compartments and death counters
compartment_names <- c("S", "E", "I", "R", "V1S", "V1E", "V1I", "V1R", "V2S", "V2E", "V2I", "V2R", 
                       "SIAS", "SIAE", "SIAI", "SIAR", "H0", "H1", "H2", "H3",
                       "D0", "D1", "D2", "D3", "DTotal",
                       "W1", "W2", "W3", "WTotal")

col_names <- c("time")
for(comp in compartment_names) {
  for(age in age_groups) {
    col_names <- c(col_names, paste0(comp, "_", age))
  }
}
colnames(results_Tunisia_scenario_1) <- col_names
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Epidemic control assessment  using Rt 
#Next Generation Matrix approach
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#O. Diekmann1, J. A. P. Heesterbeek2,*, and M. G. Roberts3
#Title: The construction of next-generation matrices for compartmental epidemic models

calculate_effective_Rt_parallel <- function(results_Tunisia_scenario_1, parameters_1) {
  library(parallel)
  
  # Parameters
  p <- parameters_1$p
  gamma <- parameters_1$gamma
  m_contact_1y <- parameters_1$m_contact_1y
  n_age <- parameters_1$n_age
  seas <- 1
  
  idx <- function(offset) ((offset * n_age + 2):((offset + 1) * n_age + 1))  # if offset = 0 --> 2:102 = S
  
  get_compartment <- function(offset) {
    as.matrix(results_Tunisia_scenario_1[, idx(offset)])
  }
  
  # Extraction of  compartments
  S    <- get_compartment(0)
  E    <- get_compartment(1)
  I    <- get_compartment(2)
  R    <- get_compartment(3)
  V1S  <- get_compartment(4)
  V1E  <- get_compartment(5)
  V1I  <- get_compartment(6)
  V1R  <- get_compartment(7)
  V2S  <- get_compartment(8)
  V2E  <- get_compartment(9)
  V2I  <- get_compartment(10)
  V2R  <- get_compartment(11)
  SIAS <- get_compartment(12)
  SIAE <- get_compartment(13)
  SIAI <- get_compartment(14)
  SIAR <- get_compartment(15)
  H0   <- get_compartment(16)
  H1   <- get_compartment(17)
  H2   <- get_compartment(18)
  H3   <- get_compartment(19)
  
  # Susceptible and total
  N_total <- S + E + I + R + V1S + V1E + V1I + V1R + V2S + V2E + V2I + V2R + SIAS +SIAE + SIAI + SIAR + H0 + H1 + H2 + H3
  susceptible <- S + V1S + V2S + SIAS
  prop_sus <- ifelse(N_total > 0, susceptible / N_total, 0)
  
  n_time <- nrow(prop_sus)
  
  # NGM matrix precomputation
  NGM_list <- lapply(1:n_time, function(t) {
    p * seas * (1 / gamma) * m_contact_1y %*% diag(prop_sus[t, ])
  })
  
  # Parallel computation
  n_cores <- max(1, parallel::detectCores() - 1)
  cl <- makeCluster(n_cores)
  clusterExport(cl, varlist = c("NGM_list"), envir = environment())
  
  R_eff <- parLapply(cl, NGM_list, function(mat) {
    max(Re(eigen(mat, only.values = TRUE)$values))
  })
  
  stopCluster(cl)
  
  # Return result
  data.frame(
    time_days = results_Tunisia_scenario_1$time,
    time_years = results_Tunisia_scenario_1$time / 365,
    R_eff = unlist(R_eff)
  )
}

#Rt data
#Timer
start_time <- Sys.time()# start
Rt_data_Tunisia_scenario_1 <- calculate_effective_Rt_parallel(results_Tunisia_scenario_1, parameters_1)
end_time <- Sys.time() #End
end_time - start_time  #Duration
# Plot 1: Basic R_eff over time
(Rt_basic_plot <- ggplot(Rt_data_Tunisia_scenario_1, aes(x = time_years, y = R_eff)) +
    geom_line(color = "#E63946", size = 1.2) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "black", alpha = 0.7, size = 1) +
    geom_vline(xintercept = seq(0, 3, by = 2), linetype = "dotted", color = "blue", alpha = 0.5) +
    annotate("text", x = 2, y = max(Rt_data_Tunisia_scenario_1$R_eff) * 0.9, 
             label = "Rt = 1 (Epidemic threshold)", color = "black", size = 3.5) +
    annotate("text", x = 1, y = max(Rt_data_Tunisia_scenario_1$R_eff) * 0.8, 
             label = "SIA campaigns", color = "blue", size = 3) +
    labs(
      x = "Time (Years)",
      y = "Rt",
      title = "Rt over time",
      
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12)
    ))
#
# Adjusted: time_years to actual calendar years starting at 2000
Rt_data_Tunisia_scenario_1$year <- 2000 + Rt_data_Tunisia_scenario_1$time_years

# Plot 2: R_eff with epidemic threshold zones
(Rt_zone_plot <- ggplot(Rt_data_Tunisia_scenario_1, aes(x = time_years, y = R_eff)) +
    # Colored background zones
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 1, ymax = Inf, 
             fill = "#ffcccc", alpha = 0.3) +  # Epidemic zone (R > 1)
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 1, 
             fill = "#ccffcc", alpha = 0.3) +  # Control zone (R < 1)
    
    geom_line(color = "#E63946", size = 1.3) +
    geom_point(data = Rt_data_Tunisia_scenario_1[seq(1, nrow(Rt_data_Tunisia_scenario_1), by = 50), ], 
               color = "#E63946", size = 1) +
    geom_hline(yintercept = 1, linetype = "solid", color = "black", size = 1) +
    geom_vline(xintercept = seq(0, 2, by = 2), linetype = "dashed", color = "blue", alpha = 0.6) +
    
    # Text annotations
    annotate("text", x = max(Rt_data_Tunisia_scenario_1$time_years) * 0.8, y = max(Rt_data_Tunisia_scenario_1$R_eff) * 0.95, 
             label = "Epidemic Zone:(Rt > 1)", color = "#8B0000", size = 2, fontface = "bold") +
    annotate("text", x = max(Rt_data_Tunisia_scenario_1$time_years) * 0.8, y = 0.5, 
             label = "Control Zone:(Rt < 1)", color = "#006400", size = 2, fontface = "bold") +
    annotate("text", x = 4, y = max(Rt_data_Tunisia_scenario_1$R_eff) * 0.1, 
             label = "SIA campaigns every 2 years", color = "blue", size = 3, angle = 0) +
    
    labs(
      x = "Time (Years)",
      y = "Rt",
      title = "Epidemic control assessment in Tunisia :SIA-RI",
      #subtitle = "Green:disease control (Rt < 1)|Red :Potential epidemic spread (Rt > 1)"
    ) +
    scale_y_continuous(limits = c(0, max(Rt_data_Tunisia_scenario_1$R_eff) * 1.1)) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11),
      panel.grid.minor = element_blank()
    ))
#Year in thousand
Rt_data_Tunisia_scenario_1$year <- as.numeric(2000 + Rt_data_Tunisia_scenario_1$time_years)

Rt_zone_plot <- ggplot(Rt_data_Tunisia_scenario_1, aes(x = year, y = R_eff)) +
  scale_x_continuous(breaks = seq(2000, max(Rt_data_Tunisia_scenario_1$year), by = 5)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 1, ymax = Inf, 
           fill = "#ffcccc", alpha = 0.3) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 1, 
           fill = "#ccffcc", alpha = 0.3) +
  geom_line(color = "#E63946", size = 1.3) +
  geom_point(data = Rt_data_Tunisia_scenario_1[seq(1, nrow(Rt_data_Tunisia_scenario_1), by = 50), ], 
             aes(x = year, y = R_eff), color = "#E63946", size = 1) +
  geom_hline(yintercept = 1, linetype = "solid", color = "black", size = 1) +
  geom_vline(xintercept = seq(2023, max(Rt_data_Tunisia_scenario_1$year), by = 3), 
             linetype = "dashed", color = "blue", alpha = 0.6) +
  annotate("text", x = 2020, y = max(Rt_data_Tunisia_scenario_1$R_eff) * 0.95, 
           label = "Epidemic Zone: (Rt > 1)", color = "#8B0000", size = 4, fontface = "bold") +
  annotate("text", x = 2020, y = 0.5, 
           label = "Control Zone: (Rt < 1)", color = "#006400", size = 4, fontface = "bold") +
  annotate("text", x = 2004, y = max(Rt_data_Tunisia_scenario_1$R_eff) * 0.1, 
           label = "SIA campaigns every 2 years", color = "blue", size = 3) +
  labs(
    x = "Year",
    y = "Rt",
    title = "Epidemic Control Assessment in Tunisia: SIA + RI"
  ) +
  scale_y_continuous(limits = c(0, max(Rt_data_Tunisia_scenario_1$R_eff) * 1.1)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    panel.grid.minor = element_blank()
  )

Rt_zone_plot
# Plot 3: R_eff distribution histogram
(Rt_hist_plot <- ggplot(Rt_data_Tunisia_scenario_1, aes(x = R_eff)) +
    geom_histogram(bins = 30, fill = "#118AB2", alpha = 0.7, color = "white") +
    geom_vline(xintercept = 1, linetype = "dashed", color = "#E63946", size = 1.2) +
    geom_vline(xintercept = mean(Rt_data_Tunisia_scenario_1$R_eff), linetype = "solid", color = "orange", size = 1) +
    annotate("text", x = 1.1, y = max(table(cut(Rt_data_Tunisia_scenario_1$R_eff, breaks = 30))) * 0.8,
             label = "Epidemic\nthreshold", color = "#E63946", size = 3.5, hjust = 0) +
    annotate("text", x = mean(Rt_data_Tunisia_scenario_1$R_eff) + 0.1, y = max(table(cut(Rt_data_Tunisia$R_eff, breaks = 30))) * 0.6,
             label = paste("Mean =", round(mean(Rt_data_Tunisia_scenario_1$R_eff), 2)), 
             color = "orange", size = 3.5, hjust = 0) +
    labs(
      x = "Rt",
      y = "Frequency",
      title = "Distribution of Rt values:RI+SIA",
      subtitle = paste("Proportion of time with Rt < 1:", 
                       round(100 * sum(Rt_data_Tunisia_scenario_1$R_eff < 1) / nrow(Rt_data_Tunisia_scenario_1), 1), "%")
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12)
    ))

# Plot 4: Combined plot showing R_eff and vaccination coverage
# a.vaccination coverage
vaccine_coverage_over_time <- results_Tunisia_long1 %>%
  filter(compartment %in% c("V1S","V1E", "V1I", "V1R", "V2S","V2E", "V2I", "V2R", "SIAS","SIAE", "SIAI", "SIAR")) %>%
  group_by(time_days) %>%
  summarise(total_vaccinated = sum(value), .groups = "drop") %>%
  left_join(
    results_Tunisia_long1 %>%
      group_by(time_days) %>%
      summarise(total_population = sum(value), .groups = "drop"),
    by = "time_days"
  ) %>%
  mutate(
    vaccination_coverage = (total_vaccinated / total_population) * 100,
    time_years = time_days / 365
  )
names(vaccine_coverage_over_time)
# Rt and vaccination data
combined_data <- Rt_data_Tunisia %>%
  left_join(vaccine_coverage_over_time, by = c("time_days", "time_years"))

# Dual-axis plot
(Rt_coverage_plot <- ggplot(combined_data, aes(x = time_years)) +
    geom_line(aes(y = R_eff), color = "#E63946", size = 1.2) +
    geom_line(aes(y = vaccination_coverage/40), color = "#118AB2", size = 1.2) +  # Scale down coverage for dual axis
    geom_hline(yintercept = 1, linetype = "dashed", color = "black", alpha = 0.7) +
    geom_vline(xintercept = 1*2, linetype = "dashed", color = "purple", alpha = 0.7)+
    geom_vline(xintercept = 2*2, linetype = "dashed", color = "purple", alpha = 0.7)+
    geom_vline(xintercept = 3*2, linetype = "dashed", color = "purple", alpha = 0.7)+
    # geom_vline(xintercept = 4*2, linetype = "dashed", color = "purple", alpha = 0.7)+
    # geom_vline(xintercept = 5*2, linetype = "dashed", color = "purple", alpha = 0.7)+
    # Dual y-axis
    scale_y_continuous(
      name = "Rt",
      sec.axis = sec_axis(~ . * 40, name = "Vaccination Coverage (%)")
    ) +
    
    labs(
      x = "Time (Years)",
      title = "Rt and  vaccination coverage over time in Tunisia",
      #subtitle = "Red line: Rt (left axis), Blue line: Vaccination coverage (right axis)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.title.y.left = element_text(color = "#E63946"),
      axis.title.y.right = element_text(color = "#118AB2"),
      axis.text.y.left = element_text(color = "#E63946"),
      axis.text.y.right = element_text(color = "#118AB2")
    ))
# Fixed maximums
max_Rt <- max_Reff
max_coverage <- 100
scaling_factor <- max_Rt / max_coverage  # 0.2

# Plot with proper scaling
(Rt_coverage_plot <- ggplot(combined_data, aes(x = time_years)) +
    geom_line(aes(y = R_eff), color = "#E63946", size = 1.2) +
    geom_line(aes(y = vaccination_coverage * scaling_factor), color = "#118AB2", size = 1.2) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "black", alpha = 0.7) +
    geom_vline(xintercept = seq(1,3)*2, linetype = "dashed", color = "purple", alpha = 0.7) +
    scale_y_continuous(
      name = "Rt",
      limits = c(0, max_Rt),  # Primary axis limits
      sec.axis = sec_axis(
        trans = ~./scaling_factor,
        name = "Vaccination Coverage (%)"
      )
    ) +
    coord_cartesian(ylim = c(0, max_Rt)) +  # Ensures both scales match
    labs(
      x = "Time (Years)",
      title = "Rt and vaccination coverage over time in Tunisia"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.title.y.left = element_text(color = "#E63946"),
      axis.title.y.right = element_text(color = "#118AB2"),
      axis.text.y.left = element_text(color = "#E63946"),
      axis.text.y.right = element_text(color = "#118AB2")
    ))
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#             scenario 2 : Routine immunization scaled up (RI scaled up)
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
parameters_2<-parameters
length(parameters_2$cov_MCV1)
parameters_2$cov_MCV1[26:36]<-0.95
parameters_2$cov_MCV2[26:36]<-0.95

parameters_2$rate1_vec <- -log(1 - parameters_2$cov_MCV1)/365
parameters_2$rate2_vec <- -log(1 - parameters_2$cov_MCV1)/365
end_time <- Sys.time()
output_Tunisia_scenario_2 <- ode(y = Y,times = times,func = Tunisia_model,parms = parameters_2,method = "lsoda")
end_time <- Sys.time()
(duration <- end_time - start_time)
# Process results_Tunisia
results_Tunisia_scenario_2 <- as.data.frame(output_Tunisia_scenario_2)
# Column names (including hospitalization and wasted doses compartments and death counters
compartment_names <- c("S", "E", "I", "R", "V1S", "V1E", "V1I", "V1R", "V2S", "V2E", "V2I", "V2R", 
                       "SIAS", "SIAE", "SIAI", "SIAR", "H0", "H1", "H2", "H3",
                       "D0", "D1", "D2", "D3", "DTotal",
                       "W1", "W2", "W3", "WTotal")

col_names <- c("time")
for(comp in compartment_names) {
  for(age in age_groups) {
    col_names <- c(col_names, paste0(comp, "_", age))
  }
}
colnames(results_Tunisia_scenario_2) <- col_names
#Rt-2
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Epidemic control assessment  using Rt 
#Next Genaration Matrix approach
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#O. Diekmann1, J. A. P. Heesterbeek2,*, and M. G. Roberts3
#Title: The construction of next-generation matrices for compartmental epidemic models

calculate_effective_Rt_parallel <- function(results_Tunisia_scenario_2, parameters_2) {
  library(parallel)
  
  # Parameters
  p <- parameters_2$p
  gamma <- parameters_2$gamma
  m_contact_1y <- parameters_2$m_contact_1y
  n_age <- parameters_2$n_age
  seas <- 1
  
  idx <- function(offset) ((offset * n_age + 2):((offset + 1) * n_age + 1))
  
  get_compartment <- function(offset) {
    as.matrix(results_Tunisia_scenario_2[, idx(offset)])
  }
  
  # Extraction of  compartments
  S    <- get_compartment(0)
  E    <- get_compartment(1)
  I    <- get_compartment(2)
  R    <- get_compartment(3)
  V1S  <- get_compartment(4)
  V1E  <- get_compartment(5)
  V1I  <- get_compartment(6)
  V1R  <- get_compartment(7)
  V2S  <- get_compartment(8)
  V2E  <- get_compartment(9)
  V2I  <- get_compartment(10)
  V2R  <- get_compartment(11)
  SIAS <- get_compartment(12)
  SIAE <- get_compartment(13)
  SIAI <- get_compartment(14)
  SIAR <- get_compartment(15)
  H0   <- get_compartment(16)
  H1   <- get_compartment(17)
  H2   <- get_compartment(18)
  H3   <- get_compartment(19)
  
  # Susceptible and total
  N_total <- S + E + I + R + V1S + V1E + V1I + V1R + V2S + V2E + V2I + V2R + SIAS +SIAE + SIAI + SIAR + H0 + H1 + H2 + H3
  susceptible <- S + V1S + V2S + SIAS
  prop_sus <- ifelse(N_total > 0, susceptible / N_total, 0)
  
  n_time <- nrow(prop_sus)
  
  # NGM matrix precomputation
  NGM_list <- lapply(1:n_time, function(t) {
    p * seas * (1 / gamma) * m_contact_1y %*% diag(prop_sus[t, ])
  })
  
  # Parallel computation
  n_cores <- max(1, parallel::detectCores() - 1)
  cl <- makeCluster(n_cores)
  clusterExport(cl, varlist = c("NGM_list"), envir = environment())
  
  R_eff <- parLapply(cl, NGM_list, function(mat) {
    max(Re(eigen(mat, only.values = TRUE)$values))
  })
  
  stopCluster(cl)
  
  # Result
  data.frame(
    time_days = results_Tunisia_scenario_2$time,
    time_years = results_Tunisia_scenario_2$time / 365,
    R_eff = unlist(R_eff)
  )
}

#  Rt data
start_time <- Sys.time()
Rt_data_Tunisia_scenario_2 <- calculate_effective_Rt_parallel(results_Tunisia_scenario_2,parameters_2)
end_time <- Sys.time()
end_time - start_time

# Plot 1: Basic R_eff over time
(Rt_basic_plot <- ggplot(Rt_data_Tunisia_scenario_2, aes(x = time_years, y = R_eff)) +
    geom_line(color = "#E63946", size = 1.2) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "black", alpha = 0.7, size = 1) +
    geom_vline(xintercept = seq(0, 3, by = 2), linetype = "dotted", color = "blue", alpha = 0.5) +
    annotate("text", x = 2, y = max(Rt_data_Tunisia_scenario_2$R_eff) * 0.9, 
             label = "Rt = 1 (Epidemic threshold)", color = "black", size = 3.5) +
    annotate("text", x = 1, y = max(Rt_data_Tunisia_scenario_2$R_eff) * 0.8, 
             label = "SIA campaigns", color = "blue", size = 3) +
    labs(
      x = "Time (Years)",
      y = "Rt",
      title = "Rt over time: Scenario 2: RI scaled up",
      #subtitle = "Impact of continuous and pulse vaccination on transmission potential"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12)
    ))
#
# Adjusted: time_years to actual calendar years starting at 2000
Rt_data_Tunisia_scenario_2$year <- 2000 + Rt_data_Tunisia_scenario_2$time_years

# Plot using calendar years on x-axis
Rt_basic_plot <- ggplot(Rt_data_Tunisia_scenario_2, aes(x = year, y = R_eff)) +
  geom_line(color = "#E63946", size = 1.2) +
  
  # Horizontal line at Rt = 1
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", alpha = 0.7, size = 1) +
  
  # Vertical lines for SIA campaigns every 2 years from 2000 to 2045
  geom_vline(xintercept = seq(2023, 2035, by = 2), linetype = "dotted", color = "blue", alpha = 0.5) +
  
  # Annotations
  annotate("text", x = 2005, y = max(Rt_data_Tunisia_scenario_2$R_eff) * 0.9, 
           label = "Rt = 1 (Epidemic threshold)", color = "black", size = 3.5) +
  annotate("text", x = 2005, y = max(Rt_data_Tunisia_scenario_2$R_eff) * 0.8, 
           label = "SIA campaigns", color = "blue", size = 3) +
  
  # Labels and theme
  labs(
    x = "Year",
    y = "Rt",
    title = "Effective Reproduction Number (Rt):RI scaled up"
    # subtitle = "Impact of continuous and pulse vaccination on transmission potential"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )

# Print the plot
Rt_basic_plot
#plotly::ggplotly(Rt_basic_plot)
# Plot 2: R_eff with epidemic threshold zones
(Rt_zone_plot <- ggplot(Rt_data_Tunisia_scenario_2, aes(x = time_years, y = R_eff)) +
    # Colored background zones
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 1, ymax = Inf, 
             fill = "#ffcccc", alpha = 0.3) +  # Epidemic zone (R > 1)
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 1, 
             fill = "#ccffcc", alpha = 0.3) +  # Control zone (R < 1)
    
    geom_line(color = "#E63946", size = 1.3) +
    geom_point(data = Rt_data_Tunisia_scenario_2[seq(1, nrow(Rt_data_Tunisia_scenario_2), by = 50), ], 
               color = "#E63946", size = 1) +
    geom_hline(yintercept = 1, linetype = "solid", color = "black", size = 1) +
    geom_vline(xintercept = seq(0, 2, by = 2), linetype = "dashed", color = "blue", alpha = 0.6) +
    
    # Text annotations
    annotate("text", x = max(Rt_data_Tunisia_scenario_2$time_years) * 0.8, y = max(Rt_data_Tunisia_scenario_2$R_eff) * 0.95, 
             label = "Epidemic Zone:(Rt > 1)", color = "#8B0000", size = 4, fontface = "bold") +
    annotate("text", x = max(Rt_data_Tunisia_scenario_2$time_years) * 0.8, y = 0.5, 
             label = "Control Zone:(Rt < 1)", color = "#006400", size = 4, fontface = "bold") +
    annotate("text", x = 4, y = max(Rt_data_Tunisia_scenario_2$R_eff) * 0.1, 
             label = "SIA campaigns every 2 years", color = "blue", size = 3, angle = 0) +
    
    labs(
      x = "Time (Years)",
      y = "Rt",
      title = "Epidemic control assessment in Tunisia :SIA-RI",
      #subtitle = "Green:disease control (Rt < 1)|Red :Potential epidemic spread (Rt > 1)"
    ) +
    scale_y_continuous(limits = c(0, max(Rt_data_Tunisia_scenario_2$R_eff) * 1.1)) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11),
      panel.grid.minor = element_blank()
    ))
#Year in thousand
Rt_data_Tunisia_scenario_2$year <- as.numeric(2000 + Rt_data_Tunisia_scenario_2$time_years)

Rt_zone_plot <- ggplot(Rt_data_Tunisia_scenario_2, aes(x = year, y = R_eff)) +
  scale_x_continuous(breaks = seq(2000, max(Rt_data_Tunisia_scenario_2$year), by = 5)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 1, ymax = Inf, 
           fill = "#ffcccc", alpha = 0.3) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 1, 
           fill = "#ccffcc", alpha = 0.3) +
  geom_line(color = "#E63946", size = 1.3) +
  geom_point(data = Rt_data_Tunisia_scenario_2[seq(1, nrow(Rt_data_Tunisia_scenario_2), by = 50), ], 
             aes(x = year, y = R_eff), color = "#E63946", size = 1) +
  geom_hline(yintercept = 1, linetype = "solid", color = "black", size = 1) +
  geom_vline(xintercept = seq(2023, max(Rt_data_Tunisia_scenario_2$year), by = 3), 
             linetype = "dashed", color = "blue", alpha = 0.6) +
  annotate("text", x = 2020, y = max(Rt_data_Tunisia_scenario_2$R_eff) * 0.95, 
           label = "Epidemic Zone: (Rt > 1)", color = "#8B0000", size = 4, fontface = "bold") +
  annotate("text", x = 2020, y = 0.5, 
           label = "Control Zone: (Rt < 1)", color = "#006400", size = 4, fontface = "bold") +
  annotate("text", x = 2004, y = max(Rt_data_Tunisia_scenario_2$R_eff) * 0.1, 
           label = "SIA campaigns every 2 years", color = "blue", size = 3) +
  labs(
    x = "Year",
    y = "Rt",
    title = "Epidemic Control Assessment in Tunisia: SIA-RI"
  ) +
  scale_y_continuous(limits = c(0, max(Rt_data_Tunisia_scenario_2$R_eff) * 1.1)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    panel.grid.minor = element_blank()
  )

Rt_zone_plot
#plotly::ggplotly(Rt_zone_plot)
# Plot 3: R_eff distribution histogram
(Rt_hist_plot <- ggplot(Rt_data_Tunisia_scenario_2, aes(x = R_eff)) +
    geom_histogram(bins = 30, fill = "#118AB2", alpha = 0.7, color = "white") +
    geom_vline(xintercept = 1, linetype = "dashed", color = "#E63946", size = 1.2) +
    geom_vline(xintercept = mean(Rt_data_Tunisia_scenario_2$R_eff), linetype = "solid", color = "orange", size = 1) +
    annotate("text", x = 1.1, y = max(table(cut(Rt_data_Tunisia_scenario_2$R_eff, breaks = 30))) * 0.8,
             label = "Epidemic\nthreshold", color = "#E63946", size = 3.5, hjust = 0) +
    annotate("text", x = mean(Rt_data_Tunisia_scenario_2$R_eff) + 0.1, y = max(table(cut(Rt_data_Tunisia_scenario_2$R_eff, breaks = 30))) * 0.6,
             label = paste("Mean =", round(mean(Rt_data_Tunisia_scenario_2$R_eff), 2)), 
             color = "orange", size = 3.5, hjust = 0) +
    labs(
      x = "Rt",
      y = "Frequency",
      title = "Distribution of Rt values",
      subtitle = paste("Proportion of time with Rt < 1:", 
                       round(100 * sum(Rt_data_Tunisia_scenario_2$R_eff < 1) / nrow(Rt_data_Tunisia_scenario_2), 1), "%")
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12)
    ))

# Plot 4: Combined plot showing R_eff and vaccination coverage
# a.vaccination coverage
vaccine_coverage_over_time <- results_Tunisia_long1 %>%
  filter(compartment %in% c("V1S","V1E", "V1I", "V1R", "V2S","V2E","V2I", "V2R", "SIAS","SIAE", "SIAI", "SIAR")) %>%
  group_by(time_days) %>%
  summarise(total_vaccinated = sum(value), .groups = "drop") %>%
  left_join(
    results_Tunisia_long1 %>%
      group_by(time_days) %>%
      summarise(total_population = sum(value), .groups = "drop"),
    by = "time_days"
  ) %>%
  mutate(
    vaccination_coverage = (total_vaccinated / total_population) * 100,
    time_years = time_days / 365
  )
names(vaccine_coverage_over_time)
# Rt and vaccination data
combined_data <- Rt_data_Tunisia_scenario_2 %>%
  left_join(vaccine_coverage_over_time, by = c("time_days", "time_years"))

# Dual-axis plot
(Rt_coverage_plot <- ggplot(combined_data, aes(x = time_years)) +
    geom_line(aes(y = R_eff), color = "#E63946", size = 1.2) +
    geom_line(aes(y = vaccination_coverage/40), color = "#118AB2", size = 1.2) +  # Scale down coverage for dual axis
    geom_hline(yintercept = 1, linetype = "dashed", color = "black", alpha = 0.7) +
    geom_vline(xintercept = 1*2, linetype = "dashed", color = "purple", alpha = 0.7)+
    geom_vline(xintercept = 2*2, linetype = "dashed", color = "purple", alpha = 0.7)+
    geom_vline(xintercept = 3*2, linetype = "dashed", color = "purple", alpha = 0.7)+
    # geom_vline(xintercept = 4*2, linetype = "dashed", color = "purple", alpha = 0.7)+
    # geom_vline(xintercept = 5*2, linetype = "dashed", color = "purple", alpha = 0.7)+
    # Dual y-axis
    scale_y_continuous(
      name = "Rt",
      sec.axis = sec_axis(~ . * 40, name = "Vaccination Coverage (%)")
    ) +
    
    labs(
      x = "Time (Years)",
      title = "Rt and  vaccination coverage over time in Tunisia",
      #subtitle = "Red line: Rt (left axis), Blue line: Vaccination coverage (right axis)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.title.y.left = element_text(color = "#E63946"),
      axis.title.y.right = element_text(color = "#118AB2"),
      axis.text.y.left = element_text(color = "#E63946"),
      axis.text.y.right = element_text(color = "#118AB2")
    ))
##
# Fixed maximums
max_Rt <- max_Reff
max_coverage <- 100
scaling_factor <- max_Rt / max_coverage  # 0.2

# Plot with proper scaling
(Rt_coverage_plot <- ggplot(combined_data, aes(x = time_years)) +
    geom_line(aes(y = R_eff), color = "#E63946", size = 1.2) +
    geom_line(aes(y = vaccination_coverage * scaling_factor), color = "#118AB2", size = 1.2) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "black", alpha = 0.7) +
    geom_vline(xintercept = seq(1,3)*2, linetype = "dashed", color = "purple", alpha = 0.7) +
    scale_y_continuous(
      name = "Rt",
      limits = c(0, max_Rt),  # Primary axis limits
      sec.axis = sec_axis(
        trans = ~./scaling_factor,
        name = "Vaccination Coverage (%)"
      )
    ) +
    coord_cartesian(ylim = c(0, max_Rt)) +  # Ensures both scales match
    labs(
      x = "Time (Years)",
      title = "Rt and vaccination coverage over time in Tunisia"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.title.y.left = element_text(color = "#E63946"),
      axis.title.y.right = element_text(color = "#118AB2"),
      axis.text.y.left = element_text(color = "#E63946"),
      axis.text.y.right = element_text(color = "#118AB2")
    ))
#Rt 3
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#               scenario_3 : RIS-SIA-5Y                                         @
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
parameters_3<-parameters
parameters_3$sia_cov<-0.9
parameters_3$sia_cycle<-5*365
#SIA rate
parameters_3$r_3<--log(1-parameters_3$sia_cov)/parameters_3$sia_duration

output_Tunisia_scenario_3 <- ode(y = Y,times = times,func = Tunisia_model,parms = parameters_3,method = "lsoda")
end_time <- Sys.time()
(duration <- end_time - start_time)
# Process results_Tunisia
results_Tunisia_scenario_3 <- as.data.frame(output_Tunisia_scenario_3)
# Column names (including hospitalization and wasted doses compartments and death counters
compartment_names <- c("S", "E", "I", "R", "V1S", "V1E", "V1I", "V1R", "V2S", "V2E", "V2I", "V2R", 
                       "SIAS", "SIAE", "SIAI", "SIAR", "H0", "H1", "H2", "H3",
                       "D0", "D1", "D2", "D3", "DTotal",
                       "W1", "W2", "W3", "WTotal")

col_names <- c("time")
for(comp in compartment_names) {
  for(age in age_groups) {
    col_names <- c(col_names, paste0(comp, "_", age))
  }
}
colnames(results_Tunisia_scenario_3) <- col_names

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Epidemic control assessment  using Rt 
#Next Generation Matrix approach
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#O. Diekmann1, J. A. P. Heesterbeek2,*, and M. G. Roberts3
#Title: The construction of next-generation matrices for compartmental epidemic models

calculate_effective_Rt_parallel <- function(results_Tunisia_scenario_3, parameters_3) {
  library(parallel)
  
  # Parameters
  p <- parameters_3$p
  gamma <- parameters_3$gamma
  m_contact_1y <- parameters_3$m_contact_1y
  n_age <- parameters_3$n_age
  seas <- 1
  
  idx <- function(offset) ((offset * n_age + 2):((offset + 1) * n_age + 1))
  
  get_compartment <- function(offset) {
    as.matrix(results_Tunisia_scenario_3[, idx(offset)])
  }
  
  # Extraction of  compartments
  S    <- get_compartment(0)
  E    <- get_compartment(1)
  I    <- get_compartment(2)
  R    <- get_compartment(3)
  V1S  <- get_compartment(4)
  V1E  <- get_compartment(5)
  V1I  <- get_compartment(6)
  V1R  <- get_compartment(7)
  V2S  <- get_compartment(8)
  V2E  <- get_compartment(9)
  V2I  <- get_compartment(10)
  V2R  <- get_compartment(11)
  SIAS <- get_compartment(12)
  SIAE <- get_compartment(13)
  SIAI <- get_compartment(14)
  SIAR <- get_compartment(15)
  H0   <- get_compartment(16)
  H1   <- get_compartment(17)
  H2   <- get_compartment(18)
  H3   <- get_compartment(19)
  
  # Susceptible and total
  N_total <- S + E + I + R + V1S + V1E + V1I + V1R + V2S + V2E + V2I + V2R + SIAS +SIAE + SIAI + SIAR + H0 + H1 + H2 + H3
  susceptible <- S + V1S + V2S + SIAS
  prop_sus <- ifelse(N_total > 0, susceptible / N_total, 0)
  
  n_time <- nrow(prop_sus)
  
  # NGM matrix precomputation
  NGM_list <- lapply(1:n_time, function(t) {
    p * seas * (1 / gamma) * m_contact_1y %*% diag(prop_sus[t, ])
  })
  
  # Parallel computation
  n_cores <- max(1, parallel::detectCores() - 1)
  cl <- makeCluster(n_cores)
  clusterExport(cl, varlist = c("NGM_list"), envir = environment())
  
  R_eff <- parLapply(cl, NGM_list, function(mat) {
    max(Re(eigen(mat, only.values = TRUE)$values))
  })
  
  stopCluster(cl)
  
  # Return result
  data.frame(
    time_days = results_Tunisia_scenario_3$time,
    time_years = results_Tunisia_scenario_3$time / 365,
    R_eff = unlist(R_eff)
  )
}

#  Rt data
start_time<-Sys.time()
(Rt_data_Tunisia_scenario_3 <- calculate_effective_Rt_parallel(results_Tunisia_scenario_3, parameters_3))
end_time<-Sys.time()
(end_time-start_time)

# Plot 1: Basic R_eff over time
(Rt_basic_plot <- ggplot(Rt_data_Tunisia_scenario_3, aes(x = time_years, y = R_eff)) +
    geom_line(color = "#E63946", size = 1.2) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "black", alpha = 0.7, size = 1) +
    geom_vline(xintercept = seq(0, 3, by = 2), linetype = "dotted", color = "blue", alpha = 0.5) +
    annotate("text", x = 2, y = max(Rt_data_Tunisia_scenario_3$R_eff) * 0.9, 
             label = "Rt = 1 (Epidemic threshold)", color = "black", size = 3.5) +
    annotate("text", x = 1, y = max(Rt_data_Tunisia_scenario_3$R_eff) * 0.8, 
             label = "SIA campaigns", color = "blue", size = 3) +
    labs(
      x = "Time (Years)",
      y = "Rt",
      title = "Rt over time; Scenario 3:RIS-SIA-5Y",
      #subtitle = "Impact of continuous and pulse vaccination on transmission potential"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12)
    ))
#
# Adjusted: time_years to actual calendar years starting at 2000
Rt_data_Tunisia_scenario_3$year <- 2000 + Rt_data_Tunisia_scenario_3$time_years
dim(Rt_data_Tunisia_scenario_3)
# Plot using calendar years on x-axis
Rt_basic_plot <- ggplot(Rt_data_Tunisia_scenario_3, aes(x = year, y = R_eff)) +
  geom_line(color = "#E63946", size = 1.2) +
  
  # Horizontal line at Rt = 1
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", alpha = 0.7, size = 1) +
  
  # Vertical lines for SIA campaigns every 2 years from 2000 to 2045
  geom_vline(xintercept = seq(2023, 2035, by = 2), linetype = "dotted", color = "blue", alpha = 0.5) +
  
  # Annotations
  annotate("text", x = 2005, y = max(Rt_data_Tunisia_scenario_3$R_eff) * 0.9, 
           label = "Rt = 1 (Epidemic threshold)", color = "black", size = 3.5) +
  annotate("text", x = 2005, y = max(Rt_data_Tunisia_scenario_3$R_eff) * 0.8, 
           label = "SIA campaigns", color = "blue", size = 3) +
  
  # Labels and theme
  labs(
    x = "Year",
    y = "Rt",
    title = "Effective Reproduction Number (Rt):RI-SIA-5Y"
    # subtitle = "Impact of continuous and pulse vaccination on transmission potential"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )
# Print the plot
Rt_basic_plot

# Plot 2: R_eff with epidemic threshold zones
(Rt_zone_plot <- ggplot(Rt_data_Tunisia_scenario_3, aes(x = time_years, y = R_eff)) +
    # Colored background zones
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 1, ymax = Inf, 
             fill = "#ffcccc", alpha = 0.3) +  # Epidemic zone (R > 1)
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 1, 
             fill = "#ccffcc", alpha = 0.3) +  # Control zone (R < 1)
    
    geom_line(color = "#E63946", size = 1.3) +
    geom_point(data = Rt_data_Tunisia_scenario_3[seq(1, nrow(Rt_data_Tunisia_scenario_3), by = 50), ], 
               color = "#E63946", size = 1) +
    geom_hline(yintercept = 1, linetype = "solid", color = "black", size = 1) +
    geom_vline(xintercept = seq(0, 2, by = 2), linetype = "dashed", color = "blue", alpha = 0.6) +
    
    # Text annotations
    annotate("text", x = max(Rt_data_Tunisia_scenario_3$time_years) * 0.8, y = max(Rt_data_Tunisia_scenario_2$R_eff) * 0.95, 
             label = "Epidemic Zone:(Rt > 1)", color = "#8B0000", size = 4, fontface = "bold") +
    annotate("text", x = max(Rt_data_Tunisia_scenario_3$time_years) * 0.8, y = 0.5, 
             label = "Control Zone:(Rt < 1)", color = "#006400", size = 4, fontface = "bold") +
    annotate("text", x = 4, y = max(Rt_data_Tunisia_scenario_3$R_eff) * 0.1, 
             label = "SIA campaigns every 2 years", color = "blue", size = 3, angle = 0) +
    
    labs(
      x = "Time (Years)",
      y = "Rt",
      title = "Epidemic control assessment in Tunisia :SIA+RI",
      #subtitle = "Green:disease control (Rt < 1)|Red :Potential epidemic spread (Rt > 1)"
    ) +
    scale_y_continuous(limits = c(0, max(Rt_data_Tunisia_scenario_3$R_eff) * 1.1)) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11),
      panel.grid.minor = element_blank()
    ))
#Year in thousand
Rt_data_Tunisia_scenario_3$year <- as.numeric(2000 + Rt_data_Tunisia_scenario_3$time_years)

Rt_zone_plot <- ggplot(Rt_data_Tunisia_scenario_3, aes(x = year, y = R_eff)) +
  scale_x_continuous(breaks = seq(2000, max(Rt_data_Tunisia_scenario_3$year), by = 5)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 1, ymax = Inf, 
           fill = "#ffcccc", alpha = 0.3) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 1, 
           fill = "#ccffcc", alpha = 0.3) +
  geom_line(color = "#E63946", size = 1.3) +
  geom_point(data = Rt_data_Tunisia_scenario_3[seq(1, nrow(Rt_data_Tunisia_scenario_3), by = 50), ], 
             aes(x = year, y = R_eff), color = "#E63946", size = 1) +
  geom_hline(yintercept = 1, linetype = "solid", color = "black", size = 1) +
  geom_vline(xintercept = seq(2023, max(Rt_data_Tunisia_scenario_3$year), by = 3), 
             linetype = "dashed", color = "blue", alpha = 0.6) +
  annotate("text", x = 2020, y = max(Rt_data_Tunisia_scenario_3$R_eff) * 0.95, 
           label = "Epidemic Zone: (Rt > 1)", color = "#8B0000", size = 4, fontface = "bold") +
  annotate("text", x = 2020, y = 0.5, 
           label = "Control Zone: (Rt < 1)", color = "#006400", size = 4, fontface = "bold") +
  annotate("text", x = 2004, y = max(Rt_data_Tunisia_scenario_3$R_eff) * 0.1, 
           label = "SIA campaigns every 2 years", color = "blue", size = 3) +
  labs(
    x = "Year",
    y = "Rt",
    title = "Epidemic Control Assessment in Tunisia: SIA + RI"
  ) +
  scale_y_continuous(limits = c(0, max(Rt_data_Tunisia_scenario_3$R_eff) * 1.1)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    panel.grid.minor = element_blank()
  )
Rt_zone_plot
# Plot 3: R_eff distribution histogram
(Rt_hist_plot <- ggplot(Rt_data_Tunisia_scenario_3, aes(x = R_eff)) +
    geom_histogram(bins = 30, fill = "#118AB2", alpha = 0.7, color = "white") +
    geom_vline(xintercept = 1, linetype = "dashed", color = "#E63946", size = 1.2) +
    geom_vline(xintercept = mean(Rt_data_Tunisia_scenario_3$R_eff), linetype = "solid", color = "orange", size = 1) +
    annotate("text", x = 1.1, y = max(table(cut(Rt_data_Tunisia_scenario_3$R_eff, breaks = 30))) * 0.8,
             label = "Epidemic\nthreshold", color = "#E63946", size = 3.5, hjust = 0) +
    annotate("text", x = mean(Rt_data_Tunisia_scenario_3$R_eff) + 0.1, y = max(table(cut(Rt_data_Tunisia_scenario_3$R_eff, breaks = 30))) * 0.6,
             label = paste("Mean =", round(mean(Rt_data_Tunisia_scenario_3$R_eff), 2)), 
             color = "orange", size = 3.5, hjust = 0) +
    labs(
      x = "Rt",
      y = "Frequency",
      title = "Distribution of Rt values",
      subtitle = paste("Proportion of time with Rt < 1:", 
                       round(100 * sum(Rt_data_Tunisia_scenario_3$R_eff < 1) / nrow(Rt_data_Tunisia_scenario_3), 1), "%")
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12)
    ))
# Plot 4: Combined plot showing R_eff and vaccination coverage
# a.vaccination coverage
vaccine_coverage_over_time <- results_Tunisia_long1 %>%
  filter(compartment %in% c("V1S","V1E", "V1I", "V1R", "V2S","V2E" ,"V2I", "V2R", "SIAS", "SIAE","SIAI", "SIAR")) %>%
  group_by(time_days) %>%
  summarise(total_vaccinated = sum(value), .groups = "drop") %>%
  left_join(
    results_Tunisia_long1 %>%
      group_by(time_days) %>%
      summarise(total_population = sum(value), .groups = "drop"),
    by = "time_days"
  ) %>%
  mutate(
    vaccination_coverage = (total_vaccinated / total_population) * 100,
    time_years = time_days / 365
  )
names(vaccine_coverage_over_time)
# Rt and vaccination data
combined_data <- Rt_data_Tunisia_scenario_3 %>%
  left_join(vaccine_coverage_over_time, by = c("time_days", "time_years"))

# Dual-axis plot
(Rt_coverage_plot <- ggplot(combined_data, aes(x = time_years)) +
    geom_line(aes(y = R_eff), color = "#E63946", size = 1.2) +
    geom_line(aes(y = vaccination_coverage/40), color = "#118AB2", size = 1.2) +  # Scale down coverage for dual axis
    geom_hline(yintercept = 1, linetype = "dashed", color = "black", alpha = 0.7) +
    geom_vline(xintercept = 1*2, linetype = "dashed", color = "purple", alpha = 0.7)+
    geom_vline(xintercept = 2*2, linetype = "dashed", color = "purple", alpha = 0.7)+
    geom_vline(xintercept = 3*2, linetype = "dashed", color = "purple", alpha = 0.7)+
    # geom_vline(xintercept = 4*2, linetype = "dashed", color = "purple", alpha = 0.7)+
    # geom_vline(xintercept = 5*2, linetype = "dashed", color = "purple", alpha = 0.7)+
    # Dual y-axis
    scale_y_continuous(
      name = "Rt",
      sec.axis = sec_axis(~ . * 40, name = "Vaccination Coverage (%)")
    ) +
    
    labs(
      x = "Time (Years)",
      title = "Rt and  vaccination coverage over time in Tunisia",
      #subtitle = "Red line: Rt (left axis), Blue line: Vaccination coverage (right axis)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.title.y.left = element_text(color = "#E63946"),
      axis.title.y.right = element_text(color = "#118AB2"),
      axis.text.y.left = element_text(color = "#E63946"),
      axis.text.y.right = element_text(color = "#118AB2")
    ))
##
# Fixed maximums
max_Rt <- max_Reff
max_coverage <- 100
scaling_factor <- max_Rt / max_coverage  # 0.2

# Plot with proper scaling
(Rt_coverage_plot <- ggplot(combined_data, aes(x = time_years)) +
    geom_line(aes(y = R_eff), color = "#E63946", size = 1.2) +
    geom_line(aes(y = vaccination_coverage * scaling_factor), color = "#118AB2", size = 1.2) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "black", alpha = 0.7) +
    geom_vline(xintercept = seq(1, 3) * 2, linetype = "dashed", color = "purple", alpha = 0.7) +
    scale_y_continuous(
      name = "Rt",
      sec.axis = sec_axis(
        transform = ~ . / scaling_factor,
        name = "Vaccination Coverage (%)"
      )
    ) +
    coord_cartesian(ylim = c(0, max_Rt)) +  
    labs(
      x = "Time (Years)",
      title = "Rt and vaccination coverage over time in Tunisia"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.title.y.left = element_text(color = "#E63946"),
      axis.title.y.right = element_text(color = "#118AB2"),
      axis.text.y.left = element_text(color = "#E63946"),
      axis.text.y.right = element_text(color = "#118AB2")
    ))

#Rt-4
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#              Scanario_4:Worse:  : RI-reduced                                         @
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
parameters_4<-parameters
#Reduced RI
parameters_4$cov_MCV1[26:36]<-0.1
parameters_4$cov_MCV2[26:36]<-0.1
parameters_4$rate1<--log(1-parameters_4$cov_MCV1)/365
parameters_4$rate2<--log(1-parameters_4$cov_MCV2)/365
#Timer
end_time <- Sys.time()
output_Tunisia_scenario_4 <- ode(y = Y,times = times,func = Tunisia_model,parms = parameters_4,method = "lsoda")
end_time <- Sys.time()
(duration <- end_time - start_time)
# Process results_Tunisia
results_Tunisia_scenario_4 <- as.data.frame(output_Tunisia_scenario_4)
# Column names (including hospitalization and wasted doses compartments and death counters
compartment_names <- c("S", "E", "I", "R", "V1S", "V1E", "V1I", "V1R", "V2S", "V2E", "V2I", "V2R", 
                       "SIAS", "SIAE", "SIAI", "SIAR", "H0", "H1", "H2", "H3",
                       "D0", "D1", "D2", "D3", "DTotal",
                       "W1", "W2", "W3", "WTotal")

col_names <- c("time")
for(comp in compartment_names) {
  for(age in age_groups) {
    col_names <- c(col_names, paste0(comp, "_", age))
  }
}
colnames(results_Tunisia_scenario_4) <- col_names

#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Epidemic control assessment  using Rt 
#Next Genaration Matrix approach
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#O. Diekmann1, J. A. P. Heesterbeek2,*, and M. G. Roberts3
#Title: The construction of next-generation matrices for compartmental epidemic models

calculate_effective_Rt_parallel <- function(results_Tunisia_scenario_4, parameters_4) {
  library(parallel)
  
  # Parameters
  p <- parameters_4$p
  gamma <- parameters_4$gamma
  m_contact_1y <- parameters_4$m_contact_1y
  n_age <- parameters_4$n_age
  seas <- 1
  
  idx <- function(offset) ((offset * n_age + 2):((offset + 1) * n_age + 1))
  
  get_compartment <- function(offset) {
    as.matrix(results_Tunisia_scenario_4[, idx(offset)])
  }
  
  # Extraction of  compartments
  S    <- get_compartment(0)
  E    <- get_compartment(1)
  I    <- get_compartment(2)
  R    <- get_compartment(3)
  V1S  <- get_compartment(4)
  V1E  <- get_compartment(5)
  V1I  <- get_compartment(6)
  V1R  <- get_compartment(7)
  V2S  <- get_compartment(8)
  V2E  <- get_compartment(9)
  V2I  <- get_compartment(10)
  V2R  <- get_compartment(11)
  SIAS <- get_compartment(12)
  SIAE <- get_compartment(13)
  SIAI <- get_compartment(14)
  SIAR <- get_compartment(15)
  H0   <- get_compartment(16)
  H1   <- get_compartment(17)
  H2   <- get_compartment(18)
  H3   <- get_compartment(19)
  
  # Susceptible and total
  N_total <- S + E + I + R + V1S + V1E + V1I + V1R + V2S + V2E + V2I + V2R + SIAS +SIAE + SIAI + SIAR + H0 + H1 + H2 + H3
  susceptible <- S + V1S + V2S + SIAS
  prop_sus <- ifelse(N_total > 0, susceptible / N_total, 0)
  
  n_time <- nrow(prop_sus)
  
  # NGM matrix precomputation
  NGM_list <- lapply(1:n_time, function(t) {
    p * seas * (1 / gamma) * m_contact_1y %*% diag(prop_sus[t, ])
  })
  
  # Parallel computation
  n_cores <- max(1, parallel::detectCores() - 1)
  cl <- makeCluster(n_cores)
  clusterExport(cl, varlist = c("NGM_list"), envir = environment())
  
  R_eff <- parLapply(cl, NGM_list, function(mat) {
    max(Re(eigen(mat, only.values = TRUE)$values))
  })
  
  stopCluster(cl)
  
  # Return result
  data.frame(
    time_days = results_Tunisia_scenario_4$time,
    time_years = results_Tunisia_scenario_4$time / 365,
    R_eff = unlist(R_eff)
  )
}
# Rt data
start_time<-Sys.time()
(Rt_data_Tunisia_scenario_4 <- calculate_effective_Rt_parallel(results_Tunisia_scenario_4, parameters_4))
end_time<-Sys.time()
(end_time-start_time)

# Plot 1: Basic R_eff over time
(Rt_basic_plot <- ggplot(Rt_data_Tunisia_scenario_4, aes(x = time_years, y = R_eff)) +
    geom_line(color = "#E63946", size = 1.2) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "black", alpha = 0.7, size = 1) +
    geom_vline(xintercept = seq(0, 3, by = 2), linetype = "dotted", color = "blue", alpha = 0.5) +
    annotate("text", x = 2, y = max(Rt_data_Tunisia_scenario_4$R_eff) * 0.9, 
             label = "Rt = 1 (Epidemic threshold)", color = "black", size = 3.5) +
    annotate("text", x = 1, y = max(Rt_data_Tunisia_scenario_4$R_eff) * 0.8, 
             label = "SIA campaigns", color = "blue", size = 3) +
    labs(
      x = "Time (Years)",
      y = "Rt",
      title = "Rt over time; Scenario 3:RIS-SIA-5Y",
      #subtitle = "Impact of continuous and pulse vaccination on transmission potential"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12)
    ))
# Adjusted: time_years to actual calendar years starting at 2000
Rt_data_Tunisia_scenario_4$year <- 2000 + Rt_data_Tunisia_scenario_4$time_years

# Plot using calendar years on x-axis
Rt_basic_plot <- ggplot(Rt_data_Tunisia_scenario_4, aes(x = year, y = R_eff)) +
  geom_line(color = "#E63946", size = 1.2) +
  
  # Horizontal line at Rt = 1
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", alpha = 0.7, size = 1) +
  
  # Vertical lines for SIA campaigns every 2 years from 2000 to 2045
  geom_vline(xintercept = seq(2023, 2035, by = 2), linetype = "dotted", color = "blue", alpha = 0.5) +
  
  # Annotations
  annotate("text", x = 2005, y = max(Rt_data_Tunisia_scenario_4$R_eff) * 0.9, 
           label = "Rt = 1 (Epidemic threshold)", color = "black", size = 3.5) +
  annotate("text", x = 2005, y = max(Rt_data_Tunisia_scenario_4$R_eff) * 0.8, 
           label = "SIA campaigns", color = "blue", size = 3) +
  
  # Labels and theme
  labs(
    x = "Year",
    y = "Rt",
    title = "Effective Reproduction Number (Rt):RI-SIA-5Y"
    # subtitle = "Impact of continuous and pulse vaccination on transmission potential"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )
# Print the plot
Rt_basic_plot
# Plot 2: R_eff with epidemic threshold zones
(Rt_zone_plot <- ggplot(Rt_data_Tunisia_scenario_4, aes(x = time_years, y = R_eff)) +
    # Colored background zones
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 1, ymax = Inf, 
             fill = "#ffcccc", alpha = 0.3) +  # Epidemic zone (R > 1)
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 1, 
             fill = "#ccffcc", alpha = 0.3) +  # Control zone (R < 1)
    
    geom_line(color = "#E63946", size = 1.3) +
    geom_point(data = Rt_data_Tunisia_scenario_4[seq(1, nrow(Rt_data_Tunisia_scenario_4), by = 50), ], 
               color = "#E63946", size = 1) +
    geom_hline(yintercept = 1, linetype = "solid", color = "black", size = 1) +
    geom_vline(xintercept = seq(0, 2, by = 2), linetype = "dashed", color = "blue", alpha = 0.6) +
    
    # Text annotations
    annotate("text", x = max(Rt_data_Tunisia_scenario_4$time_years) * 0.8, y = max(Rt_data_Tunisia_scenario_4$R_eff) * 0.95, 
             label = "Epidemic Zone:(Rt > 1)", color = "#8B0000", size = 4, fontface = "bold") +
    annotate("text", x = max(Rt_data_Tunisia_scenario_4$time_years) * 0.8, y = 0.5, 
             label = "Control Zone:(Rt < 1)", color = "#006400", size = 4, fontface = "bold") +
    annotate("text", x = 4, y = max(Rt_data_Tunisia_scenario_4$R_eff) * 0.1, 
             label = "SIA campaigns every 2 years", color = "blue", size = 3, angle = 0) +
    
    labs(
      x = "Time (Years)",
      y = "Rt",
      title = "Epidemic control assessment in Tunisia :SIA+RI",
      #subtitle = "Green:disease control (Rt < 1)|Red :Potential epidemic spread (Rt > 1)"
    ) +
    scale_y_continuous(limits = c(0, max(Rt_data_Tunisia_scenario_4$R_eff) * 1.1)) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11),
      panel.grid.minor = element_blank()
    ))
#Year in thousand
Rt_data_Tunisia_scenario_4$year <- as.numeric(2000 + Rt_data_Tunisia_scenario_4$time_years)

Rt_zone_plot <- ggplot(Rt_data_Tunisia_scenario_4, aes(x = year, y = R_eff)) +
  scale_x_continuous(breaks = seq(2000, max(Rt_data_Tunisia_scenario_4$year), by = 5)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 1, ymax = Inf, 
           fill = "#ffcccc", alpha = 0.3) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 1, 
           fill = "#ccffcc", alpha = 0.3) +
  geom_line(color = "#E63946", size = 1.3) +
  geom_point(data = Rt_data_Tunisia_scenario_4[seq(1, nrow(Rt_data_Tunisia_scenario_4), by = 50), ], 
             aes(x = year, y = R_eff), color = "#E63946", size = 1) +
  geom_hline(yintercept = 1, linetype = "solid", color = "black", size = 1) +
  geom_vline(xintercept = seq(2023, max(Rt_data_Tunisia_scenario_4$year), by = 3), 
             linetype = "dashed", color = "blue", alpha = 0.6) +
  annotate("text", x = 2020, y = max(Rt_data_Tunisia_scenario_4$R_eff) * 0.95, 
           label = "Epidemic Zone: (Rt > 1)", color = "#8B0000", size = 4, fontface = "bold") +
  annotate("text", x = 2020, y = 0.5, 
           label = "Control Zone: (Rt < 1)", color = "#006400", size = 4, fontface = "bold") +
  annotate("text", x = 2004, y = max(Rt_data_Tunisia_scenario_4$R_eff) * 0.1, 
           label = "SIA campaigns every 2 years", color = "blue", size = 3) +
  labs(
    x = "Year",
    y = "Rt",
    title = "Epidemic Control Assessment in Tunisia: SIA + RI"
  ) +
  scale_y_continuous(limits = c(0, max(Rt_data_Tunisia_scenario_4$R_eff) * 1.1)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    panel.grid.minor = element_blank()
  )

Rt_zone_plot

#plotly::ggplotly(Rt_zone_plot)
# Plot 3: R_eff distribution histogram
(Rt_hist_plot <- ggplot(Rt_data_Tunisia_scenario_4, aes(x = R_eff)) +
    geom_histogram(bins = 30, fill = "#118AB2", alpha = 0.7, color = "white") +
    geom_vline(xintercept = 1, linetype = "dashed", color = "#E63946", size = 1.2) +
    geom_vline(xintercept = mean(Rt_data_Tunisia_scenario_4$R_eff), linetype = "solid", color = "orange", size = 1) +
    annotate("text", x = 1.1, y = max(table(cut(Rt_data_Tunisia_scenario_4$R_eff, breaks = 30))) * 0.8,
             label = "Epidemic\nthreshold", color = "#E63946", size = 3.5, hjust = 0) +
    annotate("text", x = mean(Rt_data_Tunisia_scenario_4$R_eff) + 0.1, y = max(table(cut(Rt_data_Tunisia_scenario_4$R_eff, breaks = 30))) * 0.6,
             label = paste("Mean =", round(mean(Rt_data_Tunisia_scenario_4$R_eff), 2)), 
             color = "orange", size = 3.5, hjust = 0) +
    labs(
      x = "Rt",
      y = "Frequency",
      title = "Distribution of Rt values",
      subtitle = paste("Proportion of time with Rt < 1:", 
                       round(100 * sum(Rt_data_Tunisia_scenario_4$R_eff < 1) / nrow(Rt_data_Tunisia_scenario_4), 1), "%")
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12)
    ))

# Plot 4: Combined plot showing R_eff and vaccination coverage
# a.vaccination coverage
vaccine_coverage_over_time <- results_Tunisia_long1 %>%
  filter(compartment %in% c("V1S", "V1E","V1I", "V1R", "V2S","V2E", "V2I", "V2R", "SIAS","SIAE", "SIAI", "SIAR")) %>%
  group_by(time_days) %>%
  summarise(total_vaccinated = sum(value), .groups = "drop") %>%
  left_join(
    results_Tunisia_long1 %>%
      group_by(time_days) %>%
      summarise(total_population = sum(value), .groups = "drop"),
    by = "time_days"
  ) %>%
  mutate(
    vaccination_coverage = (total_vaccinated / total_population) * 100,
    time_years = time_days / 365
  )
names(vaccine_coverage_over_time)
# Rt and vaccination data
combined_data <- Rt_data_Tunisia_scenario_4 %>%
  left_join(vaccine_coverage_over_time, by = c("time_days", "time_years"))

# Dual-axis plot
(Rt_coverage_plot <- ggplot(combined_data, aes(x = time_years)) +
    geom_line(aes(y = R_eff), color = "#E63946", size = 1.2) +
    geom_line(aes(y = vaccination_coverage/40), color = "#118AB2", size = 1.2) +  # Scale down coverage for dual axis
    geom_hline(yintercept = 1, linetype = "dashed", color = "black", alpha = 0.7) +
    #geom_vline(xintercept = 1*2, linetype = "dashed", color = "purple", alpha = 0.7)+
    #geom_vline(xintercept = 2*2, linetype = "dashed", color = "purple", alpha = 0.7)+
    #geom_vline(xintercept = 3*2, linetype = "dashed", color = "purple", alpha = 0.7)+
    # geom_vline(xintercept = 4*2, linetype = "dashed", color = "purple", alpha = 0.7)+
    # geom_vline(xintercept = 5*2, linetype = "dashed", color = "purple", alpha = 0.7)+
    # Dual y-axis
    scale_y_continuous(
      name = "Rt",
      sec.axis = sec_axis(~ . * 40, name = "Vaccination Coverage (%)")
    ) +
    
    labs(
      x = "Time (Years)",
      title = "Rt and  vaccination coverage over time in Tunisia",
      #subtitle = "Red line: Rt (left axis), Blue line: Vaccination coverage (right axis)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.title.y.left = element_text(color = "#E63946"),
      axis.title.y.right = element_text(color = "#118AB2"),
      axis.text.y.left = element_text(color = "#E63946"),
      axis.text.y.right = element_text(color = "#118AB2")
    ))
##
# Fixed maximums
max_Rt <- max_Reff
max_coverage <- 100
scaling_factor <- max_Rt / max_coverage  # 0.2

# Plot with proper scaling
(Rt_coverage_plot <- ggplot(combined_data, aes(x = time_years)) +
    geom_line(aes(y = R_eff), color = "#E63946", size = 1.2) +
    geom_line(aes(y = vaccination_coverage * scaling_factor), color = "#118AB2", size = 1.2) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "black", alpha = 0.7) +
    geom_vline(xintercept = seq(1,3)*2, linetype = "dashed", color = "purple", alpha = 0.7) +
    scale_y_continuous(
      name = "Rt",
      limits = c(0, max_Rt),  # Primary axis limits
      sec.axis = sec_axis(
        trans = ~./scaling_factor,
        name = "Vaccination Coverage (%)"
      )
    ) +
    coord_cartesian(ylim = c(0, max_Rt)) +  # Ensures both scales match
    labs(
      x = "Time (Years)",
      title = "Rt and vaccination coverage over time in Tunisia"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.title.y.left = element_text(color = "#E63946"),
      axis.title.y.right = element_text(color = "#118AB2"),
      axis.text.y.left = element_text(color = "#E63946"),
      axis.text.y.right = element_text(color = "#118AB2")
    ))
#

#{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}
#          Scnario_5: Pre-screening                 #
#{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}

parameters_5 <- parameters
#parameters_5$eff1_c <- 1e-6 #No vaccine in individual with natural immunity
#parameters_5$eff2_c <- 1e-6 #No vaccine in individual with natural/vaccine induced immunity immunity
parameters_5$eff3_c <- 1e-12 #No SIA  in individual with natural immunity or induced immunity immunity
# Timer
start_time <- Sys.time()
output_Tunisia_scenario_5 <- ode(y = Y, times = times,func = Tunisia_model,parms = parameters_5,method = "lsoda")
end_time <- Sys.time()
(duration <- end_time - start_time)
# Process results
results_Tunisia_scenario_5 <- as.data.frame(output_Tunisia_scenario_5)
# Rename columns
compartment_names <- c("S", "E", "I", "R", "V1S", "V1E", "V1I", "V1R", "V2S", "V2E", "V2I", "V2R", 
                       "SIAS", "SIAE", "SIAI", "SIAR", "H0", "H1", "H2", "H3",
                       "D0", "D1", "D2", "D3", "DTotal",
                       "W1", "W2", "W3", "WTotal")

col_names <- c("time")
for (comp in compartment_names) {
  for (age in age_groups) {
    col_names <- c(col_names, paste0(comp, "_", age))
  }
}
colnames(results_Tunisia_scenario_5) <- col_names

# 2025-2035 index
index_2025<-365*25
index_2035<-nrow(results_Tunisia_scenario_4)
#New subsets
#results_Tunisia<-results_Tunisia[index_2025:index_2035,]
#results_Tunisia_scenario_1<-results_Tunisia_scenario_1[index_2025:index_2035,]
#results_Tunisia_scenario_2<-results_Tunisia_scenario_2[index_2025:index_2035,]
#results_Tunisia_scenario_3<-results_Tunisia_scenario_3[index_2025:index_2035,]
#results_Tunisia_scenario_4<-results_Tunisia_scenario_4[index_2025:index_2035,]
#results_Tunisia_scenario_5<-results_Tunisia_scenario_5[index_2025:index_2035,]

head(results_Tunisia)
#Combined scenarios-model
# Load required library
library(ggplot2)

# Sigma with reporting probability
report_sigma <- parameters[["report"]] * parameters[["sigma"]]

#  0. Baseline:RI at 2023 level 
results_Tunisia <- as.data.frame(results_Tunisia)
results_Tunisia$scenario <- "RI"
results_Tunisia$Country <- "Tunisia"

# 0.a. Incidence
results_Tunisia$incidence <- report_sigma * (
  rowSums(results_Tunisia[, Eindex + 1]) +
    rowSums(results_Tunisia[, V1Eindex + 1]) +
    rowSums(results_Tunisia[, V2Eindex + 1]) +
    rowSums(results_Tunisia[, SIAEindex + 1])
)

# 0.b. Cumulative incidence
results_Tunisia$cum_incidence <- cumsum(results_Tunisia$incidence)

# 0.c. Severe cases
results_Tunisia$hospitalization <- rowSums(results_Tunisia[, H0index + 1]) +
  rowSums(results_Tunisia[, H1index + 1]) +
  rowSums(results_Tunisia[, H2index + 1]) +
  rowSums(results_Tunisia[, H3index + 1])

# 0.d. Cumulative severe cases
results_Tunisia$cum_hospitalization <- cumsum(results_Tunisia$hospitalization)

# 0.e. Deaths
results_Tunisia$deaths <- rowSums(results_Tunisia[, DTotalindex + 1])



# 0.f. Cumulative deaths
results_Tunisia$cum_deaths <- cumsum(results_Tunisia$deaths)
summary(results_Tunisia$cum_death)
summary(results_Tunisia$deaths)

# 0.g. Wasted doses
results_Tunisia$wasted_doses <-rowSums(results_Tunisia[, W3index + 1])

# 0.h. Cumulative wasted doses
results_Tunisia$cum_wasted_doses <- cumsum(results_Tunisia$wasted_doses)


#Scenario 1 :"RI+SIA-2Y"
results_Tunisia_scenario_1 <- as.data.frame(results_Tunisia_scenario_1)
results_Tunisia_scenario_1$scenario <- "RI+SIA-2Y"
results_Tunisia_scenario_1$Country <- "Tunisia"

# 1.a. Incidence
results_Tunisia_scenario_1$incidence <- report_sigma * (
  rowSums(results_Tunisia_scenario_1[, Eindex + 1]) +
    rowSums(results_Tunisia_scenario_1[, V1Eindex + 1]) +
    rowSums(results_Tunisia_scenario_1[, V2Eindex + 1]) +
    rowSums(results_Tunisia_scenario_1[, SIAEindex + 1])
)

# 1.b. Cumulative incidence
results_Tunisia_scenario_1$cum_incidence <- cumsum(results_Tunisia_scenario_1$incidence)

# 1.c. Severe cases
results_Tunisia_scenario_1$hospitalization <- rowSums(results_Tunisia_scenario_1[, H0index + 1]) +
  rowSums(results_Tunisia_scenario_1[, H1index + 1]) +
  rowSums(results_Tunisia_scenario_1[, H2index + 1]) +
  rowSums(results_Tunisia_scenario_1[, H3index + 1])

# 1.d. Cumulative severe cases
results_Tunisia_scenario_1$cum_hospitalization <- cumsum(results_Tunisia_scenario_1$hospitalization)

# 1.e. Deaths
results_Tunisia_scenario_1$deaths <- rowSums(results_Tunisia_scenario_1[, DTotalindex + 1])

# 1.f. Cumulative deaths
results_Tunisia_scenario_1$cum_deaths <- cumsum(results_Tunisia_scenario_1$deaths)

# 1.g. Wasted doses
results_Tunisia_scenario_1$wasted_doses <-rowSums(results_Tunisia_scenario_1[, W3index + 1])

# 1.h. Cumulative wasted doses
results_Tunisia_scenario_1$cum_wasted_doses <- cumsum(results_Tunisia_scenario_1$wasted_doses)

#Scenario 2 :RI-Scaled-up
results_Tunisia_scenario_2 <- as.data.frame(results_Tunisia_scenario_2)
results_Tunisia_scenario_2$scenario <- "RI-Scaled-up"
results_Tunisia_scenario_2$Country <- "Tunisia"

# 2.a. Incidence
results_Tunisia_scenario_2$incidence <- report_sigma * (
  rowSums(results_Tunisia_scenario_2[, Eindex + 1]) +
    rowSums(results_Tunisia_scenario_2[, V1Eindex + 1]) +
    rowSums(results_Tunisia_scenario_2[, V2Eindex + 1]) +
    rowSums(results_Tunisia_scenario_2[, SIAEindex + 1])
)

# 2.b. Cumulative incidence
results_Tunisia_scenario_2$cum_incidence <- cumsum(results_Tunisia_scenario_2$incidence)

# 2.c. Severe cases
results_Tunisia_scenario_2$hospitalization <- rowSums(results_Tunisia_scenario_2[, H0index + 1]) +
  rowSums(results_Tunisia_scenario_2[, H1index + 1]) +
  rowSums(results_Tunisia_scenario_2[, H2index + 1]) +
  rowSums(results_Tunisia_scenario_2[, H3index + 1])

# 2.d. Cumulative severe cases
results_Tunisia_scenario_2$cum_hospitalization <- cumsum(results_Tunisia_scenario_2$hospitalization)

# 2.e. Deaths
results_Tunisia_scenario_2$deaths <- rowSums(results_Tunisia_scenario_2[, DTotalindex + 1])

# 2.f. Cumulative deaths
results_Tunisia_scenario_2$cum_deaths <- cumsum(results_Tunisia_scenario_2$deaths)

# 2.g. Wasted doses
results_Tunisia_scenario_2$wasted_doses <-rowSums(results_Tunisia_scenario_2[, W3index + 1])

# 2.h. Cumulative wasted doses
results_Tunisia_scenario_2$cum_wasted_doses <- cumsum(results_Tunisia_scenario_2$wasted_doses)


#Scenario 3:"RI+SIA-5Y"
results_Tunisia_scenario_3 <- as.data.frame(results_Tunisia_scenario_3)
results_Tunisia_scenario_3$scenario <- "RI+SIA-5Y"
results_Tunisia_scenario_3$Country <- "Tunisia"

# 3.a. Incidence
results_Tunisia_scenario_3$incidence <- report_sigma * (
  rowSums(results_Tunisia_scenario_3[, Eindex + 1]) +
    rowSums(results_Tunisia_scenario_3[, V1Eindex + 1]) +
    rowSums(results_Tunisia_scenario_3[, V2Eindex + 1]) +
    rowSums(results_Tunisia_scenario_3[, SIAEindex + 1])
)

# 3.b. Cumulative incidence
results_Tunisia_scenario_3$cum_incidence <- cumsum(results_Tunisia_scenario_3$incidence)

# 3.c. Severe cases
results_Tunisia_scenario_3$hospitalization <- rowSums(results_Tunisia_scenario_3[, H0index + 1]) +
  rowSums(results_Tunisia_scenario_3[, H1index + 1]) +
  rowSums(results_Tunisia_scenario_3[, H2index + 1]) +
  rowSums(results_Tunisia_scenario_3[, H3index + 1])

# 3.d. Cumulative severe cases
results_Tunisia_scenario_3$cum_hospitalization <- cumsum(results_Tunisia_scenario_3$hospitalization)

# 3.e. Deaths
results_Tunisia_scenario_3$deaths <- rowSums(results_Tunisia_scenario_3[, DTotalindex + 1])

# 3.f. Cumulative deaths
results_Tunisia_scenario_3$cum_deaths <- cumsum(results_Tunisia_scenario_3$deaths)

# 3.g. Wasted doses
results_Tunisia_scenario_3$wasted_doses <- rowSums(results_Tunisia_scenario_3[, W3index + 1])

# 3.h. Cumulative wasted doses
results_Tunisia_scenario_3$cum_wasted_doses <- cumsum(results_Tunisia_scenario_3$wasted_doses)
# Scenario 4: "RI-Reduced"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~>
results_Tunisia_scenario_4 <- as.data.frame(results_Tunisia_scenario_4)
results_Tunisia_scenario_4$scenario <- "RI-Reduced"
results_Tunisia_scenario_4$Country <- "Tunisia"

# 4.a. Incidence
results_Tunisia_scenario_4$incidence <- report_sigma * (
  rowSums(results_Tunisia_scenario_4[, Eindex + 1]) +
    rowSums(results_Tunisia_scenario_4[, V1Eindex + 1]) +
    rowSums(results_Tunisia_scenario_4[, V2Eindex + 1]) +
    rowSums(results_Tunisia_scenario_4[, SIAEindex + 1])
)

# 4.b. Cumulative incidence
results_Tunisia_scenario_4$cum_incidence <- cumsum(results_Tunisia_scenario_4$incidence)

# 4.c. Severe cases
results_Tunisia_scenario_4$hospitalization <- rowSums(results_Tunisia_scenario_4[, H0index + 1]) +
  rowSums(results_Tunisia_scenario_4[, H1index + 1]) +
  rowSums(results_Tunisia_scenario_4[, H2index + 1]) +
  rowSums(results_Tunisia_scenario_4[, H3index + 1])

# 4.d. Cumulative severe cases
results_Tunisia_scenario_4$cum_hospitalization <- cumsum(results_Tunisia_scenario_4$hospitalization)

# 4.e. Deaths
results_Tunisia_scenario_4$deaths <- rowSums(results_Tunisia_scenario_4[, DTotalindex + 1])

# 4.f. Cumulative deaths
results_Tunisia_scenario_4$cum_deaths <- cumsum(results_Tunisia_scenario_4$deaths)

# 4.g. Wasted doses
results_Tunisia_scenario_4$wasted_doses <-rowSums(results_Tunisia_scenario_4[, W3index + 1])

# 4.h. Cumulative wasted doses
results_Tunisia_scenario_4$cum_wasted_doses <- cumsum(results_Tunisia_scenario_4$wasted_doses)

#Scenario 5.Pre-screened vaccination (Routine,SIA)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~>
results_Tunisia_scenario_5 <- as.data.frame(results_Tunisia_scenario_5)
results_Tunisia_scenario_5$scenario <- "Pre-screening"
results_Tunisia_scenario_5$Country <- "Tunisia"

# 5.a. Incidence
results_Tunisia_scenario_5$incidence <- report_sigma * (
  rowSums(results_Tunisia_scenario_5[, Eindex + 1]) +
    rowSums(results_Tunisia_scenario_5[, V1Eindex + 1]) +
    rowSums(results_Tunisia_scenario_5[, V2Eindex + 1]) +
    rowSums(results_Tunisia_scenario_5[, SIAEindex + 1])
)

# 5.b. Cumulative incidence
results_Tunisia_scenario_5$cum_incidence <- cumsum(results_Tunisia_scenario_5$incidence)

# 5.c. Severe cases
results_Tunisia_scenario_5$hospitalization <- rowSums(results_Tunisia_scenario_5[, H0index + 1]) +
  rowSums(results_Tunisia_scenario_5[, H1index + 1]) +
  rowSums(results_Tunisia_scenario_5[, H2index + 1]) +
  rowSums(results_Tunisia_scenario_5[, H3index + 1])

# 5.d. Cumulative severe cases
results_Tunisia_scenario_5$cum_hospitalization <- cumsum(results_Tunisia_scenario_5$hospitalization)

# 5.e. Deaths
results_Tunisia_scenario_5$deaths <- rowSums(results_Tunisia_scenario_5[, DTotalindex + 1])

# 5.f. Cumulative deaths
results_Tunisia_scenario_5$cum_deaths <- cumsum(results_Tunisia_scenario_5$deaths)

# 5.g. Wasted doses
results_Tunisia_scenario_5$wasted_doses <- rowSums(results_Tunisia_scenario_5[, W1index + 1]) +
  rowSums(results_Tunisia_scenario_5[, W2index + 1]) +
  rowSums(results_Tunisia_scenario_5[, W3index + 1])

# 5.h. Cumulative wasted doses
results_Tunisia_scenario_5$cum_wasted_doses <- cumsum(results_Tunisia_scenario_5$wasted_doses)


# Checks
dim(results_Tunisia)
dim(results_Tunisia_scenario_1)
dim(results_Tunisia_scenario_2)
dim(results_Tunisia_scenario_3)
dim(results_Tunisia_scenario_4)
dim(results_Tunisia_scenario_5)

# Impact of interventions/secnarios 
# Incidence

(scenario_1_cum_incidence <- (tail(results_Tunisia$cum_incidence, 1) - tail(results_Tunisia_scenario_1$cum_incidence, 1)) / tail(results_Tunisia$cum_incidence, 1) * 100)
(scenario_2_cum_incidence <- (tail(results_Tunisia$cum_incidence, 1) - tail(results_Tunisia_scenario_2$cum_incidence, 1)) / tail(results_Tunisia$cum_incidence, 1) * 100)
(scenario_3_cum_incidence <- (tail(results_Tunisia$cum_incidence, 1) - tail(results_Tunisia_scenario_3$cum_incidence, 1)) / tail(results_Tunisia$cum_incidence, 1) * 100)
(scenario_4_cum_incidence <- (tail(results_Tunisia$cum_incidence, 1) - tail(results_Tunisia_scenario_4$cum_incidence, 1)) / tail(results_Tunisia$cum_incidence, 1) * 100)

#Severe cases 
(scenario_1_cum_hospitalization <- (tail(results_Tunisia$cum_hospitalization, 1) - tail(results_Tunisia_scenario_1$cum_hospitalization, 1)) / tail(results_Tunisia$cum_hospitalization, 1) * 100)
(scenario_2_cum_hospitalization <- (tail(results_Tunisia$cum_hospitalization, 1) - tail(results_Tunisia_scenario_2$cum_hospitalization, 1)) / tail(results_Tunisia$cum_hospitalization, 1) * 100)
(scenario_3_cum_hospitalization <- (tail(results_Tunisia$cum_hospitalization, 1) - tail(results_Tunisia_scenario_3$cum_hospitalization, 1)) / tail(results_Tunisia$cum_hospitalization, 1) * 100)
(scenario_4_cum_hospitalization <- (tail(results_Tunisia$cum_hospitalization, 1) - tail(results_Tunisia_scenario_4$cum_hospitalization, 1)) / tail(results_Tunisia$cum_hospitalization, 1) * 100)
#Deaths
(scenario_1_cum_deaths <- (tail(results_Tunisia$cum_deaths, 1) - tail(results_Tunisia_scenario_1$cum_deaths, 1)) / tail(results_Tunisia$cum_deaths, 1) * 100)
(scenario_2_cum_deaths <- (tail(results_Tunisia$cum_deaths, 1) - tail(results_Tunisia_scenario_2$cum_deaths, 1)) / tail(results_Tunisia$cum_deaths, 1) * 100)
(scenario_3_cum_deaths <- (tail(results_Tunisia$cum_deaths, 1) - tail(results_Tunisia_scenario_4$cum_deaths, 1)) / tail(results_Tunisia$cum_deaths, 1) * 100)
(scenario_4_cum_deaths <- (tail(results_Tunisia$cum_deaths, 1) - tail(results_Tunisia_scenario_2$cum_deaths, 1)) / tail(results_Tunisia$cum_deaths, 1) * 100)


#Visualization
library(ggplot2)
#Averted cases 
averted_cases_Tunisia <- data.frame(
  Scenario = c("RI-SIA-2Y", "RI-SCALED-UP", "RI-SIA-5Y", "RI-REDUCED"),
  Averted_Cases = c(
    tail(results_Tunisia$cum_incidence, 1) - tail(results_Tunisia_scenario_1$cum_incidence, 1),
    tail(results_Tunisia$cum_incidence, 1) - tail(results_Tunisia_scenario_2$cum_incidence, 1),
    tail(results_Tunisia$cum_incidence, 1) - tail(results_Tunisia_scenario_3$cum_incidence, 1),
    tail(results_Tunisia$cum_incidence, 1) - tail(results_Tunisia_scenario_4$cum_incidence, 1)
  )
)

#Plot
library(ggplot2)
library(scales)

ggplot(averted_cases_Tunisia, aes(x = Scenario, y = Averted_Cases, fill = Scenario)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = scales::label_number(scale = 1, accuracy = 1, suffix = " ")) +  
  labs(title = "",
       x = "Scenario",
       y = "Averted cases", fill = "Scenario") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

#Save final averted deaths
head(averted_cases_Tunisia) 
write.csv(averted_cases_Tunisia,"Final_averted_cases_Tunisia.csv",row.names = FALSE)

#Severe cases
averted_severe_cases_Tunisia <- data.frame(
  Scenario = c("RI-SIA-2Y", "RI-SCALED-UP", "RI-SIA-5Y", "RI-REDUCED"),
  Averted_Severe_cases = c(
    tail(results_Tunisia$cum_hospitalization, 1) - tail(results_Tunisia_scenario_1$cum_hospitalization, 1),
    tail(results_Tunisia$cum_hospitalization, 1) - tail(results_Tunisia_scenario_2$cum_hospitalization, 1),
    tail(results_Tunisia$cum_hospitalization, 1) - tail(results_Tunisia_scenario_3$cum_hospitalization, 1),
    tail(results_Tunisia$cum_hospitalization, 1) - tail(results_Tunisia_scenario_4$cum_hospitalization, 1)
  )
)

library(ggplot2)
ggplot(averted_severe_cases_Tunisia, aes(x = Scenario, y = Averted_Severe_cases, fill = Scenario)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d() + 
  scale_y_continuous(labels = scales::label_number(scale = 1e-3, accuracy = 1, suffix = "K")) +  
  labs(title = " ",
       x = "Scenario",
       y = "Averted severe cases", 
       fill = "Scenario") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))
#Save final averted deaths
write.csv(averted_severe_cases_Tunisia,"Final_averted_severe_cases_Tunisia.csv",row.names = FALSE)
#~~~~~~~~~~~~~~~~~#
# Averted deaths  #
#~~~~~~~~~~~~~~~~~#
averted_deaths_Tunisia <- data.frame(
  Scenario = c("RI-SIA-2Y", "RI-SCALED-UP", "RI-SIA-5Y", "RI-REDUCED"),
  Averted_deaths = c(
    tail(results_Tunisia$cum_deaths, 1) - tail(results_Tunisia_scenario_1$cum_deaths, 1),
    tail(results_Tunisia$cum_deaths, 1) - tail(results_Tunisia_scenario_2$cum_deaths, 1),
    tail(results_Tunisia$cum_deaths, 1) - tail(results_Tunisia_scenario_3$cum_deaths, 1),
    tail(results_Tunisia$cum_deaths, 1) - tail(results_Tunisia_scenario_4$cum_deaths, 1)
  )
)
# Plot of averted deaths
library(ggplot2)
ggplot(averted_deaths_Tunisia, aes(x = Scenario, y = Averted_deaths, fill = Scenario)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d() + 
  scale_y_continuous(labels = scales::label_number(scale = 1, accuracy = 1, suffix = " ")) +  
  labs(title = " ",
       x = "Scenario",
       y = "Averted deaths", 
       fill = "Scenario") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))
#Save final averted deaths
write.csv(averted_deaths_Tunisia,"Final_averted_deaths_Tunisia.csv",row.names = FALSE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Scenario_5: Pre-screened vaccination
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~>

averted_wasted_doses_Tunisia <- data.frame(
  Scenario = c("RI-SIA-5Y", "RI-SIA-2Y"),
  Averted_wasted_doses = c(
    -tail(results_Tunisia_scenario_1$cum_wasted_doses, 1) + tail(results_Tunisia_scenario_5$cum_wasted_doses, 1),
    
    tail(results_Tunisia_scenario_3$cum_wasted_doses, 1) - tail(results_Tunisia_scenario_5$cum_wasted_doses, 1)
  )
)
# Plot of averted_wasted_doses_Tunisia
library(ggplot2)

ggplot(averted_wasted_doses_Tunisia, aes(x = Scenario, y =  Averted_wasted_doses, fill = Scenario)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d() + 
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, accuracy = 1, suffix = "M ")) +  
  labs(title = " ",
       x = "Scenario",
       y = "Averted wasted doses/Pre-screening", 
       fill = "Scenario") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))
#Save final averted deaths
write.csv(averted_wasted_doses_Tunisia,"Final_averted_wasted_doses_Tunisia.csv",row.names = FALSE)
#results_Tunisia_scenarios<-rbind(results_Tunisia,
#                                     results_Tunisia_scenario_1,
#                                     results_Tunisia_scenario_2,
#                                     results_Tunisia_scenario_3,
#                                     results_Tunisia_scenario_4
#) 
#write.csv(results_Tunisia_scenarios,"0.results_Tunisia_scenarios.csv",row.names = FALSE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# A. EPIDEMIC CONTROL ASSESSMENT: Rt data visualization
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Visualization of Rt-data
Rt_data_Tunisia$scenario<-"Baseline"
dim(Rt_data_Tunisia)
Rt_data_Tunisia_scenario_1$scenario<-"RI-SIA-2Y"
dim(Rt_data_Tunisia_scenario_1)
Rt_data_Tunisia_scenario_2$scenario<-"RI-scaled-up"
dim(Rt_data_Tunisia_scenario_2)

Rt_data_Tunisia_scenario_3$scenario<-"RI-SIA-5Y"
dim(Rt_data_Tunisia_scenario_3)
Rt_data_Tunisia_scenario_4$scenario<-"RI-Reduced"
dim(Rt_data_Tunisia_scenario_4)
Rt_data_Tunisia_scenarios<-rbind(Rt_data_Tunisia,
                               Rt_data_Tunisia_scenario_1,
                               Rt_data_Tunisia_scenario_2,
                               Rt_data_Tunisia_scenario_3,
                               Rt_data_Tunisia_scenario_4
)

#Checks
x<-Rt_data_Tunisia_scenarios
table(x$scenario)
x_0<-Rt_data_Tunisia_scenarios|>
  filter(scenario=="Baseline")
x_1<-Rt_data_Tunisia_scenarios|>
  filter(scenario=="RI-SIA-2Y")
x_2<-Rt_data_Tunisia_scenarios|>
  filter(scenario=="RI-scaled-up")
x_3<-Rt_data_Tunisia_scenarios|>
  filter(scenario=="RI-SIA-5Y")

x_4<-Rt_data_Tunisia_scenarios|>
  filter(scenario=="RI-Reduced")
summary(x_0$R_eff)
summary(x_1$R_eff)
summary(x_2$R_eff)
summary(x_3$R_eff)
summary(x_4$R_eff)
# R_eff lines together
plot(x_0$R_eff, type = "l", col = "red", lwd = 2,
     ylim = range(c(x_0$R_eff, x_1$R_eff, x_2$R_eff, x_3$R_eff, x_4$R_eff)),
     xlab = "Index", ylab = "R_eff", main = "Rt")

lines(x_1$R_eff, col = "blue", lwd = 2)   #RI-SIA-2Y
lines(x_2$R_eff, col = "green", lwd = 2)  #RI-scaled-up
lines(x_3$R_eff, col = "purple", lwd = 2) #RI-SIA-5Y
lines(x_4$R_eff, col = "orange", lwd = 2) #RI-reduced
legend("topright",
       legend = c("Baseline", "RI-SIA-2Y", "RI-scaled-up", "RI-SIA-5Y", "RI-reduced"),
       col = c("red", "blue", "green", "purple", "orange"),
       lty = 1, lwd = 2)

#Year in thousand
Rt_data_Tunisia_scenarios$year <- as.numeric(2000 + Rt_data_Tunisia_scenarios$time_years)
write.csv(Rt_data_Tunisia_scenarios,"Final_Rt_data_Tunisia_scenarios.csv",row.names = FALSE)
#Visualization
library(ggplot2)
table(Rt_data_Tunisia_scenarios$scenario)
Final_Rt_Tunisia<-ggplot(Rt_data_Tunisia_scenarios, aes(x = year, y = R_eff, color = scenario)) +
  # Red rectangle at the bottom (y from min to 1)
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 1, alpha = 0.3, fill = "#ffcccc") +
  # Red rectangle at the top (y from 1 to max)
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 1, ymax = Inf, alpha = 0.3, fill = "#ccffcc") +
  geom_line(size = 0.8) +
  geom_vline(xintercept = 2019, linetype = "dashed", color = "red", linewidth = 0.7) +
  geom_vline(xintercept = 2025, linetype = "dashed", color = "blue", linewidth = 0.7) +
  annotate("text", x = 2014.02, y = 0.99 * max_Reff, label = "Pre-COVID-19", hjust = 0, size = 2, color = "darkgreen") +
  annotate("text", x = 2020.02, y = 0.99 * max_Reff, label = "COVID-19", hjust = 0, size = 2, color = "red") +
  annotate("text", x = 2026.02, y = 0.99 * max_Reff, label = "2025-2035 horizon", hjust = 0, size = 2, color = "blue") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "darkred", linewidth = 0.7) +
  labs(
    title = expression("Measles time-varying " ~ R[t] ~ "in Tunisia"),
    subtitle = "Tunisia - Scenario Analysis",
    x = "Year",
    y = expression(R[t]),
    color = "Scenario"
  ) +
  scale_x_continuous(breaks = seq(2000, 2035, 1), limits = c(2000, 2035)) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 8),
    axis.text.x = element_text(size = 4, angle = 90, hjust = 1),
    legend.position = "bottom",
    legend.title = element_text(size = 8), #scenario
    legend.text= element_text(size = 8)    # baseline,...
  ) +
  guides(color = guide_legend(
    override.aes = list(size = 5)
  ))
print(Final_Rt_Tunisia)
ggsave("Final_Rt_Tunisia.png",plot=Final_Rt_Tunisia,height = 10, width = 12,dpi = 200 )
#At the end of each code. I saved the workenvironment in order to be re-used without re-running
#save.image(file = "Final_1_Tunisia.RData")
bb<- Sys.time()# start timer
bb-aa
