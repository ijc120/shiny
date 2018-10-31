library(shiny)
library(leaflet)
library(dplyr)
library(tidyr)
library(tidyverse)
library(markdown)
library(googleVis)
library(shinydashboard)
library(plotly)
library(shinythemes)
library(DT)

# load datasets
d1 <- readr::read_csv("CDC.csv")
data <- d1 %>% rename(year = "Edition",measure = "Measure Name",state = "State Name")
data <- data %>% select(.,-2,-8:-11)
d2 <- readr::read_csv("ACSreport.csv")
df2<- d2[,!grepl("Margin of Error",names(d2))]
df2<-select(df2, 3,11:13,23,182:186,200:212,219,314)
df2<- df2 %>% rename(Median_Household_Income="Estimate; INCOME IN THE PAST 12 MONTHS ( INFLATION-ADJUSTED DOLLARS) - Households - Median household income (dollars)",Male="Estimate; SEX AND AGE - Total population - Male",
                     Female="Estimate; SEX AND AGE - Total population - Female",
                     state= "Geography",
                     Median_Age="Estimate; SEX AND AGE - Median age (years)",
                     Totalpop="Estimate; SEX AND AGE - Total population")

df<- left_join(df2,data,by=c('state',"year"))

df <- separate(df, measure, c("cat", "subcat"), sep = " - ")
df$subcat <- df$subcat %>% replace(., is.na(.), "All")
lis <- df$cat %>% unique()
# clean dataset- subcateogry- income,age,gender and education
df$subcat <- sub(' than ',' Than ', df$subcat)
df$subcat <-sub('25-','25,000-', df$subcat)
df$subcat <-sub('50-','50,000-', df$subcat)
income_lis <- df[grep("\\$", df$subcat),] %>% select(.,subcat) %>% unique()
df$subcat <-sub('Aged ','', df$subcat)
df$subcat <-sub(' to ','-', df$subcat)
age <- df[grep("^[0-9]", df$subcat),] %>% select(.,subcat) %>% unique()
gender_lis <- df[grep("*ale", df$subcat),] %>% select(.,subcat) %>% unique()
df$subcat <-sub('HS','High School', df$subcat)
df$subcat <-sub('Females','Female', df$subcat)
df$subcat <-sub('Males','Male', df$subcat)
df$subcat <-sub(' Graduate',' Grad', df$subcat)
e1 <- df[grepl("School", df$subcat),] %>% select(.,subcat) %>% unique()
e2 <- df[agrep("College", df$subcat),] %>% select(.,subcat) %>% unique()
edu_lis <- rbind(e1,e2)
geo1 <- df[grep("ur", df$subcat),] %>% select(.,subcat) %>% unique()
geo2 <- df[grep("Ur", df$subcat),] %>% select(.,subcat) %>% unique()
st_lis <- rbind(geo1,geo2)

# subset dataframe and remove unwanted variables
df1 <- subset(df, subcat!="Hawaiian PI"& subcat!="White Race"& subcat!="API Race"&subcat!="AIAN Race"&subcat!="Black Race"&subcat!="Below Poverty"&subcat!="American Indian/Alaskan Native"&subcat!="Asian"&subcat!="American Indian/Alaska Native"&subcat!="Black/African American"&subcat!="Hispanic/Latino"&subcat!="Asian/Pacific Islander"&subcat!="American Indian"&subcat!="Mother Age 15-19"&subcat!="Mother Age 25-29"&subcat!="Mother Age 35-39"&subcat!="Mother Age 20-24"&subcat!="Mother Age 30-34"&subcat!="Mother Age 40-44"&subcat!="Mother 20-24"&subcat!="Mother 30-34"&subcat!="Mother 40-44"&subcat!="White"&subcat!="Am. Indian"&subcat!="Mother 25-29"&subcat!="Youth"&subcat!="Mother 35-39"&subcat!="Mother 15-19" &subcat!="Adolescents"&subcat!="Native Hawaiian/ PI"&subcat!="Black"&subcat!="Hawaiian/Pacific Islander"&subcat!="Multiracial"&subcat!="Children"&subcat!="Above Poverty"&subcat!="Hispanic"&subcat!="Other Race")
df1 <- subset(df1, cat=="Binge Drinking"|cat=='Cancer Deaths'|cat=="Cardiovascular Deaths"|cat=="Chronic Drinking"|cat=="Diabetes"|cat=="Drug Deaths"|cat=="Excessive Drinking"|cat=="Frequent Mental Distress"|cat=="Frequent Physical Distress"|cat=="Heart Attack"|cat=="Heart Disease"|cat=="High Blood Pressure"|cat=="High Cholesterol"|cat=="Insufficient Sleep"|cat=="Obesity"|cat=="Poor Mental Health Days"|cat=="Poor Physical Health Days"|cat=="Smoking"|cat=="Suicide"|cat=="Physical Inactivity"|cat=="Stroke")
df1$subcat %>% unique()
df1$cat %>% unique()
# 188:192,194:198
industry <- df1 %>% select(contains("INDUSTRY"))
industry_lis <- sub('.* - ', '', colnames(industry)) %>% unique()
occupation <- df1 %>% select(contains("OCCUPATION"))
occupation_lis <- sub('.* - ', '', colnames(occupation))

    
    # function to get value for each subcategory
sub_filter <- function(D,Y,C,SC){
  D %>%
    as.data.frame(.) %>%
    filter(.,year==Y) %>% 
    filter(.,cat==C) %>% 
    filter(.,subcat==SC) %>%
    select(.,Value)}
    # funcation to get value for category
cat_filter <- function(data,Year,Measure){
  data  %>%
    as.data.frame(.) %>%
    filter(.,year==Year) %>%
    filter(.,cat== Measure ) %>%
    filter(.,subcat=="All")%>% 
    select(.,Value)}

barplot_filter <- function(data,Year,State,Measure,M){
  data  %>%
    as.data.frame(.) %>%
    filter(.,year==Year) %>%
    filter(.,state==State) %>% 
    filter(.,cat== Measure ) %>%
    filter(.,subcat==M) %>% 
    select(.,Value, year,state,cat,subcat)}

  
adddf <- df1 %>% filter(.,subcat=="All") %>% select(state,cat,Value,year) %>% spread(cat, Value)
newdf <- left_join(df1,adddf,by=c('state',"year"))
#newdf <- newdf %>% rename(Poor_mental_health="Poor Mental Health Days")
measure_lis <- unique(newdf$cat)
result_lis <- measure_lis[c(2,3,5,8,9,10,11,12,13,17,18,20)]
factor_lis <- measure_lis[c(1,4,6,7,14,15,16,19)]
year_lis <- unique(newdf$year)
geo_lis <- unique(newdf$state)
sub_lis <- unique(newdf$subcat)
state_lis <- unique(newdf$state)

measures <-  newdf %>% select(31:49) %>% na.omit()
correlation <- round(cor(measures), 3)
nms <- names(measures)

bar_filter <- function(data,Y,State,Measure){
data %>%
    filter(.,year==Y) %>% 
    filter(.,state==State) %>% 
    filter(.,cat==Measure) %>% 
    pull(.,Value)}
# create dataframes for bar charts
geodata <- newdf %>% filter(subcat %in% st_lis[[1]]) %>% select(year,Value,cat,subcat,state)
gendata <- newdf %>% filter(subcat %in% gender_lis[[1]]) %>% select(year,Value,cat,subcat,state)
edudata <- newdf %>% filter(subcat %in% edu_lis[[1]]) %>% select(year,Value,cat,subcat,state)
incomedata <- newdf %>% filter(subcat %in% income_lis[[1]]) %>% select(year,Value,cat,subcat,state)
agedata <- newdf %>% filter(subcat %in% age[[1]]) %>% select(year,Value,cat,subcat,state)
