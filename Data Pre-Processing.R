#package installation
install.packages("ggplot2")
install.packages("dplyr")
install.packages("ggpubr")
install.packages("ggpol")
install.packages("scales")
install.packages("ggrepel")
install.packages("lubridate")
install.packages("ggthemes")
install.packages("ggpmisc")

#Package Loading
library(ggplot2)
library(dplyr)
library(ggpubr)
library(ggpol)
library(scales)
library(ggrepel)
library(lubridate)
library(ggthemes)
library(ggpmisc)

# import and store the data set in employee_attrition
employee_attrition <- read.csv("C:/Users/Tracy Khor/Desktop/CAMPUS/PFDA/PFDA/employee_attrition.csv",header = TRUE)
#order the status year-> keeps the latest record
employee_attrition = employee_attrition[order(employee_attrition$STATUS_YEAR,decreasing = TRUE),]
#REMOVE DUPLICATE DATA
employee_attrition = employee_attrition[!duplicated(employee_attrition$EmployeeID),]


# display data
View(employee_attrition)
View(Date)


Date <- data.frame(EmployeeID = employee_attrition$EmployeeID,
                   recordYear=as.Date(employee_attrition$recorddate_key,format= "%m/%d/%Y"),
                   birthYear=as.Date(employee_attrition$birthdate_key,format= "%m/%d/%Y"),
                   orighireYear=as.Date(employee_attrition$orighiredate_key,format= "%m/%d/%Y"),
                   terminationYear = as.Date(employee_attrition$terminationdate_key,format= "%m/%d/%Y"))
Date <- Date %>% 
  mutate(recordYear= lubridate::year(recordYear),
         birthYear=lubridate::year(birthYear),
         orighireYear= lubridate::year(orighireYear),
         terminationYear=lubridate::year(terminationYear)
  ) %>%
  mutate(range = case_when(
    between(birthYear, 1940, 1949) ~ "1940s",
    between(birthYear, 1950, 1959) ~ "1950s",
    between(birthYear, 1960, 1969) ~ "1960s",
    between(birthYear, 1970, 1979) ~ "1970s",
    between(birthYear, 1980, 1989) ~ "1980s",
    between(birthYear, 1990, 1999) ~ "1990s"))%>%
  mutate(age_range = case_when(
    between(employee_attrition$age, 19, 20) ~ "below 21",
    between(employee_attrition$age, 21, 25) ~ "21-25",
    between(employee_attrition$age, 26, 30) ~ "26-30",
    between(employee_attrition$age, 31, 35) ~ "31-35",
    between(employee_attrition$age, 36, 40) ~ "32-40",
    between(employee_attrition$age, 41, 45) ~ "41-45",    
    between(employee_attrition$age, 46, 50) ~ "46-50",
    between(employee_attrition$age, 51, 55) ~ "51-55",
    between(employee_attrition$age, 55, 60) ~ "56-60",
    between(employee_attrition$age, 61, 65) ~ "61-65"))


employee_attrition <- merge(employee_attrition,Date,by="EmployeeID")

employee_attrition <- subset(employee_attrition, select = -c(gender_short)) #drop column 

names(employee_attrition)[12] <- "Gender"



# Data Exploration
summary(employee_attrition)   #access employee_attrition data
names(employee_attrition)     #view the column name in employee_attrition
str(employee_attrition)       #used to neatly present a R object's internal structure
attributes(employee_attrition)#display the names, class and etc
employee_attrition[1:5,]      #display the first five rows
head(employee_attrition)      #display the first six rows
tail(employee_attrition)      #display the last six rows

#count the total of female and male
employee_attrition |>
  group_by(Gender) |>
  summarise(
    count=n()) |>
  knitr::kable()

#count job_title
employee_attrition |>
  group_by(job_title) |>
  summarise(
    count=n()) |>
  knitr::kable()

#count age
employee_attrition |>
  group_by(age) |>
  summarise(
    count=n()) |>
  knitr::kable()

#count STATUS_YEAR
employee_attrition |>
  group_by(STATUS_YEAR) |>
  summarise(
    count=n()) |>
  knitr::kable()

#count STATUS
employee_attrition |>
  group_by(STATUS) |>
  summarise(
    count=n()) |>
  knitr::kable()

#count BUSINESS_UNIT
employee_attrition |>
  group_by(BUSINESS_UNIT) |>
  summarise(
    count=n()) |>
  knitr::kable()

#count length_of_service
employee_attrition |>
  group_by(length_of_service) |>
  summarise(
    count=n()) |>
  knitr::kable()

#count city_name
employee_attrition |>
  group_by(city_name) |>
  summarise(
    count=n()) |>
  knitr::kable()

#count department_name
employee_attrition |>
  group_by(department_name) |>
  summarise(
    count=n()) |>
  knitr::kable()

#count store_name
employee_attrition |>
  group_by(store_name) |>
  summarise(
    count=n()) |>
  knitr::kable()

#count termreason_desc
employee_attrition |>
  group_by(termreason_desc) |>
  summarise(
    count=n()) |>
  knitr::kable()

#count termtype_desc
employee_attrition |>
  group_by(termtype_desc) |>
  summarise(
    count=n()) |>
  knitr::kable()

#count store_name, city_name
employee_attrition |>
  group_by(store_name,city_name) |>
  summarise(freq=n()) |>
  knitr::kable()

#count store name, business unit
employee_attrition |>
  group_by(BUSINESS_UNIT,store_name) |>
  summarise(freq=n()) |>
  knitr::kable()

#count store name, business unit, city_name
employee_attrition |>
  group_by(store_name,BUSINESS_UNIT,city_name) |>
  summarise(freq=n()) |>
  knitr::kable()

