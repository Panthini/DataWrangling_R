
#TASK1 (1)

# Tidy data is a standard structural data to map the meaning of dataset. 
# Weather the dataset is tidy or messy, is depending on the matching of rows, 
# columns and tables with the observations and variable types (Wickham, 2014). 
# Sports_and_recreation dataset in its current form is not considered to be in 
# tidy format due to few reasons:
#   .	Variables are stored in both rows and columns, in reality variables should 
#     be stored in column and observations should be stored in rows.
#   .	Columns in the same table contain multiple types of observations and variables.
#   .	Multiple variables are stored in one column.
#   .	Multiple observations are stored in a single column.
#   .	Values should be in cells, not header.


install.packages("devtools")
install.packages("CRAN")

#tibble package to use data frame from tidyverse version
install.packages("tbl_df")


#loading a dataset from cran-r packages
install.packages("corrDNA")
library(corrDNA)

install.packages("plyr")
library(plyr)


#TASK1 (2) 
install.packages("tidyverse")
library(tidyverse)
library(tibble)
library(readxl)
library(stringr)

ColName_before <- c("Facility_Id","FacilityName","Address","Latlon","LGA","SportsPlayed",
                    "NumberFieldCourts","FieldSurfaceType","FacilityAge","FacilityCondition",
                    "X1","X2","X3","X4","X5","X6","X7")

colName_after <- c("FacilityID", "FacilityName","StreetNo","StreetName","StreetType","Suburb",
              "PostalCode","LGA","Lat","Lon","SportsPlayed","NumberFieldCourts","FieldSurfaceType",
              "Age","Condition","UpgradeAge")

# (a) read excel file
sports <- read_excel("F:/Panthini PC/Study MS/Subjects/Sem 2/Data Wrangling and R/Assignment 3/sports_and_recreation.xlsx", col_names = ColName_before)

# reading data into data frame
as.tibble(sports)

# (b)

#Running a summary of the data framw to identify missing values
summary(sports_and_recre)
  
#Deleting empty rows from the data set
sports_new <- sports[-c(1,3972,12120),]

# (c) 
# Creating a new column with a unique identifier for each facility ID
sports_new <- sports_new %>% 
  mutate(row = rep(1:4866, each = 6))


#Gathering all columns
sports_new2 <- sports_new %>% 
  gather(column.name, sp_play, NumberFieldCourts:X7)

# (d)
#  Spreading the variable data in rows in to seperate columns
sports_new2 <- sports_new2 %>% 
  spread(SportsPlayed, sp_play, convert = TRUE)

sports_new2 <- sports_new2 %>% 
  drop_na(SportsPlayed)

#Deleting extra coloumns
sports_new2 <- subset(sports_new2, select = -c(row,column.name))


sports_new3 <- sports_new2 %>%
  separate(Latlon, c("Lat", "Lon"), sep = ",", extra = "drop")


### TASK1 (2e)
sports_new3$Lat <- gsub("(","",sports_new3$Lat,fixed = TRUE)
sports_new3$Lon <- gsub(")","",sports_new3$Lon,fixed = TRUE)

sports_new4 <- sports_new3


### TASK1 (2f) spliting the  adress
sports_new4$StreetNo <- gsub("[[:alpha:]]","",sports_new3$streetName)
sports_new4$StreetName <- gsub("[[:digit:]]","",sports_new3$streetName)
sports_new4$StreetType <- ""
sports_new4$Suburb <- gsub("[[:digit:]]","",sports_new3$Suburb)
sports_new4$PostalCode<- gsub("[[:alpha:]]","",sports_new3$Suburb)


### TASK1 (2g) include the header row to dataframe
sports_new4 <- sports_new4 %>%
  rename( Sports_Played = SportsPlayed,
          Number_Field_Courts = NumberFieldCourts ,
          Field_Surface_Type = FieldSurfaceType ,
          Facility_Age = FacilityAge,
          Facility_Condition = FacilityCondition,
          Facility_Upgrade_Age = FacilityUpgradeAge
  )

sports_new4 <- sports_new4[ ,colName_after]

### TASK1 (2h) summary of dataframe
summary(sports_new4)

### TASK1 (2i) sorting dataframe by Facility_Id and then Sports_Played
sports_new4 <- sports_new4[order(sports_new4$Facility_Id),]
sports_new4 <- sports_new4[order(sports_new4$Sports_Played),]

### TASK1 (2j) writing the result to a csv file
write.csv(sports_new4, file = "sport_new4.csv", na = "")

### TASK1 (3)
softball_atleast_5fields <- count(filter(sports_new4, Sports_Played == "Softball" &
                                        Number_Field_Courts >= 5))

########################################################################################################

#TASK2

install.packages("lubridate")
library(lubridate)
library(ddply)

# package to use %>%
install.packages("magrittr")
install.packages("dplyr")
library(magrittr) 
library(dplyr)

install.packages("geosphere")
library(geosphere)

## (1)
list_dataset  <- read.csv("F:/Panthini PC/Study MS/Subjects/Sem 2/Data Wrangling and R/Assignment 3/listings.csv", header = TRUE)
summary(list_dataset)

list_dataset %>%
  group_by(room_type) %>%
  summarise(average = mean(price))

## (2a)
list_dataset$name[grepl("Cosy.*Spacious|Spacious.*Cosy|cosy.*spacious|spacious.*cosy|
                  Cosy.*spacious|Spacious.*cosy", list_dataset$name)]

## (2b)
list_dataset$name[grepl("Vibrant.*Bright|Bright.*Vibrant|vibrant.*bright|bright.*vibrant|
                  Vibrant.*bright|vibrant.*Bright", list_dataset$name)]

## (3)

list_dataset$last_review <- as.Date(list_dataset$last_review)

FirstDay <- ymd("2017-12-24")
SecondDay <- ymd("2018-01-06")

(date_interval <- (FirstDay %--% SecondDay))

## (4) 

str(list_dataset$host_name)

host <- count(list_dataset,host_name, sort = TRUE, name = "max_host")
length(host$max_host[host$max_host > 10 & host$max_host <= 20])


## (5)

AverageNoReview <- list_dataset %>%
  group_by(neighbourhood) %>%
  summarise(Average = mean(number_of_reviews))

top_n(AverageNoReview,5)

AverageAvailability <- list_dataset %>%
  group_by(neighbourhood) %>%
  summarise(Average = mean(availability_365))

FinalAverage <- merge(AverageAvailability,AverageNoReview, by = "neighbourhood")

top_n(FinalAverage,5)

## (6)

x <- list_dataset$longitude
y <- list_dataset$latitude

co_ordinates <- cbind(x,y)

distance_km <- distHaversine(co_ordinates,c(144.9631,-37.8136), r = 6370)
list_dataset$distance_from_CBD <- distance_km

filtered_destination <- filter(list_dataset, distance_from_CBD >= 5, distance_from_CBD <= 10)
nrow(filtered_destination)

filtered_destination <- subset(filtered_destination, select = -c(neighbourhood_group))

############################################################################################################

## TASK3

install.packages("summarytools")
library(summarytools)

## (1)

summary(sports_new4)
summary(list_dataset)

summary(AverageAvailability)
summary(AverageNoReview)

## (2)
vector <- list_dataset$distance_from_CBD
hist(vector, main = " Distribution of distance from CBD in km", 
     xlab = "Distance from CBD (km)", col = "yellow")

## (3)

vector1 <- select(sports$SportsPlayed == "cricket")


