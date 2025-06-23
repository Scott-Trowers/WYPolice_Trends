theme_set(theme_classic())
set.seed(5)

# Libraries
library(visdat)
library(tidyverse)
library(sf)
library(ggplot2)
library(RColorBrewer)
library(viridis)


#### Pre Processing ####

# Load the data
Apr_Crimes <- read.csv("Datasets//2020-04//2020-04-west-yorkshire-street.csv")
May_Crimes <- read.csv("Datasets//2020-05//2020-05-west-yorkshire-street.csv")
Jun_Crimes <- read.csv("Datasets//2020-06//2020-06-west-yorkshire-street.csv")
Jul_Crimes <- read.csv("Datasets//2020-07//2020-07-west-yorkshire-street.csv")
Aug_Crimes <- read.csv("Datasets//2020-08//2020-08-west-yorkshire-street.csv")
Sep_Crimes <- read.csv("Datasets//2020-09//2020-09-west-yorkshire-street.csv")

dataset_list <- list(
  Apr_Crimes = Apr_Crimes,
  May_Crimes = May_Crimes,
  Jun_Crimes = Jun_Crimes,
  Jul_Crimes = Jul_Crimes,
  Aug_Crimes = Aug_Crimes,
  Sep_Crimes = Sep_Crimes
)

# April and July have less columns (12) than the other months (13)
# Identify missing columns:
column_names <- unique(unlist(lapply(dataset_list, names))) # list of all possible column
lapply(dataset_list, function(df) {
  setdiff(column_names, names(df))
})
    # April is missing 'Reported.by' and July is missing 'Falls.within'

# Check columns are in the same format across datasets:
unique(lapply(dataset_list, function(df) {
  sapply(df[intersect(names(df), column_names)], class)
}))
  # They are!

# Check dates align
for (df in dataset_list) {
  print(min(df$Month))
  print(max(df$Month))
  print(sum(is.na(df$Month)))
}
    # There are apparently no missing values in any of the month column, but there are blank values
    # Convert blank character values to NA:
dataset_list <- lapply(dataset_list, function(df) {
  df %>% mutate(across(where(is.character), ~na_if(., "")))
})
    
# Combine the datasets, matched using column name. Missing columns will automatically be populated with NAs. Create an extra column stating which dataset it came from
Crimes <- bind_rows(
  Apr = dataset_list[[1]],
  May = dataset_list[[2]],
  Jun = dataset_list[[3]],
  Jul = dataset_list[[4]],
  Aug = dataset_list[[5]],
  Sep = dataset_list[[6]],
  .id = "Dataset"
)

rm(Apr_Crimes, May_Crimes, Jun_Crimes, Jul_Crimes, Aug_Crimes, Sep_Crimes, dataset_list, df, column_names)


#### Explore the data ####
dim(Crimes)
  # 158k+ observations, 14 columns
vis_dat(Crimes, warn_large_data = FALSE, sort = FALSE)
  # X is an integer, Longitute and Latitude are numeric, and the rest are character.
  # There is missingness throughout the data, which needs to be explored
  # Context appears to be completely NA
vis_miss(Crimes, warn_large_data = FALSE, sort_miss = TRUE)
  # Confirms Context is completely NA and can be dropped
Crimes$Context <- NULL
  # Last.outcome.category and Crime.ID is 20% missing, spread throughout the data
  # Aside from Falls.within an Reported.by (which were completely missing for one particular month each), other values are fairly complete, ranging from 3% to 0% missing
  # From our original variables, only X is complete
  # A handful of observations appear to be responsible for the missing values in the other columns
vis_miss(Crimes, warn_large_data = FALSE, sort_miss = TRUE, facet = Dataset)
  # 7% of the remaining data is complete

sapply(Crimes, function(col) length(unique(na.omit(col))) == 1)
  # Falls.within and Reported.by are redundant, as they only take one value ("West Yorkshire Police")

sapply(Crimes, function(col) length(unique(na.omit(col))) - length(na.omit(col)) == 0)
  # No columns are completely unique
for (Mon in unique(Crimes$Dataset)) {
  df <- Crimes[Crimes$Dataset == Mon, ]
  print(Mon)
  print(
    sapply(df, function(col) length(unique(na.omit(col))) - length(na.omit(col)) == 0)
  )
}
  # But it is a unique identifier (as per the police data website) to each value per sub-dataset, so we can remove as it is also redundant
  # As per the data.police.uk website, LSOA.code and LSOA.name both relate to a defined neighbourhood - both are therefore not needed, and only the more descriptive will be kept
Crimes = Crimes %>% select(-c(X, Crime.ID, Reported.by, Falls.within))

head(Crimes)
#Single Column Explore: Crime.type, Last.outcome.category, Month/Dataset, LSOA.name, Location, Long/Lat
  #Crime.type
crime_counts <- table(Crimes$Crime.type, useNA = "ifany") %>%
  as.data.frame() %>%
  mutate(
    Crime_Type = as.character(Var1),
    Crime_Type = ifelse(is.na(Crime_Type), "Missing Data", Crime_Type)
  ) %>%
  select(Crime_Type, Frequency = Freq)

ggplot(
  data = crime_counts, 
  aes(x = Frequency, y = reorder(Crime_Type, Frequency))) +
  geom_bar(stat = "identity", fill="blue3") +
  geom_text(aes(label = Frequency), hjust = -0.1, size = 3) + 
  labs(
    title = "Distribution of Crime Types",
    subtitle = "West Yorkshire Police, Apr to Sept 2020",
    y = "Crime Type",
    x = "Frequency") +
  theme(legend.position = "none")
 

outcome_counts <- table(Crimes$Last.outcome.category, useNA = "ifany") %>%
  as.data.frame() %>%
  mutate(
    Outcome = as.character(Var1),
    Outcome = ifelse(is.na(Outcome), "Missing Data", Outcome)
  ) %>%
  select(Outcome, Frequency = Freq)

ggplot(
  data = outcome_counts, 
  aes(x = Frequency, y = reorder(Outcome, Frequency))) +
  geom_bar(stat = "identity", fill="blue3") +
  geom_text(aes(label = Frequency), hjust = -0.1, size = 3) + 
  labs(
    title = "Distribution of Outcomes",
    subtitle = "West Yorkshire Police, Apr to Sept 2020",
    y = "Outcome",
    x = "Frequency") +
  theme(legend.position = "none")


Month_counts <- table(Crimes$Month, useNA = "ifany") %>%
  as.data.frame() %>%
  mutate(
    Month = as.character(Var1),
    Month = ifelse(is.na(Month), "Missing Data", Month)
  ) %>%
  select(Month, Frequency = Freq)

ggplot(
  data = Month_counts, 
  aes(x = Frequency, y = Month)) +
  geom_bar(stat = "identity", fill="blue3") +
  geom_text(aes(label = Frequency), hjust = -0.1, size = 3) + 
  scale_y_discrete(limits=rev) + 
  labs(
    title = "Distribution of Observations by Month",
    subtitle = "West Yorkshire Police, Apr to Sept 2020",
    y = "Month",
    x = "Frequency") +
  theme(legend.position = "none")


LSOA_counts <- table(Crimes$LSOA.name, useNA = "ifany") %>%
  as.data.frame() %>%
  mutate(
    LSOA.name = as.character(Var1),
    LSOA.name = ifelse(is.na(LSOA.name), "Missing Data", LSOA.name)
  ) %>%
  select(LSOA.name, Frequency = Freq)

ggplot(
  data = LSOA_counts, 
  aes(x = Frequency, y = reorder(LSOA.name, Frequency))) +
  geom_bar(stat = "identity", fill="blue3") +
  geom_text(aes(label = Frequency), hjust = -0.1, size = 3) + 
  labs(
    title = "Distribution of Observations by LSOA Name",
    subtitle = "West Yorkshire Police, Apr to Sept 2020",
    y = "LSOA Name",
    x = "Frequency") +
  theme(legend.position = "none")


# Correct missing 'Month' Values (may need to adjust later commentary)
# Add comms to single variate analysis so far
# Fix mapping issue
# Explore location
# analyse missingness
# multi-variate analysis
# time series, ML (clustering, Correspondance Analysis), geo-spatial (and temporal?)






print("sss")




LSOAs <- st_read("Datasets//LSOAs//LSOA_2021_EW_BSC_V4.shp")
names(LSOAs)
ggplot(data = LSOAs) +
  geom_sf() 

points = na.omit(distinct(Crimes[,c("Longitude", "Latitude", "LSOA.name", "Dataset")]))

LSOAs <- st_transform(LSOAs, crs = 4326)
points_sf <- st_as_sf(points, coords = c("Longitude", "Latitude"), crs = 4326)

ggplot(LSOAs) + geom_sf()
ggplot(points_sf) + geom_sf()

ggplot(LSOAs) + geom_sf() + geom_sf(data=points_sf,aes(col=LSOA.name)) + theme(legend.position = "none")


head(Crimes)

# Examing data structure
# Explore missingness
