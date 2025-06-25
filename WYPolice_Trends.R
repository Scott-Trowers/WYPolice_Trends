# Libraries
library(visdat)
library(tidyverse)
library(sf)
library(ggplot2)
library(RColorBrewer)
library(viridis)
library(scales)

theme_set(theme_classic())
set.seed(5)


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
  print(paste("From", min(df$Month), "to", max(df$Month)))
  print(paste("NAs:", sum(is.na(df$Month))))
}
    # There are apparently no missing values in any of the month column, but there are blank values
    # Convert blank character values to NA:
dataset_list <- lapply(dataset_list, function(df) {
  df %>% mutate(across(where(is.character), ~na_if(., "")))
})
for (df in dataset_list) {
  print(paste("From", min(na.omit(df$Month)), "to", max(na.omit(df$Month))))
  print(paste("NAs:", sum(is.na(df$Month))))
}
    # NAs excluded, all the dates align correctly
    # Checking the police.data website gives confidence that the observations are from the relevant dataset, 
    # So we can take the month from the dataset name when missing
for (df_name in names(dataset_list)) {
  dataset_list[[df_name]]$Month <- paste0("2020-0", match(sub("_Crimes", "", df_name), month.abb))
  print(paste("From", min(dataset_list[[df_name]]$Month), "to", max(dataset_list[[df_name]]$Month)))
  print(paste("NAs:", sum(is.na(dataset_list[[df_name]]$Month))))
}
    
# Combine the datasets, matched using column name. Missing columns will automatically be populated with NAs. Create an extra column stating which dataset it came from
Crimes <- bind_rows(
  Apr = dataset_list[[1]],
  May = dataset_list[[2]],
  Jun = dataset_list[[3]],
  Jul = dataset_list[[4]],
  Aug = dataset_list[[5]],
  Sep = dataset_list[[6]],
)

rm(Apr_Crimes, May_Crimes, Jun_Crimes, Jul_Crimes, Aug_Crimes, Sep_Crimes, dataset_list, df, column_names, df_name)

#### Explore the data ####
dim(Crimes)
  # 158k+ observations, 13 columns
vis_dat(Crimes, warn_large_data = FALSE, sort = FALSE)
  # X is an integer, Longitude and Latitude are numeric, and the rest are character.
  # There is some missingness throughout the data, which needs to be explored
  # Context appears to be completely NA
vis_miss(Crimes, warn_large_data = FALSE, sort_miss = TRUE)
  # Confirms Context is completely NA and can be dropped
Crimes$Context <- NULL
  # Last.outcome.category and Crime.ID is 20% missing, spread throughout the data
  # Aside from Falls.within and Reported.by (which were completely missing for one particular month each), other values are fairly complete, ranging from 0% to 3% missing
  # Only X and Month are complete
  # A handful of observations appear to be responsible for the majority of missing values in the other columns
vis_miss(Crimes, warn_large_data = FALSE, sort_miss = TRUE, facet = Month)
  # 7.4% of the remaining data is missing

sapply(Crimes, function(col) length(unique(na.omit(col))) == 1)
  # Falls.within and Reported.by are redundant, as they only take one value ("West Yorkshire Police")

sapply(Crimes, function(col) length(unique(na.omit(col))) - length(na.omit(col)) == 0)
  # No columns are completely unique
for (Mon in unique(Crimes$Month)) {
  df <- Crimes[Crimes$Month == Mon, ]
  print(Mon)
  print(
    sapply(df, function(col) length(unique(na.omit(col))) - length(na.omit(col)) == 0)
  )
}
  # But X and Crime ID are a unique identifier (as per the police data website) to each value per month, so we can remove as it is also redundant
  # As per the data.police.uk website, LSOA.code and LSOA.name both relate to a defined neighbourhood - both are therefore not needed, and only the more descriptive will be kept
    #Remove redundant columns, convert Month to date, and convert latitude and longitude to sf coordinates
Crimes <- Crimes %>%
  select(-c(X, Crime.ID, Reported.by, Falls.within, LSOA.code)) %>%
  mutate(Month = as.Date(paste0(Month, "-01")))
  

# Re-review the data structure:
vis_dat(Crimes, warn_large_data = FALSE, sort = FALSE)
vis_miss(Crimes, warn_large_data = FALSE, sort_miss = TRUE)
vis_miss(Crimes, warn_large_data = FALSE, sort_miss = TRUE, facet = Month)
  # Only 4.7% of the data is now missing
  # The majority is from last.outcome.category, which has 20% missingness
  # There are a handful of observations which now only have a Month value, which can be removed
Crimes <- Crimes %>% filter(!if_all(-Month, is.na))

head(Crimes)
#Single Variate Explore: Crime.type, Last.outcome.category, Month, LSOA.name, Location, Long/Lat
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
  geom_segment(aes(x = 0, xend = Frequency, 
                   y = reorder(Crime_Type, Frequency), 
                   yend = reorder(Crime_Type, Frequency)),
               color = "grey80", linewidth = 1, linetype = "longdash") +
  geom_point(shape = 21, fill = "blue3", color = "black", size = 4) +
  geom_text(aes(label = comma(Frequency)), hjust = -0.3, size = 3) +
  scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Distribution of Crime Types",
    subtitle = "West Yorkshire Police, Apr to Sept 2020",
    y = "Crime Type",
    x = "Frequency") +
  theme(legend.position = "none")



  # Last.outcome.category
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
  geom_segment(aes(x = 0, xend = Frequency, 
                   y = reorder(Outcome, Frequency), 
                   yend = reorder(Outcome, Frequency)),
               color = "grey80", linewidth = 1, linetype = "longdash") +
  geom_point(shape = 21, fill = "blue3", color = "black", size = 4) +
  geom_text(aes(label = comma(Frequency)), hjust = -0.3, size = 3) +
  scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Distribution of Outcomes",
    subtitle = "West Yorkshire Police, Apr to Sept 2020",
    y = "Crime Type",
    x = "Frequency") +
  theme(legend.position = "none")

  # Months
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
  geom_text(aes(label = comma(Frequency)), hjust = -0.3, size = 3) +
  scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.1))) +
  scale_y_discrete(limits=rev) + 
  labs(
    title = "Distribution of Observations by Month",
    subtitle = "West Yorkshire Police, Apr to Sept 2020",
    y = "Month",
    x = "Frequency") +
  theme(legend.position = "none")

  # LSOA names
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
  geom_segment(aes(x = 0, xend = Frequency, 
                   y = reorder(LSOA.name, Frequency), 
                   yend = reorder(LSOA.name, Frequency)),
               color = "grey80", linewidth = 1, linetype = "longdash") +
  geom_point(shape = 21, fill = "blue3", color = "black", size = 4) +
  geom_text(aes(label = comma(Frequency)), hjust = -0.3, size = 3) +
  scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Distribution of Observations, by LSOA name",
    subtitle = "West Yorkshire Police, Apr to Sept 2020",
    y = "Crime Type",
    x = "Frequency") +
  theme(legend.position = "none")

# There are too many to visualise, let's just look at the top 20 by Frequency
LSOA_counts.Top20 <- LSOA_counts %>%
  arrange(desc(Frequency)) %>%
  slice_head(n = 20)

ggplot(
  data = LSOA_counts.Top20, 
  aes(x = Frequency, y = reorder(LSOA.name, Frequency))) +
  geom_segment(aes(x = 0, xend = Frequency, 
                   y = reorder(LSOA.name, Frequency), 
                   yend = reorder(LSOA.name, Frequency)),
               color = "grey80", linewidth = 1, linetype = "longdash") +
  geom_point(shape = 21, fill = "blue3", color = "black", size = 4) +
  geom_text(aes(label = comma(Frequency)), hjust = -0.3, size = 3) +
  scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Distribution of Observations, by LSOA name (Top 20)",
    subtitle = "West Yorkshire Police, Apr to Sept 2020",
    y = "Crime Type",
    x = "Frequency") +
  theme(legend.position = "none")

# We can also group the LSOAs into their wider areas
Crimes$LSOA.area <- sub("\\s\\d+[A-Z]$", "", Crimes$LSOA.name)
LSOA_Area_counts <- table(Crimes$LSOA.area, useNA = "ifany") %>%
  as.data.frame() %>%
  mutate(
    LSOA.area = as.character(Var1),
    LSOA.area = ifelse(is.na(LSOA.area), "Missing Data", LSOA.area)
  ) %>%
  select(LSOA.area, Frequency = Freq)

ggplot(
  data = LSOA_Area_counts, 
  aes(x = Frequency, y = reorder(LSOA.area, Frequency))) +
  geom_segment(aes(x = 0, xend = Frequency, 
                   y = reorder(LSOA.area, Frequency), 
                   yend = reorder(LSOA.area, Frequency)),
               color = "grey80", linewidth = 1, linetype = "longdash") +
  geom_point(shape = 21, fill = "blue3", color = "black", size = 4) +
  geom_text(aes(label = comma(Frequency)), hjust = -0.3, size = 3) +
  scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Distribution of Observations, by LSOA Area",
    subtitle = "West Yorkshire Police, Apr to Sept 2020",
    y = "Crime Type",
    x = "Frequency") +
  theme(legend.position = "none")

  # Locatiom
Location_counts <- table(Crimes$Location, useNA = "ifany") %>%
  as.data.frame() %>%
  mutate(
    Location = as.character(Var1),
    Location = ifelse(is.na(Location), "Missing Data", Location)
  ) %>%
  select(Location, Frequency = Freq) %>%
  arrange(desc(Frequency)) %>%
  slice_head(n = 40)

ggplot(
  data = Location_counts, 
  aes(x = Frequency, y = reorder(Location, Frequency))) +
  geom_segment(aes(x = 0, xend = Frequency, 
                   y = reorder(Location, Frequency), 
                   yend = reorder(Location, Frequency)),
               color = "grey80", linewidth = 1, linetype = "longdash") +
  geom_point(shape = 21, fill = "blue3", color = "black", size = 4) +
  geom_text(aes(label = comma(Frequency)), hjust = -0.5, size = 2.6) +
  scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Distribution of Observations, by Location",
    subtitle = "West Yorkshire Police, Apr to Sept 2020",
    y = "Crime Type",
    x = "Frequency") +
  theme(legend.position = "none")

# Lat/Long
#### Convert those with co-ordinates into an sf and plot
Crimes_sf <- Crimes %>%
  filter(!is.na(Latitude) & !is.na(Longitude)) %>%   
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

ggplot(Crimes_sf) + geom_sf()
# Many of the co-ordinates are impossible - England as a whole has a latitude between 49 and 56, and a longitude between -6.4 and 1.8
# But many points fall outside this, so we can remove their co-ordinates as they are inaccurate
Crimes <- Crimes %>%
  mutate(LatLong_Flag = (Latitude < 49 | Latitude > 56 | Longitude < -6 | Longitude > 2))

Crimes_sf2 <- Crimes %>%
  filter(!is.na(Latitude) & !is.na(Longitude)) %>%  
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

Crimes_sf2 %>%
  filter(!LatLong_Flag) %>%
  ggplot(aes(col = LatLong_Flag)) +
  geom_sf()

# more of the values seem to be misplaced, but colouring by LSOA.area may confirm this (i.e. if there are 'strays')
Crimes_sf2 %>%
  filter(!LatLong_Flag) %>%
  ggplot(aes(col = LSOA.area)) +
  geom_sf() + 
  theme(legend.position = "none")
#They look to be legitimate - although some remaining co-ordinates appear to be outside West Yorkshire, they are labelled correctly and may have been reported to WYP

Crimes_sf2 %>%
  filter(LatLong_Flag) %>%
  ggplot(aes(col = LSOA.area)) +
  geom_sf() 
# The majority of 'impossible' co-ordinates do have an LSOA.name, which we can use to impute the location (with a little noise to prevent duplicate locations)
Crimes_imputed <- Crimes %>%
  mutate(
    Latitude = if_else(LatLong_Flag == TRUE, NA, Latitude),
    Longitude = if_else(LatLong_Flag == TRUE, NA, Longitude)
  ) %>%
  group_by(LSOA.name) %>%
  mutate(
    Longitude = if_else(
      is.na(Longitude) & length(Longitude[!is.na(Longitude)]) > 0,
      sample(Longitude[!is.na(Longitude)], 1) + runif(1, -0.0005, 0.0005),
      Longitude
    ),
    Latitude = if_else(
      is.na(Latitude) & length(Latitude[!is.na(Latitude)]) > 0,
      sample(Latitude[!is.na(Latitude)], 1) + runif(1, -0.0005, 0.0005),
      Latitude
    )
  ) %>%
  ungroup()


# Correct missing 'Month' Values (may need to adjust later commentary)
# Add comms to single variate analysis so far
# Fix mapping issue
# Explore location
# analyse missingness
# multi-variate analysis
# time series, ML (clustering, Correspondance Analysis), geo-spatial (and temporal?)













# Examing data structure
# Explore missingness
