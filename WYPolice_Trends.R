# Libraries
library(visdat)
library(tidyverse)

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
  .id = "Month"
)

rm(Apr_Crimes, May_Crimes, Jun_Crimes, Jul_Crimes, Aug_Crimes, Sep_Crimes, column_types, Crimes_Data, dataset_list, df, missing_columns, type_matrix, col, col_types, column_names, name)


