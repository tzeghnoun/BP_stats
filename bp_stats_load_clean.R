# Script to download BP Statistical Reveiew from an Excel file in one list.
# And creating few function to help clean and manage the different dt included in the
# downloaded list for our dashboard report.

# Start by cleaning up memory of current R session:
rm(list=ls(all=TRUE))

# Load necessary packages.
library(rio) 
library(lubridate) 
library(data.table)
library(tidyverse)
library(ggExtra)
library(viridis)
library(ggthemes)
library(countrycode)
library(RColorBrewer)
library(ggmap)
library(mapproj)
library(ggdark)
library(highcharter)
library(plotly)
library(dygraphs)
library(rjson) 
library(formattable)
library(kableExtra)
library(ggthemes)
library(ggdark)


# Loading and cleaning datasets

# importing all the sheets at ones. The output (bp_stats) is a list of df
bp_stats <- import_list("data/bp-stats-review-2019-all-data.xlsx", skip = 2)

# Create a function to converte list to df
# Courtesy Hadley W.
list_to_df <- function(listfordf){
  if(!is.list(listfordf)) stop("it should be a list")
  
  df <- list(list.element = listfordf)
  class(df) <- c("tbl_df", "data.frame")
  attr(df, "row.names") <- .set_row_names(length(listfordf))
  
  if (!is.null(names(listfordf))) {
    df$name <- names(listfordf)
  }
  
  df
}
 

# Converting the generated list to a list of df & tidying the names of the df

# Converting the list
bp_stats <- list_to_df(bp_stats)

# Replace space with underscore
bp_stats$name <- tolower(bp_stats$name)
bp_stats$name <- str_replace_all(bp_stats$name,"-","") 
bp_stats$name <- str_replace_all(bp_stats$name,"\\s+","_")
 
# Creating a function to gather years (Transforming df from a wide to a long.)
# df that are concerned by this function are similar to the primary energy consumption df structure). 
# Those df have the following index in the bp_stats list: c(2, 4, 6, 7, 8, 9, 10, 11, 12, 13, 17, 18, 24, 25, 26, 27, 28, 29, 30, 33, 34, 38, 39, 40, 44, 45, 46, 47, 48, 49, 51, 52, 53, 54, 55, 56, 57, 58, 59, 61, 62, 63, 64, 65, 66, 67, 68, 69)

wide_to_long_1 <- function(i) {
# setting few index to check the lenght of the year vector to 2018
d <- 0
inx <- 0
tmp <- 0
idx <- ncol(bp_stats$list.element[[i]])
tmp <- stringr::str_length(names(bp_stats$list.element[[i]][idx]))
# checking the length of the columns (here it's a list of years in character from
# 1965 (or so) to 2018 + few columns that have been parsed and look like :"year...56" 
# including 2018...55). We need to keep 2018 & get rid of the unused columns.
while (tmp > 4) {
  idx <- idx - 1
  tmp <- stringr::str_length(names(bp_stats$list.element[[i]][idx])) 
  d <- d + 1
} 
# setting the index of the lenght to use to subset the df
x <- ncol(bp_stats$list.element[[i]]) - d + 1
bp_stats$list.element[[i]] <- bp_stats$list.element[[i]][, 1:x]

# gathering and tidying the df
bp_stats$list.element[[i]] <- bp_stats$list.element[[i]] %>% 
  gather(year, values, 2:ncol(bp_stats$list.element[[i]])) %>% 
  setNames(c("country", "year", paste0(bp_stats$name[i]))) %>% 
  filter(!is.na(country), !is.na(year), !is.na(paste0(bp_stats$name[i]))) %>% 
  mutate(year = str_sub(year, 1, 4),
         year = as.integer(year))
bp_stats$list.element[[i]] <- as.data.table(bp_stats$list.element[[i]])
}

# Create function that adds regions to the df
get_regions <- function(df) {
  
  # check if the variable year exists in the df then add the index
  if("year" %in% colnames(df))
  {
    df_region <- df %>% .[year== 2018, 
                          list(country = country,
                               idx = if_else(str_detect(country, "^Total"), .I, 0L))] %>% 
      add_row()
  } else { # add the index column
    df_region <- df %>% .[, 
                          list(country = country,
                               idx = if_else(str_detect(country, "^Total"), .I, 0L))] %>% 
      add_row()
  }
  # create a country & region tibble 

  # Adding regions
  i <-  nrow(df_region) - 1  # will start the process of indexing the countries regarding there region. Starting from the last row (that's why I created the additional row) 
  # feeling the countries index with there region index (replacing the 0 values of the region index) 
  while (i > 0) {
    df_region$idx[i] = ifelse(df_region$idx[i] == 0, df_region$idx[i+1], df_region$idx[i])
    i <- i - 1  
  } 

  # cleaning the df. Removing the Totals and the empty rows and also the index column  
  df_region <- df_region %>% 
    .[!is.na(idx), list(country = country, region = country[idx])] %>% 
    .[, list(country = country, region = str_replace(region, "^Total(.+)", "\\1"))] %>% 
    .[!str_detect(country, "^Total")]
  
  # Adding the region column to oil_production df by joing them
  df <- merge(df, df_region, by = "country")
  setkey(df, region)
}

# adding function to add a variable column for each region & Algeria
get_color <- function(dt) {
  dt <- dt[, color := NA_character_]
  dt <- dt[, color := ifelse(str_detect(region, "Africa"), "#A65628", color)
           ][, color := ifelse(str_detect(region, "Asia Pacific"), "#FFFF33", color)
             ][, color := ifelse(str_detect(region, "CIS"), "red", color)
               ][, color := ifelse(str_detect(region, "Europe"), "#377EB8", color) 
                 ][, color := ifelse(str_detect(region, "Middle East"), "#66A61E", color)
                   ][, color := ifelse(str_detect(region, "North America"), "#7570B3", color)
                     ][, color := ifelse(str_detect(region, "Cent"), "#A6761D", color)
                       ][, color := ifelse(str_detect(country, "Algeria"), "#D95F02", color)]
}
