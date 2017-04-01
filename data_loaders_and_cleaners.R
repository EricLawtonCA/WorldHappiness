# This file contains functions for loading data from public sources and providing cleaning functions such as 
# * converting identifiers to canonical forms so that different sources can be matched
# * Filling in missing data from other sources

# All data is in local copies in .csv format, to save bandwidth in reading and extracting for every test run
# 
# All data is retrieved via a get_... function so that 
#  1. the analysis code does not have to changed every time a data source is changed or
#  2. a better source is found or
#  3. a better cleansing approach is found 

get_ISO_Countries <- function(){
  # This is my own "fudged" data set - ISO countries + shorter short name according to some other sites.
  return(read.csv("data/ISO_countries_plus.csv", stringsAsFactors=FALSE))
}

get_formatted_GDP <- function(){
  
  # read the GDP data from World Bank dataset and format it for use with other data.
  
   GDP <- read.csv("data/World dev indic 63a9470b-b534-4baa-ab0e-27997694143f_Data.csv", 
                   stringsAsFactors=FALSE)
   GDP <- subset(GDP, Series.Code != "") # Strip out the comment rows where this column is empty
   GDP[1:3] <- NULL #remove unwanted columns, we won't use their names for countries
 
  # substitute our standard country names (country_ISO_codes_names is a global variable),
  # using the ISO standard 3-character alphabetic code
   GDP <- merge(GDP,country_ISO_codes_names[c("country_name","Alpha.3.code")], 
                by.x = "Country.Code", by.y = "Alpha.3.code")
  
  # Rename the awkward, uselessly named columns that actually stand for years
   names(GDP) <- c("country_code","1990","2000","2007","2008","2009",
                                  "2010","2011","2011","2013","2014","2015","2016", 
                   "country_name")
  
  # change to long form - one row per year
   GDP <- melt(GDP, id.vars = c("country_name","country_code")) 
  # change name of "variable" column to "year" 
   names(GDP) <- c("country_name","country_code","year","value") 
   return(transform(GDP, value = ifelse(value == "..", NA, value)))
}
  
get_IMF_GDP_estimates <- function(){
  
  # Read the IMF GDP data, which has several different formats in one dataset. 
  # GDP per capita in US$ is best for our purposes 
  GDP <- subset(WEO_stats, WEO.Subject.Code == "NGDPDPC", select=c(ISO,X2005:X2016) )
  # Rename the columns to get rid of the Xs so just year names
  names(GDP) <- c("country_code","2005","2006","2007","2008","2009","2010",
                                 "2011","2012","2013","2014","2015","2016") 
  GDP <- melt(GDP, id.vars = "country_code")
  names(GDP) <- c("country_code","year","per_person_GDP")
  # Convert string-with-commas numeric
  GDP$per_person_GDP <- as.numeric(gsub(",", "", GDP$per_person_GDP)) 
  return(GDP)
}

get_WHR_data <- function(){
  # Get the World Happiness Report data and insert a column for country code for later matching
  
  # Read the raw data
  WHR_data <- read.csv("data/online+data+chapter+2+whr+2017.csv", stringsAsFactors=FALSE)
  
  # Reverse look-up to add the country codes for later matching with other data
  WHR_data <- merge(WHR_data,country_ISO_codes_names[c("country_name","Alpha.3.code")], 
                    by.x = "country", by.y = "country_name")
  colnames(WHR_data)[colnames(WHR_data) == "Alpha.3.code"] <- "country_code_3ltr"
  return(WHR_data)
}