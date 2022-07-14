require("httr")
require("rvest")

library(httr)
library(rvest)


get_wiki_covid19_page <- function() {
    
  # Our target COVID-19 wiki page URL is: https://en.wikipedia.org/w/index.php?title=Template:COVID-19_testing_by_country  
  # Which has two parts: 
    # 1) base URL `https://en.wikipedia.org/w/index.php  
    # 2) URL parameter: `title=Template:COVID-19_testing_by_country`, seperated by question mark ?
    
  # Wiki page base
  wiki_base_url <- "https://en.wikipedia.org/w/index.php"
  # You will need to create a List which has an element called `title` to specify which page you want to get from Wiki
  # in our case, it will be `Template:COVID-19_testing_by_country`
 
  # - Use the `GET` function in httr library with a `url` argument and a `query` arugment to get a HTTP response
    
  # Use the `return` function to return the response

}




# Call the get_wiki_covid19_page function and print the response
covid19_url <- "https://en.wikipedia.org/w/index.php?title=Template:COVID-19_testing_by_country" 
response <- GET(covid19_url) 
response

# Get the root html node from the http response in task 1 
root_node<-read_html("https://en.wikipedia.org/w/index.php?title=Template:COVID-19_testing_by_country")
root_node

# Get the table node from the root html node
table_node<-html_node(root_node,"table")
table_node


# Read the table node and convert it into a data frame, and print the data frame for review
covid19_data_frame<-html_table(table_node)
covid19_data_frame

# Print the summary of the data frame
summary(covid19_data_frame)

preprocess_covid_data_frame <- function(data_frame) {
    
    shape <- dim(data_frame)

    # Remove the World row
    data_frame<-data_frame[!(data_frame$`Country or region`=="World"),]
    # Remove the last row
    data_frame <- data_frame[-173, ]
    
    # We dont need the Units and Ref columns, so can be removed
    data_frame["Ref."] <- NULL
    data_frame["Units[b]"] <- NULL
    
    # Renaming the columns
    names(data_frame) <- c("country", "date", "tested", "confirmed", "confirmed.tested.ratio", "tested.population.ratio", "confirmed.population.ratio")
    
    # Convert column data types
    data_frame$country <- as.factor(data_frame$country)
    data_frame$date <- as.factor(data_frame$date)
    data_frame$tested <- as.numeric(gsub(",","",data_frame$tested))
    data_frame$confirmed <- as.numeric(gsub(",","",data_frame$confirmed))
    data_frame$'confirmed.tested.ratio' <- as.numeric(gsub(",","",data_frame$`confirmed.tested.ratio`))
    data_frame$'tested.population.ratio' <- as.numeric(gsub(",","",data_frame$`tested.population.ratio`))
    data_frame$'confirmed.population.ratio' <- as.numeric(gsub(",","",data_frame$`confirmed.population.ratio`))
    
    return(data_frame)
}



# call `preprocess_covid_data_frame` function and assign it to a new data frame
new_data_frame_covid19<-preprocess_covid_data_frame(covid19_data_frame)
new_data_frame_covid19

# Print the summary of the processed data frame again
summary(new_data_frame_covid19)

# Export the data frame to a csv file
write.csv(new_data_frame_covid19, file="covid.csv",row.names=FALSE)

# Get working directory
wd <- getwd()
# Get exported 
file_path <- paste(wd, sep="", "/covid.csv")
# File path
print(file_path)
file.exists(file_path)

## Download a sample csv file
# covid_csv_file <- download.file("https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0101EN-Coursera/v2/dataset/covid.csv", destfile="covid.csv")
# covid_data_frame_csv <- read.csv("covid.csv", header=TRUE, sep=",")

# Read covid_data_frame_csv from the csv file
read.csv("covid.csv")
# Get the 5th to 10th rows, with two "country" "confirmed" columns
subset_data_frame = new_data_frame_covid19[c(5:10),c('country','confirmed')]
subset_data_frame

# Get the total confirmed cases worldwide
total_confirmed=sum(new_data_frame_covid19$confirmed)
total_confirmed
# Get the total tested cases worldwide
total_tested_cases=sum(new_data_frame_covid19$tested)
total_tested_cases
# Get the positive ratio (confirmed / tested)
positive_ratio=sum(new_data_frame_covid19$confirmed.tested.ratio)
positive_ratio

# Get the `country` column
new_data_frame_covid19$country

# Check its class (should be Factor)
class(new_data_frame_covid19$country)

# Conver the country column into character so that you can easily sort them
as.character(new_data_frame_covid19$country)

# Sort the countries AtoZ
sort(new_data_frame_covid19$country)

# Sort the countries ZtoA
descending_list=sort(new_data_frame_covid19$country,decreasing=TRUE)

# Print the sorted ZtoA list
descending_list



# Use a regular expression `United.+` to find matches
matches=regexpr('United.+',new_data_frame_covid19$country)
# Print the matched country names
regmatches(new_data_frame_covid19$country,matches)


# Select a subset (should be only one row) of data frame based on a selected country name and columns
covid_data_frame_Angola <- new_data_frame_covid19 %>% select(country, confirmed,  confirmed.population.ratio) %>% filter(country == "Angola")
# Select a subset (should be only one row) of data frame based on a selected country name and columns
covid_data_frame_Argentina <- new_data_frame_covid19 %>% select(country, confirmed,  confirmed.population.ratio) %>% filter(country == "Argentina")
covid_data_frame_Angola


# Select a subset (should be only one row) of data frame based on a selected country name and columns


# Use if-else statement
# if (check which confirmed.population value is greater) {
#    print()
# } else {
#    print()
# }
if (covid_data_frame_Argentina$confirmed.population > covid_data_frame_Angola$confirmed.population) {
    print("Argentina has higher COVID-19 infection risk")
  } else {
    print("Angola has higher COVID-19 infection risk")
 }


# Get a subset of any countries with `confirmed.population.ratio` less than the threshold
subset(new_data_frame_covid19, subset = confirmed.population.ratio < 1)


