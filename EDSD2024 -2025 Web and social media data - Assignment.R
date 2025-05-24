
#EDSD 2024/2025 - Web and social media data Analysis - Assignment
# Lecturer:Tom Theile
#Email:theile@demogr.mpg.de

#Name of Student: Gilbert HABAASA
#Date: Sunday 15 December 2024



#Please enter your name
#Gilbert HABAASA

#Please enter your email address
# gilbert.habaasa@ined.fr

#1.Please write the HTML-code for a small Webpage.
#Include a headline and a paragraph and again a headline and a paragraph.
#Use CSS to change the color of the headlines to red.
#extract headlines:


#2.Scrape the roles of the whole staff of the MPIDR
#Scrape this page https://www.demogr.mpg.de/en/about_us_6113/staff_directory_1899/ with R and `rvest`.
#We are not interested in the names, but only in the job titles of the people. Collect all the
#job titles ("Guest Researcher", "Research Scientist",...). 
#Count them and print out the total number of people. Then create an aggregated table by distinct job titles and the number of people with this job title.
#Paste the code into this answer:
#Paste the resulting tables into this answer:

# Install required package
install.packages("rvest")
install.packages("dplyr")

# Load libraries
library(rvest)
library(dplyr)


mpidr_html <- read_html("https://www.demogr.mpg.de/en/about_us_6113/staff_directory_1899/")
mpidr_html

# let us select all Job Titles with this selector and extract the text:
selector <- ".staffdirectory-item-function"
jobtitles <- mpidr_html %>% html_elements(".staffdirectory-item-function") %>% html_text2()

# See how we got a list of all the jobtitles:
jobtitles


# Count the total number of people
total_people <- length(jobtitles)

# Print the total count
cat("Total number of people:", total_people, "\n")


# Summarize the job titles
job_title_summary <- jobtitles %>%
  as.data.frame() %>%
  group_by(value = jobtitles) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

# Print the summary table
print(job_title_summary,n=100)


#3.What is an API? Please explain in 2 or 3 sentences
#These are websites designed to be read by machines and not humanbeings. 
#API in full is Application Programming Interfaces.
#The most common classes of APIs are:Operating System APIs;R-package APIs, and Web-APIs.

#4.Use R and and two APIs to fetch a joke and let ChatGPT explain the joke.
#Use R to fetch a joke from an API (you can alternatively use a different API to fetch some text)
#Extract that joke from the response and print it to the console
#Send the joke to the ChatGPT API and ask for an explanation.
#Print the explanation to the console
#You can find a new OpenAI API-key in the file 'openAI-key.txt'
#Alternatively: Use a different API than the joke API and fetch something of similar importance.
#Paste the code into this answer

# Required Packages
install.packages("httr")
install.packages("jsonlite")

# Required libraries
library(httr)  # For HTTP requests
library(jsonlite)  # For JSON parsing

#Using R to fetch a joke from an API and printing it in the console

library(httr) # for making HTTP requests
library(jsonlite) # for working with JSON data

jokes <- httr::GET(url = "https://official-joke-api.appspot.com/jokes/random")
jokes

jokes_content <- content(jokes)
jokes_content
jokes_content$setup
jokes_content$punchline
joke <- paste("Q:",jokes_content$setup,"A:",jokes_content$punchline)
joke

# My OpenAI API key is included here
# api_key <- "use yours"

# Set up HTTP Headers
http_headers <- add_headers(
  `Content-Type` = "application/json",
  Authorization = paste("Bearer", api_key)
)

# Request Body
request_body <- list(
  model = "gpt-4o-mini",
  messages = list(
    list(role = "system", content = "You are a smart assistant who gets every joke."),
    list(role = "user", content = "Explain me this joke"),
    list(role="user",content=joke)
  )
)

# Convert Request Body to JSON
request_body_json <- jsonlite::toJSON(request_body, auto_unbox = T)

# Send POST Request
response <- httr::POST(
  url = "https://api.openai.com/v1/chat/completions",
  body = request_body_json,
  http_headers
)

response

# Print Response
print(content(response))


# Extract the content from the response
response_content <- content(response, "text", encoding = "UTF-8")

# Convert the content into JSON
response_json <- jsonlite::fromJSON(response_content, simplifyVector = FALSE)

# Print the whole JSON response
response_json

# Extract the desired value
completion <- response_json$choices[[1]]$message$content

# Print the result
print(completion)



#5.Paste the output of the script into this answer:
#Assignment 1 (This will be a part of the final assignment):
#Create a creative/interesting plot of this dataset.
#Think about the different dimensions and contents of how you can look at this.
#Sentiment over time? Sentiment over newspaper? Count of some words over time?
#Sentiment associated to specific ...?

# Load required Packages
install.packages("wordcloud")
install.packages("tidytext")
install.packages("RColorBrewer")
install.packages("tidyr")
install.packages("dplyr")


# Load required libraries
library(readr)       # For reading CSV files
library(ggplot2)     # For data visualization
library(dplyr)       # For data manipulation
library(stringr)     # For string operations
library(wordcloud)      # For generating word clouds
library(RColorBrewer)   # For color palettes
library(tidyr)  # For reshaping and tidying data
library(tidytext)  # For text mining

# 1.Load the dataset
df_headlines_sentiments_1 <- read_csv("C:/Users/admin/OneDrive - London School of Hygiene and Tropical Medicine/‌INED 2024/Web and Social Media data/2024-EDSD-Course-Materials-Web-and-social-media-data/01 internet and webscraping/data/df_headlines_sentiments_1.csv")
View(df_headlines_sentiments_1)

# Convert 'date' column to Date type if it's not already
df_headlines_sentiments_1$date <- as.Date(df_headlines_sentiments_1$date)


# 2. Plot counts for different sentiments in Newspaper headlines in the US (joy, sadness) over time
sentiment_cols <- c("joy", "sadness")

df_long <- df_headlines_sentiments_1 %>%
  select(date, all_of(sentiment_cols)) %>%
  pivot_longer(cols = all_of(sentiment_cols), names_to = "sentiment", values_to = "count")

ggplot(df_long, aes(x = date, y = count, color = sentiment)) +
  geom_line() +
  labs(title = "Sentiment Counts for US Newspapers over Time", x = "Date", y = "Count") +
  theme_minimal() +
  theme(legend.position = "bottom")

# 3. Plot for negative and positive sentiments
if ("negative" %in% colnames(df_headlines_sentiments_1) & "positive" %in% colnames(df_headlines_sentiments_1)) {
  df_neg_pos <- df_headlines_sentiments_1 %>%
    select(date, negative, positive) %>%
    pivot_longer(cols = c("negative", "positive"), names_to = "type", values_to = "value")
  
  ggplot(df_neg_pos, aes(x = date, y = value, color = type)) +
    geom_line() +
    labs(title = "Sentiment Category in US Newspaper Headlines over Time", x = "Date", y = "Value") +
    theme_minimal() +
    theme(legend.position = "bottom")
}

# 4. Filter and analyse headlines containing specific keywords
filterwords <- "Mexico|border|Immigrant"

df_filtered <- df_headlines_sentiments_1 %>%
  filter(str_detect(headlines, filterwords))

# Plot sentiment over time for filtered data
if (nrow(df_filtered) > 0) {
  ggplot(df_filtered, aes(x = date, y = sentiments)) + 
    geom_line(color = "blue") +
    labs(title = "Sentiment Score for US Headlines on Mexico, Border and Immigrants over Time", x = "Date", y = "Sentiment Score") +
    theme_minimal()
} else {
  print("No matching records found for the filter criteria.")
}

ggplot(df_filtered, aes(x = news_url, y = anger)) +
  geom_boxplot() +
  labs(title = "Sentiment Distribution by Newspaper in the US", x = "Newspaper", y = "Sentiment Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot a bar graph
df_sentiment_summary <- df_filtered %>%
  mutate(sentiment = case_when(
    sentiments > 0 ~ "total_positive",
    sentiments < 0 ~ "total_negative",
    sentiments == 0 ~ "neutral"
  )) %>%
  group_by(sentiment) %>%
  summarise(count = n(), .groups = "drop")

ggplot(df_sentiment_summary, aes(x = sentiment, y = count, fill = sentiment)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("total_positive" = "green", "total_negative" = "red", "neutral" = "gray")) +
  labs(
    title = "Sentiment Category for US Headlines on Mexico, Border and Immigrants",
    x = "Sentiment",
    y = "Total Count"
  ) +
  theme_minimal()



# Create a word cloud

filterwords <- "Mexico|border|Immigrant"

df_filtered <- df_headlines_sentiments_1 %>%
  filter(str_detect(headlines, filterwords))  # Replace 'headlines' with the actual column name

# Check if filtered data has rows
if (nrow(df_filtered) > 0) {
  # Step 2: Tokenize the filtered headlines into words
  word_counts <- df_filtered %>%
    unnest_tokens(word, headlines) %>%  # Tokenize words from 'headlines'
    count(word, sort = TRUE)            # Count occurrences of each word
  
  # Remove stop words (common words like "the", "and", etc.)
  data("stop_words")  # Load default stop words
  word_counts <- word_counts %>%
    anti_join(stop_words, by = "word") %>%
    filter(!word %in% c("mexico", "border", "immigrant"))  # Optionally remove specific keywords
  
  # Generate the word cloud
  wordcloud(words = word_counts$word, 
            freq = word_counts$n, 
            max.words = 100, 
            colors = brewer.pal(8, "Dark2"),
            random.order = FALSE)
  
  # Add a title to the plot
  title("US Headlines on Mexico,Border and Immigrants", cex.main = 1.5)
} else {
  print("No matching records found for the specified keywords.")
}

  
#6. Use Wikidata to create and download a list of all Universities worldwide.
#This would be an appropriate identifier: https://www.wikidata.org/wiki/Q3918
#Create a table with columns for the name of the universities, the wikidata-id. It would be great to have optionally more columns with information about every university (gps-coordinates? Twitter-handles? Be creative, if you want). But be careful not to exclude universities without this data. It is not easy to get the extra-columns with the information you want, but with the help of the examples you can find out how to to this (any help is allowed).
#Save the data as csv-file, please. You can upload it in the next answer. You can use the query service https://query.wikidata.org/
#Copy the SparQL code here into this answer:
#Please upload the Wikidata CSV-file here
#Please upload at most one file


#7. Use data from SMD and WDI (please use different measures than in the examples, be creative)
#merge them and create an interesting plot!
#Paste the code into this answer, please.
#Upload the plot here, please. If you have several plots, try to either arrange them on one or just email them to me: theile@demogr.mpg.de
#Please upload at most one file


#install.packages("WDI")
library("WDI")
library(ggplot2)

url_smd <- "https://raw.githubusercontent.com/MPIDR/Global-flows-and-rates-of-international-migration-of-scholars/master/data_processed/scopus_2024_V1_scholarlymigration_country_enriched.csv"
df_smd <- read.csv(url_smd)
View(df_smd)

countries_to_plot <- c("UGA", "RWA", "KEN", "TZA")
df_filtered <- df_smd %>% 
  filter(iso3code %in% countries_to_plot)

g <- ggplot(df_filtered, aes(year, number_of_inmigrations, color=iso3code)) + 
  geom_line()+
  labs(
    x = "Year",
    y = "Number of Inmigrations",
    color = "Country"
  )
g


# Now using Total Fertility data in East Africa using the WDI data!


dat = WDI(indicator=c("SP.DYN.TFRT.IN") , country=c("UGA","RWA","KEN","TZA"), start=1960, end=2012)
View(dat)



df_smd_wdi <- inner_join(df_smd, dat, by = c("iso2code" = "iso2c", "year" = "year"))

dim(df_smd)
dim(dat)
dim(df_smd_wdi)

g <- ggplot(df_smd_wdi, aes(year,SP.DYN.TFRT.IN, color=iso3code)) + 
  geom_line()+
  labs(
    x = "Year",
    y = "Total Fertility Rate (births per woman)",
    color = "Country"
  )
g

g <- ggplot(df_smd_wdi, aes(number_of_inmigrations,SP.DYN.TFRT.IN, color=iso3code)) + 
  geom_point()+
  labs(
    x = "Number of Inmigrations",
    y = "Total Fertility Rate (births per woman)",
    color = "Country"
  )
g

