#intalling tidyverse library 
install.packages("tidyverse")
#calling the reader library 
#there are 2 ways to load a data. 1- from import. 2- from switching the directory and loading the data using read library.
library(readr)
covid_df <- read_csv('covid19.csv')

print(covid_df)

#determining the dimension 
dim(covid_df)
#determine the column names
vector_cols <- colnames(covid_df)
print(vector_cols)
#display the first 5 rows
head(covid_df)
#summary from the tibble package 
#installing the package 
install.packages("tibble")
#calling the package 
library(tibble)
glimpse(covid_df)
#The glimpse fuction give use a summary of the dataset so that we have a clue of the data we are working with
#Calling library dplyr 
library(dplyr)
#isolatiing unwanted records from province columns for better analysis, and removing province column
covid_df_all_states <- covid_df %>% filter(Province_State == "All States") %>% select(-Province_State)
view(covid_df_all_states)
#Even though we removed the columns states column, we filtered the dataset to contain only all states records.
covid_df_no_all_states <- covid_df %>% filter(Province_State != "All States") %>% select(-Province_State)
#Makinf sure the filtering is corret. we can see the total rows when adding the 2 lists with all states and no all states matches the original covid data
nrow(covid_df_no_all_states)
view(covid_df_no_all_states)
total_rows = nrow(covid_df_no_all_states) + nrow(covid_df_all_states)
print(total_rows)
#we can see the total rows when adding the 2 lists with all states and no all states matches the original covid data
#we can notice that there are columns that provide daily information and others that provide cumulative information.
covid_df_all_states_daily <- covid_df_all_states %>%
  select(Date, Country_Region, active, hospitalizedCurr, daily_tested, daily_positive)
#Extracting the top ten tested cases countries
covid_df_all_states_daily_sum <- covid_df_all_states_daily %>%
  group_by(Country_Region)%>%
  summarise( tested = sum(daily_tested), positive = sum(daily_positive), active = sum(active), hospitalized = sum(hospitalizedCurr))%>%
  arrange(desc(tested))
covid_df_all_states_daily_sum
view(covid_df_all_states_daily_sum)
#Extracting the top ten rows
covid_top_10 <- head(covid_df_all_states_daily_sum, 10)
#Identifying the highest positive against tested cases
#Creating vectors from our dataframe
countries <- c(covid_top_10$Country_Region)
tested_cases <- c(covid_top_10$tested)
positive_cases <- c(covid_top_10$positive)
active_cases <- c(covid_top_10$active)
hospitalized_cases <- c(covid_top_10$hospitalized)
#giving proper names 
names(positive_cases) <- countries
names(tested_cases) <- countries
names(active_cases) <- countries
names(hospitalized_cases) <- countries
#Identify the top three positive against tested cases
positive_cases/ tested_cases
positive_tested_top_3 <- c("United Kingdom" = 0.11, "United States" = 0.10, "Turkey" = 0.08)
view(active_cases)
view(positive_tested_top_3)
#To make sure we won't lose other information about these countries we can create a matrix that contains the ratio and the overall number of COVID-19 tested, positive, active and hospitalized cases.
united_kingdom <- c(0.11, 1473672, 166909, 0, 0)
united_states <- c(0.10, 17282363, 1877179, 0, 0)
turkey <- c(0.08, 2031192, 163941, 2980960, 0)
#Creating a matrix containing the vectors created above
covid_mat <- rbind(united_kingdom,united_states,turkey)
view(covid_mat)
#Renaming the matrix columns
colnames(covid_mat) <- c("Ratio", "tested", "positive", "active", "hospitalized")
covid_mat
#Putting all together
#Creating and asigning our question and answer variable
question <- "Which countries have had the highest number of positive cases against the number of tests?"
answer <- c("Positive tested cases" = positive_tested_top_3)
answer
dataframes_list <- list(complete = covid_df, all_states = covid_df_all_states, daily = covid_df_all_states_daily,top_10 = covid_top_10)
print(dataframes_list)
matrices_list <- list(covid_mat)
vectors_list <- list(vector_cols, countries)
data_structure_list <- list("dataframe" = dataframes_list, "matrix" = matrices_list, "vector" = vectors_list)
covid_analysis_list <- list(question, answer, data_structure_list)
covid_analysis_list[2]
#From our result we derived that the top 3 countries in terms of positive cases againt number of test are the United Kindom, United State and Turkey. This was done by dividing out positive case by number tested after cleaning our dataset.
