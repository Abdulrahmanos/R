#objective
#In our previous project we looked at the sales of books from a book company to determine how well each of the books performed in general and how well the books performed in the different regions they were sold. In this project, our goal is to evaluate the effectiveness of a program launched by the company to convince customers to buy more books. Primarily, we want to know if the program was effective overall and how effective the program was on the different types of customers who buy the books.
library(tidyverse)
library(lubridate)
df <- read_csv("sales2019.csv")
dim(df)
glimpse(df)
colnames_df <- colnames(df)

#Types of columns. 
for (col in colnames(df)) {
  paste0(col, " : ", typeof(df[[col]])) %>% print}
#Our data set has a total of 5000 rows and 5 columns. 4 of the 5 columns have character data type with the exception of the total_purchased column which has double data type

#Null values 
for (col in colnames(df)) {
  c(col, is.na(df[[col]]) %>% sum) %>% print()
}
#here are two columns missing data. The first is the user_submitted_review column, which contains the review left by the customer. The second is total_purchased, which represents how many books were purchased by the customer.
#we're going to handle these two columns differently. The reason for this is due to the fact that we care a lot more about the total_purchased column, because it contains the actual information on book sales. We want to determine if the company's new program helped to improve sales. 
#In order to keep as much information on sales as possible, we're going to take a different approach to handling missing data.
#In short, we're going to remove any rows that have missing data in user_submitted_review.
#For total_purchased, we're going to use a slightly more sophisticated approach. We are going to replace all of the NA values with an average value that we calculate from the complete dataset. 

df <- df %>%
  filter(is.na(user_submitted_review) != TRUE)
dim(df)

#885 were removed from the dataset

avg_total_purchased <- df %>%
  filter(!is.na(total_purchased)) %>%
  pull(total_purchased) %>% 
  mean()

df <- df %>%
  mutate(
    total_purchased_notnull = if_else(is.na(total_purchased),
                                      avg_total_purchased,
                                      total_purchased)
  )  

for(col in colnames(df)){
  null_val <- df %>% pull(col) %>% is.na %>% sum()
  paste(col, ":", null_val) %>% print()
}
df <- df %>% select(-total_purchased)
df

#The user_submitted_review column contains reviews in the form of sentences. Ultimately, we want to be able to classify reviews as either positive or negative. This allows us to count the number of negative or positive reviews in the analysis part of the workflow.
unique(df$user_submitted_review)

is_positive_rev <- function(review) {
  case_when(
    str_detect(review, "okay") ~ TRUE,
    str_detect(review, "Awesome") ~ TRUE,
    str_detect(review, "OK") ~ TRUE,
    str_detect(review, "learn") ~ TRUE,
    str_detect(review, "Never read a better book") ~ TRUE,
    TRUE ~ FALSE
  )
}
df <- df %>%
  mutate(
    positive_review =
      is_positive_rev(user_submitted_review)
  )

#another method
# extracting the positive reviews from the unique reviews in the data set
#unique_reviews <- book_sales %>% pull(user_submitted_review) %>% unique()
#positive_reviews <- unique_reviews[c(1, 2, 4, 5, 9)]
#positive_reviews %>% print()
# creating positive_review column
#book_sales <- book_sales %>% mutate(
 # positive_review = if_else(user_submitted_review %in% positive_reviews,
  #                          TRUE, FALSE)
#)

#the dates are currently represented in string form. These must be properly formatted before we can make any comparisons based on date and time.
#we need a clear way to distinguish between sales that happen before the program starts and those that happen after. We need to distinguish between these two groups so that we can use what we've learned to easily calculate the summary values we want from the data.

#creating new column of date_unix
df <- df %>%

mutate(
  date_unix = unlist(map(date, mdy))
)

#creating grouping column
df <- df %>%
  mutate(
    category_program = if_else(date_unix<mdy("07-01-2019"),"before", "after")
  )
#grouping and summarizing
program_summary <- df %>%
  group_by(category_program) %>%
  summarise(
    total_purchases = sum(total_purchased_notnull)
  )
program_summary

#subgrouping
program_summary_subgroup <- df %>%
  group_by(customer_type, category_program) %>%
  summarise(
    total_purchases = sum(total_purchased_notnull)
  )
program_summary_subgroup

#grouping on reviews
review_program_summary <- df %>%
  group_by(category_program) %>%
  summarise(
    percentage_of_positive_review = sum(positive_review) / (nrow(df)-sum(positive_review))
  )
review_program_summary

#program isn't effective'