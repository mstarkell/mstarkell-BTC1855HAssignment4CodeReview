#' Code Review for:
#' BTC1855H Assignment 4 - "Martians are Coming"
#' Nishant Sarkar

#' This code completes the requirements set out in the "Assignment 4" module.
#' Please ensure that ufo_subset.csv is in your working directory.
#' See the bottom for an additional note. 
#' CR: Note from reviewer: all code review comments are marked with 'CR'

#' Loading tidyverse package.
# install.packages("tidyverse")
library(tidyverse)

# Loading ufo_subset.csv. "read_csv" is used to create a tibble. 
ufo_data <- read_csv("ufo_subset.csv")
# Removing spaces in ufo_data column names using make.names(), which replaces space characters with a "."
names(ufo_data) <- make.names(names(ufo_data), unique = TRUE)
#' CR: This successfully removed the spaces in the column names. Perhaps consider using an underscore instead of a period to keep names consistent.
library(tidyr)
library(dplyr)
library(lubridate)
library(stringr)
#' CR: The code below needs the above packages to run so I loaded them in using the library() function
#' CR: It may be beneficial to make note of this to ensure the user can run the code properly.

# Tidying ufo_data according to the assignment requirements.
ufo_data_tidy <- ufo_data %>%
  distinct() %>%                                                        # Removing duplicate rows using distinct() 
  mutate(shape = ifelse(is.na(shape), "unknown", shape)) %>%            # Finding rows where 'shape' is missing, and replacing with "unknown" by checking if the value is NA
  drop_na(country) %>%                                                  # Removing rows where the country column has an NA value
  mutate(datetime = as.Date(datetime), date_posted = as.Date(format(dmy(date_posted), "%Y-%m-%d"))) %>%  # Converting dates to ymd format. date_posted is dmy, so the format() command is used to convert it to ymd. 
  mutate(is_hoax = str_detect(tolower(comments), "hoax|star|capella|arcturus|sirius|vega|ISS|mercury|
                                                 venus|mars|jupiter|saturn|star|stars|bird|celestial|
                                                 aircraft|airplane|satellite|advertising|meteor|sky diver", 
                              negate = FALSE) &
           !str_detect(tolower(comments), "not a hoax|not a star", negate = FALSE)) %>%
  #' The above code adds an is_hoax column. It checks the comments column for a list of keywords using the str_detect function in the stringr package, and if the entry has one of these keywords and 
  #' does not include the phrases "not a hoax" or "not a star", it is flagged as TRUE in is_hoax. This comes with several drawbacks. All of these words are only indicative of a hoax if they are included 
  #' in a NUFORC comment, but as written, this code checks the whole comments entry. Consequently, if the original entry made some reference to one of these words (ex. "star-shaped UFO"), it will still 
  #' be marked as a hoax even without a NUFORC comment. However, just using "hoax" is not enough - this code was written under the assumption that "hoax" refers to anything that NUFORC thinks isn't a UFO, 
  #' as per Dr. K. Also, it is possible that some keywords are missing. Still, the code accurately classifies most of the hoax/incorrect entries.
  #' CR: Removing the duplicate rows was a smart addition to this code to ensure duplicate entries are excluded
  #' CR: Missing values for shape were successfully imputed with 'unknown'. As an alternative, you can use the replace() function.
  #' CR: Rows that did not have country information were also successfully removed. As an alternative, you could also use a filter for this.
  #' CR: From looking at the ufo_date_tidy dataset, it appears that all dates were converted to Y-M-D format. As an alternative, you could use the lubridate::ymd() function
  #' CR: Looking at the data set, after running the code, I cannot see the time in the datetime column anymore. 
  #' CR: I'm not sure if this is a problem on my end, but perhaps consider separating the date and time columns and then setting them to their correct format to avoid this issue.
  #' CR: Your filter for identifying possible hoaxes was really well thought out and succinctly addressed the drawbacks of this method. Great job. 
  #' CR: I am not sure what the solution would be to the drawbacks, but perhaps you could set the code to return TRUE only if a keyword AND 'NUFORC' is found in the comment column. 
  #' CR: This method may prevent falsely identifying non-hoaxes as hoaxes. 


  #' 
  mutate(report_delay = as.numeric(date_posted - datetime)) %>%         # Adding report_delay column. Calculating report delay by subtracting datetime from date posted. Negative values indicate an event that is posted before it happened.
  filter(report_delay >= 0)                                             # Removing rows where the report delay is negative.

#' CR: The mutate() and filter() functions are effective choices for accomplishing the intended tasks.
#' CR: Looking at the altered dataset, it appears that the report delays were successfully calculated and negative time differences were removed.
#' Creating a tibble reporting the percentage of hoax sightings per country. It first groups the tibble by country, then uses the summary() function to create a summary table. The summary table calculates the mean of 
#' "is_hoax" for each group and multiplies by 100 to find the percentage of TRUE values in the is_hoax column. 
percent_hoax <- ufo_data_tidy %>%
  group_by(country) %>%
  summarize(percent_hoax = mean(is_hoax, na.rm = TRUE)*100)
view(percent_hoax)
#' CR: When attempting to run the line 64 (view(percent_hoax)), I am receiving an error message. Consider using the print() function instead.
#' CR: However, I am still able to see the tibble reporting the percentage of hoax sightings per country.
#' CR: This section of code successfully reports the percentage of hoax sightings per country,

# Creating a tibble reporting the average reporting delay per country in days - groups tibble by country, then creates summary table summarizing mean report_delay per country.
delay_summary <- ufo_data_tidy %>%
  group_by(country) %>%
  summarize(average_delay = mean(report_delay))
view(delay_summary)
#' CR: Again, I had an error from the view function but could still view the tibble
#' CR: Similar to reporting the hoax sightings per country, the group_by function allows for the successful calculation of the average reporting delay per country
#' CR: Although this is not necessarily a problem, for 'de', the average report delay is reported as '125.', which is missing a number after a decimal. 
#' CR: Since the other values have numbers after the decimals, I am not sure why this is occurring. 

# Investigating duration.seconds column.
sum(is.na(ufo_data_tidy$duration.seconds))           # Returns zero, there are no missing values
all(is.numeric(ufo_data_tidy$duration.seconds))      # Returns TRUE, all values are numeric
any(ufo_data_tidy$duration.seconds < 0)              # Returns FALSE, all values are positive (including zero)
summary(ufo_data_tidy$duration.seconds)              #' It seems that there are some incredibly large values - the max is 82.8 million seconds, which is 26 years. There are clearly some values in duration.seconds that 
#' are far too big. The median is 180 seconds (~3 minutes) with 3Q = 600 seconds (10 minutes), so there is a small number of very large values. Furthermore, some reports 
#' have a duration of 0, which is not possible. duration.seconds should be modified to have an upper and lower limit on the range.
#' CR: I identified similar problems when checking the data in the seconds column. This is a thorough investigation!
#' CR: I think your point about the range being too large is correct as this will create a skewed histogram that appears to only have one bar.

# Fixing duration.seconds column's range issue by filtering rows where duration.seconds = 0 or 86400 seconds (24 hours)
# Used 24 hours as an arbitrary cutoff just because UFO sightings that are longer than that seem too excessive.
ufo_data_tidy <- ufo_data_tidy %>%
  filter(duration.seconds < 86400) %>%
  filter(duration.seconds > 0)
#' CR: I believe this is a valid way to filter through the data as your rationale is well-justified.

# Creating a histogram of duration.seconds. Using Log10 duration.seconds to accomodate for very large values.
hist((log(ufo_data_tidy$duration.seconds)), main = "UFO Sightings per Duration", xlab = "Log 10 Duration of Sighting (seconds)", ylab = "Number of Sightings", xlim = c(-2, 12))
#' CR: Even with the addition of the filter, including 'log' in the histogram is effective at accommodating for very large values.
#' CR: Your method allows the data to be visualized in a more meaningful way (allows the shape of the distribution to be visualized).

#' NOTE FOR REVIEWER: I wasn't able to figure out how to impute countries from the city column. I imagine that you could check to see if Country is missing, then check if the 'City' entry has parentheses. In the original 
#' dataset, cities with a designated country would have it inside parentheses. After the check, you could pull the first two characters inside the parenthesis for a contry code (ex. ca for Canada, fr for France). Then, 
#' if there isn't a parentheses, you could just remove the row altogether since we want to remove rows where country information is missing. The issue is that there are country codes that don't follow this format (ex. 
#' us instead of un for United States), and there are a lot of things inside parentheses that aren't countries. I have no clue how you could check for only countries, maybe you might have an idea?
#' CR: When attempting the bonus question, I had a very similar thought process and identified the same issues as you. The only solution I can think of (which I am unsure of how to implement),
#' CR: would be to check the city columns for the country names within brackets specifically (e.g., perhaps using str_detect), then using the ifelse function to turn the country names into their 
#' CR: appropriate two-letter country codes and imputing these codes into the country column if there is a corresponding missing value. This is definitely a complicated task.
#' CR: Overall, I think this assignment is really well done. Your code functioned as intended, you accomplished all the tasks outlined in the assignment, and you clearly justified your code. Great job!
