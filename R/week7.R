#Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(lubridate)
library(ggplot2)


#Data Import and Cleaning
week7_tbl<-read.csv("../data/Week3.csv")%>%
  mutate(timeStart = ymd_hms(timeStart),
        mutate(across(contains("q"), as.integer)))%>%
  mutate(timeEnd = ymd_hms(timeEnd))%>%
  mutate(condition = recode(condition, "A" = "Block A", "B" = "Block B", "C" = "Control"),
         gender = recode(gender, "M" = "Male", "F" = "Female")) %>%
  filter(q6 == 1) %>%
  select(-q6)%>%
  mutate(timeSpent = as.numeric(difftime(timeEnd, timeStart, units = "mins")))



#Visualization
#week7_tbl%>% select(starts_with("q"))%>%


ggplot(week7_tbl, aes(x = timeStart, y = q1)) +
  geom_point() +
  labs(x = "Date of Experiment", y = "Q1 Score")



ggplot(week7_tbl, aes(x = q1, y = q2, color = gender)) +
  geom_point() +
  labs(x = "q1", y = "q2", color = "Gender") #need to edit later


  
