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
  geom_point(position = "jitter") +
  labs(x = "q1", y = "q2", color = "Gender") #need to edit later


week7_tbl %>% 
  select(q1, q2, gender)%>% 
ggplot(aes(x = q1, y = q2)) +
  geom_point(position = "jitter") +
  facet_grid(. ~ gender) +
  labs(x = "Score on Q1", y = "Score on Q2") #need to edit later
  
week7_tbl %>%
  ggplot(aes(x = gender, y =timeSpent)) +
  geom_boxplot() +
  labs(x = "Gender", y = "Elapsed time (mins)") #may need to edit later

