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



week7_tbl %>% 
  mutate(gender = factor(gender, levels = c("Male", "Female"))) %>%
  ggplot(aes(x = q1, y = q2, color = gender)) +
  geom_point(position = "jitter") +
  labs(x = "q1", y = "q2", color = "Participant Gender") 
week7_tbl %>% 
  mutate(gender = factor(gender, levels = c("Male", "Female"))) %>%
ggplot(aes(x = q1, y = q2)) +
  geom_point(position = "jitter") +
  facet_grid(. ~ gender) +
  labs(x = "Score on Q1", y = "Score on Q2") 

week7_tbl %>%
  mutate(gender = factor(gender, levels = c("Male", "Female"))) %>%
  ggplot(aes(x = gender, y =timeSpent)) +
  geom_boxplot() +
  labs(x = "Gender", y = "Elapsed time (mins)") 

ggplot(week7_tbl, aes(x = q5, y = q7, color = condition)) +
  geom_jitter()+
  stat_smooth(method = "lm",se = FALSE)+
  labs(x = "Score on Q5", y = "Score on Q7",color = "Experimental Condition")+
  theme(legend.position = "bottom",
        legend.background = element_rect(fill = "#E0E0E0"))+
  coord_fixed()
  
  