#Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(lubridate)



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
