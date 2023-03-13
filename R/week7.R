#Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(lubridate)
library(ggplot2)
library(GGally)

#Data Import and Cleaning
week7_tbl<-read.csv("../data/Week3.csv")%>%
  mutate(timeStart = ymd_hms(timeStart))%>%
  mutate(timeEnd = ymd_hms(timeEnd))%>%
  mutate(gender = factor(gender, levels = c("M","F"), labels=c("Male","Female")))%>%
  mutate(condition = factor(condition, levels = c("A","B","C"),labels=c("Block A", "Block B", "Control")))%>%
  filter(q6 == 1) %>%
  select(-q6)%>%
  mutate(timeSpent = as.numeric(difftime(timeEnd, timeStart, units = "mins")))




#Visualization
week7_tbl%>%
  select(starts_with("q"))%>% 
  ggpairs()
(ggplot(week7_tbl,aes(x = timeStart, y = q1)) +
  geom_point(size=0.8) +
  labs(x = "Date of Experiment", y = "Q1 Score")+
  theme(axis.title = element_text(size = 7),
        axis.text = element_text(size = 5,color="#666666")))%>%
  ggsave("../figs/fig1.png",.,width = 4.8, height = 2.8)
(ggplot(week7_tbl,aes(x = q1, y = q2, color = gender)) +
  geom_point(position = "jitter",size=0.95) +
  labs(x = "q1", y = "q2", color = "Participant Gender")+
  theme(text = element_text(size = 6.2),axis.text=element_text(color="#666666")))%>%
  ggsave("../figs/fig2.png",.,width = 4.9, height = 2.9)
(ggplot(week7_tbl,aes(x = q1, y = q2)) +
  geom_point(position = "jitter",size=0.98) +
  facet_grid(. ~ gender) +
  labs(x = "Score on Q1", y = "Score on Q2")+
  theme(text = element_text(size = 6.4),
        axis.text=element_text(color="#666666")))%>%
  ggsave("../figs/fig3.png",.,width = 5, height = 2.8)
(ggplot(week7_tbl,aes(x = gender, y =timeSpent)) +
  geom_boxplot() +
  labs(x = "Gender", y = "Time Elapsed (mins)")+
  theme(text = element_text(size = 6.5),
        axis.text=element_text(color="#666666")))%>%
  ggsave("../figs/fig4.png",., width = 4.8, height = 2.8)
(ggplot(week7_tbl,aes(x = q5, y = q7, color = condition)) +
  geom_jitter(width=0.2,size=0.95)+
  stat_smooth(method = "lm",size=0.75,se = FALSE)+
  labs(x = "Score on Q5", y = "Score on Q7",color = "Experimental Condition")+
  theme(legend.position = "bottom",
        legend.background = element_rect(fill = "#E0E0E0"),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 7.2),
        legend.margin = margin(3, 3, 3, 3, "pt"),
        axis.title = element_text(size = 6.8),
        axis.text = element_text(size = 5.5,color="#666666")))%>%
  ggsave("../figs/fig5.png",.,width = 4.5, height = 2.8,dpi = 600)