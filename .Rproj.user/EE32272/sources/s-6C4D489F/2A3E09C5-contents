library(tidyverse)
library(ggthemes)
library(grid)

rm(list=ls())

rd1 <- data.frame(read_csv("https://api.covid19india.org/csv/latest/raw_data1.csv"))
rd2 <- data.frame(read_csv("https://api.covid19india.org/csv/latest/raw_data2.csv"))
rd3 <- data.frame(read_csv("https://api.covid19india.org/csv/latest/raw_data3.csv"))
rd4 <- data.frame(read_csv("https://api.covid19india.org/csv/latest/raw_data4.csv"))

rd1 %>% select(Date.Announced, Detected.City, Detected.District, Detected.State, State.code, Current.Status, Status.Change.Date, Cases = Num.cases) -> covidcases

covidcases %>% 
  bind_rows(
    rd2 %>% select(Date.Announced, Detected.City, Detected.District, Detected.State, State.code, Current.Status, Status.Change.Date, Cases =Num.cases)
  ) %>% 
  bind_rows(
    rd3 %>% select(Date.Announced, Detected.City, Detected.District, Detected.State, State.code, Current.Status, Status.Change.Date, Cases =Num.Cases)
  )%>% 
  bind_rows(
    rd4 %>% select(Date.Announced, Detected.City, Detected.District, Detected.State, State.code, Current.Status, Status.Change.Date, Cases =Num.Cases)
  ) -> covidcases

covidcases %>% 
  rename(
    Date = Date.Announced,
    City = Detected.City,
    District = Detected.District,
    State = Detected.State,
    UpdatedOn = Status.Change.Date,
  ) -> covidcases

summary(covidcases)

covidcases %>% 
  mutate(
    Date = as.Date(Date, '%d/%m/%Y'),
    City = as.factor(City),
    District = as.factor(District),
    State = as.factor(State),
    State.code = as.factor(State.code),
    Current.Status = as.factor(Current.Status),
    UpdatedOn = as.Date(UpdatedOn, '%d/%m/%Y')
  ) -> covidcases

covidcases %>%
  filter(Current.Status == "Hospitalized") %>%
  filter(!is.na(Cases)) %>% 
  group_by(Date, State) %>%
  summarise(Cases=sum(`Cases`)) -> dailyDetectedCases

covidcases %>%
  filter(Current.Status == "Recovered") %>%
  filter(!is.na(Cases)) %>% 
  group_by(Date, State) %>%
  summarise(Cases=sum(`Cases`)) -> dailyRecoveredCases

covidcases %>%
  filter(Current.Status == "Deceased") %>%
  filter(!is.na(Cases)) %>% 
  group_by(Date, State) %>%
  summarise(Cases=sum(`Cases`)) -> dailyDeceasedCases
  
dailyDetectedCases %>%
  group_by(State) %>%
  filter(sum(Cases) > 500) %>%
ggplot(aes(x=Date, y=Cases)) + geom_point() + geom_col(lwd = 0.5,fill = "Black") + scale_x_date('',date_breaks = '15 days', date_labels = '%d-%m') + labs(title="Daily Reported Covid Cases In States with More than 500 Cases", caption="© Omkar Shukla (@shukla_omkar)\n Data source: covid19india.org" ) + xlab('Date') + facet_wrap(~State, scales = "free") + theme_economist() -> dailyCasesWithStates

ggsave(filename = "GT500States.png", height = 7 , width = 11, units = 'in', dpi = 300)

covidcases %>%
  filter(!is.na(Cases) & !is.na(State)) %>%
  group_by(Date,State,Current.Status) %>%
  summarise(Cases=sum(Cases))-> overallStatus

factor(overallStatus$Current.Status)

overallStatus %>%
  group_by(State) %>%
  filter(sum(Cases) > 500 & Current.Status != "Migrated") %>%
  ggplot(aes(x=Date, y=Cases, fill = Current.Status)) + geom_col(lwd = 0.25) + scale_x_date('',date_breaks = '15 days', date_labels = '%d-%m') + labs(title="Daily Reported Covid Cases In States with More than 500 Cases", caption="© Omkar Shukla (@shukla_omkar)\n Data source: covid19india.org" ) + xlab('Date') + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) +  theme_economist() + theme(plot.title = element_text(hjust = 0.5, size = 16), legend.position="bottom", legend.title=element_text(size=10),legend.text=element_text(size=9)) +
  guides(fill=guide_legend(title="Status")) + facet_wrap(~State, scales = "free") -> dailyCasesWithStates

ggsave(filename = "Overall_Status.png", height = 9 , width = 18, units = 'in', dpi = 300)