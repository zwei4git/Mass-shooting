library(readr)
library(tidyverse)
library(readxl)
library(ggplot2)
library(ggrepel)

setwd("~/Gun_data/")

#https://www.gunviolencearchive.org/reports/mass-shooting?
ms_files<-list.files(pattern = c("mass.*.csv"),path = "~/Gun_data/")
ms_data<-do.call(rbind, lapply(ms_files, function(x) read_csv(x)))

colnames(ms_data)<-c('Incident_ID','Date','State','City_or_County',
                     'Address','Num_killed','Num_Injured','Operations')

#https://worldpopulationreview.com/state-rankings/gun-ownership-by-state
Gun_ownership_by_state <- read_csv("Gun_ownership_by_state.csv")

#https://data.ers.usda.gov/reports.aspx?ID=17827
state_pop <- read_excel("~/PopulationReport.xlsx")
state_pop%>%
  select(State=Name,pop_2020=`Pop. 2021`)->state_pop



#Massshooting per capita by gun ownership (2022)
ms_data%>%
  mutate(year=as.numeric(substr(Date,nchar(Date)-3,nchar(Date))))%>%
  group_by(State)%>%
  count()%>%
  left_join(state_pop,by='State')%>%
  left_join(Gun_ownership_by_state,by='State')%>%
  mutate(ms_per100k=100000*n/pop_2020)%>%
  filter(!is.na(gunOwnership ))->ms_plot_data

ms_plot_data%>%
  ggplot(aes(x=gunOwnership,y=ms_per100k,group=State))+
  geom_point(color='red',size=1.6)+
  geom_text_repel(aes(label=State),min.segment.length = 0,size=3.5,force = 18)+
  scale_x_continuous(name='Gun Ownership',labels = scales::percent)+
  scale_y_continuous(name='Mass Shooting per 100k Population')+
  ggtitle('Mass Shooting from 2014 to July 5, 2022 by Gun Ownership')+
  theme_bw()+
  theme(plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text  = element_text(face = "bold"))->ms_gunownership

ms_gunownership

  



