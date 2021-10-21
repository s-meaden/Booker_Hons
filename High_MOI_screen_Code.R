library(tidyverse)
library(reshape2)          


df<-read.csv("C:\\Users\\fbook\\OneDrive\\Documents\\Honours\\Lab Work\\Data\\fern_high_MOI_gc.csv")
meta<-read.csv("C:\\Users\\fbook\\OneDrive\\Documents\\Honours\\Lab Work\\Data\\high_moi_metadata.csv")

meta<-rename (meta, well = ï..well)

#plot each well individually
df2<-df %>%
  left_join(., meta, by = "well") %>%
  mutate(., well_column = as.numeric(gsub("[A-Z]+", "", well))) %>%
  mutate(treatment =gsub(" ", "",treatment)) %>%
  mutate(strain=gsub("delta", "\U0394", strain)) %>%
  mutate(minutes = time*12) %>%
  mutate(time_hrs = minutes/60) 

p1<-c("WT", "\U0394") 

temp<-df2 %>%
  filter(!treatment== "N/A") %>% 
  #filter(!well %in% drops)  %>%
  filter(!time_hrs== "0.2") %>%
  filter(!strain %in% p1)
#+/- ! in above line to give the other strains

ggplot(temp, aes(time_hrs, value))+
  geom_line(aes(group=well, colour=strain), size=1.25)+
  facet_wrap(~ treatment )+
  theme_classic()+
  scale_color_manual(values= c("lightsalmon2", "plum"))+
  #need to also change the colurs when you split them
  theme(text= element_text(size=15))+
  xlab("Hours Post-Infection")+
  ylab("Optical Density")


