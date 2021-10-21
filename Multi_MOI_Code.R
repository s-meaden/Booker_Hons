library(tidyverse)
library(reshape2)      

df<-read.csv("C:\\Users\\fbook\\OneDrive\\Documents\\Honours\\Lab Work\\Data\\fern_moi_assay_14_sept.csv")
meta<-read.csv("C:\\Users\\fbook\\OneDrive\\Documents\\Honours\\Lab Work\\Data\\fern_moi_assay_14_meta.csv")

#plot each of the wells separately
df2<-df %>%
  left_join(., meta, by = "well") %>%
  mutate(., well_column = as.numeric(gsub("[A-Z]+", "", well))) %>%
  mutate(strain=gsub("delta", "\U0394", strain)) %>%
  mutate(minutes = time*12) %>%
  mutate(time_hrs = minutes/60)

p1<-c("WT", "\U0394") 


df2 %>%
  ggplot(., aes(time_hrs, value,))+
  geom_line()+
  facet_wrap( ~ well, ncol = 12)

df2 %>%
  filter(!well %in% drops)  %>%
  filter(!strain== "blank") %>%
  filter(!time_hrs== "0.2") %>%
  filter(!strain %in% p1) %>%
  #+/- ! in above line to give the other strains
  group_by(time_hrs, strain, MOI, phage) %>%
  summarise(avg_OD=mean(value), SD=sd(value, na.rm = TRUE), se=SD/sqrt(length(value))) %>%
  mutate(ymin=avg_OD-se,ymax=avg_OD+se) %>%
  ggplot(., aes(time_hrs, avg_OD))+
  geom_line(size=1.25, aes(colour=strain))+
  geom_ribbon(aes(ymin=ymin, ymax = ymax ,group=strain), alpha = 0.1)+
  facet_wrap(~ MOI + phage)+
  theme_classic()+
  scale_color_manual(values= c("lightsalmon2", "plum"))+
  #need to also change the colurs when you split them
  theme(text= element_text(size=15))+
  xlab("Hours Post-Infection")+
  ylab("Average Optical Density")

#below were technical errors with not enough phage added
drops<-c("E8","F8","G8","H8","H7","G7")

df2 %>%
  filter(!well %in% drops)  %>%
  filter(!strain== "blank") %>%
  filter(!time_hrs== "0.2") %>%
  filter(strain %in% p1) %>%
  #+/- ! in above line to give the other strains
  group_by(time_hrs, strain, MOI, phage) %>%
  summarise(avg_OD=mean(value), SD=sd(value, na.rm = TRUE), se=SD/sqrt(length(value))) %>%
  mutate(ymin=avg_OD-se,ymax=avg_OD+se) %>%
  ggplot(., aes(time_hrs, avg_OD))+
  geom_line(size=1.25, aes(colour=strain))+
  geom_ribbon(aes(ymin=ymin, ymax = ymax ,group=strain), alpha = 0.1)+
  facet_wrap(~ MOI + phage)+
  theme_classic()+
  scale_color_manual(values= c("lightpink2", "lightblue3"))+
  #need to also change the colurs when you split them
  theme(text= element_text(size=15))+
  xlab("Hours Post-Infection")+
  ylab("Average Optical Density")