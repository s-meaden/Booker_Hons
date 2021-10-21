library(tidyverse)

df<-read.csv("C:\\Users\\fbook\\OneDrive\\Documents\\Honours\\Lab Work\\Data\\fern_moi_assay_14_sept.csv")

drops<-c("WT", "delta","blank")

#plot each well individually
df %>%
  mutate(column = as.numeric(gsub("[A-Z]", "", well))) %>%
  filter( time > 1) %>%
  ggplot(., aes(time, value))+
  geom_line()+
  geom_point()+
  facet_wrap(~ Value + column, ncol = 12)

#read in metadata
plaq<-read.csv("C:\\Users\\fbook\\OneDrive\\Documents\\Honours\\Lab Work\\Data\\pfu_ml.csv")
meta<-read.csv("C:\\Users\\fbook\\OneDrive\\Documents\\Honours\\Lab Work\\Data\\fern_moi_assay_14_meta.csv")

plaq<-rename (plaq, well = ï..well)

#plot growth curves coloured for PFU/mL
df %>%
  #mutate(column = as.numeric(gsub("[A-Z]", "", well))) %>%
  mutate(minutes = time*12) %>%
  mutate(time_hrs = minutes/60)%>%
  filter(!time_hrs== "0.2") %>%
  #mutate(., phage = ifelse(column > 8, "no", "yes")) %>%
  left_join( plaq, by = "well") %>%
  left_join(meta, by = "well") %>%
  mutate(strain=gsub("delta", "\U0394", strain))%>%
  drop_na() %>%
  mutate(log_pfu = log10(pfu_ml)) %>%
  filter(!strain %in% drops)%>%
  ggplot(., aes(time_hrs, value))+
  geom_point( aes(color = pfu_ml))+
  facet_wrap(~ strain + phage + well, ncol = 6)+
  theme_classic()+
  scale_color_gradient(trans = "log10")+
  theme(text = element_text(size = 15))+
  labs(color = "PFU\nper mL")+
  xlab("Hours Post Infection")+
  ylab("Optical Density")

tmp<-plaq %>%
  left_join(meta, by = "well") %>%
  filter(phage == "T7")%>%
  filter(!strain %in% drops)%>%
  mutate(strain=gsub("delta", "\U0394", strain))%>%
  group_by(strain) %>%
  summarise( mean_pfu = mean(pfu_ml, na.rm = TRUE), sd = sd(pfu_ml, na.rm = TRUE), se = sd(pfu_ml, na.rm = TRUE)/sqrt(length(pfu_ml)))

raw<-plaq %>%
  left_join(meta, by = "well") %>%
  filter(phage == "T7")%>%
  filter(!strain %in% drops)%>%
  mutate(strain=gsub("delta", "\U0394", strain)) %>%
  rename(mean_pfu = pfu_ml)

#plot the mean PFU/ml for t7 treated cells
ggplot(tmp, aes(strain,mean_pfu, fill = strain))+
  scale_y_log10()+
  #geom_bar(position = 'dodge', stat = 'summary', fun = 'mean', colour= "black", size=0.75) +
  geom_bar( stat = "identity", colour= "black", size=0.75, width= 0.5)+
  geom_errorbar( aes(ymin = mean_pfu - se, ymax = mean_pfu + se), width = 0.2)+
  geom_point(data = raw)+
  theme_classic()+
  scale_fill_manual(values= c("lightsalmon2", "plum"))+
  theme(text=element_text(size= 20))+
  ylab("PFU/mL")+
  xlab("Strain")

kruskal.test(raw$mean_pfu,raw$strain)