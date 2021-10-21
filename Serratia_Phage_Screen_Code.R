library(tidyverse)
library(FSA)

df<-read.csv("C:\\Users\\fbook\\OneDrive\\Documents\\Honours\\Lab Work\\Data\\Phage Screen.csv")

#plot the Serratia phage screen
df %>%
  mutate(Strain = gsub("delta", "\U0394", Strain)) %>%
  ggplot(aes(Strain, PFU.mL, fill=Strain))+
  geom_bar(stat= "identity", colour= "black", size= 0.75)+
  facet_grid(~Phage)+
  scale_y_log10()+
  theme_classic()+
  scale_fill_manual(values= c("khaki2", "mediumorchid1"))+
  theme(text = element_text(size = 15))+
  ylab("PFU/mL")

