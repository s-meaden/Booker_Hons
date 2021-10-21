library(tidyverse)
library(FSA)

df<-read.csv("C:\\Users\\fbook\\OneDrive\\Documents\\Honours\\Lab Work\\Data\\EOP.csv")

p1<-c("WT", "delta")

tmp<-df%>%
  rename (Strain = ï..Strain)

# plot the EOP (ko vs wt)
tmp %>%
  filter(Strain %in% p1)%>%
  mutate(Strain=gsub("delta", "\U0394", Strain))%>%
  mutate(EOP_percent= EOP*100)%>%
  ggplot(., aes(Strain,EOP_percent,fill = Strain))+
  scale_y_log10()+
  geom_bar(position = 'dodge', stat = 'summary', fun = 'mean', colour= "black", size=0.75) +
  geom_errorbar(stat = 'summary', position = 'dodge', width = 0.1, size= 0.75) +
  geom_point(aes(x = Strain))+
  facet_grid( ~ Phage)+
  theme_classic()+
  scale_fill_manual(values= c("lightpink2", "lightblue3"))+
  theme(text=element_text(size= 20))+
  ylab("EOP (%)")


# plot EOP of plasmid strains (overexpression and empty vector)
tmp %>%
  filter(!Strain %in% p1)%>%
  mutate(Strain=gsub("delta", "\U0394", Strain))%>%
  mutate(Strain = gsub(" ", "\n", Strain)) %>%
  mutate(EOP_percent= EOP*100)%>%
  ggplot(., aes(Strain,EOP_percent,fill = Strain))+
  scale_y_log10()+
  geom_bar(position = 'dodge', stat = 'summary', fun = 'mean', colour= "black", size=0.75) +
  geom_errorbar(stat = 'summary', position = 'dodge', width = 0.1, size= 0.75) +
  geom_point(aes(x = Strain))+
  facet_grid( ~ Phage)+
  theme_classic()+
  scale_fill_manual(values= c("lightsalmon2", "plum"))+
  theme(text=element_text(size= 20))+
  ylab("EOP (%)")


#test for difference between WT and KO strains by phage
kruskal.test(EOP ~ Strain, df1)
kruskal.test(EOP ~ Strain, data=subset(df1, Phage == "T4"))
kruskal.test(EOP ~ Strain, data=subset(df1, Phage == "T5"))
kruskal.test(EOP ~ Strain, data=subset(df1, Phage == "T7"))
kruskal.test(EOP ~ Strain, data=subset(df1, Phage == "PhiX174"))

#and the same for the plasmid containing strains
kruskal.test(EOP ~ Strain, df2)
kruskal.test(EOP ~ Strain, data=subset(df2, Phage == "T4"))
kruskal.test(EOP ~ Strain, data=subset(df2, Phage == "T5"))
kruskal.test(EOP ~ Strain, data=subset(df2, Phage == "T7"))
kruskal.test(EOP ~ Strain, data=subset(df2, Phage == "PhiX174"))



## To calculate EOP I did the following:
## averaged the PFU/ML data for WT strain and each of the 4 phage
## then divided each of the other strains PFU/ML by the WT average
## for that phage.