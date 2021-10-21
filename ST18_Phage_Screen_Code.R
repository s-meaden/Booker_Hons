library(tidyverse)
library(FSA)

df<-read.csv("C:\\Users\\fbook\\OneDrive\\Documents\\Honours\\Lab Work\\Data\\ST18 Screen.csv")


#plot the same data in a bar plot
df %>%
  mutate(Strain = gsub(" ", "\n", Strain)) %>%
  ggplot(., aes(Strain,PFU.mL,Phage, fill = Strain))+
  scale_y_log10()+
  geom_bar(position = 'dodge', stat = 'summary', fun = 'mean', colour= "black", size=0.75) +
  geom_errorbar(stat = 'summary', position = 'dodge', width = 0.1, size= 0.75) +
  geom_point(aes(x = Strain), shape = 16 , size= 2, position = 
               position_jitterdodge(jitter.width = 0.2, jitter.height=0.4, 
                                    dodge.width=0.9))+
  facet_grid( ~ Phage)+
  theme_classic()+
  scale_fill_manual(values= c("palevioletred1", "paleturquoise1", "thistle1"))+
  theme(text=element_text(size= 20))


#test for differences
kruskal.test(PFU.mL ~ Strain, df)
kruskal.test(PFU.mL ~ Strain, data=subset(df, Phage == "T4"))
kruskal.test(PFU.mL ~ Strain, data=subset(df, Phage == "T5"))
kruskal.test(PFU.mL ~ Strain, data=subset(df, Phage == "T7"))
kruskal.test(PFU.mL ~ Strain, data=subset(df, Phage == "PhiX174"))

#no significant differences between any of these.