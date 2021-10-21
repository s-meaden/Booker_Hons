library(tidyverse)

df<-read.csv("C:\\Users\\fbook\\OneDrive\\Documents\\Honours\\Lab Work\\Data\\ECOI.csv")

p1<-c("WT", "delta")

head(df)

df1<-rename (df, Strain = ï..strain, pfu.ml = PFU.Ml )%>%
  filter(Strain %in% p1) %>%
  mutate(Strain=gsub("delta", "\U0394", Strain))

df2<-rename (df, Strain = ï..strain, pfu.ml = PFU.Ml )%>%
  filter(!Strain %in% p1) %>%
  mutate(Strain=gsub("delta", "\U0394", Strain))

#plot ECOI of WT and KO strain
ggplot(df1, aes(Strain, ECOI...., fill=Strain))+
  geom_jitter(width=0.01)+
  geom_boxplot()+
  scale_y_log10()+
  theme_classic()+
  scale_fill_manual(values= c("lightpink2", "lightblue3"))+
  theme(text = element_text(size = 20))+
  ylab("ECOI (%)")

#plot ECOI of plasmid containing strains (overexpression and empty vector)
ggplot(df2, aes(Strain, ECOI...., fill=Strain))+
  geom_jitter(width=0.01)+
  geom_boxplot()+
  scale_y_log10()+
  theme_classic()+
  scale_fill_manual(values= c("lightsalmon2", "plum"))+
  theme(text = element_text(size = 20))+
  ylab("ECOI (%)")


hist(df$ECOI)

kruskal.test(ECOI ~ strain, data= df1)

kruskal.test(ECOI ~ strain, data= df2)

#significant difference between WT and KO but no significant difference between the 
#compliment and the empty vector
