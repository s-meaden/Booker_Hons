library(tidyverse)

df<-read.csv("C:\\Users\\fbook\\OneDrive\\Documents\\Honours\\Lab Work\\Data\\Standard_curve.csv")


ggplot(df, aes(CFU_ml, OD600))+
  geom_smooth(method = "lm")+
  geom_point()+
  theme(text = element_text(size = 15))

# Make a model (this is the same model as the graph above).
line <- lm(CFU_ml~ OD600, data = df)


#add whatever OD you want and it will give you the predicted CFU/ml
# Make a new dataset for 0.01. NA because we don't know 
new_data<-data.frame(CFU_ml = NA, OD600 = 0.05)
new_data<-data.frame(CFU_ml = NA, OD600 = 0.1)

# Get your model's prediction for CFU based on an OD of 0.01
new_data$predicted_cfu_ml<-predict(line, new_data)

new_data
