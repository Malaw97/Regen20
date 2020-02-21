library(dplyr)
library(readxl)
library(ggplot2)
df <- read_xlsx("regen20-dataset.xlsx")

df %>% head() 
df %>% colnames()

#check if there is any missing data
df %>% is.na() %>%
  sum()

#check the distribution of age
summary(as.factor(df$age))
df$age <- as.factor(df$age)

#check the number of patients
summary(as.factor(df$target))
df$target <- as.factor(df$target)


#look at how cardiovascular disease is spread among people's age
g <- ggplot(df, aes(age, fill = target))
g + geom_bar(position = 'dodge') +
  labs(title = 'age vs target') +
  scale_fill_manual(labels = c('healthy','disease'), values = c('deepskyblue1', 'orangered2'))


#training sets and testing sets 