library(readr)
library(dplyr)
library(ggplot2)
library(reshape2)

### This is Roee Gimel first research script using R.
### In this analysis we will use the following functions:
#### mean(), median()
#### IQR(), barplot()
#### quantile(), is.na()
#### min(), pie(),
#### table(), max()
#### summary(), hist()

path <- paste0(getwd(),'/prostate_cancer_data.csv')
data <- read.table(file = path,header = T,sep=",")

nrow(data)

# you can print thios function in order to see first 5 rows >> head(data)

# 1. What are the research population ?!



### Answer by yourself

# 2. Describe SmokingHistory, FamilyHistoryPCa

## SmokingHistory

bring_my_dist <- function(vec){
  unique_values <- unique(vec)
  n_unique = length(unique_values)
  vec_dist <- rep(NA,n_unique)
  
  for(i in 1:n_unique){
    vec_dist[i] <- sum(vec == unique_values[i])
  }
  
  names(vec_dist) <- unique_values
  return(vec_dist)
}

bring_my_dist(data$SmokingHistory)
bring_my_dist(data$FamilyHistoryPCa)


# Number of rows 
N = nrow(data)

## SmokingHistory

df_1 <- data %>%
  group_by(SmokingHistory) %>%
  summarise(n = n(),
           `%` = n*100/N)

ggplot(data = df_1,
       map = aes(x='',y=n,fill=SmokingHistory)) +
  geom_bar(stat = 'identity',
           position = position_fill(0.5)) +
  geom_text(map = aes(label = paste0(round(`%`,1),'%')),
            position = position_fill(0.5)) + 
  labs(y = '%',
       x = '',
       title = 'Smoking History Distribution',
       caption = 'prostate_cancer_data.csv') + 
  coord_flip()


# FamilyHistoryPCa
df_2 <- data %>%
  group_by(FamilyHistoryPCa) %>%
  summarise(n = n(),
            `%` = n*100/N)  

  ggplot(data = df_2,
         map = aes(x='',
                   y=n,
                   fill=FamilyHistoryPCa)) +
  geom_bar(stat = 'identity',
           position = position_fill(0.5)) + 
  geom_text(map = aes(label = paste0(round(`%`,1),'%')),
            position = position_fill(0.5)) + 
  labs(y = '%',
       x = '',
       title = 'Family History PCa Distribution',
       caption = 'prostate_cancer_data.csv') + 
  coord_flip()

# 3. Within the {PCa == Yes} population, calculate the proportion of men with {FamilyHistoryPCa = Yes}
  df_3 <- data %>%
    group_by(Group,FamilyHistoryPCa) %>%
    summarise(n = n()) %>% 
    group_by(Group) %>%
    mutate(`%` = n*100/sum(n))

  ggplot(data = df_3,
         aes(x=Group ,y=n,fill=FamilyHistoryPCa)) +
  geom_bar(stat = 'identity',
           position = position_fill(0.5))  + 
  geom_text(aes(label = paste0(round(`%`,1),'%')),position = position_fill(0.5)) + 
  labs(y = '%',
       x = '',
       title = 'Family History vs. PCa',
       caption = 'prostate_cancer_data.csv') + 
  theme(legend.position = 'top')

### What do you think based on the results ? is there a strong relationship between history to PCa ? 

# 4. For: Age,PSA,Gleason
# a.

new_df <- data %>% select(Age,PSA,Gleason) 

new_df %>%
  ggplot(map = aes(x = Age)) +
  geom_histogram(binwidth = 5,fill = '#F7DC6F') +
  labs(x = 'Age',
       y = 'Count',
       title = 'Age Distribution')

new_df %>%
  ggplot(map = aes(x = PSA)) +
  geom_histogram(binwidth = 15,fill = '#F7DC6F') +
  labs(y = 'Count',
       title = 'PSA Distribution')

new_df %>%
  ggplot(map = aes(x = Gleason)) +
  geom_histogram(binwidth = 1,fill = '#F7DC6F') +
  labs(y = 'Count',
       title = 'Gleason Distribution')

new_df %>% melt() %>%
  filter(!is.na(value)) %>%
  group_by(variable) %>%
  summarise(avg = mean(value),
            trimmed_avg = mean(value,trim = 0.1),
            sd = sd(value),
            range = range(value)[2]-range(value)[1],
            min = min(value),
            x_0.10 = quantile(value,0.1),
            x_0.25 = quantile(value,0.25),
            median = median(value),
            x_0.75 = quantile(value,0.75),
            x_0.75 = quantile(value,0.9),
            max = max(value),
            IQR = x_0.75 - x_0.25) 

summary(data)

  summarise(average = mean(Age))
  
dt <- tibble(Polo = c(10,15,45,56,234),
             Roee = c(20,78,213,234,23))
vec = c(10,15,45,56,234)
mean()

for(i in 1:4){
  print(mean(vec[-i]))
  vec = vec[-i]
}

mean(c(15,45,56))
mean(dt$Polo,trim = 0.2)

mean(x, trim = 0.2) 

dt %>% melt()



  data %>% 
    head(100)

summary(data)