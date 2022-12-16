library(tidyverse)

#### Reading in festival data 
festival_data <- read_csv('assign_2.csv')

#### Creating a separate data set of duplicate observations
festival_data_1 <- festival_data %>%
  filter(X == 1)

#### Finding the number of duplicates 
n_repeated_1 <- festival_data_1 %>%
  nrow()

#### Creating a separate data set that contains no duplicate values
festival_data_unique <- festival_data %>%
  distinct()

#### Creating a new column that takes on a binary value 1 if an individual extended their trip and 0 otherwise
festival_data_unique <- festival_data_unique %>%
  mutate(extend_b = ifelse(extend == 'Yes', 1, 0))

#### Finding if those who extended their stay also spent more time each day at the festival and how many more (or less) hours they attended
extend_1_hours <- festival_data_unique %>%
  filter(extend_b==1) %>%
  summarise(mean(hours_attend, na.rm = T))

extend_0_hours <- festival_data_unique %>%
  filter(extend_b==0) %>%
  summarise(mean(hours_attend, na.rm = T))

extend_more_hours <- extend_1_hours - extend_0_hours

#### Finding the distinct intervals of spending from people at the festival
f_data_spend_entertainment_total_amounts <- festival_data_unique %>%
  distinct(spend_entertainment_total) %>%
  drop_na()

#### Creating a table that counts the amount of people that spent over $100 and more specifically fall within the spending range intervals saved in spend_more_100
spend_more_100 <- c("$500-$999", "$200-$499")

festival_data_unique <- festival_data_unique %>% 
  mutate(s_entertainment_total_more_100 = ifelse(spend_entertainment_total %in% spend_more_100, 1, 0))

#### Calculating the proportion of women who spent more than $100 on entertainment
spend_entertainment_total_more_100_women <- festival_data_unique %>%
  filter(gender=='Female') %>%
  summarise(spend_entertainment_total_more_100_women = mean(s_entertainment_total_more_100))

#### Creating a tibble with all possible cominations of gender and age and the corresponding proportion of participants who spent more than $100 on entertaining
spend_entertainment_total_more_100_demo <- festival_data_unique %>%
  group_by(gender, age) %>%
  summarise(se_t_m_100_demo = mean(s_entertainment_total_more_100, na.rm = T))

#### Creating a table of amount of people and proportion of people by all different age ranges
ages <- festival_data_unique %>%
  group_by(age) %>%
  count() 

ages <- ages %>%
  mutate(proportion = n/287)

#### Finding the amount of times people have visited the festival
visits <- festival_data_unique %>%
  group_by(visits) %>%
  count(sort = T)
 
#### Finding the amount of people that extended their stay and also spent more than $100 dollars on entertainment at the festival 
extend_hours_over_100 <- festival_data_unique %>%
  filter(extend_b==1, spend_entertainment_total %in% spend_more_100) %>%
  nrow()

#### Finding the amount of people that didn't extend their stay but still spent more than $100 dollars on entertainment
nonextend_hours_over_100 <- festival_data_unique %>%
  filter(extend_b==0, spend_entertainment_total %in% spend_more_100) %>%
  nrow()

#### Finding the amount of people that extended their stay at the festival
extended_hours <- festival_data_unique %>%
  filter(extend_b==1) %>%
  nrow()

#### Finding the amount of people that didn't extend their stay at the festival
nonextended_hours <- festival_data_unique %>%
  filter(extend_b==0) %>%
  nrow()

#### Finding the proportion of people that spent over $100 when they extended their stay and the proportion of people that spent over $100 when they didn't extended their stay
prop_extend_100 <- extend_hours_over_100 / extended_hours
prop_nonextend_100 <- nonextend_hours_over_100 / nonextended_hours




