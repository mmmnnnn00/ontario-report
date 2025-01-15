library(tidyverse)

sample_data <- read_csv("data/sample_data.csv")
summarize(sample_data, average_cell = mean(cells_per_ml))
sample_data %>% # %>% <- Cmd+Shift+M 
  summarize(average_cells = mean(cells_per_ml))

# Filtering Rows
## == is equal to, != not equal to 
## filter(env_group %in% c("Deep", "Shallow_May")) 

sample_data %>%
  filter(str_detect(env_group == "Shallow")) %>%
  summarize(average_cells = mean(cells_per_ml))

sample_data %>%
  summarize(average_chlorophyll = mean(chlorophyll))

sample_data %>%
  filter(env_group == "Shallow_September") %>%
  summarize(average_chlorophyll = mean(chlorophyll))

sample_data %>%
  filter(str_detect(env_group, "September")) %>%
  summarize(average_chlorophyll = mean(chlorophyll))

# group_by
sample_data %>% 
  group_by(env_group) %>% 
  summarize(average_cells = mean(cells_per_ml),
            min_cells = min(cells_per_ml))

sample_data %>% 
  group_by(env_group) %>% 
  summarize(average_temperature = mean(temperature))

# Make new variables with mutate()
# calculate the TN:TP

sample_data %>% 
  mutate(tn_tp_ratio = total_nitrogen / total_phosphorus)  # if you want to check the table temporally add "%>% view" at the end of this line

sample_data %>% 
  mutate(temp_is_hot = temperature > 8) %>% 
  group_by(env_group, temp_is_hot) %>% 
  summarize(avg_temp = mean(temperature),
            avg_cells = mean(cells_per_ml))

# selecting columns with select
## only select the columns that you select
sample_data %>% 
  select(sample_id, depth)

## get rid of the columns that you do not want to see
sample_data %>% 
  select(-env_group)

## select column A to column D (include A, B, C, D)
sample_data %>% 
  select(sample_id:temperature)

## select columns that their names start with "xxx"
sample_data %>% 
  select(starts_with("total"))

## challenge -> create a dataframe with only sample_id, env_group, depth, temperature and cells_per_ml
sample_data %>% 
  select(sample_id, env_group, depth, temperature, cells_per_ml)

sample_data %>% 
  select(sample_id:temperature)

sample_data %>% 
  select(1:5)

sample_data %>% 
  select(-starts_with("total"), -diss_org_carbon, -chlorophyll)

sample_data %>% 
  select(-(total_nitrogen: chlorophyll))

# Cleaning data

# skip=2: skip the first two line(rows) of the dataset
# remove the lot number and sequencer columns
# assign this all to an object called "taxon_clean"
taxon_clean <-read_csv("data/taxon_abundance.csv", skip = 2) %>% 
  select(-...10, ) %>%
  rename(sequencer = ...9) %>% #rename the column named "...9" into "sequencer"
  select(-Lot_Number, -sequencer) 
  
taxon_long <- taxon_clean %>% 
  pivot_longer(cols = Proteobacteria:Cyanobacteria,
               names_to = "Phylum",
               values_to = "Abundance")
  
taxon_long %>% 
  group_by(Phylum) %>% 
  summarize(avg_abund = mean(Abundance))

taxon_long %>% 
  ggplot() +
  aes(x = sample_id, 
      y = Abundance, 
      fill = Phylum) +
  geom_col() +
  theme(axis.text.x = element_text(angle=90))

# Making long data wide

taxon_long %>% 
  pivot_wider(names_from = "Phylum",
              values_from = "Abundance") %>% view

# Joining data frames
head(sample_data)
head(taxon_clean)

# inner join (only choose data that fit table 1 and 2)
# full join (choose all data)
# left join 1,2 (according to table 1)
# right join 1,2 (according to table 2)
# anti join (only choose data that present in one table)

# inner join
inner_join(sample_data, taxon_clean, by = "sample_id") 

anti_join(sample_data, taxon_clean, by = "sample_id")

sample_data$sample_id

taxon_clean$sample_id 

taxon_clean_goodSep <- taxon_clean %>% 
  mutate(sample_id = str_replace(sample_id, pattern ="Sep", replacement = "September"))

inner_join(sample_data, taxon_clean_goodSep, by = "sample_id")
# can also compare multiple column names:
## e.g., inner_join(df1, df2, by = c("id", "name", "date"))
## e.g., inner_join(df1, df2, by = c("id_df1" = "id_df2", "name_df1" = "name_df2"))

sample_and_taxon <- inner_join(sample_data, taxon_clean_goodSep, by = "sample_id")
write_csv(sample_and_taxon, file = "data/sample_and_taxon.csv")

# make a plot
#ask :where does chloroflexi like to live?
library(ggpubr)
sample_and_taxon %>% 
  ggplot() +
  aes(x = depth,
      y = Chloroflexi)+
  geom_point() +
  labs(x = "Depth (m)", 
       y = "Choloroflexi relative abundance")+
    geom_smooth(method = "lm")+
  #stat_regression
  stat_cor()+
  annotate(geom ="text",
           x =25, y=.3,
           label = "This is a text lablel")
# What is the average abundance
# and sd of Chloroflexi

sample_and_taxon %>% 
  group_by(env_group) %>% 
  summarize(avg_chloro = mean(Chloroflexi),
            sd_chloro = sd(Chloroflexi))








