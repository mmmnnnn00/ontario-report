2+2
# assign values to objects
name <- "agar"
name

year <- 1881
year

name <-"Fanny Hesse"
name

# bad names for object
# ex1: number in front of the characters
# ex2: upper letter and lower letter both exist

#install.packages("tidyverse")
library(tidyverse)
sample_data <- read_csv("/Users/arielle/Downloads/ontario-report/sample_data.csv")
#read_csv(file="sample_data.csv") # reads in csv file
#read_csv("sample_data.csv")

# lets comment
Sys.Date() # outputs the current date
getwd() # the current working directory
sum(5,6) # add numbers

#creat first plot
ggplot(data = sample_data) + 
  aes(x = temperature) + 
  labs(x = "Temperature (C)") +
  aes(y = cells_per_ml/1000000) +
  labs(y= "Cells per mL (millions/mL)") +
  geom_point() + 
  labs (title = "Does temperature affect microbial abundance?") +
  aes(color = env_group) + 
  aes(size = chlorophyll) + 
  aes(shape = env_group) +
  labs(size = "Chlorophyll (ug/L)", 
       color = "Environmental Group",
       shape = "Environmental Group")

# Combined "neater" code
ggplot(data = sample_data) +
  aes(x=temperature,
      y=cells_per_ml/1000000,
      color=env_group,
      size= chlorophyll)
  geom_point()+
  labs(x = "Temperature (C)",
       y= "Cells per mL (millions/mL)",
       title = "Does temperature affect microbial abundance?",
       size = "Chlorophyll (ug/L)",
       color = "Environmental Group")
  
# import datasets
buoy_data <- read_csv("buoy_data.csv")
View(buoy_data)
dim(buoy_data)
head(buoy_data) #see beginning of the data
tail(buoy_data) #see end of the data

# plot some more
ggplot(data = buoy_data) +
  aes (x = day_of_year,
       y = temperature, 
       group = sensor, 
       color = buoy) +
  geom_line()

# plot some more
# introduce facets
ggplot(data = buoy_data) +
  aes (x = day_of_year,
       y = temperature, 
       group = sensor, 
       color = depth) +
  geom_line()+
  facet_wrap(~buoy, scales = "free_y")

# plot some more
# facets grid
# facet_wrap vs facet_grid
ggplot(data = buoy_data) +
  aes (x = day_of_year,
       y = temperature, 
       group = sensor, 
       color = depth) +
  geom_line() +
  facet_grid(rows= vars(buoy))

# structure of data object
str(buoy_data)

# discrete plots
# box plot
ggplot(data = sample_data)+
  aes(x = env_group,
      y =cells_per_ml)+
# geom_boxplot() vs geom_violin()
  geom_boxplot() +
  geom_jitter(aes(size = chlorophyll)) 
  #it's important to put the _jitter code after the boxplot code

# box plot
ggplot(data = sample_data)+
  aes(x = env_group,
      y =cells_per_ml)+
  geom_boxplot(fill = "pink") #color vs fill

# box plot
ggplot(data = sample_data)+
  aes(x = env_group,
      y =cells_per_ml)+
  geom_boxplot(aes(fill = env_group))+
  scale_fill_manual(values = c("pink", "tomato","papayawhip"))


# box plot- scale fill brewer
ggplot(data = sample_data)+
  aes(x = env_group,
      y =cells_per_ml)+
  geom_boxplot(aes(fill = env_group))+
  scale_fill_brewer(palette = "Set1")


# let R tell you the colors that you can use
colors() #single color
RColorBrewer::display.brewer.all() #palette

# custom palette time
# install.packages("wesanderson")
library(wesanderson)

ggplot(data = sample_data) +
  aes(x = env_group,
      y =cells_per_ml)+
  geom_boxplot(aes(fill = env_group))+
  scale_fill_manual(values = wes_palette('Cavalcanti1'))

# custom palette time
# install.packages("harrypotter")
library(harrypotter)
ggplot(data = sample_data) +
  aes(x = env_group,
      y =cells_per_ml)+
  geom_boxplot(aes(fill = env_group))+
  scale_fill_hp(discrete = TRUE, option = "Always")

# box plot
# change transparency
ggplot(data = sample_data) +
  aes(x = env_group,
      y =cells_per_ml)+
  geom_boxplot(fill = "darkblue", 
               alpha = 0.3)

# univariate plots
ggplot(sample_data) +
  aes(x = cells_per_ml) +
  geom_histogram(bins=50)

ggplot(sample_data) +
  aes(x = cells_per_ml) +
  geom_density(aes(fill=env_group), alpha = 0.5) +
  theme_bw()
               

# box plot
# change transparency
box_plot_export <- 
  ggplot(data = sample_data) +
    aes(x = env_group,
        y = cells_per_ml) +
    geom_boxplot()+
    theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1))
box_plot_export

# saving plot
# can also save the plot by exporting it directly from the window on the right
ggsave("awesome_plot.jpg", width=6, height = 4)

# add changes to the plot for black and white
box_plot_export <- box_plot_export + theme_bw()
box_plot_export
ggsave("awesome_plot.jpg",plot = box_plot_export, width=6, height = 4)


