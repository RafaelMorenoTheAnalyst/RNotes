#Install Packages if needed

#install.packages("tidyverse")
#install.packages("scales")
#install.packages("lemon")
#install.packages("plotly")

#Load Packages
library(plotly)
library(lemon) #load the lemon package which is used for extending ggplot2 functionality
library(scales) #load the scales package
library(tidyverse) #load the tidyverse package
library(lubridate)

movies <- read_csv("movie_profit.csv") #Load the CSV we will use
View(movies) #see your data-set in a tabular format

options(scipen = 999) #remove scientific notation


#Create a scatter plot
ggplot(data = movies) + #load your data
  aes(x = production_budget, y = worldwide_gross, color = genre) + #Map x and y axis
  geom_point() + #specify that this is a scatter plot
  scale_x_continuous(labels = dollar_format()) + #add dollar signs for x
  scale_y_continuous(labels = dollar_format()) +    
  labs(x = "Production Budget", y = "Worldwide Gross", title = "Relationship between Production Budget and Worldwide Gross", color = "Type of Movie", subtitle = "There is a positive relationship between production budget and worldwide gross revenue") +
  facet_rep_wrap(~ genre, repeat.tick.labels = TRUE)


#Create a bar chart - for basic counts
ggplot(data = movies)+
  aes(x=mpaa_rating, fill = mpaa_rating)+
  labs(x = "MPAA Rating", y ="Number of Movies", title = "Number of Movies Per MPAA Rating")+
  geom_bar()

#Bar Chart from previous example but x and y axis are flipped 
ggplot(data = movies) + #load your data
  aes(x = mpaa_rating) + 
  labs(x = "MPAA Rating", y ="Number of Movies", title = "Number of Movies Per MPAA Rating")+
  geom_bar() +
  coord_flip()

#Create another bar chart - for relationship between mpaa_rating and worldwide gross revenue

ggplot(data = movies)+
  aes(x = mpaa_rating, y = worldwide_gross)+
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = dollar_format())

ggplot(data = movies) +
  aes(x = mpaa_rating, y = worldwide_gross) +
  geom_bar(stat = "identity") +  #this overrides the default of counting so that you can have a custom mapped y-axis
scale_y_continuous(labels = dollar_format())



##Bar chart with genre and count
ggplot(data = movies) + #load your data
  aes(x = genre) + 
  geom_bar() +
  coord_flip() + 
  labs(title = "Your title goes here")


#?scale_x_continuous - this is how you see documentation by putting a question mark first. 
movies %>%
  filter(distributor == "Walt Disney")%>%
  ggplot(aes(x=genre, y = production_budget))+
  geom_bar(stat = "identity")+
  labs(title = "Walt Disney Bar Chart", x = "Genre",y= "Production Budget" )+
  scale_y_continuous(labels = dollar_format())

movies%>%
  filter(distributor == "Walt Disney") %>%
  group_by(genre) %>%
  summarise(production_average = mean(production_budget))%>%
  ggplot(aes(x = genre, y= production_average))+
  geom_bar(stat = "identity")+
  scale_y_continuous(labels = dollar_format())+
  labs(title = "Walt Disney Production Averages by Genre", x= "Genre", y = "Production Average")

### Making a Histogram
movies%>%
  filter(distributor == "Walt Disney")%>%
  ggplot()+
  aes(x = production_budget)+
  geom_histogram(bins=20)

###summary stats
movies %>% 
  filter(distributor== "Walt Disney")%>%
  summary()#pulls summary stats

###Faceted Histograms for Disney
movies%>% 
  filter(distributor == "Walt Disney")%>%
  ggplot()+
  aes(x = production_budget)+
  geom_histogram()+
  facet_rep_wrap(~genre,repeat.tick.labels = TRUE)+
  scale_x_continuous(labels = dollar_format())

###adding mpaa color to the previous graph
###Faceted Histograms for Disney
movies%>% 
  filter(distributor == "Walt Disney")%>%
  ggplot()+
  aes(x = production_budget, fill = mpaa_rating)+
  geom_histogram(color="Black")+
  facet_rep_wrap(~genre,repeat.tick.labels = TRUE)+
  scale_x_continuous(labels = dollar_format())
###box plot
movies%>%
  filter(distributor =="Walt Disney"& mpaa_rating!="NA")%>%
  ggplot()+
  aes(x=mpaa_rating, y = production_budget, fill = mpaa_rating)+
  geom_boxplot(show.legend = FALSE, color="Black")+
  labs(title = "Poduction Budget Per MPAA Ratings", x= "MPAA Rating", y = "Production Budget")+
  scale_y_continuous(labels = dollar_format())

###plotly example
boxplotly <- movies%>%
  filter(distributor =="Walt Disney"& mpaa_rating!="NA")%>%
  ggplot()+
  aes(x=mpaa_rating, y = production_budget, fill = mpaa_rating)+
  geom_boxplot(show.legend = FALSE, color="Black")+
  labs(title = "Poduction Budget Per MPAA Ratings", x= "MPAA Rating", y = "Production Budget")+
  scale_y_continuous(labels = dollar_format())


ggplotly(boxplotly)###gives me an interactive chart

###line graph
movies%>%
  filter(distributor == "Walt Disney")%>%
  ggplot()+
  aes(x = release_date, y= production_budget)+
  geom_line()+
  geom_smooth(se=FALSE)

###lets imrpove the line graph
movies%>%
  mutate(year = year(release_date))%>%
  filter(year>=1980)%>%
  count(year)%>%
  ggplot()+
  aes(x = year, y = n)+
  geom_line()+
  labs(x = "Year", y = "Count")+
  scale_x_continuous(limits = c(1980,2018), n.breaks=20)

###ggplotly interacitve line graph
line_interactive <- movies%>%
  mutate(year = year(release_date))%>%#ads column
  filter(year>=1980)%>%
  count(year)%>%
  ggplot()+
  aes(x = year, y = n)+
  geom_line()+
  labs(title = "Number of movies Released from 1980 - 2018", x = "Year", y = "Number of Movies Released")+
  scale_x_continuous(limits = c(1980,2018), n.breaks=20)

ggplotly(line_interactive)