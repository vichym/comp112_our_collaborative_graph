---
title: 'Weekly Exercises #5'
author: "Vichearith Meas"
output: 
  html_document:
    keep_md: TRUE
    toc: TRUE
    toc_float: TRUE
    df_print: paged
    code_download: true
    theme: journal
    code_folding: hide
---





```r
library(tidyverse)     # for data cleaning and plotting
library(gardenR)       # for Lisa's garden data
library(lubridate)     # for date manipulation
library(openintro)     # for the abbr2state() function
library(palmerpenguins)# for Palmer penguin data
library(maps)          # for map data
library(ggmap)         # for mapping points on maps
library(gplots)        # for col2hex() function
library(RColorBrewer)  # for color palettes
library(sf)            # for working with spatial data
library(leaflet)       # for highly customizable mapping
library(ggthemes)      # for more themes (including theme_map())
library(plotly)        # for the ggplotly() - basic interactivity
library(gganimate)     # for adding animation layers to ggplots
library(transformr)    # for "tweening" (gganimate)
library(gifski)        # need the library for creating gifs but don't need to load each time
library(shiny)         # for creating interactive apps
theme_set(theme_minimal())
library(janitor)       # for cleaning variable names
library(babynames)    # babynames dataset
library(ggrepel)
```



## Put your homework on GitHub!

Go [here](https://github.com/llendway/github_for_collaboration/blob/master/github_for_collaboration.md) or to previous homework to remind yourself how to get set up. 

Once your repository is created, you should always open your **project** rather than just opening an .Rmd file. You can do that by either clicking on the .Rproj file in your repository folder on your computer. Or, by going to the upper right hand corner in R Studio and clicking the arrow next to where it says Project: (None). You should see your project come up in that list if you've used it recently. You could also go to File --> Open Project and navigate to your .Rproj file. 

## Instructions

* Put your name at the top of the document. 

* **For ALL graphs, you should include appropriate labels.** 

* Feel free to change the default theme, which I currently have set to `theme_minimal()`. 

* Use good coding practice. Read the short sections on good code with [pipes](https://style.tidyverse.org/pipes.html) and [ggplot2](https://style.tidyverse.org/ggplot2.html). **This is part of your grade!**

* **NEW!!** With animated graphs, add `eval=FALSE` to the code chunk that creates the animation and saves it using `anim_save()`. Add another code chunk to reread the gif back into the file. See the [tutorial](https://animation-and-interactivity-in-r.netlify.app/) for help. 

* When you are finished with ALL the exercises, uncomment the options at the top so your document looks nicer. Don't do it before then, or else you might miss some important warnings and messages.

## Warm-up exercises from tutorial

  1. Choose 2 graphs you have created for ANY assignment in this class and add interactivity using the `ggplotly()` function.
  

```r
# graph from TidyTuesday 2
hbcu_all <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-02/hbcu_all.csv') %>% clean_names()

hbcu_by_gender <- hbcu_all %>%
  select(year, males, females) %>%
  pivot_longer(males:females,
               names_to = "gender",
               values_to = "students") %>%
  mutate(gender = str_remove(gender, "s$"))

hbcu_by_gender%>% 
  group_by(gender) %>% 
  mutate(accum= cumsum(students)) %>% 
  ggplot(aes(x=year, y=students, color = gender))+
  geom_point()+
  geom_line()+
  theme(axis.text.x = element_text(angle = 0, size=12))+
  labs(title = "Student Enrollment By Gender Over Time", x="Number of Enrollment", y="Year", caption = "By Vichearith Meas", subtitle = "Which gender enrolls more in post highschool education?", color="Gender")
```

![](05_exercises_files/figure-html/Graph 1.1-1.png)<!-- -->

```r
data_site <- 
  "https://www.macalester.edu/~dshuman1/data/112/2014-Q4-Trips-History-Data.rds" 
Trips <- readRDS(gzcon(url(data_site)))
Stations<-read_csv("http://www.macalester.edu/~dshuman1/data/112/DC-Stations.csv")
Trips %>%  
  select("duration","sdate", "client") %>%
  mutate(day_of_week = wday(sdate, label = TRUE)) %>% 
  mutate(time_of_day = hour(sdate)+ (round((minute(sdate)/60),2))) %>% 
  ggplot(aes(x=time_of_day, color=day_of_week))+
  geom_density(
    aes(
    fill = client, alpha=0.5), 
    color=NA, 
    position=position_stack())+
  facet_wrap(~day_of_week)+
  theme(legend.position = "none")+
  labs(title="Trips by Time of Day", x="Hour of day", 
       y="Trips", 
       caption = "by Vichearith Meas")
```

![](05_exercises_files/figure-html/Graph 1.2-1.png)<!-- -->

  
  
  2. Use animation to tell an interesting story with the `small_trains` dataset that contains data from the SNCF (National Society of French Railways). These are Tidy Tuesday data! Read more about it [here](https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-02-26).


```r
small_trains_accum_animate <-
small_trains%>% 
  mutate(date = make_date(year, month)) %>% 
  select(date,
         departure_station,
         arrival_station,
         total_num_trips) %>% 
  group_by(departure_station,
           date) %>%
  summarise(
    total_trip= sum(total_num_trips)) %>% 
  mutate(
    cum_total_trip = cumsum(total_trip)) %>%
  ggplot(aes(
    x=date, 
    y=cum_total_trip, 
    color = departure_station)) +
  geom_line()+
  geom_point(aes(group = seq_along(date))) +
  scale_color_discrete()+
  scale_y_log10()+
  geom_text_repel(aes(label=paste(departure_station, cum_total_trip,sep = ":")))+
  theme(legend.position = "none", 
        axis.title = element_blank()) + 
  labs(title="Cummulative Number of Trip by Station")+
  transition_reveal(date)+
  view_follow()

animate(small_trains_accum_animate, duration= 20)
anim_save("05_exercise_1_1.gif")
```


```r
knitr::include_graphics("05_exercise_1_1.gif")
```

![](05_exercise_1_1.gif)<!-- -->


```r
q <-
  small_trains %>% 
  group_by(month, year) %>% 
  summarise(avg_delay_depart = sum(avg_delay_all_departing)/n(),  
            avg_delay_arrive = sum(avg_delay_all_arriving)/n()) %>% 
  pivot_longer(names_to = "avg_delay_type", 
               cols= avg_delay_depart:avg_delay_arrive) %>% 
  mutate(date = make_date(year, month)) %>% 
  ggplot(aes(x = date, 
             y = value))+
  geom_area(aes(fill = avg_delay_type) ,alpha= 0.5)+
  geom_line(aes(color = avg_delay_type, position = "stack"))+
  scale_fill_discrete(labels = c("Ariival", "Departure")) +
  theme(legend.position = "top", axis.title.x = element_blank())+
  guides(color = FALSE)+
  labs(title = "Average Delay Minutes between 2015 and 2019", fill="Delay" , y = "Delay Time (min)", subtitle = "{frame_along}")+
  transition_reveal(date)+
  view_follow()
# 
animate(q, duration= 10, end_pause = 10)
# 
anim_save("05_exercise_1_2.gif")
```



```r
knitr::include_graphics("05_exercise_1_2.gif")
```

![](05_exercise_1_2.gif)<!-- -->
## Garden data

  3. In this exercise, you will create a stacked area plot that reveals itself over time (see the `geom_area()` examples [here](https://ggplot2.tidyverse.org/reference/position_stack.html)). You will look at cumulative harvest of tomato varieties over time. You should do the following:
  * From the `garden_harvest` data, filter the data to the tomatoes and find the *daily* harvest in pounds for each variety.  
  * Then, for each variety, find the cumulative harvest in pounds.  
  * Use the data you just made to create a static cumulative harvest area plot, with the areas filled with different colors for each vegetable and arranged (HINT: `fct_reorder()`) from most to least harvested (most on the bottom).  
  * Add animation to reveal the plot over date. 

I have started the code for you below. The `complete()` function creates a row for all unique `date`/`variety` combinations. If a variety is not harvested on one of the harvest dates in the dataset, it is filled with a value of 0.


```r
w<- 
  garden_harvest %>% 
  filter(vegetable == "tomatoes") %>% 
  group_by(date, variety) %>% 
  summarize(daily_harvest_lb = sum(weight)*0.00220462) %>% 
  ungroup() %>% 
  complete(variety, date, fill = list(daily_harvest_lb = 0)) %>% 
  group_by(variety) %>% 
  mutate(cum_harvest = cumsum(daily_harvest_lb)) %>%  
  ggplot(aes(x=date, y = cum_harvest , fill= fct_reorder(variety,cum_harvest)))+
  geom_area()+
  theme(legend.position = "bottom", 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank())+
  labs(title = "Accumulative Weight by Variety in Pound", 
       fill="Varirties",
       subtitle = "{frame_along}")+
  transition_reveal(date)+
  view_follow()

animate(w, duration= 10, end_pause = 10)

anim_save("05_exercise_3_1.gif")
```

```r
knitr::include_graphics("05_exercise_3_1.gif")
```

![](05_exercise_3_1.gif)<!-- -->

## Maps, animation, and movement!

  4. Map my `mallorca_bike_day7` bike ride using animation! 
  Requirements:
  * Plot on a map using `ggmap`.  
  * Show "current" location with a red point. 
  * Show path up until the current point.  
  * Color the path according to elevation.  
  * Show the time in the subtitle.  
  * CHALLENGE: use the `ggimage` package and `geom_image` to add a bike image instead of a red point. You can use [this](https://raw.githubusercontent.com/llendway/animation_and_interactivity/master/bike.png) image. See [here](https://goodekat.github.io/presentations/2019-isugg-gganimate-spooky/slides.html#35) for an example. 
  * Add something of your own! And comment on if you prefer this to the static map and why or why not.

```r
mallorca_map <- get_stamenmap(
    bbox = c(left = 2.0002, bottom = 39.3773, right =3.0789, top = 39.8802), 
    maptype = "terrain",
    zoom = 11
)
```



```r
z <-
ggmap(mallorca_map) +
  geom_point(data = mallorca_bike_day7, 
            aes(x = lon, y = lat),
            size = 1, color="red") +
  geom_path(data = mallorca_bike_day7, 
            aes(x = lon, y = lat, color = ele), size = 1)+
  theme(legend.position = "bottom", 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank())+
  scale_color_gradient(low="#34ebd8", high="#fa0515")+
  labs(title = "Biking Path", 
       color="Elevation",
       subtitle = "{frame_along}")+
  theme_map()+ 
  transition_reveal(time)
animate(z, duration = 10, fps = 10)

anim_save("05_exercise_4_1.gif")
```


```r
knitr::include_graphics("05_exercise_4_1.gif")
```

![](05_exercise_4_1.gif)<!-- -->

  
  5. In this exercise, you get to meet my sister, Heather! She is a proud Mac grad, currently works as a Data Scientist at 3M where she uses R everyday, and for a few years (while still holding a full-time job) she was a pro triathlete. You are going to map one of her races. The data from each discipline of the Ironman 70.3 Pan Am championships, Panama is in a separate file - `panama_swim`, `panama_bike`, and `panama_run`. Create a similar map to the one you created with my cycling data. You will need to make some small changes: 1. combine the files (HINT: `bind_rows()`, 2. make the leading dot a different color depending on the event (for an extra challenge, make it a different image using `geom_image()!), 3. CHALLENGE (optional): color by speed, which you will need to compute on your own from the data. You can read Heather's race report [here](https://heatherlendway.com/2016/02/10/ironman-70-3-pan-american-championships-panama-race-report/). She is also in the Macalester Athletics [Hall of Fame](https://athletics.macalester.edu/honors/hall-of-fame/heather-lendway/184) and still has records at the pool. 
  

```r
panama_race <-
  panama_bike %>% 
  bind_rows(panama_run) %>%
  bind_rows(panama_swim) %>% 
  mutate(ele = replace_na(ele, 0)) %>% 
  arrange(time)
```



```r
panama_map <-
    get_stamenmap(
    bbox = c(left = -79.6050, 
             bottom = 8.9021, 
             right = -79.4703, 
             top = 8.99), 
    maptype = "terrain",
    zoom = 14
)
```



```r
y <- 
  ggmap(panama_map) +
  geom_point(data = panama_race, 
            aes(x = lon, y = lat, color=event),
            size = 2) +
  geom_path(data = panama_race, 
            aes(x = lon, y = lat, color = event), size = 0.5)+
  theme(legend.position = "bottom", 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank())+
 scale_color_discrete()+
  labs(title = "Race Path", 
       color="Events",
       subtitle = "{frame_along}")+
  theme_map()+
  transition_reveal(time)
animate(y, duration = 10, fps = 10)
```

```r
anim_save("05_exercise_5_1.gif")
```


```r
knitr::include_graphics("05_exercise_5_1.gif")
```

![](05_exercise_5_1.gif)<!-- -->

## COVID-19 data

  6. In this exercise, you are going to replicate many of the features in [this](https://aatishb.com/covidtrends/?region=US) visualization by Aitish Bhatia but include all US states. Requirements:
 * Create a new variable that computes the number of new cases in the past week (HINT: use the `lag()` function you've used in a previous set of exercises). Replace missing values with 0's using `replace_na()`.  d
  * Filter the data to omit rows where the cumulative case counts are less than 20.  
  * Create a static plot with cumulative cases on the x-axis and new cases in the past 7 days on the y-axis. Connect the points for each state over time. HINTS: use `geom_path()` and add a `group` aesthetic.  Put the x and y axis on the log scale and make the tick labels look nice - `scales::comma` is one option. This plot will look pretty ugly as is.
  * Animate the plot to reveal the pattern by date. Display the date as the subtitle. Add a leading point to each state's line (`geom_point()`) and add the state name as a label (`geom_text()` - you should look at the `check_overlap` argument).  
  * Use the `animate()` function to have 200 frames in your animation and make it 30 seconds long. 
  * Comment on what you observe.
  

```r
covid <- covid19 %>% 
  group_by(state) %>% 
  mutate(day_7_lag = lag(cases, n=7, order_by = date)) %>% 
  mutate_at(vars(day_7_lag), ~replace_na(., 0)) %>% 
  mutate(cum_cases = cumsum(cases)) %>% 
  mutate(weekly_new_cases = cases - day_7_lag) 
```



```r
x <- covid %>%
  filter(cum_cases > 20) %>% 
  ggplot(aes(x = cum_cases, y = weekly_new_cases, group = state))+
  geom_text(aes(label = state))+
  geom_path(color= "grey")+
  geom_abline(intercept = 10, slope = 2)+
  geom_point(color="pink")+
 scale_y_log10(
   breaks = scales::trans_breaks("log10", function(x) 10^x),
   labels = scales::trans_format("log10", scales::math_format(10^.x))
 ) +
 scale_x_log10(
   breaks = scales::trans_breaks("log10", function(x) 10^x),
   labels = scales::trans_format("log10", scales::math_format(10^.x))
 ) +
   labs(title = "Trajectory of Total Covid-19 Confirmed Cases in {frame_along}",
       x= "Total Confirm Cases",
       y = "New Confirm Cases in the Past Week")+
  transition_reveal(date)

animate(x, duration= 30)
anim_save("05_exercise_6_1.gif")
```
California was one of the earliest state to be affected by the Covid-19. This state continues to be the leader on the graph. While other states keep ascending in April, New York went down for awhile before going back up in the second wave. Overall, there seems to be multiple waves of Covid since the beginning. Many states cases went up and down multiple times.  

```r
knitr::include_graphics("05_exercise_6_1.gif")
```

![](05_exercise_6_1.gif)<!-- -->
  
  7. In this exercise you will animate a map of the US, showing how cumulative COVID-19 cases per 10,000 residents has changed over time. This is similar to exercises 11 & 12 from the previous exercises, with the added animation! So, in the end, you should have something like the static map you made there, but animated over all the days. The code below gives the population estimates for each state and loads the `states_map` data. Here is a list of details you should include in the plot:
  
  * Put date in the subtitle.   
  * Because there are so many dates, you are going to only do the animation for all Fridays. So, use `wday()` to create a day of week variable and filter to all the Fridays.   
  * Use the `animate()` function to make the animation 200 frames instead of the default 100 and to pause for 10 frames on the end frame.   
  * Use `group = date` in `aes()`.   
  * Comment on what you see.  



```r
census_pop_est_2018 <- read_csv("https://www.dropbox.com/s/6txwv3b4ng7pepe/us_census_2018_state_pop_est.csv?dl=1") %>% 
  separate(state, into = c("dot","state"), extra = "merge") %>% 
  select(-dot) %>% 
  mutate(state = str_to_lower(state))

states_map <- map_data("state")

covid19_with_pop <- 
  covid %>% 
  mutate(day_of_week = wday(date, label=TRUE, abbr=TRUE)) %>% 
  filter(day_of_week=="Fri") %>% 
  mutate(state = str_to_lower(state)) %>% 
  left_join(census_pop_est_2018,
            by = "state") %>% 
  mutate(cum_cases_per_10000 = (cum_cases/est_pop_2018)*10000)
```

```r
covid19_with_pop
```



```r
k <- 
  covid19_with_pop %>% 
  ggplot() +
  geom_map(map= states_map, 
           aes(map_id = state,
               fill=cum_cases_per_10000,
               group= date))+ 
  expand_limits(x = states_map$long, 
                y = states_map$lat) +
  theme_map()+
  scale_fill_continuous_tableau(palette="Red")+
  labs(title="Cumulative COVID-19 Cases per 10,000 Residents Over Time",
       subtitle = "Date: {closest_state}", 
       caption = "by Vichearith Meas", 
       fill="Cases per 10000") +
  transition_states(date)

animate(k, nframes = 200, end_pause=10)
```

```r
anim_save("05_exercise_7_1.gif")
```




```r
knitr::include_graphics("05_exercise_7_1.gif")
```

![](05_exercise_7_1.gif)<!-- -->

## Your first `shiny` app (for next week!)

NOT DUE THIS WEEK! If any of you want to work ahead, this will be on next week's exercises.

  8. This app will also use the COVID data. Make sure you load that data and all the libraries you need in the `app.R` file you create. Below, you will post a link to the app that you publish on shinyapps.io. You will create an app to compare states' cumulative number of COVID cases over time. The x-axis will be number of days since 20+ cases and the y-axis will be cumulative cases on the log scale (`scale_y_log10()`). We use number of days since 20+ cases on the x-axis so we can make better comparisons of the curve trajectories. You will have an input box where the user can choose which states to compare (`selectInput()`) and have a submit button to click once the user has chosen all states they're interested in comparing. The graph should display a different line for each state, with labels either on the graph or in a legend. Color can be used if needed. 
  
## GitHub link

  9. Below, provide a link to your GitHub page with this set of Weekly Exercises. Specifically, if the name of the file is 05_exercises.Rmd, provide a link to the 05_exercises.md file, which is the one that will be most readable on GitHub. If that file isn't very readable, then provide a link to your main GitHub page.
  
  [GitHub link](https://github.com/vichym/comp112_our_collaborative_graph/blob/main/05_exercises.md)


**DID YOU REMEMBER TO UNCOMMENT THE OPTIONS AT THE TOP?**
