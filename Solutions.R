###---------------------------------------------------###
### Solutions
###---------------------------------------------------###

# install.packages("nycflights13") 
library(nycflights13)
library(dplyr)

#------ 1
flights_am = filter(flights, sched_dep_time <= 1200)
flights_pm = filter(flights, sched_dep_time > 1200)

#------ 2
flights_am_full = mutate(flights_am, 
                            dep_ontime = as.integer(abs(dep_delay) < 10), # departs within 10 minutes of schedule
                            arr_ontime = as.integer(abs(arr_delay) < 10), # arrives within 10 minutes of schedule
                            latedep_ontimearr = as.integer(abs(dep_delay) > 10 & abs(arr_delay) < 10), # late departure but ontime arrival
                            date = as.Date(paste(year, month, day, sep="-"))) # make the 'date' column
flights_pm_full = mutate(flights_pm, 
                            dep_ontime = as.integer(abs(dep_delay) < 10), # departs within 10 minutes of schedule
                            arr_ontime = as.integer(abs(arr_delay) < 10), # arrives within 10 minutes of schedule
                            latedep_ontimearr = as.integer(abs(dep_delay) > 10 & abs(arr_delay) < 10), # late departure but ontime arrival
                            date = as.Date(paste(year, month, day, sep="-"))) # make the 'date' column

#------ 3
fl_am = select(flights_am_full, 
                  -year, -month, -day, # drop the columns we don't want
                  sched_dep = sched_dep_time, # rename sched_dep_time
                  sched_arr = sched_arr_time) # rename sched_arr_time
fl_pm = select(flights_pm_full, 
                  -year, -month, -day, # drop the columns we don't want
                  sched_dep = sched_dep_time, # rename sched_dep_time
                  sched_arr = sched_arr_time) # rename sched_arr_time


#------ 4
summarize(fl_am, 
            pDepOntime = mean(dep_ontime, na.rm=T),  # a
            pArrOntime=mean(arr_ontime, na.rm=T), # a
            pDepLateArrOntime = mean(latedep_ontimearr, na.rm=T), # b
            airtime=mean(air_time, na.rm=T), # c
            avgDist = mean(distance, na.rm=T), # d
            sdDist = sd(distance, na.rm=T)) # d
summarize(fl_pm, 
            pDepOntime = mean(dep_ontime, na.rm=T),  # a
            pArrOntime=mean(arr_ontime, na.rm=T), # a
            pDepLateArrOntime = mean(latedep_ontimearr, na.rm=T), # b
            airtime=mean(air_time, na.rm=T), # c
            avgDist = mean(distance, na.rm=T), # d
            sdDist = sd(distance, na.rm=T)) # d

#------ 5
fl_am = flights %>% 
          filter(sched_dep_time <= 1200) %>%
          mutate(dep_ontime = as.integer(abs(dep_delay) < 10), # departs within 10 minutes of schedule
                 arr_ontime = as.integer(abs(arr_delay) < 10), # arrives within 10 minutes of schedule
                 latedep_ontimearr = as.integer(abs(dep_delay) > 10 & abs(arr_delay) < 10), # late departure but ontime arrival
                 date = as.Date(paste(year, month, day, sep="-"))) %>% # make the 'date' column
          select(-year, -month, -day, # drop the columns we don't want
                 sched_dep = sched_dep_time, # rename sched_dep_time
                 sched_arr = sched_arr_time) # rename sched_arr_time

fl_pm = flights %>% 
          filter(sched_dep_time > 1200) %>%
          mutate(dep_ontime = as.integer(abs(dep_delay) < 10), # departs within 10 minutes of schedule
                 arr_ontime = as.integer(abs(arr_delay) < 10), # arrives within 10 minutes of schedule
                 latedep_ontimearr = as.integer(abs(dep_delay) > 10 & abs(arr_delay) < 10), # late departure but ontime arrival
                 date = as.Date(paste(year, month, day, sep="-"))) %>% # make the 'date' column
          select(-year, -month, -day, # drop the columns we don't want
                 sched_dep = sched_dep_time, # rename sched_dep_time
                 sched_arr = sched_arr_time) # rename sched_arr_time

#------ 6
wthr = weather %>%
        mutate(date = as.Date(paste(year, month, day, sep="-"))) %>% # create date column
        select(-year, -month, -day) # drop unnecessary columns

#------ 7
# a
planes %>%
  filter(seats == max(seats, na.rm=T) | seats == min(seats, na.rm=T)) %>% # get the fastes planes
  arrange(seats) %>%
  select(seats, engine, engines) # select the seats, engine type, and no. of engines

# b
planes %>%
  filter(speed == max(speed, na.rm=T) | speed == min(speed, na.rm=T)) %>% # get the fastes planes
  arrange(speed) %>%
  select(speed, engine, engines) # select the seats, engine type, and no. of engines

#------ 8
# a
flights %>% 
  group_by(tailnum) %>%
  tally() %>%
  arrange(desc(n))

flights %>% 
  group_by(tailnum, origin) %>%
  tally() %>%
  arrange(desc(n))

# b
flights %>% 
  group_by(tailnum) %>%
  summarize(mnAirtime = mean(air_time, na.rm=T)) # mean airtime by plane

# c
flights %>% 
  group_by(carrier) %>%
  summarize(mnDepDelay = mean(dep_delay, na.rm=T), # mean delay times by carrier
            mnArrDelay = mean(arr_delay, na.rm=T)) %>%
  arrange(desc(mnDepDelay), desc(mnArrDelay))

#------ 9
# First create a data frame with all of the relevant statistics for a-d
wth = weather %>% 
  group_by(origin) %>% # group by weather station 
  summarize(maxGust = max(wind_gust, na.rm=T), # max wind gust
            mnWindSpd = mean(wind_speed, na.rm=T), # avg wind speed
            sdWindSpd = sd(wind_speed, na.rm=T), # SD of wind speed
            mnVisib = mean(visib, na.rm=T)) # avg visibility

# a
wth %>% arrange(desc(maxGust))

# b
wth %>% arrange(desc(mnWindSpd))

# c
wth %>% arrange(desc(sdWindSpd))

# d
wth %>% arrange(mnVisib)


#------ 10
# a
planes %>% 
  group_by(manufacturer) %>%
  tally() %>% # count planes by manufacturer
  arrange(desc(n)) %>% # organize by count in descending order
  slice(c(1, n())) # take the first & last rows

# b
planes %>%
  group_by(manufacturer) %>%
  summarize(mnAge = mean(2017 - year, na.rm=T)) %>% # avg age
  arrange(desc(mnAge)) %>%
  slice(c(1, n())) # take the first & last rows

# c
planes %>% 
  group_by(year) %>%
  tally() %>%
  arrange(n) %>%
  slice(c(1, n())) # take the first & last rows

# d
planes %>% 
  group_by(engine) %>%
  tally() %>%
  arrange(n) %>%
  slice(c(1, n())) # take the first & last rows


#------ 11
flights %>% 
  left_join(weather) %>%
  group_by(carrier) %>%
  summarize(mnWind = mean(wind_speed, na.rm=T)) %>%
  arrange(desc(mnWind))

#------ 12
planes %>% 
  mutate(age = 2017 - year) %>%
  inner_join(flights, by='tailnum')  %>%
  filter(age == max(age, na.rm=T)) %>%
  distinct(carrier, manufacturer, type, age)

#------ 13
flights %>%
  distinct(carrier, tailnum) %>%
  inner_join(planes) %>%
  mutate(age = 2017.0-year) %>%
  group_by(carrier) %>%
  mutate(mnAge = mean(age, na.rm=T),
        medAge = quantile(age, probs = .5, na.rm=T)) %>% 
  ungroup() %>%
  distinct(carrier, mnAge, medAge) %>% 
  filter(mnAge == max(mnAge, na.rm=T) | medAge == max(medAge, na.rm=T))
  

#------ 14
tmp = flights %>% 
  left_join(airports, by=c('dest' = 'faa')) %>%
  filter(lat >= 45) %>%
  left_join(planes, by="tailnum") %>%
  mutate(age = 2017 - year.y)

tmp %>% 
  distinct(tailnum, seats, age) %>%
  summarize(minAge = min(age, na.rm=T),
            maxAge = max(age, na.rm=T),
            meanSize = mean(seats, na.rm=T))

tmp %>% 
  group_by(origin) %>%
  tally() %>%
  arrange(n)


