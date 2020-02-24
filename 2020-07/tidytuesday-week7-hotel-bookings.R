pacman::p_load(tidyverse, lubridate, DataExplorer, timeDate, rcartocolor, ggmap, gganimate, ggrepel, plotly, transformr)
# Get the Data

hotels <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-11/hotels.csv') %>% 
  mutate(date = glue::glue("{arrival_date_year}-{arrival_date_month}-{arrival_date_day_of_month}"),
         date = lubridate::ymd(date))

hotels %>%
  ggplot(aes(x=hotel, fill=hotel))+
  geom_bar()+
  labs(title = "Bookings in the dataset per hotel (Arrival date: 01.07.2015 - 31.08.2017)",
       x= NULL,
       y= "Number of Bookings")+
  guides(fill=FALSE)+
  theme_minimal()+
  theme(plot.background = element_rect(fill = "#F4F4F4"))

#calculating the stay duration and data cleaning
days_duration <- hotels %>% 
  dplyr::filter(reservation_status != "Canceled") %>%
  select(hotel, date, adults, children, reservation_status, reservation_status_date, stays_in_week_nights, stays_in_weekend_nights, days_in_waiting_list) %>% 
  mutate(reservation_status_date = ymd(reservation_status_date),
         nights_stay = stays_in_week_nights+stays_in_weekend_nights) %>% 
  select(-stays_in_week_nights, -stays_in_weekend_nights) %>% 
  mutate(nights_stay = ifelse(reservation_status == "No-Show", 1, nights_stay)) %>% #reducing no-shows to one night stays
  filter(nights_stay != 0) %>% #remove 0 days stays
  mutate(check_out_date = date+nights_stay,
         last_night = check_out_date-1) %>% #last night of stay were beds are occupied (at check out date new guest can move in)
  mutate(booking_id = row_number()) #add booking id for unnesting

days_duration_unnested <- days_duration %>%
  transmute(booking_id = booking_id,
            date = map2(date, last_night,  ~ seq(.x, .y, by = 'day')), #add date sequence for each day of the booking
            beds = adults+children, #calculate number of beds per booking
            nights_stay = nights_stay,
            hotel = hotel) %>%
  unnest(cols = c(date)) #bring table into long format one row per day per booking

hotels_summary <- days_duration_unnested %>% 
  group_by(hotel, date) %>% 
  summarise(beds_occupied = sum(beds)) %>% 
  ungroup() %>% 
  group_by(hotel) %>% 
  summarise(avg= round(mean(beds_occupied), 2), 
            min=min(beds_occupied), 
            max=max(beds_occupied), 
            SD= round(sd(beds_occupied), 2))

city_hotel_max <- hotels_summary$max[1]
resort_hotel_max <- hotels_summary$max[2]

hotels_summary

#plot: animated weekly average bed utilization
p_weekly_utilization <- days_duration_unnested %>% 
  group_by(hotel, date) %>%
  mutate(utilization_rate = ifelse(hotel == "City Hotel", sum(beds)/city_hotel_max, sum(beds)/resort_hotel_max)) %>%
  ungroup() %>%
  group_by(hotel, week =isoweek(date)) %>%
  summarise(avg_utilization_rate = mean(utilization_rate)) %>% 
  ggplot(aes(x=week, y=avg_utilization_rate, color = hotel))+
  geom_line(size=1)+
  theme_minimal()+
  theme(plot.background = element_rect(fill = "#F4F4F4"))+
  labs(title = "Weekly average bed utilization",
       x = "Week number",
       y = "Utilization in %",
       color = NULL)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  gganimate::transition_reveal(as.numeric(week))

gganimate::animate(p_weekly_utilization, end_pause = 40)

#plot: bed utilization by weekday/hotel
days_duration_unnested %>% 
  group_by(hotel, date) %>%
  mutate(utilization_rate = ifelse(hotel == "City Hotel", sum(beds)/city_hotel_max, sum(beds)/resort_hotel_max)) %>%
  ungroup() %>%
  group_by(hotel,
           weekday = wday(date, label = T, abbr = F)) %>%
  ggplot(aes(x=weekday, y=utilization_rate, fill = hotel))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.background = element_rect(fill = "#F4F4F4"))+
  labs(title = "Average bed utilization by weekday",
       x = NULL,
       y = "Utilization in %",
       fill = NULL)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

#plot: average nights per month/hotel
days_duration_unnested %>% 
  group_by(hotel, month=month(date, label = T, abbr = T)) %>%
  summarise(avg_nights = mean(nights_stay)) %>% 
  ggplot(aes(x=month, y=avg_nights, fill = hotel))+
  geom_bar(position="dodge", stat="identity")+
  theme_minimal()+
  theme(plot.background = element_rect(fill = "#F4F4F4"))+
  labs(title = "Average number of nights per booking",
       x = NULL,
       y = "Number of nights",
       fill = NULL)
