library(tidyverse)
url_in <-"https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"

file_names <- c("time_series_covid19_confirmed_US.csv", 
                "time_series_covid19_confirmed_global.csv", 
                "time_series_covid19_deaths_US.csv",
                "time_series_covid19_deaths_global.csv")
urls <- str_c(url_in, file_names)
urls


us_cases <- read_csv(urls[1])
global_cases <- read_csv(urls[2])
us_deaths <- read_csv(urls[3])
global_deaths <- read_csv(urls[4])

global_cases

global_cases <- global_cases %>%
    pivot_longer(cols = -c('Province/State','Country/Region','Lat','Long'),
                    names_to =  "date",
                    values_to = "cases") %>%
    select(-c(Lat,Long))



global_deaths <- global_deaths %>%
    pivot_longer(cols = -c('Province/State','Country/Region'
                           ,'Lat','Long'),
                 names_to = "date",
                 values_to = "deaths") %>%
    select(-c(Lat,Long))


global <- global_cases %>% 
  full_join(global_deaths) %>%
  rename(Country_Region = 'Country/Region',
         Province_State = 'Province/State') %>%
  mutate(date= mdy(date))

summary(global)

global <- global %>% filter(cases >0)

global %>% filter(cases > 103000000)

us_cases <- us_cases %>% 
  pivot_longer(cols= -c(UID:Combined_Key), names_to = 'date', values_to = 'cases') %>%
    select(Admin2:cases) %>%
    mutate(date = mdy(date)) %>%
    select(-c(Lat,Long_))

us_cases
us_deaths <- us_deaths %>% 
  pivot_longer(cols= -c(UID:Population), names_to = 'date', values_to = 'deaths') %>%
  select(Admin2:deaths) %>%
  mutate(date = mdy(date)) %>%
  select(-c(Lat,Long_))

us <- us_cases %>% full_join(us_deaths)
us

global <- global %>% 
        unite("Combined_Key",
              c(Province_State, Country_Region),
              sep = ", ",
              na.rm= TRUE,
              remove= FALSE)

uid_lookup_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/UID_ISO_FIPS_LookUp_Table.csv"
uid <- read_csv(uid_lookup_url) %>% select( -c(Lat,Long_, Combined_Key, code3,iso2,iso3,Admin2))
uid

global <- global %>% 
        left_join(uid, by = c("Province_State","Country_Region")) %>%
        select(-c(UID,FIPS)) %>% select(Province_State, Country_Region, date,
                                          cases,deaths,Population, Combined_Key)

#if we want to look at it by state and country we group it

us_by_state <- us %>%
        group_by(Province_State, Country_Region, date) %>%
        summarize(cases=sum(cases), deaths= sum(deaths),
                  Population= sum(Population)) %>%
        mutate(deaths_per_mill = deaths * 1000000 / Population) %>%
        select(Province_State, Country_Region, date,
               cases, deaths, deaths_per_mill, Population) %>%
        ungroup()

#if we want to look at the whole us we group it by Country_Region
us_totals <- us_by_state %>%
  group_by(Country_Region, date) %>%
  summarize(cases=sum(cases), deaths= sum(deaths),
            Population= sum(Population)) %>%
  mutate(deaths_per_mill = deaths * 1000000 / Population) %>%
  select( Country_Region, date,
         cases, deaths, deaths_per_mill, Population) %>%
  ungroup()

tail(us_totals)

us_totals %>%
    filter(cases> 0) %>%
    ggplot(aes(x=date, y=cases)) + 
    geom_line(aes(color= "cases")) +
    geom_point(aes(color= "cases")) +
    geom_line(aes( y= deaths, color="deaths")) +
    geom_point(aes(y= deaths, color= "deaths")) +
    scale_y_log10() + 
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle= 90)) +
    labs(title = "COVID19 in US", y = NULL)

#LETS make a graph for the state of NY
state <- "New York"

us_by_state %>%
  filter(cases> 0) %>%
  filter(Province_State == state) %>%
  ggplot(aes(x=date, y=cases)) + 
  geom_line(aes(color= "cases")) +
  geom_point(aes(color= "cases")) +
  geom_line(aes( y= deaths, color="deaths")) +
  geom_point(aes(y= deaths, color= "deaths")) +
  scale_y_log10() + 
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle= 90)) +
  labs(title = str_c("COVID19 in ", state), y = NULL)

max(us_totals$date)
max(us_totals$deaths)


us_by_state <- us_by_state %>% 
        mutate(new_cases = cases - lag(cases),
                new_deaths = deaths - lag(deaths))
us_totals <- us_totals %>% 
        mutate(new_cases= cases - lag(cases),
               new_deaths = deaths - lag(deaths))

tail(us_totals %>% select(new_cases, new_deaths,everything()))

#graph new cases and new deaths

us_totals %>%
  
  ggplot(aes(x=date, y=new_cases)) + 
  geom_line(aes(color= "new_cases")) +
  geom_point(aes(color= "new_cases")) +
  geom_line(aes( y= new_deaths, color="new_deaths")) +
  geom_point(aes(y= new_deaths, color= "new_deaths")) +
  scale_y_log10() + 
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle= 90)) +
  labs(title = "COVID19 in US", y = NULL)

#new cases by state

us_by_state %>%
  filter(Province_State == state) %>%
  ggplot(aes(x=date, y=new_cases)) + 
  geom_line(aes(color= "new_cases")) +
  geom_point(aes(color= "new_cases")) +
  geom_line(aes( y= new_deaths, color="new_deaths")) +
  geom_point(aes(y= new_deaths, color= "new_deaths")) +
  scale_y_log10() + 
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle= 90)) +
  labs(title = str_c("COVID19 in ", state), y = NULL)

#what are the worst and best cases? how do we measure this?
#lets transform the data again
#group by state then choose the max cases by state 
us_state_totals <- us_by_state %>% 
      group_by(Province_State) %>% 
      summarize(deaths= max(deaths), cases= max(cases),
                population= max(Population),
                cases_per_thou = (1000 * cases) /population,
                deaths_per_thou = (1000 * deaths) / population) %>%
    filter(cases > 0, population > 0)

#bottom 10 states with lowest deaths per thousand
us_state_totals %>%
      slice_min(deaths_per_thou, n = 10) %>%
  select(deaths_per_thou, cases_per_thou, everything())

#top 10 states with highest deaths per thousand

us_state_totals %>%
  slice_max(deaths_per_thou, n = 10) %>%
  select(deaths_per_thou, cases_per_thou, everything())

#are deaths recorded the same in all these states? how valid is this data?
#BUILD YOUR MODEL 
#do we want to consider elevation? climate? location? etc to the model?
#Does the variable have a sttatistical significance?

#LINEAR MODEL
mod <- lm(deaths_per_thou ~ cases_per_thou, data = us_state_totals)
summary(mod)

#min and max cases per thousand state
us_state_totals %>% slice_min(cases_per_thou)

us_state_totals %>% slice_max(cases_per_thou)

x_grid <- seq(1,450)
new_df <- tibble(cases_per_thou = x_grid)
us_state_totals %>% mutate(pred= predict(mod))

us_tot_w_pred <- us_state_totals %>% mutate(pred=predict(mod))

#graphing actuals vs predictions using linear modeling
us_tot_w_pred %>% ggplot() +
  geom_point(aes(x= cases_per_thou,y=deaths_per_thou), color= "blue") +
  geom_point(aes(x = cases_per_thou, y = pred),color="red")

#obviously this is just a simple linear regression so we might have to 
#consider some other variables or models etc...

#ONE of the final analysis is finding Biase sources.
#I might have some bias against the topic I chose. How do I handle outliers?
# do i get rid of them? sometimes they are more interesting. 
# WE have to recognize our bias and set it aside 