library(tidyverse)
library(glue)
library(here)
library(visdat)
library(skimr)


# Ds labs check -----------------------------------------------------------

gapminder <- dslabs::gapminder 

gapminder <- gapminder %>% 
  mutate(gdp_per_cap = gdp / population)


gapminder <- gapminder %>% 
  mutate(decade = case_when(year >= 1960 & year < 1970 ~ "1960s",
                            year >= 1970 & year < 1980 ~ "1970s",
                            year >= 1980 & year < 1990 ~ "1980s",
                            year >= 1990 & year < 2000 ~ "1990s",
                            year >= 2000 & year < 2010 ~ "2000s",
                            year >= 2010 & year < 2020 ~ "2010s",
                            TRUE ~ NA_character_ )   %>%
           as_factor()
  ) 

gapminder %>% skimr::skim()


## Data subsets

big_ten <- c("Japan" , "Russia", "Italy", "Norway","Mexico",
             "Australia", "Canada", "New Zealand", "United Kingdom", 
             "United States")

five_eyes <- c("Australia", "Canada", "New Zealand", "United Kingdom", 
               "United States")

five_other <- c("Japan" , "Russia", "Italy", "Norway","Mexico")



## Big Ten
big_ten_dat <- gapminder %>% 
  filter(country %in% big_ten) 

big_ten_dat <-  big_ten_dat %>% droplevels()

big_ten_dat %>% 
  skimr::skim()

big_ten_dat$country %>% 
  unique()


## Five Eyes
five_eyes_dat <- big_ten_dat  %>% 
  filter(country %in% five_eyes)

five_eyes_dat %>% 
  skimr::skim()

## Five Other
five_other_dat <- big_ten_dat  %>%  
  filter(country %in% five_other)

five_other_dat %>% 
  skimr::skim()

#### Use read_csv() or another function
load(here::here("data", "tidy_data", "world_map2_project.rda" ))




# Add Gapminder Downloads -------------------------------------------------


# raw Global Studies dat from Gapminder.org
water_access <- read_csv(
  here::here("data",  
             "raw_data", 
             "at_least_basic_water_source_overall_access_percent.csv")  
  )    


# raw Global Studies dat from Gapminder.org
gni_percapita <- read_csv(
  here::here("data","raw_data", 
             "gnipercapita_ppp_current_international.csv") 
  )



# raw Global Studies dat from Gapminder.org
energy_capita <- read_csv(here::here("data",
                                     "raw_data",
                                     "energy_use_per_person.csv"))







# Energy Case Study -------------------------------------------------------

energy_capita  %>% vis_dat()

energy_capita  %>% glimpse()


setdiff(energy_capita$country, world_map2_ISO$country)

setdiff(world_map2_ISO$country, energy_capita$country) %>% 
  enframe(name = NULL, value ="diff")


energy_tidy <- energy_capita  %>%
  pivot_longer(cols = !country,
               names_to = "year",
               names_transform = list(year = as.integer),
               values_to = "energy")


energy_tidy <- energy_tidy %>% 
  mutate(decade = case_when(year >= 1960 & year < 1970 ~ "1960s",
                            year >= 1970 & year < 1980 ~ "1970s",
                            year >= 1980 & year < 1990 ~ "1980s",
                            year >= 1990 & year < 2000 ~ "1990s",
                            year >= 2000 & year < 2010 ~ "2000s",
                            year >= 2010 & year < 2020 ~ "2010s",
                            TRUE ~ NA_character_ )   %>%
           as_factor()
  ) 

energy_tidy  %>% vis_dat()

energy_tidy  %>% glimpse()





# Water Acess Case Study --------------------------------------------------

water_access  %>% vis_dat()
water_access  %>% glimpse()


setdiff(water_access$country,world_map2_ISO$country)
setdiff(world_map2_ISO$country, water_access$country)  %>% 
  enframe(name = NULL, value ="diff")

## Tidy it
water_tidy <- water_access %>%
  pivot_longer(cols = !country,
               names_to = "year",
               names_transform = list(year = as.integer),
               values_to = "water")




water_tidy <- water_tidy %>% 
  mutate(decade = case_when(year >= 1960 & year < 1970 ~ "1960s",
                            year >= 1970 & year < 1980 ~ "1970s",
                            year >= 1980 & year < 1990 ~ "1980s",
                            year >= 1990 & year < 2000 ~ "1990s",
                            year >= 2000 & year < 2010 ~ "2000s",
                            year >= 2010 & year < 2020 ~ "2010s",
                            TRUE ~ NA_character_ )   %>%
           as_factor()
  ) 

water_tidy %>% 
  vis_dat()

water_tidy %>% 
  glimpse()



# GNI Per Capita PPP Case Study -------------------------------------------

gni_percapita %>% 
  vis_dat()

gni_percapita %>% 
  glimpse()


setdiff(gni_percapita$country, world_map2_ISO$country) %>% 
  enframe(name = NULL, value ="diff")

setdiff(world_map2_ISO$country, gni_percapita$country) %>% 
  enframe(name = NULL, value ="diff")


#
## Troubleshoot to TIDY
#


# convert char data to numeric 
gni_tidy <- gni_percapita %>%
  pivot_longer(cols = !country,
               names_to = "year",
               names_transform = list(year = as.integer),
               values_to = "gni_ppp_cap") %>%
  mutate(gni_ppp_cap = readr::parse_number(gni_ppp_cap) )%>%
  mutate(gni_ppp_cap = case_when(gni_ppp_cap < 200 ~ gni_ppp_cap * 1000,
                                 TRUE ~ gni_ppp_cap) )

# reconcile names
gni_tidy  <- gni_tidy %>%
  mutate( country = case_when(country == "Curaçao" ~ "Curacao",
                              country == "Sint Maarten (Dutch part)"  ~ "Sint Maarten" ,
                              TRUE ~ country) )


gni_tidy  <- gni_tidy %>%
  mutate(decade = case_when(year >= 1960 & year < 1970 ~ "1960s",
                            year >= 1970 & year < 1980 ~ "1970s",
                            year >= 1980 & year < 1990 ~ "1980s",
                            year >= 1990 & year < 2000 ~ "1990s",
                            year >= 2000 & year < 2010 ~ "2000s",
                            year >= 2010 & year < 2020 ~ "2010s",
                            TRUE ~ NA_character_ )   %>%
           as_factor()
  ) 




# Brief check
gni_tidy  %>% slice_min(gni_ppp_cap, n = 4)
gni_tidy %>% slice_max(gni_ppp_cap, n = 4)

setdiff(gni_tidy$country, world_map2_ISO$country)

gni_tidy %>%
  vis_dat()



del_these <- c("water_access", "energy_capita" , "gni_percapita")


rm(list = del_these)

rm(del_these)


# Save Data ---------------------------------------------------------------

#save.image("~/R_STUDIO/Hopkins_VIZ/data/tidy_data/Hopkins_Final.RData")

save.image(here::here("data" , "tidy_data", "Hopkins_Final.RData"))

