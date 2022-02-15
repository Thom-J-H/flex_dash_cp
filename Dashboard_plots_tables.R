# TJ Haslam
# 15 Feb 2022

# Libraries and data ------------------------------------------------------


library(tidyverse)
library(glue)
library(here)
library(visdat)
library(skimr)
library(plotly)
library(reactable)

load(here::here("data" , "tidy_data", "Hopkins_Final.RData"))

# Custom breaks for the plots to come -------------------------------------

breaks_d <- seq(1955, 2020, by = 10)





# Five Eyes Correlation Plot ----------------------------------------------



lm_graph_5ey <- five_eyes_dat %>% 
  mutate(across(where(is.numeric), round, 2))%>%
  ggplot( aes(x = year, 
              y = life_expectancy,
              size = (log(gdp_per_cap) * 0.6),
              color = country, 
              text = paste("Nation:", country, "<br /> Life Exp:", life_expectancy,  "<br />Per Capita GDP:", gdp_per_cap,
                           "<br /> Year:", year )) ) + 
  geom_point(alpha = 0.7) +
  labs(title = "Five Eyes Nations",
       y ="Life Expectancy", 
       x = "Year + Per Capita GDP + Nation",
       color = "Nation",
       text = "Per Capita",
       subtitle = "life_expectancy ~ per_cap_gdp + country + year") +
  scale_x_continuous(breaks = breaks_d) +  
  theme_minimal() +
  guides(size = "none")
#lm_graph_5ey



lm_graph_5ey <- plotly::ggplotly(lm_graph_5ey, tooltip = "text" )


lm_graph_5ey 


# Five Other Correlation Plot ---------------------------------------------






lm_graph_5ot <- five_other_dat %>%
  mutate(across(where(is.numeric), round, 2) )

lm_graph_5ot <- lm_graph_5ot %>%
  ggplot( aes(x = year, 
              y = life_expectancy,
              size = (log(gdp_per_cap) * 0.6),
              color = country, 
              text = paste("Nation:", country, "<br /> Life Exp:", life_expectancy,  "<br />Per Capita GDP:", gdp_per_cap,
                           "<br /> Year:", year )) ) + 
  geom_point(alpha = 0.7) +
  labs(title = "Five Other Nations",
       y ="Life Expectancy", 
       x = "Year + Per Capita GDP + Nation",
       color = "Nation",
       text = "Per Capita",
       subtitle = "life_expectancy ~ per_cap_gdp + country + year") +
  scale_x_continuous(breaks = breaks_d)  +  
  theme_minimal() +
  guides(size = "none")


lm_graph_5ot <- plotly::ggplotly(lm_graph_5ot, tooltip = "text" )



lm_graph_5ot


# Five Eyes & Other Data Sets ---------------------------------------------
## For the Linear Models

fm_5graphs <- as.formula("life_expectancy ~ gdp_per_cap + country + year")

my_mod_5ey <- lm(formula = fm_5graphs , data = five_eyes_dat)



clean_my_mod_5ey <- broom::tidy(my_mod_5ey, 
                                conf.int = TRUE,  
                                conf.level = 0.95) %>%
  mutate(across(where(is.numeric), round, 4))



clean_my_mod_5ey %>%
  reactable(., highlight = TRUE, 
            striped = TRUE,
            theme = reactableTheme(
              stripedColor = "#EDEDED",
              highlightColor = "#FFE4E1") ) 


### Linear Model (5 Ot)



my_mod_5ot <- lm(formula = fm_5graphs , data = five_other_dat)
clean_my_mod_5ot  <- broom::tidy(my_mod_5ot , 
                                 conf.int = TRUE,  
                                 conf.level = 0.95) %>%
  mutate(across(where(is.numeric), round, 4))

clean_my_mod_5ot %>%
  reactable(., highlight = TRUE, 
            striped = TRUE,
            theme = reactableTheme(
              stripedColor = "#EDEDED",
              highlightColor = "#FFE4E1") ) 




# Global Views -- Choropleths ---------------------------------------------



gapminder_map <- gapminder %>%
  filter(year == 1960 | year == 2016) %>% tibble()




gapminder_map$country <- recode(gapminder_map$country,
                                `Macedonia, FYR`  = "North Macedonia",
                                `West Bank and Gaza` = "Palestine",
                                `Swaziland` =  "Eswatini") 

gapminder_map <- gapminder_map %>% 
  select(year, country, life_expectancy)


gapminder_map_1960 <-  gapminder_map %>% 
  filter(year == 1960) %>% 
  complete(country = world_map2_ISO$country, 
           fill = (list(water = NA )) ) %>%
  left_join(world_map2_ISO, by = "country") %>%
  replace_na(list(year = 1960))

gapminder_map_2016 <-  gapminder_map %>% 
  filter(year == 2016) %>% 
  complete(country = world_map2_ISO$country, 
           fill = (list(water = NA )) ) %>%
  left_join(world_map2_ISO, by = "country") %>%
  replace_na(list(year = 2016))


map_2016 <- gapminder_map_2016 %>%
  filter(code_3 != "ATA") %>%
  ggplot(aes(x = long, 
             y = lat, 
             group = group, 
             label = country)) +
  geom_polygon(aes(fill = life_expectancy) )+
  scale_fill_viridis_c(option = "C") +
  labs(fill = "",
       title = "Life Expectancy (2016)") +
  theme_void() 

plotly::ggplotly(map_2016)





map_1960 <- gapminder_map_1960 %>%
  filter(code_3 != "ATA") %>%
  ggplot(aes(x = long, 
             y = lat, 
             group = group, 
             label = country)) +
  geom_polygon(aes(fill = life_expectancy) )+
  scale_fill_viridis_c(option = "C") +
  labs(fill = "",
       title = "Life Expectancy (1960)") +
  theme_void() 


plotly::ggplotly(map_1960)





# ANOVA Tests -------------------------------------------------------------

five_eyes_2000s <- five_eyes_dat %>%
  filter(decade == "2000s")

anova_one  <- aov(life_expectancy ~ country,  five_eyes_2000s )

results_one  <- TukeyHSD(anova_one) %>% 
  broom::tidy() %>% dplyr::arrange(desc(abs(estimate))) %>%
  mutate(across(where(is.numeric), round, 4))

results_one$decade <- "2000s"

five_eyes_1960s <- five_eyes_dat %>%
  filter(decade == "1960s")

anova_two <- aov(life_expectancy ~ country,  five_eyes_1960s  )

results_two <- TukeyHSD(anova_two ) %>% 
  broom::tidy() %>% dplyr::arrange(desc(abs(estimate))) %>%
  mutate(across(where(is.numeric), round, 4))

results_two$decade <- "1960s"

results_five_eyes <- results_one %>% 
  bind_rows(results_two) %>% 
  select(decade, everything())

## Graph

gg <- results_five_eyes  %>% 
  ggplot(aes(estimate, contrast , 
             color = contrast,
             text = paste(contrast, "<br /> Diff:", estimate,"<br />", 
                          "CI:", conf.low, conf.high))) +
  geom_point() +
  geom_errorbarh(aes(xmin =  conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 0, alpha = 0.3, linetype = 2) +
  facet_wrap(~decade) +
  labs(title = "Life Expectancy Means: 1960s & 2000s",
       subtitle = "ANOVA Results for Five Eyes Nations. 95% CI.",
       y = "", x = "Mean Difference Estimate") +
  guides(color = "none") +
  theme(axis.text.y = element_text(size=7))

gg <- plotly::ggplotly(gg, tooltip = c("text" ))

gg %>% layout(showlegend = FALSE)


##
## ANOVA 5 Other

five_ot_2000s <- five_other_dat %>%
  filter(decade == "2000s")

anova_three <- aov(life_expectancy ~ country,  five_ot_2000s  )

results_three <- TukeyHSD(anova_three ) %>% 
  broom::tidy() %>% dplyr::arrange(desc(abs(estimate))) %>%
  mutate(across(where(is.numeric), round, 4))


results_three$decade <-  "2000s"


five_ot_1960s <- five_other_dat %>%
  filter(decade == "1960s")

anova_four <- aov(life_expectancy ~ country,  five_ot_1960s )

results_four <- TukeyHSD(anova_four ) %>% 
  broom::tidy() %>% dplyr::arrange(desc(abs(estimate))) %>%
  mutate(across(where(is.numeric), round, 4))


results_four$decade <-  "1960s"



results_five_ot <- results_three %>% 
  bind_rows(results_four) %>% 
  select(decade, everything()) 


## Graph


gg2 <- results_five_ot %>% 
  ggplot(aes(estimate, contrast , 
             color = contrast,
             text = paste(contrast, "<br /> Diff:", estimate,"<br />", 
                          "CI:", conf.low, conf.high))) +
  geom_point() +
  geom_errorbarh(aes(xmin =  conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 0, alpha = 0.3, linetype = 2) +
  facet_wrap(~decade) +
  labs(title = "Life Expectancy Means: 1960s & 2000s",
       subtitle = "ANOVA Results for Five Other Nations. 95% CI.",
       y = "", x = "Mean Difference Estimate") +
  guides(color = "none") +
  theme(axis.text.y = element_text(size=7))

gg2 <- plotly::ggplotly(gg2, tooltip = c("text" ))

gg2 %>% layout(showlegend = FALSE)



### Data Tables

## FIVE EYES
results_five_eyes  %>%
  select(-term) %>%
  reactable(., highlight = TRUE, 
            striped = TRUE,
            theme = reactableTheme(
              stripedColor = "#EDEDED",
              highlightColor = "#FFE4E1") ) 

## FIVE OT
results_five_ot %>%
  select(-term) %>%
  reactable(., highlight = TRUE, 
            striped = TRUE,
            theme = reactableTheme(
              stripedColor = "#EDEDED",
              highlightColor = "#FFE4E1") ) 



# Energy Usage Plots and Tables -------------------------------------------


energy_big_ten <- energy_tidy %>% 
  filter(country %in% big_ten)

energy_big_ten <- energy_big_ten %>% 
  left_join(big_ten_dat, by =c("country", "year", "decade") )

energy_big_ten <- energy_big_ten %>% 
  select(country, year, energy, life_expectancy) %>%
  filter(year != 2015) %>% 
  filter(year >= 1990)



energy_big_ten %>% 
  plot_ly(x = ~year, 
          y = ~energy, 
          text = ~paste("Nation:", country,
                        "<br />Energy:", energy,
                        "<br />Year:", year),
          mode = "lines+markers",
          color = ~country, colors = "Set3",
          showlegend = TRUE ) %>%
  layout(
    xaxis = list(title = "Year"),
    yaxis = list(title = "Energy Usage Per Capita"),
    title = "Big Ten: Energy Usage Per Capita" )


energy_big_ten %>% 
  plot_ly(x = ~energy, 
          y = ~life_expectancy, 
          text = ~paste("Nation:", country,
                        "<br />Energy:", energy,
                        "<br />Life Expectancy:", life_expectancy),
          size = 30) %>%
  add_markers(frame = ~year, ids = ~country, 
              color = ~country, colors = "Set3",
              showlegend = TRUE,
  ) %>%
  layout(
    xaxis = list(title = "Energy Usage Per Capita"),
    yaxis = list(title = "Life Expectancy"),
    title = "Life Expectancy vs. Energy Usage Per Capita" )



energy_big_ten %>%
  select(year, everything()) %>%
  arrange(year) %>%
  reactable(., highlight = TRUE, 
            striped = TRUE,
            theme = reactableTheme(
              stripedColor = "#EDEDED",
              highlightColor = "#FFE4E1") ) 
























