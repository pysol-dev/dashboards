
#### economy
rm(list=ls())

library(fredr)
library(tsibble)
library(dplyr)

### Set Key
fredr_set_key("8454091b420f6979c70e84de8e611118")

### OFFICIAL Unemployment Rate ###
pop <- fredr(series_id = "POPTHM") ###population monthly 1959
output <- fredr(series_id = "PRS85006092") ##percent 1947 annual rate quarterly
gdp <- fredr(series_id = "GDPC1") 

### turn pop data into quarterly
###first turn date into date format
pop <- pop %>%
  mutate(date = as.Date(date, format="%Y%m%d"))

###1959 Q1
pop_qtr <- pop %>%
  mutate(qtr = yearquarter(date)) %>%
group_by(qtr) %>%
  summarize_all(mean)
###rename population
pop_qtr$pop <- pop_qtr$value

head(pop_qtr)

###turn into percent change annually
pop_percent <- pop_qtr %>%
  mutate(pop_change = (pop-lag(pop))/lag(pop)*100) %>%
as_tsibble(index=qtr)

###add on productivity value columns
###sounds like a right join

colnames(output)
output$output <- output$value

###output as date format
output <- output %>%
  mutate(date = as.Date(date, format="%Y%m%d"),
         qtr=yearquarter(date)) %>%
  as_tsibble(index=qtr)


pot_gdp <- left_join(pop_percent, output, by="qtr")

pot_gdp <- pot_gdp %>%
  mutate(pot_gdp = pop_change + output)

pot_gdp$realtime_end.x <- NULL
pot_gdp$realtime_end.y <- NULL
pot_gdp$value.x <- NULL
pot_gdp$value.y <- NULL
pot_gdp$realtime_start.y <- NULL
pot_gdp$realtime_start.x <- NULL
pot_gdp$series_id.x <- NULL
pot_gdp$series_id.y <- NULL
pot_gdp$date.x <- NULL
pot_gdp$date.y <- NULL

head(pot_gdp)

output <- output %>%
  filter_index("1959 Q1" ~ .)
head(pop_percent)
library(ggplot2)
library(tsibble)
library(dplyr)
library(fpp3)
install.packages("ggplot2")

pot_gdp %>%
  filter_index("1970" ~ .) %>%
  autoplot(pot_gdp)

###plot potential gdp
ggplot(pot_gdp) +
  geom_line(aes(qtr, pot_gdp), color="darkblue") +
  geom_hline(yintercept = 0) +
  labs(title="Potential Output",
       subtitle="Annual Percent Change",
       y="Output",
       x="Date") +
  theme_bw()

###gdp work

gdp <- gdp %>%
  mutate(gdp_change = (value-lag(value))/ lag(value)*100,
         qtr=yearquarter(date)) %>%
         as_tsibble(index=qtr)

head(gdp)

###left join master output gap

output_gap_trial <- left_join(pot_gdp, gdp, by="qtr")

###create output gap variable in master set

output_gap <- output_gap_trial %>%
  mutate(output_gap = gdp_change - pot_gdp)

output_gap$realtime_end <- NULL
output_gap$realtime_start <- NULL
output_gap$value <- NULL
output_gap$realtime_start <- NULL
output_gap$realtime_start <- NULL
output_gap$series_id <- NULL
head(output_gap)

###output gap
###federal funds rate
###graphed
ggplot(output_gap) +
  geom_line(aes(qtr, output_gap), color="darkblue") +
  geom_line(aes(qtr, funds)) +
  geom_hline(yintercept=0) +
  labs(title="Output Gap",
       subtitle="Quarterly Change",
       y="Percent",
       x="Date",
       caption="Source: FRED") +
  theme_bw()
  
  
### check how funds rate and output gap relate

funds <- fredr(series_id = "FEDFUNDS") ###1954
head(funds)

funds %>%
  mutate(date = as.Date(date, format="%Y%m%d"),
         qtr = yearquarter(date))

funds <- funds %>%
  mutate(qtr = yearquarter(date)) %>%
  group_by(qtr) %>%
  summarize_all(mean)

funds <- funds %>%
  mutate(funds_change = ((funds - lag(funds))/lag(funds))*100)
  as_tsibble(index=qtr)

funds <- funds %>%
  filter_index("1959 Q1" ~ .)

funds <- funds %>%
  as_tsibble(index=qtr)

funds$funds <- funds$value

ggplot(x=funds$funds, y=output_gap$output_gap) +
  geom_point() +
  geom_smooth()

###left join funds to output gap

output_gap <- left_join(output_gap, funds, by="qtr")

head(output_gap)

output_gap$realtime_end <- NULL
output_gap$realtime_start <- NULL
output_gap$date.x <- NULL
output_gap$date.y <- NULL
output_gap$date.y <- NULL
output_gap$realtime_start <- NULL
output_gap$realtime_start <- NULL
output_gap$series_id <- NULL

###correlation test
cor.test(output_gap$output_gap, output_gap$funds)

### geom_point plot 
ggplot(output_gap, aes(funds, output_gap)) +
  geom_point() +
  geom_smooth() +
  labs(title="Output Gap ~ Funds Rate",
       subtitle="95 % CI (-0.00019 - 0.243)",
       y="Output Gap",
       x="Federal Funds Rate") +
  theme_bw()

reg <- lm(funds ~ output_gap, data=output_gap)

plot(reg)













  
