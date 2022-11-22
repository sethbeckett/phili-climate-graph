library(tidyverse)
library(lubridate)

# phil_data <- read.csv('data/USC00366886.csv')
# 
# phil_cols <- colnames(phil_data)
# 
# phil_cols[str_detect(phil_cols, 'AVG') & str_detect(phil_cols, 'WI')]
# 
# phil_data['comp_flag_ANN.TAVG.NORMAL']

# pulled data from https://www.ncei.noaa.gov/access/us-climate-normals/#dataset=normals-annualseasonal&timeframe=30&location=PA&station=USW00013739
# to get 44.2 F avg winter temp

# pulled from specific station USC00366886
# data ordered from https://www.ncdc.noaa.gov/cdo-web and didn't actually use above station, used airport data

# https://www.ncdc.noaa.gov/cdo-web/datasets/GHCND/locations/CITY:US420015/detail

avg_temp <- 44.2

phil_temps <- read_csv('data/3144244.csv')

summary(phil_temps$DATE)

head(phil_temps)

phil_winters <- phil_temps |>
  filter(month(DATE) %in% c(12, 1, 2))

phil_winters$TAVG <- (phil_winters$TMAX + phil_winters$TMIN) / 2

phil_winters <- phil_winters[c(3, 6)]

phil_winters$YEAR <- case_when(
  month(phil_winters$DATE) == 12 ~ year(phil_winters$DATE) + 1,
  TRUE ~ year(phil_winters$DATE)
)

phil_winters <- phil_winters[c(2,3)]

phil_winters$ABV_AVG <- phil_winters$TAVG > avg_temp

avg_abv <- phil_winters |>
  group_by(YEAR) |>
  summarise(days_abv = sum(ABV_AVG, na.rm = TRUE)) |>
  filter(YEAR != 2022)



ggplot(avg_abv, aes(x = YEAR, y = days_abv)) +
  geom_line(size=1.5) +
  geom_smooth(method = 'lm') +
  theme_bw() +
  ylab('Days Above Avg') +
  xlab('Year') +
  scale_x_continuous(breaks = seq(1990, 2025, 5)) +
  ggtitle("Philadelphia Winter Days Above Avg", "(Avg is average winter temp from 1991-2020)") +
  theme(
    text = element_text(size = 18),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    plot.title = element_text(hjust = 0.5)
  )
  