library(readr)
library(ggmap)
library(dplyr)
conflict.data <- read_csv(
    "ACLED Version 6 All Africa 1997-2015_csv_dyadic.csv.gz")
bbox <- make_bbox(LONGITUDE,
                  LATITUDE,
                  data=conflict.data,
                  f=0.2)
africa <- get_map(bbox)
ggmap(africa) +
    geom_point(aes(x=LONGITUDE,
                   y=LATITUDE,
                   color=YEAR,
                   size=FATALITIES),
               data=conflict.data %>%
                   filter(FATALITIES < 6000),
               alpha=0.5) +
    xlim(-20, 40) +
    ylim(-35, 35) +
    scale_color_gradient(limits=c(1997, 2015),
                         low="orangered1",
                         high="red4")
ggsave("africa_conflicts.png")
