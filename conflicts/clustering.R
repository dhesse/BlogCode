library(readr)
library(ggmap)
library(dplyr)
library(lubridate)
conflict.data <- read_csv(
    "ACLED Version 6 All Africa 1997-2015_csv_dyadic.csv.gz")
conflict.data <- conflict.data %>%
    filter(FATALITIES > 10) %>%
    mutate(lat=scale(LATITUDE),
           long=scale(LONGITUDE),
           time=scale(as.numeric(dmy(EVENT_DATE)))
           )
model <- hclust(
    dist(
        conflict.data %>%
        select(lat, long, time)))
conflict.data$CLUSTER <- as.factor(cutree(model, h=3))
bbox <- make_bbox(LONGITUDE,
                  LATITUDE,
                  data=conflict.data,
                  f=0.2)
africa <- get_map(bbox)
ggmap(africa) +
    geom_point(aes(x=LONGITUDE,
                   y=LATITUDE,
                   color=CLUSTER),
               data=conflict.data,
               size=2.5) +
    xlim(-20, 40) +
    ylim(-35, 35)
ggsave("africa_clusters.png")
actor1.counts <- conflict.data %>%
    group_by(CLUSTER, ACTOR1) %>%
    summarise(actor1count = n(),
              actor1fatalities = sum(FATALITIES)) %>%
    select(CLUSTER,ACTOR=ACTOR1,actor1count,actor1fatalities)
actor2.counts <- conflict.data %>%
    group_by(CLUSTER, ACTOR2) %>%
    summarise(actor2count = n(),
              actor2fatalities = sum(FATALITIES)) %>%
    select(CLUSTER,ACTOR=ACTOR2,actor2count,actor2fatalities)
actor.counts <- actor1.counts %>%
    full_join(actor2.counts)
actor.counts[is.na(actor.counts)] <- 0
actor.counts <- actor.counts %>%
    mutate(`Incident Count` = actor1count + actor2count,
           Fatalities = actor1fatalities + actor2fatalities)
actor.counts %>%
    select(CLUSTER, ACTOR, `Incident Count`) %>%
    group_by(CLUSTER) %>%
    filter(min_rank(desc(`Incident Count`)) <= 3) %>%
    ungroup() %>%
    head()
htmlTable(actor.counts %>%
    select(CLUSTER, ACTOR, `Incident Count`) %>%
    group_by(CLUSTER) %>%
    filter(min_rank(desc(`Incident Count`)) <= 3) %>%
    ungroup() %>%
    arrange(CLUSTER, desc(`Incident Count`)))
