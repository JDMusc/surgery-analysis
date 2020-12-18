library(readtv)
library(changepoint)
library(tidyverse)


events = read_csv('data/events_phi_free.csv')

inputData = events
# Load Data
# user set input data
data <- inputData %>%
  mutate(`Any Event` = 1) %>%
  group_by(Case) %>%
  group_modify(~ .x %>% mutate(deltaTime = Time -
                                 lag(Time), RelativeTime = Time - min(Time,
                                                                      na.rm = TRUE
                                 ))) %>%
  ungroup()

# Basic Filter
selected_data <- data
filtered_data <- selected_data

# CPA
cpa_input_df <- filtered_data %>% preprocessForCpa(3,
                                                   agg_fn_expr = length(.values), stride = 1, index_col = "RelativeTime",
                                                   values_col = NULL, facet_col = "Case"
)
cpa_markers <- cpa_input_df %>%
  group_by(Case) %>%
  filter(n() > 1) %>%
  group_modify(~ arrange(.x, RelativeTime)) %>%
  group_modify(~ cpt.mean(pull(.x, CpaInput),
                          Q = 3,
                          method = "BinSeg", penalty = "BIC", pen.value = 0.05
  ) %>% {
    data.frame(
      cpts = pull(.x, RelativeTime)[.@cpts],
      vals = .@param.est$mean
    )
  })
base_plot_df <- filtered_data %>% mutate(`:=`(
  Phase,
  factor(Phase)
))
base_p <- base_plot_df %>% ggplot(aes(
  y = `Any Event`,
  x = RelativeTime
)) +
  geom_point(aes(
    shape = Phase,
    colour = Event.Type
  )) +
  geom_segment(aes(
    xend = RelativeTime,
    yend = 0, y = `Any Event`, colour = Event.Type
  )) +
  ggforce::facet_grid_paginate(Case ~ .,
                               ncol = 1,
                               nrow = 3L, page = 1
  ) +
  scale_y_continuous(breaks = 1)
event_freq_p <- base_p + geom_line(aes(
  x = RelativeTime,
  y = CpaInput
), data = cpa_input_df, linetype = "dotdash") +
  scale_y_continuous() + ylab("Any Event; count of Any Event")
p <- addCpaMarkersToPlot(
  event_freq_p, cpa_markers,
  cpa_input_df, "RelativeTime", "CpaInput"
)

plot(p)
