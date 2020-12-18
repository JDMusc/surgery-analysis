# Load Data
# user set input data
events = read_csv('data/events_phi_free.csv')
inputData = events
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
plot_data <- filtered_data
p <- plot_data %>% ggplot(aes(y = `Any Event`, x = RelativeTime)) +
  geom_point(aes(colour = Event.Type)) +
  geom_segment(aes(
    xend = RelativeTime,
    yend = 0, y = `Any Event`, colour = Event.Type
  )) +
  ggforce::facet_grid_paginate(Case ~ .,
                               ncol = 1,
                               nrow = 3L, page = 5
  ) +
  scale_y_continuous(breaks = 1)

plot(p)
