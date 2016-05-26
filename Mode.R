modes <- A %>%
  group_by(group_var) %>%
  summarise(mode = Mode(var))