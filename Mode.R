Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Example using dplyr

modes <- A %>%      
  group_by(group_var) %>%
  summarise(mode = Mode(var))