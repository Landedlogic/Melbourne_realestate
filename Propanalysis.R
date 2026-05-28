
df %>%
  summarise(
    median_income = median(suburb_median_income, na.rm = TRUE),
    .by = property_type)
