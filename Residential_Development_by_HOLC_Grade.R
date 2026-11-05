library(sf)
library(dplyr)
library(patchwork)
library(ggplot2)

my_shape <- st_read("S26-stat436-group-project-main/data/holc/milwaukee_holc.geojson")
plot(st_geometry(my_shape))

master_property_sales <- read.csv("S26-stat436-group-project-main/data/property_sales/master_property_sales.csv")

residential_taxkeys <- master_property_sales |>
  filter(PropType %in% c("Residential", "Condominium", "Lg Apartment")) |>
  pull(taxkey) |>
  unique()

tract_summary <- mprop |>
  filter(
    TAXKEY %in% residential_taxkeys,
    !is.na(YR_BUILT),
    !is.na(GEO_TRACT)
  ) |>
  mutate(era = case_when(
    YR_BUILT < 1950       ~ "Pre-1950",
    YR_BUILT < 1980       ~ "1950–1979",
    YR_BUILT < 2000       ~ "1980–1999",
    YR_BUILT >= 2000      ~ "2000–Present"
  )) |>
  group_by(GEO_TRACT, era) |>
  summarise(homes_built = n(), .groups = "drop")

holc_grades <- my_shape |>
  st_drop_geometry() |>
  select(GEOID, grade, pct_tract) |>
  mutate(GEOID = as.character(GEOID))

tract_summary <- tract_summary |>
    mutate(GEO_TRACT = paste0("55079", str_pad(GEO_TRACT, width = 6, pad = "0")))

grade_summary <- tract_summary |>
  mutate(GEO_TRACT = as.character(GEO_TRACT)) |>
  left_join(holc_grades, by = c("GEO_TRACT" = "GEOID")) |>
  filter(!is.na(grade)) |>
  mutate(homes_weighted = homes_built * pct_tract) |>
  group_by(grade, era) |>
  summarise(homes_built = sum(homes_weighted, na.rm = TRUE), .groups = "drop") |>
  mutate(era = factor(era, levels = c("Pre-1950", "1950–1979", "1980–1999", "2000–Present")))


# Map
p_map <- ggplot(my_shape) +
  geom_sf(aes(fill = grade)) +
  scale_fill_manual(
    values = c("A" = "#76a865", "B" = "#7cb9e8",
               "C" = "#ffff00", "D" = "#d9534f"),
    na.value = "grey90"
  ) +
  labs(fill = "HOLC Grade") +
  theme_void()

# Bar chart
p_bar <- ggplot(grade_summary, aes(x = grade, y = homes_built, fill = era)) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "RdYlBu") +
  labs(
    x = "HOLC Grade",
    y = "Homes Built",
    fill = "Era"
  ) +
  theme_minimal()

# Combine
combined <- p_map + p_bar +
  plot_annotation(
    title = "Milwaukee Residential Development vs 1938 HOLC Grades",
    subtitle = "Left: HOLC redlining grades | Right: Homes built by grade and era",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 11)
    )
  )

combined
