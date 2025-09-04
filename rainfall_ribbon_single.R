# ================== Single-Basin Cumulative Rainfall Plot ==================
# INPUT: set the basin you want to plot (match the name before ".shp")
BASIN_CHOICE <- "Medina"     # e.g., "Bexar", "Guadalupe", "Medina", etc.

# --------------------------------------------------------------------------
# Same headers, color scheme, and look as the multi-panel figure
# Files live in ".//output" and each CSV has columns:
#   month, day, year, avg_cum_in_rainfall  (others ignored)
# "usgs_dissolved.shp" is the whole basin (Edwards Recharge Zone).
# --------------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(janitor)

# ---------- Load data ----------
csvs <- list.files(".//output", pattern = "\\.csv$", full.names = TRUE)

parse_basin_name <- function(fp){
  fn  <- basename(fp)
  raw <- sub("\\.shp.*$", "", fn, ignore.case = TRUE)
  nm  <- if (tolower(raw) == "usgs_dissolved") "Edwards Recharge Zone" else raw
  nm  <- gsub("_", " ", nm)
  if (tolower(nm) == "medinas") nm <- "Medina"
  nm
}

load_one <- function(fp){
  nm <- parse_basin_name(fp)
  readr::read_csv(fp, show_col_types = FALSE) |>
    clean_names() |>
    mutate(region = nm,
           date   = make_date(year, month, day)) |>
    select(region, date, avg_cum_in_rainfall)
}

rain_all <- purrr::map_dfr(csvs, load_one)

# ---------- Filter to requested basin ----------
stopifnot(BASIN_CHOICE %in% unique(rain_all$region))
rain <- rain_all |> filter(region == BASIN_CHOICE) |> arrange(date)

# ---------- Axes & theme (same scheme) ----------
yr <- year(min(rain$date, na.rm = TRUE))
x_breaks <- as.Date(sprintf("%d-%02d-01", yr, c(1,4,7,10)))
x_labels <- format(x_breaks, "%b")

y_max   <- ceiling(max(rain$avg_cum_in_rainfall, na.rm = TRUE) / 5) * 5
y_lines <- c(5, 15, 25)
pal_line <- "#1f78b4"   # blue, same as before

base_theme <- theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor  = element_blank(),
    panel.grid.major.x= element_blank(),
    legend.position   = "none",
    axis.title        = element_blank(),
    plot.title.position = "plot",
    text              = element_text(colour = "black"),
    axis.text         = element_text(colour = "black"),
    panel.background  = element_rect(fill = "white", colour = NA),
    plot.background   = element_rect(fill = "white",  colour = NA),
    plot.margin       = margin(6, 12, 6, 12),
    axis.ticks.length = unit(2, "pt")
  )

# ---------- Plot ----------
p <- ggplot(rain, aes(date, avg_cum_in_rainfall)) +
  geom_hline(yintercept = y_lines, colour = "grey85", linewidth = 0.4) +
  geom_line(linewidth = 1.2, color = pal_line, lineend = "round") +
  scale_x_date(breaks = x_breaks, labels = x_labels,
               expand = expansion(mult = c(0.005, 0.01))) +
  scale_y_continuous(limits = c(0, y_max),
                     breaks = scales::pretty_breaks(n = 6),
                     minor_breaks = NULL) +
  base_theme +
  labs(
    title = "Cumulative Precipitation Year-to-Date",
    subtitle = "Average cumulative rainfall (inches) for basin and subbasins",
    caption = "Source: NEXRAD Stg4 Radar"
  ) +
  # Basin name as panel title (bold, like the main panel previously)
  annotate("text", x = min(rain$date, na.rm = TRUE), y = y_max,
           label = BASIN_CHOICE, hjust = 0, vjust = -0.8,
           fontface = "bold", size = 4)

# ---------- Save & show ----------
ggsave(sprintf("cumulative_ytd_%s_inches.png", gsub("\\s+", "_", BASIN_CHOICE)),
       p, width = 12.5, height = 8, dpi = 300, bg = "white")
print(p)