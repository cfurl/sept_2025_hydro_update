# Cumulative Rainfall Hyetograph (inches) — Basin + Subbasins
# Inputs: CSVs in ".//output" with columns:
#   month, day, year, avg_cum_in_rainfall
# Naming: everything before ".shp" is basin name;
#   "usgs_dissolved.shp" => "Edwards Recharge Zone"
# Small plots: x-axis labels on ALL; y-axis labels only on FIRST column.

library(tidyverse)
library(lubridate)
library(janitor)
library(patchwork)

# ---------- 1) Read all CSVs ----------
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
    mutate(
      region = nm,
      date   = make_date(year, month, day)
    ) |>
    select(region, date, avg_cum_in_rainfall)
}

rain <- map_dfr(csvs, load_one) |> arrange(region, date)

# ---------- 2) Main vs subbasins ----------
main_name <- "Edwards Recharge Zone"
stopifnot(main_name %in% rain$region)

main_dat <- filter(rain, region == main_name)
subs     <- filter(rain, region != main_name)

sub_order <- c("Bexar","Blanco","Cibolo-Dry Comal",
               "Frio-Dry Frio","Guadalupe","Medina",
               "Nueces","Sabinal","Seco-Hondo")
subs <- subs |> mutate(region = factor(region, levels = sub_order))

# ---------- 3) Axes, scales, theme ----------
yr <- year(min(rain$date, na.rm = TRUE))
x_breaks <- as.Date(sprintf("%d-%02d-01", yr, c(1,4,7,10)))
x_labels <- format(x_breaks, "%b")

y_max   <- ceiling(max(rain$avg_cum_in_rainfall, na.rm = TRUE) / 5) * 5
y_lines <- c(5, 15, 25)
pal_line <- "#1f78b4"

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
    plot.margin       = margin(6, 10, 6, 10),
    axis.ticks.length = unit(2, "pt")
  )

plot_panel_sub <- function(dat, title = NULL, show_x = TRUE, show_y = FALSE){
  ggplot(dat, aes(date, avg_cum_in_rainfall)) +
    geom_hline(yintercept = y_lines, colour = "grey85", linewidth = 0.4) +
    geom_line(linewidth = 1.0, color = pal_line, lineend = "round") +
    scale_x_date(breaks = x_breaks, labels = x_labels,
                 expand = expansion(mult = c(0.005, 0.01))) +
    scale_y_continuous(limits = c(0, y_max),
                       breaks = y_lines, labels = if (show_y) waiver() else NULL,
                       minor_breaks = NULL) +
    base_theme +
    labs(title = title) +
    theme(
      plot.title = element_text(face = "bold", size = 12, margin = margin(b = 2)),
      axis.text.x = if (show_x) element_text(size = 9) else element_blank(),
      axis.text.y = if (show_y) element_text(size = 8) else element_blank()
    )
}

plot_panel_main <- function(dat, title = NULL){
  ggplot(dat, aes(date, avg_cum_in_rainfall)) +
    geom_hline(yintercept = y_lines, colour = "grey85", linewidth = 0.4) +
    geom_line(linewidth = 1.2, color = pal_line, lineend = "round") +
    scale_x_date(breaks = x_breaks, labels = x_labels,
                 expand = expansion(mult = c(0.005, 0.01))) +
    scale_y_continuous(limits = c(0, y_max),
                       breaks = scales::pretty_breaks(n = 6),
                       minor_breaks = NULL) +
    base_theme +
    labs(title = title)
}

# ---------- 4) Build panels ----------
p_main <- plot_panel_main(main_dat, title = main_name)

p_subs <- lapply(seq_along(sub_order), function(i){
  reg <- sub_order[i]
  dat <- filter(subs, region == reg)
  if (nrow(dat) == 0) ggplot() + theme_void()
  else plot_panel_sub(dat, title = reg, show_x = TRUE, show_y = (i %% 3 == 1))
})

grid_right <- wrap_plots(plotlist = p_subs, ncol = 3)

# ---------- 5) Compose final ----------
final_plot <-
  (p_main | grid_right) +
  plot_layout(widths = c(2.0, 1.0)) +
  plot_annotation(
    title = "Cumulative Precipitation 1/1/2025 - 9/4/2025",
    subtitle = "Basin averaged daily cumulative rainfall (in) across Edwards Aquifer Region",
    caption = "Source: NEXRAD Stg4 Radar",
    theme = theme(
      plot.title = element_text(size = 18, face = "bold"),
      plot.subtitle = element_text(size = 12, margin = margin(t = 4, b = 6)),
      plot.caption = element_text(size = 9, color = "#444444", margin = margin(t = 8)),
      plot.background = element_rect(fill = "white", colour = NA)
    )
  )

# ---------- 6) Save & show ----------
ggsave("Main panel with subs.png",
       final_plot, width = 12.5, height = 8, dpi = 300, bg = "white")
print(final_plot)
