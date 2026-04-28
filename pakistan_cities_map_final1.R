# ============================================================
#  URBAN PAKISTAN — Population Bubble Map
#  Created by  : Mazhar Shakoor
#  Date        : 2026
#  Description : Population distribution across major cities
#  Tools       : R, ggplot2, sf, rnaturalearth, ggrepel
#  Data        : Natural Earth, Estimated City Populations 2024
# ============================================================


# ── STEP 1: Fix timeout (run this first, every session) ──────
options(timeout = 300)


# ── STEP 2: Install packages (run once only) ─────────────────
# install.packages("ggplot2")
# install.packages("sf")
# install.packages("rnaturalearth")
# install.packages("rnaturalearthdata")
# install.packages("ggrepel")


# ── STEP 3: Load libraries ───────────────────────────────────
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)


# ── STEP 4: Load Pakistan boundary ───────────────────────────
pakistan <- ne_countries(
  country     = "Pakistan",
  scale       = "medium",
  returnclass = "sf"
)
pakistan <- st_make_valid(pakistan)


# ── STEP 5: City data ────────────────────────────────────────
cities <- data.frame(
  city       = c("Karachi", "Lahore", "Faisalabad", "Rawalpindi", "Gujranwala",
                 "Peshawar", "Multan", "Hyderabad", "Islamabad", "Quetta",
                 "Bahawalpur", "Sargodha", "Sialkot", "Sukkur", "Larkana",
                 "Sheikhupura", "Rahim Yar Khan", "Jhang", "Dera Ghazi Khan", "Gujrat"),
  population = c(16000000, 13000000, 3600000, 2300000, 2300000,
                 2000000,  1900000,  1800000, 1100000, 1000000,
                 800000,   700000,   650000,  600000,  550000,
                 500000,   450000,   400000,  380000,  350000),
  lon        = c(67.01, 74.35, 73.09, 73.06, 74.19,
                 71.57, 71.48, 68.37, 73.06, 67.01,
                 71.68, 72.67, 74.54, 68.87, 68.21,
                 73.98, 70.30, 72.32, 70.64, 74.08),
  lat        = c(24.86, 31.55, 31.42, 33.60, 32.16,
                 34.01, 30.20, 25.37, 33.72, 30.19,
                 29.40, 32.08, 32.49, 27.70, 27.56,
                 31.71, 28.42, 31.27, 30.05, 32.57)
)


# ── STEP 6: Add labels and city categories ───────────────────
cities$pop_millions <- round(cities$population / 1000000, 1)
cities$pop_label    <- paste0(cities$city, "\n", cities$pop_millions, "M")
cities$color_group  <- ifelse(cities$population >= 10000000, "Mega  (10M+)",
                       ifelse(cities$population >=  2000000, "Large  (2M - 10M)",
                                                             "Medium  (< 2M)"))

cities_sf             <- st_as_sf(cities, coords = c("lon", "lat"), crs = 4326)
cities_sf$color_group <- cities$color_group


# ── STEP 7: Colour palette ───────────────────────────────────
col_bg        <- "#eef4fb"
col_land      <- "#cfe0f0"
col_border    <- "#1a6496"
col_mega      <- "#c0392b"
col_large     <- "#e67e22"
col_medium    <- "#27ae60"
col_label     <- "#1a1a2e"
col_title     <- "#1a3a5c"
col_subtitle  <- "#2c7bb6"
col_author    <- "#c0392b"
col_caption   <- "#5d6d7e"
col_segment   <- "#1a6496"
col_legend_bg <- "#ffffff"


# ── STEP 8: Build the map ────────────────────────────────────
p <- ggplot() +

  # Pakistan land and border
  geom_sf(data      = pakistan,
          fill      = col_land,
          colour    = col_border,
          linewidth = 1.1) +

  # Outer glow layer 1
  geom_sf(data  = cities_sf,
          aes(size = population * 2.0, colour = color_group),
          alpha = 0.08, shape = 16) +

  # Outer glow layer 2
  geom_sf(data  = cities_sf,
          aes(size = population * 1.2, colour = color_group),
          alpha = 0.15, shape = 16) +

  # Main bubble
  geom_sf(data   = cities_sf,
          aes(size   = population * 0.5,
              colour = color_group,
              fill   = color_group),
          alpha  = 0.88,
          shape  = 21,
          stroke = 1.3) +

  # Colour scales
  scale_colour_manual(
    values = c("Mega  (10M+)"      = col_mega,
               "Large  (2M - 10M)" = col_large,
               "Medium  (< 2M)"    = col_medium),
    name   = "City Category"
  ) +

  scale_fill_manual(
    values = c("Mega  (10M+)"      = col_mega,
               "Large  (2M - 10M)" = col_large,
               "Medium  (< 2M)"    = col_medium),
    name   = "City Category"
  ) +

  scale_size_continuous(range = c(3, 26), guide = "none") +

  # City name labels with population
  geom_text_repel(
    data          = cities,
    aes(x = lon, y = lat, label = pop_label),
    colour        = col_label,
    size          = 2.5,
    fontface      = "bold",
    box.padding   = 0.55,
    point.padding = 0.45,
    segment.color = col_segment,
    segment.size  = 0.35,
    segment.alpha = 0.6,
    max.overlaps  = 25,
    lineheight    = 0.85,
    bg.color      = col_bg,
    bg.r          = 0.12
  ) +

  # Map extent — proper margins around Pakistan
  coord_sf(
    xlim   = c(58, 80),
    ylim   = c(20, 39),
    expand = FALSE
  ) +

  # Author name — prominently placed
  annotate("text",
           x        = 59,
           y        = 21.0,
           label    = "Created by Mazhar Shakoor",
           colour   = col_author,
           size     = 4.2,
           fontface = "bold.italic",
           family   = "serif",
           hjust    = 0) +

  # Title and caption
  labs(
    title    = "URBAN PAKISTAN",
    subtitle = "Population Distribution Across Major Cities  |  2024",
    caption  = "Data: Estimated City Populations 2024  |  Projection: WGS 84  |  Made with R & ggplot2"
  ) +

  theme_void() +

  theme(
    plot.background  = element_rect(fill = col_bg, colour = NA),
    panel.background = element_rect(fill = col_bg, colour = NA),

    plot.title = element_text(
      colour  = col_title,
      size    = 30,
      face    = "bold",
      hjust   = 0.5,
      family  = "serif",
      margin  = margin(t = 20, b = 4)
    ),

    plot.subtitle = element_text(
      colour  = col_subtitle,
      size    = 12,
      hjust   = 0.5,
      family  = "serif",
      margin  = margin(b = 10)
    ),

    plot.caption = element_text(
      colour  = col_caption,
      size    = 8,
      hjust   = 0.5,
      family  = "serif",
      margin  = margin(t = 6, b = 6)
    ),

    # Legend placed bottom-right outside Pakistan boundary
    legend.position   = c(0.88, 0.18),
    legend.background = element_rect(fill      = col_legend_bg,
                                     colour    = col_border,
                                     linewidth = 0.5),
    legend.title = element_text(colour = col_title,
                                size   = 9,
                                face   = "bold",
                                family = "serif",
                                margin = margin(b = 6)),
    legend.text  = element_text(colour = col_label,
                                size   = 8,
                                family = "serif"),
    legend.key.size = unit(0.7, "cm"),
    legend.margin   = margin(8, 12, 8, 12),
    legend.box.just = "right",

    plot.margin = margin(5, 15, 5, 15)
  ) +

  guides(
    colour = guide_legend(
      override.aes = list(size   = 5,
                          alpha  = 1,
                          shape  = 21,
                          stroke = 1.2)
    ),
    fill = "none"
  )


# ── STEP 9: Preview in RStudio Plots panel ───────────────────
print(p)



)

message("✅ Done! Your map has been saved to your Desktop.")
