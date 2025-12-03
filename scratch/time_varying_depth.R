

catch <- nwfscSurvey::pull_catch(sci_name = "Merluccius productus", 
                                 survey = "NWFSC.Combo",
                                 sample_types = c("NA", NA, "Life Stage", "Size")[1:4])

library(sdmTMB)
library(viridis)
catch <- add_utm_columns(catch, ll_names = c("Longitude_dd", "Latitude_dd"))
mesh <- make_mesh(catch, xy_cols = c("X","Y"), cutoff = 25)

fit <- sdmTMB(cpue_kg_km2 ~ 0,
              time_varying = ~ 1 + Depth_m + I(Depth_m^2),
              spatiotemporal = "iid",
              spatial = "on",
              time = "Year",
              share_range = TRUE,
              family = tweedie(),
              mesh = mesh,
              data = catch)

nd <- expand.grid(
  Depth_m = seq(min(catch$Depth_m),
                max(catch$Depth_m), length.out = 100),
  Year = unique(catch$Year)
)

p <- predict(fit, newdata = nd, se_fit = TRUE, re_form = NA)

p1 <- ggplot(p, aes(Depth_m, exp(est),
                    ymin = exp(est - 1 * est_se),
                    ymax = exp(est + 1 * est_se),
                    group = as.factor(Year)
)) +
  geom_line(aes(colour = Year), lwd = 1) +
  geom_ribbon(aes(fill = Year), alpha = 0.1) +
  scale_color_viridis(begin = 0.2, end = 0.8) + 
  scale_fill_viridis(begin = 0.2, end = 0.8) + 
  coord_cartesian(expand = FALSE, xlim = c(55, 1000)) +
  labs(x = "Depth (m)", y = "Biomass density (kg/km2)")
ggsave(filename="plots/depth_by_year.png")

library(ggridges)
p2 <- p %>%
  group_by(Year) %>%
  mutate(height_scaled = exp(est) / max(exp(est)))

p2$Year <- factor(p2$Year)

ggplot(
  p2,
  aes(
    x = -Depth_m,
    y = Year,
    height = height_scaled,
    group = Year,
    fill = Year
  )
) +
  geom_ridgeline(scale = 0.9, alpha = 0.6) +
  scale_fill_viridis_d() +
  theme_ridges() +
  theme(legend.position = "none") +
  labs(x = "Depth (m)") + 
  xlim(-800,0) + coord_flip()
ggsave(filename="plots/depth_by_year_ridgeplot.png")