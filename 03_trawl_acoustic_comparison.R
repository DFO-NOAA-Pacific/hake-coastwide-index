library(readr)

url <- "https://raw.githubusercontent.com/pacific-hake/hake-assessment/master/data-tables/survey-by-country.csv"
acoustic <- read_csv(url, skip=2) # skip 2 lines, comments
acoustic <- dplyr::select(acoustic, year, total) |>
  dplyr::rename(est = total) |>
  dplyr::mutate(lwr = NA, upr = NA, survey = "acoustic")

wctbs <- readRDS("indices/doy.rds") |>
  dplyr::select(year, est, lwr, upr) |>
  dplyr::mutate(survey="trawl")

# scale acoustic to be on same mean in 2003
acoustic$est <- acoustic$est * as.numeric((wctbs[wctbs$year==2003,"est"] / acoustic[acoustic$year==2003,"est"]))

indices <- rbind(acoustic, wctbs)

ggplot(indices, aes(year, est, group=survey)) + 
  geom_ribbon(aes(ymin=lwr,ymax=upr, fill = survey), alpha=0.3) + 
  geom_line(aes(col = survey),alpha=0.6) +
  geom_point(aes(col=survey),alpha=0.6) + 
  xlab("Year") + ylab("Estimate") + 
  scale_color_viridis_d(option="magma", begin=0.3, end=0.8) + 
  scale_fill_viridis_d(option="magma", begin=0.3, end=0.8) + 
  theme_bw()

ggsave("plots/trawl_acoustic_comparison.png")

