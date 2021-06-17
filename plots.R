# https://rss.org.uk/news-publication/news-publications/2021/section-group-reports/mary-eleanor-spear-dataviz-competition-for-childre/
# https://stackoverflow.com/questions/5208679/order-bars-in-ggplot2-bar-graph
library(dplyr)
library(ggplot2)
library(here)
library(janitor)
library(ragg)
library(readxl)
library(tidyr)

cottonviz <- read_xlsx(here("data-raw", "CottonViz-data.xlsx")) %>%
  clean_names()

tidy_cottonviz <- cottonviz %>%
  pivot_longer(-year) %>%
  mutate(
    name = factor(name, levels = c("stocks", "exports", "us_consumption"))
  )

line_plot <- tidy_cottonviz %>%
  filter(name != "total_supply") %>%
  ggplot() +
    geom_line(aes(year, value, group = name, linetype = name))

agg_png(here("line-plot.png"), res = 300, height = 8, width = 7.43, units = "in")
print(line_plot)
dev.off()

bar_plot <- tidy_cottonviz %>%
  filter(name != "total_supply") %>%
  ggplot() +
    geom_col(aes(year, value, fill = name))

agg_png(here("bar-plot.png"), res = 300, height = 8, width = 7.43, units = "in")
print(line_plot)
dev.off()
