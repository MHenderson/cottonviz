# https://rss.org.uk/news-publication/news-publications/2021/section-group-reports/mary-eleanor-spear-dataviz-competition-for-childre/
# https://stackoverflow.com/questions/5208679/order-bars-in-ggplot2-bar-graph
# https://stackoverflow.com/questions/13445753/force-ggplot2-scatter-plot-to-be-square-shaped#13446470
# https://stackoverflow.com/questions/26191833/add-panel-border-to-ggplot2
# https://stackoverflow.com/questions/26367296/how-do-i-make-my-axis-ticks-face-inwards-in-ggplot2
library(dplyr)
library(ggplot2)
library(here)
library(janitor)
library(patchwork)
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

(line_plot <- tidy_cottonviz %>%
  filter(name != "total_supply") %>%
  ggplot() +
    geom_line(aes(year, value, group = name, linetype = name), size = 1.5) +
    scale_y_continuous(limits = c(0, 12000),
                       breaks = c(0, 2000, 4000, 6000, 8000, 10000, 12000),
                       labels = c(0, 2, 4, 6, 7, 10, 12)) +
    scale_x_continuous(breaks = 1942:1948,
                       labels = c("1942", "'43", "'44", "'45", "'46", "'47", "'48")) +
    geom_curve(data = data.frame(x = 1944.00916767693, y = 3091.0796538305,  xend = 1944.40110836753, yend = 2895.10930853056),
               mapping = aes(x = x, y = y, xend = xend, yend = yend),
               curvature = 0.03,
               arrow = arrow(30L, unit(0.1, "inches"), "last", "closed"),
               inherit.aes = FALSE
    ) +
    geom_text(data = data.frame(x = 1943.55843588274, y = 3091.0796538305, label = "Exports"),
              mapping = aes(x = x, y = y, label = label),
              inherit.aes = FALSE
    ) +
    geom_curve(data = data.frame(x = 1945.18498974873, y = 10498.7587061682, xend = 1944.94982533437, yend = 9754.07139402847),
               mapping = aes(x = x,  y = y, xend = xend, yend = yend),
               arrow = arrow(30L, unit(0.1,  "inches"), "last", "closed"),
               inherit.aes = FALSE
    ) +
    geom_text(data = data.frame(x = 1946.10605037164, y = 10733.9231205282, label = "U.S. Consumption"),
              mapping = aes(x = x, y = y, label = label),
              inherit.aes = FALSE
    ) +
    geom_curve(data = data.frame(x = 1945.69451264651, y = 7010.48655982931,  xend = 1945.34176602497, yend = 6618.54586922943),
               mapping = aes(x = x,  y = y, xend = xend, yend = yend),
               curvature = 0.095,
               arrow = arrow(30L,  unit(0.1, "inches"), "last", "closed"),
               inherit.aes = FALSE
    ) +
    geom_text(data = data.frame(x = 1946.63517030395, y = 7049.6806288893,  label = "Carry-over stocks"),
              mapping = aes(x = x, y = y, label = label),
              inherit.aes = FALSE
    ) +
    labs(
      subtitle = "Millions of Boles"
    ) +
    theme(
      aspect.ratio = 1,
      panel.background = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA, size = 1),
      axis.ticks.length = unit(-0.25, "cm"),
      axis.text.x = element_text(margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
      axis.text.y = element_text(margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.position = "none"
    )
  )

(bar_plot <- tidy_cottonviz %>%
  filter(name != "total_supply") %>%
  ggplot() +
    geom_col(aes(year, value, fill = name)) +
    geom_label(data = data.frame(x = 1945, y = 15011.659038515, label = "STOCKS*"),
              mapping = aes(x = x, y = y, label = label),
              size = 4.59, inherit.aes = FALSE) +
    geom_label(data = data.frame(x = 1945, y = 11315.1425716157, label = "EXPORTS"),
              mapping = aes(x = x, y = y, label = label),
              size = 4.59, inherit.aes = FALSE) +
    geom_label(data = data.frame(x = 1945, y = 6832.13323941873, label = "U.S. CONSUMPTION"),
              mapping = aes(x = x, y = y, label = label),
              size = 4.59, inherit.aes = FALSE) +
    scale_y_continuous(limits = c(0, 25000),
                       breaks = c(0, 5000, 10000, 15000, 20000, 25000),
                       labels = c(0, 5, 10, 15, 20, 25)) +
    scale_x_continuous(breaks = 1942:1948,
                       labels = c("1942", "'43", "'44", "'45", "'46", "'47", "'48")) +
    labs(
      subtitle = "Millions of Boles"
    ) +
    theme(
      panel.background = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA, size = 1),
      axis.ticks.length = unit(-0.25, "cm"),
      axis.text.x = element_text(margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
      axis.text.y = element_text(margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.position = "none"
    )
  )

(plot_ <- line_plot + bar_plot +
    plot_annotation(
      title = 'Distribution of United States Cotton',
      caption = "U.S. Supply of U.S. Cotton")
)

agg_png(here("plot.png"), res = 300, height = 5, width = 10, units = "in")
print(plot_)
dev.off()
