library(gapminder)
library(dplyr)
library(ggplot2)
library(ggrepel)

# Foreground & Background Example -----------------------------------------

set.seed(1989) # for ggrepel to produce identical results
bg_col <- "#8C8C8C"
gapminder %>%
  filter(year == 2007) %>%
  mutate(e0rank = n() - rank(lifeExp)) %>%
  filter(e0rank <= 30) %>%
  ggplot(aes(x = gdpPercap, y = lifeExp)) +
  geom_smooth(method = "lm", size = 2) +
  geom_text_repel(aes(label = country, color = continent),
                  #color = bg_col, segment.color = bg_col,
                  size = 3, point.padding = unit(0.2, "lines")) +
  geom_point(aes(fill = continent),
             size = 4, shape = 21) +
  scale_x_continuous(name = "Income per person in $",
                     breaks = seq(10000, 50000, 10000)) +
  scale_y_continuous(name = "Life expectancy in years",
                     breaks = seq(25, 85, 1)) +
  scale_fill_brewer("", type = "qual", palette = 6) +
  scale_color_brewer(type = "qual", palette = 6, guide = "none") +
  theme_classic() +
  theme(
    axis.line.x = element_line(),
    axis.line.y = element_line(),
    aspect.ratio = 1
  )

ggsave("fb.pdf",
       path = "~/lucile/share/jowncloud/sci/paa2016-datavis/doc/fig/raw/",
       width = 7, height = 7)

# Categorical Example -----------------------------------------------------

load("~/lucile/share/jowncloud/sci/paa2016-datavis/data/france_dominant_cod.RData")

# transform
france_dominant_cod %>%
  group_by(year, sex) %>%
  mutate(age_start = c(0, 1, seq(5, 100, 5)),
         age_width = c(diff(age_start), NA),
         cod = factor(cod, levels = c("Circulatory diseases",
                                      "Neoplasms",
                                      "Infections",
                                      "External",
                                      "Other"))) %>%
  filter(sex != "total", age_start != 100) %>%
  # ggplot
  ggplot(aes(x = year + 0.5, y = age_start + 0.5*age_width,
             height = age_width)) +
  # heatmap
  geom_tile(aes(fill = cod)) +
  # lexis grid
  geom_abline(intercept = seq(-2000, -1800, 10),
              color = "white", lty = 2, size = 0.1) +
  geom_hline(yintercept = seq(10, 90, 10),
             color = "white", size = 0.1) +
  geom_vline(xintercept = seq(1930, 1990, 10),
             color = "white", size = 0.1) +
  # facet
  facet_grid(~sex) +
  # scale
  scale_y_continuous("Age", breaks = seq(0, 100, 10), expand = c(0,0)) +
  scale_x_continuous("Year", breaks = seq(1920, 2000, 10), expand = c(0,0)) +
  scale_fill_manual("Dominant cause of death",
                    values = c("Infections" = "#63C46A",
                               "Neoplasms" = "#E7CC60",
                               "Circulatory diseases" = "#FF4B56",
                               "External" = "#C463AF",
                               "Other" = "#969696")) +
  # misc
  coord_equal() +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.ticks.length = unit(0, "cm")
  )

ggsave("cod.pdf",
       path = "~/lucile/share/jowncloud/sci/paa2016-datavis/doc/fig/raw/",
       width = 12, height = 6)
