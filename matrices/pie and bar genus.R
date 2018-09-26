library(readr)
library(ggplot2)
library(dplyr)
library(DT)
library(tools)
library(readxl)
library(reshape2)
library(tidyr)
library(plyr)

Starting_Data <- read_excel("starting Data .xlsx")
Starting_Data2 <- melt(Starting_Data, id.var = "Genus")
print(Starting_Data2)

ggplot(data = Starting_Data2, aes(
  x = Genus,
  y = (value),
  fill = Genus
)) +
  geom_bar(stat = "identity", width = 0.7) +
  theme(
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1)
  ) +
  scale_y_continuous(limits = c(0, 0.5)) + ylab("Relative Frequency") +
  scale_fill_manual(
    values = c(
      "Bacteroides" = "coral4",
      "Mucinivorans" = "chartreuse3",
      "Prevotella" = "cadetblue3",
      "Streptococcus" = "brown3",
      "Treponema" = "blueviolet",
      "Staphylococcus" = "dodgerblue4",
      "Clostridium" = "deeppink3",
      "Helicobacter" = "darkorange3",
      "Turicibacter" = "khaki3",
      "Bacillus" = "goldenrod3",
      "Not Considered" = "yellow3",
      "Not Selected" = "105"
    )
  )

ggplot(data = Starting_Data2, aes(
  x = variable,
  y = (value),
  fill = Genus
)) +
  geom_bar(stat = "identity") +
  theme(
    legend.title = element_blank(),
    axis.text.y  = element_blank(),
    axis.text.x = element_blank(),
    # axis.ticks = element_blank(),
    # axis.ticks.y  = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
    # axis.line = element_blank()
  ) +
  coord_polar("y", start = 0) +
  scale_fill_manual(
    values = c(
      "Bacteroides" = "coral4",
      "Mucinivorans" = "chartreuse3",
      "Prevotella" = "cadetblue3",
      "Streptococcus" = "brown3",
      "Treponema" = "blueviolet",
      "Staphylococcus" = "dodgerblue4",
      "Clostridium" = "deeppink3",
      "Helicobacter" = "darkorange3",
      "Turicibacter" = "khaki3",
      "Bacillus" = "goldenrod3",
      "Not Considered" = "yellow3",
      "Not Selected" = "105"
    )
  )

