library(readr)
library(ggplot2)
library(dplyr)
library(DT)
library(tools)
library(readxl)
library(reshape2)
library(tidyr)
library(plyr)

PractiseData <-
  data.frame(
    Model = c("Voldemort's Model", "Sauron's Model", "Dumbledore's Model", "Gandalf's Model"),
    Accuracy = c(0.589415, 0.590705, 0.590867, 0.592849) 
  )
blue.bold.italic.16.text <- element_text(face = "bold.italic", color = "blue", size = 16)

ggplot(data = PractiseData, aes(x = Model, y = Accuracy, fill = Model)) + geom_bar(stat ="identity") +
  coord_cartesian(ylim = c(0.585, 0.595)) + theme(axis.text.y = blue.bold.italic.16.text, axis.title.y = blue.bold.italic.16.text) +
  scale_fill_manual(values = c("#FF8888", "#FF0000", "Gray", "Gray"))


