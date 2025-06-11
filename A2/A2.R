library(ggplot2)
library(tidyr) 

raw_data <- read.csv("data.csv")
# Remove Total 
filtered_data <- raw_data[raw_data$STATE != "TOTAL", ]

data_long <- pivot_longer(filtered_data, cols = -STATE, names_to = "YEAR", values_to = "VOTES")


p <- ggplot(data_long, mapping = aes(x= STATE ,y=VOTES, fill=YEAR))

p + geom_bar(stat = "identity", position = "dodge") + 
  labs(title = 'Pre-poll voting figures of Federal election \n for the same time at the 2022 and 2025') + 
  theme_bw() +
  theme(plot.title = element_text(
    face = "bold",
    size = 16,
    hjust = 0.5
  ))
    

