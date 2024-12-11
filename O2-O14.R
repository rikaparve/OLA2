library(tidyverse)
library(ggplot2)

###### Calculate the average m2-price per city type

kvm_bytype <- data.frame(m2_price=merged_data$kvmpris, City_size=merged_data$Størrelse)

# Calculate the average m2-price per city size
average_price_by_size <- kvm_bytype %>%
  group_by(City_size) %>%
  summarise(avg_m2_price = mean(m2_price))

#Make sure the order is correct
unique(average_price_by_size$City_size)

average_price_by_size$City_size <- factor(
  average_price_by_size$City_size,
  levels = c("landsby", "lille by", "almindelig by", "større by", "storby")
)

# Plot
ggplot(average_price_by_size, aes(x = City_size, y = avg_m2_price)) +
  geom_bar(stat = "identity", fill= "seagreen") +
  labs(
    title = "Gennemsnit kvm-pris stiger med størrelsen af byen",
    x = "Størrelsen af byen",
    y = "Gennemsnitlig kvm-pris"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 14),  # Increase x-axis tick value size
    axis.text.y = element_text(size = 14)   # Increase y-axis tick value size
  )



