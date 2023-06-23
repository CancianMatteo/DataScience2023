library(ggplot2)

# Creazione dei dati di esempio
dati <- data.frame(x = 1:10, y = 1:10, valore = c(10, 8, 6, 4, 2, 2, 4, 6, 8, 10))

# Plot con scala di colori che sfuma dal verde al rosso
ggplot(dati, aes(x, y, fill = valore)) +
  geom_tile() +
  scale_fill_gradient(low = "green", high = "red")



# Array of values
values <- c(1, 2, 3, 4, 5, 9, 10)

# Define the color range from green to red
color_range <- colorRampPalette(c("green", "red"))

# Apply the color range to the values
colors <- color_range(length(values))[as.numeric(cut(values, length(values)))]

# View the generated array of colors
colors




# Create a sample dataset
data <- data.frame(
  Category = c("A", "B", "C", "D"),
  Value = c(20, 30, 10, 40)
)

# Create the pie chart
pie_chart <- ggplot(data, aes(x = "", y = Value, fill = Category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  scale_fill_manual(values = c("#F8766D", "#A3A500", "#00BF7D", "#00B0F6")) +
  labs(title = "Pie Chart")

# Display the chart
print(pie_chart)




# Create a dataset
data <- data.frame(
  category = c("Category A", "Category B", "Category C", "Category D"),
  value = c(30, 20, 15, 35)
)

# Reorder the levels of the "category" variable based on values
data$category <- factor(data$category, levels = data$category[order(-data$value)])

# Calculate the percentage values
data$percent <- data$value / sum(data$value) * 100

# Create the pie chart with percentage values
pie_chart <- ggplot(data, aes(x = "", y = value, fill = category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text(aes(label = paste(round(percent), "%")), 
            position = position_stack(vjust = 0.5))

# Display the chart
print(pie_chart)

