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
