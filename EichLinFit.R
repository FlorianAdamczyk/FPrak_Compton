# Erstelle einen Data Frame mit Daten für x und y
df <- data.frame(
    energy = c(0,511,662),
    canal = c(125.8+-0.1, 624.1+-1.6, 758.9+-1.6)
)
plot(df)

# Erstelle eine lineare Regression
fit <- lm(canal ~ energy, data = df)

# zeige die werte davon an
summary(fit)

#plotte die Daten
library(ggplot2) # ggplot2 ist ein Paket für die Erstellung von Grafiken

(graph <- ggplot(df, aes(x = energy, y = canal)) + 
    geom_smooth(method = "lm",     col = "#ff0000") +
    geom_point(size = 4, col = "#0b41b6") +
    labs(title = "Lineare Regression", x = "Energie [keV]", y = "Kanal") +
    annotate("text", x = 0.15*max(df$energy), y = 1.1*max(df$canal), label = paste("y = ", round(fit$coefficients[1], 2), " + ", round(fit$coefficients[2], 2), "x", sep = ""), col = "#000000", size = 5)
)

# Speichere den Graphen
ggsave("EichLinFit.png", plot = graph, width = 1920, height = 1200, units = "px")
