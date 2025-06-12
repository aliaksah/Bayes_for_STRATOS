library(ggplot2)

# Function for creating violin plots
plot_violin_residuals <- function(qres, data, title) {
  
  # create data frame
  res_data <- data.frame(
    Residuals = residuals(qres),
    Fitted = factor(qres$fittedPredictedResponse), # Binäre Werte 0/1 als Faktor
    Bull = factor(data$bull),  # Binärer Prädiktor
    ATT4 = factor(data$ATT4)   # Kategorischer Prädiktor mit 4 Levels
  )
  
  # creating violin plots
  p1 <- ggplot(res_data, aes(x = Fitted, y = Residuals)) +
    geom_violin(fill = "lightblue", alpha = 0.6) +
    geom_jitter(width = 0.2, alpha = 0.5) +
    theme_minimal() +
    labs(title = paste(title, "- Residuals vs Fitted Values"), x = "Fitted Values (0/1)", y = "Residuals")
  
  p2 <- ggplot(res_data, aes(x = Bull, y = Residuals)) +
    geom_violin(fill = "lightgreen", alpha = 0.6) +
    geom_jitter(width = 0.2, alpha = 0.5) +
    theme_minimal() +
    labs(title = paste(title, "- Residuals vs Bull"), x = "Bull (Binary Predictor)", y = "Residuals")
  
  p3 <- ggplot(res_data, aes(x = ATT4, y = Residuals)) +
    geom_violin(fill = "lightcoral", alpha = 0.6) +
    geom_jitter(width = 0.2, alpha = 0.5) +
    theme_minimal() +
    labs(title = paste(title, "- Residuals vs ATT4"), x = "ATT4 (Categorical Predictor)", y = "Residuals")
  
  # return plots
  return(list(p1, p2, p3))
}

# Beispiel-Anwendung der Funktion
plots <- plot_violin_residuals(qres.m1, pisa2018, "Residual Diagnostics")
print(plots[[1]])  # Residuen gegen Fitted Values
print(plots[[2]])  # Residuen gegen Bull
print(plots[[3]])  # Residuen gegen ATT4
