# Функция для визуализации (исправленная)
visualize_sar_splines <- function(result_df, symbol = "MSFT") {
  
  # Создаем длинный формат данных для линий
  line_data <- result_df %>%
    select(Date, Close, SAR_Original, SAR_Smoothed) %>%
    melt(id.vars = "Date", 
         variable.name = "Series", 
         value.name = "Price")
  
  ggplot() +
    # Линии: Close, SAR_Original, SAR_Smoothed
    geom_line(data = line_data, 
              aes(x = Date, y = Price, color = Series, linetype = Series), 
              linewidth = 1) +
    # Точки для закрытия с цветом по позиции относительно SAR
    geom_point(data = result_df, 
               aes(x = Date, y = Close, color = Position), 
               size = 1, alpha = 0.6) +
    scale_color_manual(
      name = "Legend",
      values = c("Close" = "black", 
                 "SAR_Original" = "red", 
                 "SAR_Smoothed" = "blue",
                 "Above SAR" = "green",
                 "Below SAR" = "orange"),
      labels = c("Close" = "Close Price", 
                 "SAR_Original" = "Original SAR", 
                 "SAR_Smoothed" = "Smoothed SAR",
                 "Above SAR" = "Price Above SAR",
                 "Below SAR" = "Price Below SAR")
    ) +
    scale_linetype_manual(
      name = "Series",
      values = c("Close" = "solid", 
                 "SAR_Original" = "dotted", 
                 "SAR_Smoothed" = "solid"),
      labels = c("Close" = "Close Price", 
                 "SAR_Original" = "Original SAR", 
                 "SAR_Smoothed" = "Smoothed SAR")
    ) +
    labs(title = paste("Parabolic SAR with Spline Smoothing -", symbol),
         subtitle = "Black: Close Price | Red: Original SAR | Blue: Smoothed SAR",
         y = "Price",
         x = "Date") +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.box = "vertical")
}

# Альтернативная упрощенная версия визуализации
visualize_sar_splines_simple <- function(result_df, symbol = "MSFT") {
  
  ggplot(result_df, aes(x = Date)) +
    # Цена закрытия
    geom_line(aes(y = Close, color = "Close Price"), linewidth = 1) +
    # Оригинальный SAR
    geom_line(aes(y = SAR_Original, color = "Original SAR"), 
              linetype = "dotted", linewidth = 1) +
    # Сглаженный SAR
    geom_line(aes(y = SAR_Smoothed, color = "Smoothed SAR"), 
              linetype = "solid", linewidth = 1) +
    # Точки с цветом по позиции
    geom_point(aes(y = Close, color = Position), 
               size = 1, alpha = 0.6) +
    scale_color_manual(
      name = "",
      values = c("Close Price" = "black", 
                 "Original SAR" = "red", 
                 "Smoothed SAR" = "blue",
                 "Above SAR" = "green",
                 "Below SAR" = "orange"),
      breaks = c("Close Price", "Original SAR", "Smoothed SAR", 
                 "Above SAR", "Below SAR")
    ) +
    labs(title = paste("Parabolic SAR with Spline Smoothing -", symbol),
         subtitle = "Black: Close Price | Red: Original SAR | Blue: Smoothed SAR",
         y = "Price",
         x = "Date") +
    theme_minimal() +
    theme(legend.position = "bottom")
}

# Полный исправленный код
if (!require(quantmod)) install.packages("quantmod")
if (!require(splines)) install.packages("splines")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(reshape2)) install.packages("reshape2")
if (!require(dplyr)) install.packages("dplyr")

library(quantmod)
library(splines)
library(ggplot2)
library(reshape2)
library(dplyr)

# Реализация Parabolic SAR на R
parabolic_sar <- function(high, low, close, start = 0.02, inc = 0.02, max = 0.2) {
  n <- length(high)
  result <- rep(NA, n)
  maxMin <- rep(NA, n)
  acceleration <- rep(NA, n)
  isBelow <- rep(NA, n)
  isFirstTrendBar <- rep(FALSE, n)
  
  # Инициализация первых значений
  result[1] <- if (close[2] > close[1]) low[1] else high[1]
  isBelow[1] <- close[2] > close[1]
  maxMin[1] <- if (isBelow[1]) high[1] else low[1]
  acceleration[1] <- start
  isFirstTrendBar[1] <- TRUE
  
  for (i in 2:n) {
    if (i == 2) {
      result[i] <- result[i-1] + acceleration[i-1] * (maxMin[i-1] - result[i-1])
    } else {
      result[i] <- result[i-1] + acceleration[i-1] * (maxMin[i-1] - result[i-1])
    }
    
    # Проверка смены тренда
    if (isBelow[i-1]) {
      if (result[i] > low[i]) {
        isFirstTrendBar[i] <- TRUE
        isBelow[i] <- FALSE
        result[i] <- max(high[i], maxMin[i-1])
        maxMin[i] <- low[i]
        acceleration[i] <- start
      } else {
        isBelow[i] <- isBelow[i-1]
        maxMin[i] <- maxMin[i-1]
        acceleration[i] <- acceleration[i-1]
      }
    } else {
      if (result[i] < high[i]) {
        isFirstTrendBar[i] <- TRUE
        isBelow[i] <- TRUE
        result[i] <- min(low[i], maxMin[i-1])
        maxMin[i] <- high[i]
        acceleration[i] <- start
      } else {
        isBelow[i] <- isBelow[i-1]
        maxMin[i] <- maxMin[i-1]
        acceleration[i] <- acceleration[i-1]
      }
    }
    
    # Обновление ускорения и экстремумов
    if (!isFirstTrendBar[i] && i > 1) {
      if (isBelow[i]) {
        if (high[i] > maxMin[i-1]) {
          maxMin[i] <- high[i]
          acceleration[i] <- min(acceleration[i-1] + inc, max)
        }
      } else {
        if (low[i] < maxMin[i-1]) {
          maxMin[i] <- low[i]
          acceleration[i] <- min(acceleration[i-1] + inc, max)
        }
      }
    }
  }
  
  return(result)
}

# Функция для сглаживания сплайнами
smooth_with_splines <- function(x, y, df = 10) {
  spline_model <- lm(y ~ ns(x, df = df))
  predicted <- predict(spline_model, newdata = data.frame(x = x))
  return(predicted)
}

# Основная функция для анализа
analyze_with_sar_splines <- function(symbol = "MSFT", start_date = "2025-01-01", 
                                     sar_start = 0.02, sar_inc = 0.02, sar_max = 0.2,
                                     spline_df = 15) {
  
  getSymbols(symbol, from = start_date, to = Sys.Date(), src = "yahoo")
  data <- get(symbol)
  
  high <- as.numeric(Hi(data))
  low <- as.numeric(Lo(data))
  close <- as.numeric(Cl(data))
  dates <- index(data)
  
  sar_values <- parabolic_sar(high, low, close, sar_start, sar_inc, sar_max)
  time_index <- 1:length(sar_values)
  sar_smoothed <- smooth_with_splines(time_index, sar_values, spline_df)
  
  result_df <- data.frame(
    Date = dates,
    High = high,
    Low = low,
    Close = close,
    SAR_Original = sar_values,
    SAR_Smoothed = sar_smoothed,
    Position = ifelse(close > sar_values, "Above SAR", "Below SAR")
  )
  
  return(result_df)
}

# Упрощенная версия визуализации
visualize_sar_splines <- function(result_df, symbol = "MSFT") {
  
  ggplot(result_df, aes(x = Date)) +
    geom_line(aes(y = Close, color = "Close Price"), linewidth = 1) +
    geom_line(aes(y = SAR_Original, color = "Original SAR"), 
              linetype = "dotted", linewidth = 1) +
    geom_line(aes(y = SAR_Smoothed, color = "Smoothed SAR"), 
              linetype = "solid", linewidth = 1) +
    geom_point(aes(y = Close, color = Position), size = 1, alpha = 0.6) +
    scale_color_manual(
      name = "",
      values = c("Close Price" = "black", 
                 "Original SAR" = "red", 
                 "Smoothed SAR" = "pink",
                 "Above SAR" = "green",
                 "Below SAR" = "orange")
    ) +
    labs(title = paste("Parabolic SAR with Spline Smoothing -", symbol),
         y = "Price",
         x = "Date") +
    theme_minimal() +
    theme(legend.position = "bottom")
}

# Запуск анализа
result_df <- analyze_with_sar_splines("MSFT", "2025-01-01")
visualize_sar_splines(result_df)

# Показать первые несколько строк результатов
head(result_df)
