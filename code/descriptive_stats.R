
library(dplyr)
library(ggplot2)
library(e1071)
library(patchwork)
library(reshape2)

INPUT_PATH <- "C:/Users/ASUS/Downloads/All_GPUs_cleaned.csv"
df <- read.csv(INPUT_PATH, check.names = FALSE, stringsAsFactors = FALSE)


# Biểu đồ hộp: Số lượng GPU theo nhà sản xuất
hinh_1 <- ggplot(df, aes(x = Manufacturer, fill = Manufacturer)) +
  geom_bar(width = 0.7) +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.4, fontface = "bold") +
  theme_minimal() +
  labs(x = "Nhà sản xuất", y = "Số bản ghi")
print(hinh_1)


# Bảng: Thống kê mô tả của 5 biến định lượng

num_cols <- c("Year", "Memory (MB)", "Memory_Bandwidth (GB/sec)", "Memory_Bus (Bit)", "Memory_Speed (MHz)")
num_data <- df[, intersect(num_cols, names(df))]

calc_stats_full <- function(x) {
  mean_val <- mean(x, na.rm = TRUE)
  sd_val <- sd(x, na.rm = TRUE)
  cv_val <- (sd_val / mean_val) * 100
  
  c(
    n = sum(!is.na(x)),
    Min = min(x, na.rm = TRUE),
    Max = max(x, na.rm = TRUE),
    Mean = mean_val,
    Dolechchuan = sd_val,
    CV_Percent = cv_val,
    Phuongsai = var(x, na.rm = TRUE),
    Median = median(x, na.rm = TRUE),
    Skewness = e1071::skewness(x, na.rm = TRUE, type = 2)
  )
}

bang_2 <- round(as.data.frame(sapply(num_data, calc_stats_full)), 3)
options(width = 150)
print(bang_2)
write.csv(bang_2, "Bang_2_Thong_ke_5_bien.csv", row.names = TRUE)


# Histogram: Phân phối, giá trị mean median 5 biến định lượng
hist_plots <- lapply(names(num_data), function(col) {
  vals <- df[[col]]
  
  mean_v   <- mean(vals, na.rm = TRUE)
  median_v <- median(vals, na.rm = TRUE)
  skew_v   <- e1071::skewness(vals, na.rm = TRUE, type = 2)
  
  fill_color <- if (col == "Year") "#386888" else "#48CAE4"
  
  # Tach subtitle thanh 2 dong 
  sub_text <- sprintf(
    "Skewness = %.3f\nMean = %s  |  Median = %s",
    skew_v,
    format(round(mean_v, 2), big.mark = ","),
    format(round(median_v, 2), big.mark = ",")
  )
  
  p <- ggplot(df, aes(x = .data[[col]])) +
    geom_histogram(bins = 30, fill = fill_color, colour = "white", alpha = 0.9) +
    geom_vline(xintercept = mean_v, color = "#d62828", 
               linetype = "dashed", linewidth = 0.8) +
    geom_vline(xintercept = median_v, color = "#2a9d8f", 
               linetype = "dotdash", linewidth = 0.8) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title    = element_text(face = "bold", size = 11, hjust = 0.5),
      plot.subtitle = element_text(size = 8.5, color = "gray30", hjust = 0.5,
                                   lineheight = 1.4),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(size = 8, angle = 45, hjust = 1)
    ) +
    labs(
      title = paste0("Phân phối: ", col),
      subtitle = sub_text,
      x = col,
      y = "Tần số"
    )
  
  if (col == "Year") {
    p <- p + scale_x_continuous(
      breaks = seq(1998, 2018, by = 3),
      labels = function(x) as.integer(x) 
    )
  } else if (col == "Memory (MB)") {
    p <- p + scale_x_continuous(
      breaks = seq(0, 33000, by = 6000),    
      labels = scales::comma
    )
  } else if (col == "Memory_Bandwidth (GB/sec)") {
    p <- p + scale_x_continuous(
      breaks = seq(0, 1300, by = 200),
      labels = scales::comma
    )
  } else if (col == "Memory_Bus (Bit)") {
    p <- p + scale_x_continuous(
      breaks = seq(0, 8000, by = 2000),     
      labels = scales::comma
    )
  } else if (col == "Memory_Speed (MHz)") {
    p <- p + scale_x_continuous(
      breaks = seq(0, 2200, by = 400),
      labels = scales::comma
    )
  }
  
  return(p)
})

hinh_2 <- wrap_plots(hist_plots, ncol = 3) +
  plot_annotation(
    title = "Phân phối của toàn bộ biến định lượng",
    caption = "─ ─  Trung bình (Mean)    ─ · ─  Trung vị (Median)",
    theme = theme(
      plot.title   = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.caption = element_text(size = 9, hjust = 0.5, color = "gray40")
    )
  )

print(hinh_2)

# Tính toán các khoảng phân vị để tìm ngoại lai
bandwidth <- df[["Memory_Bandwidth (GB/sec)"]]

q1 <- quantile(bandwidth, 0.25, na.rm = TRUE)
q3 <- quantile(bandwidth, 0.75, na.rm = TRUE)
iqr <- q3 - q1
lower <- q1 - 1.5 * iqr
upper <- q3 + 1.5 * iqr

outlier_mask <- bandwidth < lower | bandwidth > upper
cat("--- KẾT QUẢ QUY TẮC TUKEY ---\n")
cat("Q1, Q3, IQR:", round(q1, 2), round(q3, 2), round(iqr, 2), "\n")
cat("Cận Tukey (Lower, Upper):", round(lower, 2), round(upper, 2), "\n")
cat("Số mẫu ngoại lệ (Outliers):", sum(outlier_mask, na.rm = TRUE), "\n")
cat("Tỷ lệ ngoại lệ (%):", round(mean(outlier_mask, na.rm = TRUE) * 100, 2), "%\n\n")


# Bảng: Thống kê Bandwidth theo nhà sản xuất 
bang_3 <- df %>%
  group_by(Manufacturer) %>%
  summarise(
    n = n(),
    Mean = round(mean(`Memory_Bandwidth (GB/sec)`, na.rm = TRUE), 2),
    Median = round(median(`Memory_Bandwidth (GB/sec)`, na.rm = TRUE), 2),
    Std = round(sd(`Memory_Bandwidth (GB/sec)`, na.rm = TRUE), 2),
    Q1 = round(quantile(`Memory_Bandwidth (GB/sec)`, 0.25, na.rm = TRUE), 2),
    Q3 = round(quantile(`Memory_Bandwidth (GB/sec)`, 0.75, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(n))

print(bang_3)
write.csv(bang_3, "Bang_3_Bandwidth_theo_Hang.csv", row.names = FALSE)


# Line: Băng thông bộ nhớ trung bình theo năm của từng nhà sản xuất
annual_bw_mfr <- df %>%
  group_by(Year, Manufacturer) %>%
  summarise(
    Mean_BW = mean(`Memory_Bandwidth (GB/sec)`, na.rm = TRUE),
    .groups = "drop"
  )

hinh_5 <- ggplot(annual_bw_mfr, aes(x = Year, y = Mean_BW, color = Manufacturer)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(
    breaks = seq(1998, 2017, by = 2),
    labels = function(x) sprintf("%d", x)
  ) +
  theme_minimal() +
  labs(
    title = "Băng thông bộ nhớ trung bình theo Năm và Nhà sản xuất",
    x = "Năm phát hành",
    y = "Băng thông trung bình (GB/sec)",
    color = "Nhà sản xuất"
  )

print(hinh_5)


# Scatter
predictors <- c("Memory (MB)", "Memory_Speed (MHz)", "Year", "Memory_Bus (Bit)")

scatter_plots <- lapply(predictors, function(pred) {
  r_val <- cor(df[[pred]], df[["Memory_Bandwidth (GB/sec)"]], use = "complete.obs")
  
  ggplot(df, aes(x = .data[[pred]], y = `Memory_Bandwidth (GB/sec)`, color = Manufacturer)) +
    geom_point(alpha = 0.5, size = 1.3) +
    
    # Đường hồi quy tuyến tính đại diện cho Pearson r
    geom_smooth(method = "lm", color = "black", linetype = "dashed", se = FALSE, linewidth = 0.7) +
    
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(labels = function(x) if(pred == "Year") sprintf("%d", x) else scales::comma(x)) +
    
    theme_minimal(base_size = 10) +
    theme(
      plot.title = element_text(face = "bold", size = 10.5),
      plot.subtitle = element_text(size = 9, color = "gray20")
    ) +
    labs(
      title = paste0("Memory Bandwidth vs ", pred),
      subtitle = sprintf("Pearson r = %.3f", r_val),
      x = pred,
      y = "Memory Bandwidth (GB/sec)",
      color = "Nhà sản xuất:"
    )
})

# Gộp các biểu đồ
hinh_scatter <- wrap_plots(scatter_plots, ncol = 2) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = 'bottom', legend.title = element_text(face = "bold"))

print(hinh_scatter)
ggsave("Pics/TKMoTa/08_scatter_bandwidth_predictors.png", plot = hinh_scatter, width = 10, height = 8.5, dpi = 300)


# Top 15 kiến trúc GPU
top15_arch <- df %>%
  count(Architecture, name = "Count") %>%
  top_n(15, Count) %>%
  arrange(Count)

hinh_top15_arch <- ggplot(top15_arch, aes(x = reorder(Architecture, Count), y = Count)) +
  geom_col(fill = "#5B4B8A", width = 0.75) +
  geom_text(aes(label = Count), hjust = -0.2, size = 3.5) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Top 15 kiến trúc GPU có nhiều bản ghi nhất",
    x = "Kiến trúc (Architecture)",
    y = "Số lượng bản ghi"
  ) +
  expand_limits(y = max(top15_arch$Count) * 1.1)

print(hinh_top15_arch)

