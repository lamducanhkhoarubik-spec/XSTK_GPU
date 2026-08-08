# ==============================================================================
# BTL XÁC SUẤT THỐNG KÊ - PHÂN TÍCH BĂNG THÔNG BỘ NHỚ (GPU) THEO HÃNG SẢN XUẤT

# ==============================================================================

# ------------------------------------------------------------------------------
# KHỞI TẠO THƯ VIỆN 
# ------------------------------------------------------------------------------
# Tự động cài đặt gói nếu chưa có
if (!require("nortest")) install.packages("nortest")
if (!require("rstatix")) install.packages("rstatix")
if (!require("car")) install.packages("car")

library(nortest)
library(rstatix)
library(car)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(e1071)
library(patchwork)
library(reshape2)
library(corrplot)
library(lmtest)

# ==============================================================================
# PHẦN 1: TIỀN XỬ LÝ DỮ LIỆU (data_preprocessing.R)
# ==============================================================================

# 1. Đọc dữ liệu từ dataset
df <- read.csv("C:/Users/ACER/Documents/BTL_XSTK/XSTK_GPU/dataset/All_GPUs.csv")

# Xóa khoảng trắng cho các cột dạng character
df <- df %>% mutate(across(where(is.character), str_trim))

# Thay thế các ký tự nhiễu thành NA
noise_values <- c("-", "null", "NaN", "None", "", "nan")
df_nan <- df %>%
  mutate(across(everything(), ~ ifelse(as.character(.) %in% noise_values, NA, .)))

# Thống kê số lượng và phần trăm NA
missing_summary <- data.frame(
  Column = colnames(df_nan),
  Count = colSums(is.na(df_nan)),
  Percentage = (colSums(is.na(df_nan)) / nrow(df_nan)) * 100
)

# Biểu đồ tỷ lệ NA
missing_summary$Column <- factor(missing_summary$Column, levels = missing_summary$Column)

ggplot(missing_summary, aes(x = Column, y = Percentage)) +
  geom_bar(stat = "identity", fill = "dodgerblue", color = "black") +
  geom_text(
    data = filter(missing_summary, Percentage > 0),
    aes(label = sprintf("%.1f%%", Percentage)),
    vjust = -0.8,
    fontface = "bold", 
    size = 3
  ) +
  labs(
    title = "Biểu đồ thống kê tỷ lệ % dữ liệu thiếu (NA) sau khi làm sạch",
    x = "Tên cột",
    y = "Tỷ lệ thiếu (%)"
  ) +
  scale_y_continuous(limits = c(0, max(missing_summary$Percentage + 10, 100))) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray", linetype = "dashed", size = 0.5)
  )

# 2. Xóa các cột thiếu > 15% dữ liệu
cols_to_drop <- missing_summary %>% 
  filter(Percentage > 15) %>% 
  pull(Column)

df_brief_1 <- df_nan %>% 
  select(!all_of(cols_to_drop))

# 3. DÙNG REGEX trích xuất 4 chữ số (Năm) từ Release_Date
if ("Release_Date" %in% colnames(df_brief_1)) {
  df_brief_1 <- df_brief_1 %>%
    mutate(Year = as.integer(str_extract(Release_Date, "\\d{4}"))) %>%
    select(-Release_Date)
}

# 4. Xóa đơn vị và đổi tên các cột Memory
columns_to_clean <- list(
  Memory = "MB",
  Memory_Bandwidth = "GB/sec",
  Memory_Bus = "Bit",
  Memory_Speed = "MHz",
  Process = "nm"
)

for (old_col in names(columns_to_clean)) {
  if (old_col %in% colnames(df_brief_1)) {
    unit <- columns_to_clean[[old_col]]
    
    # Xóa đơn vị & chuyển về kiểu numeric
    df_brief_1[[old_col]] <- str_replace_all(as.character(df_brief_1[[old_col]]), regex(unit, ignore_case = TRUE), "")
    df_brief_1[[old_col]] <- str_trim(df_brief_1[[old_col]])
    df_brief_1[[old_col]][df_brief_1[[old_col]] %in% c("", "nan", "NA")] <- NA
    df_brief_1[[old_col]] <- as.numeric(df_brief_1[[old_col]])
    
    # Đổi tên cột kèm đơn vị
    new_col_name <- paste0(old_col, " (", unit, ")")
    df_brief_1 <- df_brief_1 %>% rename(!!new_col_name := all_of(old_col))
  }
}

# 5. Chọn các cột dữ liệu cần thiết
selected_columns <- c(
  "Name",
  "Architecture",
  "Manufacturer",
  "Year",
  "Memory (MB)",
  "Memory_Bandwidth (GB/sec)",
  "Memory_Bus (Bit)",
  "Memory_Speed (MHz)"
)

df_imputed <- df_brief_1 %>% 
  select(all_of(selected_columns))

# ==============================================================================
# 6. KHẮC PHỤC LỖI NHIỄM DỮ LIỆU
# - Biến phụ thuộc Y (Memory_Bandwidth): Xóa các dòng bị khuyết (Listwise Deletion)
# - Biến định danh cốt lõi (Year, Architecture): Xóa nếu bị khuyết
# ==============================================================================
df_imputed <- df_brief_1 %>% 
  select(all_of(selected_columns)) %>% 
  filter(!is.na(`Memory_Bandwidth (GB/sec)`) & !is.na(Year) & !is.na(Architecture))

cat("Số dòng hợp lệ sau khi loại bỏ khuyết Y và định danh:", nrow(df_imputed), "\n")

# ==============================================================================
# 7. ĐIỀN KHUYẾT CHỈ DÀNH CHO CÁC BIẾN ĐỘC LẬP X (Memory, Bus, Speed)
# Điền khuyết phân cấp 3 lớp (Grouped Median) để bảo toàn cấu trúc dữ liệu
# ==============================================================================
predictor_x_cols <- c("Memory (MB)", "Memory_Bus (Bit)", "Memory_Speed (MHz)")

# Cấp 1: Điền khuyết X theo Nhóm Hãng + Kiến trúc (Manufacturer + Architecture)
df_imputed <- df_imputed %>%
  group_by(Manufacturer, Architecture) %>%
  mutate(across(all_of(predictor_x_cols), ~ ifelse(is.na(.), median(., na.rm = TRUE), .))) %>%
  ungroup()

# Cấp 2: Nếu nguyên Kiến trúc bị thiếu -> Điền X theo Median của Hãng (Manufacturer)
df_imputed <- df_imputed %>%
  group_by(Manufacturer) %>%
  mutate(across(all_of(predictor_x_cols), ~ ifelse(is.na(.), median(., na.rm = TRUE), .))) %>%
  ungroup()

# Cấp 3: Dự phòng  -> Điền X theo Median chung toàn bộ Dataset
for (col in predictor_x_cols) {
  if (any(is.na(df_imputed[[col]]))) {
    col_median <- median(df_imputed[[col]], na.rm = TRUE)
    df_imputed[[col]][is.na(df_imputed[[col]])] <- col_median
  }
}

# Kiểm tra chất lượng tập dữ liệu sạch hoàn toàn
cat("Tổng số NA còn lại trong dataset:", sum(is.na(df_imputed)), "\n")
cat("\nPhân bố số mẫu hợp lệ theo từng hãng (Dữ liệu không bị rò rỉ):\n")
print(table(df_imputed$Manufacturer))

# ==============================================================================
# PHẦN 2: THỐNG KÊ MÔ TẢ (descriptive_stats.R)
# ==============================================================================

INPUT_PATH <- "C:/Users/ACER/Documents/BTL_XSTK/XSTK_GPU/dataset/All_GPUs_cleaned.csv"
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



# ==============================================================================
# BTL XÁC SUẤT THỐNG KÊ - PHÂN TÍCH BĂNG THÔNG BỘ NHỚ (GPU) THEO HÃNG SẢN XUẤT
# ==============================================================================

# ------------------------------------------------------------------------------
# BƯỚC 1: KHỞI TẠO THƯ VIỆN VÀ ĐỌC DỮ LIỆU
# ------------------------------------------------------------------------------
# Đọc file dữ liệu
df <- read.csv("C:/Users/ACER/Documents/BTL_XSTK/XSTK_GPU/dataset/All_GPUs_cleaned.csv")

# Ép kiểu dữ liệu danh mục cho biến hãng sản xuất
df$Manufacturer <- as.factor(df$Manufacturer)

# Kiểm tra tổng quan số dữ liệu khuyết (NA) ở biến Băng thông
cat("Số lượng dữ liệu khuyết ở biến Băng thông:", sum(is.na(df$Memory_Bandwidth..GB.sec.)), "\n\n")


# ------------------------------------------------------------------------------
# BƯỚC 2: KIỂM ĐỊNH PHÂN PHỐI CHUẨN (NORMALITY TEST)
# ------------------------------------------------------------------------------
# Tách dữ liệu cho từng nhóm
nvidia_data <- na.omit(df$Memory_Bandwidth..GB.sec.[df$Manufacturer == "Nvidia"])
amd_data    <- na.omit(df$Memory_Bandwidth..GB.sec.[df$Manufacturer == "AMD"])
intel_data  <- na.omit(df$Memory_Bandwidth..GB.sec.[df$Manufacturer == "Intel"])
ati_data    <- na.omit(df$Memory_Bandwidth..GB.sec.[df$Manufacturer == "ATI"])

# Tính p-value kiểm định Shapiro-Wilk cho từng nhóm
p_nvidia <- shapiro.test(nvidia_data)$p.value
p_amd    <- shapiro.test(amd_data)$p.value
p_intel  <- shapiro.test(intel_data)$p.value
p_ati    <- shapiro.test(ati_data)$p.value

# Hàm định dạng p-value hiển thị gọn gàng
format_p <- function(p) {
  if (p < 0.001) return("p < 0.001")
  return(paste0("p = ", round(p, 4)))
}

# Chia màn hình vẽ biểu đồ thành khung 2x2 và chỉnh lề dưới để chứa p-value
par(mfrow = c(2, 2), mar = c(5, 4, 3, 2))

# 2.1 Nvidia
qqnorm(nvidia_data, 
       main = "Q-Q Plot: Nvidia",
       sub = paste("Shapiro-Wilk:", format_p(p_nvidia)),
       col.sub = "red", font.sub = 2)
qqline(nvidia_data, col = "red")

# 2.2 AMD
qqnorm(amd_data, 
       main = "Q-Q Plot: AMD",
       sub = paste("Shapiro-Wilk:", format_p(p_amd)),
       col.sub = "red", font.sub = 2)
qqline(amd_data, col = "red")

# 2.3 Intel
qqnorm(intel_data, 
       main = "Q-Q Plot: Intel",
       sub = paste("Shapiro-Wilk:", format_p(p_intel)),
       col.sub = "red", font.sub = 2)
qqline(intel_data, col = "red")

# 2.4 ATI
qqnorm(ati_data, 
       main = "Q-Q Plot: ATI",
       sub = paste("Shapiro-Wilk:", format_p(p_ati)),
       col.sub = "red", font.sub = 2)
qqline(ati_data, col = "red")

# Trả lại cấu hình đồ thị ban đầu (1x1)
par(mfrow = c(1, 1))

# Kiểm định Shapiro-Wilk cho từng nhóm (an toàn với mọi cỡ mẫu)
cat("=== KIỂM ĐỊNH CHUẨN SHAPIRO-WILK ===\n")
cat("-- Nvidia --\n"); print(shapiro.test(nvidia_data))
cat("-- AMD --\n");    print(shapiro.test(amd_data))
cat("-- Intel --\n");  print(shapiro.test(intel_data))
cat("-- ATI --\n");    print(shapiro.test(ati_data))


# ------------------------------------------------------------------------------
# BƯỚC 3: KIỂM ĐỊNH PHƯƠNG SAI ĐỒNG NHẤT (LEVENE'S TEST)
# ------------------------------------------------------------------------------
cat("\n=== KIỂM ĐỊNH PHƯƠNG SAI ĐỒNG NHẤT (LEVENE) ===\n")
levene_res <- leveneTest(Memory_Bandwidth..GB.sec. ~ Manufacturer, data = df)
print(levene_res)


# ------------------------------------------------------------------------------
# BƯỚC 4: KIỂM ĐỊNH WELCH'S ANOVA
# (Sử dụng khi phương sai không đồng nhất)
# ------------------------------------------------------------------------------
cat("\n=== KIỂM ĐỊNH WELCH'S ANOVA ===\n")
welch_res <- welch_anova_test(Memory_Bandwidth..GB.sec. ~ Manufacturer, data = df)
print(welch_res)


# ------------------------------------------------------------------------------
# BƯỚC 5: KIỂM ĐỊNH HẬU ĐỊNH (POST-HOC GAMES-HOWELL TEST)
# ------------------------------------------------------------------------------
cat("\n=== KIỂM ĐỊNH HẬU ĐỊNH GAMES-HOWELL ===\n")
gh_res <- games_howell_test(Memory_Bandwidth..GB.sec. ~ Manufacturer, data = df)
print(gh_res)


# ------------------------------------------------------------------------------
# BƯỚC 6: TRỰC QUAN HÓA KẾT QUẢ (BOXPLOT)
# ------------------------------------------------------------------------------
# Lấy p-value của Welch's ANOVA để hiển thị dưới chú thích Boxplot
p_welch <- welch_res$p

boxplot(Memory_Bandwidth..GB.sec. ~ Manufacturer, data = df,
        main = "So sánh Băng thông bộ nhớ giữa các Hãng sản xuất GPU",
        sub = paste("Welch's ANOVA test:", format_p(p_welch)),
        col.sub = "blue", font.sub = 2,
        xlab = "Nhà sản xuất (Manufacturer)",
        ylab = "Băng thông bộ nhớ (GB/s)",
        col = c("red", "green", "blue", "orange"),
        outline = TRUE) # Dùng outline = FALSE để ẩn bớt giá trị ngoại lệ cực đoan giúp biểu đồ rõ ràng hơn

# ==============================================================================
# PHẦN 4: HỒI QUY TUYẾN TÍNH BỘI (MHHQTT.R)
# ==============================================================================

# =====================================================================
#  HOI QUY TUYEN TINH BOI - BANG THONG BO NHO GPU
#  Bien phu thuoc : Memory_Bandwidth (GB/sec)
# =====================================================================

## ---- 0. Đọc dữ liệu ----
main_data <- read.csv("C:/Users/ACER/Documents/BTL_XSTK/XSTK_GPU/dataset/All_GPUs_cleaned.csv", check.names = FALSE)


# =====================================================================
#  1. PHÂN TÍCH TƯƠNG QUAN  (sàng lọc biến)
# =====================================================================
gpu_numeric <- main_data[, c("Year", "Memory (MB)", "Memory_Bus (Bit)",
                             "Memory_Speed (MHz)", "Memory_Bandwidth (GB/sec)")]
round(cor(gpu_numeric), 2)                       

corrplot(cor(gpu_numeric), method = "color", addCoef.col = "black",
         number.cex = 0.8, type = "upper", tl.col = "red")   

# =====================================================================
#  2. CHIA TẬP HUẤN LUYỆN / KIỂM TRA  (80/20, cố định seed để tái lập)
# =====================================================================
set.seed(123)
train_size  <- floor(0.8 * nrow(main_data))
train_index <- sample(seq_len(nrow(main_data)), size = train_size)
train_data  <- main_data[train_index, ]          # 2578 quan sát
test_data   <- main_data[-train_index, ]          # 645 quan sát
cat("Số dòng Train:", nrow(train_data), " - Số dòng Test:", nrow(test_data), "\n")

# =====================================================================
#  3. MÔ HÌNH 1 : tuyến tính thô với cả 4 biến số
# =====================================================================
model_1 <- lm(`Memory_Bandwidth (GB/sec)` ~ Year + `Memory (MB)` +
                `Memory_Bus (Bit)` + `Memory_Speed (MHz)`,
              data = train_data)
summary(model_1)

# =====================================================================
#  4. KIỂM TRA CÁC GIẢ ĐỊNH của model_1
# =====================================================================
# 4.1 Bốn đồ thị chẩn đoán
par(mfrow = c(2, 2))
plot(model_1)
par(mfrow = c(1, 1))

# 4.2 Kỳ vọng sai số bằng 0 (mẫu lớn -> dùng kiểm định Z)
res1  <- residuals(model_1)
n1    <- length(res1)
z0    <- (mean(res1) - 0) / (sd(res1) / sqrt(n1))
z_crit <- qnorm(1 - 0.05 / 2)                    
data.frame(mean = mean(res1), sd = sd(res1), n = n1, z0 = z0, z_crit = z_crit)

# 4.3 Tính chuẩn của sai số (Shapiro-Wilk)
shapiro.test(res1)

# 4.4 Phương sai đồng nhất (Breusch-Pagan)
bptest(model_1)

# 4.5 Tính độc lập của sai số (Durbin-Watson)
dwtest(model_1)

# 4.6 Đa cộng tuyến (VIF - hệ số phóng đại phương sai)
vif(model_1)

# =====================================================================
#  5. DỰ BÁO TRÊN TẬP KIỂM TRA + ĐÁNH GIÁ (model_1)
# =====================================================================
test_data$pred <- predict(model_1, test_data)
head(test_data[, c("Memory (MB)", "Memory_Bandwidth (GB/sec)", "Memory_Bus (Bit)",
                   "Memory_Speed (MHz)", "pred")])

rmse1 <- sqrt(mean((test_data$`Memory_Bandwidth (GB/sec)` - test_data$pred)^2))
r2_1  <- 1 - sum((test_data$`Memory_Bandwidth (GB/sec)` - test_data$pred)^2) /
  sum((test_data$`Memory_Bandwidth (GB/sec)` 
       - mean(test_data$`Memory_Bandwidth (GB/sec)`))^2)
c(RMSE = rmse1, R2 = r2_1)
cat("Số dự báo âm:", sum(test_data$pred < 0), "\n")

# Biểu đồ thực tế vs dự báo
results_df <- data.frame(Actual = test_data$`Memory_Bandwidth (GB/sec)`,
                         Predicted = test_data$pred,
                         Model = "Linear Regression")
ggplot(results_df, aes(x = Actual, y = Predicted, color = Model)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  labs(title = "Đánh giá kết quả dự báo Băng thông bộ nhớ (GB/sec)",
       x = "Giá trị thực tế", y = "Giá trị dự báo") +
  theme_minimal()

# =====================================================================
#  6. MÔ HINH 2 : log-log (bỏ Year, lấy log(x+1) các biến còn lại)
# =====================================================================
train_log <- train_data
test_log  <- test_data
for (col in c("Memory (MB)", "Memory_Bus (Bit)", "Memory_Bandwidth (GB/sec)", "Memory_Speed (MHz)")) {
  train_log[[col]] <- log(train_log[[col]] + 1)   # +1 để tránh log(0)
  test_log[[col]]  <- log(test_log[[col]]  + 1)
}

model_2 <- lm(`Memory_Bandwidth (GB/sec)` ~ `Memory (MB)` +
                `Memory_Bus (Bit)` + `Memory_Speed (MHz)`,
              data = train_log)
summary(model_2)

# Kiểm tra lại tính chuẩn sau khi biến đổi
shapiro.test(residuals(model_2))
plot(model_2, which = 2, id.n = 0)               
bptest(model_2)
vif(model_2)

# =====================================================================
#  7. DỰ BÁO (model_2) - báo cáo trên CẢ HAI thang để so sánh công bằng
# =====================================================================
pred2_log  <- predict(model_2, test_log)
# (1) đánh giá trên thang log
rmse2_log <- sqrt(mean((test_log$`Memory_Bandwidth (GB/sec)` - pred2_log)^2))
r2_2_log  <- 1 - sum((test_log$`Memory_Bandwidth (GB/sec)` - pred2_log)^2) /
  sum((test_log$`Memory_Bandwidth (GB/sec)` 
       - mean(test_log$`Memory_Bandwidth (GB/sec)`))^2)
# (2) chuyển ngược về thang gốc GB/sec để so sánh công bằng với model_1
pred2_orig <- exp(pred2_log) - 1
yt_orig    <- test_data$`Memory_Bandwidth (GB/sec)`
rmse2_orig <- sqrt(mean((yt_orig - pred2_orig)^2))
r2_2_orig  <- 1 - sum((yt_orig - pred2_orig)^2) /
  sum((yt_orig - mean(yt_orig))^2)

cat("model_2 (thang log)  RMSE =", round(rmse2_log, 4),
    " R2 =", round(r2_2_log, 4), "\n")
cat("model_2 (thang gốc)  RMSE =", round(rmse2_orig, 4),
    " R2 =", round(r2_2_orig, 4),
    " số dự báo âm =", sum(pred2_orig < 0), "\n")



# So sánh model_2 sau khi đổi ngược về thang gốc GB/sec
test_data$pred_m2 <- exp(predict(model_2, test_log)) - 1   # đổi ngược: exp(.)-1

df2 <- data.frame(Actual    = test_data$`Memory_Bandwidth (GB/sec)`,
                  Predicted = test_data$pred_m2,
                  Model     = "Log-Log (model_2)")
ggplot(df2, aes(x = Actual, y = Predicted, color = Model)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  labs(title = "Model_2 (thang gốc): thực tế vs dự báo Băng thông (GB/sec)",
       x = "Giá trị thực tế", y = "Giá trị dự báo") +
  theme_minimal()













# ==============================================================================
# PHẦN 5: THỐNG KÊ SUY DIỄN 1 MẪU / 2 MẪU, ANOVA & HỒI QUY (thongkesuydien_1mau_2mau.R)
# ==============================================================================

# ------------------------------------------------------------------------------
# 5.1 GIỚI THIỆU VÀ CHUẨN BỊ DỮ LIỆU
# ------------------------------------------------------------------------------

# 1. Đọc dữ liệu từ file CSV
df <- read.csv("All_GPUs.csv", stringsAsFactors = FALSE)

# 2. Xóa khoảng trắng thừa trong chuỗi
df <- df %>% mutate(across(where(is.character), str_trim))

# 3. Chuẩn hóa giá trị nhiễu về NA
noise_values <- c("-", "null", "NaN", "None", "", "nan")
df[df %in% noise_values] <- NA

# Hàm hỗ trợ trích xuất số thực từ chuỗi chứa đơn vị
clean_numeric <- function(col) {
  as.numeric(str_extract(as.character(col), "\\d+\\.?\\d*"))
}

# 4. Trích xuất các biến định lượng cần thiết
df_clean <- df %>%
  mutate(
    Memory_Bandwidth_num = clean_numeric(Memory_Bandwidth),
    Memory_num           = clean_numeric(Memory),
    Memory_Bus_num       = clean_numeric(Memory_Bus),
    Memory_Speed_num     = clean_numeric(Memory_Speed),
    Process_num          = clean_numeric(Process)
  ) %>%
  filter(!is.na(Memory_Bandwidth_num) & !is.na(Manufacturer))

# 5. BẢNG 1: Thống kê mô tả biến Memory_Bandwidth theo hãng sản xuất
bang_1 <- df_clean %>%
  filter(Manufacturer %in% c("Nvidia", "AMD", "Intel", "ATI")) %>%
  group_by(Manufacturer) %>%
  summarise(
    n          = n(),
    Mean_GBs   = round(mean(Memory_Bandwidth_num, na.rm = TRUE), 2),
    SD_GBs     = round(sd(Memory_Bandwidth_num, na.rm = TRUE), 2),
    Median_GBs = round(median(Memory_Bandwidth_num, na.rm = TRUE), 2),
    IQR_GBs    = round(IQR(Memory_Bandwidth_num, na.rm = TRUE), 2)
  ) %>%
  arrange(desc(n))

cat("\n=== BẢNG 1: THỐNG KÊ MÔ TẢ MEMORY_BANDWIDTH ===\n")
print(bang_1)

# ------------------------------------------------------------------------------
# 5.2 KIỂM ĐỊNH TRUNG BÌNH MỘT MẪU (One-Sample t-test)
# H0: mu = 140  vs  H1: mu > 140 (Dành cho hãng Nvidia)
# ------------------------------------------------------------------------------

nvidia_bandwidth <- df_clean %>%
  filter(Manufacturer == "Nvidia") %>%
  pull(Memory_Bandwidth_num)

# Kiểm tra phân phối chuẩn bằng Shapiro-Wilk & Biểu đồ Q-Q
shapiro_nvidia <- shapiro.test(nvidia_bandwidth[1:min(5000, length(nvidia_bandwidth))])
cat("\n--- Kiểm định Shapiro-Wilk (Nvidia) ---\n")
print(shapiro_nvidia)

# Vẽ biểu đồ Q-Q
qqnorm(nvidia_bandwidth, main = "Q-Q Plot - Memory_Bandwidth Nvidia")
qqline(nvidia_bandwidth, col = "red", lwd = 2)

# Chạy kiểm định One-Sample t-test một phía
t_test_1samp <- t.test(nvidia_bandwidth, mu = 140, alternative = "greater")
cat("\n--- Kết quả One-Sample t-test (Nvidia) ---\n")
print(t_test_1samp)

# ------------------------------------------------------------------------------
# 5.3 KIỂM ĐỊNH TRUNG BÌNH HAI MẪU ĐỘC LẬP (Welch Two-Sample t-test)
# H0: mu_Nvidia = mu_AMD  vs  H1: mu_Nvidia != mu_AMD
# ------------------------------------------------------------------------------

amd_bandwidth <- df_clean %>%
  filter(Manufacturer == "AMD") %>%
  pull(Memory_Bandwidth_num)

# Chạy kiểm định Welch Two-Sample t-test
t_test_2samp <- t.test(nvidia_bandwidth, amd_bandwidth, var.equal = FALSE, alternative = "two.sided")
cat("\n--- Kết quả Welch Two-Sample t-test (Nvidia vs AMD) ---\n")
print(t_test_2samp)

# ------------------------------------------------------------------------------
# 5.4 PHÂN TÍCH PHƯƠNG SAI (Welch ANOVA) VÀ HẬU KIỂM (Games-Howell)
# ------------------------------------------------------------------------------

df_4vendors <- df_clean %>%
  filter(Manufacturer %in% c("Nvidia", "AMD", "Intel", "ATI")) %>%
  mutate(Manufacturer = factor(Manufacturer, levels = c("AMD", "ATI", "Intel", "Nvidia")))

# Biểu đồ hộp (Boxplot)
boxplot(Memory_Bandwidth_num ~ Manufacturer, data = df_4vendors,
        main = "Memory_Bandwidth theo hang san xuat",
        xlab = "Manufacturer", ylab = "Memory_Bandwidth (GB/s)",
        col = c("lightgreen", "lightpink", "lightyellow", "lightblue"))

# Kiểm định Levene về tính đồng nhất phương sai
levene_res <- leveneTest(Memory_Bandwidth_num ~ Manufacturer, data = df_4vendors)
cat("\n--- Kết quả Kiểm định Levene ---\n")
print(levene_res)

# Phân tích phương sai Welch's ANOVA (var.equal = FALSE)
welch_anova_res <- oneway.test(Memory_Bandwidth_num ~ Manufacturer, data = df_4vendors, var.equal = FALSE)
cat("\n--- Kết quả One-Way Welch ANOVA ---\n")
print(welch_anova_res)

# Phân tích hậu kiểm Games-Howell Test
gh_res <- games_howell_test(df_4vendors, Memory_Bandwidth_num ~ Manufacturer)
cat("\n--- BẢNG 2: KẾT QUẢ HẬU KIỂM GAMES-HOWELL ---\n")
print(gh_res)

# ------------------------------------------------------------------------------
# 5.5 MÔ HÌNH HỒI QUY TUYẾN TÍNH BỘI & TỐI ƯU HÓA LOGARIT
# ------------------------------------------------------------------------------

# Tập dữ liệu đủ quan sát cho mô hình hồi quy
d2 <- df_clean %>%
  filter(
    !is.na(Memory_num) & 
      !is.na(Memory_Bus_num) & 
      !is.na(Memory_Speed_num) & 
      !is.na(Process_num) &
      Memory_Bandwidth_num > 0
  )

# 1. Mô hình Hồi quy OLS Ban đầu
model_lm <- lm(Memory_Bandwidth_num ~ Memory_num + Memory_Bus_num + 
                 Memory_Speed_num + Process_num, data = d2)

cat("\n=== KẾT QUẢ MÔ HÌNH HỒI QUY OLS BAN ĐẦU ===\n")
print(summary(model_lm))

# Kiểm tra Đa cộng tuyến bằng VIF
cat("\n--- Hệ số VIF mô hình ban đầu ---\n")
print(vif(model_lm))

# Đồ thị chẩn đoán mô hình OLS ban đầu
par(mfrow = c(2, 2))
plot(model_lm)
par(mfrow = c(1, 1))

# Kiểm định Breusch-Pagan cho phương sai sai số
bp_test <- ncvTest(model_lm)
cat("\n--- Kiểm định Breusch-Pagan (Mô hình ban đầu) ---\n")
print(bp_test)

# 2. Khắc phục: Mô hình Hồi quy Logarit Tinh gọn (Mô hình cuối cùng)
model_log_final <- lm(log(Memory_Bandwidth_num) ~ Memory_Bus_num + 
                        Memory_Speed_num + Process_num, data = d2)

cat("\n=== KẾT QUẢ MÔ HÌNH HỒI QUY LOGARIT CUỐI CÙNG (model_log_final) ===\n")
print(summary(model_log_final))

# Khoảng tin cậy 95% cho các hệ số mô hình cuối
cat("\n--- Khoảng tin cậy 95% các hệ số (model_log_final) ---\n")
print(confint(model_log_final))