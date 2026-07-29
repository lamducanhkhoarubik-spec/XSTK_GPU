# Khai báo thư viện
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)

# 1. Đọc dữ liệu từ dataset
df <- read.csv("C:\Users\ACER\Documents\BTL_XSTK\XSTK_GPU\dataset\All_GPUs.csv")

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

# 6. XÓA DÒNG nếu bị trống ở: Year, Architecture HOẶC Memory (MB)
df_imputed <- df_imputed %>% 
  filter(!is.na(Year) & !is.na(Architecture) & !is.na(`Memory (MB)`))

cat("Số dòng sau khi xóa các dòng trống Year, Architecture và Memory (MB):", nrow(df_imputed), "\n")

# 7. ĐIỀN KHUYẾT các cột Memory còn lại bằng TRUNG VỊ THEO MANUFACTURER
df_imputed <- df_imputed %>%
  group_by(Manufacturer) %>%
  mutate(
    `Memory_Bandwidth (GB/sec)` = ifelse(
      is.na(`Memory_Bandwidth (GB/sec)`), 
      median(`Memory_Bandwidth (GB/sec)`, na.rm = TRUE), 
      `Memory_Bandwidth (GB/sec)`
    ),
    `Memory_Bus (Bit)` = ifelse(
      is.na(`Memory_Bus (Bit)`), 
      median(`Memory_Bus (Bit)`, na.rm = TRUE), 
      `Memory_Bus (Bit)`
    ),
    `Memory_Speed (MHz)` = ifelse(
      is.na(`Memory_Speed (MHz)`), 
      median(`Memory_Speed (MHz)`, na.rm = TRUE), 
      `Memory_Speed (MHz)`
    )
  ) %>%
  ungroup()

# Phương án dự phòng: Nếu một hãng nào đó bị NA toàn bộ cột, điền bằng Median chung toàn bộ dataset
other_memory_cols <- c("Memory_Bandwidth (GB/sec)", "Memory_Bus (Bit)", "Memory_Speed (MHz)")
for (col in other_memory_cols) {
  if (any(is.na(df_imputed[[col]]))) {
    col_median <- median(df_imputed[[col]], na.rm = TRUE)
    df_imputed[[col]][is.na(df_imputed[[col]])] <- col_median
  }
}

# Kiểm tra lại số lượng NA sau khi làm sạch hoàn tất
cat("Tổng số NA còn lại trong dataset:", sum(is.na(df_imputed)), "\n")