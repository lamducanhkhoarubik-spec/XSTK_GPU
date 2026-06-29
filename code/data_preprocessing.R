#Khai báo các thư viện
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)

#Đọc dữ liệu từ dataset
df <- read.csv(C:/Users/ACER/Documents/BTL_XSTK/XSTK_GPU/All_GPUs.csv)
# 1. Xóa khoảng trắng cho TẤT CẢ các cột dạng chữ (character)
df <- df %>%
  mutate(across(where(is.character), str_trim))
# 2. Thay thế các kí tự nhiễu thành NA (NaN trong R)
noise_values <- c("-", "null", "NaN", "None", "", "nan")

df_nan <- df %>%
  mutate(across(everything(), ~ ifelse(as.character(.) %in% noise_values, NA, .)))

# df_nan <- df_nan %>% mutate(across(c(Memory, Memory_Bandwidth, Memory_Bus, Memory_Speed), as.numeric))
# 3. Thống kê số lượng và PHẦN TRĂM NA ở từng cột dữ liệu
missing_summary <- data.frame(
  Column = colnames(df_nan),
  Count = colSums(is.na(df_nan)),
  Percentage = (colSums(is.na(df_nan)) / nrow(df_nan)) * 100
)
print("Thống kê số lượng và tỷ lệ % NA sau khi làm sạch:")
print(missing_summary)
# 4. Vẽ biểu đồ cột hiển thị PHẦN TRĂM NA có kèm số trên đầu cột
# Sắp xếp lại thứ tự xuất hiện của Tên cột trên biểu đồ để không bị lộn xộn
missing_summary$Column <- factor(missing_summary$Column, levels = missing_summary$Column)

ggplot(missing_summary, aes(x = Column, y = Percentage)) +
  # Vẽ cột màu xanh dodgerblue giống Python
  geom_bar(stat = "identity", fill = "dodgerblue", color = "black") +
    geom_text(
    data = filter(missing_summary, Percentage > 0), # Chỉ hiển thị nếu > 0%
    aes(label = sprintf("%.1f%%", Percentage)),      # Định dạng 1 chữ số thập phân
    vjust = -0.8,                                   # Đẩy chữ lên phía trên đầu cột
    fontface = "bold", 
    size = 3
  ) +
  # --------------------------------------------
# Tiêu đề và nhãn trục
labs(
  title = "Biểu đồ thống kê tỷ lệ % dữ liệu thiếu (NA) sau khi làm sạch",
  x = "Tên cột",
  y = "Tỷ lệ thiếu (%)"
) +
  # Tự động nới rộng trục Y giống plt.ylim() để không bị che khuất chữ
  scale_y_continuous(limits = c(0, max(missing_summary$Percentage + 10, 100))) +
  
  # Cấu hình giao diện: xoay chữ 45 độ, căn chỉnh text, thêm lưới ngang
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    panel.grid.major.x = element_blank(), # Ẩn lưới dọc cho giống Python
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray", linetype = "dashed", size = 0.5)
  )
# 1. Lấy danh sách tên các cột có tỷ lệ dữ liệu thiếu (NA) > 15%
cols_to_drop <- missing_summary %>% 
  filter(Percentage > 15) %>% 
  pull(Column) # pull() tương đương với .index.tolist() trong trường hợp này để lấy ra vector tên cột
cat("Các cột sẽ bị xóa vì thiếu > 15% dữ liệu:", paste(cols_to_drop, collapse = ", "), "\n")
# 2. Xóa các cột này khỏi DataFrame df_nan để tạo df_brief
# Dấu ! nghĩa là phủ định (giữ lại các cột KHÔNG nằm trong danh sách cols_to_drop)
df_brief <- df_nan %>% 
  select(!all_of(cols_to_drop))
df_brief_1 <- df_brief
# 2. Đổi cột Release_Date thành Year
if ("Release_Date" %in% colnames(df_brief_1)) {
  
  # Làm sạch khoảng trắng thừa
  df_brief_1 <- df_brief_1 %>%
    mutate(Release_Date = str_trim(as.character(Release_Date)))
  
  # Cách 1: Chuyển đổi bằng dmy (Date) và lấy năm
  # Trong R mặc định locale tiếng Anh thường nhận diện tốt "01-Mar-2009"
  date_parsed <- as.Date(df_brief_1$Release_Date, format = "%d-%b-%Y")
  df_brief_1$Year <- as.numeric(format(date_parsed, "%Y"))
  
  # PHƯƠNG ÁN DỰ PHÒNG: Nếu bị NA do khác biệt ngôn ngữ hệ thống, 
  fallback_year <- as.numeric(sapply(strsplit(df_brief_1$Release_Date, "-"), tail, 1))
  df_brief_1$Year <- ifelse(is.na(df_brief_1$Year), fallback_year, df_brief_1$Year)
  
  # Ép kiểu số nguyên (Integer) để hiển thị dạng 2009 (không có .0)
  df_brief_1$Year <- as.integer(df_brief_1$Year)
  
  # Xóa cột cũ Release_Date
  df_brief_1 <- df_brief_1 %>% select(-Release_Date)
}
# 3. Xử lý xóa đơn vị và đổi tên các cột dữ liệu
# Cấu hình danh sách cột, đơn vị cần xóa và tên mới
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
    # Ép kiểu chuỗi, xóa chữ đơn vị (không phân biệt hoa thường) và xóa khoảng trắng
    df_brief_1[[old_col]] <- str_trim(str_replace(as.character(df_brief_1[[old_col]]), regex(unit, ignore_case = TRUE), ""))
    # Đổi các ô rỗng hoặc chữ "nan" thành NA thực sự
    df_brief_1[[old_col]][df_brief_1[[old_col]] %in% c("", "nan", "NA")] <- NA
    # Chuyển cột về dạng số (numeric)
    df_brief_1[[old_col]] <- as.numeric(df_brief_1[[old_col]])
    # Đổi tên cột thành Tên_Cột (Đơn_Vị)
    new_col_name <- paste0(old_col, " (", unit, ")")
    df_brief_1 <- df_brief_1 %>% rename(!!new_col_name := all_of(old_col))
  }
}
# 4. Định nghĩa cột dữ liệu
selected_columns <- c(
  "Name",
  "Architecture",
  "Manufacturer",
  "Year",                        # Cột Year đã đổi từ Release_Date ở bước trước
  "Memory (MB)",                 # Các cột đã được đổi tên kèm đơn vị
  "Memory_Bandwidth (GB/sec)",
  "Memory_Bus (Bit)",
  "Memory_Speed (MHz)"
)
df_filtered <- df_brief_1 %>% 
  select(all_of(selected_columns))

df_imputed <- df_filtered
# 5. XÓA DÒNG (dropna) nếu dòng đó bị khuyết ở cột 'Year' HOẶC 'Architecture'
df_imputed <- df_imputed %>% 
  filter(!is.na(Year) & !is.na(Architecture))

# In số dòng sau khi đã dropna ở 2 cột này để bạn kiểm tra
cat("Số dòng sau khi xóa khuyết thiếu ở Year và Architecture:", nrow(df_imputed), "\n")
# 6. ĐIỀN KHUYẾT (Imputation) ngẫu nhiên 2^i cho cột 'Memory (MB)'
memory_choices <- 2^(5:12)
na_memory_indices <- which(is.na(df_imputed$`Memory (MB)`))

if (length(na_memory_indices) > 0) {
  df_imputed$`Memory (MB)`[na_memory_indices] <- sample(
    memory_choices, 
    size = length(na_memory_indices), 
    replace = TRUE
  )
}
# 6. ĐIỀN KHUYẾT bằng TRUNG VỊ cho các cột Memory còn lại
other_memory_cols <- c("Memory_Bandwidth (GB/sec)", "Memory_Bus (Bit)", "Memory_Speed (MHz)")

for (col in other_memory_cols) {
  if (col %in% colnames(df_imputed)) {
    col_median <- median(df_imputed[[col]], na.rm = TRUE)
    df_imputed[[col]][is.na(df_imputed[[col]])] <- col_median
  }
}