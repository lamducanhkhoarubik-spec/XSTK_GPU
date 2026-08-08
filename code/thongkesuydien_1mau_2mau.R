# Khai báo các thư viện cần thiết
library(dplyr)
library(stringr)
library(car)        # Dùng cho kiểm định Levene và tính hệ số VIF
library(rstatix)    # Dùng cho kiểm định hậu kiểm Games-Howell
library(ggplot2)

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