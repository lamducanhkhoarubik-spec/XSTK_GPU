# ==============================================================================
# BTL XÁC SUẤT THỐNG KÊ - PHÂN TÍCH BĂNG THÔNG BỘ NHỚ (GPU) THEO HÃNG SẢN XUẤT
# ==============================================================================

# ------------------------------------------------------------------------------
# BƯỚC 1: KHỞI TẠO THƯ VIỆN VÀ ĐỌC DỮ LIỆU
# ------------------------------------------------------------------------------
# Tự động cài đặt gói nếu chưa có
if (!require("nortest")) install.packages("nortest")
if (!require("rstatix")) install.packages("rstatix")
if (!require("car")) install.packages("car")

library(nortest)
library(rstatix)
library(car)

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