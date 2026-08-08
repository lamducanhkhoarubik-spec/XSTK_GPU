# =====================================================================
#  HOI QUY TUYEN TINH BOI - BANG THONG BO NHO GPU
#  Bien phu thuoc : Memory_Bandwidth (GB/sec)
# =====================================================================

## ---- 0. Đọc dữ liệu ----
main_data <- read.csv("All_GPUs_cleaned.csv", check.names = FALSE)


# =====================================================================
#  1. PHÂN TÍCH TƯƠNG QUAN  (sàng lọc biến)
# =====================================================================
gpu_numeric <- main_data[, c("Year", "Memory (MB)", "Memory_Bus (Bit)",
                             "Memory_Speed (MHz)", "Memory_Bandwidth (GB/sec)")]
round(cor(gpu_numeric), 2)                       

library(corrplot)
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
library(lmtest)
bptest(model_1)

# 4.5 Tính độc lập của sai số (Durbin-Watson)
dwtest(model_1)

# 4.6 Đa cộng tuyến (VIF - hệ số phóng đại phương sai)
library(car)
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
library(ggplot2)
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

library(ggplot2)
df2 <- data.frame(Actual    = test_data$`Memory_Bandwidth (GB/sec)`,
                  Predicted = test_data$pred_m2,
                  Model     = "Log-Log (model_2)")
ggplot(df2, aes(x = Actual, y = Predicted, color = Model)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  labs(title = "Model_2 (thang gốc): thực tế vs dự báo Băng thông (GB/sec)",
       x = "Giá trị thực tế", y = "Giá trị dự báo") +
  theme_minimal()











