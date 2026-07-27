# ============================================================
# GPU DESCRIPTIVE STATISTICS - CORE ANALYSIS
# Chỉ giữ phần phân tích chính, xuất kết quả dạng bảng số
# ============================================================

# Cấu hình
INPUT_PATH <- ""
OUTPUT_DIR <- "ket_qua_thong_ke"
RANDOM_SEED <- 20260726

# Load packages
library(dplyr)
library(readr)
library(e1071)

set.seed(RANDOM_SEED)
options(scipen = 999)

# -------------------- HÀM ĐỌC DỮ LIỆU --------------------
read_data <- function(path) {
  # Đọc CSV, tự động thử nhiều encoding
  encodings <- c("UTF-8", "UTF-8-BOM", "windows-1252", "ISO-8859-1")
  for (enc in encodings) {
    df <- tryCatch(
      read_csv(path, locale = locale(encoding = enc), show_col_types = FALSE),
      error = function(e) NULL
    )
    if (!is.null(df)) return(as.data.frame(df))
  }
  stop("Không đọc được file CSV")
}

# -------------------- ÁNH XẠ CỘT --------------------
map_columns <- function(df) {
  names_lower <- tolower(names(df))
  
  cols <- list(
    manufacturer = c("manufacturer", "brand", "vendor"),
    year = c("year", "release_year"),
    bandwidth = c("memory_bandwidth", "memory_bandwidth (gb/sec)"),
    memory = c("memory", "memory_mb", "memory (mb)"),
    bus = c("memory_bus", "memory_bus (bit)"),
    speed = c("memory_speed", "memory_speed (mhz)"),
    architecture = c("architecture", "gpu_architecture"),
    name = c("name", "gpu_name", "product_name")
  )
  
  found <- list()
  for (key in names(cols)) {
    match <- names_lower[names_lower %in% cols[[key]]][1]
    found[[key]] <- if (!is.na(match)) names(df)[which(names_lower == match)[1]] else NA
  }
  
  # Kiểm tra cột bắt buộc
  required <- c("manufacturer", "year", "bandwidth")
  missing <- required[sapply(required, function(x) is.na(found[[x]]))]
  if (length(missing) > 0) {
    stop("Thiếu cột: ", paste(missing, collapse = ", "))
  }
  
  found
}

# -------------------- THỐNG KÊ MÔ TẢ --------------------
descriptive_stats <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  n <- length(x)
  
  if (n == 0) return(rep(NA, 14))
  
  q <- quantile(x, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
  
  c(
    n = n,
    mean = mean(x),
    median = q[3],
    min = q[1],
    q1 = q[2],
    q3 = q[4],
    max = q[5],
    sd = if (n >= 2) sd(x) else NA,
    var = if (n >= 2) var(x) else NA,
    iqr = q[4] - q[2],
    skewness = if (n >= 3) skewness(x, type = 2) else NA,
    kurtosis = if (n >= 4) kurtosis(x, type = 2) else NA,
    missing = sum(is.na(x)),
    missing_pct = mean(is.na(x)) * 100
  )
}

# -------------------- PHÂN TÍCH CHÍNH --------------------
main <- function() {
  cat("\n=== PHÂN TÍCH THỐNG KÊ MÔ TẢ GPU ===\n\n")
  
  # 1. Đọc dữ liệu
  cat("[1/4] Đọc dữ liệu...\n")
  df <- read_data(INPUT_PATH)
  cols <- map_columns(df)
  cat("  -", nrow(df), "dòng,", ncol(df), "cột\n")
  cat("  - Biến chính:", paste(unlist(cols[1:6]), collapse = ", "), "\n\n")
  
  # 2. Lấy các biến cần phân tích
  target_cols <- c(cols$bandwidth, cols$memory, cols$bus, cols$speed, cols$year)
  target_cols <- target_cols[!is.na(target_cols)]
  df_num <- df[, target_cols, drop = FALSE]
  
  # 3. Tính thống kê
  cat("[2/4] Tính thống kê mô tả...\n")
  stats_list <- lapply(df_num, descriptive_stats)
  stats_df <- do.call(rbind, stats_list)
  colnames(stats_df) <- c("n", "Mean", "Median", "Min", "Q1", "Q3", "Max", 
                          "SD", "Var", "IQR", "Skewness", "Kurtosis", 
                          "Missing", "Missing_%")
  stats_df <- round(stats_df, 3)
  
  # 4. Thống kê theo Manufacturer
  cat("[3/4] Thống kê theo nhà sản xuất...\n")
  manufacturer_stats <- df %>%
    group_by(.data[[cols$manufacturer]]) %>%
    summarise(
      n = sum(!is.na(.data[[cols$bandwidth]])),
      Mean = mean(.data[[cols$bandwidth]], na.rm = TRUE),
      Median = median(.data[[cols$bandwidth]], na.rm = TRUE),
      SD = sd(.data[[cols$bandwidth]], na.rm = TRUE),
      Q1 = quantile(.data[[cols$bandwidth]], 0.25, na.rm = TRUE),
      Q3 = quantile(.data[[cols$bandwidth]], 0.75, na.rm = TRUE),
      IQR = Q3 - Q1,
      .groups = "drop"
    ) %>%
    arrange(desc(Median))
  
  # 5. Phân phối Manufacturer
  cat("[4/4] Phân phối nhà sản xuất...\n")
  manufacturer_dist <- df %>%
    count(.data[[cols$manufacturer]]) %>%
    mutate(
      pct = n / sum(n) * 100,
      cum_pct = cumsum(pct)
    ) %>%
    arrange(desc(n))
  
  # 6. Tương quan với Bandwidth
  cat("\n[5/5] Tương quan với Memory Bandwidth...\n")
  cor_columns <- setdiff(names(df_num), cols$bandwidth)
  cor_matrix <- cor(df_num, use = "pairwise.complete.obs")
  
  correlation <- data.frame(
    Variable = cor_columns,
    Pearson = cor_matrix[cols$bandwidth, cor_columns],
    Spearman = cor(df_num, use = "pairwise.complete.obs", method = "spearman")[cols$bandwidth, cor_columns]
  )
  correlation <- correlation[order(-abs(correlation$Pearson)), ]
  row.names(correlation) <- NULL
  
  # -------------------- XUẤT KẾT QUẢ --------------------
  dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
  
  # Lưu CSV
  write.csv(stats_df, file.path(OUTPUT_DIR, "01_descriptive_stats.csv"))
  write.csv(manufacturer_stats, file.path(OUTPUT_DIR, "02_bandwidth_by_manufacturer.csv"))
  write.csv(manufacturer_dist, file.path(OUTPUT_DIR, "03_manufacturer_distribution.csv"))
  write.csv(correlation, file.path(OUTPUT_DIR, "04_correlations.csv"))
  
  # -------------------- BÁO CÁO DẠNG TEXT --------------------
  report <- c(
    "========================================",
    "BÁO CÁO THỐNG KÊ MÔ TẢ GPU",
    "========================================",
    "",
    paste("1. TỔNG QUAN DỮ LIỆU"),
    paste("   - Số quan sát:", nrow(df)),
    paste("   - Số biến:", ncol(df)),
    paste("   - Năm:", min(df[[cols$year]], na.rm = TRUE), "–", max(df[[cols$year]], na.rm = TRUE)),
    paste("   - Tổng ô khuyết:", sum(is.na(df))),
    "",
    paste("2. THỐNG KÊ MÔ TẢ CÁC BIẾN CHÍNH"),
    paste("   (n =", nrow(df), "quan sát)"),
    "",
    paste("   Variable  | n | Mean | Median | SD | Min | Max"),
    paste("   ----------|---|------|--------|----|-----|-----")
  )
  
  for (var in rownames(stats_df)) {
    row <- stats_df[var, ]
    report <- c(report,
      sprintf("   %-10s | %3d | %6.2f | %7.2f | %5.2f | %6.2f | %6.2f",
              var, row["n"], row["Mean"], row["Median"], row["SD"], row["Min"], row["Max"])
    )
  }
  
  report <- c(report,
    "",
    paste("3. PHÂN PHỐI NHÀ SẢN XUẤT"),
    "",
    paste("   Manufacturer | n | % | Cum %"),
    paste("   -------------|---|---|------")
  )
  
  for (i in 1:nrow(manufacturer_dist)) {
    row <- manufacturer_dist[i, ]
    report <- c(report,
      sprintf("   %-13s | %3d | %4.1f | %5.1f",
              row[[1]], row$n, row$pct, row$cum_pct)
    )
  }
  
  report <- c(report,
    "",
    paste("4. TƯƠNG QUAN VỚI MEMORY BANDWIDTH"),
    "",
    paste("   Variable        | Pearson | Spearman"),
    paste("   ----------------|---------|---------")
  )
  
  for (i in 1:nrow(correlation)) {
    report <- c(report,
      sprintf("   %-16s | %7.3f | %7.3f",
              correlation$Variable[i], correlation$Pearson[i], correlation$Spearman[i])
    )
  }
  
  report <- c(report,
    "",
    "========================================",
    "GHI CHÚ:",
    paste("  -", cols$bandwidth, "là biến mục tiêu"),
    "  - SD: độ lệch chuẩn mẫu",
    "  - Skewness > 1: lệch phải mạnh; < -1: lệch trái mạnh",
    "  - Tương quan không chứng minh quan hệ nhân quả",
    "========================================"
  )
  
  # Ghi báo cáo
  writeLines(report, file.path(OUTPUT_DIR, "REPORT.txt"))
  
  # In ra console
  cat("\n", paste(report, collapse = "\n"), "\n\n")
  
  cat("✅ Đã lưu kết quả tại:", OUTPUT_DIR, "\n")
  cat("  - 01_descriptive_stats.csv\n")
  cat("  - 02_bandwidth_by_manufacturer.csv\n")
  cat("  - 03_manufacturer_distribution.csv\n")
  cat("  - 04_correlations.csv\n")
  cat("  - REPORT.txt\n")
}

# Chạy
main()