# XSTK_GPU
# GPU Dataset – Tổng quan dữ liệu

## Nguồn
File gốc: `All_GPUs.csv`

## Sau tiền xử lý (`df_imputed`)
- **3316 dòng**, **8 cột**

## Mô tả các biến

| Biến | Kiểu | Mô tả |
|------|------|--------|
| `Name` | chr | Tên GPU |
| `Architecture` | chr | Kiến trúc vi xử lý đồ họa (Tesla, RV630, ...) |
| `Manufacturer` | chr | Nhà sản xuất (Nvidia, AMD, ...) |
| `Year` | int | Năm phát hành |
| `Memory (MB)` | num | Dung lượng bộ nhớ (MB) |
| `Memory_Bandwidth (GB/sec)` | num | Băng thông bộ nhớ (GB/s) |
| `Memory_Bus (Bit)` | num | Độ rộng bus bộ nhớ (Bit) |
| `Memory_Speed (MHz)` | num | Tốc độ bộ nhớ (MHz) |

## Các bước tiền xử lý
1. Trim khoảng trắng, thay giá trị nhiễu (`-`, `null`, `NaN`, ...) → `NA`
2. Xóa các cột có tỷ lệ `NA > 15%`
3. Chuyển `Release_Date` → `Year` (dạng integer)
4. Xóa đơn vị khỏi các cột số, đổi tên kèm đơn vị
5. Xóa dòng thiếu `Year` hoặc `Architecture`
6. Imputation:
   - `Memory (MB)`: điền ngẫu nhiên từ tập `{2^5, ..., 2^12}`
   - Các cột memory còn lại: điền bằng **trung vị**