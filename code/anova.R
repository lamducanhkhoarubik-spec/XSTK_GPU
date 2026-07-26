# Thêm đường dẫn
df <- read.csv("")

#Kiểm định có theo phân phối chuẩn không
nvidia = subset(df, df$Manufacturer == "Nvidia")
qqnorm(nvidia$Memory_Bandwidth..GB.sec.)
qqline(nvidia$Memory_Bandwidth..GB.sec.)
ad.test(nvidia$Memory_Bandwidth..GB.sec.)

amd = subset(df, df$Manufacturer == "AMD")
qqnorm(amd$Memory_Bandwidth..GB.sec.)
qqline(amd$Memory_Bandwidth..GB.sec.)
ad.test(amd$Memory_Bandwidth..GB.sec.)

intel = subset(df, df$Manufacturer == "Intel")
qqnorm(intel$Memory_Bandwidth..GB.sec.)
qqline(intel$Memory_Bandwidth..GB.sec.)
ad.test(intel$Memory_Bandwidth..GB.sec.)

ati = subset(df, df$Manufacturer == "ATI")
qqnorm(ati$Memory_Bandwidth..GB.sec.)
qqline(ati$Memory_Bandwidth..GB.sec.)
ad.test(ati$Memory_Bandwidth..GB.sec.)


 # 1. Tai cac thu vien can thiet
if (!require("rstatix")) install.packages("rstatix")
if (!require("car")) install.packages("car")

library(rstatix)
library(car)

# 2. Ep kieu bien dinh danh (Factor)
df$Manufacturer <- as.factor(df$Manufacturer)

# 3. Kiem dinh phuong sai dong nhat (Levene's Test)
leveneTest(Memory_Bandwidth ~ Manufacturer, data = df)

# 4. Thuc hien Welch's ANOVA
welch_res <- welch_anova_test(Memory_Bandwidth ~ Manufacturer, data = df)
print(welch_res)