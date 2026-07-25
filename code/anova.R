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


df_anova <- aov(Memory_Bandwidth..GB.sec. ~ Manufacturer, data = df)
summary(df_anova)

"""               Df   Sum Sq Mean Sq F value Pr(>F)    
Manufacturer      3  3619014   1206338   71.73 <2e-16 ***
Residuals        3312 55703211  16819                   
---"""

print(qf(0.05,3,3312,lower.tail = FALSE))
##   2.607591

TukeyHSD(df_anova)
  """Tukey multiple comparisons of means
    95% family-wise confidence level

Fit: aov(formula = Memory_Bandwidth..GB.sec. ~ Manufacturer, data = df)

$Manufacturer
                   diff         lwr        upr     p adj
ATI-AMD        79.26092   42.523005  115.99883 0.0000002
Intel-AMD    -102.07075 -125.360200  -78.78131 0.0000000
Nvidia-AMD     18.41914    6.086049   30.75224 0.0007268
Intel-ATI    -181.33167 -222.781017 -139.88232 0.0000000
Nvidia-ATI    -60.84177  -97.280148  -24.40339 0.0001073
Nvidia-Intel  120.48990   97.675876  143.30392 0.0000000
"""