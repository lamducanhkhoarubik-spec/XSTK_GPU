# Thêm đường dẫn
df <- read.csv("")

anova <- aov(Memory_Bandwidth..GB.sec. ~ Manufacturer, data = df)
summary(anova)

"""               Df   Sum Sq Mean Sq F value Pr(>F)    
Manufacturer      3  3619014   1206338   71.73 <2e-16 ***
Residuals        3312 55703211  16819                   
---"""

print(qf(0.05,3,3312,lower.tail = FALSE))
##   2.607591