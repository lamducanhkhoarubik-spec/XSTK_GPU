# GPU Memory Bandwidth Analysis (XSTK_GPU)

A statistics course project (MT2013 – Probability & Statistics, Ho Chi Minh City University of Technology, VNU-HCM) analyzing GPU hardware specifications, with a focus on **memory bandwidth**. The project covers the full pipeline from data cleaning to descriptive statistics, hypothesis testing, ANOVA, and multiple linear regression, implemented in R.

## Overview

Using a dataset of 3,406 GPU models (1998–2017) from Nvidia, AMD, Intel, and ATI, this project investigates:

- How memory bandwidth is distributed and how it varies across manufacturers.
- Whether Nvidia's average memory bandwidth exceeds a performance benchmark (140 GB/s).
- Whether there is a statistically significant difference in memory bandwidth between Nvidia and AMD.
- Whether manufacturer has a significant effect on memory bandwidth (ANOVA).
- How well memory bandwidth can be predicted from hardware specs (multiple linear regression).

## Dataset

- **Source:** [Computer Parts (CPUs and GPUs) – Kaggle](https://www.kaggle.com/datasets/iliassekkaf/computerparts)
- **Raw size:** 3,406 observations × 34 variables
- **After cleaning:** 3,223 observations × 8 variables
- **Time span:** 1998–2017
- **Manufacturers:** Nvidia (52.16%), AMD (38.50%), Intel (6.83%), ATI (2.51%)

## Project Structure

```
XSTK_GPU/
├── dataset/
│   └── All_GPUs.csv          # Raw dataset
├── main.tex / main.pdf       # Full written report (LaTeX)
├── scripts/                  # R scripts for each analysis stage
└── README.md
```

## Methodology

1. **Data preprocessing**
   - Standardized inconsistent missing-value markers (`"-"`, `"null"`, `"NaN"`, `"None"`, `""`, `"nan"`) to `NA`.
   - Dropped columns with more than 15% missing data.
   - Extracted release year from the `Release_Date` field via regex.
   - Stripped units from numeric fields (e.g., `MB`, `GB/sec`, `Bit`, `MHz`, `nm`) and cast to numeric.
   - Applied listwise deletion for the target variable and key categorical fields (`Memory_Bandwidth`, `Year`, `Architecture`).
   - Imputed remaining missing predictor values using a three-tier grouped median strategy (by manufacturer + architecture → by manufacturer → global median).

2. **Descriptive statistics**
   - Summary statistics (mean, median, standard deviation, skewness, CV) for all quantitative variables.
   - Distribution and outlier analysis of `Memory_Bandwidth` using the Tukey IQR rule.
   - Manufacturer-level comparisons and time-trend visualization.
   - Pearson correlation between `Memory_Bandwidth` and other quantitative variables.

3. **Inferential statistics**
   - **One-sample Z-test:** Is Nvidia's average memory bandwidth greater than 140 GB/s?
   - **Two-sample Z-test:** Is there a significant difference between Nvidia and AMD's average memory bandwidth?
   - **Welch's ANOVA + Games–Howell post-hoc test:** Comparing memory bandwidth across all four manufacturers (used instead of classical ANOVA due to violated normality and homogeneity-of-variance assumptions).

4. **Multiple linear regression**
   - **`model_1`** (raw linear model): `Memory_Bandwidth ~ Year + Memory + Memory_Bus + Memory_Speed`. Achieves R² ≈ 0.77 but suffers from heteroscedasticity, non-normal residuals, a physically implausible negative coefficient for `Year` (multicollinearity), and 13 negative bandwidth predictions.
   - **`model_2`** (log–log model): `ln(Bandwidth+1) ~ ln(Memory+1) + ln(Memory_Bus+1) + ln(Memory_Speed+1)`, dropping `Year`. Reflects the multiplicative physical relationship `Bandwidth ∝ Bus × Speed`, recovers an elasticity for bus width close to the theoretical value of 1, removes all negative predictions, and improves residual diagnostics.

## Key Findings

- Memory capacity, bandwidth, and bus width are strongly right-skewed; the median is a more representative measure than the mean for these variables.
- 119 observations (3.69%) are statistically flagged as outliers in memory bandwidth but represent legitimate high-end GPUs and were retained.
- Nvidia's average memory bandwidth is statistically significantly greater than 140 GB/s (Z = 3.70, p < 0.001).
- Nvidia's average memory bandwidth is significantly higher than AMD's, with a 95% CI for the difference of [8.34, 28.32] GB/s.
- Manufacturer has a statistically significant effect on memory bandwidth (Welch's ANOVA, p < 0.001); post-hoc ranking: **ATI > Nvidia > AMD > Intel**.
- The log–log regression model achieves comparable predictive accuracy to the raw linear model on the original scale (R² ≈ 0.77, RMSE ≈ 59 GB/s) while being far more interpretable and free of invalid negative predictions.
- Residual analysis of the log–log model uncovered a systematic data-entry error (bus width recorded as 88 bit instead of 352 bit) across a batch of GeForce GTX 1080 Ti entries.

## Tools & Libraries

- **R** with `tidyverse`, `ggplot2`, `car`, `lmtest`, `rstatix`, `corrplot`, `e1071`
- **LaTeX** for report typesetting

## Limitations & Future Work

- Formal normality/homoscedasticity tests are highly sensitive at this sample size (n = 3,223) and tend to reject even after visible improvement in diagnostic plots.
- The log-scale back-transformation is subject to Jensen's inequality bias; a Duan smearing correction was not applied.
- Future extensions: remove duplicate SKUs and correct systematic data errors, use robust (HC3) or weighted least squares to address heteroscedasticity, and explore non-linear models (GAM, Random Forest, XGBoost).

## Team (MT01)

| # | Name | Contribution |
|---|------|--------------|
| 1 | Nguyễn Văn Cường | Data overview, background theory |
| 2 | Nguyễn Huỳnh Việt Hải | Extended discussion, LaTeX typesetting |
| 3 | Nguyễn Hàm Hoàng | Descriptive statistics, extended discussion, LaTeX typesetting |
| 4 | Lâm Đức Anh Khoa | Data preprocessing, ANOVA modeling |
| 5 | Lê Thành Nam | Inferential statistics (one-sample & two-sample tests) |
| 6 | Trần Bình Phương | Multiple linear regression modeling |
| 7 | Trần Phương Trinh | Data overview, background theory |
| 8 | Phạm Minh Trung | Descriptive statistics |

**Instructor:** Msc. Nguyễn Kiều Dung

## References

See the full reference list in the report (`main.pdf`), including course textbooks and standard references such as Devore's *Probability and Statistics for Engineering and the Sciences* and Wooldridge's *Introductory Econometrics*.

## License

This project was created for academic purposes as part of the MT2013 course at Ho Chi Minh City University of Technology (VNU-HCM).