library(car)         # 方差齐性检验
library(nortest)     # 正态性检验
library(dplyr)       # 数据处理
library(DT)          # 生成交互式HTML表格
library(htmltools)   # 整合HTML内容
library(rstudioapi)  # 调用Viewer面板

# =====================模拟=====================
# set.seed(123)  # 固定随机种子
# data <- data.frame(
#   group = factor(rep(c("组1", "组2"), each = 30)),  # 每组30个样本
#   value = c(rnorm(30, mean = 5, sd = 1.2),          # 组1：正态分布
#             rnorm(30, mean = 7, sd = 1.5))          # 组2：正态分布
# )
# # ===================== 2. 提取两组数据=====================
# group1 <- data[data$group == levels(data$group)[1], "value"]
# group2 <- data[data$group == levels(data$group)[2], "value"]
# n1 <- length(group1); n2 <- length(group2)



# ===================== 3. 所有正态性检验=====================
normality_test <- function(x, group) {
  # ========== 基础检查：样本量不足报错 ==========
  if (length(x) < 3) {
    stop(paste("分组", group, "样本量<3，无法完成正态性检验！"))
  }
  
  # ========== 运行所有正态性检验（htest对象） ==========
  Shapiro_Wilk <- shapiro.test(x)       # n<50
  ks <- ks.test(x, "pnorm", mean = mean(x), sd = sd(x)) # n>5000
  ks_Lilliefors <- nortest::lillie.test(x)
  ad <- nortest::ad.test(x)
  skewness_test <- moments::agostino.test(x)
  kurtosis_test <- moments::anscombe.test(x)
  JarqueBera <- tseries::jarque.bera.test(x) # Jarque-Bera（偏度+峰度联合检验）
  
  # ========== 整理变量 ==========
  methods = c("Shapiro-Wilk", "Kolmogorov-Smirnov", "Lilliefors (Kolmogorov-Smirnov)", 
              "Anderson-Darling", "D'Agostino skewness", "Anscombe-Glynn kurtosis",
              "Jarque Bera")
  statistic = c(Shapiro_Wilk$statistic, ks$statistic, ks_Lilliefors$statistic,
                ad$statistic, skewness_test$statistic["z"],kurtosis_test$statistic["z"],
                JarqueBera$statistic)
  p_value = c(Shapiro_Wilk$p.value, ks$p.value,ks_Lilliefors$p.value,
              ad$p.value, skewness_test$p.value,kurtosis_test$p.value,
              JarqueBera$p.value)
  data.frame(
    methods = methods,
    group= group,
    n = length(x),
    statistic = statistic,
    p_value = p_value,
    significance =dplyr::case_when(p_value>=0.05 ~ "ns",
                                   p_value>=0.01 ~ "*",
                                   p_value>=0.001 ~ "**",
                                   p_value<0.001 ~ "***",
                                   ),
    normality = ifelse(p_value > 0.05, "Yes", "No")
  )
}

normality_result <- rbind(normality_test(group1, levels(data$group)[1]),
      normality_test(group2, levels(data$group)[2]))


# ===================== 4. 所有方差齐性检验=====================
homogeneity_test <- function(data,formula) {
  f_test <- var.test(formula, ratio = 1)
  
  levene <- car::leveneTest(formula, data = data, center = mean)
  brown_trimmed0.1_mean <-  car::leveneTest(formula, data = data, center=mean, trim=0.1)
  
  brown_median <- car::leveneTest(formula, data = data, center = median)
  brown_forsyth <- HH::hov(formula, data = data)
  fligner <- fligner.test(formula, data = data)
  
  methods = c("F test to compare two variances", 
              "Levene test (mean)", "Levene test (mean trim=0.1)", 
              "Levene test (median)", "Brown-Forsythe",
              "Fligner-Killeen")
  statistic = c(f_test$statistic, levene$`F value`[1],brown_trimmed0.1_mean$`F value`[1],
                brown_median$`F value`[1],brown_forsyth$statistic,fligner$statistic)
  
  p_value = c(f_test$p.value,levene$`Pr(>F)`[1],brown_trimmed0.1_mean$`Pr(>F)`[1],
              brown_median$`Pr(>F)`[1],brown_forsyth$p.value,fligner$p.value)
  
  data.frame(
    methods = methods,
    statistic = statistic,
    p_value = p_value,
    significance =dplyr::case_when(p_value>=0.05 ~ "ns",
                                   p_value>=0.01 ~ "*",
                                   p_value>=0.001 ~ "**",
                                   p_value<0.001 ~ "***",
    ),
    homogeneity =  ifelse(p_value > 0.05, "Yes", "No")
  )
}
homogeneity_result <- homogeneity_test(data,formula = data$value~data$group)

# ===================== 5. 差异比较检验=====================
difference_test <- function(data,formula) {
  t_test_equal <- t.test(formula, data, var.equal = TRUE)
  t_test_unequal <- t.test(formula, data, var.equal = FALSE)
  
  mann_whitney <- wilcox.test(formula, data, exact = NULL,correct = TRUE)
  kruskal <- kruskal.test(formula, data)
  
  
  
  methods = c("Two Sample t-test", "Welch Two Sample t-test", 
              "Wilcoxon rank sum test (Mann-Whitney)",
              "Kruskal-Wallis rank sum test")
  
  # 统一统计量格式（部分检验统计量名称不同，补充标注）
  stats <- c(t_test_equal$statistic, 
             t_test_unequal$statistic,    
             mann_whitney$statistic, 
             kruskal$statistic)
  stats_names <- c("t", "t", "W", "χ²")  # 补充统计量类型

  
  p_value = c(t_test_equal$p.value, t_test_unequal$p.value,
              mann_whitney$p.value, kruskal$p.value)
  data.frame(
    methods = methods,
    
    statistic_type = stats_names,
    statistic = stats,
    p_value = p_value,
    significance =dplyr::case_when(p_value>=0.05 ~ "ns",
                                   p_value>=0.01 ~ "*",
                                   p_value>=0.001 ~ "**",
                                   p_value<0.001 ~ "***",
    ),
    difference =  ifelse(p_value < 0.05, "Yes", "No")
  )
}

difference_result <- difference_test(data,formula)


# ===================== 生成HTML页面并在Viewer面板展示=====================
# 步骤1：将每个结果转换成交互式DT表格（带筛选、排序功能）
tbl_normality <- datatable(normality_result, 
                           caption = htmltools::tags$caption(
                             style = "caption-side: top; font-size: 14px; font-weight: bold;",
                             "表1：正态性检验结果"
                           ),
                           options = list(pageLength = 20, dom = "ltipr")) |> 
  formatStyle(columns = "normality", 
              backgroundColor = styleEqual(c("Yes", "No"), c("#d4edda", "#f8d7da"))) |> 
  formatStyle(columns = "methods", 
              backgroundColor = styleEqual(c("Shapiro-Wilk"), c("#24edda"))) |> 
  formatStyle(columns = "group", 
              backgroundColor = styleEqual(levels(data$group), c("#d4edda", "#f8d7da")))


tbl_homogeneity <- datatable(homogeneity_result, 
                             caption = htmltools::tags$caption(
                               style = "caption-side: top; font-size: 14px; font-weight: bold;",
                               "表2：方差齐性检验结果"
                             ),
                             options = list(pageLength = 10, dom = "ltipr"))  |>  
  formatStyle(columns = "homogeneity", 
              backgroundColor = styleEqual(c("Yes", "No"), c("#d4edda", "#f8d7da")))|> 
  formatStyle(columns = "methods", 
              backgroundColor = styleEqual(c("Levene test (mean)"), c("#24edda")))

tbl_difference <- datatable(difference_result, 
                            caption = htmltools::tags$caption(
                              style = "caption-side: top; font-size: 14px; font-weight: bold;",
                              "表3：差异比较检验结果"
                            ),
                            options = list(pageLength = 5, dom = "ltipr")) %>% 
  formatStyle(columns = "difference", 
              backgroundColor = styleEqual(c("Yes", "No"), c("#f8d7da", "#d4edda")))


# 步骤2：整合所有表格到一个HTML页面
html_content <- tagList(
  tags$h2("两独立样本比较完整检验报告"),
  tags$hr(),
  tbl_normality,
  tags$hr(),
  tbl_homogeneity,
  tags$hr(),
  tbl_difference
)

save_html(html_content, file = "TwoSampleTest.html")        # 保存整合后的HTML内容
viewer(url = "TwoSampleTest.html")                         # 在RStudio Viewer面板打开
