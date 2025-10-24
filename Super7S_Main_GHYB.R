# @author: my@daisg.com
# @date: 2025-10-24
# @version: 0.1
# Copyright (c) 2025 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
## Super 7 star lottery with AI(mlr3)
# 基于分形理论的七星彩预测
library(GH.AN.LIST) # 最新数据获取

# -----------------------------------------
# A：数据准备与任务创建
# -----------------------------------------
# 获取数据：数据 7S_Data。
Super7S_Data <- GH_LIST(1,1000,6)
# str(Super7S_Data) # 数据集的基本信息
# head(Super7S_Data)  # 前几行数据
# sum(is.na(Super7S_Data)) # 检查缺失值

# -----------------------------------------
# B：方法一：直接分形拟合预测函数
# -----------------------------------------
predict_frac_direct <- function(ts, E = 1/15) {
  # 参数: ts - 数值向量，一个位置的历史号码，顺序为从最早期到最近期（ts[1]最旧，ts[n]最新）
  #        E - 能量修正值，默认1/15
  # 返回: 预测的下一个号码（0-9的整数）
  
  n <- length(ts)
  if (n < 2) {
    stop("需要至少两期数据进行分析")
  }
  
  r <- 1:n  # 分形分析中的标度索引r
  F_values <- ts  # 对应位置的号码值F
  
  # 为避免号码0导致对数运算无效，对F值进行微小平移（F+1）
  log_r <- log(r)
  log_F <- log(F_values + 1)
  
  # 在双对数坐标下进行线性拟合，求分形参数
  model <- lm(log_F ~ log_r)
  beta0 <- coef(model)[1]  # 截距项 log(C)
  beta1 <- coef(model)[2]  # 斜率项，与分形维数D相关
  C <- exp(beta0)
  D <- beta1
  
  # 预测下一期的值（r_new = n+1）
  r_new <- n + 1
  # 使用分形公式 F = C * r^D + E 进行预测
  F_pred <- C * (r_new)^D + E
  
  # 将预测值转换回原始尺度（减去1并四舍五入取整），并限制在0-9范围内
  predicted_number <- round(F_pred - 1)
  predicted_number <- max(0, min(9, predicted_number))  # 确保结果在0-9之间
  
  return(predicted_number)
}

# -----------------------------------------
# B：方法二：基于累计积分形预测函数
# -----------------------------------------
predict_frac_cumulative <- function(ts, E = 1/15) {
  n <- length(ts)
  if (n < 2) {
    stop("需要至少两期数据进行分析")
  }
  
  r <- 1:n
  F_values <- ts
  
  # 构造一阶累计积序列 K1 
  K1 <- cumsum(F_values)
  
  # 对累计积序列进行分形拟合
  log_r <- log(r)
  log_K1 <- log(K1 + 1)  # 同样避免对数0
  
  model <- lm(log_K1 ~ log_r)
  beta0 <- coef(model)[1]
  beta1 <- coef(model)[2]
  C_k <- exp(beta0)
  D_k <- beta1
  
  r_new <- n + 1
  # 预测下一期的累计积分形值
  K1_pred <- C_k * (r_new)^D_k + E
  
  # 计算下一期号码的预测值：预测累计和 - 上一期累计和
  F_pred <- K1_pred - K1[n]
  
  predicted_number <- round(F_pred)
  predicted_number <- max(0, min(9, predicted_number))
  
  return(predicted_number)
}

# -----------------------------------------
# C：主分析函数：对七星彩每个位置进行预测
# -----------------------------------------
super7_predict <- function(data, method = "direct") {
  # 参数: data - 数据框，格式如Super7S_Data
  #        method - 预测方法，"direct"（方法一）或 "cumulative"（方法二）
  
  # 确保数据按期号降序排列（最新一期在第一行）
  if (data$ISSUE[1] < data$ISSUE[nrow(data)]) {
    data <- data[order(-data$ISSUE), ]  # 如果期号升序，则反转数据框
  }
  
  predictions <- numeric(7)
  method_name <- ifelse(method == "direct", "直接分形拟合", "累计积分形分析")
  
  cat("使用分形理论进行七星彩预测（方法：", method_name, "）\n")
  cat("数据期数范围：", data$ISSUE[nrow(data)], "至", data$ISSUE[1], "\n")
  cat("各位置预测结果：\n")
  
  for (i in 1:7) {
    col_name <- paste0("QX", i)
    # 提取该位置的历史号码，并反转顺序为从旧到新
    ts_raw <- data[[col_name]]
    ts_old_first <- rev(ts_raw)  # 现在ts_old_first[1]是最早期，ts_old_first[n]是最近期
    
    if (method == "direct") {
      pred <- predict_frac_direct(ts_old_first)
    } else {
      pred <- predict_frac_cumulative(ts_old_first)
    }
    predictions[i] <- pred
    cat("  QX", i, " 预测号码: ", pred, "\n")
  }
  
  return(predictions)
}

# -----------------------------------------
# D：执行预测示例
# -----------------------------------------
if (exists("Super7S_Data")) {
  cat("=== 七星彩分形预测分析 ===\n")
  cat("数据预览：\n")
  print(head(Super7S_Data))
  
  cat("\n--- 方法一：直接分形拟合预测 ---\n")
  pred_direct <- super7_predict(Super7S_Data, method = "direct")
  
  cat("\n--- 方法二：累计积分形预测 ---\n")
  pred_cumulative <- super7_predict(Super7S_Data, method = "cumulative")
  
  cat("\n=== 预测结果汇总 ===\n")
  result_df <- data.frame(
    位置 = paste0("QX", 1:7),
    直接分形预测 = pred_direct,
    累计积分形预测 = pred_cumulative
  )
  print(result_df)
} else {
  cat("请先加载数据。运行：Super7S_Data <- GH_LIST(1, 100, 6)\n")
}