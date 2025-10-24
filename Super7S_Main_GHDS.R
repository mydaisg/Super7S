# @author: my@daisg.com
# @date: 2025-10-24
# @version: 0.1
# Copyright (c) 2025 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
## Super 7 star lottery with AI(mlr3)
# 七星彩分形预测程序
## 基于序列自相似性理论

# A：加载必要的包
if(!require("dtw")) install.packages("dtw")
if(!require("zoo")) install.packages("zoo")
if(!require("GH.AN.LIST")) install.packages("GH.AN.LIST")
library(dtw)
library(zoo)
library(GH.AN.LIST)

# B：数据准备
Super7S_Data <- GH_LIST(1,2000,6)
# str(Super7S_Data) # 数据集的基本信息
# head(Super7S_Data)  # 前几行数据
# sum(is.na(Super7S_Data)) # 检查缺失值

# C：分形预测主函数
fractal_super7_prediction <- function(Super7S_Data, 
                                      window_size = 10, 
                                      top_matches = 5,
                                      prediction_length = 1) {
  
  # 数据预处理
  data_preprocessed <- preprocess_data(Super7S_Data)
  
  # 提取数字序列矩阵
  number_matrix <- as.matrix(data_preprocessed[, paste0("QX", 1:7)])
  
  # 为每个位置单独预测
  predictions <- list()
  
  for(position in 1:7) {
    cat("正在预测第", position, "位数字...\n")
    
    # 提取该位置的数字序列
    position_series <- number_matrix[, position]
    
    # 寻找相似片段
    similar_segments <- find_similar_segments(
      series = position_series,
      window_size = window_size,
      top_matches = top_matches
    )
    
    # 基于相似片段进行预测
    position_pred <- predict_from_similar_segments(
      similar_segments = similar_segments,
      current_window = tail(position_series, window_size),
      prediction_length = prediction_length
    )
    
    predictions[[position]] <- position_pred
  }
  
  # 组合预测结果
  final_prediction <- combine_predictions(predictions)
  
  return(final_prediction)
}

# D：数据预处理函数
preprocess_data <- function(data) {
  # 确保数据按期号排序
  if("ISSUE" %in% colnames(data)) {
    data <- data[order(data$ISSUE), ]
  }
  
  # 检查数据完整性
  if(any(is.na(data[, paste0("QX", 1:7)]))) {
    warning("数据中存在缺失值，将进行简单填充")
    data[, paste0("QX", 1:7)] <- na.approx(data[, paste0("QX", 1:7)], rule = 2)
  }
  
  return(data)
}

# E：寻找相似片段函数
find_similar_segments <- function(series, window_size, top_matches) {
  n <- length(series)
  current_window <- tail(series, window_size)
  
  # 存储所有相似片段及其距离
  segments <- list()
  distances <- numeric()
  
  # 滑动窗口寻找相似片段
  for(i in 1:(n - window_size - 1)) {
    test_window <- series[i:(i + window_size - 1)]
    
    # 使用DTW计算序列相似度
    dtw_distance <- dtw(current_window, test_window)$distance
    
    segments[[i]] <- list(
      start_index = i,
      end_index = i + window_size - 1,
      window_data = test_window,
      next_values = series[(i + window_size):min(i + window_size + 2, n)],
      distance = dtw_distance
    )
    distances[i] <- dtw_distance
  }
  
  # 选择最相似的前top_matches个片段
  if(length(distances) > 0) {
    top_indices <- order(distances)[1:min(top_matches, length(distances))]
    best_segments <- segments[top_indices]
    
    return(best_segments)
  } else {
    return(list())
  }
}

# F：基于相似片段进行预测
predict_from_similar_segments <- function(similar_segments, current_window, prediction_length) {
  if(length(similar_segments) == 0) {
    return(rep(NA, prediction_length))
  }
  
  # 收集所有相似片段的后续值
  all_next_values <- numeric()
  weights <- numeric()
  
  for(i in 1:length(similar_segments)) {
    segment <- similar_segments[[i]]
    
    # 使用距离的倒数作为权重（距离越小，权重越大）
    weight <- 1 / (1 + segment$distance)
    
    # 取后续值
    next_vals <- segment$next_values[1:prediction_length]
    
    all_next_values <- c(all_next_values, next_vals)
    weights <- c(weights, rep(weight, length(next_vals)))
  }
  
  # 移除NA值
  valid_indices <- !is.na(all_next_values)
  all_next_values <- all_next_values[valid_indices]
  weights <- weights[valid_indices]
  
  if(length(all_next_values) == 0) {
    return(rep(NA, prediction_length))
  }
  
  # 加权预测
  predictions <- numeric(prediction_length)
  for(i in 1:prediction_length) {
    indices <- seq(i, length(all_next_values), by = prediction_length)
    if(length(indices) > 0) {
      weighted_vals <- all_next_values[indices]
      weighted_weights <- weights[indices]
      
      # 计算加权平均，然后四舍五入到最接近的整数
      predictions[i] <- round(weighted.mean(weighted_vals, weighted_weights))
    }
  }
  
  # 确保预测值在合理范围内（0-9）
  predictions <- pmin(pmax(predictions, 0), 9)
  
  return(predictions)
}

# G：组合各位置预测结果
combine_predictions <- function(predictions) {
  # 提取每个位置的第一期预测
  next_prediction <- sapply(predictions, function(x) {
    if(length(x) > 0 && !is.na(x[1])) x[1] else NA
  })
  
  # 创建结果数据框
  result <- data.frame(
    Position = paste0("QX", 1:7),
    Predicted_Number = next_prediction,
    Confidence = sapply(predictions, function(x) {
      if(length(x) > 0 && !is.na(x[1])) 1 else 0
    })
  )
  
  return(result)
}

# H：模式强度分析函数
analyze_pattern_strength <- function(Super7S_Data, window_size = 8) {
  number_matrix <- as.matrix(Super7S_Data[, paste0("QX", 1:7)])
  pattern_strengths <- numeric(7)
  
  for(position in 1:7) {
    series <- number_matrix[, position]
    current_window <- tail(series, window_size)
    
    # 计算与历史片段的平均相似度
    similarities <- numeric()
    
    for(i in 1:(length(series) - window_size - 1)) {
      test_window <- series[i:(i + window_size - 1)]
      dtw_dist <- dtw(current_window, test_window)$distance
      similarity <- 1 / (1 + dtw_dist)  # 转换为相似度
      similarities <- c(similarities, similarity)
    }
    
    pattern_strengths[position] <- ifelse(length(similarities) > 0, 
                                          mean(similarities, na.rm = TRUE), 0)
  }
  
  return(data.frame(
    Position = paste0("QX", 1:7),
    Pattern_Strength = pattern_strengths,
    Signal_Quality = ifelse(pattern_strengths > 0.15, "强", "弱")
  ))
}

# I：使用示例
if(exists("Super7S_Data")) {
  # 运行预测
  prediction_result <- fractal_super7_prediction(
    Super7S_Data = Super7S_Data,
    window_size = 8,      # 分析窗口大小
    top_matches = 5,      # 使用最相似的5个片段
    prediction_length = 3 # 预测未来3期
  )
  
  # 分析模式强度
  strength_analysis <- analyze_pattern_strength(Super7S_Data)
  
  # 输出结果
  cat("====== 七星彩分形预测结果 ======\n")
  print(prediction_result)
  
  cat("\n======= 模式强度分析 =======\n")
  print(strength_analysis)
  
  cat("\n======== 推荐操作策略 ========\n")
  strong_signals <- sum(strength_analysis$Signal_Quality == "强")
  if(strong_signals >= 4) {
    cat("✅ 强信号模式检测到，建议操作\n")
    cat("推荐号码:", paste(prediction_result$Predicted_Number, collapse = " "))
  } else if(strong_signals >= 2) {
    cat("⚠️  中等信号，建议谨慎操作\n")
    cat("参考号码:", paste(prediction_result$Predicted_Number, collapse = " "))
  } else {
    cat("❌ 弱信号模式，建议观望\n")
  }
  
  # 可视化当前模式
  visualize_patterns(Super7S_Data)
} else {
  cat("请先加载Super7S_Data数据\n")
}

# J：模式可视化函数
visualize_patterns <- function(Super7S_Data, positions = 1:3) {
  number_matrix <- as.matrix(Super7S_Data[, paste0("QX", 1:7)])
  
  par(mfrow = c(length(positions), 1))
  for(pos in positions) {
    series <- number_matrix[, pos]
    plot(series, type = "o", col = "blue", 
         main = paste("第", pos, "位数字序列模式"),
         xlab = "期数", ylab = "数字", 
         ylim = c(0, 9))
    
    # 标记最近5期
    n <- length(series)
    points((n-4):n, series[(n-4):n], col = "red", pch = 19, cex = 1.2)
    
    grid()
  }
  par(mfrow = c(1, 1))
}

# K：策略性能回测函数（简化版）
backtest_strategy <- function(Super7S_Data, window_size = 8, test_periods = 20) {
  number_matrix <- as.matrix(Super7S_Data[, paste0("QX", 1:7)])
  n <- nrow(number_matrix)
  
  hits <- numeric(7)
  attempts <- numeric(7)
  
  # 从后往前回测
  for(test_point in (n-test_periods):(n-1)) {
    train_data <- Super7S_Data[1:test_point, ]
    
    # 使用当前可用的数据进行预测
    current_pred <- fractal_super7_prediction(
      Super7S_Data = train_data,
      window_size = window_size,
      top_matches = 5,
      prediction_length = 1
    )
    
    actual_numbers <- number_matrix[test_point + 1, ]
    predicted_numbers <- current_pred$Predicted_Number
    
    # 计算命中率
    for(pos in 1:7) {
      if(!is.na(predicted_numbers[pos])) {
        attempts[pos] <- attempts[pos] + 1
        if(predicted_numbers[pos] == actual_numbers[pos]) {
          hits[pos] <- hits[pos] + 1
        }
      }
    }
  }
  
  hit_rates <- hits / attempts
  return(data.frame(
    Position = paste0("QX", 1:7),
    Hit_Rate = hit_rates,
    Attempts = attempts
  ))
}

# L：运行回测（可选）
if(exists("Super7S_Data") && nrow(Super7S_Data) > 50) {
  backtest_results <- backtest_strategy(Super7S_Data)
  cat("\n====== 回测结果 ======\n")
  print(backtest_results)
  cat("平均命中率:", mean(backtest_results$Hit_Rate, na.rm = TRUE), "\n")
}


# Super 7 star main 程序说明
# 一. 分形理论实现
# 1、使用DTW（动态时间规整）算法计算序列相似度
# 2、基于历史相似片段进行模式匹配预测
# 3、考虑序列的局部自相似性特征
# 
# 二. 风险管理集成
# 1、模式强度分析自动评估信号质量
# 2、根据信号强度给出操作建议
# 3、内置回测功能验证策略有效性
# 
# 三. 使用建议
# # 调整参数优化策略
# optimized_prediction <- fractal_super7_prediction(
#   Super7S_Data = Super7S_Data,
#   window_size = 12,    # 增大窗口捕捉更长模式
#   top_matches = 8,     # 使用更多相似片段
#   prediction_length = 2 # 预测多期
# )
# 四. 策略执行原则
# 1、只在"强信号"模式下操作
# 2、结合模式强度分析决定投入比例
# 3、定期回测验证策略有效性
# 4、设置严格的止损停止条件
# 
# 这个程序的设计理念是"狙击策略"，只在高质量信号出现时出手，
# 实现了宽哥一直坚持的
# "强信号操作、固定试错比例资金、策略休眠"风险管理原则。