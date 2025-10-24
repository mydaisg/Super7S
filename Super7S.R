# @author: my@daisg.com
# @date: 2025-10-24
# @version: 0.1
# Copyright (c) 2025 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
## Super 7 star lottery with AI(mlr3)
library(GH.AN.LIST) # 最新数据获取

# ---------------------------
# A：数据准备与任务创建
# ---------------------------
# 获取数据：数据 7S_Data。
Super7S_Data <- GH_LIST(1,100,6)
str(Super7S_Data) # 数据集的基本信息
head(Super7S_Data)  # 前几行数据
sum(is.na(Super7S_Data)) # 检查缺失值

