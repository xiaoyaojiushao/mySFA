rm(list=ls())

library(readxl)
library(frontier)
library(plm)

## 载入frontier包自带的水稻数据集
# data(riceProdPhil)

## 载入数据集合
#data1 <- read.csv("ybysbForSFA.csv")
#data1 <- read.csv("E:/论文Papers/国家社科/代码和数据/SFAfinalData20200803.csv",
#                  encoding = "UTF-8")
data1 <- read_excel('E:/论文Papers/国家社科/代码和数据/SFAfinalData20200803.xlsx', sheet = 'SFAfinalData20200803', col_names = TRUE)

## 转化成面板数据
# riceProdPhil <- pdata.frame(riceProdPhil, index=c("FMERCODE", "YEARDUM"))
data1 <- pdata.frame(data1, index=c("code", "year"))

data1$fix2 = log(data1$fix)^2
data1$pay2 = log(data1$pay)^2
data1$fixpay = log(data1$pay)*log(data1$fix)
## 估计模型
#知乎https://zhuanlan.zhihu.com/p/66834148       
result <- sfa(log(pp_result) ~ log(fix)+log(pay)+ fix2 + pay2 + fixpay  |    #这条线后面写上变量z
                #                x1+x2+x3+x4+x5+x6,
                open + protect + compete + fluid + manage,
                #log(open) + log(protect) + log(compete) + log(fluid) + log(manage), # 用户主受教育年限和分类土地面积中高地所占百分比当z
              data = data1)

#result <- sfa(log(quality) ~ log(invest)+log(money),
#              data = data1,
#              truncNorm = TRUE, # 提示函数使用截断正态分布假设
#              timeEffect = TRUE) 

## 打印估计结果
print(summary(result))

## 求效率
a=efficiencies(result)

##似然比检验
lrtest(result)
