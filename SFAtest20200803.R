rm(list=ls())

library(readxl)
library(frontier)
library(plm)

## ����frontier���Դ���ˮ�����ݼ�
# data(riceProdPhil)

## �������ݼ���
#data1 <- read.csv("ybysbForSFA.csv")
#data1 <- read.csv("E:/����Papers/�������/���������/SFAfinalData20200803.csv",
#                  encoding = "UTF-8")
data1 <- read_excel('E:/����Papers/�������/���������/SFAfinalData20200803.xlsx', sheet = 'SFAfinalData20200803', col_names = TRUE)

## ת�����������
# riceProdPhil <- pdata.frame(riceProdPhil, index=c("FMERCODE", "YEARDUM"))
data1 <- pdata.frame(data1, index=c("code", "year"))

data1$fix2 = log(data1$fix)^2
data1$pay2 = log(data1$pay)^2
data1$fixpay = log(data1$pay)*log(data1$fix)
## ����ģ��
#֪��https://zhuanlan.zhihu.com/p/66834148       
result <- sfa(log(pp_result) ~ log(fix)+log(pay)+ fix2 + pay2 + fixpay  |    #�����ߺ���д�ϱ���z
                #                x1+x2+x3+x4+x5+x6,
                open + protect + compete + fluid + manage,
                #log(open) + log(protect) + log(compete) + log(fluid) + log(manage), # �û����ܽ������޺ͷ�����������иߵ���ռ�ٷֱȵ�z
              data = data1)

#result <- sfa(log(quality) ~ log(invest)+log(money),
#              data = data1,
#              truncNorm = TRUE, # ��ʾ����ʹ�ýض���̬�ֲ�����
#              timeEffect = TRUE) 

## ��ӡ���ƽ��
print(summary(result))

## ��Ч��
a=efficiencies(result)

##��Ȼ�ȼ���
lrtest(result)