
# 구조방정식에 필요한 패키지 설치
# Web-R에서, 간단하게 사용할 수 있도록 구현해주셨지만, 
# 문건웅님 자료를 참고하여 석사 때 논문을 다시 한번 분석

#install.packages('sem')
#install.packages('lavaan')
#install.packages('semPlot')
#install.packages('ReporteRs')
install.packages('semMediation')
install.packages('r-sem')
#install.packages('dtplyr')
#install.packages('extrafont')
if (!requireNamespace("dbplyr")) install.packages("dbplyr")
library(sem)
library(lavaan)
library(semPlot)
library(ReporteRs)
library(semMediation)
library(extrafont)
library(r-sem)

#### 1.데이터 전처리 ####

# 1)논문 데이터 불러오기 

Raw <- readxl::read_xls('Raw_Data_1107.xls')

saveRDS(Raw,"Raw.RDS")



# 2) 데이터 전처리

library(RSQLite)
library(dplyr)
library(tidyverse)
library(dtplyr)


#### (1) 변수명 지정 ####

class(Raw)
Raw_df <- data.frame(Raw,stringsAsFactors = FALSE)
Raw_df <- Raw_df[,-1:-6]
Raw_df <- Raw_df[-1,]

colnames(Raw_df) <- c("OI1","OI2","OI3","OI4","OI5","JaC1","JaC2","JaC3","JaC4","JaC5",
                      "JaC6","JaC7","JS1","JS2","JS3","JS4","JS5","JS6","JP1","JP2","JP3","JP4","JP5"
                      ,"JP6_R","JP7_R","gender","age","position","Position_ex","Job","Job_ex","edu"
                      ,"edu_ex","year")

Raw_df <- Raw_df[,-29]
Raw_df <- Raw_df[,-30]
Raw_df <- Raw_df[,-31]

head(Raw_df)

#### (2) NA 날리기 ####

# 응답 결과 확인 NA 문항 날리기 
ncol(Raw_df)
# 31개 문항에 대해 
nrow(Raw_df)
# 총 240명의 응답 결과 확인 
sum(is.na(Raw_df))

# 78개의 NA 값을 포함하고 있음, 논문을 쓸때는 demography정보가 없는 
# 질문도 그대로 사용하였으나, 이번에는 엄밀하게 분석하기 위해 모두 날리고 분석을 진행해보겠음

Raw_df <- na.omit(Raw_df)
nrow(Raw_df)
# 총 227명의 응답 결과를 바탕으로 논문에서 진행한 구조방정식을 진행

#### (3) 역문항 역코딩하기 ####
# 역문항은 JP6_R, JP7_R 임, 역코딩 하기 전에 빈도표 생성해서 제대로 바뀌었는지 비교한다. 


table(Raw_df$JP6_R)
# 2   3   4   5 
# 4  18 132  73 
table(Raw_df$JP7_R)
# 2   3   4   5 
# 3  11 137  76 

Raw_df$JP6_R<- as.numeric(Raw_df$JP6_R)
class(Raw_df$JP6_R)
Raw_df$JP6_R <- 5-Raw_df$JP6_R
table(Raw_df$JP6_R)
# 0   1   2   3 
# 73 132  18   4 

Raw_df$JP7_R<- as.numeric(Raw_df$JP7_R)
class(Raw_df$JP7_R)
Raw_df$JP7_R <- 5-Raw_df$JP7_R
table(Raw_df$JP6_R)
# 0   1   2   3 
# 73 132  18   4 

Raw_df$JP6_R<- as.character(Raw_df$JP6_R)
Raw_df$JP7_R<- as.character(Raw_df$JP7_R)

# 1-1. 탐색적 요인분석 
library(psych)
library(GPArotation)
library(mvtnorm)

EFA <- Raw_df[,c("OI1","OI2","OI3","OI4","OI5","JaC1","JaC2","JaC3","JaC4","JaC5","JaC6","JaC7","JS1","JS2","JS3","JS4","JS5","JS6","JP1","JP2","JP3","JP4","JP5","JP6_R","JP7_R")]

EFA <- apply((EFA),1,as.numeric)
EFA%>% cor() %>% eigen()

EFA_P<- principal(EFA,rotate = 'none'
                  
                  )



names(EFA_P)
EFA_P$values
plot(EFA_P$values,type='b')
abline(a=1,b=0, col='red')
abline(v=20)
ggplot(EFA,aes(x=EFA_P$values))+geom_point()

EFA_s <- scale(EFA)

fac1 <- factanal(EFA_s,4)
  
printf(digit=2,sort=T)




# 1-2. 확인적 요인분석 




