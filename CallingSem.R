
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

EFA <- Raw_df[,c("OI1","OI2","OI3","OI4","OI5","JaC1","JaC2","JaC3","JaC4","JaC5","JaC6","JaC7",
                 "JS1","JS2","JS3","JS4","JS5","JS6","JP1","JP2","JP3","JP4","JP5","JP6_R","JP7_R")]

EFA <- apply((EFA),1,as.numeric)
EFA%>% cor() %>% eigen()

EFA_P<- principal(EFA,rotate = 'none')



names(EFA_P)
EFA_P$values
plot(EFA_P$values,type='b')
abline(a=1,b=0, col='red')
abline(v=20)
ggplot(EFA,aes(x=EFA_P$values))+geom_point()

EFA_s <- scale(EFA)
fac1 <- factanal(EFA_s,4)
printf(digit=2,sort=T)


# 탐색적 요인분석은, 요인 개수가 거의 20개가 나옴
# 이미 학술적으로 요인이 검증되었으므로, 확인적 요인분석을 진행함



# 1-2. 확인적 요인분석 

#잠재변수 지정

#model 설정
HS.Model <- 'Organization_Identity = ~OI1+OI2+OI3+OI4+OI5
            Job_As_Calling= ~JaC1+JaC2+JaC3+JaC4+JaC5+JaC6+JaC7
            Job_Satisfaction= ~JS1+JS2+JS3+JS4+JS5+JS6
            Job_Performance= ~JP1+JP2+JP3+JP4+JP5+JP6_R+JP7_R'

#CFA(확증적 요인분석)

fit <- cfa(HS.Model,data=Raw_df)

#확증적 요인분석 결과 추출

# #lavaan 패키지 버전, 해가 수렴했는지 여부와 반복 횟수, 관측값 개수, 모델추정 방법,
#모델적합도 검정결과(카이제곱 검정통계량, 자유도, p-값).
# 다음 섹션은 Chisq 적합도 지표 이외의 추가적인 적합도 지표
# 마지막 섹션은 모수추정치를 담고 있다. 잠재변수, 공분산, 잔차분산의 순서로 모수추 청치

summary(fit, fit.measures=TRUE, standardized = TRUE)

#모수 추정치 추출 
parameterEstimates(fit, standardized=TRUE)

# 도표로 보기
windows(record=T)
diagram<-semPlot::semPaths(fit,
                           whatLabels="std", intercepts=FALSE, style="lisrel",
                           nCharNodes=0, nCharEdges=0,
                           curveAdjacent = TRUE,title=TRUE, layout="tree2",curvePivot=TRUE)

#요약 테이블로 만들기

library(knitr)
options(knitr.kable.NA="")

parameterEstimates(fit, standardized=TRUE) %>%
   filter(op=="=~") %>%
  mutate(stars=ifelse(pvalue < 0.001, "***",ifelse(pvalue < 0.01, "**",ifelse(pvalue < 0.05, "*", "")))) %>%
  select("Latent Factor"=lhs, Indicator=rhs, B=est, SE=se,
  Z=z, "p-value"=pvalue, Sig.=stars, Beta=std.all) %>%
  kable(digits=3, format="pandoc", caption="Factor Loadings")

residuals(fit,type="cor")$cov




library(semTools)
install.packages('semtools')
