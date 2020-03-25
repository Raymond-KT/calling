
# 구조방정식에 필요한 패키지 설치
# Web-R에서, 간단하게 사용할 수 있도록 구현해주셨지만, 
# 문건웅님 자료를 참고하여석사 때 논문을 다시 한번 분석

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

# 2) 데이터 전처리

library(RSQLite)
library(dplyr)
library(tidyverse)
library(dtplyr)


# Raw Data를 DB화 / 데이터 수는 적지만, 연습을 위해 시행

calling <- dbConnect(SQLite())

copy_to(calling, 
        Raw, 
        temporary=FALSE, 
        name='calling'
        )

dbListTables(calling)

# 테이블 연결정보를 R 객체에 저장 

tb_calling <- tbl(calling, "calling")
indexes = list("응답자ID")
tb_calling_idx <- tbl(calling, "응답자ID")

View(tb_calling)



Raw_df <- data.frame(Raw,stringsAsFactors = FALSE)

head(Raw_df)

# 번호 재지정



