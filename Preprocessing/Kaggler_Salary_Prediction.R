##Preprocessing
## 1. Country - 5개 대륙으로 나눔. done 
## 2. Salary - 결측치 제거하지 않음. 극단적인 이상치만 제거.  done
## 3. Student - 다 넣고 진행함. (salary no selection이 많음.)  
## 4. 변수 내 grouping 필요 - ML 총 5개로 재분류 필요. 
## DS, DA, Others, Software Engineer, Student 
## 복수 응답지 남기고 나머지 제거. done
## Student : 살리기. 
## Confidence NA : 제거할 것. done
## Duration Time : 제거에만 활용. done 

library(ggplot2)
library(scales)
library(gridExtra)
library(grid)
library(corrplot)
library(RColorBrewer)

# data manipulation 
library(dplyr); library(readr); library(data.table)
library(forcats) ## factor manipulation 
library(stringr); library(tidyr); library(tibble)
library(lazyeval); library(broom); library(purrr)
library(reshape2); library(rlang)

#library(ggrepel); library(ggforce); library(ggridges)
library(countrycode); library(geofacet); library(highcharter)
library(plotly)

## code start : 
path <- "D:/project/ML_DS/"

multiple <- read_csv("D:/project/ML_DS/multipleChoiceResponses.csv", na=character()) %>%
  slice(2:n()) %>%
  rename("duration" = `Time from Start to Finish (seconds)`)

multiple2 <- read_csv("D:/project/ML_DS/multipleChoiceResponses.csv", na=character()) %>%
  rename("duration" = `Time from Start to Finish (seconds)`)

names <- as.character(multiple2[1,])

names

write.table(names, file="D:/project/ML_DS/names.txt",
            sep=",", row.names = FALSE)

vars <- c(gender = "Q1",
          country = "Q3", 
          degree = "Q4",
          Role = "Q6", 
          Age = "Q2", 
          undergraduate_major = "Q5", 
          industry = "Q7", 
          Experience = "Q8", 
          value = "Q9",
          confidence = "Q26")

multiple <- multiple %>% rename(!!vars)

multiple$duration <- as.numeric(multiple$duration)

multiple <- multiple %>%
  select(-contains("OTHER_TEXT")) %>%
  select(-contains("Q33")) %>%
  select(-contains("Q39")) %>%
  select(-c("Q17", "Q18","Q20", "Q22", "Q32", "Q37", "Q12_MULTIPLE_CHOICE"))

## Duration : 너무 적은 시간 or 너무 오래 걸린 사람들 제거. 
summary(multiple$duration/60.)

multiple %>% 
  mutate(duration = duration/60.) %>%
  select(c(duration)) %>%
  ggplot(aes(log(duration,10))) + geom_histogram(bins = 60, alpha=0.6, position = "identity") +
  ggtitle("Duration of survey, log_scaled")

## 4000 분(10^3 * 4) 이상 제거, 3분 이하 체거?? 

multiple %>% 
  mutate(duration = duration/60.) %>%
  filter(duration < 4000) %>%
  filter(duration > 4) %>%
  select(c(duration)) %>%
  ggplot(aes(duration)) + geom_histogram(bins = 60, alpha=0.6, position = "identity") +
  ggtitle("Duration of survey")

multiple %>% 
  mutate(duration = duration/60.) %>%
  filter(duration < 4000) %>%
  filter(duration > 4) %>%
  select(c(duration)) %>%
  ggplot(aes(log(duration, 10),)) + geom_histogram(bins = 60, alpha=0.6, position = "identity") +
  ggtitle("Duration of survey, log_scaled, and drop more than 4000 min, less then 4 min")

sort(multiple$duration/60., decreasing = TRUE)[1:150]

sum((multiple$duration/60. > 4000)) ## 263개 
sum((multiple$duration/60. < 4)) ## 2592개 

multiple[multiple$duration/60. < 3, 11:25]
tail(multiple[multiple$duration/60. < 3, 11:25])
## 2592개
## 대체로 NA가 많음.

multiple[multiple$duration/60. < 4, 300:310]
multiple[multiple$duration/60. < 4, 12:25]
## 역시 초반은 괜찮다. 
tail(multiple[multiple$duration/60. < 4, 12:25])
tail(multiple[multiple$duration/60. < 4, 300:310])
tail(multiple[multiple$duration/60. < 4, 210:230])
## No data available 

multiple[multiple$duration/60. > 4000, 11:25]
tail(multiple[multiple$duration/60. > 4000, 11:25])
## 의외로 초반 data는 제대로 만들어져있음. 

multiple[multiple$duration/60. > 4000, 300:310]
tail(multiple[multiple$duration/60. > 4000, c(300:310)])
tail(multiple[multiple$duration/60. > 4000, c(314:330)])

## 3rd Quantile ~ maximum
multiple %>% 
  mutate(duration = duration/60.) %>%
  filter(duration > 4000) %>%
  select(c(duration)) %>%
  ggplot(aes(log(duration,10),)) + geom_histogram(bins = 60, alpha=0.6, position = "identity") +
  ggtitle("Duration of survey")

## 4분 이하, 600분 이상 제거시 ?? 

multiple <- multiple %>%
  filter(duration/60. < 4000) %>%
  filter(duration/60. > 4)

## Confidence 
na_conf <- which(multiple$confidence=="") ## 2093개 

multiple %>% 
  mutate(duration = duration/60., 
         conf_NA = as.factor(ifelse(multiple$confidence == "", 1, 0))) %>%
  select(c(duration, confidence, conf_NA)) %>%
  ggplot(aes(log(duration,10), fill = conf_NA)) + geom_histogram(bins = 60, alpha=0.6,
                                                           position = "identity") +
  ggtitle("Duration of survey for Confidence = NA vs Confidence available duration")
## 1500 분 (25시간) 이상 : strange value. 
## 초반 4~5분 : Strange value. 너무 짧은 시간 내에 응답을 마쳤다. 

multiple_p <- multiple %>% 
  filter(!multiple$confidence=="")

## Salary 이상치 : 

summary(multiple_p$value)
multiple_p$value[multiple_p$value==""] <- "No selection"
multiple_p[1:10, c("value")]

salary_buckets <- c("0-10,000", "10-20,000", "20-30,000", "30-40,000", "40-50,000", 
                    "50-60,000", "60-70,000", "70-80,000", "80-90,000", "90-100,000", 
                    "100-125,000", "125-150,000", "150-200,000", "200-250,000", "250-300,000", 
                    "300-400,000", "400-500,000", "500,000+", 
                    "I do not wish to disclose my approximate yearly compensation", "No selection")

multiple_p$value <- factor(multiple_p$value, levels = salary_buckets)
summary(multiple_p$value)

na_salary <- which(multiple_p$value == "No selection") ## 1435개 

multiple_p %>% 
  mutate(duration = duration/60., 
         salary_NA = as.factor(ifelse(value == "No selection", 1, 0))) %>%
  select(c(duration, value, salary_NA)) %>%
  ggplot(aes(log(duration,10), fill = salary_NA)) + geom_histogram(bins = 60, alpha=0.6,
                                                         position = "identity") +
  ggtitle("Duration of survey for salary = NA vs salary available duration")

## Trend 매우 유사. 제거하기 힘들듯 하다. 아주 이상한 값만 제거. 

summary(multiple_p[na_salary, 'duration'])

## 제거하지 않고 No selection -> do not want to disclose에 합치기. 

multiple_p$value[multiple_p$value=="No selection"] <- "I do not wish to disclose my approximate yearly compensation"
multiple_p$value <- factor(multiple_p$value, levels = salary_buckets[-20])
summary(multiple_p$value)

## 아마 상위 20%로 구간 나눠서 fitting 해야 할지도. 

multiple_p %>% 
  mutate(duration = duration/60., 
         disclose = as.factor(ifelse(value == "I do not wish to disclose my approximate yearly compensation", 1, 0))) %>%
  select(c(duration, value, disclose)) %>%
  ggplot(aes(log(duration, 10), fill = disclose)) + geom_histogram(bins = 60, alpha=0.6,
                                                           position = "identity") +
  ggtitle("Duration of survey for Salary = do not want to disclose vs Salary = Available")


## Country = NA ? 
sum(multiple_p$country=="") #NA 없어짐. 

asia <- c("Bangladesh","China","Hong Kong (S.A.R.)","India","Indonesia","Iran, Islamic Republic of...","Israel",
          "Japan","Malaysia","Pakistan","Philippines","Republic of Korea","Singapore","South Korea","Thailand",
          "Turkey","Viet Nam")
europe <- c("Austria","Belarus","Belgium","Czech Republic","Denmark","Finland","France","Germany","Greece",
            "Hungary","Ireland","Italy","Netherlands","Norway","Poland","Portugal","Romania","Russia","Spain",
            "Sweden","Switzerland","Ukraine","United Kingdom of Great Britain and Northern Ireland")
southamerica <- c("Argentina","Brazil","Chile","Colombia","Peru")
northamerica <- c("Canada","Mexico","United States of America")
oceania <- c("Australia","New Zealand")
africa <- c("Egypt","Kenya","Morocco","Nigeria","South Africa","Tunisia")

asia.idx <- which(multiple_p$country %in% asia)
europe.idx <- which(multiple_p$country %in% europe)
southame.idx <- which(multiple_p$country %in% southamerica)
northame.idx <- which(multiple_p$country %in% northamerica)
oceania.idx <- which(multiple_p$country %in% oceania)
africa.idx <- which(multiple_p$country %in% africa)
region <- rep(NA,nrow(multiple_p))
region[asia.idx] <- "Asia"
region[europe.idx] <- "Europe"
region[southame.idx] <- "SouthAmerica"
region[northame.idx] <- "NorthAmerica"
region[oceania.idx] <- "Oceania"
region[africa.idx] <- "Africa"
multiple_p$country <- factor(region)

table(multiple_p$country)/sum(table(multiple_p$country))

## Student 살리기 : 
## 3. Student - 다 넣고 진행함. (salary no selection이 많음.)  

table(multiple_p$Role); table(multiple_p$industry)

std.id <- unique(c(which(multiple_p$Role == "Student"), which(multiple_p$industry == "I am a student"))) ## 4189
sort(std.id, decreasing = TRUE)[1:10]

table(multiple_p[std.id, "value"])
## total = 4189, 1858 choosed do not want to disclose yearly compensation. 

multiple_p[std.id, "Role"] <- "Student"
multiple_p[std.id, "industry"] <- "Student"

####### Degree = NA 없어짐 
sum(multiple_p$degree=="")
sum(multiple_p$degree=="I prefer not to answer") ## 178개 

## drop I prefer not to answer 
multiple_p %>% 
  select(degree, duration, value, Role, industry) %>%
  filter(degree == "I prefer not to answer") %>%
  group_by(value) %>% count() %>%
  ggplot(aes(x=value, y=n, fill=value)) + geom_bar(stat="identity") + 
  labs(x="", title = "Degree=Do not want to answer people's value")
## 압도적으로 value도 응답 없음이 많다. 제거! 

multiple_p %>% 
  select(degree, duration, value, Role, industry) %>%
  filter(degree == "I prefer not to answer") %>%
  filter(!Role == "Student") %>%
  group_by(value) %>% count() %>%
  ggplot(aes(x=value, y=n, fill=value)) + geom_bar(stat="identity") + 
  labs(x="", title = "Degree=Do not want to answer people's value, without students")
## 초반부 조금 상승, No yearly compensation 변함 없음. 거의 Sofware Engineers. why??

multiple_p %>% 
  select(degree, duration, value, Role, industry) %>%
  filter(degree == "I prefer not to answer") %>%
  group_by(industry) %>% count() %>%
  ggplot(aes(x=industry, y=n, fill=industry)) + geom_bar(stat="identity") + 
  labs(x="", title = "Degree=Do not want to answer people's industry")
## computer/tech가 대부분
## => 신기한 패턴! computer science 쪽의 178명은 자신의 학력을 기재하지 않음. 

multiple_p %>% 
  select(degree, duration, value, Role, industry) %>%
  filter(degree == "I prefer not to answer") %>%
  group_by(Role) %>% count() %>%
  ggplot(aes(x=Role, y=n, fill=Role)) + geom_bar(stat="identity") + 
  labs(x="", title = "Degree=Do not want to answer people's Role/Job")
## 거의 software engineer 

multiple_p %>% 
  select(value) %>%
  group_by(value) %>% count() %>%
  ggplot(aes(x=value, y=n, fill=value)) + geom_bar(stat="identity") + 
  labs(x="", title = "Value dist")

## Total distribution, do not want to disclose가 너무 많음. 

multiple_p <- multiple_p %>%
  filter(!degree == "I prefer not to answer")

## 4. 변수 내 grouping 필요 - ML 총 5개로 재분류 필요. / DS, DA, Software Engineer, Others, Student 
## ## DS, DA, Others, Software Engineer, Student 
## Ds : DS, DA = BA + DA + 
## Software Engineer : SE + DBA/Database Engineer 
## overfitting경계를 위해서 총 5% 미만의 개체 응답 수는 제거. 
## Remove not employed 

table(multiple_p$Role)

# not empolyed 제거
multiple_p <- multiple_p %>%
  filter(!Role == "Not employed")

multiple_p %>%
  select(value, Role) %>%
  count(value, Role) %>% ggplot(aes(x=Role, y = n, fill=value)) +
  geom_bar(stat="identity", position='fill') +
  theme(axis.text.x = element_text(angle=45, hjust=1), legend.position = "none")

## DBA/Database Engineer+Data Engineer summation. 
multiple_p[which(multiple_p$Role == "DBA/Database Engineer"), "Role"] <- 'Data Engineer/DBA'
multiple_p[which(multiple_p$Role == "Data Engineer"), "Role"] <- 'Data Engineer/DBA'

## Statistician + Research Scientist 합치기 
multiple_p[which(multiple_p$Role == "Statistician"), "Role"] <- 'Statistician/Research Scientist'
multiple_p[which(multiple_p$Role == "Research Scientist"), "Role"] <- 'Statistician/Research Scientist'

## Sales person 제거
multiple_p[which(multiple_p$Role == "Salesperson"), "Role"] <- 'Other'

## Data Journalist 제거필요 
c(table(multiple_p[multiple_p$Role == "Marketing Analyst", "value"]), table(multiple_p[multiple_p$Role == "Business Analyst", "value"]))

## Marketing Analyst = others
multiple_p[which(multiple_p$Role == "Marketing Analyst"), "Role"] <- 'Other'

# Data Journalist 
multiple_p <- multiple_p %>%
  filter(!Role == "Data Journalist")

#Developer advocate = other로 
multiple_p[which(multiple_p$Role == "Developer Advocate"), "Role"] <- 'Other'

multiple_p %>%
  select(value, Role) %>%
  count(value, Role) %>% ggplot(aes(x=Role, y = n, fill=value)) +
  geom_bar(stat="identity", position='fill') +
  theme(axis.text.x = element_text(angle=45, hjust=1), legend.position = "none")

table(multiple_p$Role)
table(multiple_p$value)

##Data Analyst + Business Analyst 
multiple_p[which(multiple_p$Role == "Business Analyst"), "Role"] <- 'Data Analyst'
multiple_p[which(multiple_p$Role == "Data Analyst"), "Role"] <- 'Data Analyst'

#Not employed = all i do not want to disclose = 제거 

multiple_p[which(multiple_p$Role == "Consultant"), "Role"] <- 'Consultant/Manager'
multiple_p[which(multiple_p$Role == "Manager"), "Role"] <- 'Consultant/Manager'

## Principal Investigator : 어디 따로 합치기엔 너무 분포가 다름. 그냥 제거해야할듯 
## Research Assistant 역시 그냥 제거? 아님 약간 다를수도 있으니 남기나? 근데 너무 패턴이 다르다. 

multiple_p %>%
  select(value, Role) %>%
  filter(value != "I do not wish to disclose my approximate yearly compensation") %>%
  count(value, Role) %>% ggplot(aes(x=Role, y = n, fill=value)) +
  geom_bar(stat="identity", position='fill') +
  theme(axis.text.x = element_text(angle=45, hjust=1))

## chief oficer : 가장 고액연봉자가 많은 그룹. 지우긴 좀 그렇다. 
## 
multiple_p[which(multiple_p$Role == "Product/Project Manager"), "Role"] <- 'Consultant/Project/Manager'
multiple_p[which(multiple_p$Role == "Consultant/Manager"), "Role"] <- 'Consultant/Project/Manager'

#developer advocate = head hunter? 개발자 영입자. 
tb <- table(multiple_p$Role)/sum(table(multiple_p$Role))
## 5% 미만 개체 전부 out : 
tb
tb[tb > 0.03]

role <- c(names(tb[tb > 0.03]))

multi_test <- multiple_p %>% 
  filter(Role %in% role)
table(multi_test$Role)


multiple_p %>% 
  select(c(value, Role)) %>%
  filter(value == "I do not wish to disclose my approximate yearly compensation") %>%
  group_by(Role) %>% count() %>% 
  ggplot(aes(Role, y = n), fill=Role) + geom_bar(stat="identity")


new <- multiple_p %>% 
  select(c(value, Role)) %>%
  filter(value == "I do not wish to disclose my approximate yearly compensation") 

table(new$Role)/nrow(new)

#https://www.kaggle.com/andresionek/is-there-any-job-out-there-kaggle-vs-glassdoor
## DS, SE, Statistician, Data Analyst, Engineer/DBA, Business Analyst
## Other은 빼야겠다. 
## Student는 일단 넣고 시작. 

## drop other gender 

multi_test <- multi_test %>% 
  filter(!Role %in% c("Prefer not to say", "Prefer to self-describe"))
table(multi_test$Age)

## 1. 28, 29,30 동종업계끼리 묶기

m <- multi_test %>% 
  select(contains("Q28")) 

New = data.frame()
Amazon = c(1, 3, 5, 7, 9, 11, 14)
IBM = c(34, 35, 36, 37, 38, 39, 40)
Azure = c(26, 27, 28, 29, 30, 31, 32, 33, 41)
Google = c(2, 4, 6, 8, 10, 12, 13, 15)

for(i in 1:nrow(m)){
  t = sum(m[i, Amazon] != "")
  m[i, 'Q28_Part_1'] <-  ifelse(t < 1, "", "AWS")
}

for(i in 1:nrow(m)){
  t = sum(m[i, Google] != "")
  m[i, 'Q28_Part_2'] <-  ifelse(t < 1, "", "Google")
}

for(i in 1:nrow(m)){
  t = sum(m[i, IBM] != "")
  m[i, 'Q28_Part_3'] <-  ifelse(t < 1, "", "IBM")
}

for(i in 1:nrow(m)){
  t = sum(m[i, Azure] != "")
  m[i, 'Q28_Part_4'] <-  ifelse(t < 1, "", "Azure")
}

m$Q28_Part_4
m2 <- m %>%
  select(Q28_Part_1, Q28_Part_2, Q28_Part_3, Q28_Part_4)

columns = colnames(m)
columns

multi_test <- multi_test %>%
  select(-colnames(m)[c(Amazon,IBM,Azure,Google)])

multi_test = bind_cols(multi_test, m2)
multi_test[1:4, 310:319]

## 29
m <- multi_test %>% 
  select(contains("Q29")) 

New = data.frame()
Amazon = c(1, 2, 5, 8)
IBM = c(23, 24, 25, 26)
Microsoft = c(9, 15)
Google = c(3, 4, 6, 7, 18)
Azure = c(19, 20, 21, 22)

for(i in 1:nrow(m)){
  t = sum(m[i, Amazon] != "")
  m[i, 'Q29_Part_1'] <-  ifelse(t < 1, "", "AWS")
}

for(i in 1:nrow(m)){
  t = sum(m[i, Google] != "")
  m[i, 'Q29_Part_2'] <-  ifelse(t < 1, "", "Google")
}

for(i in 1:nrow(m)){
  t = sum(m[i, IBM] != "")
  m[i, 'Q29_Part_3'] <-  ifelse(t < 1, "", "IBM")
}

for(i in 1:nrow(m)){
  t = sum(m[i, Azure] != "")
  m[i, 'Q29_Part_4'] <-  ifelse(t < 1, "", "Azure")
}

for(i in 1:nrow(m)){
  t = sum(m[i, Microsoft] != "")
  m[i, 'Q29_Part_5'] <-  ifelse(t < 1, "", "Microsoft")
}

#m$Q29_Part_4

m2 <- m %>%
  select(Q29_Part_1, Q29_Part_2, Q29_Part_3, Q29_Part_4, Q29_Part_5)

columns = colnames(m)
columns

multi_test <- multi_test %>%
  select(-colnames(m)[c(Amazon,IBM,Azure,Google, Microsoft)])

multi_test = bind_cols(multi_test, m2)

## Q30
m <- multi_test %>% 
  select(contains("Q30")) 

New = data.frame()

Google = c(3, 4, 5, 7, 10)
Amazon = c(1, 2, 6, 8, 9)
Azure = c(18, 19, 20)
IBM = c(21, 22, 23)


for(i in 1:nrow(m)){
  t = sum(m[i, Amazon] != "")
  m[i, 'Q30_Part_1'] <-  ifelse(t < 1, "", "AWS")
}

for(i in 1:nrow(m)){
  t = sum(m[i, Google] != "")
  m[i, 'Q30_Part_2'] <-  ifelse(t < 1, "", "Google")
}

for(i in 1:nrow(m)){
  t = sum(m[i, IBM] != "")
  m[i, 'Q30_Part_3'] <-  ifelse(t < 1, "", "IBM")
}

for(i in 1:nrow(m)){
  t = sum(m[i, Azure] != "")
  m[i, 'Q30_Part_4'] <-  ifelse(t < 1, "", "Azure")
}

m2 <- m %>%
  select(Q30_Part_1, Q30_Part_2, Q30_Part_3, Q30_Part_4)

columns = colnames(m)
columns

multi_test <- multi_test %>%
  select(-colnames(m)[c(Amazon,IBM,Azure,Google)])

multi_test = bind_cols(multi_test, m2)

## 2. Confidence = 3 factor로 교체. 

multi_test[multi_test$confidence=="Probably not",'confidence'] <- "Definitely not"
multi_test[multi_test$confidence=="Probably yes",'confidence'] <- "Definitely yes"

table(multi_test$confidence)


## 3. 직무 corrplot 보고 salary에 영향 안끼치는 class끼리 묶기. 


multi_test <- multi_test %>%
  mutate_if(is_character, as.factor)

write_csv(multi_test, "D:/project/ML_DS/multiple3.csv", na="NA", append = FALSE, col_names = TRUE)

