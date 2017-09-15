install.packages("plyr")
install.packages("sqldf")
install.packages("Rcpp")
install.packages("ggplot2")
install.packages("ggmap")
install.packages("qmap")
library(ggmap)
library(Rcpp)
library(plyr)
library(sqldf)

###데이터 import###########################################################

data12 <- read.csv("C:/Users/4F/desktop/유동인구_유동인구기본_2012.csv")
data13 <- read.csv("C:/Users/4F/desktop/유동인구_유동인구기본_2013.csv")
data14 <- read.csv("C:/Users/4F/desktop/유동인구_유동인구기본_2014.csv")
data15 <- read.csv("C:/Users/4F/desktop/유동인구_유동인구기본_2015.csv")
position_data <- read.csv("C:/Users/4F/desktop/유동인구_조사지점위치정보_전체_2015_GPS FINAL.csv")

data <- rbind(data12,data13,data14,data15)

###########################################################################


###1시간단위로 조사지역별 평균 도출########################################

mean_data <- ddply(data, .(조사지점코드,조사요일), summarise, avg = mean(유동인구수))
position <- position_data[-c(2,5,6,7)]

###########################################################################


####sql을 이용, 주말 유동인구 데이터만 선택################################
#조사지점&좌표 데이터와 조사지점&유동인구평균 데이터를 merge

weekend<- sqldf::sqldf("select 조사지점코드, 조사요일, avg
                       from mean_data
                       where 조사요일 == '금' or 조사요일 =='토'")
colnames(weekend)<-c("조사지점코드", "조사요일","avg")

weekend<-sqldf::sqldf("select 조사지점코드, 조사요일, avg(avg) as avg
                      from weekend
                      group by 조사지점코드")

colnames(weekend)<-c("조사지점코드", "조사요일","avg")

weekend<- weekend[-c(2)]

move_position <- merge(x = weekend, y = position, by = '조사지점코드')

##########################################################################


####sql 을 이용, 주중 유동인구 데이터만 선택###############################
# 조사지점&좌표 데이터와 조사지점&유동인구평균 데이터를 merge

weekday<-sqldf::sqldf("select 조사지점코드, 조사요일, avg
                      from mean_data
                      where 조사요일 != '금' and 조사요일 !='토'")
colnames(weekday)<-c('조사지점코드', '조사요일','avg')
weekday<- sqldf::sqldf("select 조사지점코드, 조사요일, avg(avg) as avg
                       from weekday
                       group by 조사지점코드")

colnames(weekday)<-c('조사지점코드', '조사요일','avg')

weekday <- weekday[-c(2)]


move_position <- merge(x = weekday, y = position, by = '조사지점코드')

##########################################################################

move_position$avg <- round(move_position$avg,digits=0)
move_position <- move_position[-c(1)]


### RCPP를 이용하여 KMeans 알고리즘을 위한 데이터의 팽창을 빠르게 수행#####
### RCPP를 이용한 CPP 함수의 구현부분

cppFunction('NumericVector f1(NumericVector x, NumericVector y) {
            int n = x.size();
            int cnt = 0;
            for(int i = 0; i < n; i++) cnt += x[i];
            NumericVector ret(cnt);
            for(int i = 0; i < n; i++) for(int j = 0; j < x[i]; j++) ret[--cnt] = y[i];
            return ret;
            }
            ')

############################################################################


### 밀도의 시각화를 위한 데이터의 팽창#########################################
### 데이터를 logscale하여 그 갯수만큼 데이터를 팽창

move_position1 <- move_position
move_position1$avg <- round(log2(move_position1$avg+2),digits=0)
seperated_pos1 <- as.data.frame(t(rbind(f1(move_position1$avg,move_position1$X좌표),f1(move_position1$avg,move_position1$Y좌표))))
colnames(seperated_pos1) <- c('lon','lat')

############################################################################


### 데이터의 팽창, Kmeans 알고리즘의 수행####################################

seperated_pos <- as.data.frame(t(rbind(f1(move_position$avg,move_position$X좌표),f1(move_position$avg,move_position$Y좌표))))
colnames(seperated_pos) <- c('lon','lat')
aa <- kmeans(seperated_pos,100)

############################################################################


### Kmeans알고리즘의 결과 생성된 center와 밀도를 시각화######################

seoulMap<-qmap(location='seoul',zoom=11, maptype="toner-lite", color='bw')
temp <- as.data.frame(aa$centers)
colnames(temp) <- c('lon','lat')
seoulMap + geom_point(data = temp,aes(x = lon,y = lat),color='red') + geom_polygon(data=seperated_pos1, aes(x= lon, y = lat,fill=..level..),stat='density2d',alpha=0.3)

############################################################################


### Kmeans 알고리즘의 평가####################################################
### 평가함수는 sum of squares

clustered_pos <- seperated_pos
clustered_pos <- cbind(aa$cluster,clustered_pos)
clustered_pos <- unique(clustered_pos)
names(clustered_pos)[1] <- c("idx")

center_pos <- aa$centers
idx <- 1:nrow(center_pos)
center_pos <- as.data.frame(cbind(idx,center_pos))
colnames(move_position) <- c('avg','lon','lat')

merged_pos_idx <- merge(x = move_position, y = clustered_pos, by = c('lon','lat'))
merged_pos_center <- merge(x = merged_pos_idx, y = center_pos, by = 'idx')
eval_sum <- sum(merged_pos_center$avg*(sqrt((merged_pos_center$lon.x - merged_pos_center$lon.y)^2 + (merged_pos_center$lat.x - merged_pos_center$lat.y)^2)))

#############################################################################
