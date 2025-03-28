library(corrr)
library(tidyverse)
library(nycflights13)

colnames(flights)

##### 12.1 CS_1 (NYC Flights)

### Soru 1
# Havada kalış süresi (air_time) en fazla olan uçak için üretim yılı (year),
# üretici (manufacturer) ve model (model) bilgisi nedir?

data1<- left_join(flights,planes,by="tailnum") 

data1 %>% filter(air_time==max(air_time,na.rm = T)) %>%
  select(year.y, manufacturer,model)

# ------------------------------------------------------------------------------

### Soru 2
# Havada kalış süresi (air_time), sıcaklık (temp), rüzgar hızı (wind_speed)
# ve nem (humid) arasında ne tür bir ilişki vardır?

data2<- left_join(weather,flights,by=c("origin","year","month","day","hour"))
data2 %>% select(air_time,temp,wind_speed,humid) %>% 
  correlate() 

## Correlation table.
# # A tibble: 4 × 5
# term       air_time    temp wind_speed   humid
# <chr>         <dbl>   <dbl>      <dbl>   <dbl>
#   1 air_time    NA      -0.0367     0.0263  0.0405
# 2 temp        -0.0367 NA         -0.140   0.0374
# 3 wind_speed   0.0263 -0.140     NA      -0.187 
# 4 humid        0.0405  0.0374    -0.187  NA  

# From this corralation we can say make some assumptins based on our information. 
# For example temp has a negative correlation with air_time, therefor we can say
# say that if the temp increases, the air_time will be shorter. If the temp decreases
# the air_time will be longer. But we have to point out that since the correlation is weak
# we cant say this relationship for sure.

# ------------------------------------------------------------------------------

### Soru 3
# Her bir hava yolu şirketi (name) için ortalama ve ortanca gecikme süresilerini
# (dep_delay) inceleyip yorumlayınız.

data3<- right_join(airlines,flights,by="carrier")
data3 %>% filter(dep_delay>0) %>% na.omit() %>% group_by(name) %>% summarise(mean=mean(dep_delay),median=median(dep_delay))

# ------------------------------------------------------------------------------

### Soru 4
# Flights veri setinde yer alan gecikme süresi (dep_delay) değişkeni için 2013 yılı ortalama gecikme süresi kaçtır?

flights %>% filter(dep_delay>0) %>% summarise(mean=mean(dep_delay))

# ==============================================================================

library(Lahman)
library(tidyverse)

##### 12.2 CS_2 (Sean ‘Lahman’ Baseball Database)
### Soru 1

View(People)

# AwardsPlayers veri setini incelediğinizde en çok ödül alan oyuncu kimdir? 
# (People veri seti nameFirst ve nameLast değişkenleri ile yanıtlayınız.)

t<-table(AwardsPlayers$playerID) %>% as.data.frame()
names(t)<-c("playerID","num.of.aw")
data1<-People %>% left_join(t,by="playerID")    # hepsine na.omit() uygulamak?
data1 %>% filter(num.of.aw==max(num.of.aw,na.rm=T)) %>%   
  select(nameFirst,nameLast)

# ------------------------------------------------------------------------------

### Soru 2 
# People veri setinde yer alan weight ve height değişkenlerini kullanarak body 
# mass index (BMI) hesaplayınız. 

People %>% mutate(BMI=weight/(height^2)*703) %>% filter(BMI>=25 & BMI<29.9) %>% nrow()

# ------------------------------------------------------------------------------

### Soru 3
# Salaries ve AwardsPlayers veri setlerini inceleyiniz, en fazla ödül alan 
# oyuncunun en yüksek maaşa sahip olduğu söylenebilir mi?

#soru 1 de odul sayilari hesaplandi (t)

data2<-Salaries %>% left_join(t,by="playerID") 
which.max(data2$salary)==which.max(data2$num.of.aw)

slice(data2,which.max(data2$salary))     #max salary

slice(data2,which.max(data2$num.of.aw))  #max num of awards

# ------------------------------------------------------------------------------

### Soru 4
# En çok alınan ödül hangisidir?

table(AwardsPlayers$awardID) %>% as.data.frame() %>% arrange(desc(Freq))

# ==============================================================================

library(ggplot2)
library(tidyverse)

##### 12.3 CS_3 (Diamonds)

### Soru 1

diamonds %>% mutate(t=x^2-sqrt(y)+(1/z)) %>% filter(t==min(t)) %>% select(depth)

# ------------------------------------------------------------------------------

### Soru 2

diamonds %>% mutate(discount = case_when(
  cut=="Fair" ~ price*0.01,
  cut=="Good" ~ price*0.02,
  cut=="Very Good" ~ price*0.025,
  cut=="Premium" ~ price*0.03,
  cut=="Ideal" ~ price*0.03,
),new.price=price-discount) %>% filter(color=="E" & clarity=="SI2" & new.price==min(new.price)) 

# ------------------------------------------------------------------------------

### Soru 3

cl<-diamonds$clarity %>% as.factor() # outputs
cl %>% levels() # [1] "I1"   "SI2"  "SI1"  "VS2"  "VS1"  "VVS2" "VVS1" "IF"  

cl %>% nlevels()

# ------------------------------------------------------------------------------

### Soru 4

diamonds %>% group_by(cut) %>% summarise(mean.pr=mean(price))

# or

diamonds %>% filter(cut=="Ideal") %>% summarise(mean(price))


diamonds %>% group_by(cut) %>% summarise(mean.pr=mean(price)) %>%  filter(mean.pr==min(mean.pr)) %>% select(cut,mean.pr)

