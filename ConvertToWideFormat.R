#1. load data
#2. extract some feature and reshape them to wide format
#averaged --> missing data imputated --> log transformed.
#2.1 extract hour (file name: app.hour.w) (missing value imputated)
#2.2 extract app name (total 106 apps. only 28 are used by more than 10 users, should keep 28 only?)
#2.2.1 file name: app.name.w. (missing value imputated to 0)
#2.2.2 file name: app.name2.w (only the apps which are used by more than 10 users).(missing value imputated to 0)
#2.3 extract app category
#2.3.1 file name: app.cat.w. (missing value imputated to 0)
#2.3.2 file name: app.cat2.w (only the app category which are used by more than 10 users).(missing value imputated to 0)
#3. merge dataframe and data transformation(log)
# 3.1 wave + notification + all app. file name 'app.h' merged with personality.'app.h.p'
# 3.2 wave + notification + mostused app. file name 'app2.h'.merged with personality.'app2.h.p'
# 3.2 wave + notification + all app category. file name 'appcat.h'. merged with personality.'appcat.h.p'
# 3.3 wave + notification + mostused app category. file name 'appcat2.h'. merged with personality.'appcat2.h.p'



library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(readr)
library(readxl)


#l.load data
app <-  read_csv("E:/Data//validUserDataforAnalysis/exported_fromR_apph_final.csv")

#extract number of days each user was recorded. It is used to average the following data, then each user is evened.
appday <- app %>% group_by(d_id,sdate) %>% summarise(n())
appday2 <- appday %>% group_by(d_id) %>% summarise(nb_day = n())
appday2 <- as.data.frame(appday2)

#2.extract some features

#2.1 extract for hour and reshape it to wide format
hour <- app %>% select(c(d_id,shour,tduration))

#define different waves for the activity in the day according to the hour distruction.
hour['wave'] <- NA

mor <- c('07','08','09','10')
lun <- c('11','12','13')
aft <- c('14','15','16','17','18')
eve <- c('19','20','21','22','23')
nig <- c('00','01','02','03','04','05','06')

hour$shour <- as.character(hour$shour)

hour$wave[which(hour$shour %in% mor)] <- 'm'
hour$wave[which(hour$shour %in% lun)] <- 'l'
hour$wave[which(hour$shour %in% aft)] <- 'a'
hour$wave[which(hour$shour %in% eve)] <- 'e'
hour$wave[which(hour$shour %in% nig)] <- 'n'

hour$shour <- as.factor(hour$shour)
hour$wave <- as.factor(hour$wave)

#extract the time each user had in every wave.
app.hour <- hour %>% group_by(d_id,wave) %>%  summarise(timed = sum(tduration)) %>%  arrange(desc(d_id))

app.hour$wave <- as.factor(app.hour$wave)
app.hour$d_id <- as.factor(app.hour$d_id)

barplot(summary(app.hour$wave))

#reshape, convert it from long format to wide format
app.hour <- as.data.frame(app.hour)
app.hour.w1 <- reshape(app.hour, idvar = 'd_id', timevar = 'wave', direction = 'wide')
app.hour.w1 <- as.data.frame(app.hour.w1)

#average the value
app.hour.w2 <- merge(app.hour.w1,appday2, by = 'd_id')

var1.1 <- app.hour.w2

for (i in 2:(ncol(var1.1) - 1)) {
  var1.1[, i] <- var1.1[, i]/var1.1[, ncol(var1.1)]
  
}

app.hour.w3 <- var1.1 %>% select(-c(nb_day))

#impute missing values
#for 'n' (night wave), imputate with 1/5 of the mean. Other waves imputate to mean

var1.2 <- app.hour.w3

for (i in 1:nrow(var1.2)) {
  for (j in 2:5) {
    if (is.na(var1.2[i, j])) {
      var1.2[i, j] <- round(rowMeans(var1.2[i, -1],na.rm = TRUE), 0)
    }
  }
}

for (i in 1:nrow(var1.2)) {
  if (is.na(var1.2[i, 6])) {
    var1.2[i, 6] <- round((rowMeans(var1.2[i, -1],na.rm = TRUE)/5),0)
  }
}

app.hour.w4 <- as.data.frame(var1.2)
colnames(app.hour.w4) <- c('d_id','aft','eve','lun','mor','nig')

#transform data
var1.3 <- app.hour.w4

for (i in 2:ncol(var1.3)) {
  var1.3[,i] <- log(var1.3[,i])
  
} 

app.hour.w <- var1.3

#2.2 extract app name

name <- app %>% select(c(d_id,app_name_en,tduration))

app.name <- name %>% group_by(d_id, app_name_en) %>%  summarise(timeapp = sum(tduration)) %>%  arrange(desc(d_id))

app.name$d_id <- as.factor(app.name$d_id)
app.name$app_name_en <- as.factor(app.name$app_name_en)

# 2.2.1 reshpe it to wide format and imputate missing value to 1, so log(1) = 0
app.name <- as.data.frame(app.name)
app.name.w1 <- reshape(app.name, idvar = 'd_id', timevar = 'app_name_en', direction = 'wide')

#average the value
app.name.w2 <- merge(app.name.w1,appday2, by = 'd_id')

var2.1 <- app.name.w2

for (i in 2:(ncol(var2.1) - 1)) {
  var2.1[,i] <- var2.1[,i]/var2.1[,ncol(var2.1)]
  
}

app.name.w3 <- var2.1 %>% select(-c(nb_day))

#impute missing values
var2.2 <- app.name.w3

for (i in 1:nrow(var2.2)) {
  for (j in 2:ncol(var2.2)) {
    if (is.na(var2.2[i,j])) {
      var2.2[i,j] <- 1
    }
  }
}


for (i in 1:nrow(var2.2)) {
  for (j in 2:ncol(var2.2)) {
    if (var2.2[i,j] < 1) {
      var2.2[i,j] <- 1
    }
  }
}
app.name.w4 <- as.data.frame(var2.2)


#transform data
var2.3 <- app.name.w4

for (i in 2:ncol(var2.3)) {
  var2.3[,i] <- log(var2.3[,i])
  
} 

app.name.w <- var2.3

#2.2.2 only the app which are used by more than 10 users.

app.name.count <- app.name %>% group_by(app_name_en) %>% summarise(number_user = n()) %>% arrange(desc(number_user))

app.name.count <- as.data.frame(app.name.count)

mostused.app <- app.name.count %>% filter(number_user > 9)

barplot(app.name.count$number_user, main = 'App usage barplot by users')

app.name2 <- name %>% group_by(d_id, app_name_en) %>%  summarise(timeapp = sum(tduration)) %>% filter(app_name_en %in% mostused.app$app_name_en)

#reshape it to wide format and imputate missing value to 1, log(1) = 0.
app.name2 <- as.data.frame(app.name2)
app.name2.w1 <- reshape(app.name2, idvar = 'd_id', timevar = 'app_name_en', direction = 'wide')

#average the value
app.name2.w2 <- merge(app.name2.w1,appday2, by = 'd_id')

var3.1 <- app.name2.w2

for (i in 2:(ncol(var3.1) - 1)) {
  var3.1[,i] <- var3.1[,i]/var3.1[,ncol(var3.1)]
  
}

app.name2.w3 <- var3.1 %>% select(-c(nb_day))

#impute missing values
var3.2 <- app.name2.w3

for (i in 1:nrow(var3.2)) {
  for (j in 2:ncol(var3.2)) {
    if (is.na(var3.2[i,j])) {
      var3.2[i,j] <- 1
    }
  }
}


for (i in 1:nrow(var3.2)) {
  for (j in 2:ncol(var3.2)) {
    if (var3.2[i,j] < 1) {
      var3.2[i,j] <- 1
    }
  }
}
app.name2.w4 <- as.data.frame(var3.2)


#transform data
var3.3 <- app.name2.w4

for (i in 2:ncol(var3.3)) {
  var3.3[,i] <- log(var3.3[,i])
  
} 

app.name2.w <- var3.3

# 2.3 extract app catetogry

category <- app %>% select(c(d_id, app_category, tduration))

app.cat <- category %>% group_by(d_id, app_category) %>%  summarise(timecat = sum(tduration)) %>%  arrange(desc(d_id))

app.cat <- as.data.frame(app.cat)

# 2.3.1 reshpe it to wide format and imputate missing value to 1, log(1) = 0
app.cat.w1 <- reshape(app.cat, idvar = 'd_id', timevar = 'app_category', direction = 'wide')

#average the value
app.cat.w2 <- merge(app.cat.w1,appday2, by = 'd_id')

var4.1 <- app.cat.w2

for (i in 2:(ncol(var4.1) - 1)) {
  var4.1[,i] <- var4.1[,i]/var4.1[,ncol(var4.1)]
  
}

app.cat.w3 <- var4.1 %>% select(-c(nb_day))

#when the value is less than 1, change them to 1.
#impute missing values, when it is NA, change them to 1.
var4.2 <- app.cat.w3

for (i in 1:nrow(var4.2)) {
  for (j in 2:ncol(var4.2)) {
    if (is.na(var4.2[i,j])) {
      var4.2[i,j] <- 1
    }
  }
}

for (i in 1:nrow(var4.2)) {
  for (j in 2:ncol(var4.2)) {
    if (var4.2[i,j] < 1) {
      var4.2[i,j] <- 1
    }
  }
}

app.cat.w4 <- as.data.frame(var4.2)


#transform data
var4.3 <- app.cat.w4

for (i in 2:ncol(var4.3)) {
  var4.3[,i] <- log(var4.3[,i])
  
} 

app.cat.w <- var4.3

#2.3.2 app category which are used by more than 10 users.
app.cat.count <- app.cat %>%  group_by(app_category) %>%  summarise(number_user = n())
app.cat.count <- as.data.frame(app.cat.count) %>%  arrange(desc(number_user))

barplot(app.cat.count$number_user, names.arg = app.cat.count$app_category)

mostused.cat <- app.cat.count %>% filter(number_user > 9)

app.cat2 <- category %>% group_by(d_id, app_category) %>% summarise(timecat = sum(tduration)) %>% filter(app_category %in% mostused.cat$app_category)

# reshpe it to wide format and imputate missing value to 1. log(1) = 0.

app.cat2 <- as.data.frame(app.cat2)
app.cat2.w1 <- reshape(app.cat2, idvar = 'd_id', timevar = 'app_category', direction = 'wide')

#average the value
app.cat2.w2 <- merge(app.cat2.w1,appday2, by = 'd_id')

var5.1 <- app.cat2.w2

for (i in 2:(ncol(var5.1) - 1)) {
  var5.1[,i] <- var5.1[,i]/var5.1[,ncol(var5.1)]
  
}

app.cat2.w3 <- var5.1 %>% select(-c(nb_day))

#impute missing values
var5.2 <- app.cat2.w3

for (i in 1:nrow(var5.2)) {
  for (j in 2:ncol(var5.2)) {
    if (is.na(var5.2[i,j])) {
      var5.2[i,j] <- 1
    }
  }
}


for (i in 1:nrow(var5.2)) {
  for (j in 2:ncol(var5.2)) {
    if (var5.2[i,j] < 1) {
      var5.2[i,j] <- 1
    }
  }
}
app.cat2.w4 <- as.data.frame(var5.2)


#transform data
var5.3 <- app.cat2.w4

for (i in 2:ncol(var5.3)) {
  var5.3[,i] <- log(var5.3[,i])
  
} 

app.cat2.w <- var5.3

#3. merge dataframe
#import personality
personality <- read_excel("E:/Data/validUserDataforAnalysis/personality.xls")
personality <- as.data.frame(personality)
personality <- personality %>% select(-c(survey_id, device_id))
personality <- as.data.frame(personality)

#import notification
noti.w <- read_csv("E:/Data/validUserDataforAnalysis/notification_wideformat.csv")
noti.w <- noti.w %>% select(c(d_id,silence,sound,vibrate))
noti.w <- as.data.frame(noti.w)

# 3.1 wave +notification + all app. file name 'app.h' and data transformation
app.h.orgi <- merge(app.hour.w, app.name.w, by = 'd_id')

#extract the one which are in both tables. and the one which are not in noti.w
app.h.both <- merge(app.h.orgi,noti.w, by = 'd_id')
app.h.one <- subset(app.h.orgi, !(app.h.orgi$d_id %in% app.h.both$d_id ))

app.h.one['silence'] <- NA
app.h.one['sound'] <- NA
app.h.one['vibrate'] <- NA

#merge
app.h.merge <- rbind(app.h.both,app.h.one) 

#imputate the missing data
for (i in 1:nrow(app.h.merge)) {
  for (j in 2:ncol(app.h.merge)) {
    if (is.na(app.h.merge[i,j])) {
      app.h.merge[i,j] <- mean(app.h.merge[,j],na.rm = TRUE)
    }
  }
}

app.h <- app.h.merge

app.h.p <- merge(app.h,personality, by = 'd_id')

app.h <- as.data.frame(app.h)
app.h.p <- as.data.frame(app.h.p)

# 3.2 wave +notification  + popular app. file name 'app2.h'
app2.h.orgi <- merge(app.hour.w, app.name2.w, by = 'd_id')

#extract the one which are in both tables. and the one which are not in noti.w
app2.h.both <- merge(app2.h.orgi, noti.w, by = 'd_id')
app2.h.one <- subset(app2.h.orgi, !(app2.h.orgi$d_id %in% app2.h.both$d_id ))

app2.h.one['silence'] <- NA
app2.h.one['sound'] <- NA
app2.h.one['vibrate'] <- NA

#merge
app2.h.merge <- rbind(app2.h.both,app2.h.one) 

#imputate the missing data
for (i in 1:nrow(app2.h.merge)) {
  for (j in 2:ncol(app2.h.merge)) {
    if (is.na(app2.h.merge[i,j])) {
      app2.h.merge[i,j] <- mean(app2.h.merge[,j],na.rm = TRUE)
    }
  }
}

app2.h <- app2.h.merge


app2.h.p <- merge(app2.h, personality, by = 'd_id')

app2.h <- as.data.frame(app2.h)
app2.h.p <- as.data.frame(app2.h.p)

# 3.2 wave +notification  + all app category. file name 'appcat.h'
appcat.h.orgi <- merge(app.hour.w, app.cat.w, by = 'd_id')

#extract the one which are in both tables. and the one which are not in noti.w
appcat.h.both <- merge(appcat.h.orgi,noti.w, by = 'd_id')
appcat.h.one <- subset(appcat.h.orgi, !(appcat.h.orgi$d_id %in% appcat.h.both$d_id ))

appcat.h.one['silence'] <- NA
appcat.h.one['sound'] <- NA
appcat.h.one['vibrate'] <- NA

#merge
appcat.h.merge <- rbind(appcat.h.both,appcat.h.one) 

#imputate the missing data
for (i in 1:nrow(appcat.h.merge)) {
  for (j in 2:ncol(appcat.h.merge)) {
    if (is.na(appcat.h.merge[i,j])) {
      appcat.h.merge[i,j] <- mean(appcat.h.merge[,j],na.rm = TRUE)
    }
  }
}

appcat.h <- appcat.h.merge

appcat.h.p <- merge(appcat.h, personality, by = 'd_id')

appcat.h <- as.data.frame(appcat.h)
appcat.h.p <- as.data.frame(appcat.h.p)

# 3.3wave +n otification  + popular app category. file name 'appcat2.h'
appcat2.h.orgi <- merge(app.hour.w, app.cat2.w, by = 'd_id')


#extract the one which are in both tables. and the one which are not in noti.w
appcat2.h.both <- merge(appcat2.h.orgi, noti.w, by = 'd_id')
appcat2.h.one <- subset(appcat2.h.orgi, !(appcat2.h.orgi$d_id %in% appcat2.h.both$d_id ))

appcat2.h.one['silence'] <- NA
appcat2.h.one['sound'] <- NA
appcat2.h.one['vibrate'] <- NA

#merge
appcat2.h.merge <- rbind(appcat2.h.both,appcat2.h.one) 

#imputate the missing data
for (i in 1:nrow(appcat2.h.merge)) {
  for (j in 2:ncol(appcat2.h.merge)) {
    if (is.na(appcat2.h.merge[i,j])) {
      appcat2.h.merge[i,j] <- mean(appcat2.h.merge[,j],na.rm = TRUE)
    }
  }
}

appcat2.h <- appcat2.h.merge


appcat2.h.p <- merge(appcat2.h, personality, by = 'd_id')

appcat2.h <- as.data.frame(appcat2.h)
appcat2.h.p <- as.data.frame(appcat2.h.p)


#save the converted files
write.csv(app.h, file = "E:\\Data\\validUserDataforAnalysis\\app_wide_format.csv")
write.csv(app.h.p, file = "E:\\Data\\\\validUserDataforAnalysis\\app_personality_wide_format.csv")
write.csv(app2.h, file = "E:\\Data\\validUserDataforAnalysis\\app_popular_wide_format.csv")
write.csv(app2.h.p, file = "E:\\Data\\validUserDataforAnalysis\\app_popular_personality_wide_format.csv")
write.csv(appcat.h, file = "E:\\Data\\validUserDataforAnalysis\\app_category_wide_format.csv")
write.csv(appcat.h.p, file = "E:\\Data\validUserDataforAnalysis\\app_category_personality_wide_format.csv")
write.csv(appcat2.h, file = "E:\\Data\\validUserDataforAnalysis\\app_category_popular_wide_format.csv")
write.csv(appcat2.h.p, file = "E:\\Data\\2017MasterThesisDB\\validUserDataforAnalysis\\app_category_popular_personality_wide_format.csv")