library(magrittr)
library(dplyr)
library(lubridate)
library(FSA)
library(DescTools)
library(ggplot2)
library(ggthemes)
library(corrplot)
library(RColorBrewer)


DATA <- read.csv("StudentsPerformance.csv")
DATA <- DATA %>% rename(race = race.ethnicity, education = parental.level.of.education, test = test.preparation.course, math = math.score, reading = reading.score, writing = writing.score)
# genero para math
A = DATA %$% Summarize(math~gender, digits = 2, na.rm = T,percZero="always")
B = DATA %$% tapply(math,gender,FUN = function(x) round(mean(Mode(x)),0)) %>% cbind(.) 
genre_math = A %>% left_join(data.frame(gender = row.names(B), mode = as.numeric(B)),  by = "gender")
# race para math
A = DATA %$% Summarize(math~race, digits = 2, na.rm = T,percZero="always")
B = DATA %$% tapply(math,race,FUN = function(x) round(mean(Mode(x,na.rm = T)),0)) %>% cbind(.) 
race_math = A %>% left_join(data.frame(race = row.names(B), mode = as.numeric(B)),  by = "race")
# education para math
A = DATA %$% Summarize(math~education, digits = 2, na.rm = T,percZero="always")
B = DATA %$% tapply(math,education,FUN = function(x) round(mean(Mode(x,na.rm = T)),0)) %>% cbind(.) 
education_math = A %>% left_join(data.frame(education = row.names(B), mode = as.numeric(B)),  by = "education")
# lunch para math
A = DATA %$% Summarize(math~lunch, digits = 2, na.rm = T,percZero="always")
B = DATA %$% tapply(math,lunch,FUN = function(x) round(mean(Mode(x,na.rm = T)),0)) %>% cbind(.) 
lunch_math = A %>% left_join(data.frame(lunch = row.names(B), mode = as.numeric(B)),  by = "lunch")
# test para math
A = DATA %$% Summarize(math~test, digits = 2, na.rm = T,percZero="always")
B = DATA %$% tapply(math,test,FUN = function(x) round(mean(Mode(x,na.rm = T)),0)) %>% cbind(.) 
test_math = A %>% left_join(data.frame(test = row.names(B), mode = as.numeric(B)),  by = "test")

# genero para reading
A = DATA %$% Summarize(reading~gender, digits = 2, na.rm = T,percZero="always")
B = DATA %$% tapply(reading,gender,FUN = function(x) round(mean(Mode(x)),0)) %>% cbind(.) 
genre_reading = A %>% left_join(data.frame(gender = row.names(B), mode = as.numeric(B)),  by = "gender")
# race para reading
A = DATA %$% Summarize(reading~race, digits = 2, na.rm = T,percZero="always")
B = DATA %$% tapply(reading,race,FUN = function(x) round(mean(Mode(x,na.rm = T)),0)) %>% cbind(.) 
race_reading = A %>% left_join(data.frame(race = row.names(B), mode = as.numeric(B)),  by = "race")
# education para reading
A = DATA %$% Summarize(reading~education, digits = 2, na.rm = T,percZero="always")
B = DATA %$% tapply(reading,education,FUN = function(x) round(mean(Mode(x,na.rm = T)),0)) %>% cbind(.) 
education_reading = A %>% left_join(data.frame(education = row.names(B), mode = as.numeric(B)),  by = "education")
# lunch para reading
A = DATA %$% Summarize(reading~lunch, digits = 2, na.rm = T,percZero="always")
B = DATA %$% tapply(reading,lunch,FUN = function(x) round(mean(Mode(x,na.rm = T)),0)) %>% cbind(.) 
lunch_reading = A %>% left_join(data.frame(lunch = row.names(B), mode = as.numeric(B)),  by = "lunch")
# test para reading
A = DATA %$% Summarize(reading~test, digits = 2, na.rm = T,percZero="always")
B = DATA %$% tapply(reading,test,FUN = function(x) round(mean(Mode(x,na.rm = T)),0)) %>% cbind(.) 
test_reading = A %>% left_join(data.frame(test = row.names(B), mode = as.numeric(B)),  by = "test")

# genero para writing
A = DATA %$% Summarize(writing~gender, digits = 2, na.rm = T,percZero="always")
B = DATA %$% tapply(writing,gender,FUN = function(x) round(mean(Mode(x)),0)) %>% cbind(.) 
genre_writing = A %>% left_join(data.frame(gender = row.names(B), mode = as.numeric(B)),  by = "gender")
# race para writing
A = DATA %$% Summarize(writing~race, digits = 2, na.rm = T,percZero="always")
B = DATA %$% tapply(writing,race,FUN = function(x) round(mean(Mode(x,na.rm = T)),0)) %>% cbind(.) 
race_writing = A %>% left_join(data.frame(race = row.names(B), mode = as.numeric(B)),  by = "race")
# education para writing
A = DATA %$% Summarize(writing~education, digits = 2, na.rm = T,percZero="always")
B = DATA %$% tapply(writing,education,FUN = function(x) round(mean(Mode(x,na.rm = T)),0)) %>% cbind(.) 
education_writing = A %>% left_join(data.frame(education = row.names(B), mode = as.numeric(B)),  by = "education")
# lunch para writing
A = DATA %$% Summarize(writing~lunch, digits = 2, na.rm = T,percZero="always")
B = DATA %$% tapply(writing,lunch,FUN = function(x) round(mean(Mode(x,na.rm = T)),0)) %>% cbind(.) 
lunch_writing = A %>% left_join(data.frame(lunch = row.names(B), mode = as.numeric(B)),  by = "lunch")
# test para writing
A = DATA %$% Summarize(writing~test, digits = 2, na.rm = T,percZero="always")
B = DATA %$% tapply(writing,test,FUN = function(x) round(mean(Mode(x,na.rm = T)),0)) %>% cbind(.) 
test_writing = A %>% left_join(data.frame(test = row.names(B), mode = as.numeric(B)),  by = "test")

# Graficos

ggplot(DATA, aes(x = gender, y = math)) +
  geom_boxplot(outlier.shape = NA, aes(fill=gender))+
  geom_point(position = "jitter") + theme_bw() +
  labs(title="Diagrama de Cajas", subtitle = "Género ~ Matemáticas", x = "",y = "Matemáticas") +
  scale_fill_brewer(palette = "Set2",guide=FALSE)

ggplot(DATA, aes(x = race, y = math)) +
  geom_boxplot(outlier.shape = NA, aes(fill=race))+
  geom_point(position = "jitter") + theme_bw() +
  labs(title="Diagrama de Cajas", subtitle = "Raza Étnica ~ Matemáticas", x = "",y = "Matemáticas") +
  scale_fill_brewer(palette = "Set2",guide=FALSE)

ggplot(DATA, aes(x = education, y = math)) +
  geom_boxplot(outlier.shape = NA, aes(fill=education))+
  geom_point(position = "jitter") + theme_bw() +
  labs(title="Diagrama de Cajas", subtitle = "Nivel Educación ~ Matemáticas", x = "",y = "Matemáticas") +
  scale_fill_brewer(palette = "Set2",guide=FALSE)

ggplot(DATA, aes(x = lunch, y = math)) +
  geom_boxplot(outlier.shape = NA, aes(fill=lunch))+
  geom_point(position = "jitter") + theme_bw() +
  labs(title="Diagrama de Cajas", subtitle = "Lunch ~ Matemáticas", x = "",y = "Matemáticas") +
  scale_fill_brewer(palette = "Set2",guide=FALSE)


cor = DATA %>% select(math, reading, writing) %>% cor
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))(10)
corrplot(cor,diag = F,type = "upper", method="number", tl.srt=45, sig.level = 0.01, insig = "blank", col = col, cl.cex = 0.9, number.cex = 1.5,mar =c(0,0,1,0),cl.ratio = 0.5 )


ggplot(DATA , aes(x= math, y= reading, colour = gender))+
  geom_point() +
  geom_smooth(method = "loess",formula = y ~ x, span = 0.8) + 
  facet_wrap(~race) + 
  theme_bw() + 
  labs(title="Diagrama de dispersión", subtitle = "Lectura ~ Matemáticas para el Género y la raza Étnica") +
  scale_colour_manual(values=brewer.pal(n = 2,name = "Set2"))

ggplot(DATA , aes(x= writing, y= reading, colour = lunch))+ 
  geom_point() +
  geom_smooth(method = "loess",formula = y ~ x, span = 0.8) + 
  facet_wrap(~education) + 
  theme_bw() +
  labs(title="Diagrama de dispersión", subtitle = "Reading ~ escritura para el Nivel de Educación y el Lunch") +
  scale_colour_manual(values=brewer.pal(n = 2,name = "Set2"))

ggplot(DATA, aes(x=math, color=gender, fill=gender)) + 
  geom_histogram(aes(y=..density..), alpha=0.5,binwidth = 5, position="identity")+
  geom_density(alpha=.2) +  
  theme_bw() +
  labs(title="Diagrama de Densidad", subtitle = "Matemáticas ~ Género", y = "Densidad") + 
  scale_fill_manual(values=brewer.pal(n = 2,name = "Set2"))

ggplot(DATA, aes(x=reading, color=gender, fill=gender)) + 
  geom_histogram(aes(y=..density..), alpha=0.5,binwidth = 5, position="identity")+
  geom_density(alpha=.2) + 
  facet_wrap(~race) + 
  theme_bw() +
  labs(title="Diagrama de Densidad", subtitle = "Lectura ~ Género por Raza Étnica", y = "Densidad") + 
  scale_fill_manual(values=brewer.pal(n = 3,name = "Set2"))

ggplot(DATA, aes(x=writing, color=gender, fill=gender)) + 
  geom_histogram(aes(y=..density..), alpha=0.5,binwidth = 5, position="identity")+
  geom_density(alpha=.2) +  
  theme_bw() +
  facet_wrap(~education) + 
  labs(title="Diagrama de Densidad", subtitle = "Escritura ~ Género por Nivel de Educación", y = "Densidad") + 
  scale_fill_manual(values=brewer.pal(n = 3,name = "Set2"))

########################################################################################################################################################

chisq.test(table(DATA$gender,DATA$education))
chisq.test(table(DATA$gender,DATA$race))

shapiro.test(DATA$math)
shapiro.test(DATA$reading)
shapiro.test(DATA$writing)

chisq.test(table(DATA$gender))

library(lmtest)
model=lm(math ~ reading + writing, data = DATA)
coeftest(model)

