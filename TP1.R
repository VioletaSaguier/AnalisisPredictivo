install.packages("funModeling")
install.packages(data.table)
install.packages("tree")

library(funModeling)
library(dplyr)
library(tibble)
library(hms)
library(stringr)
library(tidyverse)
library(plyr)
library("mlbench")
library("caret")
library(rpart)
library(rpart.plot)
library(rfm)
library(ggplot2)

library(MASS)
library(tidyr)
library(skimr)
library(ggpubr)
library(tree)


library(data.table)


#IMPORTAR Y PREPARA VARIABLES
df=read_csv("/Users/violetasaguier/cinemaTicket_Ref.csv")


summary(df)
glimpse(df)
df_status(df)

head(df, 5)

--------- --- # limpieza en variables 
  
# cambio tipo de dato
df$film_code <- as.factor(df$film_code)

# cambio tipo de dato
df$cinema_code <- as.factor(df$cinema_code)

# pasar capacity a integer
df$capacity <- as.integer(df$capacity)

# pasar porcentaje a valor sobre 1
df$occu_perc <- (df$occu_perc/100)

# pasar tickets_sold a integer
df$tickets_sold <- as.integer(df$tickets_sold)

# pasar tickets_out a integer
df$tickets_out <- as.integer(df$tickets_out)

# pasar ticket use a integer
df$ticket_use <- as.integer(df$ticket_use)

# pasar month a integer
df$month <- as.factor(df$month)

# pasar day a integer
df$day <- as.factor(df$day)

#crear variable weekday
df$weekday <- lubridate::wday(df$date, label=TRUE)

 -----------------

#ANALIZAR NAs,MISSINGS 
summary(df)
df_status(df)
glimpse(df)

head(df, 5)
# pocos Nas, capacity y occu perc, no más de 9%.
map_dbl(df, function(x) mean(is.na(x)) * 100) %>% 
  sort(decreasing=T) %>% 
  as.data.frame()

# filtrado de Nas capacity occu perc --> asocio a una fala en la carga. No coincide cine pelicula o fecha. Tambien NA en capacity
# df = df %>% filter(is.na(capacity))
df %>% filter(is.na(occu_perc))
df %>% filter(capacity<0) # 54 valores <0, luego los paso a positivos.
df %>% filter(is.na(capacity))
df %>% filter(is.na(capacity), is.na(occu_perc))
df %>% filter(occu_perc<0) # 0 occ neg
df %>% filter(occu_perc>1) # hay, pocas. Luego las cambio.
df %>% filter(df$capacity >= 0,) #45 capacity<0
df %>% filter(capacity==0) # 0 capacity ==0
df %>% filter(ticket_use<0) # 52 ticket_use negativos. Es inconsistente si consideramos que la plata no se devuelve (de acuerdo a total sales)
df %>% filter(ticket_use==0)
df %>% filter(total_sales<0)
df %>% filter(tickets_out>0)

df %>% filter(tickets_out>=ticket_use)

df %>% filter(cinema_code==637 && !is.na(occu_perc)) # son del cine 637
df %>% filter(film_code==1448 && !is.na(occu_perc)) # son de pelicula 1484

---- #mas modificaciones

#elimino tickets use <0
df = df[df$ticket_use >= 0,]

#elimino occu_perc > 1, no tendría sentido y no aporta al análisis. 
df$occu_perc <-ifelse(df$occu_perc>1, df$occu_perc-1, df$occu_perc)

#paso capacity a positivo
df$capacity<-as.integer(ifelse(df$capacity<0, df$capacity*(-1), df$capacity))

#creo var success 
df$success <- ifelse(df$occu_perc>0.2, "YES", "NO")

df$ticket_price=df$ticket_price/100

#elimino show time y quarter
df <- subset(df, select= -show_time)
df <- subset(df, select= -quarter)

head(df, 5)

-------
df_status(df)
glimpse(df)
summary(df)

#dc = subset(df, select = c("film_code", "cinema_code", "total_sales", "tickets_sold", "tickets_out", "show_time", "occu_perc", "ticket_price", "ticket_use", "capacity", "date", "month", "quarter", "day", "Weekday", "Year"))
# PONERLE TODAS LAS NUMERICAS dc <- dc %>% dplyr::group_by (date, Year, cinema_code,film_code, Weekday,) %>% dplyr::summarise(capacity=sum(capacity), ticket_price=mean(ticket_price), ticket_use=sum(Amount))
#summary(dc)

dc <- df %>% dplyr::group_by (film_code, date, cinema_code, weekday, month, day) %>% dplyr::summarise(total_sales=sum(total_sales), ticket_price=mean(ticket_price), tickets_sold=sum(tickets_sold))
------------

#EDA
#outliers 
boxplot(df$ticket_price,main = "Boxplot Price")
boxplot(df$capacity,main = "Boxplot Capacity")
boxplot(df$tickets_sold, main = "Boxplot Tickets sold")
boxplot(df$tickets_out,main = "Boxplot Tickets out")

ggplot(df) +geom_density(aes(x=ticket_price),color="darkblue", fill="lightblue", alpha=0.7) +scale_y_log10()+scale_x_log10()+theme_minimal()
ggplot(df_num) +geom_density(aes(x=capacity), color="darkblue", fill="lightblue", alpha=0.7) +scale_y_log10()+scale_x_log10() +theme_minimal()
ggplot(df_num) +geom_density(aes(x=tickets_sold), color="darkblue", fill="lightblue", alpha=0.7)+scale_y_log10()+scale_x_log10() +theme_minimal() 
ggplot(df_num) +geom_density(aes(x=tickets_out), color="darkblue", fill="lightblue", alpha=0.7) +scale_y_log10()+scale_x_log10() +theme_minimal()

#distribuciones asimetricas
-----------------
  
dr=df
#eliminamos success y capacity NAs
dr = dr[!is.na(dr$success),]
dr = dr[!is.na(dr$capacity),]

# total de 0 NAs
map_dbl(dr, function(x) mean(is.na(x)) * 100) %>% 
  sort(decreasing=T) %>% 
  as.data.frame()

ggplot(dr, aes(x=tickets_sold, y=ticket_price, color=success)) +geom_point(size=2, alpha=0.2)+scale_y_log10()+scale_x_log10()+ggtitle("Sold vs. Price") 
ggplot(dr, aes(x=tickets_out, y=ticket_price, color=success)) +geom_point(size=2, alpha=0.2)+scale_y_log10()+scale_x_log10()+ggtitle("Out vs. Price") 
ggplot(dr, aes(x=capacity, y=ticket_price, color=success)) +geom_point(size=2, alpha=0.2)+scale_y_log10()+scale_x_log10()+ggtitle("Capacity vs. Price")


---------------------------
  
# separacion variables numericas y categoricas
dr_num = dr %>% select_if(is.numeric(dr))
dr_cat = dr %>% select_if(function(x) !is.numeric(x))
  
#correlacion con pearson
GGally::ggcorr(
  df_num, method=c("pairwise","pearson"), 
    label=T, hjust=1, label_size=2, layout.exp=10, size=3)

#correlacion con spearman, mas robusto ante outliers.
GGally::ggcorr(
  df_num, method=c("pairwise","spearman"),  
  label=T, hjust=1, label_size=2, layout.exp=10, size=3)

----

#cantidad de transacciones por dia de la semana
table(df_cat$weekday)
barplot(table(df_cat$weekday))
barplot(table(df_cat$month))
ggplot(data = dr, mapping = aes(x = ticket_price)) + geom_freqpoly(mapping = aes(colour = "red"), binwidth = 500, alpha=1)


barplot(table(df_cat$success))

#correlacion categoricas
t1 = table(dr$day, dr$weekday)
as.matrix(t1) %>% rstatix::cramer_v()

t2 = table(dr$film_code, dr$weekday)
as.matrix(t2) %>% rstatix::cramer_v()

t3 = table(dr$cinema_code, dr$success)
as.matrix(t3) %>% rstatix::cramer_v()

t4 = table(df$cinema_code, df$film_code)
as.matrix(t4) %>% rstatix::cramer_v()

 
prop.table(table(dr$weekday, dr$success))


#PARTICION EN TRAIN Y TEST
entreno=dr[dr$date<="2018-09-03",]
testeo=dr[dr$date>"2018-09-03",]

 -----

arbol <- rpart(
    formula = tickets_sold ~ capacity, ticket_price, 
    data    = entreno, # Dataset
    method  = "anova" # Anova para especificar que es un arbol de regresión
  )

rpart.plot(arbol,extra=1,type=5)

#prueba sobre ek arbol de decision
pred=predict(arbol,testeo, type="vector") 
confusionMatrix(pred,testeo$tickets_sold)



