library(readr)
library(dplyr)
library(ggplot2)
library(remotes)
library(DataExplorer)
remotes::install_github("GuangchuangYu/nCov2019")
require(nCov2019)
require(dplyr)
require(ggrepel)
library(hrbrthemes)
library(viridis)
library(lubridate)
library(gganimate)
library(transformr)

library(coronavirus) 

#parameter has to have same country column across all rows
firstBeforeNonZeroDeath = function(df,n){
  #print(paste(n,nrow(df), df[n,5] ))
  if (n > nrow(df)) return (-1)
  if (df[[n,5]] > 0) return (n-1)
  return (firstBeforeNonZeroDeath(df,n+1))
  
}

calculatePerDayVector = function(df,n){
  #print(paste(n,nrow(df), df[n,5] ))
  if (n == 1) return (c(df[[n,5]],calculatePerDayVector(df,n+1)))
  if (n > nrow(df)) return (c())
  if (n > 1) return ( c( df[[n,5]] - df[[n-1,5]], calculatePerDayVector(df,n+1)))
  
  
}

#put intermadiary values (for better animation)
fillTheBlanks = function(df,n, colX=6, colY=7){
  res = df[0,]
  coutries = df$country
  countries = countries[duplicated(countries)]
  
  for (country in countries){
    c.df = df[country==country]
    for (i in 1: nrow(df) ){
      
    }
    
  }
  
  for (i in 1: nrow(df) ){
    newone = df[i,]
    if (i == nrow(df)){
      break
    }
    
    x = df[[i,colX]]
    next_x = fx(df,i+1)
    
    if (next_x == 0) next
    y = df[[i,colY]]
    next_y = df[[i+1,colY]]
    step_x = (next_x - x)/(2*n)
    step_y = (next_y - y)/(2*n)
    
    cols1 = rep(df[[i,1]],times=n)
    
    
    
  }
    
  
  res
}

makePath = function(country){
  path = paste0("/home/fabricio/desenvolvimento/qualificacao/covid/",country,".png")
  path
}

calculateDayVector = function(df){
  lim = nrow(df) -1
  return (0:lim)
}

prepareData <- function(df, countriesVec){
  
  #prepare empty frame
  init = df[0,]

  for (s in countriesVec){
    c.df = df[df$country == s,]
    n = firstBeforeNonZeroDeath(c.df,1)
    nr = nrow(c.df)
    #if (n == -1) next
    c.df = c.df[n:nr,]
    day = calculateDayVector(c.df)
    dead = calculatePerDayVector(c.df,1)
    c.df$day = day
    c.df$dead = dead
    init = rbind(init,c.df)
  }

  return(init)
}



#################### mais ou menos funcional ##################3

x <- get_nCov2019()
y <- load_nCov2019()
y <- load_nCov2019(lang = 'en', source='github')
y <- load_nCov2019(lang = 'en', source='dxy')
x
y



hist.df = y['global',]
hist.df = hist.df[complete.cases(hist.df),]
france.df = hist.df[hist.df$country == "France",]
france.df = france.df[france.df$time >= "2020-02-25",]
france.df[1,5] = 0
hist.df = hist.df[hist.df$country != "France",]

histAux.df = rbind(hist.df,france.df)


day = calculateDayVector(histAux.df) 
dead = calculateDayVector(histAux.df)
histAux.df$day = day
histAux.df$dead = dead

histAux.df = transform(histAux.df, image_path=paste0("/home/fabricio/desenvolvimento/qualificacao/covid/",histAux.df$country,".png"))

cs = c("Italy","Spain","France","United States","United Kingdom", "Brazil")

cs = c("Italy","Spain","France","United States")

cs = c("Italy","Germany","France","United States")

cs = c("Italy","Spain","France","United States", "Germany", "India", "Argentina", "South Korea")
cs = c("Italy","France","United States", "Germany", "India", "Argentina", "South Korea")
cs = c("Spain","United States")
cs = c("Italy","Spain","United States", "Germany",  "China")



new.df = prepareData(histAux.df,cs)

country = c(new.df$country,"Italy","Spain","France","United States", "Brazil","Brazil","Brazil","Brazil","Brazil","Brazil","Brazil","Brazil","Brazil","Brazil","Brazil","Brazil")
day = c(new.df$day,35,24,31,27, 0:11)
dead = c(new.df$dead,919,773,299,400,  0,1,3,2,5,7,7,9,12,11,20,15,22,22,23,42)
new2.df = data.frame(country,day,dead)


new.df %>%
  ggplot( aes(x=day, y=dead, group=country, color=country)) +
  theme_bw() +
  #scale_color_viridis(discrete = TRUE) +
  xlab("Dia (1 é o dia do primeiro registro de morte)") +
  ylab("Mortes (no dia)") +
  geom_line() 

library(ggimage)
#p = 
  new.df %>%
    mutate(image = makePath(country) ) %>%
  ggplot( aes(x=day, y=dead, group=country, color=country)) +
  theme_bw() +
  #scale_color_viridis(discrete = TRUE) +
  xlab("Dia (1 é o dia do primeiro registro de morte)") +
  ylab("Mortes (no dia)") +
  geom_path() +
  geom_point()+
  geom_image(aes(image = image),
             size = 0.3) +
  transition_reveal(day) +
  #view_follow()
  #view_zoom(pause_length = 1, step_length = 2, nsteps = 3)

animate(p, fps = 1)
animate(p, nframes = 300)
anim_save(filename = "anim3.gif" , path = "/home/fabricio/desenvolvimento/qualificacao", fps = 10)

new2.df %>%
  ggplot( aes(x=day, y=dead, group=country, color=country)) +
  theme_bw() +
  #scale_color_viridis(discrete = TRUE) +
  xlab("Dia (1 é o dia do primeiro registro de morte)") +
  ylab("Mortes (no dia)") +
  geom_line() +
  geom_point()+
  transition_states(country, transition_length = 2, state_length = 1) +
  #view_follow()
  view_follow()




brazil.df = histAux.df[hist.df$country == "Brazil",]
italy.df = new.df[new.df$country == "Italy",]
spain.df = new.df[new.df$country == "Spain",]
england.df = new.df[new.df$country == "UK",]
italy.df = italy.df[1:20,]


spaindata <- new.df %>%
  filter(new.df$country == "Spain") %>%
  select(time,cum_dead)


countrydata = spain.df[2:23,]
countrydata <- countrydata %>%
  select(time,dead)
library(forecast)
case <- ts(countrydata[,2], start=1,frequency = 1)
fit <- tslm(log(case) ~ trend)
fc <- forecast(fit, h=10)
summary(fit)
exponential.model <-  lm(log(spain.df$dead)~ spain.df$day)
timevalues <- seq(1, 19, 1)
Counts.exponential2 <- exp(predict(exponential.model,list(Time=timevalues)))
plot(1:19, spain.df$dead,pch=16)
lines(timevalues, Counts.exponential2,lwd=2, col = "red", xlab = "Day (s)", ylab = "Total deaths")


wilcox.test(italy.df$dead, spain.df$dead, paired = TRUE, alternative = "l")
wilcox.test(italy.df$dead, spain.df$dead, paired = TRUE)
mean(italy.df$dead)
mean(spain.df$dead)
nrow(italy.df)
nrow(spain.df)


#countries = hist.df$country
#countries
#countriesU = countries[duplicated(countries)]
#countriesU
