#############################
# Originally: 5Z Cod, expanded to other areas:
# Survey Fish Condition (Fulton's K)
# July 1, 2021
# Author: Caira Clark, Irene Andrushchenko
#############################


##DFO Spring-----------------------------

library(ROracle)
library(ggplot2)
library(plyr)
library(dplyr)
library(here)
library(readxl)

channel <- ROracle::dbConnect(DBI::dbDriver("Oracle"), username=AwesomeUser, password=AwesomePwd, oracle.dsn)

#Load condition data from all survey years
springDFO <- ROracle::dbGetQuery(channel, paste("select a.fsex, a.fshno, a.fmat, a.spec, a.mission, a.setno, b.strat, b.area, b.type, c.season, c.year,(100*(a.FWT)/(POWER(a.FLEN,3)))FultK 
                                                from groundfish.gsdet a, groundfish.gsinf b, groundfish.gsmissions c 
                                                where a.spec=10 and c.season='SPRING' and b.strat in ('5Z1','5Z2','5Z3','5Z4','5Z8') and a.mission=b.mission and a.setno=b.setno and a.mission=c.mission"))
springDFO <- springDFO[springDFO$FSEX>0 & springDFO$FMAT>5 & springDFO$YEAR>1980,] #Only using post-spawning fish; hash out if not.
springDFO <- springDFO[springDFO$AREA %in% c(523,524,522,525),] #GB Management Unit
springDFO <- na.omit(springDFO)

ps <- springDFO %>% 
  group_by(YEAR) %>%
  summarize(FultonK=mean(FULTK), SD=sd(FULTK), CV=(sd(FULTK)/mean(FULTK)*100))
mFK <- mean(ps$FultonK) ##This is the avgFultK value 

sprDFO <- ddply(springDFO,. (YEAR, FSEX), summarize, FultonK=mean(FULTK), SD=sd(FULTK), CV=(sd(FULTK)/mean(FULTK)*100))
sprDFO$avgFultK <- mFK  ### this value should change every year, with the updated time series mean (see above)
sprDFO$SEX <- with(sprDFO, ifelse(FSEX==1,'Male','Female'))

sprDFO$Stock<-'Georges Bank'

#####################
###US Bottom Trawl Surveys

#Read in file with strata allocations to spatial units

strata <- read_excel("strata_assignments_final.xlsx")
strata$STRATUM<-paste(0,strata$STRATA, sep="") #Converting strata to same format as in USNEFSC.

##NMFS Fall---------------

#Using SQL script from Access
fallNMFS<-ROracle::dbGetQuery(channel, paste("SELECT A.SEX, B.Year, A.INDWT, A.LENGTH, C.STRATUM
FROM USNEFSC.USS_MSTR_CRUISE B INNER JOIN (USNEFSC.NMFS5ZJM D INNER JOIN (USNEFSC.USS_STATION C INNER JOIN USNEFSC.USS_DETAIL A ON (C.CRUISE6 = A.CRUISE6) AND (C.STRATUM = A.STRATUM) AND (C.TOW = A.TOW) AND (C.STATION = A.STATION)) ON D.STRAT = C.STRATUM) ON B.CRUISE6 = A.CRUISE6 WHERE (((A.INDWT) Is Not Null) AND ((A.MATURITY)='S' Or (A.MATURITY)='T' Or (A.MATURITY)='I') AND ((B.SEASON)='FALL') AND ((A.SVSPP)='073') AND ((B.PURPOSE_CODE)='10') ) GROUP BY A.SEX, B.Year, C.CRUISE6,C.stratum, a.INDWT, a.LENGTH HAVING (((A.SEX)='1' Or (A.SEX)='2')) ORDER BY A.SEX, B.Year, C.CRUISE6")) #Removed Area and Stratum limit. Went from 1900 records to 4700. Seems legit. "AND ((C.AREA) In ('551','552','561','562')) AND ((C.STRATUM) In ('01160','01170','01180','01190','01200','01210','01220'))"

fallNMFS$cLENGTH <- fallNMFS$LENGTH^3
fallNMFS$FULTK <- fallNMFS$INDWT/fallNMFS$cLENGTH
fallNMFS$FULTK <- fallNMFS$FULTK*100000

fallNMFS<-merge(fallNMFS, strata, all.x=TRUE)

fallNMFS <- ddply(fallNMFS,. (YEAR, SEX, Stock), summarize, FultonK=mean(FULTK), SD=sd(FULTK), CV=(sd(FULTK)/mean(FULTK)*100))
fallNMFS$SEX <- with(fallNMFS, ifelse(SEX==1, 'Male','Female'))
mFK2 <- mean(fallNMFS$FultonK)
fallNMFS$avgFultK <- mFK2
fallNMFS$YEAR <- as.numeric(fallNMFS$YEAR)

##NMFS Spring-------------------

springNMFS<-ROracle::dbGetQuery(channel, paste("SELECT A.SEX, B.Year, c.STRATUM, A.INDWT, A.LENGTH
FROM USNEFSC.USS_MSTR_CRUISE B INNER JOIN (USNEFSC.NMFS5ZJM D INNER JOIN (USNEFSC.USS_STATION C INNER JOIN USNEFSC.USS_DETAIL A ON (C.CRUISE6 = A.CRUISE6) AND (C.STRATUM = A.STRATUM) AND (C.TOW = A.TOW) AND (C.STATION = A.STATION)) ON D.STRAT = C.STRATUM) ON B.CRUISE6 = A.CRUISE6 WHERE (((A.INDWT) Is Not Null) AND ((A.MATURITY)='S' Or (A.MATURITY)='T' Or (A.MATURITY)='I') AND ((B.SEASON)='SPRING') AND ((A.SVSPP)='073') AND ((B.PURPOSE_CODE)='10')) GROUP BY A.SEX, B.Year, C.CRUISE6, c.stratum, a.INDWT, a.LENGTH HAVING (((A.SEX)='1' Or (A.SEX)='2')) ORDER BY A.SEX, B.Year, C.CRUISE6")) #Removed area and stratum restriction. Goes from 4000 records to 8800. AND ((C.AREA) In ('551','552','561','562')) AND ((C.STRATUM) In ('01160','01170','01180','01190','01200','01210','01220'))

springNMFS$cLENGTH <- springNMFS$LENGTH^3
springNMFS$FULTK <- springNMFS$INDWT/springNMFS$cLENGTH
springNMFS$FULTK <- springNMFS$FULTK*100000

springNMFS<-merge(springNMFS, strata, all.x=TRUE)

sprNMFS <- ddply(springNMFS,. (YEAR, SEX, Stock), summarize, FultonK=mean(FULTK), SD=sd(FULTK), CV=(sd(FULTK)/mean(FULTK)*100))
sprNMFS$SEX <- with(sprNMFS, ifelse(SEX==1, 'Male','Female'))
mFK3 <- mean(sprNMFS$FultonK)
sprNMFS$avgFultK <- mFK3
sprNMFS$YEAR <- as.numeric(sprNMFS$YEAR)

##All surveys plotted together-----------------

sprDFO$source <- "DFO"
fallNMFS$source <- "NMFS Fall"
sprNMFS$source <- "NMFS Spring"
sprDFO <- sprDFO %>% select(YEAR, SEX, Stock, FultonK, SD, CV, avgFultK, source)
all <- rbind(sprDFO, fallNMFS, sprNMFS)

FultonsKplot <- ggplot(all, aes(YEAR, FultonK, col=SEX)) + geom_point(size=2) + geom_line(size=1) + geom_errorbar(aes(ymin=FultonK-SD, ymax=FultonK+SD), width=.2, position=position_dodge(.9)) +
  facet_grid(source~Stock)+ scale_color_manual(values = c('red','blue')) + theme_bw() + geom_line(aes(YEAR,avgFultK),linetype=2,size=1,col='black')+ylab('FultonK')
FultonsKplot

FultonsKplot <- ggplot(subset(all, Stock!="Western Scotian Shelf"), aes(YEAR, FultonK, col=SEX)) + geom_point(size=2) + geom_line(size=1) + geom_errorbar(aes(ymin=FultonK-SD, ymax=FultonK+SD), width=.2, position=position_dodge(.9)) +
  facet_grid(Stock~source, scale="free_y")+ scale_color_manual(values = c('red','blue')) + theme_bw() + geom_line(aes(YEAR,avgFultK),linetype=2,size=1,col='black')+ylab('FultonK')
FultonsKplot

ggsave(here("figures/AllSurveys_Condition_FultonsK.png"))

