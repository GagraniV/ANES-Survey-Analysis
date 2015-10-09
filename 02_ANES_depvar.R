setwd( "~/R/election" )
load("ANES.rda")
library(qdap) # dist_tab function

sum(is.na(ANES$vcf0876))
dist_tab(ANES$vcf0876)

#Dependent Variable:
############################################################ 

depvar.sel("vcf0876") 
  
ANESdvar<-ANES
table(ANESdvar$cappun)
depvar.sel<-function (dvar){
if(dvar=="vcf0876")
{
depvar<-ANESdvar[ ,dvar]
dv<-cbind(ANESdvar$depvar <-depvar)
dv1<-cbind(ANESdvar$depvar_g <-ANESdvar$depvar)
ANESdvar$depvar_g <- with(ANESdvar, ifelse(depvar_g == 1, 1, 
                           ifelse(depvar_g == 5, 0,2)))
ANESdvar<- subset(ANESdvar, depvar_g ==0  | depvar_g ==1)
print(table(ANESdvar$depvar_g))

#1:favor 0:oppose

save(ANESdvar,file="ANESdvar.rda")
ttitle<<-"Laws to protect homosexuals against job discrimination in the US(Source:ANES Survey)"
tylab<<-"% Laws to protect homosexuals"
tvar<<- "vcf0876"  
}
 else if (dvar=="gunlaw")
{
depvar<-ANESdvar[ ,dvar]
dv<-cbind(ANESdvar$depvar <-depvar)
dv1<-cbind(ANESdvar$depvar_g <-ANESdvar$depvar)
ANESdvar$depvar_g <- with(ANESdvar, ifelse(depvar_g == "favor", 1, 
                            ifelse(depvar_g == "oppose", 0,2)))
ANESdvar<- subset(ANESdvar, depvar_g ==0  | depvar_g ==1)
print(table(ANESdvar$depvar_g))
save(ANESdvar,file="ANESdvar.rda")
ttitle<<-"Favor Police Permit to Buy Gun(Source:General Society Survey)"
tylab<<-"%Favoring police Permit"
tvar<<- "gunlaw"
}else if (dvar=="happy")
{
depvar<-ANESdvar[ ,dvar]
dv<-cbind(ANESdvar$depvar <-depvar)
dv1<-cbind(ANESdvar$depvar_g <-ANESdvar$depvar)
ANESdvar$depvar_g <- with(ANESdvar, ifelse(depvar_g == "very happy", 1, 0))
ANESdvar<- subset(ANESdvar, depvar_g ==0  | depvar_g ==1)
print(table(ANESdvar$depvar_g))  
save(ANESdvar,file="ANESdvar.rda")
ttitle<<-"General Happiness in the US (Source:General Society Survey)"
tylab<<-"%General Happiness"
tvar<<-"happy"
}else if (dvar=="grass")
{
depvar<-ANESdvar[ ,dvar]
dv<-cbind(ANESdvar$depvar <-depvar)
dv1<-cbind(ANESdvar$depvar_g <-ANESdvar$depvar)
ANESdvar$depvar_g <- with(ANESdvar, ifelse(depvar_g == "legal", 1, 0))
ANESdvar<- subset(ANESdvar, depvar_g ==0  | depvar_g ==1)
print(table(ANESdvar$depvar_g))  
save(ANESdvar,file="ANESdvar.rda")
ttitle<<-"Marijuana be Made Legal in the US (Source:General Society Survey)"
tylab<<-"%Favoring Marijuana legalization"
tvar<<-"grass"
}else if (dvar=="trust")
{
depvar<-ANESdvar[ ,dvar]
dv<-cbind(ANESdvar$depvar <-depvar)
dv1<-cbind(ANESdvar$depvar_g <-ANESdvar$depvar)
ANESdvar$depvar_g <- with(ANESdvar, ifelse(depvar_g == "can trust", 1, 
                                     ifelse(depvar_g == "cannot trust", 0,2)))
ANESdvar<- subset(ANESdvar, depvar_g ==0  | depvar_g ==1)
print(table(ANESdvar$depvar_g))  
save(ANESdvar,file="ANESdvar.rda")
ttitle<<-"Can People be Trusted in the US (Source:General Society Survey)"
tylab<<-"%Trusting People"
tvar<<-"trust"
}else if (dvar=="fair")
{
depvar<-ANESdvar[ ,dvar]
dv<-cbind(ANESdvar$depvar <-depvar)
dv1<-cbind(ANESdvar$depvar_g <-ANESdvar$depvar)
ANESdvar$depvar_g <- with(ANESdvar, ifelse(depvar_g == "fair", 1, 
                                     ifelse(depvar_g == "take advantage", 0,2)))
ANESdvar<- subset(ANESdvar, depvar_g ==0  | depvar_g ==1)
print(table(ANESdvar$depvar_g))  
save(ANESdvar,file="ANESdvar.rda")
ttitle<<-"People are Fair or Try to Take Advantage in the US (Source:General Society Survey)"
tylab<<-"%Treating People Fairly"
tvar<<-"fair"
}else if (dvar=="satfin")
{
depvar<-ANESdvar[ ,dvar]
dv<-cbind(ANESdvar$depvar <-depvar)
dv1<-cbind(ANESdvar$depvar_g <-ANESdvar$depvar)
ANESdvar$depvar_g <- with(ANESdvar, ifelse(depvar_g == "not at all sat", 0, 1))
ANESdvar<- subset(ANESdvar, depvar_g ==0  | depvar_g ==1)
print(table(ANESdvar$depvar_g))  
save(ANESdvar,file="ANESdvar.rda")
ttitle<<-"Satisfaction with Financial Situation in the US (Source:General Society Survey)"
tylab<<-"%Financial Satisfaction"
tvar<<-"satfin"
}else if (dvar=="cappun")
{
depvar<-ANESdvar[ ,dvar]
dv<-cbind(ANESdvar$depvar <-depvar)
dv1<-cbind(ANESdvar$depvar_g <-ANESdvar$depvar)
ANESdvar$depvar_g <- with(ANESdvar, ifelse(depvar_g == "favor",1, 0))
ANESdvar<- subset(ANESdvar, depvar_g ==0  | depvar_g ==1)
print(table(ANESdvar$depvar_g))  
save(ANESdvar,file="ANESdvar.rda")
ttitle<<-"Favor or Oppose Death Penalty for Murder in the US (Source:General Society Survey)"
tylab<<-"%Favoring Death Penalty"
tvar<<-"cappun"
}
else
{
print("no varible defiend")
}
}

############################################################
#Select varible of choice. 
############################################################

depvar.sel("owngun")
#depvar.sel("gunlaw")
#depvar.sel("happy")
#depvar.sel("grass")
#depvar.sel("trust")
#depvar.sel("fair")
#depvar.sel("satfin")
#depvar.sel("cappun")
#depvar.sel("xxx")
