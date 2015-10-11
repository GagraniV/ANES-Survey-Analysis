setwd( "~/R/election" )
load("ANES.rda")
library(qdap) # dist_tab function

sum(is.na(ANES$vcf0876))
dist_tab(ANES$vcf0876)

#Dependent Variable:
############################################################ 


ANESdvar<-ANES

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
 else if (dvar=="vcf0877")
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
ttitle<<-"Allow homosexuals to serve in the US Armed Forces(Source:ANES Survey)"
tylab<<-"%Homosexuals serveing in the US Armed Forces"
tvar<<- "vcf0877"

}else if (dvar=="vcf0878")
{
depvar<-ANESdvar[ ,dvar]
dv<-cbind(ANESdvar$depvar <-depvar)
dv1<-cbind(ANESdvar$depvar_g <-ANESdvar$depvar)
ANESdvar$depvar_g <- with(ANESdvar, ifelse(depvar_g == 1, 1,
                                     ifelse(depvar_g == 5, 0,2)))
ANESdvar<- subset(ANESdvar, depvar_g ==0  | depvar_g ==1)
print(table(ANESdvar$depvar_g))  
save(ANESdvar,file="ANESdvar.rda")
ttitle<<-" Allow homosexual couples to adopt children (Source:ANES Survey)"
tylab<<-"%homosexual adopt children"
tvar<<-"vcf0878"

}else if (dvar=="vcf9131")
{
depvar<-ANESdvar[ ,dvar]
dv<-cbind(ANESdvar$depvar <-depvar)
dv1<-cbind(ANESdvar$depvar_g <-ANESdvar$depvar)
ANESdvar$depvar_g <- with(ANESdvar, ifelse(depvar_g == 1, 1,
                                    ifelse(depvar_g == 2, 0,3)))
ANESdvar<- subset(ANESdvar, depvar_g ==0  | depvar_g ==1)
print(table(ANESdvar$depvar_g))  
save(ANESdvar,file="ANESdvar.rda")
ttitle<<-"Less Government Better OR Government Do More (Source:ANES Survey)"
tylab<<-"%Less or more Government"
tvar<<-"vcf9131"
#1:less 2:more

}else if (dvar=="vcf9132")
{
depvar<-ANESdvar[ ,dvar]
dv<-cbind(ANESdvar$depvar <-depvar)
dv1<-cbind(ANESdvar$depvar_g <-ANESdvar$depvar)
ANESdvar$depvar_g <- with(ANESdvar, ifelse(depvar_g == 1, 1, 
                                     ifelse(depvar_g == 2, 0,3)))
ANESdvar<- subset(ANESdvar, depvar_g ==0  | depvar_g ==1)
print(table(ANESdvar$depvar_g))  
save(ANESdvar,file="ANESdvar.rda")
ttitle<<-"Govt Handle Economy OR Free Market Can Handle (Source:ANES Survey)"
tylab<<-"%Trusting People"
tvar<<-"vcf9132"
#1:strong 2:free

}else if (dvar=="vcf0887")
{
depvar<-ANESdvar[ ,dvar]
dv<-cbind(ANESdvar$depvar <-depvar)
dv1<-cbind(ANESdvar$depvar_g <-ANESdvar$depvar)
ANESdvar$depvar_g <- with(ANESdvar, ifelse(depvar_g == 1, 1, 
                                     ifelse(depvar_g == 2, 0,
                                      ifelse(depvar_g == 3, 2,4)))) 
ANESdvar<- subset(ANESdvar, depvar_g ==0  | depvar_g ==1 | depvar_g ==2)
print(table(ANESdvar$depvar_g))  
save(ANESdvar,file="ANESdvar.rda")
ttitle<<-"Federal Spending- Child Care(Source:ANES Survey)"
tylab<<-"%Federal Spending"
tvar<<-"vcf0887"
#1. Increased 2. Same 3. Decreased

}else if (dvar=="vcf0888")
{
depvar<-ANESdvar[ ,dvar]
dv<-cbind(ANESdvar$depvar <-depvar)
dv1<-cbind(ANESdvar$depvar_g <-ANESdvar$depvar)
ANESdvar$depvar_g <- with(ANESdvar, ifelse(depvar_g == 1, 1, 
                                     ifelse(depvar_g == 2, 0,
                                      ifelse(depvar_g == 3, 2,4)))) 
ANESdvar<- subset(ANESdvar, depvar_g ==0  | depvar_g ==1 | depvar_g == 2)
print(table(ANESdvar$depvar_g))  
save(ANESdvar,file="ANESdvar.rda")
ttitle<<-"Federal Spending- Dealing with Crime (Source:ANES Survey)"
tylab<<-"%Federal Spending"
tvar<<-"vcf0888"
#1. Increased 2. Same 3. Decreased

}else if (dvar=="vcf0890")
{
depvar<-ANESdvar[ ,dvar]
dv<-cbind(ANESdvar$depvar <-depvar)
dv1<-cbind(ANESdvar$depvar_g <-ANESdvar$depvar)
ANESdvar$depvar_g <- with(ANESdvar, ifelse(depvar_g == 1, 1, 
                                     ifelse(depvar_g == 2, 0,
                                      ifelse(depvar_g == 3, 2,4)))) 
ANESdvar<- subset(ANESdvar, depvar_g ==0  | depvar_g ==1 | depvar_g == 2)
print(table(ANESdvar$depvar_g))  
save(ANESdvar,file="ANESdvar.rda")
ttitle<<-"Federal Spending- Public Schools (Source:ANES Survey)"
tylab<<-"%Federal Spending"
tvar<<-"vcf0890"
}
#1. Increased 2. Same 3. Decreased  
else
{
print("no varible defiend")
}
}

############################################################
#Select varible of choice. 
############################################################

#depvar.sel("vcf0876")
#depvar.sel("vcf0877")
#depvar.sel("vcf0878")
#depvar.sel("vcf9131")
#depvar.sel("vcf9132")
#depvar.sel("vcf0887")
#depvar.sel("vcf0888")
#depvar.sel("vcf0890")
#depvar.sel("xxx")
