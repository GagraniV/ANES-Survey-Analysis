
# set your working directory.
setwd( "~/R/election" )
load("Time Series Cumulative Data File/anes_timeseries_cdf.rda")

View(x)
class(x)
range(x$vcf0004)
range(x$vcf0110)
nrow(x)
head(x)
names(x)

vars.to.keep<-c('vcf0004','vcf0006','vcf0006a','vcf0004','vcf0011z','vcf0102','vcf0104','vcf0110','vcf0112','vcf0114','vcf0128','vcf0138','vcf0143','vcf0146','vcf0147','vcf0303','vcf0704','vcf0704a','vcf0705','vcf0106')

#2: define a new data frame(ANES by selecting varibles listed in vars.to.keep)
ANES <- x[ ,vars.to.keep]

#3: save ANES data file in working directory.
save(ANES,file="ANES.rda")

# garbage collection: clear up RAM
gc()

###03Script
# set your working directory.
setwd( "~/R/election" )
library("survey")
library("dplyr")
load("C:/Users/gagranis/Documents/R/election/anes.rda")

names(ANES)
str(ANES)
#year
class(ANES$vcf0004)
ANES$vcf0004<-as.factor(ANES$vcf0004)
levels(ANES$vcf0004)

#Respondent Gender("2. Female" "1. Male")
class(ANES$vcf0104)
str(ANES$vcf0104)
ANES<-filter(ANES, vcf0104 == c(1,2))


anes.design <- 
  svydesign( 
  ~ vcf0006a,
  data = ANES ,  
  weights = ~vcf0011z,  
  nest = TRUE  
) 

##For one year
svyby(
      ~ factor(vcf0104),
      ~ vcf0004,
      design = subset(anes.design,vcf0004==1996),
      svymean,
      na.rm = TRUE
    )

##for multiple years
df = data.frame()
year<-levels(ANES$vcf0004)

for (i in year) {
  print(i)
  dfr=data.frame()
  dfr<-tryCatch({
    svyby(
      ~ factor(vcf0104),
      ~ vcf0004,
      design = subset(anes.design, vcf0004==i),
      svymean,
      na.rm = TRUE
    )
  },error=function(e){
    (paste("error on year:", i))
    
  }
  ) 
  {
    df<-rbind(df,dfr)
  }  
}

View(df)


