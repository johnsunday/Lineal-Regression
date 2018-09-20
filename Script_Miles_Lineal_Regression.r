df<-read.csv(file.choose())
colnames(df)[1] <- "SIMPLE_ID"
colnames(df)[2] <- "UNIQUE_MONTH"
colnames(df)[3] <- "MILEAGE"
df$"UNIQUE_MONTH"<-as.numeric(df$"UNIQUE_MONTH")
df$"MILEAGE"<-as.numeric(df$"MILEAGE")

starttime<-Sys.time()


results <- data.frame(SIMPLE_ID=character(), 
                 DATA_POINTS=character(), 
                 LINEAL_R_COEFF=double(),
                 LINEAL_R_CONST=double(),
                 CORR_COEFF=double(),
                 R_SQUARED=double(),
                 LOWER_CONF_LIMIT_COEFF=double(),
                 UPPER_CONF_LIMIT_COEFF=double(),
                 LOWER_CONF_LIMIT_CONST=double(),
                 UPPER_CONF_LIMIT_CONST=double())

i<- subset(df,df$"SIMPLE_ID"<5000)
i <- unique(i$"SIMPLE_ID")
print ("Proccessing...")
#loop for each vehicle
for(id in i)
  {

  
#Subset for each Vehicle    
sub<- subset(df,df$"SIMPLE_ID"==id)
#Correlation Coefficient between Miles and Month for each Vehicle
CORR_COEFF<-cor(sub$"UNIQUE_MONTH",sub$"MILEAGE")
#Lineal Regression  for each vehicle
model <- lm(sub$"MILEAGE" ~ sub$"UNIQUE_MONTH")

#Insert new row with:
# MDM_ID , nrow, Lineal Regression Coefficient ,Corrrelation Coefficient, R^2,
#LEFT CONFIDENCE , RIGHT CONFIDENCE

regaux<-coef(model)

LINEAL_R_CONST <-regaux["(Intercept)"]
LINEAL_R_COEFF <-regaux["sub$UNIQUE_MONTH"]

confaux<-confint(model,level=0.95)

LOWER_CONF_LIMIT_COEFF <-confaux["sub$UNIQUE_MONTH",1]
UPPER_CONF_LIMIT_COEFF <-confaux["sub$UNIQUE_MONTH",2]
LOWER_CONF_LIMIT_CONST <-confaux["(Intercept)",1]
UPPER_CONF_LIMIT_CONST <-confaux["(Intercept)",2]
R_SQUARED<-summary(model)$r.squared
SIMPLE_ID<-as.character(unique(sub$"SIMPLE_ID"))
DATA_POINTS<-as.character(nrow(sub))

row<- data.frame (SIMPLE_ID, 
                  DATA_POINTS, 
                  LINEAL_R_COEFF,
                  LINEAL_R_CONST,
                  CORR_COEFF,
                  R_SQUARED,
                  LOWER_CONF_LIMIT_COEFF,
                  UPPER_CONF_LIMIT_COEFF,
                  LOWER_CONF_LIMIT_CONST,
                  UPPER_CONF_LIMIT_CONST)

results <- bind_rows(results, row)
}             

#MERGE NEW ROW WITH RESULTS DATAFRAME
#results = rbind(results,row, stringsAsFactors=FALSE)

archivename=paste("Lineal_Regression_Analysis_",Sys.Date(),".csv",sep="")
print ("Writing CSV....")
write.csv(results, archivename)
#THE CSV WILL BE CREATED AT  C:\Users\Username\Documents

endtime<-Sys.time()

print ("THE PROCESS HAS ENDED!")
print (paste("Start Time:",starttime))
print (paste("End Time:",endtime))
