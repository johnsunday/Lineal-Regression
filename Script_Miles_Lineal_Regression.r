#READ ANALYSIS FILE
#FORMAT : 3 COLUMNS WITH HEADER: MDMVEHICLE_ID, MONTH, MILEAGE
df<-read.csv(file.choose())

# CONVERT VALUES TO NUMBERS
df$"MONTH"<-as.numeric(df$"MONTH")
df$"MILEAGE"<-as.numeric(df$"MILEAGE")

#CREATE RESULTS DATAFRAME
results <- data.frame(MDMVEHICLE_ID=character(), 
                 DATA_POINTS=numeric(), 
                 LINEAL_R_COEFF=double(),
                 LINEAL_R_CONST=double(),
                 CORR_COEFF=double(),
                 R_SQUARED=double(),
                 LOWER_CONF_LIMIT_COEFF=double(),
                 UPPER_CONF_LIMIT_COEFF=double(),
                 LOWER_CONF_LIMIT_CONST=double(),
                 UPPER_CONF_LIMIT_CONST=double())

#LOOP FOR EACH VEHICLE
for (i in df$"VEHICLE_ID"){

#SUBSET FOR EACH VEHICLE    
sub<- subset(df,df$"VEHICLE_ID"=i)
#CORRELATION COEFFICIENT BETWEEN MILES AND MONTH FOR EACH VEHICLE
CORR_COEFF<-cor(sub$"MONTH",sub$"MILEAGE")
#LINEAL REGRESSION  FOR EACH VEHICLE
model <- lm(sub$"MILEAGE" ~ sub$"MONTH")

#***************************************************
# LINEAL REGRESSION FORMULA : 
# Y= AX + B 
#PREDICT_MILES = (UNIQUE_MONTH_TO_PREDICT * COEFFICIENT ) + CONSTANT
#***************************************************

#GET LINEAL REGRESSION VALUES

#GET COEFFICIENT AND CONSTANT VALUES
regaux<-coef(model)

LINEAL_R_CONST <-regaux["(Intercept)"]
LINEAL_R_COEFF <-regaux["df$MONTH"]

#GET CONFIDENCE INTERVAL
confaux<-confint(model,level=0.95)

LOWER_CONF_LIMIT_COEFF <-confaux["df$MONTH",1]
UPPER_CONF_LIMIT_COEFF <-confaux["df$MONTH",2]
LOWER_CONF_LIMIT_CONST <-confaux["(Intercept)",1]
UPPER_CONF_LIMIT_CONST <-confaux["(Intercept)",2]

#GET R SQUARED
R_SQUARED<-summary(model)$r.squared

#INSERT NEW ROW VALUES:

row<- list (MDMVEHICLE_ID=sub$"id", 
            DATA_POINTS=nrow(sub), 
            LINEAL_R_COEFF=LINEAL_R_COEFF,
            LINEAL_R_CONST=LINEAL_R_CONST,
            CORR_COEFF=CORR_COEFF,
            R_SQUARED=R_SQUARED,
            LOWER_CONF_LIMIT_COEFF=COEFF_LOWER_CONF,
            UPPER_CONF_LIMIT_COEFF=COEFF_UPPER_CONF,
            LOWER_CONF_LIMIT_CONST=CONST_LOWER_CONF,
            UPPER_CONF_LIMIT_CONST=CONST_UPPER_CONF)
              

#MERGE NEW ROW WITH RESULTS DATAFRAME
results = rbind(results,row, stringsAsFactors=FALSE)
}
#CREATE CSV
archivename=paste("Lineal_Regression_Analysis_",Sys.Date(),".csv",sep="")
write.csv(results, archivename)
#THE CSV WILL BE CREATED AT  C:\Users\Username\Documents
print ("THE PROCESS HAS ENDED!")
