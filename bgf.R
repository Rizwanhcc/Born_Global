##Exports and the Environmental Performance
getwd()
setwd(dir = "/Users/rizwanmushtaq/Dropbox/Born_Global/")
library("plm")
library(lmtest)
library(sandwich)
library(stargazer)

df <- read.csv('final.csv')
df1 <- pdata.frame(df, index=c("Code","Year"), drop.index=TRUE, row.names=TRUE)
head(df1)
dim(df1)

#remove financial and utility firms
#https://www.naics.com/business-lists/counts-by-sic-code/
df1$industry <- df1$Industry.Group...see.separate.tab
df1$co2 <- df1$CO2.Emission..Million.tonnes.

attach(df1)
df <- df1[which(industry < 5000),]
dim(df)

df$env_score <- as.numeric(as.character(df$env_score))
df$emred_socre <- as.numeric(as.character(df$emred_socre))
df$envprod_score <- as.numeric(as.character(df$envprod_score))
df$resred_score <- as.numeric(as.character(df$resred_score))
df$board_size <- as.numeric(as.character(df$CGBSDP060))

##divided by 100
df$env_score <- df$env_score /100
df$emred_socre <- df$emred_socre /100
df$envprod_score <- df$envprod_score /100
df$resred_score <- df$resred_score /100


##Intenational assets and sales
df$int_assets <- df$WC07151
df$int_sales <- df$WC07101

##percenage to total and replace missing and infinit values with zero
df$int_assets_totot <- df$int_assets / df$WC02999
df$int_assets_totot[!is.finite(df$int_assets_totot)] <- 0
df$int_assets_totot[is.na(df$int_assets_totot)] <- 0
df$int_assets_totot <-replace(df$int_assets_totot, df$int_assets_totot < 0, 0)

##percenage to total and replace missing and infinit values with zero
df$int_sales_totot <- df$int_sales / df$WC01001 
df$int_sales_totot[!is.finite(df$int_sales_totot)] <- 0
df$int_sales_totot[is.na(df$int_sales_totot)] <- 0
df$int_sales_totot <-replace(df$int_sales_totot, df$int_sales_totot<0, 0)

##binary
df$int_assets_bin <- ifelse(df$int_assets_totot > 0, 1, ifelse(df$int_assets_totot <= 0, 0, NA))
df$int_sales_bin <- ifelse(df$int_sales_totot > 0, 1, ifelse(df$int_sales_totot <= 0, 0, NA))


#quartiles of international sales
df$int_sales_q1 <- ifelse(df$int_sales_totot > 0.25, 1, ifelse(df$int_sales_totot <= 0.25, 0, NA))
df$int_sales_q2 <- ifelse(df$int_sales_totot > 0.50, 1, ifelse(df$int_sales_totot <= 0.50, 0, NA))
df$int_sales_q3 <- ifelse(df$int_sales_totot > 0.75, 1, ifelse(df$int_sales_totot <= 0.75, 0, NA))


#quartiles of international assets
df$int_assets_q1 <- ifelse(df$int_assets_totot > 0.25, 1, ifelse(df$int_assets_totot <= 0.25, 0, NA))
df$int_assets_q2 <- ifelse(df$int_assets_totot > 0.50, 1, ifelse(df$int_assets_totot <= 0.50, 0, NA))
df$int_assets_q3 <- ifelse(df$int_assets_totot > 0.75, 1, ifelse(df$int_assets_totot <= 0.75, 0, NA))


##Contro Variables
df$liq <- df$WC02201 / df$WC03101
df$lev <- df$WC03255 / df$WC02999
df$rnd <- df$WC01201 / df$WC02999
df$tang <- df$WC02501 / df$WC02999
df$size <- log(df$WC02999)
df$roa <- df$WC08326
#Market to Book Ratio Formula
#  Market Capitalization / Net Book Value
#where, Net Book Value = Total Assets – Total Liabilities
#totoal liabiity tot assets - shareholders equity
df$tot_liab <- df$WC02999 - df$WC03995 
df$nbv <- df$WC02999 - df$tot_liab 
df$mtb <- df$WC08001 / df$nbv

##industry codes
df$industry3 <- substr(df$industry,1,3)
df$industry2 <- substr(df$industry,1,2)

##logs
df$log_co2 <- log(df$co2)


##descripitve statistics
stargazer(subset(df[c('env_score', 'emred_socre','envprod_score','resred_score','size','liq','rnd','tang','roa','mtb', 'GDP.growth')], 
          df$int_sales_q1==1), title="Summary Statistics: Quartile == 1", digits=4, type = 'html', out="q1.html",
          summary.stat = c("mean", "min", "p25", "median", "p75", "sd"))

stargazer(subset(df[c('env_score', 'emred_socre','envprod_score','resred_score','size','liq','rnd','tang','roa','mtb', 'GDP.growth')], 
          df$int_sales_q2==1), title="Summary Statistics: Quartile == 2", digits=4, type = 'html', out="q2.html",
          summary.stat = c("mean", "min", "p25", "median", "p75", "sd"))

stargazer(subset(df[c('env_score', 'emred_socre','envprod_score','resred_score','size','liq','rnd','tang','roa','mtb', 'GDP.growth')], 
          df$int_sales_q3==1), title="Summary Statistics: Quartile == 3", digits=4, type = 'html', out="q3.html",
          summary.stat = c("mean", "min", "p25", "median", "p75", "sd"))


##Fixed effects model
#https://cran.r-project.org/web/packages/plm/vignettes/A_plmPackage.html
reg1 <- plm(env_score ~ int_sales_q1+ size +liq+ rnd+lev+ tang+ roa+ mtb+ GDP.growth, data = df, 
           index = c("Code", "Year"), model = "within")

reg2 <- plm(env_score ~ int_sales_q2+ size +liq+ rnd+ lev+ tang+ roa+ mtb+GDP.growth, data = df, 
            index = c("Code", "Year"), model = "within")

reg3 <- plm(env_score ~ int_sales_q3+size + liq+ rnd+ lev+ tang+ roa+ mtb+GDP.growth, data = df, 
            index = c("Code", "Year"), model = "within")

reg4 <- plm(env_score ~ int_sales_totot+ size +liq+ rnd+lev+  tang+ roa+ mtb+GDP.growth, data = df, 
            index = c("Code", "Year"), model = "within")

#https://cran.r-project.org/web/packages/clubSandwich/vignettes/panel-data-CRVE.html
library(clubSandwich)
#vcovCR(reg1, cluster = df$industry,type="CR2")
coef_test(reg1, vcov = "CR1", cluster = df$industry, test = "naive-t")
coef_test(reg2, vcov = "CR1", cluster = df$industry, test = "naive-t")
coef_test(reg3, vcov = "CR1", cluster = df$industry, test = "naive-t")
coef_test(reg4, vcov = "CR1", cluster = df$industry, test = "naive-t")


stargazer(reg1, reg2,reg3,reg4,
          # se = rob_se, #clustered standard errors
          title="Fixed Effects", 
          out="fixed.html", #comment out for latex
          out.header=TRUE, style = "qje",
          column.labels=c("Totalenv", "Totalenv", "Totalenv", "Totalenv"), df=FALSE, digits=4)



##GMM Method'
#https://cran.r-project.org/web/packages/plm/vignettes/A_plmPackage.html
gmm1 <- pgmm(env_score ~ lag(env_score, 1:2) + lag(size, 0:1) +lag(liq, 0:1)+ lag(rnd, 0:1) + lag(lev, 0:1)+lag(tang, 0:1)+lag(roa, 0:1)+lag(mtb, 0:1)+
               int_sales_totot | lag(env_score, 2:10), data = df, effect = 'twoways', model = 'twosteps', 
             transformation = "ld")
               
             
summary(gmm1, robust = TRUE)
#The aim of the Arellano-Bond tests is to check whether the idiosyncratic error term is serially correlated. The test is conducted for the first-differenced errors. If the error term in levels is serially uncorrelated, this implies that the error term in first differences has negative first-order serial correlation (with a correlation coefficient of -0.5) but no second-order or higher-order serial correlation. Thus, we should reject the null hypothesis of no first-order serial correlation in first differences (AR(1) test) but should not reject the null hypothesis of no higher-order serial correlation in first differences (AR(2), AR(3), ...).
#If you do not reject the null hypothesis of the AR(1) test, this could indicate that your idiosyncratic error term in levels is highly serially correlated. In the extreme case, the error term in levels follows a random walk such that the first-differenced errors are serially uncorrelated. Such a situation would indeed invalidate the MSM.

# set seed for reproducibility 
set.seed(100)
#Propensity score matching
#http://www.unnatinarang.com/blog/r-creating-panel-data-from-propensity-score-matched-samples-using-the-matchit-package
library(MatchIt)

#create a copy of dataframe
df_matching <- df
df_matching <- reshape(df_matching, idvar = "Code", timevar="Year", direction = "wide")


#remove missing values in the covariates
df_matching$size[is.na(df_matching$size)] <- 0
df_matching$liq[is.na(df_matching$liq)] <- 0
df_matching$rnd[is.na(df_matching$rnd)] <- 0
df_matching$lev[is.na(df_matching$lev)] <- 0
df_matching$tang[is.na(df_matching$tang)] <- 0
df_matching$roa[is.na(df_matching$roa)] <- 0
df_matching$mtb[is.na(df_matching$mtb)] <- 0
df_matching$GDP.growth[is.na(df_matching$GDP.growth)] <- 0


#remove missing anin inf values from dependent variable
df_matching$int_sales_bin[!is.finite(df_matching$int_sales_bin)] <- 0
df_matching$int_sales_bin[is.na(df_matching$int_sales_bin)] <- 0
df_matching$int_sales_bin <-replace(df_matching$int_sales_bin, df_matching$int_sales_bin<0, 0)



m.out <- matchit(int_sales_bin ~ size + liq + rnd + lev + tang + roa + mtb + GDP.growth,
                 method = "nearest", data = df_matching, replace=FALSE)

















