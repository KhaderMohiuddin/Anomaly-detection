library(sqldf)
library(quantmod)
library(data.table)
#library(gridBase)
#library(grid)
#library(gridExtra)
#library(ORCH)

data = data.frame(fread("Edison Field Prod.csv"))
firm_data = data.frame(fread("2016CaliforniaOilandGasWells.csv"))

#name_list = c('Opr__BusinessName', 'WMtr_APINumber', 'WMtr_LeaseName',
#              'WPrd_ProductionDate', 'WPrd_OilProduced', 'WPrd_GasProduced',
#              'WPrd_WaterProduced', 'WPrd_DaysProducing')

#data = subset(data, select = name_list)
#names(data) = c('firm', 'well', 'well_lease', 'prod_date', 'oil', 'gas', 'water', 'days_prod')

data = sqldf("select Opr__BusinessName as firm, WMtr_APINumber as well, 
             WMtr_LeaseName as well_lease, WPrd_ProductionDate as prod_date, 
             WPrd_OilProduced as oil, WPrd_GasProduced as gas, 
             WPrd_WaterProduced as water, WPrd_DaysProducing as days_prod
             from data")

#name_list = c('APINumber', 'WellStatus', 'OperatorName', 'OperatorStatus', 
#              'OperatorReportingMethod', 'WellTypeCode')

#firm_data = subset(firm_data, select = name_list)
#names(firm_data) = c('well', 'well_status', 'firm', 'firm_status', 'firm_report_method', 'well_type')

firm_data = sqldf("select APINumber as well, WellStatus as well_status, OperatorName as firm,
                  OperatorStatus as firm_status, OperatorReportingMethod as firm_report_method,
                  WellTypeCode as well_type 
                  from firm_data")

#assumptions a) Replace missing data with 0. 
data$oil[is.na(data$oil)] <- 0; data$gas[is.na(data$gas)] <- 0;
data$water[is.na(data$water)] <- 0; data$days_prod[is.na(data$days_prod)] <- 0
###############################
date_fun = function(x)
{
  a2 = unlist(strsplit(x,"/"))
  a3.month = a2[1]; a3.day = a2[2]; a3.year = a2[3]
  result = as.Date(paste0(a3.year,'-',a3.month,'-',a3.day)) 
  result
}
a = matrix(data$prod_date)
a_new = apply(a,1,date_fun)
data$prod_date = as.Date(a_new)
data$yr = year(data$prod_date)
####################################
#x = sqldf("select firm, count(distinct well) as num_well, 
#      count(distinct well_lease) as num_well_lease, 
#      sum(oil) as total_oil, sum(gas) as total_gas,  sum(water) as total_water,
#      sum(days_prod) as total_days_prod, 
#      sum(oil)/count(distinct well)/sum(days_prod) as avg_oil_per_well_per_day
#      from data
#      group by firm
#      order by sum(oil) desc")

#x$pct_of_total_oil = round(x$total_oil/sum(x$total_oil),2)
#x$pct_of_total_gas = round(x$total_gas/sum(x$total_gas),2)
#x$pct_of_total_water = round(x$total_water/sum(x$total_water),2)
#x$oil_per_well = round(x$total_oil/x$num_well,0)
#x = x[x$total_oil >0,]
#x_cl = kmeans(x$total_oil,4)
#x$cluster = x_cl$cluster

#x_new = sqldf("select cluster, count(distinct firm) as num_firms, sum(num_well) as num_well, 
#sum(num_well_lease) as num_well_lease, sum(total_oil) as total_oil, 
#sum(total_water) as total_water,
#sum(pct_of_total_water) as pct_total_water
#from x 
#group by cluster 
#order by sum(pct_of_total_oil) desc")
 
#x_new$pct_total_water = round(x_new$total_water/sum(x$total_water),2)
#x_new$pct_total_oil = round(x_new$total_oil/sum(x$total_oil),2)

#x_new
#####################################################################
timer = Sys.time()
flag = 0
#flag = 1 ; y = '2015' 
minx = 1977; maxx = 2015
y_list = seq(minx,maxx)
y_list_result = data.frame(matrix(0, length(y_list), 2))
names(y_list_result) = c('yr', 'total_oil_derived')

#for (i in 1:length(y_list))
#{
#y = as.character(y_list[i])
#y = '2011'
result_summary = data.frame(matrix(0,5, 4))
names(result_summary) = c("Detector", "Number of Unique Firms", "Number of Cases Detected", "Total Oil Derived from Behavior")
#####################################################################
#Detector 1: oil but no water
a = sqldf("select firm, well, prod_date, oil, gas, water, days_prod 
                         from data
                         where oil >0 and water == 0")

if(flag == 1){a = a[substr(a$prod_date,1,4) == y,]}

b = sqldf("select firm, well, sum(oil) as oil, sum(days_prod) as days_prod 
          from a group by firm, well order by sum(oil) desc")

b2 = sqldf("select firm, sum(oil) as oil, count(well) as num_wells, sum(days_prod) as days_prod
           from b group by firm order by sum(oil) desc")

b2$pct_of_oil =  round(b2$oil/sum(b2$oil),2)

nrow(data); nrow(a);head(b2); summary(b2); sum(b2$oil)

fraud_firms = b[b$oil >= quantile(b$oil, 0.9),]
fraud_firm_wells = unique(fraud_firms$well)[1]
c = data[data$well == fraud_firm_wells,];
firm_lookup = b[b$firm == c$firm[1] & b$well == c$well[1],]

#ts.plot(c$oil, col = 3, 
#        main = c(paste('Firm = ',c$firm[1], '\nWell =',c$well[1],
#                       '\nTotal Oil =',firm_lookup$oil,
#                       '| Total Days=', firm_lookup$days_prod)),ylab = "")
#lines(c$water, col = 2)
#legend('topright', c('Oil', 'Water'), col = c(3,2), lty = c(1,1))
#which days did it actually occur
#d = c[c$oil >0 & c$water == 0,]; tail(d,20)

result_summary[1,] = cbind(1, length(unique(b2$firm)), sum(b2$num_wells), sum(b2$oil))
result_final_12 = data.frame(Detector = "Detector 1", cbind(b))
#####################################################################
#Detector 2: days on production in excess of 10 barrels
a = sqldf("select firm, well, prod_date, oil, gas, water, days_prod 
          from data
          where oil <=10 and days_prod >=28")

if(flag == 1){a = a[substr(a$prod_date,1,4) == y,]}

b = sqldf("select firm, well, sum(oil) as oil, sum(days_prod) as days_prod 
          from a group by firm, well order by sum(oil) desc")

b2 = sqldf("select firm, sum(oil) as oil, count(well) as num_wells, sum(days_prod) as days_prod
           from b group by firm order by sum(oil) desc")

b2$pct_of_oil =  round(b2$oil/sum(b2$oil),2)

nrow(data); nrow(a);head(b2); summary(b2); sum(b2$oil)

fraud_firms = b[b$oil >= quantile(b$oil, 0.9),]
fraud_firm_wells = unique(fraud_firms$well)[1]
c = data[data$well == fraud_firm_wells,];
firm_lookup = b[b$firm == c$firm[1] & b$well == c$well[1],]

#ts.plot(c$oil, col = 3, 
#        main = c(paste('Firm = ',c$firm[1], '\nWell =',c$well[1],
#                       '\nTotal Oil =',firm_lookup$oil,
#                       '| Total Days=', firm_lookup$days_prod)),ylab = "")
#lines(c$water, col = 2)
#legend('topright', c('Oil', 'Water'), col = c(3,2), lty = c(1,1))
#d = c[c$oil <=10 & c$days_prod >=28,]; tail(d,20)

result_summary[2,] = cbind(2, length(unique(b2$firm)), sum(b2$num_wells), sum(b2$oil))
result_final_12 = rbind(result_final_12, data.frame(Detector = "Detector 2", cbind(b)))
####################################################################
#Detector 3: Static oil production above 10 bbl for over 6 months

a = sqldf("select firm, well, prod_date, oil, gas, water, days_prod 
          from data
          where oil > 10")

if(flag == 1){a = a[substr(a$prod_date,1,4) == y,]}

dt = data.table(a)
dt_new = dt[,.(oil_result = sum(oil[which(Delt(oil) == 0)]),
               static_count= length(which(Delt(oil) == 0))), 
            by = .(firm, well)]

setkey(dt, firm, well)
setkey(dt_new, firm, well)
##########
#c = data[data$well == dt_new$well[1],]$oil
#c[which(Delt(c) == 0)]
#sum(c[which(Delt(c) == 0)])
#############
a = dt[dt_new[static_count > 6]]

b = sqldf("select firm, well, sum(oil) as oil, sum(days_prod) as days_prod,
          max(oil_result) as oil_result, 
          max(static_count) as static_count 
          from a group by firm, well order by max(oil_result) desc")

b2 = sqldf("select firm, sum(oil) as oil, count(well) as num_wells, 
           sum(days_prod) as days_prod, sum(oil_result) as oil_result, sum(static_count) as static_count
           from b group by firm order by sum(oil_result) desc")

b2$pct_of_oil = round(b2$oil/sum(b2$oil),2)
b2$pct_of_oil_static = round(b2$oil_result/sum(b2$oil_result),2)

nrow(data); nrow(a);head(b2); summary(b2); sum(b2$oil); sum(b2$oil_result)

fraud_firms = b[b$static_count >= quantile(b$static_count, 0.9),]
x_list = unique(fraud_firms$well)
#fraud_firm_wells = unique(fraud_firms$well)[4]

fraud_firm_wells = sample(x_list,1)
#c = a[a$well == fraud_firm_wells,];
c = data[data$well == fraud_firm_wells,]
#total_static = b[b$well == fraud_firm_wells,]$static_count
#total_oil = b[b$well == fraud_firm_wells,]$oil_result
total_static = length((c$oil[which(Delt(c$oil) == 0)]))
total_oil = sum(c$oil[which(Delt(c$oil) == 0)])

#ts.plot(c$oil, col = 3, 
#        main = c(paste('Firm = ',c$firm[1], '\nWell =',c$well[1],
#                       '\nTotal Oil =',total_oil,
#                       '| Total Num Static Periods=', total_static)),ylab = "")
#lines(c$water, col = 2)
#legend('topright', c('Oil', 'Water'), col = c(3,2), lty = c(1,1))

#dev.off()
#d = data.frame(table(c$oil[which(Delt(c$oil) == 0)]))
#names(d) = c('Static Oil Value','Frequency')
#grid.table(t(d))

result_summary[3,] = cbind(3, length(unique(b2$firm)), sum(b2$num_wells), sum(b2$oil_result))
result_final_3 = data.frame(Detector = "Detector 3", cbind(b))
#######################################################################
#Detector 4: Increase in Oil without an Increase in Water

a = sqldf("select firm, well, prod_date, oil, gas, water, days_prod 
          from data")

if(flag == 1){a = a[substr(a$prod_date,1,4) == y,]}

dt = data.table(a)
#dt_new = dt[,.(num_events = length(intersect(which(Delt(oil)>0), which(Delt(water)<=0)))), by = .(firm, well)]

dt_new = dt[,.(oil_result = sum(oil[(intersect(which(Delt(oil)>0), which(Delt(water)<=0)))]),
               num_events = length(intersect(which(Delt(oil)>0), which(Delt(water)<=0)))), 
            by = .(firm, well)]

setkey(dt, firm, well)
setkey(dt_new, firm, well)

a = dt[dt_new]

b = sqldf("select firm, well, sum(oil) as oil, sum(days_prod) as days_prod,
          max(oil_result) as oil_result, 
          max(num_events) as num_events 
          from a group by firm, well order by max(oil_result) desc")

b2 = sqldf("select firm, sum(oil) as oil, count(well) as num_wells, 
           sum(days_prod) as days_prod, sum(oil_result) as oil_result, sum(num_events) as num_events
           from b group by firm order by sum(oil_result) desc")

b2$pct_of_oil = round(b2$oil/sum(b2$oil),2)
b2$pct_of_oil_event = round(b2$oil_result/sum(b2$oil_result),2)

nrow(data); nrow(a);head(b2); summary(b2); sum(b2$oil); sum(b2$oil_result)

fraud_firms = b[b$num_events >= quantile(b$num_events, 0.9),]
x_list = unique(fraud_firms$well)
fraud_firm_wells = sample(x_list,1)
#fraud_firm_wells = unique(fraud_firms$well)[6]
c = a[a$well == fraud_firm_wells,];
total_events = length(intersect(which(Delt(c$oil)>0), which(Delt(c$water)<=0)))
total_oil = sum(c[(intersect(which(Delt(c$oil)>0), which(Delt(c$water)<=0)))]$oil)

#ts.plot(c$oil, col = 3, 
#        main = c(paste('Firm = ',c$firm[1], '\nWell =',c$well[1],
#                       '\nTotal Oil =',total_oil,
#                       '| Total Num Events =', total_events)),ylab = "")
#lines(c$water, col = 2)
#legend('topright', c('Oil', 'Water'), col = c(3,2), lty = c(1,1))

#dev.off()
#d = data.frame(table(c[(intersect(which(Delt(c$oil)>0), which(Delt(c$water)<=0)))]$oil))
#names(d) = c('Increase Val','Frequency')
#grid.table(t(d))

result_summary[4,] = cbind(4, length(unique(b2$firm)), sum(b2$num_wells), sum(b2$oil_result))
result_final_45 = data.frame(Detector = "Detector 4", cbind(b))
##################################################################
#Detector 5: Sharp uptick in water productimon

a = sqldf("select firm, well, prod_date, oil, gas, water, days_prod 
          from data where days_prod >=28")

if(flag == 1){a = a[substr(a$prod_date,1,4) == y,]}

#if(flag == 1){a1 = a[substr(a$prod_date,1,4) == as.character(y_list[i]-1),]}
#if(flag == 1){a2 = a[substr(a$prod_date,1,4) == y,]}
#a = rbind(a1,a2)

dt = data.table(a)
#dt_new = dt[,.(num_events = length(which(water > 3*runmed(water, 365)))), by = .(firm, well)]

dt_new = dt[,.(oil_result = sum(oil[(which(water > 3*runmed(water, 365)))]),
               num_events = length(which(water > 3*runmed(water, 365)))), 
            by = .(firm, well)]

head(dt_new)

setkey(dt, firm, well)
setkey(dt_new, firm, well)

a = dt[dt_new]

b = sqldf("select firm, well, sum(oil) as oil, sum(days_prod) as days_prod,
          max(oil_result) as oil_result, 
          max(num_events) as num_events 
          from a group by firm, well order by max(oil_result) desc")

b2 = sqldf("select firm, sum(oil) as oil, count(well) as num_wells, 
           sum(days_prod) as days_prod, sum(oil_result) as oil_result, sum(num_events) as num_events
           from b group by firm order by sum(oil_result) desc")

b2$pct_of_oil = round(b2$oil/sum(b2$oil),2)
b2$pct_of_oil_event = round(b2$oil_result/sum(b2$oil_result),2)

nrow(data); nrow(a);head(b2); summary(b2); sum(b2$oil); sum(b2$oil_result)

fraud_firms = b[b$num_events >= quantile(b$num_events, 0.9),]
x_list = unique(fraud_firms$well)
fraud_firm_wells = sample(x_list,1)
#fraud_firm_wells = unique(fraud_firms$well)[6]
c = data[data$well == fraud_firm_wells,];
total_events = length(which(c$water > 3*runmed(c$water, 365)))
total_oil = sum(c[(which(c$water > 3*runmed(c$water, 365))),]$oil)

result_final_45 = rbind(result_final_45,data.frame(Detector = "Detector 5", cbind(b)))

result_summary[5,] = cbind(5, length(unique(b2$firm)), sum(b2$num_wells), sum(b2$oil_result))

total_fraud_oil = sum(result_summary[1:5,4])

result_summary[6,] = cbind('Total', sum(result_summary[,2]), 
                           sum(result_summary[,3]), 
                           sum(result_summary[,4]))

write.csv(result_final_12, "result_final_12.csv")
write.csv(result_final_3, "result_final_3.csv")
write.csv(result_final_45, "result_final_45.csv")
write.csv(result_summary, "result_summary.csv")
result_summary

library(ORCH)
#write/read results to hdfs
x <- hdfs.put(result_summary, dfs.name = "result_summary_hdfs", overwrite = TRUE)
x = hdfs.get(x)
hdfs.ls()
x

Sys.time()-timer


#ts.plot(c$oil, col = 3, 
#        main = c(paste('Firm = ',c$firm[1], '\nWell =',c$well[1],
#                       '\nTotal Oil =',total_oil,
#                       '| Total Num Events =', total_events)),ylab = "")
#lines(c$water, col = 2)
#legend('topright', c('Oil', 'Water'), col = c(3,2), lty = c(1,1))

#dev.off()
#d = data.frame(table(c[(which(c$water > 3*runmed(c$water, 365))),]$oil))
#names(d) = c('Sharp uptick Val','Frequency')
#grid.table(t(d))


#result_summary[,3] = prettyNum(result_summary[,3], ",")
#result_summary[,4] = prettyNum(result_summary[,4], ",")

#head(result_summary)
#y_list_result[i,] = cbind(y, total_fraud_oil)

#}

#Enable for loop on line 75 first. The below calculates fraud as a pct of total production by year
#result = sqldf("select data.yr, sum(data.oil) as oil, y_list_result.total_oil_derived from data inner join y_list_result 
#on data.yr = y_list_result.yr 
#where data.yr <= '2015' group by data.yr")

#result$total_oil_derived = as.numeric(result$total_oil_derived)

#result$fraud_pct = round(result$total_oil_derived/result$oil,2)

#write.csv(result, "C:/Users/husabbas/Desktop/SOC-DOGGR/result.csv")

#result


