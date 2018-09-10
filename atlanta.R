atlanta <- read.csv('/home/ab/Downloads/Datasets for analysis/Crime Data/atlanta_crime.csv', header = TRUE, stringsAsFactors = FALSE)
atlanta <- atlanta[nchar(atlanta$UCR.Literal) > 2,]
atlanta <- atlanta[atlanta$UCR.Literal != 'Morning Watch' & atlanta$UCR.Literal != 'Evening Watch',]
atlanta$observations <- rep(1, nrow(atlanta))
atlanta$occur_date <- as.Date(atlanta$Report.Date)
crime_by_date <- aggregate(observations ~ occur_date, atlanta, sum)
write.csv(crime_by_date,'/home/ab/Downloads/Datasets for analysis/Crime Data/atlanta_daily_crime.csv')

#crime_by_date <- crime_by_date[crime_by_date$occur_date < '2017-01-01',]
plot(ts(crime_by_date$observations, frequency = 365, start = c(2013,1,1)))
weather_atlanta <- read.csv('/home/ab/Downloads/Datasets for analysis/Crime Data/atlanta_weather_12_17.csv', header = TRUE, stringsAsFactors = FALSE)
weather_atlanta$DATE <- as.Date(weather_atlanta$DATE)
daily_weather <- aggregate(DAILYMaximumDryBulbTemp ~ DATE, weather_atlanta, function(x){max(na.omit(x))})
plot(ts(daily_weather$DAILYMaximumDryBulbTemp))
crime_weather <- merge(crime_by_date, daily_weather, by.x = 'occur_date', by.y = 'DATE')
plot(x = crime_weather$DAILYMaximumDryBulbTemp, y = crime_weather$observations, xlim = c(0,110), ylim = c(0,140))
cor(crime_weather$DAILYMaximumDryBulbTemp, crime_weather$observations)
lm_crime <- lm(crime_weather$observations ~ crime_weather$DAILYMaximumDryBulbTemp)
summary(lm_crime)
#0.5127. The correlation between temperature and no of crimes committed is 0.5127.
rm(atlanta)

baltimore <- read.csv('/home/ab/Downloads/Datasets for analysis/Crime Data/crime_in_baltimore.csv', header = TRUE, stringsAsFactors = FALSE)
baltimore$CrimeDate <- as.Date(baltimore$CrimeDate,'%m/%d/%Y')
b_crime_by_date <- aggregate(Total.Incidents ~ CrimeDate,  baltimore, sum)
plot(ts(b_crime_by_date$Total.Incidents))

b_crime_type_by_date <- aggregate(Total.Incidents ~ Description + CrimeDate,  baltimore, sum)
plot(ts(b_crime_type_by_date$Total.Incidents[b_crime_type_by_date$Description == 'AGG. ASSAULT']))

weather_baltimore <- read.csv('~/Downloads/Datasets for analysis/Crime Data/baltimore_weather.csv', header = TRUE, stringsAsFactors = FALSE)
weather_baltimore$DATE <- as.Date(weather_baltimore$DATE)
weather_b <- aggregate(TMAX ~ DATE, weather_baltimore, max)
plot(ts(weather_b$TMAX))
b_crime_weather <- merge(b_crime_by_date, weather_b, by.x = 'CrimeDate', by.y = 'DATE')
# b_crime_weather$days <- rep(1, nrow(b_crime_weather))
# b_weather <- aggregate(days ~ TMAX, b_crime_weather, sum)
plot(x = b_crime_weather$TMAX, y = b_crime_weather$Total.Incidents, xlim = c(0,120), ylim = c(0,250))
lm_crime_baltimore <- lm(Total.Incidents ~ TMAX,b_crime_weather)
summary(lm_crime_baltimore)

cor(b_crime$Total.Incidents, b_crime$TMAX)
#The correlation between max temperature and crimes committed in Baltimore is 0.9
unique(b_crime_type_by_date$Description)
for(i in unique(b_crime_type_by_date$Description))
{b_agg_assault <- merge(b_crime_type_by_date[b_crime_type_by_date$Description == i,], weather_b,
                       by.x = 'CrimeDate', by.y = 'DATE')
print("Assault:\n")
print(i)
agg_assault_by_temp <- aggregate(Total.Incidents ~ TMAX, b_agg_assault, mean)
plot(x = agg_assault_by_temp$TMAX, y = agg_assault_by_temp$Total.Incidents, main = i)
}
rm(baltimore)

#Vancouver
vancouver <- read.csv('~/Downloads/Datasets for analysis/Crime Data/crime-in-vancouver/crime.csv', header = TRUE, stringsAsFactors = FALSE)
vancouver <- vancouver[vancouver$YEAR >= 2012,]
vancouver$date <- paste0(vancouver$YEAR,'-',vancouver$MONTH,'-',vancouver$DAY)
vancouver$date <- as.Date(vancouver$date, '%Y-%m-%d')
#To scrape weather data, use package 'weathercan'
library(weathercan)
library(rclimateca)
# van_stations <- stations_search("Vancouver", interval = "day")
# vancouver_weather <- weather_dl(station_ids = 895, start = '2012-01-01', end = '2017-07-13', interval = 'day')

#ec_climate_geosearch_locations("Vancouver", timeframe = 'daily', year = 2012:2017)
vancouver_weather <- ec_climate_data("VANCOUVER HARBOUR CS BC", timeframe = "daily", start = "2012-01-01", end = "2017-07-13")
library(weathermetrics)
vancouver_weather$max_temp_f <- celsius.to.fahrenheit(vancouver_weather$max_temp_c)

vancouver$incident <- rep(1,nrow(vancouver))
v_crime_by_date <- aggregate(incident ~ date, vancouver, sum)
plot(ts(v_crime_by_date$incident))
vancouver_weather$date <- as.Date(vancouver_weather$date)
v_weather <- aggregate(max_temp_f ~ date, vancouver_weather, sum)
date_range <- seq(min(v_weather$date), max(v_weather$date), by = 1)
date_range <- data.frame(date_range)
v_weather <- merge(date_range, v_weather, by.x = 'date_range', by.y= 'date', all.x = TRUE)
date_range[!date_range %in% v_weather$date]
library(zoo)
v_weather$max_temp_f <- na.locf(v_weather$max_temp_f)
v_crime_weather <- merge(v_crime_by_date, v_weather, by.x = 'date', by.y = 'date_range')
plot(x = v_crime_weather$max_temp_f, y = v_crime_weather$incident, xlim = c(0,90), ylim = c(0,160))
lm_crime_vancouver <- lm(incident ~ max_temp_f, v_crime_weather)
summary(lm_crime_vancouver)
cor(v_crime_weather$max_temp_f, v_crime_weather$incident)
#The correlation in Vancouver is 0.29629
rm(vancouver)

#Baton Rouge
baton_rouge <- read.csv('~/Downloads/Datasets for analysis/Crime Data/Baton_Rouge_Crime_Incidents.csv', header = TRUE, stringsAsFactors = FALSE)
baton_rouge$OFFENSE.DATE <- as.Date(baton_rouge$OFFENSE.DATE, '%m/%d/%Y')
baton_rouge <- baton_rouge[baton_rouge$OFFENSE.DATE >= '2012-01-01' & baton_rouge$OFFENSE.DATE <= '2017-08-31',]
baton_rouge$incident <- rep(1, nrow(baton_rouge))
br_crime_by_date <- aggregate(incident ~ OFFENSE.DATE, baton_rouge, sum)
plot(ts(br_crime_by_date$incident, frequency = 365.25, start = c(2011,1)))
baton_rouge_weather <- read.csv('~/Downloads/Datasets for analysis/Crime Data/baton_rouge_climate.csv', header = TRUE, stringsAsFactors = FALSE)
baton_rouge_weather$DATE <- as.Date(baton_rouge_weather$DATE)
br_daily_weather <- aggregate(DAILYMaximumDryBulbTemp ~ DATE, baton_rouge_weather, function(x){max(na.omit(x))})
plot(ts(br_daily_weather$DAILYMaximumDryBulbTemp))
rm(baton_rouge_weather)
br_daily_weather <- br_daily_weather[br_daily_weather$DATE <= '2017-08-31',]
br_crime_by_date <- br_crime_by_date[br_crime_by_date$OFFENSE.DATE <= '2017-08-31',]
br_crime_weather <- merge(br_crime_by_date, br_daily_weather, by.x = 'OFFENSE.DATE', by.y = 'DATE')

plot(x = br_crime_weather$DAILYMaximumDryBulbTemp, y = br_crime_weather$incident, xlim = c(0,110), ylim = c(0,255))
lm_crime_baton_rouge <- lm(incident ~ DAILYMaximumDryBulbTemp, br_crime_weather)
summary(lm_crime_baton_rouge)

cor(br_crime_weather$DAILYMaximumDryBulbTemp, br_crime_weather$incident)
#The correlation between the temperature and mean crimes committed in Baton Rouge, is 0.188992.
rm(baton_rouge)
chicago <- read.csv('~/Downloads/Datasets for analysis/Crime Data/Chicago_Crimes_2012_to_2017.csv', header = TRUE, stringsAsFactors = FALSE)
chicago$Date <- as.Date(chicago$Date,'%m/%d/%Y')
chicago$incident <- rep(1, nrow(chicago))
c_crime_by_date <- aggregate(incident ~ Date, chicago, sum)
plot(ts(c_crime_by_date$incident))
chicago_weather <- read.csv('~/Downloads/Datasets for analysis/Crime Data/chicago_weather.csv', header = TRUE, stringsAsFactors = FALSE)
chicago_weather$DATE <- as.Date(chicago_weather$DATE)
c_daily_weather <- aggregate(DAILYMaximumDryBulbTemp ~ DATE, chicago_weather, function(x){max(na.omit(x))})
c_daily_weather$DAILYMaximumDryBulbTemp <- gsub("s","",c_daily_weather$DAILYMaximumDryBulbTemp)
c_daily_weather$DAILYMaximumDryBulbTemp <- as.numeric(c_daily_weather$DAILYMaximumDryBulbTemp)
c_daily_weather[is.na(c_daily_weather$DAILYMaximumDryBulbTemp),]
#There are two NAs
c_daily_weather$DAILYMaximumDryBulbTemp <- na.locf(c_daily_weather$DAILYMaximumDryBulbTemp)
c_daily_weather[is.na(c_daily_weather$DAILYMaximumDryBulbTemp),]
plot(ts(c_daily_weather$DAILYMaximumDryBulbTemp))
c_crime_weather <- merge(c_crime_by_date, c_daily_weather, by.x = 'Date', by.y = 'DATE')
plot(x = c_crime_weather$DAILYMaximumDryBulbTemp, y = c_crime_weather$incident, xlim = c(0,110), ylim = c(0,1400))

lm_crime_chicago <- lm(incident ~ DAILYMaximumDryBulbTemp, c_crime_weather)
summary(lm_crime_chicago)

cor(c_crime_weather$DAILYMaximumDryBulbTemp, c_crime_weather$incident)
#The correlation in Chicago is fucking 0.5792

rm(chicago)

#Minneapolis
minneapolis <- read.csv('~/Downloads/Datasets for analysis/Crime Data/minneapolis_crimes.csv', header = TRUE, stringsAsFactors = FALSE)
minneapolis$BeginDate <- as.Date(minneapolis$BeginDate)
minneapolis$incident <- rep(1,nrow(minneapolis))
m_crime_by_date <- aggregate(incident ~ BeginDate, minneapolis,sum)
plot(ts(m_crime_by_date$incident))
minneapolis_weather <- read.csv('~/Downloads/Datasets for analysis/Crime Data/minneapolis_weather.csv', header = TRUE, stringsAsFactors = FALSE)
minneapolis_weather$DATE <- as.Date(minneapolis_weather$DATE)
m_weather <- aggregate(TMAX ~ DATE, minneapolis_weather, max)
plot(ts(m_weather$TMAX))
m_crime_weather <- merge(m_crime_by_date, m_weather, by.x = 'BeginDate', by.y = 'DATE')
plot(x = m_crime_weather$TMAX, y = m_crime_weather$incident, xlim = c(0, 110), ylim = c(0,110))

lm_crime_minneapolis <- lm(incident ~ TMAX,m_crime_weather)
summary(lm_crime_minneapolis)

cor(m_crime_weather$TMAX, m_crime_weather$incident)
#Correlation in Minneapolis is 0.6024
rm(minneapolis)

#Philly
philly <- read.csv('~/Downloads/Datasets for analysis/Crime Data/philly_crime.csv', header = TRUE, stringsAsFactors = FALSE)
philly$Dispatch_Date <- as.Date(philly$Dispatch_Date)
max(philly$Dispatch_Date)
min(philly$Dispatch_Date)
philly <- philly[philly$Dispatch_Date >= '2012-03-23',]
philly$incident <- rep(1,nrow(philly))
p_crime_by_date <- aggregate(incident ~ Dispatch_Date, philly, sum)
plot(ts(p_crime_by_date$incident))
philly_weather <- read.csv('~/Downloads/Datasets for analysis/Crime Data/philly_weather.csv', header = TRUE, stringsAsFactors = FALSE)
philly_weather$DATE <- as.Date(philly_weather$DATE)
#library(devtools)
p_daily_weather <- aggregate(DAILYMaximumDryBulbTemp ~ DATE, philly_weather, function(x){max(na.omit(x))})
plot(ts(p_daily_weather$DAILYMaximumDryBulbTemp))
p_crime_weather <- merge(p_crime_by_date, p_daily_weather, by.x = 'Dispatch_Date', by.y = 'DATE')
plot(x = p_crime_weather$DAILYMaximumDryBulbTemp, y = p_crime_weather$incident)

lm_crime_philly <- lm(incident ~ DAILYMaximumDryBulbTemp, p_crime_weather)
summary(lm_crime_philly)

cor(p_crime_weather$DAILYMaximumDryBulbTemp, p_crime_weather$incident)
#Correlation is 0.5697

nyc <- read.csv('~/Downloads/Datasets for analysis/Crime Data/new_york_crime.csv', header = TRUE, stringsAsFactors = FALSE)
nyc$CMPLNT_FR_DT <- as.Date(nyc$CMPLNT_FR_DT,'%m/%d/%Y')
nyc$RPT_DT <- as.Date(nyc$RPT_DT,'%m/%d/%Y')
nyc <- nyc[nyc$RPT_DT >= '2013-11-01',]
nyc$incident <- rep(1,nrow(nyc))
ny_crime_by_date <- aggregate(incident ~ RPT_DT, nyc, sum)
plot(ts(ny_crime_by_date$incident))
ny_weather <- read.csv('~/Downloads/Datasets for analysis/Crime Data/nyc_weather.csv',header = TRUE, stringsAsFactors = FALSE)
ny_weather$DATE <- as.Date(ny_weather$DATE)
ny_daily_weather <- aggregate(DAILYMaximumDryBulbTemp ~ DATE, ny_weather, function(x){max(na.omit(x))})
plot(ts(ny_daily_weather$DAILYMaximumDryBulbTemp))
ny_crime_weather <- merge(ny_crime_by_date, ny_daily_weather, by.x = 'RPT_DT', by.y = 'DATE')
ny_crime <- aggregate(incident ~ DAILYMaximumDryBulbTemp, ny_crime_weather, mean)
plot(x = ny_crime$DAILYMaximumDryBulbTemp, y = ny_crime$incident)

lm_crime_ny <- lm(incident ~ DAILYMaximumDryBulbTemp, ny_crime)
summary(lm_crime_ny)

cor(ny_crime$DAILYMaximumDryBulbTemp, ny_crime$incident)
#Correlation is 0.896

#Portland, Oregon
folder <- "~/Downloads/Datasets for analysis/Crime Data/portland-oregon-crime-data/"      # path to folder that holds multiple .csv files
file_list <- list.files(path=folder, pattern="*.csv") # create list of all .csv files in folder

# read in each .csv file in file_list and rbind them into a data frame called data 
portland <- 
  do.call("rbind", 
          lapply(file_list, 
                 function(x) 
                   read.csv(paste(folder, x, sep=''), 
                            stringsAsFactors = FALSE)))
portland$Report.Date <- as.Date(portland$Report.Date, '%m/%d/%Y')
portland$incident <- rep(1,nrow(portland))
po_crime_by_date <- aggregate(incident ~ Report.Date, portland, sum)
plot(ts(po_crime_by_date$incident))
po_weather <- read.csv('~/Downloads/Datasets for analysis/Crime Data/portland_weather.csv', header = TRUE, stringsAsFactors = FALSE)
po_weather$DATE <- as.Date(po_weather$DATE)
po_daily_weather <- aggregate(DAILYMaximumDryBulbTemp ~ DATE, po_weather, function(x){max(na.omit(x))})
plot(ts(po_daily_weather$DAILYMaximumDryBulbTemp))
po_crime_weather <- merge(po_crime_by_date, po_daily_weather, by.x = 'Report.Date', by.y = 'DATE')
po_crime <- aggregate(incident ~ DAILYMaximumDryBulbTemp, po_crime_weather, mean)
plot(x = po_crime$DAILYMaximumDryBulbTemp, y = po_crime$incident)
lm_crime_portland <- lm(incident ~ DAILYMaximumDryBulbTemp, po_crime)
summary(lm_crime_portland)
cor(po_crime$DAILYMaximumDryBulbTemp, po_crime$incident)
#Correlation is 0.5357

#Los Angeles
la <- read.csv('~/Downloads/Datasets for analysis/Crime Data/crime-in-los-angeles/Crime_Data_2010_2017.csv', header = TRUE, stringsAsFactors = FALSE)
la$incident <- rep(1,nrow(la))
la$Date.Occurred <- as.Date(la$Date.Occurred,'%m/%d/%Y')
la <- la[la$Date.Occurred >= '2012-01-01',]
la_crime_by_date <- aggregate(incident ~ Date.Occurred, la,sum)
plot(ts(la_crime_by_date$incident))
la_weather <- read.csv('~/Downloads/Datasets for analysis/Crime Data/la_weather.csv', header = TRUE, stringsAsFactors = FALSE)


#Type of crime
la_crime_type_by_date <- aggregate(incident ~ Crime.Code.Description + Date.Occurred, la,sum)
la_weather$DATE <- as.Date(la_weather$DATE)
la_daily_weather <- aggregate(DAILYMaximumDryBulbTemp ~ DATE, la_weather, function(x){max(na.omit(x))})
plot(ts(la_daily_weather$DAILYMaximumDryBulbTemp))
la_crime_weather <- merge(la_crime_by_date, la_daily_weather, by.x = 'Date.Occurred', by.y = 'DATE')
la_crime <- aggregate(incident ~ DAILYMaximumDryBulbTemp, la_crime_weather, mean)
plot(x = la_crime$DAILYMaximumDryBulbTemp, y = la_crime$incident)
lm_crime_la <- lm(incident ~ DAILYMaximumDryBulbTemp, la_crime)
summary(lm_crime_la)
cor(la_crime$DAILYMaximumDryBulbTemp, la_crime$incident)
#Correlation is 0.764

