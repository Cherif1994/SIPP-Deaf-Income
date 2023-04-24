# Libraries---------------------------------------------------------------------
library(data.table)
library(tidygeocoder)
library(bit64)
library(dplyr)
library(tidyr)
library(plm)
library(sandwich)
library(lmtest)
library(VGAM)
library(car)
library(blscrapeR)

# Obtain PU 2021 csv.zip dataset
dir<-paste(getwd(),'Asset/pu2021_csv.zip',sep='/')
if(file.exists(dir)){
  unzipped<-unzip(dir)
}else{
  url<-'https://www2.census.gov/programs-surveys/sipp/data/datasets/2021/pu2021_csv.zip'
  download.file(url,dir, method = 'curl')
  unzipped<-unzip(dir)
}

pu <- fread(unzipped, sep = "|", select = c(
  
  #Common case identification variables
  'SSUID','PNUM','SHHADID','MONTHCODE','ERESIDENCEID','ERELRPE','SPANEL','SWAVE',
  
  #The base weight
  'WPFINWGT',
  
  #Common demographics variables, including age at time of interview (TAGE)
  #	and monthly age during the reference period (TAGE_EHC)
  'ESEX','TAGE','TAGE_EHC','ERACE','TRACE','EHISPAN','EORIGIN','EEDUC','EHEARING',
  
  #Disabilities
  'EAMBULAT','ECOGNIT','EDIF10','EERRANDS','EFINDJOB','EGRASPD',
  'ESEEING','ESELFCARE','ESITD',
  #Example additional variables for analysis
  'TPEARN','TMWKHRS','TJB1_OCC','TJB2_OCC',
  'TJB3_OCC','TJB4_OCC','TJB5_OCC','TJB6_OCC',
  'TJB7_OCC','EHOWWELL','ESPEAK','TEHC_ST'))

#Make sure all the column names are upper-case
names(pu) <- toupper(names(pu))

#Preview the data
head(pu, 20)

#check some means against the validation xls file to help ensure that the data
#	were read in correctly. Note that the validation xls files do not include all variables.
mean(pu[["TPTOTINC"]], na.rm = TRUE)

##Read in the replicate-weight data. This dataset is small enough that most machines
##	can read the whole file into memory
#dir<-paste(getwd(),'Asset/rw2021_csv.zip',sep='/')
#unzipped<-unzip(dir)
#rw <- fread(unzipped, sep = "|")
#
##Make sure all the column names are upper-case
#names(rw) <- toupper(names(rw))
#
##Preview the data
#head(rw, 20)
#
##check some means against the validation xls file to help ensure that the data
##	were read in correctly. Note that the validation xls files do not include all variables.
#mean(rw[["REPWGT100"]], na.rm = TRUE)
#
##Merge primary data and replicate weights on SSUID, PNUM, MONTHCODE, SPANEL, and SWAVE
#data <- inner_join(pu, rw, by = c("SSUID","PNUM","MONTHCODE", "SPANEL", "SWAVE"))
#
##preview the merged data
#head(data, 20)

# Get unique identifier of each person
pu<-pu%>%
  unite(col = 'identifier', c('SSUID','PNUM'), sep = '')%>%
  filter(TAGE > 14 & TAGE < 65)

# Notes-------------------------------------------------------------------------
# Holy Grail: https://www.princeton.edu/~otorres/Panel101R.pdf
# https://algoritmaonline.com/regression-with-panel-data/

# Occupation Codes for Healthcare
#	1821. Clinical and counseling psychologists
#	1822. School psychologists
#	1825. Other psychologists
#	2001. Substance abuse and behavioral disorder counselors
#	2003. Marriage and family therapists
#	2004. Mental health counselors
#	2005. Rehabilitation counselors
#	2006. Counselors, all other
#	2011. Child, family, and school social workers
#	2012. Healthcare social workers
#	2013. Mental health and substance abuse social workers
#	2014. Social workers, all other
#	2016. Social and human service assistants
#	3000. Chiropractors
#	3010. Dentists
#	3030. Dietitians and nutritionists
#	3040. Optometrists
#	3050. Pharmacists
#	3090. Physicians
#	3100. Surgeons
#	3110. Physician assistants
#	3120. Podiatrists
#	3140. Audiologists
#	3150. Occupational therapists
#	3160. Physical therapists
#	3200. Radiation therapists
#	3210. Recreational therapists
#	3220. Respiratory therapists
#	3230. Speech-language pathologists
#	3245. Other therapists
#	3255. Registered nurses
#	3256. Nurse anesthetists
#	3258. Nurse practitioners, and nurse midwives
#	3261. Acupuncturists
#	3270. Healthcare diagnosing or treating practitioners, all other
#	3300. Clinical laboratory technologists and technicians
#	3310. Dental hygienists
#	3321. Cardiovascular technologists and technicians
#	3322. Diagnostic medical sonographers
#	3323. Radiologic technologists and technicians
#	3324. Magnetic resonance imaging technologists
#	3330. Nuclear medicine technologists and medical dosimetrists
#	3401. Emergency medical technicians
#	3402. Paramedics
#	3421. Pharmacy technicians
#	3422. Psychiatric technicians
#	3423. Surgical technologists
#	3430. Dietetic technicians and ophthalmic medical technicians
#	3500. Licensed practical and licensed vocational nurses
#	3515. Medical records specialists
#	3520. Opticians, dispensing
#	3545. Miscellaneous health technologists and technicians
#	3550. Other healthcare practitioners and technical occupations
#	3601. Home health aides
#	3602. Personal care aides
#	3603. Nursing assistants
#	3605. Orderlies and psychiatric aides
#	3610. Occupational therapy assistants and aides
#	3620. Physical therapist assistants and aides
#	3630. Massage therapists
#	3640. Dental assistants
#	3645. Medical assistants
#	3646. Medical transcriptionists
#	3647. Pharmacy aides
#	3649. Phlebotomists
occ_recoded<-c(
  'Clinical and counseling psychologists',
  'School psychologists',
  'Other psychologists',
  'Substance abuse and behavioral disorder counselors',
  'Marriage and family therapists',
  'Mental health counselors',
  'Rehabilitation counselors',
  'Counselors, all other',
  'Child, family, and school social workers',
  'Healthcare social workers',
  'Mental health and substance abuse social workers',
  'Social workers, all other',
  'Social and human service assistants',
  'Chiropractors',
  'Dentists',
  'Dietitians and nutritionists',
  'Optometrists',
  'Pharmacists',
  'Physicians',
  'Surgeons',
  'Physician assistants',
  'Podiatrists',
  'Audiologists',
  'Occupational therapists',
  'Physical therapists',
  'Radiation therapists',
  'Recreational therapists',
  'Respiratory therapists',
  'Speech-language pathologists',
  'Other therapists',
  'Registered nurses',
  'Nurse anesthetists',
  'Nurse practitioners, and nurse midwives',
  'Acupuncturists',
  'Healthcare diagnosing or treating practitioners, all other',
  'Clinical laboratory technologists and technicians',
  'Dental hygienists',
  'Cardiovascular technologists and technicians',
  'Diagnostic medical sonographers',
  'Radiologic technologists and technicians',
  'Magnetic resonance imaging technologists',
  'Nuclear medicine technologists and medical dosimetrists',
  'Emergency medical technicians',
  'Paramedics',
  'Pharmacy technicians',
  'Psychiatric technicians',
  'Surgical technologists',
  'Dietetic technicians and ophthalmic medical technicians',
  'Licensed practical and licensed vocational nurses',
  'Medical records specialists',
  'Opticians, dispensing',
  'Miscellaneous health technologists and technicians',
  'Other healthcare practitioners and technical occupations',
  'Home health aides',
  'Personal care aides',
  'Nursing assistants',
  'Orderlies and psychiatric aides',
  'Occupational therapy assistants and aides',
  'Physical therapist assistants and aides',
  'Massage therapists',
  'Dental assistants',
  'Medical assistants',
  'Medical transcriptionists',
  'Pharmacy aides',
  'Phlebotomists'
)

occ<-c(1821,1822,1825,2001,2003,
       2004,2005,2006,2011,2012,
       2013,2014,2016,3000,3010,
       3030,3040,3050,3090,3100,
       3110,3120,3140,3150,3160,
       3200,3210,3220,3230,3245,
       3255,3256,3258,3261,3270,
       3300,3310,3321,3322,3323,
       3324,3330,3401,3402,3421,
       3422,3423,3430,3500,3515,
       3520,3545,3550,3601,3602,
       3603,3605,3610,3620,3630,
       3640,3645,3646,3647,3649)

pu<-pu%>%
  mutate(inhealthjob = ifelse(
    TJB1_OCC %in% occ |
      TJB2_OCC %in% occ |
      TJB3_OCC %in% occ |
      TJB4_OCC %in% occ |
      TJB5_OCC %in% occ |
      TJB6_OCC %in% occ |
      TJB7_OCC %in% occ, 1, 0
  ))


occupations<-data.frame(occ = occ, occ_recoded)
# Understanding panel data structure--------------------------------------------
testtest<-pu%>% # If without MONTHCODE
  group_by(identifier,SPANEL)%>%
  summarise(n=n())

summary(testtest$n)

testtest<-pu%>% # If without SPANEL
  group_by(identifier,MONTHCODE)%>%
  summarise(n=n())

summary(testtest$n)

pu%>%
  group_by(SPANEL)%>%
  summarise(n=length(unique(identifier)))
# Clean up data-----------------------------------------------------------------
# Identify data to healthcare workers given period of time
tjb_occs <- c()
for(i in 1:7){
  tjb_occs[i]<-paste('TJB',i,'_OCC',sep='')
}
health<-pu%>%
  filter_at(vars(tjb_occs), any_vars(. %in% occ))

nonhealth<-pu%>%
  filter_at(vars(tjb_occs), all_vars(!(. %in% occ)))

# Binarize health workers and not

pu<-pu%>%
  mutate(inhealthjob = ifelse(
    TJB1_OCC %in% occ |
    TJB2_OCC %in% occ |
    TJB3_OCC %in% occ |
    TJB4_OCC %in% occ |
    TJB5_OCC %in% occ |
    TJB6_OCC %in% occ |
    TJB7_OCC %in% occ, 1, 0
  ))

# Count numbers of jobs per person given a period of time
pu$jobNum<-rowSums(!is.na(select(pu,tjb_occs)))

# Reset number order of EEDUC
pu$EEDUC<-pu$EEDUC-30

# If number of jobs is zero, then TEARN and TMWKHRS should be zero
pu<-pu%>%
  mutate(TMWKHRS = ifelse(jobNum == 0 & is.na(TMWKHRS),0,TMWKHRS),
         TPEARN = ifelse(jobNum == 0 & is.na(TPEARN),0,TPEARN))

# Remove specific respondents from data if NANs are in one of them
cleaned_pu<-pu%>%select('identifier','MONTHCODE','SPANEL','TPEARN',
                        'TMWKHRS','TAGE','SPANEL','EEDUC',
                        'EHEARING','jobNum','ESEX','inhealthjob',
                        'EORIGIN','ERACE','TEHC_ST')

id_with_NA<-cleaned_pu[!complete.cases(cleaned_pu), ]$identifier 

cleaned_pu<-cleaned_pu[!(identifier %in% id_with_NA)]

# Binarize 2020
cleaned_pu<-cleaned_pu%>%
  mutate(COVIDpandemic = ifelse(SPANEL == 2020, 1, 0))

# Convert residence of state into longitude and latitude
if(!file.exists('Asset/state.csv')){
  locations<-tibble::tribble(
    ~name,~addr, ~lat, ~long,
    1, 'Alabama', 32.31823, -86.90230,
    2, 'Alaska', 66.16051, -153.36914,
    4, 'Arizona', 34.04893, -111.09373,
    5, 'Arkansas', 34.80000, -92.20000,
    6, 'California', 36.77826, -119.41793,
    8, 'Colorado', 39.11301, -105.35889,
    9, 'Connecticut', 41.60000, -72.70000,
    10, 'Delaware', 27.99440, -81.76025,
    11, 'District of Columbia', 38.94214, -77.02595, 
    12, 'Florida', 27.99440, -81.76025,
    13, 'Georgia', 33.24788, -83.44116,
    15, 'Hawaii', 19.74176, -155.84444,
    16, 'Idaho', 44.06820, -114.74204,
    17, 'Illinois', 40.00000, -89.00000,
    18, 'Indiana', 40.27350, -86.12698,
    19, 'Iowa', 42.03297, -93.58154,
    20, 'Kansas', 38.50000, -98.00000,
    21, 'Kentucky', 37.83933, -84.27002,
    22, 'Louisiana', 30.39183, -92.32910,
    23, 'Maine', 45.36758, -68.97217,
    24, 'Maryland', 39.04575, -76.64127,
    25, 'Massachusetts', 42.40721, -71.38244,
    26, 'Michigan', 44.18221, -84.50684,
    27, 'Minnesota', 46.39241, -94.63623,
    28, 'Mississippi', 33.00000, -90.00000,
    29, 'Missouri', 38.57394, -92.60376,
    30, 'Montana', 46.96526, -109.53369,
    31, 'Nebraska', 41.50000, -100.00000,
    32, 'Nevada', 39.87602, -117.22412,
    33, 'New Hampshire', 44.00000, -71.50000,
    34, 'New Jersey', 39.83385, -74.87183,
    35, 'New Mexico', 34.30714, -106.01807,
    36, 'New York', 43.00000, -75.00000,
    37, 'North Carolina', 35.78217, -80.79346,
    38, 'North Dakota', 47.65059, -100.43701,
    39, 'Ohio', 40.36747, -82.99622,
    40, 'Oklahoma', 36.08462, -96.92139,
    41, 'Oregon', 44.00000, -120.50000,
    42, 'Pennsylvania', 41.20332, -77.19453,
    44, 'Rhode Island', 41.74233, -71.74233,
    45, 'South Carolina', 33.83608, -81.16373,
    46, 'South Dakota', 44.50000, -100.00000,
    47, 'Tennessee', 35.86012, -86.66016,
    48, 'Texas', 31.00000, -100.00000,
    49, 'Utah', 39.41922, -111.95068,
    50, 'Vermont', 44.00000, -72.70000,
    51, 'Virginia', 37.92687, -78.02490,
    53, 'Washington', 47.75108, -120.74013,
    54, 'West Virginia', 39.00000, -80.50000,
    55, 'Wisconsin', 44.50000, -89.50000,
    56, 'Wyoming', 43.07597, -107.29028,
    60, 'Puerto Rico and Island Areas', 18.46633, -66.10572
    )
  
  write.csv(locations, file = 'Asset/state.csv')
}else{
  locations<-read.csv('Asset/state.csv')
}

cleaned_pu<-cleaned_pu%>%
  left_join(locations, by = c('TEHC_ST'='name'))

# Obtain real income
if(file.exists('Asset/cpi.csv')){
  cpi<-read.csv('Asset/cpi.csv')
}else{
  cpi <- bls_api("CUSR0000SA0",
                 startyear = min(cleaned_pu$SPANEL), endyear = max(cleaned_pu$SPANEL))
  cpi<-cpi%>%
    mutate(period = as.integer(gsub('M','',cpi$period)))%>%
    select(year,period,value)
  write.csv(cpi, file = 'Asset/cpi.csv')
}

cleaned_pu<-cleaned_pu%>%
  left_join(select(cpi,-X),by = c('SPANEL' = 'year','MONTHCODE'='period'))%>%
  rename('current_cpi' = 'value')

cleaned_pu$base_cpi <- unlist(subset(cpi,period == 1 & year == 2018, select = value))

cleaned_pu<-cleaned_pu%>%
  mutate(inflation = (current_cpi - base_cpi)/100,
         real_income = (1-inflation)*TPEARN)

# Remove 2.5% of respondents with below zero earning
unwanted_list<-filter(cleaned_pu,real_income <  1 | real_income > quantile(cleaned_pu$real_income,probs = 0.99))$identifier
cleaned_pu<-filter(cleaned_pu, !(identifier %in% unwanted_list))

# Check sample size of each group of interest
cleaned_pu%>%
  group_by(SPANEL,MONTHCODE,EHEARING,inhealthjob)%>%
  summarise(n = n())%>%
  mutate(EHEARING = ifelse(EHEARING == 1, 'deaf','hearing'),
         inhealthjob = ifelse(inhealthjob == 1, 'health worker','any'))%>%
  pivot_wider(names_from = c('EHEARING','inhealthjob'), values_from = n)%>%View()


# Critique on colleague's work--------------------------------------------------
demographics<-select(pu,identifier,MONTHCODE,TAGE,TRACE,EORIGIN,ECOGNIT,EAMBULAT,
       ECOGNIT,EDIF10,EERRANDS,EFINDJOB,EGRASPD,ESEEING,ESELFCARE,
       ESITD,EHEARING,inhealthjob,EEDUC,ESEX)

is_mutable<-function(x){
  myvar<-sym(x)
  result<-demographics%>%
    group_by(identifier)%>%
    summarise(n = length(unique(!!myvar)))%>%
    filter(n > 1)
  if(dim(result)[1] > 1){
    return(paste0(x,': True'))
  }else{
    return(paste0(x,': False'))
  }
}

for(i in c('TAGE','TRACE','EORIGIN','ECOGNIT','EAMBULAT',
           'ECOGNIT','EDIF10','EERRANDS','EFINDJOB','EGRASPD',
           'ESEEING','ESELFCARE','ESITD','EHEARING','inhealthjob',
           'EEDUC','ESEX')){
  print(is_mutable(i))
}

# Make inhealthjob time-invariant for sample description
everchange<-demographics%>%
  group_by(identifier)%>%
  summarise(n = length(unique(inhealthjob)))%>%
  filter(n > 1)

demographics[demographics$identifier %in% everchange$identifier]$inhealthjob <- 1

demographics<-demographics%>% # Turn cross-sectional
  filter(MONTHCODE == 1)

# Age Mean, SD
demographics%>%
  group_by(EHEARING)%>%
  summarise(mean = mean(TAGE),sd = sd(TAGE))

# ESEX
sample_size<-function(x){
  result<-demographics%>%
    group_by(EHEARING, !!sym(x))%>%
    summarise(n = n())%>%
    na.omit()%>%
    return()
  
  result<-result%>%
    group_by(EHEARING)%>%
    summarise(N = sum(n))%>%
    left_join(result, by = 'EHEARING')%>%
    mutate(percentage = paste0(round(n/N*100,2),'%'))%>%
    select(EHEARING,!!sym(x),n,percentage)
  
  return(result)
}

sample_size('ESEX')

# RACETH
demographics<-demographics%>%
  mutate(RACETH = ifelse(EORIGIN == 1,'Latinx',
                  ifelse(TRACE == 1, 'White',
                  ifelse(TRACE == 2, 'Black',
                  ifelse(TRACE == 3, 'Native American',
                  ifelse(TRACE %in% c(4,5),'Asian','multiracial'))))))

sample_size('RACETH')

# Disability
demographics<-demographics%>%
  mutate(disability = ifelse((ECOGNIT == 1|EAMBULAT==1|EDIF10==1|EERRANDS==1|EFINDJOB==1|EGRASPD==1|ESELFCARE==1|ESITD==1)&(ESEEING==2),'Other',
                      ifelse(ESEEING==1,'Blind','No disability')))

sample_size('disability') # Note: this colleague accounts for more disability categories than mine

# Health worker
sample_size('inhealthjob')

# Education level
demographics<-demographics%>%
  mutate(education = ifelse(EEDUC < 9, 'No HS',
                     ifelse(EEDUC < 10, 'HS diploma',
                     ifelse(EEDUC < 12, 'Some college',
                     ifelse(EEDUC == 12, 'Associate',
                     ifelse(EEDUC < 14, 'Bachelor', 'At least Master'))))))

sample_size('education')


# Econometric analysis----------------------------------------------------------

# [X] real_income = Sum of earnings in month with inflation adjustment
# [X] TPEARN1 = lagged month of TEARN
# [X] TMWKHRS = Average Hours of Work Per Week
# [X] TMWKHRS2 = Square of TMWKHRS
# [X] EHEARING = Deaf/Hearing
# [X] TAGE = Age or proxy variable for years of working experience and late deafened
# [X] TAGE2 = Square of Age
# [X] identifier = i
# [X] MONTHCODE = t
# [X] SPANEL === 2020 = Covid-19
# [X] EEDUC = highest level of school or degree
# [X] Lat and Long <- TEHC_ST = Monthly state of residence
# [X] Count TJBX_OCC = Numbers of jobs
# [X] ESEX = Sex
# [X] ERACE + EORIGIN = Race/Ethnicity

cleaned_pu<-cleaned_pu %>%
  mutate(deafhealth = ifelse(inhealthjob == 1 & EHEARING == 1,1,0),
         hearhealth = ifelse(inhealthjob == 1 & EHEARING == 2,1,0),
         deafoutside = ifelse(inhealthjob == 0 & EHEARING == 1,1,0))

cleaned_pu<-cleaned_pu%>%
  mutate(ln_real_income = log(real_income),
         TAGE2 = TAGE**2,
         TMWKHRS_deafhealth = TMWKHRS*deafhealth,
         TMWKHRS_hearhealth = TMWKHRS*hearhealth,
         TMWKHRS_deafoutside = TMWKHRS*deafoutside)

cleaned_pu<-cleaned_pu%>%
  group_by(identifier)%>%
  mutate(ln_lag_real_income = dplyr::lag(ln_real_income, by = MONTHCODE))%>%
  ungroup()

form_for_test <- ln_real_income ~ ln_lag_real_income + deafhealth +
  hearhealth + deafoutside + TMWKHRS_deafhealth + TMWKHRS_hearhealth +
  TMWKHRS_deafoutside + TAGE + SPANEL + EEDUC + jobNum

form<-log(real_income) ~ lag(log(real_income)) + TMWKHRS*deafhealth +
  TMWKHRS*hearhealth + TMWKHRS*deafoutside +
  TAGE + SPANEL + EEDUC + jobNum

pl<-plm(form, data = cleaned_pu, 
        index = c('identifier','MONTHCODE'), model = 'pooling')

fe<-plm(form, data = cleaned_pu, 
        index = c('identifier','MONTHCODE'), model = 'within')

re<-plm(form, data = cleaned_pu, 
        index = c('identifier','MONTHCODE'), model = 'random')

# Diagnostics-------------------------------------------------------------------
pFtest(fe, pl) # Is fixed effect better than pooled ols? Yes if below 0.05 pvalue
phtest(fe,re) # Is fixed effect better than random effect? Yes if below 0.05 pvalue

phtest(form_for_test,data = cleaned_pu, method = "aux", vcov = vcovHC) # Robust Hausman test

pbgtest(fe) # Serial correlation? Yes if below 0.05 value
pcdtest(fe, test = c("cd")) # Contemporaneous correlation? Yes if below 0.05 pvalue
plmtest(fe, c("individual"), type=("bp")) # Heteroskedasticity? Yes if below 0.05 pvalue

# Is the functional form misspecified? Yes if below 0.05 pvalue
resettest(form, power = 3, 
          type = 'regressor', data = cleaned_pu)

fit <- predict(fe)
resid<-resid(fe)

plot(as.numeric(resid) ~ as.numeric(fit))

plot(as.numeric(resid) ~ seq(1,length(as.numeric(resid))))

# Use White Huber robust standard error
coeftest(fe,vcov = vcovHC(fe, method = 'arellano'))
within_intercept(fe, vcov = function(x) vcovHC(x, method="arellano"))

within_intercept(fe)


