library(Quandl)
library(dplyr)
library(ggplot2)
library(lubridate)
library(plm)


#----------------------------------------------------------------
# bring over the crime data from Quandl
MD <- Quandl("FBI_UCR/MARYLAND", api_key="1i2uuiN7DQ-Ltizgjb_q")
CA <- Quandl("FBI_UCR/CALIFORNIA", api_key="1i2uuiN7DQ-Ltizgjb_q")
NY <- Quandl("FBI_UCR/NEW_YORK", api_key="1i2uuiN7DQ-Ltizgjb_q")
FL <- Quandl("FBI_UCR/FLORIDA", api_key="1i2uuiN7DQ-Ltizgjb_q")
MA <- Quandl("FBI_UCR/MASSACHUSETTS", api_key="1i2uuiN7DQ-Ltizgjb_q")
NJ <- Quandl("FBI_UCR/NEW_JERSEY", api_key="1i2uuiN7DQ-Ltizgjb_q")
IL <- Quandl("FBI_UCR/ILLINOIS", api_key="1i2uuiN7DQ-Ltizgjb_q")
USA <- Quandl("FBI_UCR/UNITED_STATES_TOTAL", api_key="1i2uuiN7DQ-Ltizgjb_q")

#quick check to see how closely we match the Levitt paper
names(USA) <- c("year","pop",'total_violent','murder','rape','robbery','agg_assault','total_property','burglery','larceny-theft','car_theft')
USA$murder_cap <- USA$murder/(USA$pop/100000)
ggplot(USA,aes(x=year,y=murder_cap)) + geom_line() + ylim(0,12)
#--------------------------------------------------------------


#--------------------------------------------------------------
# Bring over the unemployment data

CA.ump <- Quandl("FRBC/UNEMP_ST_CA", api_key="1i2uuiN7DQ-Ltizgjb_q", collapse="annual")
MD.ump <- Quandl("FRBC/UNEMP_ST_MD", api_key="1i2uuiN7DQ-Ltizgjb_q", collapse="annual")
NY.ump <- Quandl("FRBC/UNEMP_ST_NY", api_key="1i2uuiN7DQ-Ltizgjb_q", collapse="annual")
FL.ump <- Quandl("FRBC/UNEMP_ST_FL", api_key="1i2uuiN7DQ-Ltizgjb_q", collapse="annual")
MA.ump <- Quandl("FRBC/UNEMP_ST_NJ", api_key="1i2uuiN7DQ-Ltizgjb_q", collapse="annual")
NJ.ump <- Quandl("FRBC/UNEMP_ST_NJ", api_key="1i2uuiN7DQ-Ltizgjb_q", collapse="annual")
IL.ump <- Quandl("FRBC/UNEMP_ST_IL", api_key="1i2uuiN7DQ-Ltizgjb_q", collapse="annual")

#--------------------------------------------------------------




#----------------------------------------------------------------------------------
#organize data in a single data frame
MD$state<-"MD"
CA$state<-'CA'
NY$state<-'NY'
FL$state<-'FL'
MA$state<-'MA'
NJ$state<-'NJ'
IL$state<-'IL'

crime <- rbind(MD,CA,NY,FL,MA,NJ,FL,IL)
names(crime) <- c("year","pop",'total_violent','murder','rape','robbery','agg_assault','total_property','burglery','larceny-theft','car_theft','state')
crime <- tbl_df(crime)
#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#murder rate per 100,000 residents by state with bars for 'broken windows' events

#march 1982 the original Kelling and Wilson article came out in the Atlantic
# 1990 Bratton became head of NYC Transit Police
# 1993 Bratton became head of NYPD

crime <- crime %>% mutate(murder_cap=murder/(pop/100000))
ggplot(crime,aes(x=year,y=murder_cap,color=state)) + geom_line() 

#show a few states together
ggplot(subset(crime,state %in% c("NJ","NY","MA","IL","CA")),aes(x=year,y=murder_cap,color=state)) + geom_line() + 
  geom_point() + geom_rect(aes(xmin=as.Date('1982-12-31'),
                               xmax=as.Date('1983-12-31'),ymin=-Inf,ymax=+Inf),fill='pink',alpha=0.02) + 
  geom_rect(aes(xmin=as.Date('1990-12-31'),
                xmax=as.Date('1991-12-31'),ymin=-Inf,ymax=+Inf),fill='pink',alpha=0.02) +
  geom_rect(aes(xmin=as.Date('1993-12-31'),
                xmax=as.Date('1994-12-31'),ymin=-Inf,ymax=+Inf),fill='pink',alpha=0.02) +
  theme_bw()

#------------------------------------------------------------------------------

##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
# Let's look at just the New York Data because they have a pretty nice data set 
# on crime across all areas of the state

ny.crime <- read.csv("data/arrests_NY.txt")

names(ny.crime) <- c('county','year','total','felony_total','drug_felony','violent_felony','dwi_felony','other_felony',
                     'mis_total','drug_mis','dwi_mis','prop_mis','oth_mis')

#get county populations
albany <- Quandl("FRED/NYALBA1POP", api_key="1i2uuiN7DQ-Ltizgjb_q")
albany$county='Albany'

essex <- Quandl("FRED/NYESSE1POP", api_key="1i2uuiN7DQ-Ltizgjb_q")
essex$county='Essex'

new_york_county <- Quandl("FRED/NYNEWY1POP", api_key="1i2uuiN7DQ-Ltizgjb_q") #nyc
new_york_county$county='New York'

kings <- Quandl("FRED/NYKING7POP", api_key="1i2uuiN7DQ-Ltizgjb_q")  #nyc
kings$county = 'Kings'

queens <- Quandl("FRED/NYQUEE1POP", api_key="1i2uuiN7DQ-Ltizgjb_q")  # nyc
queens$county = 'Queens'

bronx <- Quandl("FRED/NYBRON5POP", api_key="1i2uuiN7DQ-Ltizgjb_q")  #nyc
bronx$county = 'Bronx'

richmond <- Quandl("FRED/NYRICH5POP", api_key="1i2uuiN7DQ-Ltizgjb_q") #nyc
richmond$county='Richmond'

#schenectady <- Quandl("FRED/NYSCHE5POP", api_key="1i2uuiN7DQ-Ltizgjb_q")
#schenectday$county = 'schenectday'

cayuga <- Quandl("FRED/NYCAYU5POP", api_key="1i2uuiN7DQ-Ltizgjb_q")
cayuga$county = 'Cayuga'

allegany <- Quandl("FRED/NYALLE2POP", api_key="1i2uuiN7DQ-Ltizgjb_q")
allegany$county = 'Allegany'

erie <- Quandl("FRED/NYERIE9POP", api_key="1i2uuiN7DQ-Ltizgjb_q") # high pop density
erie$county = 'Erie'

nassau <- Quandl("FRED/NYNASS9POP", api_key="1i2uuiN7DQ-Ltizgjb_q") # high pop den
nassau$county = 'Nassau'

rockland <- Quandl("FRED/NYROCK5POP", api_key="1i2uuiN7DQ-Ltizgjb_q") # high pop den
rockland$county = 'Rockland'

westchester <- Quandl("FRED/NYWEST9POP", api_key="1i2uuiN7DQ-Ltizgjb_q") # high pop den
westchester$county = 'Westchester'

pop <- tbl_df(rbind(albany,essex,new_york_county,kings,queens,bronx,richmond,cayuga,allegany,
             erie,nassau,rockland,westchester)) %>% mutate(year=year(DATE))

#first let's look at violent crime in NYC counties
nyc.crime <- tbl_df(ny.crime) %>% filter(county %in% c('New York','Kings','Queens','Bronx','Richmond')) %>%
              inner_join(pop,by=c('county','year')) %>% 
              mutate(violent_pc=violent_felony/VALUE)

ggplot(nyc.crime,aes(x=year,y=violent_pc,color=county)) + geom_line() + geom_point() + 
  ylab("violent felony per capita") + theme_bw()



#add in a few more of the urban counties and run a panel fixed effects model of violent crime explained
# by misdemeanor arrest
fe.df <- tbl_df(ny.crime) %>% filter(county %in% c('New York','Kings','Queens','Bronx','Richmond',
                                           'Westchester','Rockland','Nassau','Erie')) %>%
        inner_join(pop,by=c('county','year')) %>%
        mutate(drug_pc=drug_felony/VALUE,violent_pc=violent_felony/VALUE,mis=mis_total/VALUE)

summary(plm(violent_pc ~ mis, data=fe.df, index=c("county", "year"), model="within"))
summary(plm(violent_pc ~ mis, data=fe.df[fe.df$year>1985,], index=c("county", "year"), model="within"))



