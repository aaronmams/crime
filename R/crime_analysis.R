library(Quandl)
library(dplyr)
library(ggplot2)


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

#show NY only 
ggplot(subset(crime,state %in% c("NJ","NY","MA","IL","CA")),aes(x=year,y=murder_cap,color=state)) + geom_line() + 
  geom_point() + geom_rect(aes(xmin=as.Date('1982-12-31'),
                               xmax=as.Date('1983-12-31'),ymin=-Inf,ymax=+Inf),fill='pink',alpha=0.02) + 
  geom_rect(aes(xmin=as.Date('1990-12-31'),
                xmax=as.Date('1991-12-31'),ymin=-Inf,ymax=+Inf),fill='pink',alpha=0.02) +
  geom_rect(aes(xmin=as.Date('1993-12-31'),
                xmax=as.Date('1994-12-31'),ymin=-Inf,ymax=+Inf),fill='pink',alpha=0.02) +
  theme_bw()

#------------------------------------------------------------------------------