library(foreign)
library(dplyr)

setwd("~/Dropbox/MiljanichMildenbergerStokes/Comey/")

setwd("/Users/cmiljanich/Desktop/Comey/USC Data") 
# I set wd to my mainframe because I dont have internet (at the time of doing this). 

# USC questions - probability that... 
#1) you will vote in the election?; 
#prob_vote

#2) you will vote for Clinton, Trump, or someone else?; and
#clint_vote

#3) Clinton, Trump or someone else will win?” 
#clint_win

# load USC panel data

usc <- read.dta("Data/full2016corrected.dta")
usc <- read.dta("full2016corrected.dta") # Chris only


#comey letter sent on October 28
usc$polldayno[which(usc$polldate=="2016-10-28")] # this corresponds to day 117

# comparison groups observed pre and post-comey (there should be 6) 
# first group 110 through 117 - all the way through 116 to 122
ids110 <- usc$uasid[which(usc$polldayno==110)] # vector of individuals who were asked on 111 
# Is this 111 or 110? 
group110 <- usc[which(usc$uasid %in% ids110),] #all data across all time periods for these individuals

# condensing into a single line - Basically making dataframes only for individuals
# who have respective polldayno. 
group111 <- usc[which(usc$uasid %in% usc$uasid[which(usc$polldayno==111)]),]
group112 <- usc[which(usc$uasid %in% usc$uasid[which(usc$polldayno==112)]),]
group113 <- usc[which(usc$uasid %in% usc$uasid[which(usc$polldayno==113)]),]
group114 <- usc[which(usc$uasid %in% usc$uasid[which(usc$polldayno==114)]),]
group115 <- usc[which(usc$uasid %in% usc$uasid[which(usc$polldayno==115)]),]
group116 <- usc[which(usc$uasid %in% usc$uasid[which(usc$polldayno==116)]),]

table(group110$polldayno) # this suggests that contacts were more fluid than every 7 days regularly.
# note also: there may be some confounder which explains why some people are contactable regularly and others not
# but this is likely time-invariant for the two-week window are focused on

### FIRST STRATEFGY - ONLY PEOPLE SPACED 7 DAYS APART - SIMPLE DIFFERENCE
diff110 <- group110[which(group110$polldayno==110|group110$polldayno==117),] %>%
  group_by(uasid) %>%
  transmute(comey = c(NA, diff(clint_vote)), voteprob=c(NA, diff(prob_vote)))
mean(diff110$comey, na.rm=TRUE)
mean(diff110$voteprob, na.rm=TRUE)


diff111 <- group111[which(group111$polldayno==111|group111$polldayno==118),] %>%
  group_by(uasid) %>%
  transmute(comey = c(NA, diff(clint_vote)), voteprob=c(NA, diff(prob_vote)))
mean(diff111$comey, na.rm=TRUE)
mean(diff111$voteprob, na.rm=TRUE)


diff112 <- group112[which(group112$polldayno==112|group112$polldayno==119),] %>%
  group_by(uasid) %>%
  transmute(comey = c(NA, diff(clint_vote)), voteprob=c(NA, diff(prob_vote)))
mean(diff112$comey, na.rm=TRUE)
mean(diff112$voteprob, na.rm=TRUE)


diff113 <- group113[which(group113$polldayno==113|group113$polldayno==120),] %>%
  group_by(uasid) %>%
  transmute(comey = c(NA, diff(clint_vote)), voteprob=c(NA, diff(prob_vote)))
mean(diff113$comey, na.rm=TRUE)
mean(diff113$voteprob, na.rm=TRUE)


diff114 <- group114[which(group114$polldayno==114|group114$polldayno==121),] %>%
  group_by(uasid) %>%
  transmute(comey = c(NA, diff(clint_vote)), voteprob=c(NA, diff(prob_vote)))
mean(diff114$comey, na.rm=TRUE)
mean(diff114$voteprob, na.rm=TRUE)


diff115 <- group115[which(group115$polldayno==115|group115$polldayno==122),] %>%
  group_by(uasid) %>%
  transmute(comey = c(NA, diff(clint_vote)), voteprob=c(NA, diff(prob_vote)))
mean(diff115$comey, na.rm=TRUE)
mean(diff115$voteprob, na.rm=TRUE)


diff116 <- group116[which(group116$polldayno==116|group116$polldayno==123),] %>%
  group_by(uasid) %>%
  transmute(comey = c(NA, diff(clint_vote)), voteprob=c(NA, diff(prob_vote)))
mean(diff116$comey, na.rm=TRUE)
mean(diff116$voteprob, na.rm=TRUE)
# the better strategy is likely to start with everyone who answers survey 1 day post-Comey and compare to last response

# Find first post-test and last pre-test days for each unit. 

ddplyday <- ddply(usc, .(uasid), mutate, postday = min(polldayno[polldayno>=117], na.rm = T))
ddplyday <- ddply(ddplyday, .(uasid), mutate, preday = max(polldayno[polldayno<117], na.rm = T))

# My idea here was to use ddplyr because it can split the data into subsets depending on the identifier I choose, which, 
# in this case, is "uasid". I proceed by providing the dataframe I use ("usc" and "ddplyday"), identifying what I want to
# subset on ("uasid"), stating that I want to add a variable to the new dataframe ("mutate"), and finish by creating the 
# new variable. 

# "postday" is the minimum "polldayno" that is greater than or equal to day 117, which is when the Comey letter dropped, and 
# represents the first post-treatment observation for each respondent. 
# "preday" is the maximum "polldayno" that is less than 117, and represents the last pre-treatment observation for each respondent. 

# It is probably not ideal to create subsequent datasets in two steps like I have, but I couldn't find how to add two variables 
# in the same line of code without producing "NULL". I will work on sorting this out so as to reduce user-error. I also end up
# creating a totally new separate dataframe, so I should really try to improve the code because of these two possible issues. 

# By using greaterthan or equal to I am implicitly defining treatment as occuring on day 117, which might induce assumptions
# that are not totally valid, but this can be sorted out with robustness checks if we assume that people didn't actually 
# become aware of the letter until after this date, in which, day 117 wouldn't really be a treatment. 

warnings()

# You'll note that there are some warnings when I use min/max, but I don't think it affects the computation.
# Perhaps I am wrong, but from what I have read it is a bug in the min/max function. I do get "Inf" appearing
# in some places when I print the variable though, so perhaps it isn't totally benign and I am sacrificing the 
# integrity of the data. 

# Below I check that "postday" and "preday" match the first post-treatment observation and the last pre-treatment observation. 
# This is just an eyeball approach at this point, and I don't have anymore time today to check.

ddplyday$polldayno
ddplyday$postday
ddplyday$polldayno
ddplyday$preday

# Now I merge preday and postday variables into original dataset.

usc$preday <- ddplyday$preday
usc$postday <- ddplyday$postday

usc[1:300, "postday"]
ddplyday[1:300, "postday"]

usc[1:300, "preday"]
ddplyday[1:300, "preday"]

# Looks like they match

# Below I make a variable for the difference in probability in voting for either candidate. This is to 
# create a variable that can show changes in probability for voting for either candidate as a function
# of each candidate. Trump is subtracted from Clinton, so a negative number indicates a higher probability
# of voting for Trump, and a positive number indicates higher probability of voting for Clinton. It 
# consolidates vote probabilities for each candidate, but has the drawback of not showing whether a 
# change occurred because of Trump or Clinton. I make this variable because it could be that the letter 
# does not chagne the probability of voting for one candidate, but does for the other, and creating a dynamic
# variable that responds to movement for either candidate allows us to see whether there is relative movement
# in either direction. 

# In effect, I create a value for each observational period for each observation showing the difference in probability
# of voting for either candidate, where if P(Clinton) > P(Trump) the value is positive, and if P(Clinton) < P(Trump),
# the value is negative. By measuring whether this variable increases or decreases, we can examine whether the 
# probabulity of voting for one candidate increased (decreased) relative to the other candidate. 

ddplyday$votediff <- ddplyday$clint_vote - ddplyday$trump_vote
usc$votediff <- ddplyday$votediff

usc[1:300, "votediff"]
ddplyday[1:300, "votediff"]

# Looks like they match. 

# Messing around 

# Create a dummy variable for post treatment period for ddplyday and USC datasets

ddplyday$postperiod <- ifelse(usc$polldayno>=117, 1, 0) 
usc$postperiod <- ifelse(usc$polldayno>=117, 1, 0)

usc$preperiod <- ifelse(usc$polldayno<117, 1, 0)

# Immediate pre and post treatment effect #

# So the above doesn't seem to work as it returns a bunch of NAs for both new variables. 
# What I want to do is just locate last-pre and first-post observations, use those
# observations to locate clint_vote values on those days, and take difference for clint_
# vote for these days to find treatment effect. But,  I can't don't really know how to do
# this, and I DON'T WANT TO CONTINUE CREATING DATAFRAMES, I jsut want to do the damn 
# calculation and then do this for all observational periods. 

###### Sad attempts to make variable for difference in post and pre treatment support ######

library(dplyr)
library(plyr)

variable.names(usc)
usc[,c("post_c_mean", "pre_c_mean", "clint_att", "polldayno2")] <- NULL


# Below doesn't work and this is error message: "Error in `[[<-.data.frame`(`*tmp*`, col, value = integer(0)) : 
# replacement has 0 rows, data has 13". My idea here is to just find the difference in clint_vote
# for last pre and first post treatments. Won't work of course, and don't really know what the error message
# means. 

usc <- ddply(usc, .(uasid), mutate, clint_att = clint_win[polldayno == min(polldayno>=117)]
             - clint_win[polldayno == max(polldayno<117)])

# Below doesn't work and this is error message: "Error in `[[<-.data.frame`(`*tmp*`, col, value = integer(0)) : 
# replacement has 0 rows, data has 1". Same idea here but using different argument. Same damn error-ridden result.

usc <- ddply(usc, .(uasid), mutate, clint_att = clint_win[polldayno == postday]
             - clint_win[polldayno == preday])

# Below gives same error message as above. Idea here is to just create two variables and attach them to dataframe
# by using ddply and then difference them after. But, as usual, an error message arises.

usc <- ddply(usc, .(uasid), mutate, clint_post_mean = clint_win[polldayno == min(polldayno>=117)])
usc <- ddply(usc, .(uasid), mutate, clint_pre_mean = clint_win[polldayno == max(polldayno<117)])

# So I need to find whay these aren't working/find a solution to the problem so I can find the change in
# our DV for the last-pre and first-post treatment observations. This will need to be done for each observational
# interval though, so a more efficient approach is needed where I dont just continue making dataframes which is
# not very efficient. I want to write code that returns a vector of differences rather than returning a dataframe
# but I don't know enough vocabulary to do so without using canned dplyr or plyr commands which is pretty elementary.
# Basically I dont want to just rely on ddply for all of this...

# I use this to check whether I got the results I want. 

usc[1:100 ,c("uasid", "polldayno", "postday", "preday", "clint_att", "clint_win")]

# Find ATT for preday and postday...

usc <- group_by(usc, uasid, mutate(usc, 
                                   clint_att = usc$clint_vote[usc$polldayno == usc$postday] 
                                   - usc$clint_vote[usc$polldayno == usc$preday], na.rm = T))


usc <- usc %>% group_by(uasid) %>% summarise(clint_att = 
                                               clint_vote[polldayno==min(polldayno>=117)] 
                                             - clint_vote[polldayno==max(polldayno<117)])


# Neither of the above work, so I will just use regression.

# Some regressions using fixed effect at the individual level. No need for time fixed effect because we aren't observing
# the same unit at different observational periods, but rather how a treatment occuring after a time threshold affects
# support, which soaks up time effects. See below. 

# Here I use fixed effects estimators with a dummy indicating that an observation occured post-treatment.

library(plm)
library(lmtest)

variable.names(usc) 

# DV = votediff

v_diff_plm <- plm(votediff ~ postperiod, index = c("uasid"), data = usc, model = "within", effect = "twoways") 

coeftest(v_diff_plm, vcov=function(x)
  vcovHC(x, cluster="group", type="HC1"))

# DV = clint_vote

c_diff_plm <- plm(clint_vote ~ postperiod, index = c("uasid"), data = usc, model = "within")

coeftest(c_diff_plm, vcov=function(x)
  vcovHC(x, cluster="group", type="HC1"))

# DV = trump_vote

t_diff_plm <- plm(trump_vote ~ postperiod, index = c("uasid"), data = usc, model = "within")

coeftest(t_diff_plm, vcov=function(x)
  vcovHC(x, cluster="group", type="HC1"))

mod_coefs <- cbind(v_diff_plm$coefficients, c_diff_plm$coefficients, t_diff_plm$coefficients)
mod_coefs

# Results: Null effect on clint_vote; positive and statistically significant effect on trump_vote; 
# negative and statistically significant result on votediff. This indicates that the letter did not
# have an effect on probability of voting for clinton, but that it did increase an individual's 
# probability of voting for Trump. The statistically significant negative effect on votediff suggests
# that, after the letter, respondents were less likely to vote for Clinton relative to Trump and that 
# the probability of voting for Clinton decreased because voters became more likely to vote for Trump.
# Hence, the letter, as measured here, may not have decreased support for Clinton, but increased support
# for Trump, leading to a net negative effect for Clinton. The null effect on clint_vote and 
# statistically significant postive effect on trump_vote suggests that the net negative effect
# for Clinton was due to an increase in support for Trump rather than a decrease in support for Clinton. 
# Hmm, should ruminate more. So it seems to be a relative increase in support for Trump rather than
# an absoulte decrease in support for Clinton. So they became less likely to vote for CLinton relative
# to Trump, as indicated by the negative coefficient value for votediff. 

# I don't think I need to specify time  because our treatment indicator (postperiod)
# has an explicit time dimension, and essentially represents time. If the treatment wasn't an explicit
# time function, I would need to index by (create a fixed effect) for time because each unit in the panel
# would be observed at multiple points, and you'd need to index each unit of time for each unit, but 
# because the time dimension in our model is soaked up by whether an observation occurs before or after
# treatment, and is modeled by "postperiod" we don't need to create an index (fixed effect) that operates
# as a function for time because our model doesn't actually look at units through time, it only looks 
# at how a treatment (which is time, essentially) affect support, and this effectively "acts" as a time 
# fixed effect. Phew... I think I am describing that correctly, even though it is circumlocuitous. 
# Basically, the nature of our treatment means that we dont really care when an observation takes place
# but whether it is treated or not, and because the treatment clears up reverse causality, it doesn't really
# matter where an observation is indexed because its "time" is soaked up by treatment status. Bad thing
# is that we can't look at delay effects, but that is for later. 

# Here I do it differently to see if findings are an artifact of my code. You'd really only do this if you 
# had a dataset where the data did not have indices for each unit at each time period of observation. 
# For instance, if each US state was observed 37 times, but there was no index for each state representing
# each observational time period 1, 2, 3, ... ,37 for each state, where each state has an index of [1,37]
# representing each observational time period. 

# Create indexed data

usc.d <- plm.data(usc, indexes = c("uasid"))

# Do regression

did.reg.usc <- plm(votediff ~ postperiod, data = usc.d, model = "within")

coeftest(did.reg.usc, vcov=function(x)
  vcovHC(x, cluster="group", type="HC1"))

mod_coefs

# Appears to be no difference.

# Hmm, so again maybe the letter had an effect, but that it was not an absolute change in probability of voting for 
# Clinton, but a relative increase in probability of voting for Trump relative to Clinton, post treatment. This given
# statistically significant increase in probability of voting for Trump and a null effect for CLinton. 

# Test to see if this works.