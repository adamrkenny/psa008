## PSA 008 Minimal Groups Across cultures
## Pilot #1: Validating an online minimal groups procedure
## Last updated: 5/10/22 by yarrow.dunham@yale.edu



#### setup ####

# load required packages
library(tidyverse)
library(qualtRics)

# read data; may need to adjust file path
raw <- read_survey('../data/data-pilot-01-processed.csv')



#### Data cleaning ####

# renames variable to meaningful names
# creates ingroup and outgroup scores
# selects down to only the variables to be used

reduced <- raw %>%
  rename(group = FL_47_DO,
         esteem = Q62_1,
         police=Q99_6,
         courts=Q99_7,
         gov=Q99_8,
         parties=Q99_9,
         family=Q97_1,
         neighb=Q97_2,
         personally=Q97_3,
         firstTime=Q97_4,
         anothRelig=Q97_5,
         anothNation=Q97_6,
         impTrustDummy='Q95...41') %>%
  mutate(likeFlurp = ifelse(is.na(GroupAttitude_1),Q91_1,GroupAttitude_1),
         likeZaz = ifelse(is.na(GroupAttitude_2),Q91_2,GroupAttitude_2),
         attachFlurp = ifelse(is.na(GroupAttitude_5),Q91_5,GroupAttitude_5),
         attachZaz = ifelse(is.na(GroupAttitude_6),Q91_6,GroupAttitude_6),
         identFlurp = ifelse(is.na(GroupAttitude_7),Q91_7,GroupAttitude_7),
         identZaz = ifelse(is.na(GroupAttitude_8),Q91_8,GroupAttitude_8),
         helpFlurp = ifelse(is.na(GroupAttitude_9),Q91_9,GroupAttitude_9),
         helpZaz = ifelse(is.na(GroupAttitude_10),Q91_10,GroupAttitude_10)) %>%
  mutate(IG100 = ifelse(is.na(Q93),Q74,Q93),
         IG85 = ifelse(is.na(Q94),Q76,Q94),
         IG40 = ifelse(is.na(`Q95...29`),Q78,`Q95...29`),
         IG0 = ifelse(is.na(Q96),Q80,Q96),
         OG40 = ifelse(is.na(Q97),Q82,Q97),
         OG85 = ifelse(is.na(Q98),Q84,Q98),
         OG100 = ifelse(is.na(Q99),Q86,Q99)) %>%
  mutate(ingroup = ifelse(group=='Flurp',(likeFlurp+attachFlurp+identFlurp+helpFlurp)/4,(likeZaz+attachZaz+identZaz+helpZaz)/4),
         outgroup = ifelse(group=='Zaz',(likeFlurp+attachFlurp+identFlurp+helpFlurp)/4,(likeZaz+attachZaz+identZaz+helpZaz)/4),
         ingroupAdv = ingroup-outgroup,
         hasBias = ifelse(ingroupAdv >  0, 1, 0),
         IGben = (IG100+IG85+IG40)/3,
         OGben = (OG100+OG85+OG40)/3,
         IGbenAdv = IGben - OGben,
         trustInst = 5-(police + courts + gov + parties)/4,
         trustInter = 5 - (family+neighb+personally+firstTime+anothRelig+anothNation)/5,
         ID = 1:207) %>%
  select(ID,group,esteem,likeFlurp:helpZaz,ingroup,outgroup,ingroupAdv,hasBias,
         IG100:OG100,IGben,OGben,IGbenAdv,
         police,courts,gov,parties,family,neighb,personally,firstTime,anothRelig,
         anothNation,impTrustDummy,trustInst,trustInter)
  

#### Analyses reported in main text ####

## Attitude Task
mean(reduced$ingroupAdv) # average ingroup preference effect, in scale units
sd(reduced$ingroupAdv) # sd of ingroup preference effect
mean(reduced$ingroupAdv) / sd(reduced$ingroupAdv)  # standardized effect size
t.test(reduced$ingroupAdv)

## Resource task
mean(reduced$IGbenAdv)  # average rating advantage of ingroup-favoring distributions
sd(reduced$IGbenAdv)  # sd of those advantages 
mean(reduced$IGbenAdv)/sd(reduced$IGbenAdv)  # standardized effect size
t.test(reduced$IGbenAdv)


# reliability of attitude items
psych::alpha(reduced[4:11])   # .84 alpha for attitude items




### Additional analyses of pilot data reported in Supplementary Materials ####

# no difference based on group assigned to, i.e. based on group name
t.test(reduced$ingroupAdv ~ reduced$group,var.equal=T)  

# do trust items hang together? Separately for institutional and interpersonal items
# since we do some analyses with aggregate below checking we have a reasonable alpha
psych::alpha(reduced[c(26:29)])  # .82 alpha for trust in institutions 
psych::alpha(reduced[c(30:35)])  # .80 alpha for interpersonal trust
psych::alpha(reduced[c(26:35)])  # .84 for all the above items together

# two dimensions of trust are correlated
cor.test(reduced$trustInst,reduced$trustInter) # r = .48



# look at factor structure of attitude items

reduced2 <- reduced %>%
  mutate(likeIG = ifelse(group=='Zaz',likeZaz,likeFlurp),
         likeOG = ifelse(group=='Zaz',likeFlurp,likeZaz),
         attachIG = ifelse(group=='Zaz',attachZaz,attachFlurp),
         attachOG = ifelse(group=='Zaz',attachFlurp,attachZaz),
         identIG = ifelse(group=='Zaz',identZaz,identFlurp),
         identOG = ifelse(group=='Zaz',identFlurp,identZaz),
         helpIG = ifelse(group=='Zaz',helpZaz,helpFlurp),
         helpOG = ifelse(group=='Zaz',helpFlurp,helpZaz))

# factor structure?
psych::fa(reduced2[39:46],8,rotate="none")
psych::fa(reduced2[39:46],2,rotate="none")
psych::fa(reduced2[39:46],2,rotate='oblimin')
psych::fa(reduced2[39:46],2,rotate='varimax')
# with no rotation we get two factors, one that is basically an overall ratings positivity
# and one that has opposite loadings on IG and OG
# with either rotation we get one factor for IG followed by a factor for OG
# but in both cases no hint that identification patterns differently than other items


# for visualizaiton here is a graph of all attitude items as a function of group
# evidence of bias on each item
long <- reduced %>%
  gather(question,rating,likeFlurp:helpZaz)

ggplot(long,aes(x=question,y=rating,fill=group)) +
  stat_summary(fun.y=mean, geom="bar", size=2, position = position_dodge()) +
  stat_summary(fun.data="mean_cl_boot", geom="linerange", position = position_dodge(.9)) + theme_bw(base_size = 16)

