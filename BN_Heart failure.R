library(ggplot2)
library(plyr)
library(dplyr)
library(tibble)
library(nvmix)
library(readxl)
library(pracma)
library(fit.models)
library(sqldf)
library(writexl)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(readr)
library(data.table)
library(dygraphs)
library(xts)
library(lubridate)
library(plotly)
library(hrbrthemes)
library(forecast)
library(bnlearn)
library(R.utils)
library(Rgraphviz)
library(gRain)
library(RBGL)
library(graph)
library(RCurl)

failure <- read.csv("C:/Users/liry9/Desktop/Advanced multivariate statistics/Project AMS/Prima parte/heart_failure.csv", header=TRUE)
colnames(failure)[13] <- "death"
failure<-na.omit(failure)
nrow(failure)
# https://bmcmedinformdecismak.biomedcentral.com/articles/10.1186/s12911-020-1023-5
### Variables ###
# age: numerical value -> we will define 3 classes = 
#                                                   - < 30 -> young
#                                                   - btw 30 and 60 -> adult
#                                                   - above 60 -> old
# anaemia: Decrease of red blood cells or hemoglobin (boolean = 1 if anemic, 0 otherwise)
# creatinine_phosphokinase: Level of the CPK enzyme in the blood (mcg/L)
# diabetes: If the patient has diabetes (boolean = 1 if diabetic, 0 otherwise)
# ejection_fraction (https://www.heart.org/en/health-topics/heart-failure/diagnosing-heart-failure/ejection-fraction-heart-failure-measurement#:~:text=What%20is%20%E2%80%9Cejection%20fraction%E2%80%9D%3F,pushed%20out%20with%20each%20heartbeat.): Percentage of blood leaving the heart at each contraction (percentage):
#                     - < 40 percent may be evidence of heart failure or cardiomyopathy
#                     - btw 50 and 70 -> normal;
#                     - > 75 percent may indicate a heart condition such as hypertrophic cardiomyopathy.
# high_blood_pressure: If the patient has hypertension (boolean = 1 if hypertense, 0 otherwise)
# platlets: Platelets in the blood (kiloplatelets/mL)
# serum_creatinine: Level of serum creatinine in the blood (mg/dL) -> A serum creatinine test - which measures the level of creatinine in your blood - can indicate whether your kidneys are working properly.
# serum_sodium: Level of serum sodium in the blood (mEq/L)
# sex: Woman or man (binary)
# smoking: If the patient smokes or not (boolean)
# time: Follow-up period (days)
# death: If the patient deceased during the follow-up period (boolean)

table(failure %>%
        group_by(failure$death) %>% 
        tally())
barplot(table(failure$death), main = "barplot")

############################################################################################
                  ##### Dataset analysis #####
## Age is recorded as (young) for individuals below 30 years,
# old, (adult) for individuals between 30 and 60 years old, and
# (old) for people older than 60. But since the minimum age found
# does not go below 40 we will define only two ranges:
# below 65 = adult
# above 65 = elder

failure <- failure %>% mutate(category=cut(age, breaks=c(-Inf, 65, Inf), labels=c("adult","elder")))
failure <- failure[,-1]
colnames(failure)[13] <- "age"

## anaemia is re-encoded as: 0 = non anaemic (no), 1 = anaemic person (yes)
failure <- failure %>% mutate(anaemia = ifelse (anaemia == "0","no","yes"))

## creatine phosphokinase is recorded as low if below 10,
# normal if between 10 and 120 and above 120 as high
failure %>% slice_min(creatinine_phosphokinase)
failure <- failure %>% mutate(category=cut(creatinine_phosphokinase, breaks=c(-Inf, 120, Inf), labels=c("normal","high")))
failure <- failure[,-2]
colnames(failure)[13] <- "cpk"

## Diabetes is re-encoded as: 0 = non diabetic (no), 1 = diabetic person (yes)
failure <- failure %>% mutate(diabetes = ifelse (diabetes == "0","no","yes"))

## Ejection fraction (which is a percentage) is expressed as:
# if < 45 -> heart failure/ btw 45 and 70 -> normal/ over 75: heart condition

failure <- failure %>% mutate(category=cut(ejection_fraction, breaks=c(-Inf, 45, 70, Inf), labels=c("heart failure","normal","heart condition")))
failure <- failure[,-3]
colnames(failure)[13] <- "ejection_fraction"

## High blood pressure is re-encoded as: 0 = non hypertensive (no), 1 = hypertensive person (yes)
failure <- failure %>% mutate(high_blood_pressure = ifelse (high_blood_pressure == "0","no","yes"))
colnames(failure)[3] <- "hypertension"

## Platlets is expressed as:
# https://www.idoctors.it/patologia-piastrinopenie--trombocitopenia--26747#:~:text=La%20normale%20sopravvivenza%20delle%20piastrine,definisce%20trombocitosi%20(o%20piastrinosi).
# A normal platelet count ranges from 150,000 to 450,000. If the number of platelets is too low, excessive bleeding can occur.
# below 150000 -> thrombocytopenia
# above 4500000 -> thrombocytosis
failure <- failure %>% mutate(category=cut(platelets, breaks=c(-Inf, 150000, 450000, Inf), labels=c("thrombocytopenia","normal","thrombocytosis")))
failure <- failure[,-4]
colnames(failure)[13] <- "platelets"

## serum creatinine is expressed as:
# https://www.urmc.rochester.edu/encyclopedia/content.aspx?ContentTypeID=167&ContentID=creatinine_serum
# Normal creatinine ranges from 0.9 to 1.3 mg/dL for adult males and from 0.6 to 1.1 mg/dL for adult females (we will take 0.6-1.3)
# below 0.75 -> muscle disease, etc -> low (still dangerous)
# above 1.2 -> kidney failure, infection and/or reduced blood flow to the kidneys -> high (dangerous)
failure <- failure %>% mutate(category=cut(serum_creatinine, breaks=c(-Inf, 0.75, 1.2, Inf), labels=c("low","normal","high")))
failure <- failure[,-4]
colnames(failure)[13] <- "serum_creatinine"

## serum sodium is expressed as:
# https://www.mayoclinic.org/diseases-conditions/hyponatremia/symptoms-causes/syc-20373711#:~:text=A%20normal%20blood%20sodium%20level,Certain%20medications.
# https://www.msdmanuals.com/home/hormonal-and-metabolic-disorders/electrolyte-balance/hypernatremia-high-level-of-sodium-in-the-blood#:~:text=In%20hypernatremia%2C%20the%20level%20of,%2C%20kidney%20dysfunction%2C%20and%20diuretics.
# A normal blood sodium level is between 135 and 145 milliequivalents per liter (mEq/L)
# if < 135 -> Hyponatremia occurs when the sodium in your blood falls below 135 mEq/L.
# if > 145 -> Hypernatremia involves dehydration, which can have many causes, including not drinking enough fluids, diarrhea, kidney dysfunction, and diuretics.
# dependent on age and on diabetis
failure <- failure %>% mutate(category=cut(serum_sodium, breaks=c(-Inf, 135, 145, Inf), labels=c("Hyponatremia","normal","Hypernatremia")))
failure <- failure[,-4]
colnames(failure)[13] <- "serum_sodium"

## Sex is re-encoded as: 0 = female (female), 1 = male (male)
failure <- failure %>% mutate(sex = ifelse (sex == "0","female","male"))

## Smoking is re-encoded as: 0 = non smoker (no), 1 = smoker (yes)
failure <- failure %>% mutate(smoking = ifelse (smoking == "0","no","yes"))

## (Follow up) time: given that the maximal follow up time is 285 we will divide it into 3 ranges:
# < than 95 -> short (time period);
# btw 95 and 190 -> medium (time period);
# > than 190 -> long (time period)
failure <- failure %>% mutate(category=cut(time, breaks=c(-Inf, 95, 190, Inf), labels=c("short","medium","long")))
failure <- failure[,-6]
colnames(failure)[13] <- "time"

## death is re-encoded as: 0 = not dead (no), 1 = dead (yes)
failure <- failure %>% mutate(death = ifelse (death == "0","no","yes"))

cols_failure <- sapply(failure, is.character)
failure[,cols_failure] <- lapply(failure[,cols_failure], as.factor)
class(failure$smoking)

rm(cols_failure)
head(failure)
#############################################################
## Converting labels into numerical ones ##
#cols_failure <- sapply(failure, is.numeric)
#failure[,cols_failure] <- lapply(failure[,cols_failure], as.factor)
#failure[,cols_failure] <- lapply(failure[,cols_failure], as.numeric)
#str(failure)

#failure$age <- as.numeric(failure$age)
#failure$creatinine_phosphokinase <- as.numeric(failure$creatinine_phosphokinase)
#failure$ejection_fraction <- as.numeric(failure$ejection_fraction)
#failure$platelets <- as.numeric(failure$platelets)
#failure$serum_creatinine <- as.numeric(failure$serum_creatinine)
#failure$serum_sodium <- as.numeric(failure$serum_sodium)

##################################################################
              ##### Bayesian Networks (pt. 1) #####
### Small example ###
# http://gradientdescending.com/bayesian-network-example-with-the-bnlearn-package/
## Taking into consideration only: gender, age, smoke, anaemia, diabetes
# blood pressure and death
structure <- empty.graph(c("age", "smoking", "sex", "diabetes", "anaemia","hypertension","death"))

# set relationships manually
modelstring(structure) <- "[age][smoking][sex][diabetes|age][diabetes|smoking][anaemia|sex][anaemia|smoking][hypertension|age][hypertension|smoking][hypertension|sex][hypertension|diabetes][death|age][death|smoking][death|diabetes][death|hypertension]"

# plot network
library(visNetwork)
structure
plot.network <- function(structure, ht = "450px"){
  nodes.uniq <- unique(c(structure$arcs[,1], structure$arcs[,2]))
  nodes <- data.frame(id = nodes.uniq,
                      label = nodes.uniq,
                      color = "darkturquoise",
                      shadow = TRUE)
  
  edges <- data.frame(from = structure$arcs[,1],
                      to = structure$arcs[,2],
                      arrows = "to",
                      smooth = TRUE,
                      shadow = TRUE,
                      color = "black")
  
  return(visNetwork(nodes, edges, height = ht, width = "100%"))
}
plot.network(structure)

# We'll now fit the model and output the conditional probabilities for each node.
failure_sub <- failure[failure$death %in% c("yes","no"), c("age","smoking","sex","diabetes","anaemia","hypertension","death")]
failure_sub$death <- factor(failure_sub$death)
failure_bn_mod <- bn.fit(structure, data = failure_sub)
failure_bn_mod

sqldf('SELECT *
      FROM failure 
      WHERE smoking = "yes" AND diabetes = "yes" AND anaemia = "yes" AND hypertension = "yes"')

## It is also possible to question the network on the probability
# of something given something.
# For example: which is the probability of having diabetes being a woman and hypertensive?
cat("P(dying | hypertension and being a woman) =", cpquery(failure_bn_mod, (death=="yes"), (hypertension == "yes" & sex == "female")), "\n")

cat("P(anaemia | not dead) =", cpquery(failure_bn_mod, (anaemia=="yes"), (death == "yes")), "\n")

### Chained relationships
## Another benefit of Bayes networks is that two nodes don't need to be directly connected
# to make inferences from one about the other. We will use this principle to add the other variables

### We will add now one by one the remaining nodes/variables
## follow-up period (time)
structure2 <- empty.graph(c("age", "smoking", "sex", "diabetes", "anaemia","hypertension","time","death"))

# set relationships manually
modelstring(structure2) <- "[age][smoking][sex][diabetes|age][diabetes|smoking][anaemia|sex][anaemia|smoking][hypertension|age][hypertension|smoking][hypertension|sex][hypertension|diabetes][time|age][time|smoking][time|diabetes][time|anaemia][time|hypertension][death|age][death|smoking][death|diabetes][death|hypertension][death|time]"
plot.network(structure2)

# We'll now fit the model and output the conditional probabilities for each node.
failure2_sub <- failure[failure$death %in% c("yes","no"), c("age","smoking","sex","diabetes","anaemia","hypertension","time","death")]
failure2_sub$death <- factor(failure2_sub$death)
failure2_bn_mod <- bn.fit(structure2, data = failure2_sub)
failure2_bn_mod

## creatine phosphokinase (CPK)
structure3 <- empty.graph(c("age", "smoking", "sex", "diabetes", "anaemia","hypertension","cpk","time","death"))

# set relationships manually
modelstring(structure3) <- "[age][smoking][sex][diabetes|age][diabetes|smoking][anaemia|sex][anaemia|smoking][hypertension|age][hypertension|smoking][hypertension|sex][hypertension|diabetes][hypertension|cpk][cpk|smoking][cpk|diabetes][time|age][time|smoking][time|diabetes][time|anaemia][time|hypertension][time|cpk][death|age][death|smoking][death|diabetes][death|hypertension][death|time][death|anaemia:cpk]"
plot.network(structure3)

# We'll now fit the model and output the conditional probabilities for each node.
failure3_sub <- failure[failure$death %in% c("yes","no"), c("age", "smoking", "sex", "diabetes", "anaemia","hypertension","cpk","time","death")]
failure3_sub$death <- factor(failure3_sub$death)
failure3_bn_mod <- bn.fit(structure3, data = failure3_sub)
failure3_bn_mod

## ejection fraction
structure4 <- empty.graph(c("age", "smoking", "sex", "diabetes", "anaemia","hypertension","cpk","ejection_fraction","time","death"))

# set relationships manually
modelstring(structure4) <- "[age][smoking][sex][diabetes|age][diabetes|smoking][anaemia|sex][anaemia|smoking][hypertension|age][hypertension|smoking][hypertension|sex][hypertension|diabetes][hypertension|cpk][hypertension|ejection_fraction][cpk|smoking][cpk|diabetes][ejection_fraction|age][ejection_fraction|sex][ejection_fraction|smoking][ejection_fraction|diabetes][ejection_fraction|anaemia][time|age][time|smoking][time|diabetes][time|anaemia][time|hypertension][time|cpk][time|ejection_fraction][death|age][death|smoking][death|diabetes][death|hypertension][death|time][death|ejection_fraction][death|anaemia:cpk]"
plot.network(structure4)

# We'll now fit the model and output the conditional probabilities for each node.
failure4_sub <- failure[failure$death %in% c("yes","no"), c("age","smoking","sex","diabetes","anaemia","hypertension","cpk","ejection_fraction","time","death")]
failure4_sub$death <- factor(failure4_sub$death)
failure4_bn_mod <- bn.fit(structure4, data = failure4_sub)
failure4_bn_mod

## platelet (count)
structure5 <- empty.graph(c("age", "sex", "smoking", "diabetes", "anaemia","hypertension","cpk","ejection_fraction","platelets","time","death"))

# set relationships manually
modelstring(structure5) <- "[age][sex][smoking][diabetes|age][diabetes|smoking][anaemia|sex][anaemia|smoking][hypertension|age][hypertension|smoking][hypertension|sex][hypertension|diabetes][hypertension|cpk][hypertension|ejection_fraction][cpk|smoking][cpk|diabetes][cpk|platelets][ejection_fraction|age][ejection_fraction|sex][ejection_fraction|smoking][ejection_fraction|diabetes][ejection_fraction|anaemia][platelets|age][platelets|sex][platelets|smoking][platelets|cpk][time|age][time|smoking][time|diabetes][time|anaemia][time|hypertension][time|cpk][time|ejection_fraction][time|platelets][death|age][death|smoking][death|diabetes][death|hypertension][death|time][death|ejection_fraction:platelets][death|anaemia:cpk]"
plot.network(structure5)

# We'll now fit the model and output the conditional probabilities for each node.
failure5_sub <- failure[failure$death %in% c("yes","no"), c("age","smoking","sex","diabetes","anaemia","hypertension","cpk","ejection_fraction","platelet","time","death")]
failure5_sub$death <- factor(failure5_sub$death)
failure5_bn_mod <- bn.fit(structure5, data = failure5_sub)
failure5_bn_mod

### Platelet count without ejection_fraction
structure6 <- empty.graph(c("age", "sex", "smoking", "diabetes", "anaemia","hypertension","cpk","platelets","time","death"))

# set relationships manually
modelstring(structure6) <- "[age][sex][smoking][diabetes|age][diabetes|smoking][anaemia|sex][anaemia|smoking][hypertension|age][hypertension|smoking][hypertension|sex][hypertension|diabetes][hypertension|cpk][cpk|smoking][cpk|diabetes][cpk|platelets][platelets|age][platelets|sex][platelets|smoking][platelets|cpk][time|age][time|smoking][time|diabetes][time|anaemia][time|hypertension][time|cpk][time|platelets][death|age][death|smoking][death|diabetes][death|hypertension][death|time][death|platelets][death|anaemia:cpk]"
plot.network(structure5)
####################################################################
    ##### Constraint-based algorithms in structure learning #####
### Grow-Shrink Algorithm ### 
## The gs function estimates the equivalence class of a directed acyclic
# graph (DAG) from data using the Grow-Shrink (GS) constraint-based algorithm.
fail<-failure
drops <- c("serum_creatinine","serum_sodium")
fail<-fail[ , !(names(fail) %in% drops)]

fail_gs <- gs(fail)
fail_gs
graphviz.plot(fail_gs, layout = "dot")
plot.network(fail_gs)
g_fail <- bnlearn::as.graphNEL(fail_gs) # use this to avoid printing of graphviz.plot
plot(g_fail,  attrs=list(node = list(fontsize=20)))

ci.test("smoking", "death", data = fail)


### Incremental Association (IAMB) Learning Algorithm
## Estimate the equivalence class of a directed acyclic graph (DAG) from data
# using the Incremental Association (IAMB) Constraint-based algorithm.
bn_fail2 <- iamb(fail)
plot.network(bn_fail2)

### Fast Incremental Association (Fast-IAMB) Learning Algorithm
## Estimate the equivalence class of a directed acyclic graph (DAG) from data
# using the Fast Incremental Association (FAST-IAMB) Constraint-based algorithm.
bn_fail3 <- fast.iamb(fail)
bn_fail3
plot.network(bn_fail3)

### Interleaved Incremental Association (Inter-IAMB) Learning Algorithm
## Estimate the equivalence class of a directed acyclic graph (DAG) from data
# using the Interleaved Incremental Association (Inter-IAMB) Constraint-based algorithm.
bn_fail4 <- inter.iamb(fail)
bn_fail4
graphviz.plot(bn_fail4, layout = "dot")

### Comparison of network structures ###
## The compare() funciton: takes one network as a reference network, and
# computes the number of true positive, false positive and false negative arcs
# in the other network.
compare(fail_gs, bn_fail2)
compare(fail_gs, bn_fail3)
compare(fail_gs, bn_fail4)
compare(bn_fail2, bn_fail3)
compare(bn_fail4, bn_fail3)
compare(bn_fail2, bn_fail4)

fail_bn_hc <- hc(fail, score = "bic")
plot.network(fail_bn_hc)
compare(bn_fail3, fail_bn_hc)
par(mfrow = c(1, 1))
plot(fail_gs, main = "Constraint-based algorithms", highlight = ("death"))
plot(fail_bn_hc, main = "Hill-Climbing", highlight = ("death"))

################################################################
  ##### Score-based algorithms in structure learning #####
### Whitelists and blacklists
fail_highlight <- list(nodes = ("death"), col = "red", fill = "grey")

blacklist = data.frame (from = c("age","age","age","age",
                                 "sex","sex","sex","sex","sex","sex",
                                 "smoking","smoking","smoking",
                                 "diabetes","diabetes","diabetes","diabetes","diabetes",
                                 "anaemia","anaemia","anaemia","anaemia","anaemia","anaemia","anaemia",
                                 "hypertension","hypertension","hypertension","hypertension","hypertension","hypertension","hypertension","hypertension",
                                 "time","time","time","time","time","time","time","time","time",
                                 "death","death","death","death","death","death","death","death","death","death",
                                 "cpk","cpk","cpk","cpk","cpk","cpk",
                                 "ejection_fraction","ejection_fraction","ejection_fraction","ejection_fraction","ejection_fraction","ejection_fraction","ejection_fraction",
                                 "platelets","platelets","platelets","platelets","platelets","platelets","platelets"),
                        to = c("sex","smoking","anaemia","cpk",
                               "age","smoking","diabetes","time","cpk","death",
                               "age","sex","hypertension",
                               "age","sex","smoking","anaemia","platelets",
                               "age","sex","smoking","diabetes","death","cpk","platelets",
                               "age","sex","smoking","diabetes","anaemia","cpk","ejection_fraction","platelets",
                               "age","sex","smoking","diabetes","anaemia","hypertension","cpk","ejection_fraction","platelets",
                               "age","sex","smoking","diabetes","anaemia","hypertension","time","cpk","ejection_fraction","platelets",
                               "age","sex","smoking","diabetes","anaemia","ejection_fraction",
                               "age","sex","smoking","diabetes","anaemia","cpk","platelets",
                               "age","sex","smoking","diabetes","anaemia","hypertension","ejection_fraction"))
blacklist
whitelist = data.frame (from = c("age","age", "age","age","age", "age",
                                 "sex","sex","sex","sex",
                                 "smoking","smoking","smoking","smoking","smoking","smoking","smoking","smoking",
                                 "diabetes","diabetes","diabetes","diabetes",
                                 "anaemia","anaemia",
                                 "hypertension","hypertension",
                                 "time",
                                 "cpk","cpk","cpk"),
                        to = c("diabetes","hypertension","time","death","ejection_fraction","platelets",
                               "anaemia", "hypertension","ejection_fraction","platelets",
                               "diabetes","anaemia","hypertension","time","death","cpk","ejection_fraction","platelets",
                               "hypertension","time","death","ejection_fraction",
                               "time","ejection_fraction",
                               "time","death",
                               "death",
                               "hypertension","death","platelets"))
whitelist

### The loss funtion we use is the so-called log-likelihood loss,
# which is the negated expected loglikelihood; so lower values are
# better.
# We learn the structure of a Bayesian network using a hill-climbing
# (HC) or a Tabu search (TABU) greedy search.
## Hill climbing with only whitelist
hc_whitelist<- hc(fail, score = "aic", whitelist=whitelist)
hc_whitelist
g_hc_whitelist <- Rgraphviz::layoutGraph(bnlearn::as.graphNEL(hc_whitelist))
graph::nodeRenderInfo(g_hc_whitelist) <- list(fontsize=45)
Rgraphviz::renderGraph(g_hc_whitelist, main="Hybrid HC Aic")
bnlearn::score(hc_whitelist, data=fail, type="bic")

# Hill climbing with only blacklist
hc_blacklist<- hc(fail, score = "aic", blacklist=blacklist)
hc_blacklist
g_hc_blacklist <- Rgraphviz::layoutGraph(bnlearn::as.graphNEL(hc_blacklist))
graph::nodeRenderInfo(g_hc_blacklist) <- list(fontsize=30)
Rgraphviz::renderGraph(g_hc_blacklist, main="Hybrid HC Aic")
bnlearn::score(hc_blacklist, data=fail, type="bic")

##  Hill climbing with both white and blacklist
fail_hc1 <- hc(fail, score = "aic", blacklist = blacklist, whitelist=whitelist)
fail_hc1
graphviz.plot(fail_hc1, highlight = fail_highlight, main="Hybrid HC Aic")
plot.network(fail_hc1)
bnlearn::score(fail_hc1, data=fail, type="bic")

g_fail_hc1 <- Rgraphviz::layoutGraph(bnlearn::as.graphNEL(fail_hc1))
graph::nodeRenderInfo(g_fail_hc1) <- list(fontsize=22)
Rgraphviz::renderGraph(g_fail_hc1, main="Hybrid HC Aic")

fail_hc2 <- hc(fail, score = "bic", blacklist = blacklist, whitelist=whitelist)
fail_hc2
graphviz.plot (fail_hc2, highlight = fail_highlight, main="Hybrid HC Bic")
plot.network(fail_hc2)
bnlearn::score(fail_hc2, data=fail, type="bic")

## Tabu search is implemented in the tabu() function.
fail_tabu1 <- tabu(fail, score = "aic", blacklist = blacklist, whitelist=whitelist)
fail_tabu1
graphviz.plot (fail_tabu1, highlight = fail_highlight, main="Tabu greedy search Aic")
plot.network(fail_tabu1)
bnlearn::score(fail_tabu1, data=fail, type="bic")

fail_tabu2 <- tabu(fail, score = "bic", blacklist = blacklist, whitelist=whitelist)
fail_tabu2
graphviz.plot (fail_tabu2, highlight = fail_highlight, main="Tabu greedy search Aic")
plot.network(fail_tabu2)
bnlearn::score(fail_tabu2, data=fail, type="bic")

## Grow-Shrink (GS) Constraint-based algorithm.
fail_gs <- gs(fail, blacklist= blacklist, whitelist=whitelist)
fail_gs
graphviz.plot (fail_gs, highlight = fail_highlight, main="GS constraint-based algorithm")
plot.network(fail_gs)
bnlearn::score(fail_gs, data=fail, type="bic")

## Incremental Association (IAMB) Learning Algorithm
fail_iamb <- iamb(fail, blacklist = blacklist, whitelist = whitelist)
fail_iamb
graphviz.plot (fail_iamb, highlight = fail_highlight, main="IAMB constraint-based algorithm")
plot.network(fail_iamb)
bnlearn::score(fail_iamb, data=fail, type="bic")

## Fast Incremental Association (Fast-IAMB) Learning Algorithm
fail_fiamb <- fast.iamb(fail, blacklist = blacklist, whitelist=whitelist)
fail_fiamb
graphviz.plot (fail_fiamb, highlight = fail_highlight, main="Fast-IAMB algorithm")
plot.network(fail_fiamb)
bnlearn::score(fail_fiamb, data=fail, type="bic")

## Interleaved Incremental Association (Inter-IAMB) Learning Algorithm
fail_intamb <- inter.iamb(fail, blacklist = blacklist, whitelist=whitelist)
fail_intamb
graphviz.plot (fail_intamb, highlight = fail_highlight, main="Inter-IAMB learning algorithm Aic")
plot.network(fail_intamb)
bnlearn::score(fail_intamb, data=fail, type="bic")

### Hybrid Algorithms ###
## Max-min Hill-Climbing (MMHC)
mmhc1 <- mmhc(fail)
graphviz.plot (mmhc1, highlight = fail_highlight, main="Max-min Hill-Climbing (MMHC) Aic")

fail_mmhc <- mmhc(fail, whitelist=whitelist)
fail_mmhc
graphviz.plot (fail_mmhc, highlight = fail_highlight, main="Max-min Hill-Climbing (MMHC) Aic")
bnlearn::score(fail_mmhc, data=fail, type="bic")

## General 2-Phase Restricted Maximization algorithm
fail_rsmax <- rsmax2(fail, whitelist=whitelist)
fail_rsmax
graphviz.plot (fail_rsmax, highlight = fail_highlight, main="General 2-Phase Restricted Maximization algorithm")
plot.network(fail_rsmax)
bnlearn::score(fail_rsmax, data=fail, type="bic")

################################################################
    ##### Parameter learning ######
### Fit the paramenters of the Bayesian network ###
## bn.fit() fits the parameters of a Bayesian network given its structure and a data set; bn.net returns
# the structure underlying a fitted Bayesian network.

fit_hc_whitelist1 <- bn.fit(hc_whitelist, fail, method = "mle")
fit_hc_whitelist1

fit_hc_whitelist2 <- bn.fit(hc_whitelist, fail, method = "bayes")
fit_hc_whitelist2

fit_hc_blacklist1 <- bn.fit(hc_blacklist, fail, method = "mle")
fit_hc_blacklist1

fit_hc_blacklist2 <- bn.fit(hc_blacklist, fail, method = "bayes")
fit_hc_blacklist2

fit_fail_hc1 <- bn.fit(fail_hc1, fail, method = "mle")
fit_fail_hc1

fit_fail_hc2 <- bn.fit(fail_hc1, fail, method = "bayes")
fit_fail_hc2

bn.fit(fail_hc2, fail)

bn.fit(fail_tabu1, fail)

bn.fit(fail_tabu2, fail)

bn.fit(fail_gs, fail)

bn.fit(fail_iamb, fail)

bn.fit(fail_fiamb, fail)

bn.fit(fail_intamb, fail)

bn.fit(fail_mmhc, fail)

bn.fit(fail_rsmax, fail)
##############################################################################
##############################################################################
      ##### Bayesian networks and cross validation ######
### k-fold cross validation
#bn.cv(fail_hc1, fail)