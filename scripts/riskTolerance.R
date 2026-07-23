##### What factors determine individual risk tolerance? #####
## make it a classification problem
## separate absolute risk aversion from some risk

library(tidyverse)
library(haven)
library(plotly)

## DATA
## 1. download and extract data
# download.file(
#  "https://www.federalreserve.gov/econres/files/scfp2019excel.zip",
#  "scfp2019excel.zip",
#  mode = 'wb')
#
#unzip("scfp2019excel.zip", exdir = "./data")
#
#download.file(
#  "https://www.federalreserve.gov/econres/files/scf2019s.zip",
#  mode = 'wb')
#unzip("scf2019s.zip", exdir = "./data")
# read in
scfp2019 <- read.csv("data/SCFP2019.csv")
scfp2019_OG <- read_dta("data/p19i6.dta")
## 2. select needed variables
scfp2019_df <- scfp2019 %>% 
  select(
# DEMOGRAPHIC DATA
    "Y1",         # id, for joining
    "WGT",         # WEIGHT... maybe for intro?(drop later)
    "WOMEN" = "HHSEX",       # change
    "AGECL",       # AGE CATEGORIES
    "EDCL",        # EDUC CATEGORIES
    "MARRIED",
    "KIDS",
    "RACE",
    "OCCAT1",      # work status
    "OCCAT2",      # occupation classification
    "RENT",
    "INCOME",
    "NETWORTH",    # keep???
    "WSAVED",      #  1=exceeded,2=equaled,3=less
    "NNRESRE",     # net equity in nonresidential real estate
    "WILSH",       # Wilshire index of stock prices
    "DEBT",        # total debt

  # Y - VARIABLE
    #risky
    "NMMF",        # Mutual funds <- STMUTF+TFBMUTF+GBMUTF+OBMUTF+COMUTF
    "BOND",
    "STOCKS",
    "OTHMA",       # other managed assets(keep or drop)
    #risk free
    "LIQ",         # LIQUID ASSETS <- CHECKING+SAVING+MMA+CALL+PREPAID
    "CDS",         # CERTIFICATE OF DEPOSIT
    "SAVBND",      # SAVINGS BONDS
    "CASHLI",      # cash value of whole life insurance
)
#
finrisk <- scfp2019_OG %>%
  select(FINRISK = x3014,y1
)
## 3. join
scf_explore <- scfp2019_df %>%
  left_join(finrisk, by = c("Y1" = "y1")
)
## 4. make risk tolerance
scf_explore$risky <- scf_explore$NMMF + scf_explore$BOND +scf_explore$STOCKS + scf_explore$OTHMA
scf_explore$risky_free <- scf_explore$LIQ + scf_explore$CDS +scf_explore$SAVBND + scf_explore$CASHLI
#RISK TOLERANCE
scf_explore$risk_tol <- scf_explore$risky / (scf_explore$risky + scf_explore$risky_free)
## 4. SELECT FINAL VARIABLES
scfpdata <- scf_explore %>% 
  select(
    -c("Y1","WGT","NMMF","BOND", "STOCKS","OTHMA","LIQ", "CDS", "SAVBND", "CASHLI","risky","risky_free","WILSH"
  )
)
scfpdata_num <- scfpdata
## 5. clean data... make factors
scfpdata <- na.omit(scfpdata)

scfpdata <- scfpdata %>%
  dplyr::mutate(
    WOMEN = ifelse(WOMEN == 2, 1,0), # mAke integer?
    MARRIED = ifelse(MARRIED == 1, 1, 0),
    AGECL = factor(
      AGECL,levels = 1:6,ordered = TRUE
    ),
    EDCL = factor(EDCL,levels = 1:4,ordered = TRUE
    ),
    RACE = factor(RACE,levels = 1:5,ordered = TRUE
    ),
    OCCAT1 = factor(OCCAT1,levels = c(4,3,2,1),ordered = TRUE #does this actually work
    ),
    OCCAT2 = factor(OCCAT2,levels = c(4,3,2,1),ordered = TRUE
    ),
    WSAVED = factor(WSAVED,levels = 1:3,ordered = TRUE
    ),
    FINRISK = factor(FINRISK,levels = c(4,3,2,1),ordered = TRUE
    ),
    #risk_tol  = factor(risk_tol, levels = 1:4, ordered = TRUE)
  )
## 6. explore
scfpdata_num <- na.omit(scfpdata_num)
# cor plot
dat_mat <- cor(scfpdata_num)

plot_ly(
  z = dat_mat,
  x = rownames(dat_mat),
  y = colnames(dat_mat),
  type = "heatmap",
  zmid = 0,
  colors = colorRamp(c("blue", "white", "red")),
  text = round(dat_mat, 2),
  texttemplate = "%{text}",
  textfont = list(size = 9)
)
#bi
ggplot(
  scfpdata, aes(NETWORTH,risk_tol)) +
  geom_point()

n <- length(scfpdata$risk_tol)
risk_tol_q4 <- character(n)

nonzero_vals <- scfpdata$risk_tol[scfpdata$risk_tol > 0]
q <- quantile(nonzero_vals, probs = c(1/3, 2/3))

for (i in 1:n) {
  if (scfpdata$risk_tol[i] == 0) {
    risk_tol_q4[i] <- "None"
  } else if (scfpdata$risk_tol[i] <= q[1]) {
    risk_tol_q4[i] <- "Low"
  } else if (scfpdata$risk_tol[i] <= q[2]) {
    risk_tol_q4[i] <- "Medium"
  } else {
    risk_tol_q4[i] <- "High"
  }
}

scfpdata$risk_tol_q4 <- factor(risk_tol_q4, levels = c("None", "Low", "Medium", "High"))
table(scfpdata$risk_tol_q4)


# models
# split
set.seed(2465)
scfp_index <- c(1:nrow(scfpdata))
trainprop <- 0.7
train <- sample(
    scfp_index,
    length(scfp_index)*trainprop,
    replace = FALSE
)
traindata <- scfpdata[train, ]
testdata <- scfpdata[-train, names(scfpdata) != "risk_tol_q4"]
risk_tol_train <- scfpdata[train, "risk_tol_q4"]
risk_tol_test <- scfpdata[-train, "risk_tol_q4"]

####
library(rpart)
library(rpart.plot)

testtree <- rpart(
    risk_tol_q4 ~. -risk_tol -NETWORTH - INCOME,
    traindata,
    control = rpart.control(cp=0,xval=10,minbucket = 100))

testtree2 <- rpart(
            risk_tol_q4 ~. -risk_tol -NETWORTH - INCOME,
            traindata,
            method = "class",
            control = rpart.control(
                cp = 0,
                xval = 10, 
                minbucket = 5, 
                minsplit = 10
            )
        )
testtree$cptable #cptable shows 0 as an optimal. but its a stump

#prune
ghtree_pruned <- prune(testtree, testtree$cptable[5,1])

rpart.plot(ghtree_pruned)
#make a prediction
#make confusion matrix
confusion_ghtree5 <- table(
  predict(ghtree_pruned, newdata= testdata, type = "class"),risk_tol_test)
confusion_ghtree5
1-sum(diag(confusion_ghtree5))/sum(confusion_ghtree5)
###
minbucketvector <- c(5,10,20,50,100,500,1000)
minsplit_vec <- c(10,50,100)
tree_errors <- matrix(NA,length(minsplit_vec),length(minbucketvector))
# seems like minsplit doesn't do shit
for(i in 1:length(minbucketvector)) {              # loop through minbuckets
    
    print(paste("MinBucket:",minbucketvector[i]))

    for(m in 1:length(minsplit_vec)) {             # loop through minsplits

        print(paste("MinSplit:", minsplit_vec[m]))
        #tree
        testtree <- rpart(
            risk_tol_q4 ~. -risk_tol,
            traindata,
            method = "class",
            control = rpart.control(
                cp = 0,
                xval = 10, 
                minbucket = minbucketvector[i], 
                minsplit = minsplit_vec[m]
            )
        )

        #save build & predict
        treelist[[i]] <- testtree
        yhat_tree <- predict(
            testtree, newdata = traindata[, names(traindata) != "risk_tol_q4"], 
            type = "class"
        )
        tree_confusion <- table(yhat_tree, traindata[, "risk_tol_q4"])
        #errors 
        tree_errors[m,i] <- 1-sum(diag(tree_confusion))/sum(tree_confusion)
        print(paste("Tree Error:", tree_errors[m,i]))
    }


}





# lm
test <- lm(risk_tol_q4 ~ . -risk_tol, scfpdata)
summary(test)


# models
# split
set.seed(2465)
scfp_index <- c(1:nrow(scfpdata))
trainprop <- 0.7
train <- sample(
    scfp_index,
    length(scfp_index)*trainprop,
    replace = FALSE
)
traindata <- scfpdata[train, ]
testdata <- scfpdata[-train, names(scfpdata) != "risk_tol"]
risk_tol_train <- scfpdata[train, "risk_tol"]
risk_tol_test <- scfpdata[-train, "risk_tol"]
# lm
test <- lm(risk_tol ~ ., traindata)
summary(test)

yhat_lm <- predict(
  test, newdata = traindata[, names(traindata) != "risk_tol"])

mean((risk_tol_train - yhat_lm)^2)


summary(scfpdata$risk_tol)





###
# MAKE FUNCTIONS FOR REPEATED ACTIONS like MSE and errors

# standard error
stderror <- function(true,pred) {
  
}
# weighted error
WER <- function(true,pred,weight) {
  
}
# accuracy

# recall

# precision

# other?

# confusion martix

# logit threshold
