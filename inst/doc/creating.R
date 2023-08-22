## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  message = FALSE,
  warning = FALSE,
  fig.height = 6,
  fig.width = 7,
  fig.path = "fig/tut01-",
  dev = "png",
  comment = "##"
)

# save some typing
knitr::set_alias(w = "fig.width",
                 h = "fig.height",
                 cap = "fig.cap")

# Old Sweave options
# \SweaveOpts{engine=R,eps=TRUE,height=6,width=7,results=hide,fig=FALSE,echo=TRUE}
# \SweaveOpts{engine=R,height=6,width=7,results=hide,fig=FALSE,echo=TRUE}
# \SweaveOpts{prefix.string=fig/vcd-tut,eps=FALSE}
# \SweaveOpts{keep.source=TRUE}


# preload datasets ???
set.seed(1071)
library(vcd)
library(vcdExtra)
library(ggplot2)
data(HairEyeColor)
data(PreSex)
data(Arthritis, package="vcd")
art <- xtabs(~Treatment + Improved, data = Arthritis)
if(!file.exists("fig")) dir.create("fig")


## ---- case-form---------------------------------------------------------------
names(Arthritis)      # show the variables

str(Arthritis)        # show the structure

head(Arthritis,5)     # first 5 observations, same as Arthritis[1:5,] 

## ---- frequency-form----------------------------------------------------------
# Agresti (2002), table 3.11, p. 106
GSS <- data.frame(
  expand.grid(sex = c("female", "male"), 
              party = c("dem", "indep", "rep")),
  count = c(279,165,73,47,225,191))

GSS
names(GSS)
str(GSS)

sum(GSS$count)

## ---- table-form1-------------------------------------------------------------
str(HairEyeColor)                      # show the structure

sum(HairEyeColor)                      # number of cases

sapply(dimnames(HairEyeColor), length) # table dimension sizes

## ---- table-form2-------------------------------------------------------------
# A 4 x 4 table  Agresti (2002, Table 2.8, p. 57) Job Satisfaction
JobSat <- matrix(c( 1, 2, 1, 0, 
                    3, 3, 6, 1, 
                   10,10,14, 9, 
                    6, 7,12,11), 4, 4)

dimnames(JobSat) = list(
  income = c("< 15k", "15-25k", "25-40k", "> 40k"),
  satisfaction = c("VeryD", "LittleD", "ModerateS", "VeryS")
  )

JobSat

## ---- table-form3-------------------------------------------------------------
JobSat <- as.table(JobSat)
str(JobSat)

## ---- relevel, eval=FALSE-----------------------------------------------------
#  dimnames(JobSat)$income <- c(7.5,20,32.5,60)
#  dimnames(JobSat)$satisfaction <- 1:4

## ---- reorder1----------------------------------------------------------------
HairEyeColor <- HairEyeColor[, c(1,3,4,2), ]
str(HairEyeColor)

## ---- reorder2, echo=TRUE, eval=FALSE-----------------------------------------
#  Arthritis <- read.csv("arthritis.txt",header=TRUE)
#  Arthritis$Improved <- ordered(Arthritis$Improved,
#                                levels=c("None", "Some", "Marked")
#                                )

## -----------------------------------------------------------------------------
data(Arthritis, package="vcd")
art <- xtabs(~Treatment + Improved, data = Arthritis)
mosaic(art, gp = shading_max, split_vertical = TRUE, main="Arthritis: [Treatment] [Improved]")

## ---- reorder3----------------------------------------------------------------
UCB <- aperm(UCBAdmissions, c(2, 1, 3))
dimnames(UCB)[[2]] <- c("Yes", "No")
names(dimnames(UCB)) <- c("Sex", "Admit?", "Department")

# display as a flattened table
stats::ftable(UCB)

## ---- structable--------------------------------------------------------------
structable(HairEyeColor)                   # show the table: default

structable(Hair+Sex ~ Eye, HairEyeColor)   # specify col ~ row variables

## ---- structable1,eval=FALSE--------------------------------------------------
#  HSE < - structable(Hair+Sex ~ Eye, HairEyeColor)   # save structable object
#  mosaic(HSE)                                        # plot it

## ---- table-setup-------------------------------------------------------------
 n=500
 A <- factor(sample(c("a1","a2"), n, rep=TRUE))
 B <- factor(sample(c("b1","b2"), n, rep=TRUE))
 C <- factor(sample(c("c1","c2"), n, rep=TRUE))
 mydata <- data.frame(A,B,C)

## ---- table-ex1---------------------------------------------------------------
# 2-Way Frequency Table
attach(mydata)
mytable <- table(A,B)   # A will be rows, B will be columns
mytable                 # print table

margin.table(mytable, 1) # A frequencies (summed over B)
margin.table(mytable, 2) # B frequencies (summed over A)

prop.table(mytable)    # cell percentages
prop.table(mytable, 1) # row percentages
prop.table(mytable, 2) # column percentages

## ---- table-ex2---------------------------------------------------------------
# 3-Way Frequency Table
mytable <- table(A, B, C)
ftable(mytable)

## ---- xtabs-ex1---------------------------------------------------------------
# 3-Way Frequency Table
mytable <- xtabs(~A+B+C, data=mydata)

ftable(mytable)    # print table

summary(mytable)   # chi-square test of indepedence

## ---- xtabs-ex2---------------------------------------------------------------
(GSStab <- xtabs(count ~ sex + party, data=GSS))

summary(GSStab)

## ---- dayton1-----------------------------------------------------------------
data("DaytonSurvey", package="vcdExtra")
str(DaytonSurvey)
head(DaytonSurvey)

## ---- dayton2-----------------------------------------------------------------
# data in frequency form
# collapse over sex and race
Dayton.ACM.df <- aggregate(Freq ~ cigarette+alcohol+marijuana, 
                           data=DaytonSurvey, 
                           FUN=sum)
Dayton.ACM.df

## ---- dayton3-----------------------------------------------------------------
# in table form
Dayton.tab <- xtabs(Freq ~ cigarette+alcohol+marijuana+sex+race, 
                    data=DaytonSurvey)
structable(cigarette+alcohol+marijuana ~ sex+race, 
           data=Dayton.tab)

## ---- dayton4-----------------------------------------------------------------
# collapse over sex and race
Dayton.ACM.tab <- apply(Dayton.tab, MARGIN=1:3, FUN=sum)
Dayton.ACM.tab <- margin.table(Dayton.tab, 1:3)   # same result

structable(cigarette+alcohol ~ marijuana, data=Dayton.ACM.tab)

## ---- dayton5-----------------------------------------------------------------
library(plyr)
Dayton.ACM.df <- plyr::ddply(DaytonSurvey, 
                             .(cigarette, alcohol, marijuana), 
                             plyr::summarise, Freq=sum(Freq))

Dayton.ACM.df

## ---- collapse1---------------------------------------------------------------
# create some sample data in frequency form
sex <- c("Male", "Female")
age <- c("10-19", "20-29",  "30-39", "40-49", "50-59", "60-69")
education <- c("low", 'med', 'high')
data <- expand.grid(sex=sex, age=age, education=education)
counts <- rpois(36, 100)   # random Possion cell frequencies
data <- cbind(data, counts)

# make it into a 3-way table
t1 <- xtabs(counts ~ sex + age + education, data=data)
structable(t1)

## ---- collapse2---------------------------------------------------------------
# collapse age to 3 levels, education to 2 levels
t2 <- collapse.table(t1, 
         age=c("10-29", "10-29",  "30-49", "30-49", "50-69", "50-69"),
         education=c("<high", "<high", "high"))
structable(t2)

## ----titanicp1----------------------------------------------------------------
table(Titanicp$sibsp, Titanicp$parch)

## ----titanicp2----------------------------------------------------------------
library(dplyr)
Titanicp <- Titanicp |>
  mutate(sibspF = case_match(sibsp,
                            0 ~ "0",
                            1 ~ "1",
                            2:max(sibsp) ~ "2+")) |>
  mutate(sibspF = ordered(sibspF)) |>
  mutate(parchF = case_match(parch,
                             0 ~ "0",
                             1 ~ "1",
                             2:max(parch) ~ "2+")) |>
  mutate(parchF = ordered(parchF)) 

table(Titanicp$sibspF, Titanicp$parchF)

## ---- convert-ex1-------------------------------------------------------------
as.data.frame(GSStab)

## ---- convert-ex2-------------------------------------------------------------
Art.tab <- with(Arthritis, table(Treatment, Sex, Improved))
str(Art.tab)

ftable(Art.tab)

## ---- convert-ex3-------------------------------------------------------------
Art.df <- expand.dft(Art.tab)
str(Art.df)

## ---- tv1---------------------------------------------------------------------
tv.data<-read.table(system.file("extdata","tv.dat", package="vcdExtra"))
head(tv.data,5)

## ---- tv2,eval=FALSE----------------------------------------------------------
#  tv.data<-read.table("C:/R/data/tv.dat")

## ---- tv3---------------------------------------------------------------------
TV <- array(tv.data[,5], dim=c(5,11,5,3))                                        
dimnames(TV) <- list(c("Monday","Tuesday","Wednesday","Thursday","Friday"), 
                     c("8:00","8:15","8:30","8:45","9:00","9:15","9:30",         
                       "9:45","10:00","10:15","10:30"),                            
                     c("ABC","CBS","NBC","Fox","Other"), 
                     c("Off","Switch","Persist"))

names(dimnames(TV))<-c("Day", "Time", "Network", "State")

## ---- tv3a,eval=FALSE---------------------------------------------------------
#  TV <- xtabs(V5 ~ ., data=tv.data)
#  dimnames(TV) <- list(Day = c("Monday","Tuesday","Wednesday","Thursday","Friday"),
#                       Time = c("8:00","8:15","8:30","8:45","9:00","9:15","9:30",
#                                "9:45","10:00","10:15","10:30"),
#                       Network = c("ABC","CBS","NBC","Fox","Other"),
#                       State = c("Off","Switch","Persist"))
#  
#  # table dimensions
#  dim(TV)

## ---- tv4---------------------------------------------------------------------
TV2 <- TV[,,1:3,]      # keep only ABC, CBS, NBC
TV2 <- TV2[,,,3]       # keep only Persist -- now a 3 way table
structable(TV2)

## ---- tv5---------------------------------------------------------------------
TV.df <- as.data.frame.table(TV2)
levels(TV.df$Time) <- c(rep("8:00", 2),
                        rep("8:30", 2),
                        rep("9:00", 2), 
                        rep("9:30", 2), 
                        rep("10:00",2),
                            "10:30"
                        )

TV3 <- xtabs(Freq ~ Day + Time + Network, TV.df)

structable(Day ~ Time+Network, TV3)

## ----tv-mosaic1, fig.height=6, fig.width=7------------------------------------
mosaic(TV3, shade = TRUE,
       labeling = labeling_border(rot_labels = c(0, 0, 0, 90)))

