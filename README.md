#missedastep

#secondaries


library(readr)
library(ggplot2)
library(meta)
library(forestplot)
library(stringr)

missedastep <- read_csv("/Users/hannahmoyer/Desktop/exporteddata.csv")

#stringent analysis 

pfsmissedastep <- subset(missedastep, IstherePFSdata == "yes" )

pfs_ma_mas <- metagen(
  log(hazard_ratio),
  PFSSE,
  data = pfsmissedastep,
  sm = "HR",
  byvar = stringentpos,
  comb.fixed = FALSE,
  method.tau = "PM", 
  adhoc.hakn = "ci",
)

summary(pfs_ma_mas)

saemissedastep <- subset(missedastep, IsthereSAEdata == "yes" )


sae_ma_mas <- metabin(
  Esae,
  EN,
  Csae,
  CN,
  data = saemissedastep, 
  byvar = stringentpos,
  comb.fixed = FALSE,
  method.tau = "PM", 
  adhoc.hakn = "ci",
)
summary(sae_ma_mas)

#permissive analysis 

pfsmissedastep <- subset(missedastep, IstherePFSdata == "yes" )

pfs_ma_masp <- metagen(
  log(hazard_ratio),
  PFSSE,
  data = pfsmissedastep,
  sm = "HR",
  byvar = permandstringent,
  comb.fixed = FALSE,
  method.tau = "PM", 
  adhoc.hakn = "ci",
)

summary(pfs_ma_masp)

saemissedastep <- subset(missedastep, IsthereSAEdata == "yes" )


sae_ma_masp <- metabin(
  Esae,
  EN,
  Csae,
  CN,
  data = saemissedastep, 
  byvar = permandstringent,
  comb.fixed = FALSE,
  method.tau = "PM", 
  adhoc.hakn = "ci",
)
summary(sae_ma_masp)

#forestplots 

#stingent 
pdf(
  "pfs_ma_mas.pdf",
  18,
  7.5
)

#Make a forest plot

forest(pfs_ma_mas,
       leftcols = c("P3"),
       leftlabs = c("Study"),
       just.addcols = "left",
       label.left = "<---Combination has fewer AEs---", 
       label.right = "---Comparator has fewer AEs--->", 
       fs.lr= 10
)

dev.off()

pdf(
  "sae_ma_mas.pdf",
  18,
  7.5
)

forest(sae_ma_mas,
       leftcols = c("P3","Esae", "EN","Csae","CN"),
       leftlabs = c("Study", "Events","Total ", "Events", "Total"),
       lab.e.attach.to.col = "EN",
       lab.c.attach.to.col = "CN",
       just.addcols = "left",
       label.left = "<---Combination has fewer AEs---", 
       label.right = "---Comparator has fewer AEs--->", 
       fs.lr= 10
)

dev.off()




#permissive

#pfs
pdf(
  "pfs_ma_masp.pdf",
  18,
  7.5
)


forest(pfs_ma_masp,
       leftcols = c("P3"),
       leftlabs = c("Study"),
       just.addcols = "left",
       label.left = "<---Combination has fewer AEs---", 
       label.right = "---Comparator has fewer AEs--->", 
       fs.lr= 10
)

dev.off()

#sae
pdf(
  "sae_ma_masp.pdf",
  18,
  7.5
)


forest(sae_ma_masp,
       leftcols = c("P3","Esae", "EN","Csae","CN"),
       leftlabs = c("Study", "Events","Total ", "Events", "Total"),
       lab.e.attach.to.col = "EN",
       lab.c.attach.to.col = "CN",
       just.addcols = "left",
       label.left = "<---Combination has fewer AEs---", 
       label.right = "---Comparator has fewer AEs--->", 
       fs.lr= 10
)

dev.off()

#secondaries 

#logistic regression 

missedastep$approval<-factor(missedastep$approval)
missedastep$Industy_binary<-factor(missedastep$Industy_binary)
missedastep$biomarker<-factor(missedastep$biomarker)
missedastep$drugclass<-factor(missedastep$drugclass)

mylogitbinary<-glm(stringentpos~biomarker+approval+Industy_binary+drugclass,data= missedastep,family="binomial")
summary(mylogitbinary)

#lasagna



