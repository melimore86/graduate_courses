rm(list=ls()); setwd('h:/x/workshops/uf2019/rpres')
#Tools and then install packages from zip. No code to download this package

library(RPresence)

#===========================================================
#   Exanple1.r - Single-season model with Salamander data (pg 135 in new occ. book)

#        load a csv file with detection-histories
salmdr.csv<-read.csv("Blue_Ridge_pg135.csv",header=FALSE)

#         Create PRESENCE input file object from csv
salmdr.data<-createPao(salmdr.csv,paoname="salmdr.pao")

##       fit some models
salmod1<-occMod(model=list(psi~1,p~1)     ,data=salmdr.data,type="so")

##  check out design matrices for model...
print(salmod1$dmat)
#$`psi`
#    a1
#psi "1"      meaning... logit(psi1) = 1 * a1 ... or psi1 = exp(a1)/(1+exp(a1))  same for all sites
#
#$p
#   b1
#p1 "1"                  logit(p1) = 1*b1        or p1=exp(b1)/(1+exp(b1))
#p2 "1"                  logit(p2) = 1*b1
#p3 "1"                  logit(p3) = 1*b1
#p4 "1"                  logit(p4) = 1*b1
#p5 "1"                  logit(p5) = 1*b1
#
#   so, p1=p2=p3=p4=p5

salmod2<-occMod(model=list(psi~1,p~SURVEY),data=salmdr.data,type="so",VCoutopt="noreal")

##    design matrices...
print(salmod2$dmat)
#$`psi`
#    a1
#psi "1"                       ... as in prev model, psi1=exp(a1)/(1+exp(a1))  same for all sites
#
#$p
#    b1  b2          b3          b4          b5
#p1 "1" "p.SURVEY2" "p.SURVEY3" "p.SURVEY4" "p.SURVEY5"  logit(p1) = 1*b1+0*b2+0*b3+0*b4+0*b5
#p2 "1" "p.SURVEY2" "p.SURVEY3" "p.SURVEY4" "p.SURVEY5"  logit(p2) = 1*b1+1*b2+0*b3+0*b4+0*b5
#p3 "1" "p.SURVEY2" "p.SURVEY3" "p.SURVEY4" "p.SURVEY5"  logit(p3) = 1*b1+0*b2+1*b3+0*b4+0*b5
#p4 "1" "p.SURVEY2" "p.SURVEY3" "p.SURVEY4" "p.SURVEY5"  logit(p4) = 1*b1+0*b2+0*b3+1*b4+0*b5
#p5 "1" "p.SURVEY2" "p.SURVEY3" "p.SURVEY4" "p.SURVEY5"  logit(p5) = 1*b1+0*b2+0*b3+0*b4+1*b5
#
#      p.SURVEY2 is a "survey" covariate which equals 1 if survey=2, 0 otherwise

##    In Presence, the design matrix would be...
#   b1 b2 b3 b4 b5
#p1  1  0  0  0  0           logit(p1) = 1*b1+0*b2+0*b3+0*b4+0*b5
#p2  1  1  0  0  0           logit(p2) = 1*b1+1*b2+0*b3+0*b4+0*b5
#p3  1  0  1  0  0           logit(p3) = 1*b1+0*b2+1*b3+0*b4+0*b5
#p4  1  0  0  1  0           logit(p4) = 1*b1+0*b2+0*b3+1*b4+0*b5
#p5  1  0  0  0  1           logit(p5) = 1*b1+0*b2+0*b3+0*b4+1*b5
#
#     Like Presence, there are usually more than one way to build a design matrix
#     to produce the same model.  For example, we could have the following in Presence:
#
#   b1 b2 b3 b4 b5
#p1  1  0  0  0  0            logit(p1) = 1*b1+0*b2+0*b3+0*b4+0*b5
#p2  0  1  0  0  0            logit(p2) = 0*b1+1*b2+0*b3+0*b4+0*b5
#p3  0  0  1  0  0            logit(p3) = 0*b1+0*b2+1*b3+0*b4+0*b5
#p4  0  0  0  1  0            logit(p4) = 0*b1+0*b2+0*b3+1*b4+0*b5
#p5  0  0  0  0  1            logit(p5) = 0*b1+0*b2+0*b3+0*b4+1*b5
#
#   With RPresence, we could have the following to get the above design matrix:
#
salmod2<-occMod(model=list(psi~1,p~-1+SURVEY),data=salmdr.data,type="so",VCoutopt="noreal")

print(salmod2$dmat)   #  print design matrix...
#$`psi`
#    a1
#psi "1"
#
#$p
#       b1          b2          b3          b4          b5
#p1 "p.SURVEY1" "p.SURVEY2" "p.SURVEY3" "p.SURVEY4" "p.SURVEY5"
#p2 "p.SURVEY1" "p.SURVEY2" "p.SURVEY3" "p.SURVEY4" "p.SURVEY5"
#p3 "p.SURVEY1" "p.SURVEY2" "p.SURVEY3" "p.SURVEY4" "p.SURVEY5"
#p4 "p.SURVEY1" "p.SURVEY2" "p.SURVEY3" "p.SURVEY4" "p.SURVEY5"
#p5 "p.SURVEY1" "p.SURVEY2" "p.SURVEY3" "p.SURVEY4" "p.SURVEY5"


##       create AIC table
models<-list(salmod1,salmod2)
results<-createAicTable(models)
cat('Blue-Ridge example (table 4.2 in book)\n'); print(summary(results))

##        print predicted real estimates of psi for 1st 6 sites...
salmod1_psi.real=predict(salmod1,salmdr.data$survcov,param="psi",conf=0.95);
print(head(salmod1_psi.real))
#        or...
print(head(salmod1$real$psi))
#
#        predict function is useful for printing estimates of sites NOT visited.
