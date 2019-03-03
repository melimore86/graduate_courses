rm(list=ls()); setwd('h:/x/workshops/uf2019/rpres')
#Tools and then install packages from zip. No code to download this package

library(RPresence)

#===========================================================
#   Exanple3.r - Multi-season model with skinks data (w/ covariates)
#

csv=read.csv('GrandSkinks.csv',skip=2,header=F,stringsAsFactors=F)    #  read detection-history and habitat covariate from csv
dethist=csv[,2:16]; nsites=nrow(dethist)            #  detection histories are in cols 2-16
unitnames=csv[,1]                                   #  unitnames=sitenames in col 1
nsurveyseason=rep(3,5)                              #  3 surveys per season for 5 seasons
habitat=rep('pasture',nsites)                       #  set up habitat covariate, assigning all as 'pasture'
habitat[csv[,18]==0]='tussock'                      #  set habitat='tussock' for sites with covariate=0
dethist[dethist=='-']=NA                            #  change '-' in det hist data to NA

data=createPao(dethist,unitcov=data.frame(habitat=as.factor(habitat)),nsurveyseason=nsurveyseason,title="Grand Skinks")

m0<-occMod(model=list(psi~1,gamma~1,epsilon~1,p~1),data=data,type="do.1");  #  run model (psi()gam()eps()p())
#
print(m0$dmat,quote=F)
#$`psi`
#      a1
#psi1 1                design matrix for initial occupancy
#
#$gamma
#       b1
#gamma1 1              design matrix for local colonization (Pr(unocc site becomes occ))
#gamma2 1                note: 5 seasons -> 4 intervals between seasons
#gamma3 1                      so, 4 gamma's
#gamma4 1
#
#$epsilon
#         c1
#epsilon1 1            design matrix for local extinction (Pr(occ site becomes unocc))
#epsilon2 1
#epsilon3 1
#epsilon4 1
#
#$p
#       d1
#P[1-1] 1              design matrix for detection probability
#P[1-2] 1
#P[1-3] 1               First subscript is season, 2nd is survey within season
#P[2-1] 1
#P[2-2] 1               All detection probs set equal.
#P[2-3] 1
#P[3-1] 1
#P[3-2] 1
#P[3-3] 1
#P[4-1] 1
#P[4-2] 1
#P[4-3] 1
#P[5-1] 1
#P[5-2] 1
#P[5-3] 1
#
#$th0pi                  th0pi not used for this model, ignore
#NULL

print(unique(m0$real$psi))
#           est         se    lower_0.95 upper_0.95
#unit1_1 0.3945046 0.03148657  0.3347526   0.457586

print(unique(m0$real$gamma))
#                est         se     lower_0.95 upper_0.95
#gamma1_unit1 0.06828741 0.01186537 0.04839216 0.09554089

print(unique(m0$real$epsilon))
#                  est         se    lower_0.95 upper_0.95
#epsilon1_unit1 0.1000152 0.01826093 0.06948175  0.1419199

print(unique(m0$real$p))
#           est         se     lower_0.95 upper_0.95
#p1_unit1 0.6891544 0.02307923  0.6422196  0.7324968










########  run model with site covariate (habitat) on initial occupancy (psi)...

m1<-occMod(model=list(psi~habitat,gamma~1,epsilon~1,p~1),data=data,type="do.1"); # run model psi(hab)...

print(unique(m1$real$psi))      #   print init occupancy estimates
#             est         se    lower_0.95 upper_0.95
#unit1_1   0.2210798 0.04003130  0.1525153  0.3092214     (init occ for habitat=pasture)
#unit137_1 0.5275902 0.04301951  0.4432891  0.6103467     (init occ for habitat=tussock)

print(m1$beta$psi)              #   print occ beta estimates
#                     est       se
#psi1.Int           -1.259385 0.232465             (intercept)
#psi.habitattussock  1.369858 0.284932             (effect of tussock... psi(tussock)>psi(pasture))

print(unique(m1$real$gamma))
#                est         se      lower_0.95 upper_0.95
#gamma1_unit1 0.06598938 0.01171499 0.04641852 0.09300686

print(unique(m1$real$epsilon))
#                  est         se     lower_0.95 upper_0.95
#epsilon1_unit1 0.09977483 0.01827077 0.06924039  0.1417244

print(unique(m1$real$p))
#            est         se     lower_0.95 upper_0.95
#p1_unit1 0.6831905 0.02339268  0.6356754  0.7271684






########  run model same model with survey-specific detection (p)...

m2<-occMod(model=list(psi~habitat,gamma~1,epsilon~1,p~SEASON),data=data,type="do.1"); # run model psi(hab)...

print(m2$dmat$p,quote=F)
#       d1    d2        d3        d4        d5
#P[1-1] 1 p.SEASON2 p.SEASON3 p.SEASON4 p.SEASON5         d1 will be intercept term (= p[1-1])
#P[1-2] 1 p.SEASON2 p.SEASON3 p.SEASON4 p.SEASON5
#P[1-3] 1 p.SEASON2 p.SEASON3 p.SEASON4 p.SEASON5         d2 is effect of season 2
#P[2-1] 1 p.SEASON2 p.SEASON3 p.SEASON4 p.SEASON5            p.SEASON2 = 1 if in 4th-6th surveys
#P[2-2] 1 p.SEASON2 p.SEASON3 p.SEASON4 p.SEASON5                      =0  otherwise.
#P[2-3] 1 p.SEASON2 p.SEASON3 p.SEASON4 p.SEASON5            p.SEASON3 = 1 if in 7th-9th surveys
#P[3-1] 1 p.SEASON2 p.SEASON3 p.SEASON4 p.SEASON5                      =0  otherwise.
#P[3-2] 1 p.SEASON2 p.SEASON3 p.SEASON4 p.SEASON5
#P[3-3] 1 p.SEASON2 p.SEASON3 p.SEASON4 p.SEASON5
#P[4-1] 1 p.SEASON2 p.SEASON3 p.SEASON4 p.SEASON5
#P[4-2] 1 p.SEASON2 p.SEASON3 p.SEASON4 p.SEASON5
#P[4-3] 1 p.SEASON2 p.SEASON3 p.SEASON4 p.SEASON5
#P[5-1] 1 p.SEASON2 p.SEASON3 p.SEASON4 p.SEASON5
#P[5-2] 1 p.SEASON2 p.SEASON3 p.SEASON4 p.SEASON5
#P[5-3] 1 p.SEASON2 p.SEASON3 p.SEASON4 p.SEASON5

#   ... this is equivalent to...
#       d1  d2  d3  d4  d5
#P[1-1] 1   0   0   0   0                     (P[1-j] = invlogit(d1)])
#P[1-2] 1   0   0   0   0
#P[1-3] 1   0   0   0   0
#P[2-1] 1   1   0   0   0                     (P[2-j] = invlogit(d1 + d2)])
#P[2-2] 1   1   0   0   0                      (  so, d2 = difference between P[2-j] and P[1-j])
#P[2-3] 1   1   0   0   0
#P[3-1] 1   0   1   0   0                     (P[3-j] = invlogit(d1 + d3)])
#P[3-2] 1   0   1   0   0                      (  so, d3 = difference between P[3-j] and P[1-j])
#P[3-3] 1   0   1   0   0
#P[4-1] 1   0   0   1   0                     (P[4-j] = invlogit(d1 + d4)])
#P[4-2] 1   0   0   1   0                      (  so, d4 = difference between P[4-j] and P[1-j])
#P[4-3] 1   0   0   1   0
#P[5-1] 1   0   0   0   1                     (P[5-j] = invlogit(d1 + d5)])
#P[5-2] 1   0   0   0   1                      (  so, d5 = difference between P[5-j] and P[1-j])
#P[5-3] 1   0   0   0   1

print(unique(m2$real$psi))      #   print init occupancy estimates
#             est         se    lower_0.95 upper_0.95
#unit1_1   0.2267807 0.04128730  0.1560309  0.3175409    (init occ for habitat=pasture)
#unit137_1 0.5412078 0.04632877  0.4500450  0.6296932    (init occ for habitat=tussock)

print(unique(m2$real$p))
#            est         se     lower_0.95 upper_0.95
#p1_unit1  0.6310288 0.05550088  0.5173503  0.7318123      (detection for season 1)
#p4_unit1  0.6732915 0.04283320  0.5845452  0.7511509      (detection for season 2)
#p7_unit1  0.6848552 0.04061937  0.6004417  0.7586046      (detection for season 3)
#p10_unit1 0.8139590 0.04579191  0.7074964  0.8878166      (detection for season 4)
#p13_unit1 0.6226447 0.05078796  0.5192725  0.7159479      (detection for season 5)








########  run model same model with survey-specific detection (p) and additive effect of habitat ...

m3<-occMod(model=list(psi~habitat,gamma~1,epsilon~1,p~SEASON+habitat),data=data,type="do.1"); # run model psi(hab)...

print(m3$dmat$p,quote=F)
#       d1      d2          d3          d4          d5          d6
#P[1-1] 1  p.SEASON2 p.SEASON3 p.SEASON4 p.SEASON5 p.habitattussock
#P[1-2] 1  p.SEASON2 p.SEASON3 p.SEASON4 p.SEASON5 p.habitattussock
#P[1-3] 1  p.SEASON2 p.SEASON3 p.SEASON4 p.SEASON5 p.habitattussock
#P[2-1] 1  p.SEASON2 p.SEASON3 p.SEASON4 p.SEASON5 p.habitattussock
#P[2-2] 1  p.SEASON2 p.SEASON3 p.SEASON4 p.SEASON5 p.habitattussock
#P[2-3] 1  p.SEASON2 p.SEASON3 p.SEASON4 p.SEASON5 p.habitattussock
#P[3-1] 1  p.SEASON2 p.SEASON3 p.SEASON4 p.SEASON5 p.habitattussock
#P[3-2] 1  p.SEASON2 p.SEASON3 p.SEASON4 p.SEASON5 p.habitattussock
#P[3-3] 1  p.SEASON2 p.SEASON3 p.SEASON4 p.SEASON5 p.habitattussock
#P[4-1] 1  p.SEASON2 p.SEASON3 p.SEASON4 p.SEASON5 p.habitattussock
#P[4-2] 1  p.SEASON2 p.SEASON3 p.SEASON4 p.SEASON5 p.habitattussock
#P[4-3] 1  p.SEASON2 p.SEASON3 p.SEASON4 p.SEASON5 p.habitattussock
#P[5-1] 1  p.SEASON2 p.SEASON3 p.SEASON4 p.SEASON5 p.habitattussock
#P[5-2] 1  p.SEASON2 p.SEASON3 p.SEASON4 p.SEASON5 p.habitattussock
#P[5-3] 1  p.SEASON2 p.SEASON3 p.SEASON4 p.SEASON5 p.habitattussock

tmp=unique(m3$real$p); print(tmp)
#               est         se     lower_0.95 upper_0.95
#p1_unit1    0.6187701 0.07145048  0.4726952  0.7461134         (detection for season1, pasture)
#p1_unit137  0.6356050 0.05785138  0.5166896  0.7399858         (detection for season1, tussock)
#p4_unit1    0.6605253 0.06317365  0.5283315  0.7716804         (detection for season2, pasture)
#p4_unit137  0.6764804 0.04408550  0.5848788  0.7562915         (detection for season2, tussock)
#p7_unit1    0.6730860 0.05914977  0.5486738  0.7771333
#p7_unit137  0.6887290 0.04268343  0.5996373  0.7657405
#p10_unit1   0.8024137 0.06333449  0.6498774  0.8988396
#p10_unit137 0.8135823 0.04551249  0.7079202  0.8871154
#p13_unit1   0.6074576 0.07465441  0.4558691  0.7408248
#p13_unit137 0.6244890 0.05100577  0.5205737  0.7180786

##  let's plot the seasonal detection probs...

plot(1:5,tmp$est[c(1,3,5,7,9)],type='l',ylim=c(0,1),ylab='p',xlab='season')
lines(1:5,tmp$est[c(2,4,6,8,10)],col='blue')
legend(x=3,y=1,legend=c('tussock','pasture'),lty=c(1,1),col=c('blue','black'))

##  let's see what detection probs would be for a model where habitat effect
##   is not additive (ie., season-habitat interaction... p(pasture) not
##   necessarily always less than p(tussock))

m4<-occMod(model=list(psi~habitat,gamma~1,epsilon~1,p~SEASON*habitat),data=data,type="do.1"); # run model psi(hab)...

print(m4$dmat$p,quote=F)     #  look at design matrix for p


tmp=unique(m4$real$p)                                  #  get real estimates...
points(1:5,tmp$est[c(1,3,5,7,9)],col='black')          #  plot them on graph with lines
points(1:5,tmp$est[c(2,4,6,8,10)],col='blue',pch='x')  #  from prev. model.














#     create AIC table of model results and print
results1=createAicTable(list(m0,m1,m2,m3,m4));
cat('skinks example\n');
print(results1$table)
#
#                                        Model      AIC      neg2ll  npar warn.conv warn.VC    DAIC modlike    wgt
#3           psi(habitat)gamma()epsilon()p(SEASON) 1748.119 1730.119    9         0       0  0.0000  1.0000 0.5438
#4 psi(habitat)gamma()epsilon()p(SEASON P habitat) 1750.041 1730.041   10         0       0  1.9221  0.3825 0.2080
#5 psi(habitat)gamma()epsilon()p(SEASON X habitat) 1750.700 1722.700   14         0       0  2.5812  0.2751 0.1496
#2                 psi(habitat)gamma()epsilon()p() 1751.535 1741.535    5         0       0  3.4155  0.1813 0.0986
#1                        psi()gamma()epsilon()p() 1775.015 1767.015    4         0       0 26.8964  0.0000 0.0000














##   Assuming this is the complete model-set (it's not!), there is some support for all
##   of the models, except the first (psi()gamma()epsilon()p()).  We can get estimates
##   for pis,gamma,epsilon and p which account for model uncertainty.  To do this, we
##   need to compute a weighted average estimate for each parameter, using the model
##   weights.  For example,
##        model-averaged psi(site1) = psi(top model, site1) * .5438 +
##                                    psi(2nd model, site1) * .2080 +
##                                    psi(3rd model, site1) * .1496 +
##                                    psi(4th model, site1) * .0986 +
##                                    psi(5th model, site1) * .0000.
##   This could be computed for each site.  Then repeat the process for each other
##   parameter (gamma(1-4), epsilon(1-4), P[1-1 - 5-3]).  Variances are computed using
##   the model variances and variance among models.
##   Sounds like a lot of tedious work, but RPresence has a function to do it:

mavg_psi=modAvg(results1,'psi')      #  compute model-averaged estimates of init occ, psi
print(unique(mavg_psi))
#             est         se    lower_0.95 upper_0.95
#unit1_1   0.2243819 0.04144615  0.1535436  0.3157122       (estimate for pasture sites)
#unit137_1 0.5409022 0.04649848  0.4494181  0.6297099       (estimate for tussock sites)

mavg_p=modAvg(results1,'p')      #  compute model-averaged estimates of detection, p
print(unique(mavg_p))
#               est         se    lower_0.95 upper_0.95
#p1_unit1    0.6587944 0.08792013  0.4728576  0.8060474
#p1_unit137  0.6301783 0.05960455  0.5079273  0.7377394
#p4_unit1    0.6759034 0.05812860  0.5535237  0.7781813
#p4_unit137  0.6734687 0.04272286  0.5849537  0.7511376
#p7_unit1    0.6747474 0.05511325  0.5591062  0.7724042
#p7_unit137  0.6873186 0.04095165  0.6020801  0.7615300
#p10_unit1   0.7745590 0.08948311  0.5572101  0.9036648
#p10_unit137 0.8032989 0.05936194  0.6616626  0.8950477
#p13_unit1   0.6056170 0.08178055  0.4397526  0.7502633
#p13_unit137 0.6325452 0.05293420  0.5241780  0.7289922