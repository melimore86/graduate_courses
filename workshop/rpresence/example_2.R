rm(list=ls()); setwd('h:/x/workshops/uf2019/rpres')
#Tools and then install packages from zip. No code to download this package

library(RPresence)

#===========================================================
#   Exanple2.r - Single-season model with bobcat data (w/ covariates)
#

#    read detection history data from csv file,
csv<-read.csv("data/Bobcat-3day.csv",as.is=T)

dethist=csv[,-1];  #  get rid of 1st column (site name)
sitenames=csv[,1]  #  sitenames in 1st column
nsites=nrow(dethist); nsrvys=ncol(dethist)  #  set number of sites,surveys from det. history data

#  read covariate table, exclding 1st column (sitenames)
cov1=read.csv("data/Bobcat_site_covar_3Day_class.csv",as.is=T,nrows=nsites)[,-1]
cov1=as.data.frame(cov1)
cov2=data.frame(SRVY=as.factor(rep(1:nsrvys,each=nsites)))

#          create input "pao" object, for use with occMod function
data=createPao(dethist,unitcov=cov1,survcov=cov2,title="Bobcat example",unitnames=sitenames)

mods=list(); i=0

i=i+1; mods[[i]]=occMod(model=list(psi~1,p~1),            data=data,type="so");
##         same as first model in example1

















i=i+1; mods[[i]]=occMod(model=list(psi~1,p~Trail),data=data,paoname=NULL,type="so");
##         detection(p) is function of Trail (ie., we have p(trail) and p(offtrail))
#
print(mods[[i]]$dmat$p,quote=F)
#   b1    b2
#p1 1 p.Trail        logit(p1) = b1 + b2*(0 if offtrail, 1 if on trail)
#p2 1 p.Trail
#p3 1 p.Trail
#p4 1 p.Trail
#p5 1 p.Trail
#p6 1 p.Trail
#p7 1 p.Trail
#
print(unique(mods[[2]]$real$p))  #  print estimates of p
#                     est          se      lower_0.95 upper_0.95
#p1_C & O Canal 10A 0.18289326 0.014438885 0.15627169 0.21290555   (which one is ontrail?)
#p1_C & O Canal 10C 0.03601525 0.004800781 0.02770209 0.04670328
#
print(mods[[2]]$beta$p)
#          est       se
#p1.Int  -3.287133 0.138280
#p.Trail  1.790266 0.144963   <-===== difference in p's is positive, so p(trail)>p(offtrail)

########################   ok, let's try a model with a covariate with > 2 categories...

i=i+1; mods[[i]]=occMod(model=list(psi~ABC, p~1),data=data,paoname=NULL,type="so");

print(mods[[i]]$dmat$psi)    ##     print design matrix...
#     a1    a2     a3
#psi  1 psi.ABCB psi.ABCC       #  RPresence created new covariates, psi.ABCB and psi.ABCC
#  where psi.ABCB=1 if ABC covariate="B", 0 otherwise,
#   and  psi.ABCC=1 if ABC covariate="C", 0 otherwise.
#  What about sites where ABC=A?
#    It's handled by the intercept term, a1=1
#  So, for site w/ ABC=A, psi = 1*a1 + 0*a2 + 0*a3
#                  ABC=B, psi = 1*a1 + 1*a2 + 0*a3
#                  ABC=C, psi = 1*a1 + 0*a2 + 1*a3
#  If using Presence, you would need to create those covariates
#  and put them into the site covariates table.

print(unique(mods[[i]]$real$psi))        #  print real estimates of occupancy...
#                      est        se      lower_0.95 upper_0.95
#psi_C & O Canal 10A 0.3795080 0.03262930 0.31794810  0.4452068   site labels show which is A, B, or C
#psi_C & O Canal 10C 0.1111059 0.01721709 0.08157220  0.1495909
#psi_C & O Canal 11B 0.1064011 0.01668808 0.07784119  0.1438056

print(mods[[i]]$beta$psi)        #  print beta estimates...
#            est       se
#psi.Int  -0.491637 0.138565
#psi.ABCB -1.636404 0.203186
#psi.ABCC -1.587857 0.202047


















############################  How about a model with a "continuous" covariate (detection distance)...

#   With a "categorical" covariate, there can be many categories (eg., A,B,C), but there is no order
#   among them (ie., we don't assume A < B < C, they're just different).  To estimate the parameter
#   for a categorical covariate with N categories, we need to estimate N beta parameters.
#
#   With a "continuous" covariate, there can be few or many values, but there is an order to them.
#   ( 0 < 0.5 < 1.0 < 1.5 ...).  Since there is a linear relationship (on the logit scale) to the
#   covariates, we only need 2 beta parameters to get all "real" parmameter estimates.
#
#   Caution:  Sometimes, folks use numeric values for categorical covariates  (eg., surrounding
#    vegatation = 1(grass), 2(shrubs), 3(barren)).  In this case, we need to convert the numeric values
#    for the covariate into character values so they are treated as categories.  Otherwise, RPresence
#    will work and produce a model where p(grass) < p(shrubs) < p(barren) (or the opposite order).

i=i+1; mods[[i]]=occMod(model=list(psi~1,p~Det_dist),data=data,paoname=NULL,type="so")

print(mods[[i]]$dmat$p,quote=F)
#   b1      b2
#p1 1 p.Det_dist       logit(p1) = 1*b1 + dist*b2
#p2 1 p.Det_dist          p2=p3=p4=p5=p6=p7= p1
#p3 1 p.Det_dist
#p4 1 p.Det_dist        With continuous covariates, we're forcing a linear relationship
#p5 1 p.Det_dist        between the covariate and the parameter (p).
#p6 1 p.Det_dist
#p7 1 p.Det_dist

print(mods[[i]]$beta$p)     #  look at beta's...
#             est       se
#p1.Int     -1.567032 0.240689        logit(p1) = -1.56 - 0.24*dist
#p.Det_dist -0.235397 0.190751           where dist ranges from 1.0 to 2.0

#   Rather than print the real p's, lets plot them in relation to det_dist...
dist=cov1$Det_dist                                      #  distance covariate for each site
b1=mods[[i]]$beta$p$est[1]; b2=mods[[i]]$beta$p$est[2]  #  beta (b1,b2) estimates from model
p=plogis(b1 + b2*dist)                                  #  estimated p1 for each site  (p2=p3=...=p1)
plot(dist,p,ylim=c(0,1))

dist=seq(1,3,.1)                    #  If we wanted to see what p would be for greater distances...
p=plogis(b1 + b2*dist)
plot(dist,p,ylim=c(0,1),type='l')












#######################  Next, a model with additive effect of dist and trail...

i=i+1; mods[[i]]=occMod(model=list(psi~1,p~Det_dist+Trail),data=data,paoname=NULL,type="so")

print(mods[[i]]$dmat$p,quote=F)
#  b1  b2           b3
#p1 1 p.Det_dist p.Trail              logit(p1) = 1*b1 + dist*b2 + trail*b3
#p2 1 p.Det_dist p.Trail              logit(p1) = b1 + dist*b2      if trail=0
#p3 1 p.Det_dist p.Trail              logit(p1) = b1 + dist*b2 + b3 if trail=1
#p4 1 p.Det_dist p.Trail
#p5 1 p.Det_dist p.Trail
#p6 1 p.Det_dist p.Trail
#p7 1 p.Det_dist p.Trail

print(mods[[i]]$beta$p)     #  look at beta's...
#              est       se
#p1.Int     -2.714265 0.248988           Intercept
#p.Det_dist -0.505190 0.186849           effect of distance, since < 0, detection decreases with increasing distance
#p.Trail     1.824223 0.145160           effect of trail,  since > 0, detection higher on trail vs off trail

#   Rather than print the real p's, lets plot them in relation to det_dist...
dist=cov1$Det_dist                                      #  distance covariate for each site
b1=mods[[i]]$beta$p$est[1]; b2=mods[[i]]$beta$p$est[2]  #  beta (b1,b2) estimates from model
b3=mods[[i]]$beta$p$est[3]                              #   3rd beta is additive effect of trail
p_offtrail=plogis(b1 + b2*dist)                         #  estimated p1 for each trail=0 site  (p2=p3=...=p1)
plot(dist,p_offtrail,ylim=c(0,1),ylab='p')
p_trail=plogis(b1 + b2*dist + b3)                       #  estimated p1 for each trail=1 site  (p2=p3=...=p1)
points(dist,p_trail,col='blue')













#######################  Finally, a model with interaction effect of dist and trail...
#
#    Interaction effect means we're not forcing the difference between the two lines in the plot
#     (black line = p(offtrail),  blue line = p(trail)).  So, for some distances p(offtrail)<p(trail)
#     and for some distances, p(offtrail) > p(trail).

i=i+1; mods[[i]]=occMod(model=list(psi~1,p~Det_dist*Trail),data=data,paoname=NULL,type="so")

print(mods[[i]]$dmat$p,quote=F)
#   b1  b2           b3        b4
#p1 1 p.Det_dist p.Trail p.Det_dist:Trail        logit(p1) = 1*b1 + dist*b2 + trail*b3 + dist*trail*b4
#p2 1 p.Det_dist p.Trail p.Det_dist:Trail
#p3 1 p.Det_dist p.Trail p.Det_dist:Trail        logit(p1) = b1 + dist*b2                if trail=0
#p4 1 p.Det_dist p.Trail p.Det_dist:Trail        logit(p1) = b1 + dist*b2 + b3 + dist*b4 if trail=1
#p5 1 p.Det_dist p.Trail p.Det_dist:Trail
#p6 1 p.Det_dist p.Trail p.Det_dist:Trail
#p7 1 p.Det_dist p.Trail p.Det_dist:Trail

print(mods[[i]]$beta$p)     #  look at beta's...
#                   est       se
#p1.Int           -2.427126 0.379104        Intercept for p(offtrail)
#p.Det_dist       -0.752058 0.311730        effect of distance for p(offtrail)
#p.Trail           1.366787 0.480696        difference in intercept between p(offtrail) and p(trail)
#p.Det_dist:Trail  0.388138 0.390460        difference in slope between p(offtrail) and p(trail)


#   Rather than print the real p's, lets plot them in relation to det_dist...
dist=cov1$Det_dist                                      #  distance covariate for each site
b1=mods[[i]]$beta$p$est[1]; b2=mods[[i]]$beta$p$est[2]  #  beta (b1,b2) estimates from model
b3=mods[[i]]$beta$p$est[3]; b4=mods[[i]]$beta$p$est[4]  #   3rd beta is additive effect of trail, 4th is slope difference
p_offtrail=plogis(b1 + b2*dist)                         #  estimated p1 for each trail=0 site  (p2=p3=...=p1)
plot(dist,p_offtrail,ylim=c(0,0.4),ylab='p')
p_trail=plogis(b1 + b2*dist + b3 + b4*dist)             #  estimated p1 for each trail=1 site  (p2=p3=...=p1)
points(dist,p_trail,col='blue')








#     create AIC table of model results and print
results1=createAicTable(mods);
cat('bobcat example\n');
print(results1$table)
#
#                     Model      AIC   neg2ll npar warn.conv warn.VC     DAIC modlike    wgt
#5 psi()p(Det_dist P Trail) 2849.777 2841.777    4         0       0   0.0000  1.0000 0.5961
#6 psi()p(Det_dist X Trail) 2850.781 2840.781    5         0       0   1.0043  0.6052 0.3608
#2            psi()p(Trail) 2855.029 2849.029    3         0       0   5.2523  0.0724 0.0431
#3              psi(ABC)p() 2895.752 2887.752    4         0       0  45.9757  0.0000 0.0000
#1                 psi()p() 2996.497 2992.497    2         0       0 146.7203  0.0000 0.0000
#4         psi()p(Det_dist) 2996.985 2990.985    3         0       0 147.2082  0.0000 0.0000

#    Notes:  1. This isn't a complete set of models.  Normally, we would want to find best model for
#            psi (ABC in this case) and best model for p (dist+trail) in the same model.