load("databp.RData") 
attach(databp)
# complete case analysis 
ind <- which(R == 1)
ccmean <- mean(recovtime, na.rm = TRUE) 
ccsd <-  sd(recovtime, na.rm = TRUE)/sqrt(length(ind)) 
cc_cor1<- cor(recovtime,logdose,use = "complete.obs",method = "pearson")
cc_cor2<- cor(recovtime,bloodp ,use = "complete.obs",method = "pearson")
ccmean;ccsd;cc_cor1;cc_cor2
#19.27273;2.603013;0.2391256;-0.01952862

#mean imputation
mi_recovtime <- ifelse(R == 0, mean(recovtime, na.rm = TRUE), recovtime)
mimean <- mean(mi_recovtime) 
n <- nrow(databp) 
misd <- sd(mi_recovtime)/sqrt(n)
mi_cor1<- cor(mi_recovtime,logdose,use = "complete.obs",method = "pearson")
mi_cor2<- cor(mi_recovtime,bloodp ,use = "complete.obs",method = "pearson")
mimean;misd;mi_cor1;mi_cor2
#19.27273&2.284135&0.2150612&-0.01934126


#mean regression imputation
fitrecovtime <- lm(recovtime ~ logdose + bloodp)
summary(fitrecovtime)
stdres <- rstandard(fitrecovtime)
plot(fitrecovtime$fitted.values, stdres, xlab = "Fitted values", ylab = "Studentized residuals")
qqnorm(stdres) 
qqline(stdres,col=3)

predri <- predict(fitrecovtime,newdata=databp) 
predri[4]; predri[10]; predri[22]

#regression imputation
ri_recovtime <- ifelse(R == 0, predri, recovtime)
rimean <- mean(ri_recovtime) 
risd <- sd(ri_recovtime)/sqrt(n)
ri_cor1<- cor(ri_recovtime,logdose,use = "complete.obs",method = "pearson")
ri_cor2<- cor(ri_recovtime,bloodp ,use = "complete.obs",method = "pearson")
rimean;risd;ri_cor1;ri_cor2
#19.44428 & 2.312845 & 0.2801835 & -0.0111364

#stochastic regression imputation
predsri <- predict(fitrecovtime, newdata = databp) + rnorm(n, 0, summary(fitrecovtime)$sigma) 
predsri[4]; predsri[10]; predsri[22]
sri_recovtime <- ifelse(R == 0, predsri, recovtime)
srimean <- mean(sri_recovtime) 
srisd <- sd(sri_recovtime)/sqrt(n) 
sri_cor1<- cor(sri_recovtime,logdose,use = "complete.obs",method = "pearson")
sri_cor2<- cor(sri_recovtime,bloodp ,use = "complete.obs",method = "pearson")
srimean;srisd;sri_cor1;sri_cor2
#20.68773 & 2.544854 & 0.1924259 & -0.0231179 

##############################################################################
#Do you need any extra care when
#conducting stochastic regression imputation in this example?
plot(fitrecovtime)
library(ggplot2)
library(magrittr)
library("ggpubr")
ggscatter(databp,x = "bloodp", y = "recovtime", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "bloodp", ylab = "recovtime")
ggscatter(databp,x = "logdose", y = "recovtime", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "logdose", ylab = "recovtime")
##############################################################################
#predictive mean matching 
square_4 = (predri - 14.26254)^2;order_4 = order(square_4)
square_10 = (predri - 21.51562)^2;order_10 = order(square_10)
square_22 = (predri - 26.32896)^2;order_22 = order(square_22)
pmm_recovtime <- ri_recovtime
pmm_recovtime[4] <- ri_recovtime[order_4[2]]
pmm_recovtime[10] <- ri_recovtime[order_10[2]]
pmm_recovtime[22] <- ri_recovtime[order_22[2]]
pmmmean <- mean(pmm_recovtime) 
pmmsd <- sd(pmm_recovtime)/sqrt(n) 
pmm_cor1<- cor(pmm_recovtime,logdose,use = "complete.obs",method = "pearson")
pmm_cor2<- cor(sri_recovtime,bloodp ,use = "complete.obs",method = "pearson")
pmmmean;pmmsd;pmm_cor1;pmm_cor2
#19.44 & 2.464467 & 0.3037945 & -0.04613915

#plot heteroscedastic
library(ggplot2)
library(patchwork)
databp_plot = data.frame(databp,ri_recovtime,pmm_recovtime)
x = seq(1,25)
plot_ri <- ggplot(databp,aes(x,y = ri_recovtime,colour = as.factor(R)))+
   scale_color_manual(values = c("#C4961A", "#293352"))+
   theme(legend.position = "top")+
   geom_point()

plot_pmm <- ggplot(databp,aes(x,y = pmm_recovtime,colour = as.factor(R)))+
   scale_color_manual(values = c("#C4961A", "#293352"))+
   theme(legend.position = "top")+
   geom_point()

plot_ri + plot_pmm + plot_layout(ncol=2,widths=c(1,1))

