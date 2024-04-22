library(readxl)
Master <- read_excel("C:/Users/15189/Desktop/Neutrophil Data/Master WT and kcn data for figures.xlsx")


attach(Master)
# load library ggplot2 
library(ggplot2)
library(ggsignif)

#Separating the WT and kcnh2a groups
WT <- `Neutrophil Area (%)`[Genotype == "Wild-type"]
kcnh2a <- `Neutrophil Area (%)`[Genotype == "kcnh2a"]

#Looking at summaries

#summary for WT
summary(WT)
sd(WT)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 11.50   16.30   18.75   18.68   21.43   30.47 
# SD: 4.322771

#Summary for kcnh2a
summary(kcnh2a)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 17.09   22.68   25.61   25.83   29.76   39.62
sd(kcnh2a)
hist(kcnh2a)
# SD: 5.303663
#saving as pdf

#pdf(file = "C:/Users/15189/Desktop/Neutrophil Data/Test plot1.pdf")
# boxplot(WT, kcnh2a, names = c("Wild-Type", "kcnh2a"), ylab = "Neutrophil Percentage", xlab = "Genotype")
# dev.off()
# 
# #Figure of results
# plot(rep(1:2,c(38,40))+ runif(78,-.025,.025),c(WT, kcnh2a), 
#      pch=20, xaxt = "n", ylab = "Percent Area of neutrophils in ROI", xlab = "Genotype")
# axis(side =1, at=c(1,2), labels = c("wt", "kcnh2a"))

#Trying dotplot with ggplot
# plot <-ggplot(Master, aes(x=Group, y=Area.avg, color = Group)) + 
#   geom_jitter(binaxis='y', stackdir='center', width = 0.1, height= 0.1)
# 
# plot + stat_summary(fun=mean, geom="point", shape=18,size=3, color="red") + 
#                     stat_summary(fun=median, geom="point", shape=18,size=3, color="blue")
#############
#Modifying data frame to have order be WT then k2

#Creating new data frame
new_master <- Master
#reordering groups so WT comes first           
new_master$Genotype <- factor(new_master$Genotype, levels = c("Wild-type", "kcnh2a"))

#re reunning plot

plot <-ggplot(new_master, aes(x=Genotype, y=`Neutrophil Area (%)`, color = Genotype)) + 
  geom_jitter(binaxis='y', stackdir='center', width = 0.1, height= 0.1)

plot + stat_summary(fun=mean, geom="point", shape=18,size=3, color = "blue") + 
    theme(legend.position="none", text = element_text(size = 15)) + theme_classic()+
  ylim(0,60) + scale_x_discrete(labels = c('Wild-type', expression(italic('kcnh2a -/+')))) +
  scale_color_manual(labels = c("Wild-type", expression(italic('kcnh2a -/+')) ), values = c("coral", "cyan3"))
  

#Creating boxplot
ggplot(new_master, aes(x=Genotype, y=`Neutrophil Area (%)`, fill=Genotype)) + 
  geom_boxplot(alpha=0.3, show.legend = FALSE) +
  theme(legend.position="none", text = element_text(size = 15)) + theme_classic()+
  ylim(0,60) +
  scale_x_discrete(labels = c('Wild-type', expression(italic('kcnh2a -/+')))) +
  scale_color_manual(labels = c("Wild-type", expression(italic('kcnh2a -/+')) ), values = c("coral", "cyan3"))



######################################################
par(mfrow = c(2,1))
hist(WT)

hist(kcnh2a)

#Running a T Test
neutrophil.test <- t.test(kcnh2a, WT, var.equal = TRUE)

neutrophil.test

# Two Sample t-test
# 
# data:  kcnh2a and WT
# t = 4.8192, df = 56, p-value = 1.14e-05
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   4.145765 10.044309
# sample estimates:
#   mean of x mean of y 
# 25.46330  18.36826 




### Permutation Test for means
p = 20000
diff.orig <- mean(`Neutrophil Area (%)`[Genotype == "kcnh2a"]) - mean(`Neutrophil Area (%)`[Genotype == "Wild-type"])
perm.mean <- NULL
for (i in 1:p) {
  shuffle.Genotype <- sample(Genotype)
  diff.new <- mean(`Neutrophil Area (%)`[shuffle.Genotype == "kcnh2a"]) - mean(`Neutrophil Area (%)`[shuffle.Genotype == "Wild-type"])
  perm.mean <- c(perm.mean,diff.new)}
perm.mean
diff.orig
hist(perm.mean)
abline(v=diff.orig,col="red")
p.val <- ( sum(perm.mean >= diff.orig) + 1 )/(p + 1)
p.val
# p-value : 4.99975e-05

#Bootstrap confidence interval for means
L.WT <- length(WT)
L.K2 <- length(kcnh2a)
boot <- 50000
means.Genotype.diff <- NULL
for(i in 1:boot){
  stuff.shuffle.WT <- sample(WT, size=L.WT, replace = T)
  stuff.shuffle.K2 <- sample(kcnh2a, size=L.K2, replace = T)
  means.diff.boot <- mean(stuff.shuffle.K2)- mean(stuff.shuffle.WT)
  means.Genotype.diff <- c(means.Genotype.diff,means.diff.boot)
}
hist(means.Genotype.diff, freq = F, main = "Boostrapped distrubution of difference in neutrophil area between kcnh2a and WT", cex.main = 0.5)
quantile(means.Genotype.diff, probs = c(0.025,0.975))




### Permutation Test for medians
p = 50000
diff.orig.median <- median(`Neutrophil Area (%)`[Genotype == "kcnh2a"]) - median(`Neutrophil Area (%)`[Genotype == "Wild-type"])
perm.median <- NULL
for (i in 1:p) {
  shuffle.Genotype <- sample(Genotype)
  diff.new.median <- median(`Neutrophil Area (%)`[shuffle.Genotype == "kcnh2a"]) - median(`Neutrophil Area (%)`[shuffle.Genotype == "Wild-type"])
  perm.median <- c(perm.median,diff.new.median)}
perm.median
diff.orig.median
hist(perm.median)
abline(v=diff.orig.median,col="red")
p.val <- ( sum(perm.median >= diff.orig.median) + 1 )/(p + 1)
p.val


#Bootstrap confidence interval
L.WT <- length(WT)
L.K2 <- length(kcnh2a)
boot <- 50000
median.Genotype.diff <- NULL
for(i in 1:boot){
  stuff.shuffle.WT <- sample(WT, size=L.WT, replace = T)
  stuff.shuffle.K2 <- sample(kcnh2a, size=L.K2, replace = T)
  median.diff.boot <- median(stuff.shuffle.K2)- median(stuff.shuffle.WT)
  median.Genotype.diff <- c(median.Genotype.diff,median.diff.boot)
}
hist(median.Genotype.diff, freq = F, main = "Boostrapped distrubution of difference in neutrophil area between kcnh2a and WT", cex.main = 0.5)
quantile(median.Genotype.diff, probs = c(0.025,0.975))
# 2.5%    97.5% 
#   4.292157 9.985514