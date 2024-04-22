library(readxl)
MDA_ELISA_Larvae_data_analysis <- read_excel("MDA ELISA Larvae data analysis.xlsx")
library(ggplot2)
library(ggsignif)

attach(MDA_analysis)

wt <- `Concentration (ug/mL)`[Genotype == "Wild-type"]
kcnh2a <- `Concentration (ug/mL)`[Genotype == "kcnh2a"]

#
# combined <-c(wt, kcnh2a)
# combined.ind <- c(1,1,2,2,2)
# plot(combined, rep(0,5), pch=20, type= "n")
# points(wt,c(0,0), pch=20, col ="red")
# points(kcnh2a,c(0,0,0), pch=20, col ="blue")

#Trying a ggplot with the data
#Releveling data so Wild type comes first
new_MDA <- MDA_ELISA_Larvae_data_analysis

#reordering groups so WT comes first           
new_MDA$Genotype <- factor(new_MDA$Genotype, levels = c("Wild-type", "kcnh2a"))

#re reunning plot

plot <-ggplot(new_MDA, aes(x=Genotype, y=`Concentration (ug/mL)`, color = Genotype)) +
        geom_dotplot(binaxis='y', stackdir='center', dotsize =1) + theme(text = element_text(size = 15)) + theme_classic() + 
  ylim(0,0.6) +
  scale_x_discrete(labels = c('Wild-type', expression(italic('kcnh2a -/-')))) +
  scale_color_manual(labels = c("Wild-type", expression(italic('kcnh2a -/-')) ), values = c("coral", "cyan3")) +
  ylab("MDA Protein Adduct Concentration (ug/mL)")
    
plot

summary(wt)
sd(wt)
mean.wt <- mean(wt)
mean.wt

summary(kcnh2a)
sd(kcnh2a)
mean.kcnh2a <- mean(kcnh2a)
mean.kcnh2a

#difference in means
delta <- mean.kcnh2a - mean.wt
delta


boxplot(wt, kcnh2a)

mda.test <- t.test(kcnh2a, wt, var.equal = FALSE)

mda.test



### Test for power for future sample sizes
install.packages("pwr")
install.packages("effectsize")
library(pwr)
library(effectsize)

#pooled standard deviation
sd.pooled <- sd_pooled(wt,kcnh2a)

#unpooled SD
sqrt(var(wt)+var(kcnh2a))

#Running power test for 0.8
#unpooled 0.15
mda.power <-pwr.t.test(d=(mean.wt-mean.kcnh2a)/sd.pooled, n=5:50, sig.level = 0.05, type = "two.sample", alternative = "two.sided")


# permutation test?
p = 20000
diff.orig <- mean(Concentration[Group == "kcn"]) - mean(Concentration[Group == "wt"])
perm.mean <- NULL
for (i in 1:p) {
  shuffle.group <- sample(Group)
  diff.new <- mean(Concentration[shuffle.group == "kcn"]) - mean(Concentration[shuffle.group == "wt"])
  perm.mean <- c(perm.mean,diff.new)
}
perm.mean
diff.orig
hist(perm.mean)
abline(v=diff.orig,col="red")
p.val <- ( sum(perm.mean >= diff.orig) + 1 )/(p + 1)
p.val


