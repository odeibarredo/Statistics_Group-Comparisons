############ STATISTICS ############

#********************#
#____  LIBRARY  _____#

library(ggplot2)
library(reshape2)
library(dplyr)
library(car)
library(pander)
library(moments)
library(dunn.test)
library(psych)
library(gridExtra)
library(ggsignif)
library(ggfortify)


#**********************************#
#____  LOAD AND PREPARE DATA  _____#

mydata <- read.csv("metal_concentrations.csv", header = T, sep = ";")
str(mydata) # make sure the variables are in the correct class


#**************************#
#____  DATA OVERVIEW  _____#

summary(mydata) # quick first look at global data

# For the visualization in ggplot we have to "melt" the data, so all
# the values are arranged in just one column
m_data <- melt(mydata, variable.name = "Metal")


# Now let's add more info to the dataframe. First we'll add the global
# medians, means, and se (standard error). Second, the same variables
# but for each taxa.
# This data will be very useful info to add to the visualizations

m_data <- m_data %>% group_by(Metal) %>% 
  mutate(median_global=median(value)) %>%
  mutate(mean_global=mean(value)) %>% 
  mutate(se_global=sd(value)/sqrt(length(value))) %>%
  group_by(Metal, Taxa) %>% 
  mutate(median_taxa=median(value)) %>%
  mutate(mean_taxa=mean(value)) %>% 
  mutate(se_taxa=sd(value)/sqrt(length(value)))

attach(m_data)

# Here I create a customized palette for each metal
pal_metals <- c(As="darkorange1", Se="darkcyan", 
                Cd ="chartreuse4" , Hg="maroon4")

ggplot(unique(m_data[c(1,2,8,9)]), aes(x =Metal, y=mean_taxa, fill=Metal))+
  geom_bar(stat="identity", show.legend = F) +
  facet_wrap(~Taxa) +
  scale_fill_manual(values=pal_metals) +
  geom_errorbar(aes(ymin=mean_taxa-se_taxa, ymax=mean_taxa+se_taxa),
                width=0.3,
                position=position_dodge(0.9), size = 0.8) +
  geom_hline(yintercept = unique(mean_global)[1], color="darkorange1",
             size=1, linetype = 2)  +
  geom_hline(yintercept = unique(mean_global)[2], color="darkcyan",
             size=1, linetype = 2) +
  geom_hline(yintercept = unique(mean_global)[3], color="chartreuse4",
             size=1, linetype = 2) +
  geom_hline(yintercept = unique(mean_global)[4], color="maroon4",
             size=1, linetype = 2) + 
  labs(title= "Metal concentration means and standar errors for each taxa",
       subtitle = "*dotted lines show the global metal concentration",
       x=NULL, y = "μg/g dw") +
  theme(plot.title = element_text(size=16, face = "bold"),
        plot.subtitle=element_text(size=13, face="italic"),
        axis.text = element_text(size=12, face = "bold", color = "black"),
        strip.text.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))
  

# At first look we could think that there may be signifficant differences
# in the next cases:
#   As -> the three taxas
#   Se -> Hepta vs Hydro and Rhya
#   Cd -> Hepta vs Hydro and Rhya
#   Hg -> none

# In the next step we will prove if our hypothesis is true or not

#*********************************#
#____  STATISTICAL ANALYSIS  _____#

# STEP A) NORMAL DISTRIBUTION?
attach(mydata)

# A.2) SHAPIRO-WILK

shapiro_fun <- function(x){
  pvalue <- vector()
  result <- vector()
  for(i in 2:ncol(x)){
    sw <- tapply(unlist(x[i]), x$Taxa, shapiro.test)
    result <- append(result, sw)
  }
  for(i in 1:length(result)) {
    pvalue <- append(result[[i]][[2]], pvalue)
  }
  return(round(rev(pvalue),3))
}

sw <- shapiro_fun(mydata)

# A.2) ASIMETRY

skewness_fun <- function(x){
  result <- vector()
  for(i in 2:ncol(x)){
    ske <- tapply(unlist(x[i]), x$Taxa, skewness)
    result <- append(result, unname(ske))
  }
  return(round(result,2))
}

ske <- skewness_fun(mydata)
ord_ske <- ske[c(1,4,7,10,2,5,8,11,3,6,9,12)] # for this case we need 
# to sort the values from the result acord accordingly

# We can save this results into a dataframe
dt <- data.frame("Taxa"= rep(levels(mydata$Taxa), each=4),
                 "Metal" = rep(levels(m_data$Metal),3),
                 "Shapiro-Wilk" = sw,
                 "Skewness" = ord_ske)

# A.3) LEVENE

levene_fun <- function(x){
  value <- vector()
  result <- vector()
  for(i in 2:ncol(x)){
    lev <- leveneTest(unlist(x[i]), x$Taxa, center = mean)
    result <- append(result, lev[[3]][1])
  }
  return(round(result,3))
}

lev <- levene_fun(mydata)

dt_lev <- data.frame("Metal" = levels(m_data$Metal),
                     "Levene Test" =lev)

# RESULTS:
# Normal distribution:
set.caption("*p<0.01; **-2<p>2")
pandoc.table(dt, emphasize.strong.cells = which(dt > 2 | dt < -2,
                                                 arr.ind = TRUE),
             emphasize.cells=(which(dt < 0.01,
                                    arr.ind = TRUE)))

# Homogeneity of variance:
set.caption("*p<0.01")
pandoc.table(dt_lev, emphasize.cells=(which(dt_lev < 0.01,
                                    arr.ind = TRUE)))


#################
# INTERPRETATION:

# To better understand the results of Shapiro-Wilk we can perform a 
# Q-Q plot, which is commonly used to detect deviations from the normal
# distribution.

# Let's take two SW values as examples, one <0.01 and the other >0.01
# For example: Cd in both Hepatgeniidae (0.456) and Rhyacophilidae (0.000)

par(mfrow=c(1,2))

qqnorm(mydata[Taxa=="Heptageniidae", "Cd"], 
                main = paste("Normal distribution", 
                    "[Cd] in Heptageniidae", 
                    sep="\n"),
                pch=16, cex=1.5, col="firebrick3")
qqline(mydata[Taxa=="Heptageniidae", "Cd"],  lwd=2)
text(x=-1, y=2.7, "Shapiro-Wilk = 0.456",
     cex = 1.3)

qqnorm(mydata[Taxa=="Rhyacophilidae", "Cd"],
       main = paste("Non-normal distribution", 
                    "[Cd] in Rhyacophilidae", 
                     sep="\n"),
       pch=16, cex=1.5, col="firebrick3")
qqline(mydata[Taxa=="Rhyacophilidae", "Cd"], lwd=2)
text(x=-1, y=0.9, "Shapiro-Wilk = 0.000",
     cex = 1.3)

# Cd in Heptageniidae fits the QQplot, Cd in Rhyacophilidae doesn't

# Let's now visualize a histogram to understand the skewness.
# In the case of Cd Hepatgeniidae the value is 0.30 and in Hydropsychidae
# is 2.60 (out of range)

hist(mydata[Taxa=="Heptageniidae", "Cd"], breaks = 12,
     main="[Cd] in Heptageniidae", xlab = NULL)
lines(density(mydata[Taxa=="Heptageniidae", "Cd"]), lwd=2, col="darkblue")

hist(mydata[Taxa=="Hydropsychidae", "Cd"], breaks = 12,
     main="[Cd] in Hydropsychidae", xlab = NULL)
lines(density(mydata[Taxa=="Hydropsychidae", "Cd"]), lwd=2, col="red3")

# In the case of Hydropsychidae the density line is skewed to the left


# Finally, to better understand the Levene's test we can build some 
# boxplots and pay attention to the whiskers.
pal_taxa <- c(Heptageniidae="firebrick3",
              Hydropsychidae="springgreen3", 
              Rhyacophilidae ="slateblue")

ggplot(m_data, aes(x=m_data$Taxa, y=m_data$value, fill=m_data$Taxa)) +
  geom_boxplot(show.legend = F) +
  labs(title= "Metal concentrations in each taxa") +
  facet_wrap(~ m_data$Metal, scales = "free") +
  scale_fill_manual(values=pal_taxa) +
  theme(plot.title = element_text(hjust = 0.5, size = 18,
                                  vjust = 5, face = "bold"), 
        plot.margin = unit(c(0.5,1,1,1), "cm"),
        axis.title = element_blank(),
        axis.text = element_text(size = 12, face = "bold"),
        strip.text.x = element_text(size = 16, face = "bold"))

# Heptageniidae shows a lot of variance in all metals except in Hg, 
# that's why Levene fails for the first three metals

# * NOTE:
# One important thing to consider is the amount of samples that we are
# working with, less than 30 for each metal and taxa. The smallest the
# population the harder the probability of having normal ditribution.
# Outliers can have a big impact.

# STEP B) DATA TRANSFORMATION AND SECOND ROUND CHECKING DISTRIBUTION

# None of the metals pass the three test we stablished on all taxa. 
# Transforming the data we may achieve normal distribution, so that's
# the next step. Take note though, that in Cd in Hydropsychidae showed
# skewness, a parameter that does not change even if the data is 
# transformed,so we already know that the data referring to Cd must be 
# treated with non-parametric test.

# Data transformation
log_data <- round(data.frame(apply(mydata[-1], 2, log10)),3)
log_data <- cbind("Taxa"=mydata$Taxa, log_data)

# Run Shapiro-Wilk and Levene's Test
sw_log <- shapiro_fun(log_data)

dt_sw_log <- data.frame("Taxa"= rep(levels(mydata$Taxa), each=4),
                     "Metal" = rep(levels(m_data$Metal),3),
                     "Shapiro-Wilk" = sw_log)

lev_log <- levene_fun(log_data)

dt_lev_log <- data.frame("Metal" = levels(m_data$Metal),
                         "Levene Test" =lev_log)

# RESULTS:
set.caption("*p<0.01")
pandoc.table(dt_sw_log, emphasize.cells=(which(dt_sw_log < 0.01,
                                    arr.ind = TRUE)))

set.caption("*p<0.01")
pandoc.table(dt_lev_log, emphasize.cells=(which(dt_lev_log < 0.01,
                                            arr.ind = TRUE)))


# So now, all cases pass the Levene's Test, and only two cases do not
# pass the SW -> As and Cd.
# This means that we can perform parametric test with the transformed
# data for Se and Hg, and non-parametric test for As and Cd with 
# original data.


# STEP C) SIGNIFFICANT DIFFERENCES?

## C.1) PARAMETRIC TESTS
attach(log_data)

### C.1.1) ONE WAY ANOVA (Global differences)

anova_fun <- function(x){
  result <- vector()
  for(i in 2:ncol(x)){
    lin_mod <- lm(unlist(x[i]) ~ x$Taxa)
    ano <- anova(lin_mod)
    result <- append(result, ano[[5]][1])
  }
  return(round(result,3))
}

ano <- anova_fun(log_data[c("Taxa", "Se", "Hg")])

dt_ano <- data.frame("Metal" = c("Se", "Hg"),
                     "Anova" = ano)

set.caption("*p<0.05")
pandoc.table(dt_ano, emphasize.cells=(which(dt_ano < 0.05,
                                            arr.ind = TRUE)))

### C.1.2) BONFERRONI (Post-Hoc, paired comparison)

bonferroni_fun <- function(x){
  comparisons <- vector()
  values <- vector()
  result <- list()
  for(i in 2:ncol(x)){
    bon <- pairwise.t.test(unlist(x[i]), x$Taxa, p.adj="bonferroni")
    values <- append(values, bon[[3]][c(1,2,4)])
    comparisons <- list(paste(colnames(bon$p.value)[1],
                         row.names(bon$p.value)[1], sep = " - "),
                        paste(colnames(bon$p.value)[1],
                              row.names(bon$p.value)[2], sep = " - "),
                        paste(colnames(bon$p.value)[2],
                              row.names(bon$p.value)[2], sep = " - "))
  }
  result <- list(comparisons, round(values,3))
  return(result)
} 

bon <- bonferroni_fun(log_data[c("Taxa", "Se", "Hg")])
# I also included Hg into the test even though the Anova showed that
# there wasn't differences just so you can see how all pvalues for
# this variable are indeed >0.05 (not signifficant)

dt_bon <- data.frame("Comparisons"=unlist(bon[[1]]),
                     "Se"=bon[[2]][1:3],
                      "Hg"=bon[[2]][4:6])

set.caption("*p<0.05")
pandoc.table(dt_bon, emphasize.cells=(which(dt_bon < 0.05,
                                             arr.ind = TRUE)))

## C.2) NON-PARAMETRIC TESTS
attach(mydata)

### C.2.1) KRUSKAL-WALLIS (Global differences)

kruskal_fun <- function(x){
  result <- vector()
  for(i in 2:ncol(x)){
    kru <- kruskal.test(unlist(x[i]), x$Taxa, center = mean)
    result <- append(result, kru$p.value)
  }
  return(round(result,3))
}

kru <- kruskal_fun(mydata[c("Taxa", "As", "Cd")])

dt_kru <- data.frame("Metal" = c("As", "Cd"),
                     "Kruskal-Wallis" = kru)

set.caption("*p<0.05")
pandoc.table(dt_kru, emphasize.cells=(which(dt_kru < 0.05,
                                            arr.ind = TRUE)))

### C.2.2) DUNN (Post-Hoc, paired comparison)

dunn_fun <- function(x){
  comparisons <- vector()
  values <- vector()
  result <- list()
  for(i in 2:ncol(x)){
    dun <- dunn.test(unlist(x[i]), x$Taxa)
    values <- append(values, dun$P)
    comparisons <- dun$comparisons
  }
  result <- list(comparisons, round(values,3))
  return(result)
} 

dun <- dunn_fun(mydata[c("Taxa", "As", "Cd")])

dt_dunn <- data.frame("Comparisons"=dun[[1]], "As"=dun[[2]][1:3],
                      "Cd"=dun[[2]][4:6])

set.caption("*p<0.05")
pandoc.table(dt_dunn, emphasize.cells=(which(dt_dunn < 0.05,
                                            arr.ind = TRUE)))

# SUMMARY:
# Now that we know which cases have signifficant differences we can 
# visualize the boxplots from before with annotations of this cases
# thanks to the geom_signif() function in ggplot2

As_plot <- ggplot(mydata, aes(x=Taxa, y=As, fill=Taxa)) +
  geom_boxplot(show.legend = F) +
  scale_fill_manual(values=pal_taxa) +
  labs(title="As") +
  theme(plot.title = element_text(hjust = 0.5, size = 20,
                                  vjust = 5, face = "bold"),
        plot.margin = unit(c(0.5,1,1,1), "cm"),
        axis.text = element_text(size = 12, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  geom_signif(comparisons = list(c("Heptageniidae","Hydropsychidae"), 
                                 c("Heptageniidae","Rhyacophilidae"),
                                 c("Hydropsychidae","Rhyacophilidae")), 
              map_signif_level=TRUE, step_increase=0.2,
              annotations = "*", size = 1,
              textsize=10, col="gray25", vjust=0.6)

Se_plot <- ggplot(mydata, aes(x=Taxa, y=Se, fill=Taxa)) +
  geom_boxplot(show.legend = F) +
  scale_fill_manual(values=pal_taxa) +
  labs(title="Se") +
  theme(plot.title = element_text(hjust = 0.5, size = 20,
                                  vjust = 5, face = "bold"),
        plot.margin = unit(c(0.5,1,1,1), "cm"),
        axis.text = element_text(size = 12, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  geom_signif(comparisons = list(c("Heptageniidae","Hydropsychidae"), 
                                 c("Heptageniidae","Rhyacophilidae"),
                                 c("Hydropsychidae","Rhyacophilidae")), 
              map_signif_level=TRUE, step_increase=0.2,
              annotations = "*", size = 1,
              textsize=10, col="gray25", vjust=0.6)

Cd_plot <- ggplot(mydata, aes(x=Taxa, y=Cd, fill=Taxa)) +
  geom_boxplot(show.legend = F) +
  scale_fill_manual(values=pal_taxa) +
  labs(title="Cd") +
  theme(plot.title = element_text(hjust = 0.5, size = 20,
                                  vjust = 5, face = "bold"),
        plot.margin = unit(c(0.5,1,1,1), "cm"),
        axis.text = element_text(size = 12, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  geom_signif(comparisons = list(c("Heptageniidae","Hydropsychidae"), 
                                 c("Heptageniidae","Rhyacophilidae")), 
              map_signif_level=T, step_increase=0.2, 
              annotations = "*", size = 1,
              textsize=10, col="gray25", vjust=0.6)

Hg_plot <- ggplot(mydata, aes(x=Taxa, y=Hg, fill=Taxa)) +
  geom_boxplot(show.legend = F) +
  scale_fill_manual(values=pal_taxa) +
  labs(title="Hg") +
  theme(plot.title = element_text(hjust = 0.5, size = 20,
                                  vjust = 1, face = "bold"),
        plot.margin = unit(c(0.5,1,1,1), "cm"),
        axis.text = element_text(size = 12, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

grid.arrange(As, Se, Cd, Hg, ncol=2)


######################################
# As a final step we can check the correlation between the metals and the
# taxa with a PCA

autoplot(prcomp(mydata[,2:5]), data = mydata, col = "Taxa", size=3,
         loadings=T, loadings.colour = "black", loadings.label=T, 
         loadings.label.size=7, loadings.label.colour = "black",
         frame = T, frame.type = "norm") +
  scale_color_manual(values=pal_taxa) +
  theme(plot.margin = unit(c(0.5,4,0.5,4), "cm"),
        axis.text = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 12, face = "bold"),
        legend.title = element_blank(),
        legend.position = c(0, 1),
        legend.justification = c(-5, 1),
        legend.background = element_rect(fill="transparent"))

# So most influencial metals in taxa differentiation are As and Se;
# Cd has a bit of differentiation power towards Heptageniidae;
# Hg shows same accumulation levels in three taxa

#***********************#
#____  THE END!!!  _____#
