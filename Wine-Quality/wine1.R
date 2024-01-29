library(ggplot2)
library(scales)
library(tidyr)
library(dplyr)
library(psych)
library(gridExtra)
library(GGally)
library(corrplot)
library(ggcorrplot)
library(cowplot)

wine = read.delim("winequality-red.csv",  header=TRUE,  sep=";")
wine
summary(wine)
str(wine)
a=table(wine$quality)
b=table(wine$quality)/nrow(wine)*100
c=rbind(a,b)
c=data.frame(c)
rownames(c)=c('Number of Wines','%')
colnames(c)=c(3,4,5,6,7,8)
c=round(c,digits = 2)
c

missing_values <- colSums(is.na(wine))
#a variable that calculates the total number of values in each column
total_values <- sapply(wine, function(x) length(x))
#create a variable that calculates the proportion of missing values in each column
prop_missing <- missing_values/total_values
prop_missing
#Density plots of chemicals
g1 = ggplot(wine,aes(alcohol)) + 
  geom_density(fill="#1579D2",color="#1579D2")+
  labs(
    title = "Density Plots of all the chemical attributes",
    x="Fixed acidity",
    y="Density"
  )+ 
  theme(plot.title = element_text(size = 25, face = "bold"),  
        axis.title.x = element_text( face = "bold"), 
        axis.title.y = element_text( face = "bold"), 
        axis.text.x = element_text(size = 15), 
        axis.text.y = element_text(size = 15),
  )

g2=ggplot(wine,aes(volatile.acidity)) + 
  geom_density(fill="#1579D2",color="#1579D2")+
  labs(
    x="Volatile acidity",
    y="Density"
  )+ 
  theme(plot.title = element_text(size = 25, face = "bold"),  
        axis.title.x = element_text( face = "bold"), 
        axis.title.y = element_text( face = "bold"), 
        axis.text.x = element_text(size = 15), 
        axis.text.y = element_text(size = 15),
  )

g3=ggplot(wine,aes(citric.acid)) + 
  geom_density(fill="#1579D2",color="#1579D2")+
  labs(
    x="Citric Acid",
    y="Density"
  )+ 
  theme(plot.title = element_text(size = 25, face = "bold"),  
        axis.title.x = element_text( face = "bold"), 
        axis.title.y = element_text( face = "bold"), 
        axis.text.x = element_text(size = 15), 
        axis.text.y = element_text(size = 15),
  )

g4=ggplot(wine,aes(residual.sugar)) + 
  geom_density(fill="#1579D2",color="#1579D2")+
  labs(
    x="Residual sugar",
    y="Density"
  )+ 
  theme(plot.title = element_text(size = 25, face = "bold"),  
        axis.title.x = element_text( face = "bold"), 
        axis.title.y = element_text( face = "bold"), 
        axis.text.x = element_text(size = 15), 
        axis.text.y = element_text(size = 15),
  )

g5=ggplot(wine,aes(chlorides)) + 
  geom_density(fill="#1579D2",color="#1579D2")+
  labs(
    x="Chlorides",
    y="Density"
  )+ 
  theme(plot.title = element_text(size = 25, face = "bold"),  
        axis.title.x = element_text( face = "bold"), 
        axis.title.y = element_text( face = "bold"), 
        axis.text.x = element_text(size = 15), 
        axis.text.y = element_text(size = 15),
  )

g6=ggplot(wine,aes(free.sulfur.dioxide)) + 
  geom_density(fill="#1579D2",color="#1579D2")+
  labs(
    x="Free sulfur dioxide",
    y="Density"
  )+ 
  theme(plot.title = element_text(size = 25, face = "bold"),  
        axis.title.x = element_text( face = "bold"), 
        axis.title.y = element_text( face = "bold"), 
        axis.text.x = element_text(size = 15), 
        axis.text.y = element_text(size = 15),
  )

g7=ggplot(wine,aes(total.sulfur.dioxide)) + 
  geom_density(fill="#1579D2",color="#1579D2")+
  labs(
    x="Total sulfur dioxide",
    y="Density"
  )+ 
  theme(plot.title = element_text(size = 25, face = "bold"),  
        axis.title.x = element_text( face = "bold"), 
        axis.title.y = element_text( face = "bold"), 
        axis.text.x = element_text(size = 15), 
        axis.text.y = element_text(size = 15),
  )

g8=ggplot(wine,aes(density)) + 
  geom_density(fill="#1579D2",color="#1579D2")+
  labs(
    x="Density",
    y="Density"
  )+ 
  theme(plot.title = element_text(size = 25, face = "bold"),  
        axis.title.x = element_text( face = "bold"), 
        axis.title.y = element_text( face = "bold"), 
        axis.text.x = element_text(size = 15), 
        axis.text.y = element_text(size = 15),
  )

g9=ggplot(wine,aes(pH)) + 
  geom_density(fill="#1579D2",color="#1579D2")+
  labs(
    x="PH",
    y="Density"
  )+ 
  theme(plot.title = element_text(size = 25, face = "bold"),  
        axis.title.x = element_text( face = "bold"), 
        axis.title.y = element_text( face = "bold"), 
        axis.text.x = element_text(size = 15), 
        axis.text.y = element_text(size = 15),
  )

g10=ggplot(wine,aes(sulphates)) + 
  geom_density(fill="#1579D2",color="#1579D2")+
  labs(
    x="Sulphates",
    y="Density"
  )+ 
  theme(plot.title = element_text(size = 25, face = "bold"),  
        axis.title.x = element_text( face = "bold"), 
        axis.title.y = element_text( face = "bold"), 
        axis.text.x = element_text(size = 15), 
        axis.text.y = element_text(size = 15),
  )

g11=ggplot(wine,aes(alcohol)) + 
  geom_density(fill="#1579D2",color="#1579D2")+
  labs(
    x="Alcohol",
    y="Density"
  )+ 
  theme(plot.title = element_text(size = 25, face = "bold"),  
        axis.title.x = element_text( face = "bold"), 
        axis.title.y = element_text( face = "bold"), 
        axis.text.x = element_text(size = 15), 
        axis.text.y = element_text(size = 15),
  )


plots =list(g1, g2, g3, g4,g5,g6,g7,g8,g9,g10,g11)

ggsave("DensityPlots.png", plot_grid(plotlist = plots, ncol = 2), width = 20, height = 15)

#barchart of wine quality
c1 =ggplot(wine, aes(quality))+geom_bar(fill="#1579D2",color="#1579D2")+
  labs(
    title="Bar chart of wine quality",
    subtitle="The quality of wine follows normal distribution with the majority of wine with medium quality",
    x="Quality",
    y="Count"
  )
ggsave("bar.png",c1)
#correlation map
correlation=cor(wine)
c2 =ggcorrplot(correlation, hc.order = TRUE, 
               type = "upper", 
               lab = TRUE, 
               lab_size = 9, 
               method="square", 
               title="Correlation map of redwine dataset")+ theme_classic()+ 
  theme(plot.title = element_text(size = 30, face = "bold"),  
        axis.text.x = element_text(angle = 45, hjust = 1, size = 30),
        axis.title.x = element_text(size = 30), 
        axis.title.y = element_text(size = 30), 
        axis.text.y = element_text(size = 30),
        legend.key.width = unit(4, "cm"), 
        legend.key.height = unit(4, "cm"),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30))

ggsave("Corr.png", c2 , width = 20, height = 20)

#Multivariate plots 

wine$quality_levels<-cut(wine$quality, c(2,5,7,9), right=FALSE)
wine$quality_labels <- as.factor(wine$quality_levels)
levels(wine$quality_labels) <- c('Bad','Average','Good')

p1 <- ggplot(aes(x=citric.acid, y=pH), data= subset(wine,!is.na(wine$pH)))+
  labs(
    title="Scatterplot of Citric acid Vs PH factorized by quality of wine(Bad - Average - Good)",
  )+
  geom_point(aes(color=factor(quality) ))+
  geom_smooth(method='lm')+
  facet_wrap(~quality_labels, scales = "free")+
  
  scale_color_brewer(type='qual', guide = guide_legend(title.size = 25, label.size = 25))+
  theme(plot.title = element_text(size = 30, face = "bold"),  
        axis.title.x = element_text(size = 30), 
        axis.title.y = element_text(size = 30), 
        axis.text.x = element_text(size = 30), 
        axis.text.y = element_text(size = 30),
        legend.key.width = unit(2, "cm"), 
        legend.key.height = unit(2, "cm"),
        strip.text.x = element_text(size = 35),
        strip.text.y = element_text(size = 35),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20))

p2 = ggplot(aes(x=citric.acid, y=log10(sulphates)), data= wine)+
  labs(
    title="Scatterplot of Citric acid Vs log transformed sulphates of factorized by quality of wine (Bad - Average - Good)",
  )+
  geom_point(aes(color=factor(quality)))+
  geom_smooth(method='lm')+
  facet_wrap(~quality_labels, scales = "free")+
  scale_color_brewer(type='qual', guide = guide_legend(title.size = 25, label.size = 25))+
  theme(plot.title = element_text(size = 30, face = "bold"),  
        axis.title.x = element_text(size = 30), 
        axis.title.y = element_text(size = 30), 
        axis.text.x = element_text(size = 30), 
        axis.text.y = element_text(size = 30),
        legend.key.width = unit(2, "cm"), 
        legend.key.height = unit(2, "cm"),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        strip.text.x = element_text(size = 35),
        strip.text.y = element_text(size = 35))
p3 = ggplot(aes(x=density, y=fixed.acidity), data= wine)+
  labs(
    title="Scatterplot of density Vs fixed acidity factorized by quality of wine (Bad - Average - Good)",
  )+
  geom_point(aes(color=factor(quality)))+
  geom_smooth(method='lm')+
  facet_wrap(~quality_labels, scales = "free")+
  scale_color_brewer(type='qual', guide = guide_legend(title.size = 25, label.size = 25))+
  theme(plot.title = element_text(size = 30, face = "bold"),  
        axis.title.x = element_text(size = 30), 
        axis.title.y = element_text(size = 30), 
        axis.text.x = element_text(size = 30), 
        axis.text.y = element_text(size = 30),
        legend.key.width = unit(2, "cm"), 
        legend.key.height = unit(2, "cm"),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        strip.text.x = element_text(size = 35),
        strip.text.y = element_text(size = 35))

plots =list(p1, p2, p3)

ggsave("Multi.png", plot_grid(plotlist = plots, ncol = 1, rel_widths = c(1,1), rel_heights = c(2,2)), width = 40, height = 25)
#Scatterplots of chemicals
t1 =ggplot(aes(fixed.acidity, citric.acid), data=wine)+ labs(
  title="Scatterplot of fixed acid Vs  citric acid",
)+
  geom_point()+
  geom_smooth(method='lm')+
  theme(plot.title = element_text(size = 25, face = "bold"),  
        axis.title.x = element_text(size = 30), 
        axis.title.y = element_text(size = 30), 
        axis.text.x = element_text(size = 30), 
        axis.text.y = element_text(size = 30),
        legend.key.width = unit(2, "cm"), 
        legend.key.height = unit(2, "cm"),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20))
t2= ggplot(aes(log10(alcohol), density), data=wine)+ labs(
  title="Scatterplot of density Vs log transformed alcohol",
)+
  geom_point()+
  geom_smooth(method='lm')+
  theme(plot.title = element_text(size = 25, face = "bold"),  
        axis.title.x = element_text(size = 30), 
        axis.title.y = element_text(size = 30), 
        axis.text.x = element_text(size = 30), 
        axis.text.y = element_text(size = 30),
        legend.key.width = unit(2, "cm"), 
        legend.key.height = unit(2, "cm"),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20))
t3= ggplot(aes(pH, density), data=wine)+ labs(
  title="Scatterplot of PH Vs density",
)+
  geom_point()+
  geom_smooth(method='lm')+
  theme(plot.title = element_text(size = 25, face = "bold"),  
        axis.title.x = element_text(size = 30), 
        axis.title.y = element_text(size = 30), 
        axis.text.x = element_text(size = 30), 
        axis.text.y = element_text(size = 30),
        legend.key.width = unit(2, "cm"), 
        legend.key.height = unit(2, "cm"),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20))
t4= ggplot(aes(fixed.acidity, density), data=wine)+ labs(
  title="Scatterplot of fixed acidity Vs density",
)+
  geom_point()+
  geom_smooth(method='lm')+
  theme(plot.title = element_text(size = 25, face = "bold"),  
        axis.title.x = element_text(size = 30), 
        axis.title.y = element_text(size = 30), 
        axis.text.x = element_text(size = 30), 
        axis.text.y = element_text(size = 30),
        legend.key.width = unit(2, "cm"), 
        legend.key.height = unit(2, "cm"),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20))
t5= ggplot(aes( density, pH), data=wine)+ labs(
  title="Scatterplot of density vS PH",
)+
  geom_point()+
  geom_smooth(method='lm')+
  theme(plot.title = element_text(size = 25, face = "bold"),  
        axis.title.x = element_text(size = 30), 
        axis.title.y = element_text(size = 30), 
        axis.text.x = element_text(size = 30), 
        axis.text.y = element_text(size = 30),
        legend.key.width = unit(2, "cm"), 
        legend.key.height = unit(2, "cm"),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20))
plots =list(t1, t2, t3, t4, t5)
ggsave("TRENDS.png", plot_grid(plotlist = plots, ncol = 1, rel_widths = c(1,1), rel_heights = c(2,2)), width = 18, height = 18)