
library(dplyr)
library(ggplot2)
library(GGally)

library(reshape2)
library(RColorBrewer)
library(dplyr)
library(psych)

facebook = read.delim("dataset_Facebook.csv",  header=TRUE,  sep=";")
# rename the variable for easy handling

write.csv(facebook, "facebook.csv")
facebook = na.omit(facebook)
names(facebook)[1] <- "Page.Total.Likes"
names(facebook)[2] <- "Type"
names(facebook)[3] <- "Category"
names(facebook)[4] <- "Post.Month"
names(facebook)[5] <- "Post.Weekday"
names(facebook)[6] <- "Post.Hour"
names(facebook)[7] <- "Paid"
names(facebook)[8] <- "Total.Reach"
names(facebook)[9] <- "Total.Impressions"
names(facebook)[10] <- "Engaged.Users"
names(facebook)[11] <- "Consumers"
names(facebook)[12] <- "Consumptions"
names(facebook)[13] <- "Impressions.for.Users.with.Likes"
names(facebook)[14] <- "Reach.by.Users.with.Likes"
names(facebook)[15] <- "Users.with.Likes.and.Engagement"
names(facebook)[16] <- "Comment"
names(facebook)[17] <- "Like"
names(facebook)[18] <- "Share"
names(facebook)[19] <- "Total.Interactions"
describe(facebook)


any(is.na(facebook)) # check for missing values
# get percentage of missing values for each variable
fields <- names(facebook) # get variable names
row_count <- nrow(facebook) # get number of observations

for(field in fields) {
  column <- facebook[[field]]
  has_na <- is.na(column)
  missing_data_result <- any(has_na)
  
  if(missing_data_result) {
    percent_na <- (length(column[has_na])/row_count)*100
    result <- sprintf("%s: %s%%", field, percent_na)
    print(result)
  }
}
## Cleaning Data  
#As part of the data cleaning we evaluated two methods for handling the missing data values: 1) replacing missing values with column mean, or 2) replacing missing data with 0's.  

#The main reason for imputing values is to reduce bias due to missing values in order to maintain the sample size. This results in a potentially higher efficiency than deleting observations with missing values. This allows us to utilize the collected data in an incomplete dataset.  

#By replacing the missing values with the column mean average we run the risk of multicollinearity, which exists whenever an independent variable is highly correlated with one or more of the other independent variables in a multiple regression equation. Multicollinearity is a problem because it undermines the statistical significance of an independent variable.  

#Justification for using the mean substitution is that the mean is a reasonable estimate for a randomly selected observation from a normal distribution. However, with missing values that are not strictly random, the mean substitution method may lead to inconsistent bias. Furthermore, this approach adds no new information but only increases the sample size and leads to an underestimate of the errors within the dataset.  

#By replacing the missing values with 0's, we will will tend to underestimate the standard errors and overestimate the level of precision. Thus, a single imputation of "O" gives us more apparent power than the dataset in reality. 

# OPTION 2: set missing values to the average of column
numeric_cols <- sapply(facebook, is.numeric) # determine which columns are numeric
numeric_column_names <- names(facebook[, numeric_cols]) # get the names of those columns
for(name in numeric_column_names) {
  column <- facebook[[name]]
  has_na <- is.na(column)
  missing_data_result <- any(has_na) # determine if the numeric column have missing values
  
  if(missing_data_result) {
    column_average <- mean(column, na.rm=TRUE) # get the average from the remaining observations
    facebook[[name]][is.na(column)] <- column_average # set any missing values in the column to the average
  }
}
# verify there are no remaining missing values
any(is.na(facebook))



facebook$Type <- as.factor(facebook$Type)
facebook$Paid <- as.factor(facebook$Paid)
facebook$Category <- as.factor(facebook$Category)
str(facebook)


#Looking at the variable type
pp1 = ggplot(facebook1, aes(x = Type, fill = Type)) + 
  xlab("Type of Post") +
  ylab("Number of posts") +
  geom_bar()+
  theme(plot.title = element_text(size = 25, face = "bold"),  
        axis.title.x = element_text(size = 20), 
        axis.title.y = element_text(size = 20), 
        axis.text.x = element_text(size = 20), 
        axis.text.y = element_text(size = 20),
        legend.key.width = unit(3, "cm"), 
        legend.key.height = unit(2, "cm"),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 25))

pp2 = ggplot(data = facebook1, aes(x = Type, fill = Type)) +
  xlab("Not Paid (0) / Paid (1)") +
  ylab("Number of posts") +
  geom_bar() + 
  facet_wrap(~Paid)+  
  theme(plot.title = element_text(size = 25, face = "bold"),  
        axis.title.x = element_text(size = 20), 
        axis.title.y = element_text(size = 20), 
        axis.text.x = element_text(size = 20), 
        axis.text.y = element_text(size = 20),
        legend.key.width = unit(3, "cm"), 
        legend.key.height = unit(2, "cm"),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 25))

pp3 = ggplot(data = facebook1, aes(x = Post.Weekday, fill = Paid)) + 
  xlab("Days") +
  ylab("Number of posts") +
  geom_histogram()+
  theme(plot.title = element_text(size = 25, face = "bold"),  
        axis.title.x = element_text(size = 20), 
        axis.title.y = element_text(size = 20), 
        axis.text.x = element_text(size = 20), 
        axis.text.y = element_text(size = 20),
        legend.key.width = unit(3, "cm"), 
        legend.key.height = unit(2, "cm"),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 25))
pp4 = ggplot(data = facebook1, aes(x = Post.Hour, fill = Paid )) +
  xlab("Hours of the Day") +
  ylab("Count of Posts") +
  geom_bar() + 
  facet_wrap(~Type)+
  theme(plot.title = element_text(size = 25, face = "bold"),  
        axis.title.x = element_text(size = 30), 
        axis.title.y = element_text(size = 30), 
        axis.text.x = element_text(size = 30), 
        axis.text.y = element_text(size = 30),
        legend.key.width = unit(6, "cm"), 
        legend.key.height = unit(5, "cm"),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30))



plots =list( pp1, pp2)
plots2 =list( pp3, pp4)

ggsave("MULT.png", plot_grid(plotlist = plots, ncol = 2), width = 32, height = 20)
ggsave("MULT2.png", plot_grid(plotlist = plots2, ncol = 1), width = 32, height = 20)

#Checking for comment and interactions correlation
ggplot(data = facebook, aes(x = Like, y = Total.Interactions)) + 
  geom_point()

#We noticed that there’s an outlier and we’d rather drop that in order to not skew our results. We will look at the variable and its IQR.

#boxplot to check for outliers
boxplot(facebook$Total.Interactions)

quantile(facebook$Total.Interactions)


facebook1 <- subset(facebook,facebook$Total.Interactions<229)
dim(facebook1)
boxplot(facebook1$Total.Interactions)






#Checking for interactions and likes correlation

ggplot(data = facebook1, aes(x = Like, y = Total.Interactions, color=Paid)) + 
  geom_point()



ggplot(data = facebook1, aes(x = share, y = Total.Interactions, color=Paid)) + 
  geom_point()


# Create the heatmap


# Select the columns to include in the correlation matrix
#df_corr <- glass[, 2:10]

# Calculate the correlation matrix
fb_numeric <- select_if(facebook1, is.numeric)

corr_matrixF <- cor(fb_numeric)

corr_matrixF <- as.matrix(corr_matrixF)


#reduce the size of correlation matrix
corrF<- melt(corr_matrixF)

# Set the color palette
palette <- colorRampPalette(rev(brewer.pal(9, "RdYlBu")))

# Create the heatmap
ggplot(corrF, aes(x = Var1, y = Var2, fill = value)) + 
  geom_tile(color = "white") +
  scale_fill_gradientn(colors = palette(100), limits = c(-1, 1), breaks = seq(-1, 1, 0.4)) +   geom_text(aes(label = sprintf("%0.2f", value)), size = 5) +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Heatmap",
       x = "Variables",
       y = "Variables") + coord_fixed()

ggsave("heatmap.jpeg", width = 16, height = 16)


We can clearly see that there is a positive correlation between likes and interactions. Now let’s see what the correlation looks like between share and interactions.



plots <- list(
  ggplot(facebook1, aes(x = Page.Total.Likes, y = Like, color =Paid )) +
    geom_point() +
    geom_smooth(method = "lm")+
    labs(title = "Scatterplot of page total likes vs likes per post colored by paid status", x = "Page total likes", y = "likes") +
    theme(plot.title = element_text(size = 25, face = "bold"),  
          axis.title.x = element_text(size = 20), 
          axis.title.y = element_text(size = 20), 
          axis.text.x = element_text(size = 20), 
          axis.text.y = element_text(size = 20),
          legend.key.width = unit(3, "cm"), 
          legend.key.height = unit(2, "cm"),
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 25)),
  
  ggplot(data = facebook1, aes(x = Like, y = Total.Interactions, color=Paid)) + 
    geom_smooth(method = "lm")+
    labs(title = "Scatterplot of likes vs total interactions colored by paid status", x = " Likes", y = "Total interactions ") +
    
    theme(plot.title = element_text(size = 25, face = "bold"),  
          axis.title.x = element_text(size = 20), 
          axis.title.y = element_text(size = 20), 
          axis.text.x = element_text(size = 20), 
          axis.text.y = element_text(size = 20),
          legend.key.width = unit(3, "cm"), 
          legend.key.height = unit(2, "cm"),
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 25))+
    geom_point(),
  
  
  
  ggplot(data = facebook1, aes(x = Share, y = Total.Interactions, color=Paid)) + 
    geom_smooth(method = "lm")+
    labs(title = "Scatterplot of Shares vs total interactions colored by paid status", x = "Shares", y = "Total interactions") +
    theme(plot.title = element_text(size = 25, face = "bold"),  
          axis.title.x = element_text(size = 20), 
          axis.title.y = element_text(size = 20), 
          axis.text.x = element_text(size = 20), 
          axis.text.y = element_text(size = 20),
          legend.key.width = unit(3, "cm"), 
          legend.key.height = unit(2, "cm"),
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 25))+ 
    geom_point(),
  
  
  
  ggplot(data = facebook1, aes(x = Total.Reach, y = Page.Total.Likes, color=Paid)) + 
    geom_smooth(method = "lm")+
    labs(title = "Scatterplot of total reach vs page total likes colored by paid status", x = "Total reach", y = "Page total likes") +
    theme(plot.title = element_text(size = 25, face = "bold"),  
          axis.title.x = element_text(size = 20), 
          axis.title.y = element_text(size = 20), 
          axis.text.x = element_text(size = 20), 
          axis.text.y = element_text(size = 20),
          legend.key.width = unit(3, "cm"), 
          legend.key.height = unit(2, "cm"),
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 25))+
    geom_point()
  
  
library(cowplot)
ggsave("my_plots.png", plot_grid(plotlist = plots, ncol = 2), width = 30, height = 20)
#plot_grid(plotlist = plots, ncol = 2)
```




This inital model is looking at all 19 variables within the dataset, with Page Total Likes as the chosen response variable. We plan to look at each variable and determine which ones are significant to build a final, reliable model.

model <- lm(Page.Total.Likes
            ~ Post.Month+
              Post.Weekday+
              Post.Hour+
              as.factor(Type)+
              as.factor(Paid)+
              as.factor(Category)+
              Total.Reach+
              Total.Impressions+
              Engaged.Users+
              Consumers+
              Consumptions+
              Impressions.for.Users.with.Likes+
              Reach.by.Users.with.Likes+
              Users.with.Likes.and.Engagement+
              Comment+
              Like+
              Share+
              Total.Interactions,
            data=facebook1)
par(mfrow = c(2, 2))
summary(model)
```
Residuals measure the variability in the response not explained by the model. There are several ways to measure residual. For most of our analysis we used the definition of residual as the observed data minus the fitted data (plot1). But here we also show two results of scaling residuals (which are helpful in identifying outliers and extreme values). Standardized Residuals (plot2) show the residual standardized by it's standard deviation. PRESS residuals (plot3) shows how well the model may perform in predicting new data.  
```{r, echo=FALSE}
leverage=lm.influence(model)$hat
#MS Residual
MSRes=summary(model)$sigma^2
SSRes=sum((model$residuals-mean(model$residuals))^2)
#standardized residuals
standardized_res=model$residuals/sqrt(MSRes)
#PRESS residual
PRESS_res=model$residuals/(1 - leverage)
par(mfrow=c(1,3))
plot(model$fitted.values,model$residuals,pch=20,ylab="Residual",xlab="Fitted Value", main="Residual")
abline(h=0,col="grey")
plot(model$fitted.values,standardized_res,pch=20,ylab="Standardized Residual",xlab="Fitted Value", main="Standardized Residual")
abline(h=0,col="grey")
plot(model$fitted.values,PRESS_res,pch=20,ylab="PRESS Residual",xlab="Fitted Value", main="PRESS Residual")
abline(h=0,col="grey")
```


## Checking Linearity and Equal variance
We reevaluate all variables in the dataset to see if they are viable options to remain covariates in our final model. To do this we plot each covariate against the residuals in order to determine if and how they need to be transformed. Each are expected to be linear and equal variance. After reviewing these plots we can see that the Post.Month needs to be transformed because of its nonlinearity, it violates the linearity of errors assuption.  
  
```{r, echo=FALSE}
numeric_cols <- sapply(facebook1, is.numeric) # determine which columns are numeric

numeric_column_names <- names(facebook1[, numeric_cols]) # get the names of those columns

numeric_dataset <- facebook1[,sapply(facebook1, is.numeric)]
#par(mfrow=c(4,5))
par(mfrow=c(1,4))

for(name in numeric_column_names) {
  plot(numeric_dataset[[name]],model$residuals, ylab="Residuals", xlab=name)
  abline(h=0,col="grey")
}
```
## Check Normality Assumption
To check this assumption we plot each covariate against the residual. We use the QQ plot to check the normality assumption. On the plot we plot the residuals for the model and show the QQ line to assist in visualizing the normal distribution. The plot is shown below. From this we can see that the residual plot appears to be a light tailed distribution.  The histogram below confirms this.

```{r, echo=FALSE}
{
  par(mfrow=c(1,2))
  # QQ Plot
  qqnorm(model$residuals) #plots residuals
  qqline(model$residuals) #draws line
  
  # Histogram of Residuals
  hist(model$residuals, xlab="Residuals", main="Histogram of Residuals", breaks=20)
}
```
# 4. Transformation   
Initially only Posts.Month needed transforming. We tried a number of different transformations but found that the most successful transformation was a higher order ploynomial transformation. Part of the transformations with higher order involves centering the variable's data in attempt to remove multicollinearity. We also review the variance inflation factor of the variable. Sometimes, centering the regressor variables can minimize or eliminate at least some of the ill-conditioning that may be present in a polynomial model.  

We can visualize the transformation by creating a model with the original Post.Month values and a second with the transformed Post.Month values and plotting both against residuals.

facebook1$Post.Month.Centered = facebook1$Post.Month - mean(facebook1$Post.Month) 
# transforming
lm(facebook1$Page.Total.Likes ~ facebook1$Post.Month.Centered+I(facebook1$Post.Month.Centered^2)+I(facebook1$Post.Month.Centered^3)+I(facebook1$Post.Month.Centered^4)) 

model2a <- lm(facebook1$Page.Total.Likes ~ facebook1$Post.Month)
model2b <- lm(facebook1$Page.Total.Likes ~ facebook1$Post.Month.Centered+I(facebook1$Post.Month.Centered^2)+I(facebook1$Post.Month.Centered^3)+I(facebook1$Post.Month.Centered^4)) # rebuild model with new x
{
  par(mfrow=c(1,2))
  plot(model2a$fitted.values, model2a$residuals, main="Before Transformation", xlab="Fitted", ylab="Residual")
  abline(h=0,col="grey",lwd=3)
  plot(model2b$fitted.values, model2b$residuals, main="After Transformation", xlab="Fitted", ylab="Residual")
  abline(h=0,col="grey",lwd=3)
}

# 5. Variable Selection
Variance inflation factors (VIF) are very useful in determining if multicollinearity is present. Multicollinearity occurs when independent variables are strongly correlated and can give very wrong estimates for the betas (intercepts and slopes). The square root of VIF indicates how much larger the standard error is compared with what it would be if that variable were uncorrelated.

After reviewing the VIF results, we see there are 8 variables with a VIF score higher than 10. We proceeded to do backwards elimination of variables, eliminating the highest VIF score each time. After this process we eliminated Post.Month, Total.Impressions, Engaged.Users and Total.Interactions.

## Initial VIF

model3 <- lm(facebook1$Page.Total.Likes ~ 
               facebook1$Post.Month.Centered+ facebook1$Post.Weekday+
               facebook1$Post.Hour+
               facebook1$Paid+
               facebook1$Total.Reach+
               facebook1$Total.Impressions+
               facebook1$Engaged.Users+
               facebook1$Consumers+
               facebook1$Consumptions+
               facebook1$Impressions.for.Users.with.Likes+
               facebook1$Reach.by.Users.with.Likes+
               facebook1$Users.with.Likes.and.Engagement+
               facebook1$Comment+
               facebook1$Like+
               facebook1$Share
)
summary(model3)


model4 <- lm(facebook1$Page.Total.Likes ~ 
               #dataset$Post.Month.Centered+I(dataset$Post.Month.Centered^2)+I(dataset$Post.Month.Centered^3)+I(dataset$Post.Month.Centered^4)+           dataset$Post.Weekday+
               facebook1$Post.Hour+
               facebook1$Paid+
               facebook1$Total.Reach+
               #$Total.Impressions+
               #dataset$Engaged.Users+
               facebook1$Consumers+
               facebook1$Consumptions+
               facebook1$Impressions.for.Users.with.Likes+
               facebook1$Reach.by.Users.with.Likes+
               facebook1$Users.with.Likes.and.Engagement+
               facebook1$Comment+
               facebook1$Like+
               facebook1$Share#+
             #dataset$Total.Interactions
)
summary(model4)


anova_model4 <- lm(Page.Total.Likes ~
                     Post.Hour+
                     Paid+
                     Total.Reach+
                     Consumers+
                     Consumptions+
                     Impressions.for.Users.with.Likes+
                     Reach.by.Users.with.Likes+
                     Users.with.Likes.and.Engagement+
                     Comment+
                     Like+
                     Share,
                   data=facebook1)
summary(anova_model4)

In our final model our response is Page.Total.Likes and our covariates are Post.Hour, Consumers, Reach.by.Users.with.Likes, Users.with.Likes.and.Engagement, Like and Share.  