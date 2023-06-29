-In order for an insurance company to make money, it needs to collect more in yearly 
premiums than it spends on medical care to its beneficiaries. As a result, insurers 
invest a great deal of time and money to develop models that accurately forecast 
medical expenses.

- Medical expenses are difficult to estimate because the most costly conditions are 
rare and seemingly random. Still, some conditions are more prevalent for certain 
segments of the population. For instance, lung cancer is more likely among smokers 
than non-smokers, and heart disease may be more likely among the obese.

# The goal of this analysis is to use patient data to estimate the average medical care 
# expenses for such population segments. These estimates could be used to create 
# actuarial tables which set the price of yearly premiums higher or lower depending 
# on the expected treatment costs.

For this analysis, we will use a simulated dataset containing medical expenses 
for patients in the United States. These data were created using 
demographic statistics from the U.S. Census Bureau, and thus approximately 
reflect real-world conditions.

############################################################################################
# 1.COLLECTING DATA

library(readr)
insurance <- read_csv("C:/Users/user/Downloads/insurance.csv")
View(insurance)

insurance <- read.csv("C:/Users/user/Downloads/insurance.csv", stringsAsFactors = TRUE) 

# 2.EXPLORING & PREPARING DATA

str(insurance)
# 'data.frame':	1338 obs. of  13 variables:
#   $ age     : int  19 18 28 33 32 31 46 37 37 60 ...
# $ sex     : Factor w/ 2 levels "female","male": 1 2 2 2 2 1 1 1 2 1 ...
# $ bmi     : num  NA 33.8 33 22.7 28.9 ...
# $ children: int  0 1 3 0 0 0 1 3 2 0 ...
# $ smoker  : Factor w/ 2 levels "no","yes": 2 1 1 1 1 1 1 1 1 1 ...
# $ region  : Factor w/ 5 levels "middle","northeast",..: 5 4 4 3 3 4 4 3 2 3 ...
# $ charges : num  16885 1726 4449 21984 3867 ...
# $ friends : int  5 0 9 5 4 6 10 7 7 3 ...
# $ enemies : int  3 1 3 10 3 10 2 9 5 6 ...
# $ chips   : int  7 4 5 4 4 6 3 5 5 5 ...
# $ fries   : int  5 4 2 3 4 7 6 1 3 3 ...
# $ ramen   : int  3 4 1 2 1 4 7 1 3 1 ...
# $ toportyu: int  6 3 3 4 7 4 3 6 4 5 ...

#Since the dependent variable is charges, let's take a look to see how it is distributed:
summary(insurance$charges)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
1122    4740    9382   13270   16640   63770 

- Because the mean value is greater than the median, this implies that the distribution
of insurance charges is right-skewed. We can confirm this visually using a histogram:

hist(insurance$charges)

- The large majority of individuals in our data have yearly medical expenses between
zero and $15,000, although the tail of the distribution extends far past these peaks.
Because linear regression assumes a normal distribution for the dependent variable,
this distribution is not ideal. In practice, the assumptions of linear regression are
often violated. If needed, we may be able to correct this later on.

- Another problem at hand is that regression models require that every feature is
numeric, yet we have three factor type in our data frame.

-The sex variable is divided into male and female levels, while smoker is divided 
into yes and no. From the summary() output, we know that region has four levels, 
but we need to take a closer look to see how they are distributed.

table(insurance$region)
summary(insurance$region)

#middle northeast northwest southeast southwest 
10       320       324       360       324

- Before fitting a regression model to data, it can be useful to determine how the
independent variables are related to the dependent variable and each other.
A correlation matrix provides a quick overview of these relationships. Given
a set of variables, it provides a correlation for each pairwise relationship.

cor(insurance[c("age", "bmi", "children", "charges")]) # This brings output consisting of NA

complete.data <- insurance[complete.cases(insurance),] # complete.cases function removes the NAs.
complete.data
cor(complete.data[c("age", "bmi", "children", "charges")])

#               age        bmi   children    charges
# age      1.00000000 0.04982217 0.01374549 0.09706177
# bmi      0.04982217 1.00000000 0.01334992 0.20079120
# children 0.01374549 0.01334992 1.00000000 0.06824106
# charges  0.09706177 0.20079120 0.06824106 1.00000000


- At the intersection of each row and column pair, the correlation is listed for the
variables indicated by that row and column. The diagonal is always 1 since there
is always a perfect correlation between a variable and itself. The values above and
below the diagonal are identical since correlations are symmetrical. In other words,
cor(x, y) is equal to cor(y, x).

- None of the correlations in the matrix are considered strong, but there are some 
notable associations. For instance, age and bmi appear to have a moderate 
correlation, meaning that as age increases, so does bmi. There is also a moderate 
correlation between age and charges, bmi and charges, and children and 
charges.

-It can also be helpful to visualize the relationships among features, perhaps by using 
a scatterplot. Although we could create a scatterplot for each possible relationship, 
doing so for a large number of features might become tedious.
An alternative is to create a scatterplot matrix (sometimes abbreviated as SPLOM), 
which is simply a collection of scatterplots arranged in a grid. It is used to detect 
patterns among three or more variables.


pairs(insurance[c("age", "bmi", "children", "charges")])

-If we add more information to the plot, it can be even more useful. An 
enhanced scatterplot matrix can be created with the pairs.panels() function 
in the psych package.

install.packages("psych")
library(psych)
pairs.panels(insurance[c("age", "bmi", "children", "charges")])

- The oval-shaped object on each scatterplot is a correlation ellipse. It provides a
visualization of how strongly correlated the variables are. The dot at the center of the
ellipse indicates the point of the mean value for the x axis variable and y axis variable.
The correlation between the two variables is indicated by the shape of the ellipse; the
more it is stretched, the stronger the correlation. An almost perfectly round oval, as
with bmi and children, indicates a very weak correlation

- The curve drawn on the scatterplot is called a loess smooth. It indicates the general
 relationship between the x axis and y axis variables. It is best understood by
 example. The curve for age and children is an upside-down U, peaking around
 middle age. This means that the oldest and youngest people in the sample have
 fewer children than those around middle age. Because this trend is non-linear, this
 finding could not have been inferred from the correlations alone. On the other hand,
 the loess smooth for age and bmi is a line sloping gradually up, implying that BMI
 increases with age, but we had already inferred this from the correlation matrix.

# 3. TRAINING A MODEL ON THE DATA 
 
ins_model <- lm(charges ~ age + children + bmi + sex +
                  smoker + region, data = insurance)
ins_model

# 4. EVALUATING MODEL PERFORMANCE

summary(ins_model)

# Call:
#   lm(formula = charges ~ age + children + bmi + sex + smoker + 
#        region, data = insurance)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -35612  -4281   -925   3443  31833 
# 
# Coefficients:
#                   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     -3362.584   2451.570  -1.372 0.170420    
#   age                37.676      5.467   6.891 8.56e-12 ***
#   children          565.921    157.822   3.586 0.000348 ***
#   bmi               399.466     32.563  12.267  < 2e-16 ***
#   sexmale          -255.567    382.161  -0.669 0.503777    
# smokeryes       23789.029    473.894  50.199  < 2e-16 ***
#   regionnortheast -1711.136   2227.965  -0.768 0.442609    
# regionnorthwest -2240.678   2226.856  -1.006 0.314501    
# regionsoutheast -3233.877   2222.419  -1.455 0.145875    
# regionsouthwest -2718.933   2226.031  -1.221 0.222143    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 6927 on 1317 degrees of freedom
# (11 observations deleted due to missingness)
# Multiple R-squared:  0.6765,	Adjusted R-squared:  0.6743 
# F-statistic:   306 on 9 and 1317 DF,  p-value: < 2.2e-16



-The Residuals section provides summary statistics for the errors in our 
predictions, some of which are apparently quite substantial. Since a residual 
is equal to the true value minus the predicted value, the maximum error 
of 31833 suggests that the model under-predicted expenses by nearly 
$31900 for at least one observation.

-The stars (for example, ***) indicate the predictive power of each feature 
in the model. The significance level (as listed by the Signif. codes in the 
footer) provides a measure of how likely the true coefficient is zero given the 
value of the estimate. The presence of three stars indicates a significance level 
of 0, which means that the feature is extremely likely to be related to the 
dependent variable. A common practice is to use a significance level of 0.05 
to denote a statistically significant variable. If the model had few features 
that were statistically significant, it may be cause for concern, since it would 
indicate that our features are not very predictive of the outcome. Here, our 
model has several significant variables, and they seem to be related to the 
outcome in logical ways.

-The Multiple R-squared value (also called the coefficient of determination) 
provides a measure of how well our model as a whole explains the values 
of the dependent variable. It is similar to the correlation coefficient in that 
the closer the value is to 1.0, the better the model perfectly explains the data. 
Since the R-squared value is 0.6765, we know that nearly 68 percent of 
the variation in the dependent variable is explained by our model. Because 
models with more features always explain more variation, the Adjusted 
R-squared value corrects R-squared by penalizing models with a large 
number of independent variables. It is useful for comparing the performance 
of models with different numbers of explanatory variables.

-Given the preceding three performance indicators, our model is performing fairly 
well. It is not uncommon for regression models of real-world data to have fairly 
low R-squared values; a value of 0.68 is actually quite good.

# 5.IMPROVING MODEL PERFOMANCE

##Adding a non-linear relationship

y=a+bx~ # linear r/ship
y=a+bx+bx^2 -# non-linear r/ship

age2 <- insurance$age^2
age2

### Converting a numeric variable to a binary indicator

- BMI may have zero impact on medical expenditures for individuals in the normal weight range, but it 
may be strongly related to higher costs for the obese (that is, BMI of 30 or above).

-To create the feature, we can use the ifelse() function, which for each element 
in a vector tests a specified condition and returns a value depending on whether 
the condition is true or false. For BMI greater than or equal to 30, we will return 1, 
otherwise 0:
  
  insurance$bmi30 <- ifelse(insurance$bmi >= 30, 1, 0)

- What if certain features have a combined impact on the dependent variable? For 
instance, smoking and obesity may have harmful effects separately, but it is reasonable 
to assume that their combined effect may be worse than the sum of each one alone.

When two features have a combined effect, this is known as an interaction. If we 
suspect that two variables interact, we can test this hypothesis by adding their 
interaction to the model. Interaction effects can be specified using the R formula 
syntax. To interact the obesity indicator (bmi30) with the smoking indicator (smoker), 
we would write a formula in the form:
  charges ~ bmi30*smoker

-The * operator is shorthand that instructs R to model charges ~ bmi30 + 
  smokeryes + bmi30:smokeryes
The : (colon) operator in the expanded form indicates that bmi30:smokeryes is the 
interaction between the two variables. Note that the expanded form automatically 
also included the bmi30 and smoker variables as well as the interaction.

#### Puting it all together- an improved regression model


# - We'll train the model using the lm() function as before, but this time we'll add the 
# newly constructed variables and the interaction term:
  
  ins_model2 <- lm(charges ~ age + age2 + children + bmi + sex +
                     bmi30*smoker + region, data = insurance)  
  ins_model2 
  
  summary( ins_model2)
  
  # Call:
  #   lm(formula = charges ~ age + age2 + children + bmi + sex + bmi30 * 
  #        smoker + region, data = insurance)
  # 
  # Residuals:
  #   Min     1Q Median     3Q    Max 
  # -16963  -4224  -1304   2791  27651 
  # 
  # Coefficients:
  #                   Estimate Std. Error t value Pr(>|t|)    
  # (Intercept)      3.860e+03  2.225e+03   1.734   0.0831 .  
  #   age              5.094e-02  2.201e-02   2.315   0.0208 *  
  #   age2            -5.201e-08  2.326e-08  -2.236   0.0255 *  
  #   children         6.416e+02  1.323e+02   4.851 1.38e-06 ***
  #   bmi              1.887e+02  4.475e+01   4.216 2.66e-05 ***
  #   sexmale         -6.727e+02  3.206e+02  -2.099   0.0360 *  
  #   bmi30           -8.091e+02  5.541e+02  -1.460   0.1445    
  # smokeryes        1.343e+04  5.791e+02  23.191  < 2e-16 ***
  #   regionnortheast -5.202e+02  1.870e+03  -0.278   0.7809    
  # regionnorthwest -8.998e+02  1.870e+03  -0.481   0.6304    
  # regionsoutheast -1.714e+03  1.865e+03  -0.919   0.3581    
  # regionsouthwest -1.816e+03  1.868e+03  -0.973   0.3309    
  # bmi30:smokeryes  1.937e+04  7.937e+02  24.400  < 2e-16 ***
  #   ---
  #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  # 
  # Residual standard error: 5805 on 1314 degrees of freedom
  # (11 observations deleted due to missingness)
  # Multiple R-squared:  0.7733,	Adjusted R-squared:  0.7713 
  # F-statistic: 373.6 on 12 and 1314 DF,  p-value: < 2.2e-16
  # 
 
options(scipen = 999)
smokeryes<-1.343e+04 
print(smokeryes)        #13430

options(scipen = 999)
bmi30.smokeryes <- 1.937e+04
print(bmi30.smokeryes)         #19370
 
   
- Relative to our first model, 
the R-squared valuehas improved from 0.68 to about 0.77. Our model is now explaining 77 percent of the 
variation in medical treatment costs. Additionally, our theories about the model's 
functional form seem to be validated. The higher-order age2 term is statistically 
significant, as is the obesity indicator, bmi30. The interaction between obesity and 
smoking suggests a massive effect; in addition to the increased costs of over $13,430
for smoking alone, obese smokers spend another $19,370 per year. This may suggest 
that smoking exacerbates diseases associated with obesity.


  
  
  