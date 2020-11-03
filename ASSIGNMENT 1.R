
## Question 3 ##

hw1_data = read.csv("https://raw.githubusercontent.com/hgweon2/ss3859/master/hw1_data1.csv")

## 3.a ##

Vector_of_indices = which(hw1_data$x1 < 4)
Number_of_values1 = length(Vector_of_indices)
"Number of Relevant Values: " 
Number_of_values1

## 3.b ##

Vector_of_indices2 = which((hw1_data$x1 < 4) & (hw1_data$x2 == "L"))
Number_of_values2 = length(Vector_of_indices2)
"Number of Relevant Values: " 
Number_of_values2

## 3.c ##

x1_value_vector = hw1_data$x1
Indices_of_x1_values_given_x2_equals_L = which(hw1_data$x2 == "L")
Mean_of_values_for_x2_equals_L = mean(x1_value_vector[Indices_of_x1_values_given_x2_equals_L])
Median_of_values_for_x2_equals_L = median(x1_value_vector[Indices_of_x1_values_given_x2_equals_L])
SD_of_values_for_x2_equals_L = sd(x1_value_vector[Indices_of_x1_values_given_x2_equals_L])
"Mean: "
Mean_of_values_for_x2_equals_L
"Median: "
Median_of_values_for_x2_equals_L
"Standard deviation: "
SD_of_values_for_x2_equals_L

## 3.d ##

t.test(x1_value_vector, mu = 4, alternative = "two.sided", conf.level = 0.95)
"Test Statistic: 1.7192"
"P-Value: 0.08871"
"Since P-value is greater than the test statistic, Ho cannot be rejected and therefore no we cannot argue that the true mean of x1 differs from 4 in a statistically significant manner"

## 3.e ##
Vector_containing_x1_values_given_x2_equals_L = x1_value_vector[Indices_of_x1_values_given_x2_equals_L]
t.test(x= Vector_containing_x1_values_given_x2_equals_L, mu = 2, alternative = "greater", conf.level = 0.95)
"Test Statistic: 1.7714"
"P-Value: 0.04188"
"Since P-value is less than the test statistic, Ho can be rejected and therefore we can say (it is convincing to say) that the true mean of x1 given x2 = L is greater than 2 statistically speaking"

## Question 4 ##

set.seed(10)
idx = sample(nrow(cars),30,replace=FALSE)
cars2 = cars[idx,]

## 4.a ##

plot(cars2$speed,cars2$dist, main = "Speed vs Distance", xlab = "Speed", ylab = "Distance")
"From the scatterplot in a purely graphical/visual way there certainly seems to be some relationship between x and y although at best this relationship appears to be a moderate-weak positive correlation"

## 4.b ##
"I did this two ways to check;"
"Method 1 (Manually):"

n = length(cars2$speed)
SS_XY = sum(cars2$speed * cars2$dist) - ((sum(cars2$speed) * sum(cars2$dist))/n)
SS_XX = sum(cars2$speed^2) - ((sum(cars2$speed))^2)/n
SS_YY = sum(cars2$dist^2) - ((sum(cars2$dist))^2)/n
X_sample_mean = sum(cars2$speed)/n
Y_sample_mean = sum(cars2$dist)/n
beta1 = SS_XY/SS_XX
beta1
beta0 = Y_sample_mean - (beta1 * X_sample_mean)
beta0

"Method 2 (Using a function):"

cars3 = lm(dist~speed,data = cars2)
summary(cars3)
sigma_hat2 = sum((cars3$residuals)^2)/n-2
sigma_hat2

"In either method the estimates for the model are:"
"beta0 : -18.466"
"beta1 : 3.901"
"sigma_hat^2 : 180.925"

## 4.c ##

"You can do this either by:"

Error_at_5_Estimate = cars2$dist[5] - beta0 - (beta1 * cars2$speed[5])
Error_at_5_Estimate
Error_at_10_Estimate = cars2$dist[10] - beta0 - (beta1 * cars2$speed[10])
Error_at_10_Estimate
Error_at_20_Estimate = cars2$dist[20] - beta0 - (beta1 * cars2$speed[20])
Error_at_20_Estimate

"Or:"
cars3$residuals[5]
cars3$residuals[10]
cars3$residuals[20]

"Both give estimates:"
"e5 : 5.451101"
"e10 : -3.563742"
"e20 : 4.239227"

## 4.d ##

Indices_greater_than_20 = which(abs(cars3$residuals) > 20)
Indices_not_greater_than_20 = which(abs(cars3$residuals) <= 20)
plot(x = cars2$speed[Indices_not_greater_than_20],
     y = cars2$dist[Indices_not_greater_than_20],
     main = "Speed vs Distance",
     xlab = "Speed",
     ylab = "Distance")
points(x = cars2$speed[Indices_greater_than_20], y = cars2$dist[Indices_greater_than_20], col = "red", pch = 15)


## 4.e ##

sum(cars3$residuals)
"The sum of the residuals is -5.662137*10^(-15) (Essentially zero hence why we can assume for SLR that the random error adds to zero"

## 4.f ##

"So the fitted model will be:"
"y_hat = -18.466 + 3.901*x + -5.66*10^(-15)"
"Thus our predition for x = 21 is:"
y_hat = -18.466 + 3.901*21 + (-5.66*10^(-15))
y_hat
"y_hat = 63.455 or a rollout distance of 63.455 meters for a car travelling at 21mph"

Indices_greater_than_20 = which(abs(cars3$residuals) > 20)
Indices_not_greater_than_20 = which(abs(cars3$residuals) <= 20)
plot(x = cars2$speed[Indices_not_greater_than_20],
     y = cars2$dist[Indices_not_greater_than_20],
     main = "Speed vs Distance",
     xlab = "Speed (mph)",
     ylab = "Distance (m)")
points(x = cars2$speed[Indices_greater_than_20], y = cars2$dist[Indices_greater_than_20], col = "red", pch = 15)
abline(cars3)

## 4.g ##

"The ""goodness of fit"" or percentage of the variation observed in y that can be explained by our regression is encapsulated in our coeffiecient of determination, R^2"
Y = cars2$dist
Y_hat2 = cars3$fitted.values
SST = sum((Y - mean(Y))^2)
SSR = sum((Y_hat2 - mean(Y))^2)
R_squared = SSR/SST
R_squared
"R^2 = 0.6734058 and therefore approximately 67.3% of the observed variation can be explained by our fitted model"

## 4.h ##

"The core problem with this statement is that it relies upon an extreme use of extrapolation (predicting using a model outside of the models domains/limits).  This is extremely dangerous in terms of accuracy and if done must be done with extreme care.  In this particular instance the x value used to predict y is so far out of the domain of the model that it is quite likely that the relationship of the data may have changed hence leading to potentially erroneous predictions.  Additionally to assume ""exactly"" is somewhat foolish considering your preditions are based off of estimated values not their true values.  In other words, this statement to a) potentially incorrect because it assumes that the model may be extrapolated past the domain of the data when there is no statistical evidence to support this claim and the idea of exact values is foolish due to the element of estimation at play"

## 4.i ##

"Going to perform a two-sided CI using alpha = 0.05"
n2 = length(cars2$speed)
SSE = SST - SSR
Standard_Error = sqrt(SSE/(n2-2))
Upper_bound = beta1 + qt(0.975,df = n2 - 2)*sqrt(Standard_Error/SS_XX)
Lower_bound = beta1 - qt(0.975,df = n2 - 2)*sqrt(Standard_Error/SS_XX)
Upper_bound
Lower_bound
"Therefore our CI is (3.620376, 4182593)"

## 4.j ##

"Almost the same as the last:"
X_value = 21
n2 = length(cars2$speed)
SSE = SST - SSR
Standard_Error = sqrt(SSE/(n2-2))
Upper_bound2 = (beta0 + beta1 * X_value) + qt(0.95,df = n2 - 2)*Standard_Error*sqrt((1/n2) + (((X_value - X_sample_mean)^2)/SS_XX))
Lower_bound2 = (beta0 + beta1 * X_value) - qt(0.95,df = n2 - 2)*Standard_Error*sqrt((1/n2) + (((X_value - X_sample_mean)^2)/SS_XX))
Upper_bound2
Lower_bound2
"Therefore our CI is (56.81107, 70.11938)"

