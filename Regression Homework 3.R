data = data.frame(read.csv(file.choose()))
sigma_sqr = 1
alpha = 0.05
y = data[,1]
m = data[,2]
x = data[,3]



# Problem 1 (2)

# This block of code uses the manual approach to find the beta vector for Model 1
# An automated method is used afterward to check results
col_o_ones = c()
for (i in 1:100){
  col_o_ones = append(col_o_ones,1)
}
data1 = cbind(y, col_o_ones)
data1 = cbind(data1, x)
model1_x = data1[,c(2:3)]
model1_bh = solve(t(model1_x)%*%model1_x)%*%t(model1_x)%*%y

# This block of code is the automated method for Model 1
model1 = lm(y ~ x, data)
summary(model1)
bh_11 = summary(model1)$coefficients["(Intercept)", "Estimate"]
bh_12 = summary(model1)$coefficients["x", "Estimate"]
------------------------------------------------------

# This block of code uses the manual approach to find the beta vector for Model 2
data2 = cbind(m, col_o_ones)
data2 = cbind(data2, x)
model2_x = data2[,c(2:3)]
model2_bh = solve(t(model2_x)%*%model2_x)%*%t(model2_x)%*%m

# This block of code is the automated method for Model 2
model2 = lm(m ~ x, data)
summary(model2)
bh_21 = summary(model2)$coefficients["(Intercept)", "Estimate"]
bh_22 = summary(model2)$coefficients["x", "Estimate"]
------------------------------------------------------

# This block of code uses the manual approach to find the beta vector for Model 3
data3 = cbind(y, col_o_ones)
data3 = cbind(data3, m)
data3 = cbind(data3, x)
model3_x = data3[,c(2:4)]
model3_bh = solve(t(model3_x)%*%model3_x)%*%t(model3_x)%*%y

# This block of code is the automated method for Model 3
model3 = lm(y ~ m + x, data)
summary(model3)
bh_31 = summary(model3)$coefficients["(Intercept)", "Estimate"]
bh_32 = summary(model3)$coefficients["m", "Estimate"]
bh_33 = summary(model3)$coefficients["x", "Estimate"]


# Problem 1 (3)
model2_xi = solve(t(model2_x)%*%model2_x)
var_bh_22 = sigma_sqr*model2_xi[2,2]

model3_xi = solve(t(model3_x)%*%model3_x)
var_bh_32 = sigma_sqr*model3_xi[2,2]


# Problem 1 (4)
sobel_Z = (bh_22*bh_32) / (sqrt((bh_22^2)*var_bh_32 + (bh_32^2)*var_bh_22))


# Problem 1 (5)
crit_Z = qnorm(1-alpha/2)
sobel_pval = 2*(1-pnorm(sobel_Z))









