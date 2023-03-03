n = 14
k = 3

y1 = c(8,9,6,8,5)
y2 = c(5,4,7,6,6)
y3 = c(9,3,2,4)

n1 = length(y1)
n2 = length(y2)
n3 = length(y3)

y1_bar = sum(y1)/n1
y2_bar = sum(y2)/n2
y3_bar = sum(y3)/n3
y_bar = (sum(y1)+sum(y2)+sum(y3)) / (n1+n2+n3)


# 1.1
ssb = (n1*(y1_bar-y_bar)^2 + n2*(y2_bar-y_bar)^2 + n3*(y3_bar-y_bar)^2)

# 1.2
ssw = sum((y1-y1_bar)^2) + sum((y2-y2_bar)^2) + sum((y3-y3_bar)^2)

# 1.3
sst = ssb+ssw
dfb = k-1
dfw = n-k
dft = n-1

msb = ssb/dfb
msw = ssw/dfw

F_stat = msb/msw

# 1.4
p_stat = pf(F_stat, dfb, dfw, lower.tail = FALSE)

# 2.1
data = data.frame(read.csv("~/Documents/Regression/HW6_data.csv", header = TRUE))
two_way_ANOVA = aov(grade ~ program + gender + program*gender, data = data)
summary(two_way_ANOVA)
