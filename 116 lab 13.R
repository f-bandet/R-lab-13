# Name: Faye Bandet
# Date: 11/21/19
# ISTA 116 Section B || Section Leader : Jacob Heller
# Lab Assignment 13
# Collaborator(s): Nick Ackerman

download.file("http://www.openintro.org/stat/data/atheism.RData", destfile = "atheism.RData")
load("atheism.RData")

#1
# The percentages look like they are population parameters for China and Japan. They are percentages and averages that describe the whole population.
#2
dataf <- subset(atheism, nationality == "United States")
atheism.us <- table(dataf$response, dataf$year)
atheism.us
#             2005 2012
# atheist       10   50
# non-atheist  992  952

#3
ptable <- prop.table(atheism.us[, "2012"])
ptable
#   atheist non-atheist 
# 0.0499002   0.9500998
# There were 0.0499 percent atheists. This table doesnt exactly match the chart in Table 6. There are less categories in the table for this data.It confirms step 1.

#4
prop.test(as.table(atheism.us[,"2012"]), alternative = "greater")
# The p value is 0.0499 which is basically 0.05 so the condition is met so we can make a confidence interval.The 95 percent confidence interval is
# 0.03930392 - 1.00000000

#5
atheism.glob <- table(atheism$response, atheism$year)
prop.test(as.table(atheism.glob[,"2012"], atheism.us[,"2012"]), alternative = "greater")
# The p value is 0.0669 so 2012 is statistically different for atheist and nonatheist populations in globad data because its over 0.05. There are approximately the same amount of atheists in the US as there are in the rest of the world.

#6
prop.test(as.table(atheism.us["atheist",]), alternative = "greater")
# The p value is 0.1666 so we know it is statistically different because its greater than 0.05.There were less atheists in 2005 than the ratio of worldwide atheists in 2012.The p-value is 0.1666667.

#7
n <- 1000
p <- seq(0, 1, 0.01)
se <- sqrt((p*(1-p))/n)
se
# Many rows appear with numbers for each of the standard error values for values in p.

#8
plot(se - p)
# A graph appears, negative correlation, the largest confidance interval would be for 0.

#9
dataf2 <- subset(atheism, nationality == "Spain")
SPNatheism <- table(dataf2$response, dataf2$year)
SPNatheism
prop.test(as.table(SPNatheism["atheist",]), alternative = "greater")
#              2005 2012
#  atheist      115  103
#  non-atheist 1031 1042
# p value is 0.5275
# We would reject the null because it's over 0.05, it is a random sample which checks conditions for inference because there is a large sample size, also there was not a big increse in atheism.

#10
datsf3 <- subset(atheism, nationality == "Brazil")
BRatheism <- table(dataf3$response, dataf3$year)
BRatheism
dataf4 <- subset(atheism, nationality == "Colombia")
COatheism <- table(dataf4$response, dataf4$year)
COatheism
prop.test(as.table(COatheism[, "2012"], BRatheism[, "2012"]), alternative = "greater")
# Tables created. Followed conditions for inference, large random sample. The prop test shows its a normal distribution. Small p value shows not much difference in both countries in 2012.


