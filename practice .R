# ---- workshop planning tools --- 
install.packages("binom")
install.packages("pwr")
install.packages("visreg")
install.packages("lsmeans")
install.packages("nlme")



library(binom)
library(pwr)
library(ggplot2)
library(dplyr)
library(visreg)
library(lsmeans)
library(nlme)

rand <- rnorm(1000, 1, 4)
rand <- round(rand,2)

test1 <- c("a", "b", "c")
str(test1)

test2 <- c(1,2,3,4)
str(test2)

treatment <- rep(c("treat","control"), c(10,10))
response <- rnorm(20, mean = 10, sd = 3)
mydat <- data.frame(treatment, response, stringsAsFactors = FALSE)

str(mydat)

sample1 <- sample(c("infected", "uninfected"), 20, replace = TRUE, p=c(0.5,0.5))

table(sample1)
rep(sample(c("infected", "uninfected"), 20, replace = TRUE, p=c(0.5,0.5)), 5)
table(rep(sample(c("infected", "uninfected"), 20, replace = TRUE, p=c(0.5,0.5)), 5))

sampl1 <- sample(c("mated", "unmated"), 18,replace = TRUE, p=c(0.7,0.3))
table(sampl1)

mydat <- data.frame(sampl1 <- rnorm(30, 0, 2)) 

ggplot(mydat, aes(mydat))+
  geom_histogram(binwidth = 0.1)


# --- detect a preference 

# create a random sample 
spiders <- sample(c("success", "fail"), 10, replace = TRUE, p=c(0.5,0.5))
table(spiders)

df1 <- data.frame(spiders)

# create a confidece intervall 
myCI <- binom.confint(sampl1, length(spiders), methods = "ac")
myCI
print(myCI)
myCI$upper
myCI$lower

# I have more than 1 confidence intervall --> is that correct 

# do the same with a loop and repeat it 

for(i in 1:nvalues){
  sample[i] <- sample(c("success", "fail"), 10, replace = TRUE, p=c(0.5,0.5))
  myCI <- binom.confint(sample[i], length(sample[i]), replace = TRUE, methods = "ac")
}

# I don´t know why I can´t build this loop 

# still have to finish the tasks 

sample2 <- sample(c(1,0), 20, replace=TRUE, p=c(0.7,0.3))
table(sample2)

sample3 <- sample(c("success", "fail"), 20, replace = TRUE, p=c(0.7,0.3))
table(sample3)

df1 <- data.frame(sample3)

# do I have to look up the number of successes in a table or can I program it that R automatically knows the number of successes 
z <- binom.test(sum(with(df1, df1$sample3=="fail")), length(sample2), p=0.5)
z$p.value

# try a loop for the same 
test <- rep(NA, 20)

for(counter in 1:20){
  sample.t <- (sample(c("fail", "success"), 20, replace = TRUE, p=c(0.7,0.4)))
  test[counter] = binom.test(sum(sample.t=="fail"), nrow(sample.t), p=0.5)
}

test
# how can I do this loop??? 

for(counter in 1:20){
  sample.t <- (sample(c(1,0), 20, replace = TRUE, p=c(0.7,0.4)))
  test[counter] = binom.test(sum(sample.t==1), length(sample.t), p=0.5)
}

test


# plan a 2*2 experiment 

treatment <- rep(c("treatment", "control"), c(30,30))
s1 <- sample(c("infected", "uninfected"), 30,replace = TRUE, p=c(0.2,0.8)) # that´s the control sample
s2 <- sample(c("infected", "uninfected"), 30, replace = TRUE, p=c(0.5, 0.5)) # that´s the treatment sample





experiment <- data.frame(treatment, malaria = c(s2,s1), stringsAsFactors = FALSE)


table(experiment)


ggplot(experiment, aes(experiment$malaria))+
  geom_bar(aes(fill= treatment))

ggplot(experiment, aes(experiment$malaria))+
  geom_bar(aes(fill = experiment$treatment),position = "dodge")+
  xlab("Malaria")


# 5)which sample size to reach a power of 80% 

t.control <- c(0.2,0.8)   
t.treatment <- c(0.5,0.5) 
t.prop <- cbind(t.control, t.treatment)
t.prop1 <- data.frame(t.control, t.treatment)

t.prop
t.prop1

w <- ES.w2(t.prop1/sum(t.prop1))

pwr.chisq.test(w, df=1, power = 0.80)


# repeat it but with different probabilities 

# how can I do this repetition with a for loop? 

new.treatment <- c(0.3:0.9,0.7:0.1)
t.prop2 <- data.frame(t.control, new.treatment)

w <- ES.w2(t.prop2/sum(t.prop2))

pwr.chisq.test(w, df=1, power=0.80)

# how can I do that? 




# ---- workshop linear models

# lions 

list.files()

# 1) 
df2 <- read.csv("lions.csv")

#2) 
df2 %>% head()

#3 & 4)  scatterplot with linear model

ggplot(df2, aes(df2$black, df2$age))+
  geom_point()+
  geom_smooth(method= lm, se=FALSE)

# 5) best fit of the model 
ggplot(df2, aes(df2$black, df2$age))+
  geom_point()+
  geom_smooth()

# 3) 
reg1 <- lm(df2$age~ df2$black, na.action = na.exclude)
summary(reg1)

anova(reg1)


# prediction 

p <- ggplot(df2, aes(df2$black, df2$age))


# how can I add the confidence bans 

test <- predict(reg1, interval="prediction")

new.df <- cbind(df2, test)

ggplot(new.df, aes(new.df$black, new.df$age))+
geom_point()+
  geom_line(aes(y=lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y=upr), color = "red", linetype = "dashed")+
  geom_smooth(method = lm)


# --- light treatment 

list.files()

df3 <- read.csv("knees.csv")

# look into the head 


df3 %>% head()

#3) is treatment a factor 
is.vector(df3$treatment)

is.factor(df3$treatment)
#4) confert variable treatment into a factor 
str(df3)

factor(df3$treatment)

is.factor(df3$treatment)

vector(df3$treatment)
is.vector(df3$treatment)

# 4) check out treatment with levels 
levels(df3$treatment)

#5) change the order of the levels 
levels(df3$treatment) <- c("control", "knee", "eyes")
levels(df3$treatment)

#6) plot the data 

ggplot(df3, aes(df3$treatment, df3$shift))+
  geom_point()

#7) fit a linear model 
reg2 <- lm(df3$shift~ df3$treatment)
summary(reg2)

reg3 <- lm(shift ~ treatment, data = df3)
summary(reg3)

#8) create a plot that shows the fitted values 

# how can I do that with ggplot
ggplot(reg2, aes(reg2))+
  geom_point()+
  geom_smooth(method = lm)

stripchart(fitted(reg3) ~ treatment, vertical=TRUE, add=TRUE, pch="------", method="jitter", data = df3)

stripchart(fitted(reg3) ~ treatment, vertical=TRUE, method = "jitter",data = df3)

# try package visreg 
visreg(reg3)
visreg(reg3, points.par = list(cex=1.2, col="red"))

#3) examine the assumption of the model 
plot(reg3)
hist(resid(reg3))
hist(resid(reg3), breaks = seq(from=-1.5, to=1.5, by=0.25))

#4) how does R model the categorial variables 
model.matrix(reg3)

#5) 
summary(reg3)

#6) confidence interval for estimators 
confint(reg3)

#7) anova 
anova(reg3)

#8) table with group means, standard errors and confidence intervals 
summary(reg3)
confint(reg3)
lsmeans(reg3, "treatment")




# --- Fly sex and longevity revisited

list.files()

df4 <- read.csv("fruitflies.csv")

# 2) 
df4 %>% head

# 3) factor treatemen? 
is.factor(df4$treatment)

#3) 
levels(df4$treatment)

#5) change the order of the levels 
levels(df4$treatment) <- c("no females addes", "1 pregnant female", "8 pregnant females", "1 virgin female", "8 virgin females")

#6) scatterplot 

ggplot(df4, aes(thorax.mm, longevity.days))+
         geom_point(aes(shape=factor(treatment)))

# --- fit a linear model 

#1) linear model 
reg4 <- lm(longevity.days ~ thorax.mm + treatment, data = df4)
summary(reg4)

# is there a change when I first place treatment 
reg5 <- lm(longevity.days ~ treatment + thorax.mm, data = df4)
summary(reg5)

#2) diagnostic 
plot(reg4)

plot(reg4, which = 1)

#3) log transformation of the response variable to fix the problem of increasing variance 
df4$loglongevity = log(df4$longevity.days)

reg6 <- lm(loglongevity ~ thorax.mm + treatment, data = df4)
summary(reg6)

plot(reg6, which = 1)

#4) plot with visreg 
visreg(reg6)

visreg(reg6, xvar = "thorax.mm", by = "treatment",  points.par = list(cex = 0.1, col = "red"))

visreg(reg6, xvar = "treatment", by = "thorax.mm", points.par = list(cex = 0.1, col = "red"))
# how can I scale the x-axis?

#5) 
summary(reg6)

# 6) 
confint(reg6)

#7) 
anova(reg6)

# 8) 
anova(reg5)


# workshop Linear mixed-effects model

# Repeatability of a sexual signal trait 

list.files()

df6 <- read.csv("flycatcher.csv")

# 2) 
df6 %>% head()



df7 <- economics
df7 %>% head()


FALSE || FALSE



