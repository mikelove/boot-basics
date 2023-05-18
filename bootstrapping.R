library(dplyr)
library(infer)
library(ggplot2)

set.seed(1)
n <- 200
x <- runif(n)
eps <- rexp(n, 5)
eps <- eps - mean(eps)
slope <- .2
y <- slope * x + eps
plot(x,y)
abline(0, slope, col="red")

dat <- data.frame(x,y)

# simple bootstrapping
set.seed(1)
boots <- replicate(1000, {
  idx <- sample(n, replace=TRUE)
  coef(lm(y ~ x, data=dat[idx,]))[2]
})
hist(boots)
sd(boots)

library(boot)
get_slope <- function(data, idx) coef(lm(y ~ x, data=data[idx,]))[2]
boots <- boot(dat, get_slope, R=1000)
boots
confint(boots)
hist(boots)

# S, H, G, C =
# Specify, Hypothesize, Generate, Calculate
# (how to remember: reverse alphabetical)

#set.seed(1)
#perm <- dat %>% specify(y ~ x) %>%
#  hypothesize(null="independence") %>%
#  generate(reps=1000, type="permute") %>%
#  calculate(stat="slope")

# bootstrapping a statistic:
set.seed(1)
boot <- dat %>% specify(y ~ x) %>%
  generate(reps=1000, type="bootstrap") %>%
  calculate(stat="slope")

visualize(boot, bins=30)

(ci <- get_ci(boot))

visualize(boot, bins=30) +
  shade_confidence_interval(ci, alpha=.3, fill="yellow", color="orange") + 
  geom_vline(xintercept=obs_beta$stat, color="orange")

ci
confint(lm(y ~ x), parm="x")

# Going further:
# Google: "Bootstrapping Regression Models in R"

library(car)
fit <- lm(y ~ x)
summary(fit)$coef
set.seed(1)
bfit <- Boot(fit, method="residual")
summary(bfit)

confint(bfit)
