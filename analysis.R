# Set working directory
setwd("~/Documents/GitHub/coursera-datascience-regression-models-project")

# Load up neccessary libraries
library(GGally)
library(data.tables)

# Load in data
dB <- mtcars

# Let's look at the data first to understand structure and basic summary stats
head(dB)
str(dB) # all numerical data
summary(dB)

# Doing a few simple plots to understand relationship between data
# Scatter matrix to understand the relationships between the data
png(filename="pairs.png")
pairs(dB)
dev.off()

# Box plot to understand the relationships between transmission type and mpg
g <- ggplot(dB, aes(factor(am), mpg, fill = factor(am)))
g <- g + geom_boxplot()
g <- g + geom_jitter(position = position_jitter(width = .1, height = 0))
g <- g + scale_colour_discrete(name = "Type")
g <- g + scale_fill_discrete(name = "Type", breaks = c("0", "1"), labels = c("Automatic", "Manual"))
g <- g + scale_x_discrete(breaks = c("0", "1"), labels = c("Automatic", "Manual"))
g <- g + xlab("")

png(filename="boxplot.png")
g
dev.off()

# Doing a t-test (unpaired) to test whether there is a statistically significant different between mpg depending on transmission (one sided test)
g1 <- subset(mtcars, mtcars$am==0)
g2 <- subset(mtcars, mtcars$am==1)
t1 <- t.test(g1$mpg, g2$mpg, alternative="less", paired=F)
t1.summary <- data.frame("p-value" = c(t1$p.value), "CI-Lower" = c(t1$conf[1]), "CI-Upper" = c(t1$conf[2]), row.names = c("Automatic vs. Manual:  "))
round(t1.summary, 3)

# Do a base model fitting for just mpg against am
fit <- lm(mpg ~ am, data = dB)
summary(fit)

# Do a stepwise regression 
stepfit <- step(lm(data = dB, mpg ~ .), trace = 0)
summary(s_model)