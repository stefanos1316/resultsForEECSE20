# CTX task
library(effsize)
library(reshape2)

withoutMeltdown <-c(56696, 52091, 56502, 55881, 50223)
withMeltdown <-c(1000090428, 1000087469, 1000089298, 1000085442, 100008540)

shapiro.test(withoutMeltdown)
wilcox.test(withMeltdown, withoutMeltdown, paired = FALSE, exact= FALSE)

df <- data.frame(withMeltdown, withoutMeltdown)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)
