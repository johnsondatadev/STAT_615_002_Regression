library(tidyverse)

taxdf <- tribble(
  ~SellingPrice, ~Taxes,
  279900,	3104,
  146500,	1173,
  237700,	3076,
  200000,	1608,
  159900,	1454,
  499900,	2997,
  265500,	4054,
  289900,	3002
)

taxdf %>% 
  ggplot(aes(x = Taxes, y = SellingPrice)) +
  geom_point() +
  geom_smooth(se = F, method = lm)


summary(lm(taxdf$SellingPrice~taxdf$Taxes))

lm.fit <- lm(taxdf$SellingPrice~taxdf$Taxes)

taxdf %>% 
  mutate(
    residuals = SellingPrice - (100456.77 + 62.32 * Taxes)
  ) -> taxdf_resid
taxdf_resid

lm.fit %>% 
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0)

# Yes, there is an outlier in the residual plot
anova(lm.fit)
qf(p=0.05, df1=1, df2=6, lower.tail = F)

fvalue <- 2.9181
fcritical <- 5.987378

ifelse(fvalue > fcritical, "Reject the null hypothesis", "Fail to reject the null hypothesis")
