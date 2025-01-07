library(ggplot2)
library(reshape)

rmse_endog <- read.csv('rmse_endog.csv', sep=';')
rmse_exog <- read.csv('rmse_exog.csv', sep=';')
smape_endog <- read.csv('smape_endog.csv', sep=';')
smape_exog <- read.csv('smape_exog.csv', sep=';')

rmse_min <- apply(cbind(rmse_endog, rmse_exog), 1, min)
smape_min <- apply(cbind(smape_endog, smape_exog), 1, min)

rmse_endog_trafo <- rmse_endog / rmse_min
rmse_exog_trafo <- rmse_exog / rmse_min
smape_endog_trafo <- smape_endog / smape_min
smape_exog_trafo <- smape_exog / smape_min

rmse_endog_gg <- melt(rmse_endog_trafo)
rmse_exog_gg <- melt(rmse_exog_trafo)
smape_endog_gg <- melt(smape_endog_trafo)
smape_exog_gg <- melt(smape_exog_trafo)

rmse_endog_gg$metric <- 'RMSE'
rmse_exog_gg$metric <- 'RMSE'
smape_endog_gg$metric <- 'sMAPE'
smape_exog_gg$metric <- 'sMAPE'

rmse_endog_gg$exogenous <- FALSE
rmse_exog_gg$exogenous <- TRUE
smape_endog_gg$exogenous <- FALSE
smape_exog_gg$exogenous <- TRUE

df_gg <- rbind(rmse_endog_gg, rmse_exog_gg, smape_endog_gg, smape_exog_gg)
df_gg$metric <- as.factor(df_gg$metric)
df_gg$exogenous <- as.factor(df_gg$exogenous)

levels(df_gg$variable)[levels(df_gg$variable) == 'GPR.linear'] <- 'GPR linear'
levels(df_gg$variable)[levels(df_gg$variable) == 'GPR.RBF'] <- 'GPR RBF'
levels(df_gg$variable)[levels(df_gg$variable) == 'Naive.last'] <- 'Naive last'

ggplot(data = df_gg, mapping = aes(x = variable, y = value, color=exogenous)) +
  geom_boxplot(fatten=1) +
  theme_bw() +
  facet_wrap( ~ metric, scales='free') +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
  labs(y='Relative performance',
       x='',
       color='Exogenous\n variables\n used?') +
  scale_color_manual(values=c('red', 'forestgreen'))
