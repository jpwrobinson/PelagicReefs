source('func_mod_conditional.R')

th_marg<-theme(plot.margin=unit(c(0.1, .1, .1,.1), 'cm'),
               axis.title = element_text(size = 10),
               axis.text = element_text(size=9))

## IME plots

# Panel A = 06_ime_time. m_detect. code is ready.
load(file = 'results/mod_ime_time_binom.rds')
load(file = 'results/mod_ime_time_hurdle.rds')

fitted_df_gamma<-focal
fitted_df_detect<-focalCont

# A = MLD anomaly preds, B = MLD anomaly preds
df_mldclim<-marginal_post(topDet, fitted_df_detect, 'mld_clim_s', 'mld_clim')
df_mldanom<-marginal_post(topDet, fitted_df_detect, 'mld_anom_s', 'mld_anom')
df_mldchange<-marginal_post(topDet, fitted_df_detect, 'mld_change_s', 'mld_change')

df_mldclimG<-marginal_post(topHur, fitted_df_detect, 'mld_clim_s', 'mld_clim')
df_mldanomG<-marginal_post(topHur, fitted_df_detect, 'mld_anom_s', 'mld_anom')
df_mldchangeG<-marginal_post(topHur, fitted_df_detect, 'mld_change_s', 'mld_change')

# df_time<-marginal_post_island(topDet, focal, 'time_s', 'time')
# df_timeG<-marginal_post(topHur, focal, 'time_s', 'time')

# Plot and multipanel
gA<-ggplot(df_mldclim, aes(mld_clim, .epred)) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper, group = .width),
              alpha = 0.2, fill = "steelblue") +
  geom_line(colour = "steelblue", linewidth = 0.9) +
  scale_y_continuous(expand=c(0,0), limits=c(0,1)) +
  labs(x = "", y = "P(IME)") +
  th_marg

gB<-ggplot(df_mldanom, aes(mld_anom, .epred)) +
    geom_vline(xintercept = 0, linetype=5, alpha=0.5) +
    geom_ribbon(aes(ymin = .lower, ymax = .upper, group = .width),
              alpha = 0.2, fill = "steelblue") +
  scale_y_continuous(expand=c(0,0), limits=c(0,1)) +
    geom_line(colour = "steelblue", linewidth = 0.9) +
    labs(x = "", y = "") +
    th_marg

gC<-ggplot(df_mldchange, aes(mld_change, .epred)) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper, group = .width),
              alpha = 0.2, fill = "steelblue") +
  scale_y_continuous(expand=c(0,0), limits=c(0,1)) +
  geom_line(colour = "steelblue", linewidth = 0.9) +
  labs(x = "", y = "") +
  th_marg

gD<-ggplot(df_mldclimG, aes(mld_clim, .epred)) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper, group = .width),
              alpha = 0.2, fill = "steelblue") +
  geom_line(colour = "steelblue", linewidth = 0.9) +
  scale_y_continuous(expand=c(0,0), labels=label_percent(), limits=c(0, 0.70)) +
  labs(x = "Mixed layer depth [mean], m", y = "IME strength") +
  th_marg

gE<-ggplot(df_mldanomG, aes(mld_anom, .epred)) +
  geom_vline(xintercept = 0, linetype=5, alpha=0.5) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper, group = .width),
              alpha = 0.2, fill = "steelblue") +
  geom_line(colour = "steelblue", linewidth = 0.9) +
  scale_y_continuous(expand=c(0,0), labels=label_percent(), limits=c(0, 0.70)) +
  labs(x = "Mixed layer depth [anomaly], m", y = "") +
  th_marg

gF<-ggplot(df_mldchangeG, aes(mld_change, .epred)) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper, group = .width),
              alpha = 0.2, fill = "steelblue") +
  geom_line(colour = "steelblue", linewidth = 0.9) +
  scale_y_continuous(expand=c(0,0), labels=label_percent(), limits=c(0, 0.70)) +
  labs(x = "∆ Mixed layer depth, m [1993-2026]", y = "") +
  th_marg

# add underlying data histograms 
histA <- ggplot(fitted_df_detect, aes(mld_clim)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  scale_x_continuous(expand=c(0,0)) + theme_void() 

histB<-  ggplot(fitted_df_detect, aes(mld_anom)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  scale_x_continuous(expand=c(0,0)) + theme_void() 

histC<-  ggplot(fitted_df_detect, aes(mld_change)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  scale_x_continuous(expand=c(0,0)) + theme_void() 

histD <- ggplot(fitted_df_gamma, aes(mld_clim)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  scale_x_continuous(expand=c(0,0)) + theme_void() 

histE<-  ggplot(fitted_df_gamma, aes(mld_anom)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  scale_x_continuous(expand=c(0,0)) + theme_void() 

histF<-  ggplot(fitted_df_gamma, aes(mld_change)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  scale_x_continuous(expand=c(0,0)) + theme_void() 


# inset histos
gA<-gA + annotation_custom(
    grob = ggplotGrob(histA),
    xmin = min(fitted_df_detect$mld_clim), max(fitted_df_detect$mld_clim),  # Adjust the x-axis placement of the inset
    ymin = -Inf, ymax = 0.2  # Adjust the y-axis placement of the inset
  )

gB<-gB + annotation_custom(
  grob = ggplotGrob(histB),
  xmin = min(fitted_df_detect$mld_anom), max(fitted_df_detect$mld_anom),  # Adjust the x-axis placement of the inset
  ymin = -Inf, ymax = 0.2  # Adjust the y-axis placement of the inset
)

gC<-gC + annotation_custom(
  grob = ggplotGrob(histC),
  xmin = min(fitted_df_detect$mld_change), max(fitted_df_detect$mld_change),  # Adjust the x-axis placement of the inset
  ymin = -Inf, ymax = 0.2  # Adjust the y-axis placement of the inset
)

gD<-gD + annotation_custom(
  grob = ggplotGrob(histD),
  xmin = min(fitted_df_gamma$mld_clim), max(fitted_df_gamma$mld_clim),  # Adjust the x-axis placement of the inset
  ymin = -Inf, ymax = 0.1  # Adjust the y-axis placement of the inset
)

gE<-gE + annotation_custom(
  grob = ggplotGrob(histE),
  xmin = min(fitted_df_gamma$mld_anom), max(fitted_df_gamma$mld_anom),  # Adjust the x-axis placement of the inset
  ymin = -Inf, ymax = 0.1  # Adjust the y-axis placement of the inset
)

gF<-gF + annotation_custom(
  grob = ggplotGrob(histF),
  xmin = min(fitted_df_gamma$mld_change), max(fitted_df_gamma$mld_change),  # Adjust the x-axis placement of the inset
  ymin = -Inf, ymax = 0.1  # Adjust the y-axis placement of the inset
)

g_mld_covs<-plot_grid(gA, gB, gC, gD, gE, gF, 
                      align='hv', nrow=2, 
                      labels=c('a', 'b', 'c', 'd', 'e', 'f'))

# load MLD predictions
source('Figure3_MLD.R')

gMLD_change<-plot_grid(gC + labs(y = 'P(IME)', 
                                 x = "∆ Mixed layer depth, m [1993-2026]"), 
                       gF + labs( y = 'IME strength'), labels = c('b', 'c'), nrow=1)

gIME<-plot_grid(gMLD_anom, gMLD_change, nrow=2, labels=c('a', ''), rel_widths=c(1, 0.4))

pdf(file = 'fig/Figure3.pdf', height=5, width=11)
plot_grid(gIME, 
          gMLDdelta + theme(plot.margin = unit(c(1.1, 0.1, 1.1, .5), 'cm')), 
          nrow=1, labels=c('', 'd'), rel_widths=c(1, 0.6))
dev.off()

pdf(file = 'fig/FigureSX_IME_time_covariates.pdf', height=5, width=10)
g_mld_covs
dev.off()

pdf(file = 'fig/FigureSX_MLD_time_obs.pdf', height=3.5, width=7)
plot_grid(gSX, gExtreme, nrow =1, labels =c('a', 'b'))
dev.off()
