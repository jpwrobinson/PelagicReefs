source('func_mod_conditional.R')

th_marg<-theme(plot.margin=unit(c(0.1, .1, .1,.1), 'cm'),
               axis.title = element_text(size = 10),
               axis.text = element_text(size=9))

## IME plots

# Panel A = 06_ime_time. m_detect. code is ready.
load(file = 'results/mod_ime_time_binom.rds')
load(file = 'results/mod_ime_time_hurdle.rds')

fitted_df_detect<-focal
fitted_df_gamma<-focalCont

# A = MLD anomaly preds, B = MLD anomaly preds
df_mldclim<-marginal_post(m_detectFull, fitted_df_detect, 'mld_clim_s', 'mld_clim')
df_mldanom<-marginal_post(m_detectFull, fitted_df_detect, 'mld_anom_s', 'mld_anom')
df_mldchange<-marginal_post(m_detectFull, fitted_df_detect, 'mld_change_s', 'mld_change')

df_mldclimG<-marginal_post(m_hurdleFull, fitted_df_detect, 'mld_clim_s', 'mld_clim')
df_mldanomG<-marginal_post(m_hurdleFull, fitted_df_detect, 'mld_anom_s', 'mld_anom')
df_mldchangeG<-marginal_post(m_hurdleFull, fitted_df_detect, 'mld_change_s', 'mld_change')

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
  labs(x = expression(Delta * " Mixed layer anomaly, m"), y = "") +
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

g_mld_covs<-plot_grid(gA, gB, gC + labs(x = expression(Delta * " Mixed layer anomaly, m")), 
                      gD, gE, gF + labs(x = expression(Delta * " Mixed layer anomaly, m")), 
                      align='hv', nrow=2, 
                      labels=c('a', 'b', 'c', 'd', 'e', 'f'))

# load MLD predictions
source('Figure4_MLD.R')

gMLD_change<-plot_grid(gC + labs(y = 'P(IME)', 
                                 x = expression(Delta * " Mixed layer anomaly, m")), 
                       gF + labs( y = 'IME strength'), labels = c('c', 'd'), nrow=1)

gIME<-plot_grid(gMLD_anom, gMLD_change, nrow=2, labels=c('a', ''))

pdf(file = 'fig/Figure4.pdf', height=5, width=11)
plot_grid(gIME, 
          gMLDdelta + theme(plot.margin = unit(c(0.8, 0, .5, 1), 'cm')), 
          nrow=1, labels=c('', 'b'), rel_widths=c(1, 0.36))
dev.off()

pdf(file = 'fig/FigureSX_IME_time_covariates.pdf', height=5, width=10)
g_mld_covs
dev.off()

pdf(file = 'fig/FigureSX_MLD_time_obs.pdf', height=7, width=5)
gSX
dev.off()
