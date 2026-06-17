source('func_mod_conditional.R')

th_marg<-theme(plot.margin=unit(c(0.1, .1, .1,.1), 'cm'),
               axis.title = element_text(size = 10),
               axis.text = element_text(size=9))

## IME plots

# Panel A = 06_ime_time. m_detect. code is ready.
load(file = 'results/mod_ime_time_binom.rds')
load(file = 'results/mod_ime_time_hurdle.rds')

topDet<-m_detectMLDtrend
topHur<-m_hurdleMLDtrend

# A = MLD anomaly preds, B = MLD anomaly preds
df_mldmean<-marginal_post(topDet, focal, 'mld_mean_s', 'mld_mean')
df_mldanom<-marginal_post(topDet, focal, 'mld_anom_s', 'mld_anom')
df_mldslope<-marginal_post(topDet, focal, 'mld_slope_s', 'mld_slope')

df_mldmeanG<-marginal_post(topHur, focal, 'mld_mean_s', 'mld_mean')
df_mldanomG<-marginal_post(topHur, focal, 'mld_anom_s', 'mld_anom')
df_mldslopeG<-marginal_post(topHur, focal, 'mld_slope_s', 'mld_slope')

# df_time<-marginal_post_island(topDet, focal, 'time_s', 'time')
# df_timeG<-marginal_post(topHur, focal, 'time_s', 'time')

# Plot and multipanel
gA<-ggplot(df_mldmean, aes(mld_mean, .epred)) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper, group = .width),
              alpha = 0.2, fill = "steelblue") +
  geom_line(colour = "steelblue", linewidth = 0.9) +
  lims(y = c(0, 1)) +
  labs(x = "", y = "P(IME)") +
  th_marg

gB<-ggplot(df_mldanom, aes(mld_anom, .epred)) +
    geom_vline(xintercept = 0, linetype=5, alpha=0.5) +
    geom_ribbon(aes(ymin = .lower, ymax = .upper, group = .width),
              alpha = 0.2, fill = "steelblue") +
    lims(y = c(0, 1)) +
    geom_line(colour = "steelblue", linewidth = 0.9) +
    labs(x = "", y = "") +
    th_marg

gC<-ggplot(df_mldslope, aes(mld_slope, .epred)) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper, group = .width),
              alpha = 0.2, fill = "steelblue") +
  lims(y = c(0, 1)) +
  geom_line(colour = "steelblue", linewidth = 0.9) +
  labs(x = "", y = "") +
  th_marg

gD<-ggplot(df_mldmeanG, aes(mld_mean, .epred)) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper, group = .width),
              alpha = 0.2, fill = "steelblue") +
  geom_line(colour = "steelblue", linewidth = 0.9) +
  scale_y_continuous(labels=label_percent(), limits=c(0, 0.55)) +
  labs(x = "Mixed layer depth [mean], m", y = "IME strength") +
  th_marg

gE<-ggplot(df_mldanomG, aes(mld_anom, .epred)) +
  geom_vline(xintercept = 0, linetype=5, alpha=0.5) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper, group = .width),
              alpha = 0.2, fill = "steelblue") +
  geom_line(colour = "steelblue", linewidth = 0.9) +
  scale_y_continuous(labels=label_percent(), limits=c(0, 0.55)) +
  labs(x = "Mixed layer depth [anomaly], m", y = "") +
  th_marg

gF<-ggplot(df_mldpredG, aes(mld_pred, .epred)) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper, group = .width),
              alpha = 0.2, fill = "steelblue") +
  geom_line(colour = "steelblue", linewidth = 0.9) +
  scale_y_continuous(labels=label_percent(), limits=c(0, 0.55)) +
  labs(x = "∆ Mixed layer depth, m [1993-2026]", y = "") +
  th_marg


g_mld_covs<-plot_grid(gA, gB, gC, gD, gE, gF, 
                      align='hv', nrow=2, 
                      labels=c('a', 'b', 'c', 'd', 'e', 'f'))
 
histA <- ggplot(focal, aes(mld_mean)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  scale_x_continuous(expand=c(0,0)) + theme_void() 

histB<-  ggplot(focal, aes(mld_anom)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  scale_x_continuous(expand=c(0,0)) + theme_void() 

# inset histos
gA<-gA + annotation_custom(
    grob = ggplotGrob(histA),
    xmin = min(focal$mld_mean), max(focal$mld_mean),  # Adjust the x-axis placement of the inset
    ymin = -Inf, ymax = 0.1  # Adjust the y-axis placement of the inset
  )

gB<-gB + annotation_custom(
  grob = ggplotGrob(histB),
  xmin = min(focal$mld_anom), max(focal$mld_anom),  # Adjust the x-axis placement of the inset
  ymin = -Inf, ymax = 0.1  # Adjust the y-axis placement of the inset
)

# load MLD predictions
source('Figure3_MLD.R')

gMLD_change<-plot_grid(gC + labs(y = 'P(IME)', x = "∆ Mixed layer depth, m [1993-2026]", y = ""), 
                       gF, labels = c('b', 'c'), nrow=1)

gIME<-plot_grid(gMLD_anom, gMLD_change, nrow=2, labels=c('a', ''), rel_widths=c(1, 0.4))

pdf(file = 'fig/Figure3.pdf', height=5, width=10)
plot_grid(gIME, gMLD_delta, nrow=1, labels=c('', 'd'), rel_widths=c(1, 0.4))
dev.off()

pdf(file = 'fig/FigureSX_IME_time_covariates.pdf', height=5, width=10)
g_mld_covs
dev.off()

pdf(file = 'fig/FigureSX_MLD_time_obs.pdf', height=3.5, width=5)
gSX
dev.off()
