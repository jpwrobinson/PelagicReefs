source('0_loads/00_plot_theme.R')
load(file = 'results/mod_ime_time_binom.rds')
load(file = 'results/mod_ime_time_hurdle.rds')

func_time_fig<-function(model, df, filename, ylab){
  ce<-conditional_effects(model, effects = "time_s",
                          conditions = distinct(df, island))[[1]] %>% 
    left_join(df %>% distinct(region, island, region.col))
  
  regs<-unique(ce$region)
  
  for(i in 1:length(regs)){
    
    gg<-ggplot(ce %>% filter(region %in% regs[i]), 
               aes(time_s, estimate__, ymin = lower__, ymax = upper__, fill=region.col)) +
      geom_ribbon(alpha=0.5) + geom_line() +
      facet_wrap(~island, nrow =1) +
      scale_fill_identity() +
      labs(x = 'Month-year (scaled)', y = ylab, subtitle = regs[i]) +
      theme(legend.position = 'none', 
            axis.text = element_text(size=11),
            axis.title = element_text(size=11)) 
    
    # manual y lims
    if(ylab =='IME strength' & regs[i] %in% c('Northwestern Hawaiian', 'Hawaii')){gg <- gg + scale_y_continuous(labels=label_percent(), limits = c(0, 0.75))}
    if(ylab =='IME strength' & regs[i] %in% c('Mariana')){gg <- gg + scale_y_continuous(labels=label_percent(), limits = c(0, 3.5))}
    if(ylab =='IME strength' & regs[i] %in% c('Equatorial')){gg <- gg + scale_y_continuous(labels=label_percent(), limits = c(0, 1.5))}
    if(ylab =='IME strength' & regs[i] %in% c('Samoa')){gg <- gg + scale_y_continuous(labels=label_percent(), limits = c(0, 2.2))}
    
    assign(paste0('gg', str_replace_all(regs[i], ' island', '')), gg)
    assign(paste0('gg', str_replace_all(regs[i], ' Hawaiian', '')), gg)
  }
  
  pdf(file = filename, height=9, width=14)
  print(
    ggMariana / (ggNorthwestern + ggHawaii) / (ggEquatorial + ggSamoa)
  )
  dev.off()
}

func_time_fig(m_detectFull, focal, 'fig/FigureSX_IME_temporalFullDetect.pdf', ylab = 'P(IME)')
func_time_fig(m_detectTime, focal, 'fig/FigureSX_IME_temporalOnlyDetect.pdf', ylab = 'P(IME)')

func_time_fig(m_hurdleFull, focalCont, 'fig/FigureSX_IME_temporalFullGamma.pdf', ylab = 'IME strength')
func_time_fig(m_hurdleTime, focalCont, 'fig/FigureSX_IME_temporalOnlyGamma.pdf', ylab = 'IME strength')

