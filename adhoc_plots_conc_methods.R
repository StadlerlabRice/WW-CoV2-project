# Ad hoc plots for concentration methods

# Dilution factors plot
plt.dilution <- {ggplot(vol_R, aes(Sample_name, dilution_factor, colour = Target)) + geom_jitter() + ggtitle('Conc. methods2 - Dilution factors')} %>% format_logscale_y() %>% format_classic()  %>% print()

ggsave('qPCR analysis/Methods paper/Archive/dilution factors.png', plt.dilution, width = 8, height = 4)


# Prelim plot after dilution
{ggplot(vol_R, aes(Sample_name, dilution_factor, colour = Target)) + geom_jitter() + ggtitle('Conc. methods2 - preliminary plot')} %>% format_logscale_y() %>% format_classic()  %>% print()


# Prelim plot after dilution - copies per ul RNA
prelim_copy_RNA <- {ggplot(processed_quant_data, aes(Sample_name, `Copy #`, colour = Target)) + 
    geom_jitter() + 
    ggtitle('Conc. methods2 - copies/ul RNA- preliminary plot')} %>% 
  format_logscale_y() %>%
  format_classic()  %>% 
  print()

ggsave('qPCR analysis/Methods paper/Archive/conc_methods_2_RNA.png', prelim_copy_RNA, width = 8, height = 4)


# copies per L ww plot
prelim_copy_WW <- {ggplot(processed_quant_data, aes(Sample_name, Recovered, colour = Target)) + 
    geom_jitter() + 
    ggtitle('Conc. methods2 - copies/L WW - preliminary plot')} %>% 
  format_logscale_y() %>% 
  format_classic()  %>% 
  print()

ggsave('qPCR analysis/Methods paper/Archive/conc_methods_2_WW.png', prelim_copy_WW, width = 8, height = 4)


# N1-N2 zoom on NTC
plotly::ggplotly(prelim_copy_RNA, dynamicTicks = T)
