
`r if(show_pMMoV) {"### pMMoV Copies/L"}`

```{r pmmoviolin}

den.plt_pmmov <- results_abs %>% filter(str_detect(Target_Name, 'pMMoV')) %>% 
  ggplot(aes(x = WWTP, y = Copies_Per_Liter_WW, colour = Target_Name, label = Date)) + geom_jitter(width = 1)+
  scale_x_discrete(guide = guide_axis(n.dodge=2)) + geom_violin(show.legend = F)

print(den.plt_pmmov)

ggplotly(den.plt_pmmov, tooltip = 'label')

```


`r if(show_pMMoV) {"### Logscale pMMoV Copies/L"}`

```{r logpmmovviolin}

den.plt_pmmov %>% format_logscale_y() %>% print()

```

ggplot(results_abs, aes(x = Sample_ID, y = Copies_Per_Liter_WW)) +
  geom_violin(fill = "lightblue") +
  geom_boxplot(width = 0.2, fill = "white", color = "black") +
  theme_minimal() +
  labs(x = "Sample ID", y = "Copies Per Liter WW") +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))