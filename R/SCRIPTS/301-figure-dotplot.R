###------FIGURE DOTPLOT-----
## @knitr dotplot

dotplot <- read_csv("./R/DATA-PROCESSED/RESULTS/summary_table_county.csv")  %>%
  group_by(year, scenario) %>%
  do(fitHour = tidy(lm(cost ~ 1, weights = commuters, data = .))) %>% 
  unnest(fitHour)  %>%
  pivot_wider(names_from = scenario,
              values_from = estimate) %>%
  mutate(low = case_when(
    year< 2050 ~ historic,
    year > 2050 ~ low
  ),
  extreme = case_when(
    year< 2050 ~ historic,
    year > 2050 ~ extreme
  ),
  intermediate = case_when(
    year< 2050 ~ historic,
    year > 2050 ~ intermediate
  ),
  # code = STATEID
  ) %>%
  ungroup() 
# 
dotplot_extreme <-  dotplot[17,]
dotplot_int <-  dotplot[18,]
dotplot_low <-  dotplot[19,]

dotplot_big<- 
  ggplot(dotplot, aes(x = year, y =intermediate)) +
  geom_point(size = 2) +
  geom_smooth(data=dotplot, se=F, color = "dark cyan") +
  geom_smooth(data=dotplot, se=T, aes(x=year, y = historic), linetype=0, color = "dark cyan") +
  geom_smooth(data=dotplot, se=F, aes(x=year, y = extreme), color = "red") +
  geom_smooth(data=dotplot, se=F, aes(x=year, y = low), color = "dark blue") +
  geom_errorbar(data=dotplot_extreme, width = 0, color = "red",
                aes(ymin=dotplot_extreme$extreme + (-1.282 *dotplot_extreme$std.error), 
                    ymax= dotplot_extreme$extreme + (1.282 *dotplot_extreme$std.error) )) +
  geom_errorbar(data=dotplot_int, width = 0, color = "dark cyan",
                aes(ymin=dotplot_int$intermediate + (-1.282 *dotplot_int$std.error), 
                    ymax= dotplot_int$intermediate + (1.282 *dotplot_int$std.error) )) +
  geom_errorbar(data=dotplot_low, width = 0, color = "dark blue",
                aes(ymin=dotplot_low$low + (-1.282 *dotplot_low$std.error), 
                    ymax= dotplot_low$low + (1.282 *dotplot_low$std.error) )) +
  
  geom_point(data= dotplot[which(dotplot$year == 2060),],
             aes(x=year, y = extreme), color = "red", fill = "maroon", size =2) +
  geom_point(data= dotplot[which(dotplot$year == 2060),],
             aes(x=year, y = intermediate), color = "dark cyan", fill = "dark cyan", size =2) +
  geom_point(data= dotplot[which(dotplot$year == 2060),],
             aes(x=year, y = low), color = "dark blue", fill = "dark blue", size =2) +
  geom_point(data= dotplot[which(dotplot$year %in% c(2018,2019)),],
             aes(x=year, y = intermediate), color = "maroon", fill = "maroon", size =2) +
  annotate("text", x =2061, y = dotplot$extreme[which(dotplot$year==2060)], label = "Extreme", color = "Red",hjust = 0) +
  annotate("text", x =2061, y = dotplot$intermediate[which(dotplot$year==2060)], label = "Intermediate", color = "dark cyan",hjust = 0) +
  annotate("text", x =2061, y = dotplot$low[which(dotplot$year==2060)], label = "Low", color = "dark blue",hjust = 0) +
  theme_bw() +
  xlim(2002, 2070) +
  labs(y = "Total Yearly Delay Per Commuter (Minutes)",
       x="Year") +
  NULL

dotplot_insert <- ggplot(dotplot, aes(x = year, y =intermediate)) +
  geom_point(size = 2) +
  geom_smooth(data=dotplot, se=F, color = "dark cyan") +
  geom_smooth(data=dotplot, se=T, aes(x=year, y = historic), level = .8, linetype=0, color = "dark cyan") +
  geom_smooth(data=dotplot, se=F, aes(x=year, y = extreme), color = "red") +
  geom_smooth(data=dotplot, se=F, aes(x=year, y = low), color = "dark blue") +
  
  geom_point(data= dotplot[which(dotplot$year == 2060),],
             aes(x=year, y = extreme), color = "red", fill = "maroon", size =2) +
  geom_point(data= dotplot[which(dotplot$year == 2060),],
             aes(x=year, y = intermediate), color = "dark cyan", fill = "dark cyan", size =2) +
  geom_point(data= dotplot[which(dotplot$year == 2060),],
             aes(x=year, y = low), color = "dark blue", fill = "dark blue", size =2) +
  geom_point(data= dotplot[which(dotplot$year %in% c(2018,2019)),],
             aes(x=year, y = intermediate), color = "maroon", fill = "maroon", size =2) +
  annotate("text", x =2061, y = dotplot$extreme[which(dotplot$year==2060)], label = "Extreme", color = "Red",hjust = 0) +
  annotate("text", x =2061, y = dotplot$intermediate[which(dotplot$year==2060)], label = "Intermediate-High", color = "dark cyan",hjust = 0) +
  annotate("text", x =2061, y = dotplot$low[which(dotplot$year==2060)], label = "Low", color = "dark blue",hjust = 0) +
  theme_bw() +
  labs(y = "",
       x="Year") +
  coord_cartesian(xlim = c(2002, 2019), ylim = c(0, 30)) +
  NULL

dotplot <- ggdraw() +
  draw_plot(dotplot_big) +
  draw_plot(dotplot_insert, x = 0.15, y = .4, width = .4, height = .4)

dotplot