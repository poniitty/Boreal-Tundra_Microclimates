library(pals)
library(tidyverse)
library(lubridate)
library(scales)
library(patchwork)

your_sm  <- read_csv ("data/all_data_daily.csv")

unique(your_sm$area)

your_sm$area <- recode_factor(your_sm$area,
                              MAL = "MAL",
                              AIL = "AIL",
                              VAR = "VAR",
                              TII = "TII",
                              PIS = "PIS",
                              HYY = "HYY",
                              KAR = "KAR")

your_sm %>%
  group_by(area, site) %>%
  count() %>%
  group_by(area) %>%
  count()

# colors
gnuplot (10)
your_palette <- colorRampPalette(c("#000099", "#0000FF", "#5000FF", "#9F0FF0", "#EF42BD", "#FF758A", "#FFA857", "#FFDB24"))

plotting_months <- paste0(month.abb[c(11:12, 1:10)], " ", c(rep(2019, times = 2), rep(2020, times = 10)))

# plot area summaries together as boxplots
p_T1 = your_sm %>% 
  mutate(month = paste0(month.abb[month(date)], " ", year(date))) %>% 
  mutate(month = factor(month, levels = plotting_months)) %>% 
  ggplot(aes(x=month, y=T1_mean, fill = area, color = area)) +
  geom_hline(yintercept=0, col= "lightgrey", lwd=0.25) +
  geom_hline(yintercept=10, linetype="dashed", col="lightgrey", lwd=0.25)+
  geom_hline(yintercept=(-10), linetype="dashed", col="lightgrey", lwd=0.25)+
  geom_boxplot (notch=TRUE, notchwidth = 0.05, width=0.5, position=position_dodge(0.8), outlier.shape=NA, lwd=0.25, alpha=2/10) +
  scale_fill_manual(values = rev(your_palette(7))) +
  scale_color_manual(values = rev(your_palette(7))) +
  ggtitle("Soil temperature at -6 cm below ground") +
  ylab("Temperature (°C)") +
  xlab("Time") +
  ylim(-30,30) +
  scale_x_discrete(labels=c("Nov 2019" = "Nov 2019",
                            "Dec 2019" = "",
                            "Jan 2020" = "Jan 2020",
                            "Feb 2020" = "",
                            "Mar 2020" = "Mar 2020",
                            "Apr 2020" = "",
                            "May 2020" = "May 2020",
                            "Jun 2020" = "",
                            "Jul 2020" = "Jul 2020",
                            "Aug 2020" = "",
                            "Sep 2020" = "Sep 2020",
                            "Oct 2020" = "")) +
  theme_classic() +
  theme(
    legend.position = "None")

p_T4 = your_sm %>% 
  mutate(month = paste0(month.abb[month(date)], " ", year(date))) %>% 
  mutate(month = factor(month, levels = plotting_months)) %>% 
  ggplot(aes(x=month, y=T4_mean, fill = area, color = area)) +
  geom_hline(yintercept=0, col= "lightgrey", lwd=0.25) +
  geom_hline(yintercept=10, linetype="dashed", col="lightgrey", lwd=0.25)+
  geom_hline(yintercept=(-10), linetype="dashed", col="lightgrey", lwd=0.25)+
  geom_boxplot (notch=TRUE, notchwidth = 0.05, width=0.5, position=position_dodge(0.8), outlier.shape = NA, lwd=0.25, alpha=2/10) +
  scale_fill_manual(values = rev(your_palette(7))) +
  scale_color_manual(values = rev(your_palette(7))) +
  ggtitle("Air temperature at 150 cm above ground") +
  ylab("Temperature (°C)") +
  xlab("") +
  ylim(-30,30) +
  theme_classic() +
  theme(
    legend.position = "None",
    axis.text.x = element_blank())

# plot area names only
your_palette(7)

p_names = ggplot() +
  annotate("text", x = 1, y =1.7, size = 3.5, fontface =2,
           label = "Malla",
           colour="#FFDB24") +
  annotate("text", x = 1, y =1.6, size = 3.5, fontface =2,
           label = "Ailakkavaara",
           colour="#FF9F5F") +
  annotate("text", x = 1, y =1.5, size = 3.5, fontface =2,
           label = "Värriö",
           colour="#F9649B") +
  annotate("text", x = 1, y =1.4, size = 3.5, fontface =2,
           label = "Tiilikka",
           colour="#C728D6") +
  annotate("text", x = 1, y =1.3, size = 3.5, fontface =2,
           label = "Pisa",
           colour="#6A05FA") +
  annotate("text", x = 1, y =1.2, size = 3.5, fontface =2,
           label = "Hyytiälä",
           colour="#0D00FF") +
  annotate("text", x = 1, y =1.1, size = 3.5, fontface =2,
           label = "Karkali",
           colour="#000099") +
  theme_void()

layout <- '
A
A
B
B
C
'

dev.off()
pdf(file="visuals/fig_boxplots.pdf", width = 6.30, height = 8.66)

wrap_plots(A = p_T4,
           B = p_T1,
           C = p_names,design = layout) +
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 8))
dev.off()

