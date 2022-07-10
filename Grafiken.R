library(readxl)
library(ggplot2)
library(extrafont)
library(tidyverse)
library(cowplot)
library(viridis)
library(cowplot)
windowsFonts(Liberation=windowsFont("Liberation Sans"))
windowsFonts(Times=windowsFont("Times New Roman"))

Wirkstoffe <- read_excel("./Grafikdaten.xlsx", sheet = 1)
Tage <- read_excel("./Grafikdaten.xlsx", sheet = 2)


#### Linienplot ####

str(Wirkstoffe)
Wirkstoffe <- Wirkstoffe %>% 
  mutate(Konzentration = factor(Konzentration, levels = c("0", "0.32", "1", "3.16", "10", "31.6", "100")))

Wirkstoffe_plot_Ipro <- Wirkstoffe %>% 
  filter(Woche == 1, Wirkstoff == "Iprovalicarb") %>% 
  select(-Woche) %>% 
  gather("key", "value", 3:6)

plot_W_Ipro <-  Wirkstoffe_plot_Ipro %>% 
  ggplot(aes(x = key, y = value, group = Konzentration)) +
  geom_point(aes(color = Konzentration), shape = 19, size = 2.5) +
  geom_line(aes(color = Konzentration), size = 1) +
  # scale_color_brewer(palette = "Dark2",
  #                    name = bquote("Konzentration [mg/l]:")) +
  scale_color_manual(values = c("grey30", "#E69F00", "brown3", "#009E73",
                                "black", "#0072B2", "#D55E00"),
                     name = bquote("Konzentration [mg/l]:")) +
  theme_bw() +
  guides(color = guide_legend(nrow = 2, 
                             byrow = TRUE))+
  #  geom_hline(yintercept = 0) +
  theme(legend.position = "bottom",
        text = element_text(family = "Times", size = 17),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white")) +
  scale_x_discrete(breaks = c("Tag1", "Tag2", "Tag3", "Tag4"),
                   labels = c("1", "2", "3", "4")) +
  scale_y_continuous(limits = c(0, 8),
                     breaks = seq(0, 8, 0.5)) +
  labs(x = "Tag",
       y = "Wachstum [cm]") 

ggsave(paste0("./Grafiken/Wirkstoff_Ipro_Woche1.png"),
       width = 25,
       height = 14,
       units = "cm",
       dpi = 300)

Wirkstoffe_plot_Boscalid <- Wirkstoffe %>% 
  filter(Woche == 1, Wirkstoff == "Boscalid") %>% 
  select(-Woche) %>% 
  gather("key", "value", 3:6)

plot_W_Bosca <-  Wirkstoffe_plot_Boscalid %>% 
  ggplot(aes(x = key, y = value, group = Konzentration)) +
  geom_point(aes(color = Konzentration), shape = 19, size = 2.5) +
  geom_line(aes(color = Konzentration), size = 1) +
  # scale_color_brewer(palette = "Dark2",
  #                    name = bquote("Konzentration [mg/l]:")) +
  scale_color_manual(values = c("grey30", "#E69F00", "brown3", "#009E73",
                                "black", "#0072B2", "#D55E00"),
                     name = bquote("Konzentration [mg/l]:")) +
  theme_bw() +
  guides(color = guide_legend(nrow = 2, 
                              byrow = TRUE))+
  #  geom_hline(yintercept = 0) +
  theme(legend.position = "bottom",
        text = element_text(family = "Times", size = 17),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white")) +
  scale_x_discrete(breaks = c("Tag1", "Tag2", "Tag3", "Tag4"),
                   labels = c("1", "2", "3", "4")) +
  scale_y_continuous(limits = c(0, 8),
                     breaks = seq(0, 8, 0.5)) +
  labs(x = "Tag",
       y = "Wachstum [cm]") 

ggsave(paste0("./Grafiken/Wirkstoff_Bosca_Woche1.png"),
       width = 25,
       height = 14,
       units = "cm",
       dpi = 300)

Wirkstoffe_plot_Pro <- Wirkstoffe %>% 
  filter(Woche == 1, Wirkstoff == "Prothioconazol") %>% 
  select(-Woche) %>% 
  gather("key", "value", 3:6)

plot_W_Pro <-  Wirkstoffe_plot_Pro %>% 
  ggplot(aes(x = key, y = value, group = Konzentration)) +
  geom_point(aes(color = Konzentration), shape = 19, size = 2.5) +
  geom_line(aes(color = Konzentration), size = 1) +
  # scale_color_brewer(palette = "Dark2",
  #                    name = bquote("Konzentration [mg/l]:")) +
  scale_color_manual(values = c("grey30", "#E69F00", "brown3", "#009E73",
                                "black", "#0072B2", "#D55E00"),
                     name = bquote("Konzentration [mg/l]:")) +
  theme_bw() +
  guides(color = guide_legend(nrow = 2, 
                              byrow = TRUE))+
  #  geom_hline(yintercept = 0) +
  theme(legend.position = "bottom",
        text = element_text(family = "Times", size = 17),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white")) +
  scale_x_discrete(breaks = c("Tag1", "Tag2", "Tag3", "Tag4"),
                   labels = c("1", "2", "3", "4")) +
  scale_y_continuous(limits = c(0, 8),
                     breaks = seq(0, 8, 0.5)) +
  labs(x = "Tag",
       y = "Wachstum [cm]") 

ggsave(paste0("./Grafiken/Wirkstoff_Pro_Woche1.png"),
       width = 25,
       height = 14,
       units = "cm",
       dpi = 300)

Wirkstoffe_plot_Ipro <- Wirkstoffe %>% 
  filter(Woche == 2, Wirkstoff == "Iprovalicarb") %>% 
  select(-Woche) %>% 
  gather("key", "value", 3:6)

plot_W_Ipro <-  Wirkstoffe_plot_Ipro %>% 
  ggplot(aes(x = key, y = value, group = Konzentration)) +
  geom_point(aes(color = Konzentration), shape = 19, size = 2.5) +
  geom_line(aes(color = Konzentration), size = 1) +
  # scale_color_brewer(palette = "Dark2",
  #                    name = bquote("Konzentration [mg/l]:")) +
  scale_color_manual(values = c("grey30", "#E69F00", "brown3", "#009E73",
                                "black", "#0072B2", "#D55E00"),
                     name = bquote("Konzentration [mg/l]:")) +
  theme_bw() +
  guides(color = guide_legend(nrow = 2, 
                              byrow = TRUE))+
  #  geom_hline(yintercept = 0) +
  theme(legend.position = "bottom",
        text = element_text(family = "Times", size = 17),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white")) +
  scale_x_discrete(breaks = c("Tag1", "Tag2", "Tag3", "Tag4"),
                   labels = c("1", "2", "3", "4")) +
  scale_y_continuous(limits = c(0, 8),
                     breaks = seq(0, 8, 0.5)) +
  labs(x = "Tag",
       y = "Wachstum [cm]") 

ggsave(paste0("./Grafiken/Wirkstoff_Ipro_Woche2.png"),
       width = 25,
       height = 14,
       units = "cm",
       dpi = 300)

Wirkstoffe_plot_Boscalid <- Wirkstoffe %>% 
  filter(Woche == 2, Wirkstoff == "Boscalid") %>% 
  select(-Woche) %>% 
  gather("key", "value", 3:6)

plot_W_Bosca <-  Wirkstoffe_plot_Boscalid %>% 
  ggplot(aes(x = key, y = value, group = Konzentration)) +
  geom_point(aes(color = Konzentration), shape = 19, size = 2.5) +
  geom_line(aes(color = Konzentration), size = 1) +
  # scale_color_brewer(palette = "Dark2",
  #                    name = bquote("Konzentration [mg/l]:")) +
  scale_color_manual(values = c("grey30", "#E69F00", "brown3", "#009E73",
                                "black", "#0072B2", "#D55E00"),
                     name = bquote("Konzentration [mg/l]:")) +
  theme_bw() +
  guides(color = guide_legend(nrow = 2, 
                              byrow = TRUE))+
  #  geom_hline(yintercept = 0) +
  theme(legend.position = "bottom",
        text = element_text(family = "Times", size = 17),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white")) +
  scale_x_discrete(breaks = c("Tag1", "Tag2", "Tag3", "Tag4"),
                   labels = c("1", "2", "3", "4")) +
  scale_y_continuous(limits = c(0, 8),
                     breaks = seq(0, 8, 0.5)) +
  labs(x = "Tag",
       y = "Wachstum [cm]") 

ggsave(paste0("./Grafiken/Wirkstoff_Bosca_Woche2.png"),
       width = 25,
       height = 14,
       units = "cm",
       dpi = 300)

Wirkstoffe_plot_Pro <- Wirkstoffe %>% 
  filter(Woche == 2, Wirkstoff == "Prothioconazol") %>% 
  select(-Woche) %>% 
  gather("key", "value", 3:6)

plot_W_Pro <-  Wirkstoffe_plot_Pro %>% 
  ggplot(aes(x = key, y = value, group = Konzentration)) +
  geom_point(aes(color = Konzentration), shape = 19, size = 2.5) +
  geom_line(aes(color = Konzentration), size = 1) +
  # scale_color_brewer(palette = "Dark2",
  #                    name = bquote("Konzentration [mg/l]:")) +
  scale_color_manual(values = c("grey30", "#E69F00", "brown3", "#009E73",
                                "black", "#0072B2", "#D55E00"),
                     name = bquote("Konzentration [mg/l]:")) +
  theme_bw() +
  guides(color = guide_legend(nrow = 2, 
                              byrow = TRUE))+
  #  geom_hline(yintercept = 0) +
  theme(legend.position = "bottom",
        text = element_text(family = "Times", size = 17),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white")) +
  scale_x_discrete(breaks = c("Tag1", "Tag2", "Tag3", "Tag4"),
                   labels = c("1", "2", "3", "4")) +
  scale_y_continuous(limits = c(0, 8),
                     breaks = seq(0, 8, 0.5)) +
  labs(x = "Tag",
       y = "Wachstum [cm]") 

ggsave(paste0("./Grafiken/Wirkstoff_Pro_Woche2.png"),
       width = 25,
       height = 14,
       units = "cm",
       dpi = 300)
#### Barplot ####

str(Tage)
Tage <- Tage %>% 
  mutate(Konzentration = factor(Konzentration, levels = c("0", "0.32", "1", "3.16", "10", "31.6", "100")),
         Tag = as.factor(Tag))

Tage_plot_Tag2 <- Tage %>% 
  filter(Woche == 1, Tag == 2) %>% 
  select(-Woche, -Tag) %>% 
  gather("key", "value", 2:4)

plot_Tag2 <-  Tage_plot_Tag2 %>% 
  ggplot(aes(x = Konzentration, y = value, fill = key)) +
  geom_bar(stat="identity", position = position_dodge(), color = "black")+
  # scale_fill_manual(values = c("blue4", "dodgerblue4","cyan3"),
  #                    name = bquote("Wirkstoff:")) +
  scale_fill_manual(values = c("black", "grey50","grey80"),
                    name = bquote("Wirkstoff:")) +
  # scale_fill_brewer(palette = "Blues",
  #                    name = bquote("Wirkstoff:")) +
  theme_bw() +
  #  geom_hline(yintercept = 0) +
  theme(legend.position = "bottom",
        text = element_text(family = "Times", size = 17),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white")) +
  scale_y_continuous(limits = c(0, 8),
                     breaks = seq(0, 8, 0.5)) +
  labs(x = "Konzentration [mg/l]",
       y = "Wachstum [cm]") 

ggsave(paste0("./Grafiken/Tag2_barplot_Woche1.png"),
       width = 25,
       height = 14,
       units = "cm",
       dpi = 300)

Tage_plot_Tag4 <- Tage %>% 
  filter(Woche == 1, Tag == 4) %>% 
  select(-Woche, -Tag) %>% 
  gather("key", "value", 2:4)

plot_Tag4 <-  Tage_plot_Tag4 %>% 
  ggplot(aes(x = Konzentration, y = value, fill = key)) +
  geom_bar(stat="identity", position = position_dodge(), color = "black")+
  # scale_fill_manual(values = c("blue4", "dodgerblue4","cyan3"),
  #                    name = bquote("Wirkstoff:")) +
  scale_fill_manual(values = c("black", "grey50","grey80"),
                    name = bquote("Wirkstoff:")) +
  # scale_fill_brewer(palette = "Blues",
  #                    name = bquote("Wirkstoff:")) +
  theme_bw() +
  #  geom_hline(yintercept = 0) +
  theme(legend.position = "bottom",
        text = element_text(family = "Times", size = 17),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white")) +
  scale_y_continuous(limits = c(0, 8),
                     breaks = seq(0, 8, 0.5)) +
  labs(x = "Konzentration [mg/l]",
       y = "Wachstum [cm]") 

ggsave(paste0("./Grafiken/Tag4_barplot_Woche1.png"),
       width = 25,
       height = 14,
       units = "cm",
       dpi = 300)

Tage_plot_Tag2_W2 <- Tage %>% 
  filter(Woche == 2, Tag == 2) %>% 
  select(-Woche, -Tag) %>% 
  gather("key", "value", 2:4)

plot_Tag2_W2 <-  Tage_plot_Tag2_W2 %>% 
  ggplot(aes(x = Konzentration, y = value, fill = key)) +
  geom_bar(stat="identity", position = position_dodge(), color = "black")+
  # scale_fill_manual(values = c("blue4", "dodgerblue4","cyan3"),
  #                    name = bquote("Wirkstoff:")) +
  scale_fill_manual(values = c("black", "grey50","grey80"),
                    name = bquote("Wirkstoff:")) +
  # scale_fill_brewer(palette = "Blues",
  #                    name = bquote("Wirkstoff:")) +
  theme_bw() +
  #  geom_hline(yintercept = 0) +
  theme(legend.position = "bottom",
        text = element_text(family = "Times", size = 17),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white")) +
  scale_y_continuous(limits = c(0, 8),
                     breaks = seq(0, 8, 0.5)) +
  labs(x = "Konzentration [mg/l]",
       y = "Wachstum [cm]") 

ggsave(paste0("./Grafiken/Tag2_barplot_Woche2.png"),
       width = 25,
       height = 14,
       units = "cm",
       dpi = 300)

Tage_plot_Tag4_W2 <- Tage %>% 
  filter(Woche == 2, Tag == 4) %>% 
  select(-Woche, -Tag) %>% 
  gather("key", "value", 2:4)

plot_Tag4_W2 <-  Tage_plot_Tag4_W2 %>% 
  ggplot(aes(x = Konzentration, y = value, fill = key)) +
  geom_bar(stat="identity", position = position_dodge(), color = "black")+
  # scale_fill_manual(values = c("blue4", "dodgerblue4","cyan3"),
  #                    name = bquote("Wirkstoff:")) +
  scale_fill_manual(values = c("black", "grey50","grey80"),
                    name = bquote("Wirkstoff:")) +
  # scale_fill_brewer(palette = "Blues",
  #                    name = bquote("Wirkstoff:")) +
  theme_bw() +
  #  geom_hline(yintercept = 0) +
  theme(legend.position = "bottom",
        text = element_text(family = "Times", size = 17),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white")) +
  scale_y_continuous(limits = c(0, 8),
                     breaks = seq(0, 8, 0.5)) +
  labs(x = "Konzentration [mg/l]",
       y = "Wachstum [cm]") 

ggsave(paste0("./Grafiken/Tag4_barplot_Woche2.png"),
       width = 25,
       height = 14,
       units = "cm",
       dpi = 300)


