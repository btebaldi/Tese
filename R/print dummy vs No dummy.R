rm(list = ls())
library(readxl)
library(dplyr)
library(ggplot2)


tbl <- read_excel("~/GitHub/Tese/Ox Metrics GVAR/Ox Scripts/mat_files/Result_Matrix/_MAIN/Relacao de ensaios.xlsx", 
                                 range = "B2:K24")
head(tbl)

tbl$Dummy <- factor(tbl$Dummy,
                    levels = c(TRUE, FALSE),
                    labels = c("On", "Off"))

g1 <- tbl %>% filter(!(Modelo %in% c("AR1", "AR13", "PCA", "VECM", "GVAR")) ) %>% 
ggplot() +
  geom_line(aes(x = Log10_IIS, y = TTT, colour=Dummy, linetype=Dummy)) + 
  geom_point(aes(x = Log10_IIS, y = TTT, colour=Dummy)) + 
  geom_text(aes(x = Log10_IIS, y = TTT+10,
                label=sprintf("M%d",Model_number))) + 
labs(title = "Comparison GVAR-IIS with/without seasonal dummy",
     subtitle = "Net Employment",
     y="RMSE",
     x="Log10(p-value)",
     caption = "Source: elaborated by the author") +
  theme_bw() +
  theme(legend.position="bottom")
  


# default ggplot2 theme
print(g1)
ggsave(filename = sprintf("Dummy vs No Dummy.png"),
       path = "Graficos",
       plot = g1,
       scale=1, units = "in", dpi = 300, width = 10.4, height = 5.85)
