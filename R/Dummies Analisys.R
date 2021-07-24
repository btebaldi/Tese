
# Setup -------------------------------------------------------------------
rm(list = ls())
library(readr)
library(dplyr)
# library(tidyr)
library(ggplot2)

dir.COM <- "COM IIS - Modelo 8"

COM_IIS.filepath = file.path("..", "Ox Metrics GVAR","Ox Scripts", "mat_files", "Result_Matrix", dir.COM, "Dummies by region.rds")

if(!file.exists(COM_IIS.filepath))
{
  stop("ARQUIVO NAO EXISTE")
}

tbl <- readRDS(COM_IIS.filepath)

tbl <- tbl[-1,]

tbl2 <- tbl %>% group_by(Dummy) %>% 
  summarize(qtd=n(), .groups = "drop") %>% arrange(desc(qtd)) %>% 
  mutate(qtd2=qtd/264)





tbl3 <- tibble(Date=seq(from=as.Date("1995-01-01"), to=as.Date("2016-12-01"), by="month"))

tbl3 <- tbl3 %>% left_join(tbl2, by = c("Date"="Dummy"))
tbl3$qtd[is.na(tbl3$qtd)] <- 0

tbl3$Rib1 <- 0

# (Impeachment of Dilma Rousseff 
# https://en.wikipedia.org/wiki/Impeachment_of_Dilma_Rousseff)
tbl3$Rib1[tbl3$Date %in% seq(from=as.Date("2015-12-01"),
                             to=as.Date("2016-04-01"),
                             by="month")] <- 1

# (Presidential election) 
# https://en.wikipedia.org/wiki/2014_Brazilian_general_election
tbl3$Rib1[tbl3$Date %in% seq(from=as.Date("2014-10-01"),
                             to=as.Date("2015-01-01"),
                             by="month")] <- 1


# Protesto dos 20 centavos
# Taper Tantrum do Ben Bernanke
# https://en.wikipedia.org/wiki/2013_protests_in_Brazil
# https://www.pri.org/stories/2012-03-17/brazilian-economy-what-went-wrong
# https://www.americasquarterly.org/article/revisiting-brazils-2013-protests-what-did-they-really-mean/
tbl3$Rib1[tbl3$Date %in% seq(from=as.Date("2012-12-01"),
                             to=as.Date("2013-06-01"),
                             by="month")] <- 1


# 2 2016-12-01   183 (Temer - Michel Miguel Elias Temer Lulia)
# https://www.foreignaffairs.com/articles/brazil/2016-05-16/will-temer-end-crisis-brazil
# https://en.wikipedia.org/wiki/Michel_Temer
tbl3$Rib1[tbl3$Date %in% seq(from=as.Date("2016-04-01"),
                             to=as.Date("2016-12-01"),
                             by="month")] <- 1



g <- ggplot(tbl3) + 
  geom_ribbon(aes(ymin = 0, ymax = Inf*Rib1, x = Date), fill = "blue", alpha=0.3) +
  geom_col(aes(x=Date, y=qtd), alpha = 1) + 
  labs(title="Number of models per Dummy",
       y="Model count",
       x=NULL,
       caption = "Source: elaborated by the author") +
  theme_bw() 

print(g)

ggsave(filename = sprintf("Dummy - Qtd regioes.png"),
       path = dirname(COM_IIS.filepath),
       plot = g,
       scale=1, units = "in", dpi = 300,width = 10.4, height = 5.85)




# Visao das regioes -------------------------------------------------------
tbl2 <- tbl %>% group_by(region) %>% 
  summarize(qtd=n(), .groups = "drop") %>% arrange(desc(qtd)) %>% 
  mutate(qtd2=qtd/264)

g2 <- ggplot(tbl2) +
  geom_bar(aes(x=qtd), fill="grey50") + 
  labs(title="Amount of dummies in each region",
       y="Number of regions",
       x="Dummies",
       caption = "Source: elaborated by the author") +
  theme_bw() 

print(g2)

ggsave(filename = sprintf("Dummy - Regions per dummy.png"),
       path = dirname(COM_IIS.filepath),
       plot = g2,
       scale=1, units = "in", dpi = 300,width = 10.4, height = 5.85)


summary(tbl2)
