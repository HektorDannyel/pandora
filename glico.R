library(tidyverse)
library(grid)
library(gridExtra)

pandora <- tibble(data = c("27/04", rep("28/04", 3), rep("29/04", 3), rep("30/04", 3), rep("01/05", 3), rep("02/05", 3), rep("03/05", 3),
                           rep("04/05", 3)),
       hora = c("20:49", "02:32", "14:35", "21:02", "02:32", "14:35", "20:39", "02:35", "14:35", "20:08",
                "02:40", "14:30", "21:06", "02:37", "14:30", "20:32", "02:32", "14:38", "20:41", "02:35", "14:35", "21:09"),
       glic = c(114, 251, 435, 281, 468, 445, 334, 414, 366, 343, 343, 336, 285, 343, 286, 331, 346, 276, 371, 314, 240, 359),
       comeu = c(F, T, T, F, T, T, F, T, T, F, T, T, F, T, T, F, T, T, F, T, T, F)) |> 
  rowid_to_column() |> 
  mutate(data = as.Date(paste0(data, "/2023"), format = "%d/%m/%Y")) |> 
  group_by(data) |> 
  mutate(afer = row_number(),
         vl = ifelse(afer == max(afer) & !afer == 2, rowid + .5, NA)) |> 
  ungroup()

p1 <- pandora |> 
  mutate(comeu = ifelse(comeu, "Sim", "Não")) |> 
  ggplot(aes(x = factor(rowid), y = glic, group = 1)) +
  geom_line() +
  geom_point(aes(color = comeu)) +
  geom_vline(aes(xintercept = vl), alpha = .2, linewidth = 1) +
  xlab("Aferição") + ylab("Glicose") +
  labs(title = "Evolução da glicose ao longo do tempo") +
  theme(plot.title = element_text(hjust = .5), legend.position = "bottom") +
  guides(color = guide_legend(title = "Pós refeição"))

p2 <- pandora |> 
  ggplot(aes(x = factor(afer), y = glic, group = data, color = factor(data))) +
  geom_line() +
  geom_point() +
  geom_text(aes(label = format(data, format = "%d/%m")), color = "#000000") +
  xlab("Aferição") + ylab("Glicose") +
  labs(title = "Evolução da glicose ao longo do dia") +
  theme(plot.title = element_text(hjust = .5), legend.position = "bottom") +
  guides(color = guide_legend(title = "Data"))

p3 <- pandora |> 
  mutate(glic_1 = lag(glic, 1),
         evo = glic/glic_1) |> 
  ggplot(aes(x = factor(rowid), y = evo, group = 1)) +
  geom_line() + 
  geom_point() + 
  geom_vline(aes(xintercept = vl), alpha = .2) + 
  xlab("Aferição") + ylab("Evolução (%)") +
  labs(title = "Evolução proporcional da glicose ao longo do tempo") +
  theme(plot.title = element_text(hjust = .5))

pushViewport(viewport(layout = grid.layout(2, 2)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(p1, vp = vplayout(1, 1:2))
print(p2, vp = vplayout(2, 1))
print(p3, vp = vplayout(2, 2))