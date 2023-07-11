# Pacotes -----------------------------------------------------------------

library(readxl)
library(plotly)
library(flexdashboard)
library(zoo)
library(tidyverse)
library(scales)
library(kableExtra)
library(stringi)

# Dados -------------------------------------------------------------------

dados <- read_xlsx("dados/dados.xlsx") |> 
  mutate(hora_medicao = str_extract(hora_medicao, "[0-9]+:[0-9]+")) |> 
  mutate(diq = 1.5 * diff(quantile(valor_medicao, c(.25, .75))),
         q1_lim = quantile(valor_medicao, .25) - diq,
         q3_lim = quantile(valor_medicao, .75) + diq,
         check = valor_medicao < q1_lim | valor_medicao > q3_lim)

# Boxplot geral -----------------------------------------------------------

bp_geral <- dados |> 
  ggplot(aes(x = 1, y = valor_medicao, text = ifelse(check,
                                                      paste0("# Medição : ", n_medicao, "\n",
                                                             "# Medição no dia :", n_medicao_dia, "\n",
                                                             "Data da medição: ", data_medicao, "\n",
                                                             "Hora da medição: ", hora_medicao, "\n",
                                                             "Valor da medição: ", valor_medicao),
                                                      ""))) +
  geom_boxplot() +
  geom_point(alpha = 0) +
  xlab("") + ylab("Valor da medição") +
  labs(title = "Boxplot de todas as aferições") +
  theme(plot.title = element_text(hjust = .5))

t1 <- dados |> 
  summarise_at(vars(valor_medicao), list(mean, sd, median, min, max)) |> 
  mutate(a = "Valor da medição") |> 
  select(a, everything()) |> 
  rename_all(~c(" ", "Média", "Desvio padrão", "Mediana", "Mínimo", "Máximo"))

# Boxplot aferição dia ----------------------------------------------------

bp_afericao <- dados |> 
  mutate(n_medicao_dia = factor(n_medicao_dia)) |> 
  ggplot(aes(x = n_medicao_dia, y = valor_medicao, color = n_medicao_dia, text = ifelse(check,
                                                                                        paste0("# Medição : ", n_medicao, "\n",
                                                                                               "# Medição no dia :", n_medicao_dia, "\n",
                                                                                               "Data da medição: ", data_medicao, "\n",
                                                                                               "Hora da medição: ", hora_medicao, "\n",
                                                                                               "Valor da medição: ", valor_medicao),
                                                                                        ""))) +
  geom_boxplot() +
  geom_point(alpha = 0) +
  xlab("") + ylab("Valor da medição") +
  labs(title = "Boxplot de todas as aferições, individualmente") +
  theme(plot.title = element_text(hjust = .5), legend.position = "none")

t2 <- dados |> 
  group_by(n_medicao_dia) |> 
  summarise_at(vars(valor_medicao), list(mean, sd, median, min, max)) |> 
  mutate(n_medicao_dia = as.character(n_medicao_dia)) |> 
  rename_all(~c("Número da aferição do dia", "Média", "Desvio padrão", "Mediana", "Mínimo", "Máximo"))

# Médias móveis -----------------------------------------------------------

mm <- dados |> 
  group_by(n_medicao_dia) |> 
  transmute(MM3 = rollmean(valor_medicao, k = 3, fill = NA),
            MM5 = rollmean(valor_medicao, k = 5, fill = NA),
            MM7 = rollmean(valor_medicao, k = 7, fill = NA),
            MM15 = rollmean(valor_medicao, k = 15, fill = NA)) |> 
  rename_all(~c("n_medicao_dia", "3 dias", "5 dias", "7 dias", "15 dias")) |> 
  ungroup() |> 
  gather(mm, valor, 2:5) |> 
  group_by(n_medicao_dia, mm) |> 
  summarise(valor = mean(valor, na.rm = TRUE)) |> 
  mutate(mm = factor(mm, levels = c("3 dias", "5 dias", "7 dias", "15 dias"))) |> 
  ggplot(aes(x = mm, y = valor, fill = factor(n_medicao_dia))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = number(valor, .001)), position = position_dodge(.9), vjust = -.5) +
  xlab("Média móvel X dias") + ylab("Valor médio") +
  labs(title = "Médias móveis das aferições") +
  theme(plot.title = element_text(hjust = .5), legend.position = "bottom") +
  guides(fill = guide_legend(title = "Número da aferição no dia"))

# TS ----------------------------------------------------------------------

plot_ts <- dados |> 
  ggplot(aes(x = data_medicao, y = valor_medicao, color = factor(n_medicao_dia), text = paste0("# Medição : ", n_medicao, "\n",
                                                                                               "# Medição no dia :", n_medicao_dia, "\n",
                                                                                               "Data da medição: ", data_medicao, "\n",
                                                                                               "Hora da medição: ", hora_medicao, "\n",
                                                                                               "Valor da medição: ", number(valor_medicao)))) +
  geom_line(aes(group = n_medicao_dia)) +
  xlab("Data da aferição") + ylab("Valor da aferição") +
  labs(title = "Evolução das aferições ao longo dos dias") +
  theme(plot.title = element_text(hjust = .5), legend.position = "bottom") +
  guides(color = guide_legend(title = "Aferição no dia"))

plotly_ts <- ggplotly(plot_ts, tooltip = "text")
