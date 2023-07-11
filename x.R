library(magick)
library(grid)
library(tidyverse)
library(plotly)

ufpr <- image_read("img/ufpr.png")
ufpr_r <- as.raster(ufpr)

volvo <- image_read("img/volvo.png")
volvo_r <- as.raster(volvo)

renault <- image_read("img/renault.png")
renault_r <- as.raster(renault)

ts <- image_read("img/ts.png")
ts_r <- as.raster(ts)

junto <- image_read("img/junto.png")
junto_r <- as.raster(junto)

dh <- image_read("img/dh.jpg")
dh_r <- as.raster(dh)

dados <- tibble(
  passagem = c("UFPR", "TCC", "Volvo Trucks", "Renault", "TechSolution", "Junto Seguros A", "Junto Seguros S", "dunnhumby"),
  linha = factor(c(1, 2, 3, 4, 4, 5, 5, 6)),
  inicio = c(2012, 2018, (2015 + 10/12), (2017 + 8/12), 2019.01, (2019 + 10/12), 2021.01, (2021 + 10/12)),
  fim = c(2019, 2019, (2017 + 8/12), 2018.99, (2019 + 8/12), 2020.99, (2021 + 10/12), 2025),
  desc = c("Graduação em Estatística pela Universidade Federal do Paraná, curso com alto foco em modelagem.",
           "Trabalho de Conclusão de Curso. Título: Engenharia de características para a precificação residencial em Curitiba. O trabalho foi uma aplicação de NLP, utilizando anúncios imobiliários para modelar o preço dos imóveis.",
           "Estágio na Volvo Trucks, área de Aftermarket. Principais projetos: automatização de relatórios, análises de confiabilidade dos componentes dos caminhões.",
           "Estágio na Renault, área de Qualidade do Cliente. Principais projetos: utilização de Random Forest + NLP para otimizar a detecção de incidentes graves, HUB Internacional de Ciência de Dados",
           "Estatístico na TechSolution, alocado na área de Qualidade do Cliente na Renault. Principais projetos: continuação dos projetos iniciados na época de Estágio.",
           "Estatístico na Junto Seguros, Departamento Atuarial. Principais projetos: reestruturação completa da área, automatização de processos atuariais.",
           "Cientista de Dados na Junto Seguros, Squad Advanced Analytics. Principais projetos: Modelo de Gradient Boosting para expansão de carteira de crédito, modelo GAMLSS (Binomial Zero-Inflacionada) para detecção de sinistros.",
           "Cientista de Dados na dunnhumby, Departamento de Applied Data Science. Principais projetos: Mensuração da eficácia de métricas de alocação de ofertas personalizadas.")
) |> 
  gather(tempo, ano, 3:4) |> 
  mutate(desc = str_wrap(desc, width = 50))

g <- dados |> 
  ggplot(aes(x = ano, y = linha, color = passagem, text = desc)) +
  geom_line(aes(group = linha), size = 2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("UFPR" = "#264470",
                                "TCC" = "#264470",
                                "Renault" = "#fdb515",
                                "Volvo Trucks" = "#14205b",
                                "TechSolution" = "#0b069b",
                                "Junto Seguros A" = "#9869ff",
                                "Junto Seguros S" = "#32cccc",
                                "dunnhumby" = "#291b6a")) +
  theme(legend.position = "none",
        axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
  xlab("Linha do tempo") + ylab("") +
  scale_x_continuous(breaks = seq(2012, 2022, 1)) +
  coord_cartesian(xlim = c(2012, 2022), ylim = c(1, 6.5)) +
  annotation_raster(ufpr_r, 2012, 2012.6, 1.15, 1.55) +
  annotation_raster(ufpr_r, 2018, 2018.6, 2.15, 2.55) +
  annotation_raster(volvo_r, (2015 + 10/12), 2016.3, 3.1, 3.5) +
  annotation_raster(renault_r, (2017 + 8/12), 2018, 4.1, 4.5) +
  annotation_raster(ts_r, 2019, 2019.5, 4.2, 4.4) +
  annotation_raster(junto_r, (2019 + 10/12), 2020.5, 5.1, 5.5) +
  annotation_raster(dh_r, (2021 + 10/12), 2022.2, 6.1, 6.5)

dados1 <- tibble(
  x = c("R", "Modelagem\npreditiva", "Shiny", "Text\nMining", "Machine\nLearning", "SQL", "Python", "Git"),
  y = c(5, 5, 4.5, 4.5, 4.5, 4, 4, 4),
  text = c("dplyr, ggplot2, tibble,\ntidyr, readr, purr,\nstringr, forcats, gamlss,\ntidymodels, caret, broom...",
            "LM, GLM, GAM, Séries Temporais,\nDados Longitudinais, GAMLSS, Dados Multivariados",
            "Dashboards, relatórios dinâmicos",
            "Expressões regulares, tokenização,\nanálise de sinônimos, análise de sentimentos...",
            "Random Forest, Gradient Boosting,\nNLP, Clusterização, KNN,\nSVM, Naïve Bayes...",
            "MySQL, PostgreSQL, SQL Server",
            "pandas, numpy, scikit learn,\n pyspark, seaborn...",
            "GitHub, GitLab")
) |> 
  mutate(x = factor(x, levels = x))

h <- dados1 |> 
  ggplot(aes(x = x, y = y, fill = x, text = text)) +
  geom_bar(stat = "identity") +
  theme(legend.position = "none") +
  xlab("Principais habilidades") + ylab("")
