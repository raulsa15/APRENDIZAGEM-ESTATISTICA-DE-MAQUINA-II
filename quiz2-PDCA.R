library(tidyverse)
library(readxl)
library(stringi)
library(factoextra)
library(ggrepel)

dados <- read_xlsx("produtividade_policial.xlsx")

dados <- dados %>% 
  rename_with(~ stri_trans_general(.x, "Latin-ASCII") %>% 
                tolower())

names(dados)

dados %>% select(regiao, ocorrencia, total)

?pivot_wider

dados<-dados %>% select(regiao, ocorrencia, total) %>% 
  pivot_wider(names_from = ocorrencia,
              values_from =total)

dados

# PCA ---------------------------------------------------------------------

pca <- dados %>%
  select(-regiao) %>% 
  prcomp(scale = TRUE) # aplica PCA


# o grafico abaixo mostra o percentual explicado da variancia de cada componente
fviz_eig(pca, addlabels = TRUE, 
         ncp = nrow(dados$regiao)) + # ncp - numero de componentes mostrados
  labs(x = "Componente Principal",
       y = "Percentual explicado da variância")

Phi <- pca$rotation # matriz de cargas

Z <- pca$x 

colnames(Z) <- paste0("driver_", 1:ncol(Z))


# contribuicoes das questoes para a primeira componente
pca %>% 
  fviz_contrib(choice = "var", axes = 1, sort.val = "asc",
               fill = "steelblue", color = "black") +
  labs(x = "", title = "Contribuições das ocorrência para o primeiro driver") +
  coord_flip()

# contribuicoes das questoes para a segunda componente
pca %>% 
  fviz_contrib(choice = "var", axes = 2, sort.val = "asc",
               fill = "steelblue", color = "black") +
  labs(x = "", title = "Contribuições das ocorrência para o primeiro driver") +
  coord_flip()

# a funcao abaixo obtem o valor das cargas em uma componente principal (driver)
# especifico, faz um merge com as questoes e devolve um tibble ordenado
# segundo a  contribuicao de cada questao
get_driver <- function(Phi, dados, drv, top) {
  tibble(numero = rownames(Phi), 
         carga = Phi[, drv]) %>%
    mutate(contribuicao = carga^2 / sum(carga^2)) %>%
    arrange(desc(contribuicao)) %>%
    top_n(top)
}

# obtem as 6 perguntas que mais contribuiram para a primeira componente principal
driver_1 <- get_driver(Phi, dados, drv = 1, top = 6)
driver_1

(driver_1 <- get_driver(Phi, dados, drv = 1, top = 6)) # qual nome podemos dar a este driver?

(driver_2 <- get_driver(Phi, dados, drv = 2, top = 10)) # qual nome podemos dar a este driver?


# o grafico de dispersao abaixo dados obtiddos com o PCA


dados %>% select(regiao) %>% 
  bind_cols(Z) %>% 
  ggplot(aes(driver_1, driver_2, label=regiao)) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha=0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha=0.5) +
  geom_point() + 
  geom_text_repel() +
  labs(y= "driver_2 - ocorrências", x = "driver_1 - prisões") +
  labs(title = "Projecao PCA")

# tibble com todas as avaliacoes: a respectiva regiao avaliada e as cargas 
# correspondentes nas componentes principais (apenas as 2 primeiras)
tb <- tibble(regiao = dados$regiao) %>%
  bind_cols(as_tibble(Z[,1:2]))

tb %>%
  group_by(regiao) %>% # agrupa pela marca
  summarise_all(mean) %>% # obtem a media do score de todas avaliacoes
  pivot_longer(-regiao, 
               names_to = "driver", 
               values_to = "score_medio") %>% 
  ggplot(aes(x = driver, y = score_medio, 
             group = regiao, color = regiao,
             label = ifelse(driver == "driver_1", 
                            regiao, ""))) +
  geom_line(size = 1, alpha = 0.55) +
  geom_point(size = 2) +
  labs(x = "", y = "Score Médio", 
       title = "Posicionamento dos estados mais violentos") +
  geom_label_repel(direction = "both") +
  scale_x_discrete(labels = c("driver_1 - prisões", 
                              "driver_2 - ocorrências")) +
  theme_classic() + 
  theme(legend.position = "none")
