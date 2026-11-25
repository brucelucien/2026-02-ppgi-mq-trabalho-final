# Carregamento do ecossistema tidyverse.
install.packages("tidyverse")
library(tidyverse)

# Indicação do diretório raiz dos registros.
diretorio_base <- "/Users/tamds/Google Drive/My Drive/03-TIC/03.02-doutorado/03.02.02-métodos quantitativos de pesquisa/trabalho final/Regex Golf - Instancias"
# Estabelecimento do conjunto das 50 execuções independentes (rodadas 0 a 49).
rodadas <- 0:49

# Extração do desempenho máximo por rodada, percorrendo os logs de uma rodada 'r',
# ignora os arquivos auxiliares (.shrinker.csv) para avaliar apenas a busca principal 
# e captura o maior Fitness obtido em cada uma das 15 instâncias.
processar_rodada <- function(r, base_dir) {
  pasta_rodada <- file.path(base_dir, as.character(r))
  arquivos_processo <- list.files(pasta_rodada, full.names = TRUE) %>%
    keep(~ !str_detect(basename(.x), "\\.shrinker\\.csv"))
  # map_dfr é uma espécie de loop que percorre cada caminho.
  map_dfr(arquivos_processo, function(arq) {
    #transforma "./0/Abba.csv" em "Abba"
    nome_instancia <- str_remove(basename(arq), "\\.csv")
    dados <- read_csv2(arq)
    tibble(
      Instancia      = nome_instancia,
      Rodada         = r,
      # "na.rm = TRUE" descarta valores ausentes ou inválidos (NA).
      Melhor_Fitness = max(as.numeric(dados$Fitness), na.rm = TRUE)
    )
  })
}

# executa a extração para todas as 50 rodadas, reunindo as 750 observações (15 instâncias x 50 rodadas)  
# em uma única tabela para análise estatística da distribuição.
df_figura4 <- map_dfr(rodadas, processar_rodada, base_dir = diretorio_base)

# Construção do gráfico.
grafico_fig4 <- ggplot(df_figura4, aes(x = Instancia, y = Melhor_Fitness)) +
  geom_boxplot(fill="white",color="black")+
  scale_y_continuous(
    breaks       = seq(-200, 600, by = 200),
    minor_breaks = seq(-200, 600, by = 100)
  ) +
  labs(
    x = NULL,
    y = "Fitness"
  ) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.major   = element_line(color = "grey90", linetype = "solid"),
    panel.grid.minor   = element_line(color = "grey90", linetype = "solid"),
    axis.text.x        = element_text(angle = 45, vjust = 1, hjust = 1, color = "black"),
    axis.text.y        = element_text(color = "black")
  )

# Renderiza o gráfico final na aba Plots do RStudio.
print(grafico_fig4)
