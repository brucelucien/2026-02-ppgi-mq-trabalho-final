# Trabalho Final de Métodos Quantitativos
# Questão 1
#
# 1) Gerar as análises que produzem os resultados da Tabela 10.
#
# a. Dados das instâncias, resultados de Norvig, Bartolli e humanos vindos do
# arquivo base;
#
# b. Maior resultado da busca local para cada instância calculado pelos arquivos
# de processo. Esta informação é calculada como o maior valor de fitness
# apresentado na última linha de cada arquivo da busca local de uma instância;
#
# c. Resultados médios e desvio padrão da busca local calculados pelos arquivos
# de processo. Estas duas informações são calculadas pelo valor de fitness
# apresentado na última linha de cada arquivo da busca local de uma instância;

# d. Teste estatístico comparando os resultados da busca local de cada instância
# (50 valores de fitness) aos valores de fitness produzidos pelos outros
# algoritmos (1 valor por algoritmo).
#
# Limpando ambiente
rm(list = ls())
#
# Carregando bibliotecas
if (!requireNamespace('tidyverse')) {
  install.packages('tidyverse')
}
library('tidyverse')
#
if (!requireNamespace('data.tree')) {
  install.packages('data.tree')
}
library('data.tree')
#
# Constantes
NOME_ARQUIVO_INSTANCIAS <- 'instances.csv'
ENDERECO_ARQUIVO_INSTANCIAS <-  paste('./dados/Regex Golf - Instancias/',
                                      NOME_ARQUIVO_INSTANCIAS,
                                      sep = "")
ENDERECO_DIRETORIO_DADOS <- './dados/Regex Golf - Instancias/'
EXTENSAO_CSV = '.csv'
TIPO_ARQUIVO_REDUTOR = 'REDUTOR'
TIPO_ARQUIVO_EXPERIMENTO = 'EXPERIMENTO'
NOME_EVENTO_FINAL = "Finished. The best found is here"
EVENTO_ENCONTREI_MELHOR = "Found better in current neighborhood"
EVENTO_INICIO = "Start of neighborhood"
COLUNAS = list(
  warmup = "Plain strings",
  anchors = "Anchors",
  ranges = "Ranges",
  backrefs = "Backrefs",
  abba = "Abba",
  `aman-aplan` = "A man, a plan",
  prime = "Prime",
  four = "Four",
  order = "Order",
  triples = "Triples",
  glob = "Glob",
  balance = "Balance",
  powers = "Powers",
  `long-count` = "Long count",
  alphabetical = "Alphabetical"
)
#
# Funções
util.montar_endereco_csv <- function (execucao, instancia, tipo_arquivo) {
  sufixo_redutor = ""
  if (tipo_arquivo == TIPO_ARQUIVO_REDUTOR) {
    sufixo_redutor = ".shrinker"
  }
  return(
    paste(
      ENDERECO_DIRETORIO_DADOS,
      execucao,
      '/',
      instancia,
      sufixo_redutor,
      EXTENSAO_CSV,
      sep = ""
    )
  )
}

util.obter_enderecos_csv <- function () {
  fontes_csv <- tibble(
    execucao = character(),
    instancia = character(),
    tipo_arquivo = character(),
    endereco_arquivo_csv = character()
  )
  for (execucao in execucoes) {
    for (instancia in instancias$instance) {
      for (tipo_arquivo in c(TIPO_ARQUIVO_EXPERIMENTO, TIPO_ARQUIVO_REDUTOR)) {
        endereco_csv = util.montar_endereco_csv(
          execucao = execucao,
          instancia = instancia,
          tipo_arquivo = tipo_arquivo
        )
        fontes_csv <- fontes_csv %>% add_row(
          execucao = execucao,
          instancia = instancia,
          tipo_arquivo = tipo_arquivo,
          endereco_arquivo_csv  = endereco_csv
        )
      }
    }
  }
  return(fontes_csv)
}
#
# Buscando dados de instâncias
instancias <- read_csv2(ENDERECO_ARQUIVO_INSTANCIAS)
instancias %>%
  select(instance, norvig, bartolli, humans) %>%
  arrange(instance)
#
# Buscando dados de execuções
execucoes <- tibble(list.files(ENDERECO_DIRETORIO_DADOS))
names(execucoes) = c('execucao')
execucoes <- execucoes %>%
  arrange(execucao) %>%
  filter(execucao != NOME_ARQUIVO_INSTANCIAS)
#
# Buscando endereços dos arquivos CSV
lista_csv = util.obter_enderecos_csv()
lista_csv_experimentos = lista_csv |> filter(tipo_arquivo == TIPO_ARQUIVO_EXPERIMENTO)
lista_csv_redutor = lista_csv |> filter(tipo_arquivo == TIPO_ARQUIVO_REDUTOR)
#
# Buscando dados dos arquivos de execuções
arquivos = lista_csv |> filter(tipo_arquivo == TIPO_ARQUIVO_EXPERIMENTO)
dados = tibble()
dados_evento_final = tibble()
for (i in seq_len(nrow(arquivos))) {
  arquivo = read_delim(
    arquivos$endereco_arquivo_csv[i],
    col_names = TRUE,
    show_col_types = FALSE
  )
  arquivo <- arquivo |> add_column(execucao = arquivos$execucao[i])
  eventos <- bind_cols(tibble(Instance = arquivos$instancia[i]), arquivo)
  dados <- bind_rows(dados, eventos)
  evento_final <- arquivo |> filter(`Event Name` == NOME_EVENTO_FINAL)
  evento_final <- bind_cols(tibble(Instance = arquivos$instancia[i]), evento_final)
  dados_evento_final <- bind_rows(dados_evento_final, evento_final)
}
#
# Calculando melhor resultado, média e desvio padrão
resultados_agrupados = dados_evento_final |> group_by(Instance) |> summarise(
  best_result = max(Fitness),
  media = round(mean(Fitness), digits = 1),
  desv_padrao = round(sd(Fitness), digits = 1)
)
#
# Criando tabela 10
tabela_10 = tibble(
  instance_name = character(),
  norvig_exact = integer(),
  bartoli_gp = integer(),
  humans = integer()
)
#
# Preenchendo tabela 10 com dados do arquivo de instâncias
for (i in seq_len(nrow(instancias))) {
  tabela_10 <- tabela_10 |> add_row(
    instance_name = instancias$instance[i],
    norvig_exact = instancias$norvig[i],
    bartoli_gp = instancias$bartolli[i],
    humans = instancias$humans[i]
  )
}
#
# Juntando dados do arquivo de instâncias com resultados agrupados
tabela_10 <- tabela_10 |>
  left_join(resultados_agrupados, by = join_by(instance_name == Instance)) |>
  relocate(humans, .after = desv_padrao)
#
# Ajustando nomes das colunas
tabela_10$instance_name <- sapply(tabela_10$instance_name, \(x) COLUNAS[[x]])
#
# Ordenando tabela 10
tabela_10 <- tabela_10 |> arrange(as.numeric(rownames(tabela_10)))
#
# Mostrando tabela 10
View(tabela_10)
#
# Para descobrir 'free variables'...
# codetools::findGlobals(util.montar_endereco_csv, merge = FALSE)$variables
#
# #################
# TESTE ESTATÍSTICO
# #################
#
# We performed a Wilcox-Mann-Whitney test comparing the distributions of
# fitness found by the local search to the fitness found by Norgiv’s algorithm
# and the published fitness of the genetic programming approach. All results
# were significantly different with α = 0.05, except for 'Plain Strings' (for
# which all algorithms yielded the same fitness) and 'Order' (in the comparison
# between the exact algorithm and the local search).
#
p_values_encontrados <- tibble(
  instancia = character(),
  norvig = double(),
  norvig_significativo = logical(),
  bartolli = double(),
  bartolli_significativo = logical()
)
for (nome_instancia in instancias$instance) {
  fitness_instancias = dados_evento_final |>
    filter(Instance == nome_instancia) |>
    select(Fitness) |>
    sapply(\(x) as.numeric(x))
  fitness_outros = (instancias |> filter(instance == nome_instancia) |>
                      select(norvig, bartolli))
  fitness_norvig = as.numeric(fitness_outros$norvig)
  fitness_bartolli = as.numeric(fitness_outros$bartolli)
  p_value_norvig = wilcox.test(fitness_instancias, mu = fitness_norvig)$p.value
  p_value_bartolli = wilcox.test(fitness_instancias, mu = fitness_bartolli)$p.value
  p_values_encontrados <- add_row(
    p_values_encontrados,
    instancia = nome_instancia,
    norvig = p_value_norvig,
    norvig_significativo = p_value_norvig < 0.05,
    bartolli = p_value_bartolli,
    bartolli_significativo = p_value_bartolli < 0.05
  )
}
#
# Ajustando nomes das colunas
p_values_encontrados$instancia <- sapply(p_values_encontrados$instancia, \(x) COLUNAS[[x]])
#
# Ordenando
p_values_encontrados <- p_values_encontrados |> arrange(as.numeric(rownames(p_values_encontrados)))
#
# Mostrando p-values
View(p_values_encontrados)
#
# #########
# QUESTÃO 5
# #########
#
# Calculando melhorias, por instância e por operador
melhorias_por_operador <- dados |>
  # Somente eventos de início e de melhoria
  filter(`Event Name` == EVENTO_INICIO |
           `Event Name` == EVENTO_ENCONTREI_MELHOR) |>
  # Somente colunas relevantes
  select(Instance, `Event Name`, `Operator Name`, execucao, Fitness) |>
  # Copiando fitness dos eventos de início para outra coluna
  mutate(FitnessInicio = if_else(`Event Name` == EVENTO_INICIO, Fitness, NA)) |>
  # Preenchendo as células vazias com o valor do fitness de início
  fill(FitnessInicio, .direction = 'down') |>
  # Calculando a melhoria
  mutate(melhoria = if_else(
    `Event Name` == EVENTO_ENCONTREI_MELHOR,
    Fitness - FitnessInicio,
    NA
  )) |>
  # Filtrando somente por eventos de melhoria
  filter(`Event Name` == EVENTO_ENCONTREI_MELHOR) |>
  # Deixando somente colunas relevantes
  select(Instance, `Operator Name`, execucao, melhoria) |>
  # Agrupando por execução
  group_by(Instance, `Operator Name`, execucao) |>
  # Calculando melhoria média de cada execução
  summarise(melhoria_media = mean(melhoria)) |>
  # Agrupando sem considerar execução
  group_by(Instance, `Operator Name`) |>
  # Calculando melhoria média geral (a média das médias de cada execução)
  summarise(melhoria_media = mean(melhoria_media))
#
# Ajustando nomes das instâncias
melhorias_por_operador$Instance <- sapply(melhorias_por_operador$Instance, \(x) COLUNAS[[x]])
#
# Arredondando valores de melhoria
melhorias_por_operador$melhoria_media <- round(melhorias_por_operador$melhoria_media, 1)
#
# Criando Tabela 13 com base na Tabela 10
tabela_13 <- tabela_10 |>
  select(instance_name, best_result) |>
  mutate(
    Instance = instance_name,
    Fitness = best_result,
    instance_name = NULL,
    best_result = NULL
  )
#
# Adicionando valores por operador
tabela_13 <- tabela_13 |>
  left_join(
    melhorias_por_operador |>
      filter(`Operator Name` == 'Extraction') |>
      select(melhoria_media) |>
      mutate(EXT = melhoria_media, melhoria_media = NULL),
    by = join_by(Instance == Instance),
    keep = FALSE
  ) |>
  left_join(
    melhorias_por_operador |>
      filter(`Operator Name` == 'Swap') |>
      select(melhoria_media) |>
      mutate(SW = melhoria_media, melhoria_media = NULL),
    by = join_by(Instance == Instance),
    keep = FALSE
  ) |>
  left_join(
    melhorias_por_operador |>
      filter(`Operator Name` == 'Range') |>
      select(melhoria_media) |>
      mutate(RNG = melhoria_media, melhoria_media = NULL),
    by = join_by(Instance == Instance),
    keep = FALSE
  ) |>
  left_join(
    melhorias_por_operador |>
      filter(`Operator Name` == 'Dot') |>
      select(melhoria_media) |>
      mutate(DOT = melhoria_media, melhoria_media = NULL),
    by = join_by(Instance == Instance),
    keep = FALSE
  ) |>
  left_join(
    melhorias_por_operador |>
      filter(`Operator Name` == 'Star quantifier') |>
      select(melhoria_media) |>
      mutate(STAR = melhoria_media, melhoria_media = NULL),
    by = join_by(Instance == Instance),
    keep = FALSE
  ) |>
  left_join(
    melhorias_por_operador |>
      filter(`Operator Name` == 'Concatenation') |>
      select(melhoria_media) |>
      mutate(CC = melhoria_media, melhoria_media = NULL),
    by = join_by(Instance == Instance),
    keep = FALSE
  ) |>
  left_join(
    melhorias_por_operador |>
      filter(`Operator Name` == 'Negation') |>
      select(melhoria_media) |>
      mutate(NEG = melhoria_media, melhoria_media = NULL),
    by = join_by(Instance == Instance),
    keep = FALSE
  ) |>
  left_join(
    melhorias_por_operador |>
      filter(`Operator Name` == 'Back reference') |>
      select(melhoria_media) |>
      mutate(BR = melhoria_media, melhoria_media = NULL),
    by = join_by(Instance == Instance),
    keep = FALSE
  ) |>
  relocate(Fitness, .after = last_col())
#
# Visualizando tabela 13
View(tabela_13)
