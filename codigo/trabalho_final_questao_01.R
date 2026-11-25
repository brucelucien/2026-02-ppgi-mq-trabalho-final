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
rm(list = ls())
#
# Carregando bibliotecas
if (!requireNamespace('tidyverse')) {
  install.packages('tidyverse')
}
library('tidyverse')
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
for (i in seq_len(nrow(arquivos))) {
  arquivo = read_delim(
    arquivos$endereco_arquivo_csv[i],
    col_names = TRUE,
    show_col_types = FALSE
  )
  evento_final <- arquivo |> filter(`Event Name` == NOME_EVENTO_FINAL)
  evento_final <- bind_cols(tibble(Instance = arquivos$instancia[i]), evento_final)
  dados = bind_rows(dados, evento_final)
}
#
# Calculando melhor resultado, média e desvio padrão
resultados_agrupados = dados |> group_by(Instance) |> summarise(
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
# Mostrando tabela 10
View(tabela_10 |> arrange(as.numeric(rownames(tabela_10))))
#
# Para descobrir 'free variables'...
# codetools::findGlobals(util.montar_endereco_csv, merge = FALSE)$variables
