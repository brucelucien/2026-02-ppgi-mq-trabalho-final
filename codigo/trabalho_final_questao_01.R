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
ENDERECO_ARQUIVO_INSTANCIAS <-  paste('./dados/Regex Golf - Instancias/', NOME_ARQUIVO_INSTANCIAS, sep = "")
ENDERECO_DIRETORIO_DADOS <- './dados/Regex Golf - Instancias/'
EXTENSAO_CSV = '.csv'
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
# Buscando dados coletados
dados_coletados <- tibble(execucao=character(), instancia=character(), resultado=character())
for (execucao in execucoes) {
  for (instancia in instancias$instance) {
    endereco_csv = paste(ENDERECO_DIRETORIO_DADOS, execucao, '/', instancia,EXTENSAO_CSV, sep="")
    print(endereco_csv)
    resultado_execucao = read_csv2(endereco_csv)
    # for(resultado in resultado_execucao$Fitness) {
    #   dados_coletados <- dados_coletados %>% add_row(execucao = execucao, instancia=instancia, resultado=as.character(resultado))
    #   stop('a')
    # }
    # stop('b')
  }
  stop('c')
}
dados_coletados
