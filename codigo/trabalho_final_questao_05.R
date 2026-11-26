################################################################################
# QUESTÃO 5 - TABELA 13
#
# ENUNCIADO (Regex Golf - Trabalho Final.pdf):
#   5) Gerar as análises que produzem os resultados da Tabela 13 com base nos arquivos do processo.
#   As colunas de operadores indicam o ganho de fitness provocado por cada operador, com base na
#   diferença entre o fitness registrado depois da aplicação do operador em eventos do tipo
#   "Found better in current neighborhood" e o fitness registrado no último evento
#   "Start of neighborhood" anterior ao evento que provocou a melhoria.
#
# ONDE CADA ITEM É ATENDIDO NESTE ARQUIVO:
#   Fitness do último 'Start of neighborhood' anterior -> PASSO 2 (coluna fit_inicio)
#   Fitness depois do operador em 'Found better...' e diferença (ganho) -> PASSO 2 (coluna ganho)
#   Ganho de fitness provocado por cada operador (média) -> PASSOS 3 e 4
#   Montagem e gravação -> PASSOS 5 e 6; salva em saida/tabela13.csv
#
################################################################################

# =====================================================================
# PASSO 0 - CONFIGURAÇÃO (caminhos, rodadas e pacotes)
# =====================================================================
# ---- CAMINHOS DOS SEUS ARQUIVOS (só mexa aqui se mudar as pastas de lugar) ----
# Pasta com o instances.csv e as pastas das rodadas (0, 1, 2, ... 49)
diretorio <- "/Users/brenerfa/Downloads/drive-download-20261117T175652Z-1-001/Regex Golf - Instancias"

pasta_resultados   <- diretorio                              # pasta com as rodadas
arquivo_instancias <- file.path(diretorio, "instances.csv")  # dados das instâncias

# Onde os resultados (tabelas .csv e figura) serão salvos: dentro do repositório
pasta_saida <- "/Users/brenerfa/Documents/GitHub/2026-02-ppgi-mq-trabalho-final/saida"

# Verificações com mensagens claras (se algo estiver errado, o R avisa aqui)
if (!dir.exists(diretorio)) {
  stop("A pasta não existe: ", diretorio,
       "\n=> Confira o caminho em 'diretorio' (no Console: list.files('/Users/brenerfa/Downloads')).")
}
if (!file.exists(arquivo_instancias)) {
  stop("Não achei o instances.csv em: ", diretorio,
       "\n=> Veja o que existe lá com list.files(diretorio).")
}

# Quais rodadas ler? 0:49 = as 50 rodadas.
# DICA: para testar rápido, troque por  rodadas <- 0  (só a rodada 0)
rodadas <- 0:49

# Carrega o tidyverse (se der erro, rode uma vez: install.packages("tidyverse"))
library(tidyverse)

# Cria a pasta de saída (se ela ainda não existir)
dir.create(pasta_saida, showWarnings = FALSE, recursive = TRUE)

# Lê o arquivo das instâncias (o separador é ponto e vírgula)
instancias <- read_delim(arquivo_instancias, delim = ";", show_col_types = FALSE)
# Verifica se a primeira rodada existe
if (!dir.exists(file.path(pasta_resultados, rodadas[1]))) {
  stop("Não achei a pasta da rodada ", rodadas[1], " em: ",
       normalizePath(pasta_resultados),
       "\n=> Confira se existem as pastas 0, 1, 2, ... dentro dela.")
}

head(instancias)   # confira se apareceram as colunas instance, matchlist, ...

# =====================================================================
# PASSO 1 - LER OS ARQUIVOS DE PROCESSO (ex.: abba.csv, glob.csv ...)
# =====================================================================
# Lemos um arquivo por vez (cada rodada x cada instância), guardamos numa
# lista e, no final, juntamos tudo em uma única tabela chamada "processo".

lista <- list()

for (rodada in rodadas) {
  for (nome in instancias$instance) {
    
    # monta o caminho, por exemplo: resultados/0/abba.csv
    arquivo <- file.path(pasta_resultados, rodada, paste0(nome, ".csv"))
    
    # se o arquivo não existir, avisa e passa para o próximo
    if (!file.exists(arquivo)) {
      message("Arquivo não encontrado: ", arquivo)
      next
    }
    
    # lê tudo como texto (evita problemas de tipo ao juntar os arquivos)
    d <- read_delim(arquivo, delim = ";",
                    col_types = cols(.default = col_character()))
    
    d <- d %>%
      # os nomes entre crases `` são necessários porque têm espaço
      rename(evento = `Event Name`, operador = `Operator Name`, fitness = Fitness) %>%
      mutate(fitness   = as.numeric(fitness),   # texto -> número
             linha     = row_number(),          # posição da linha no arquivo
             instancia = nome,                  # de que instância veio
             rodada    = rodada) %>%            # de que rodada veio
      select(instancia, rodada, linha, evento, operador, fitness)
    
    lista[[length(lista) + 1]] <- d
  }
}

processo <- bind_rows(lista)   # junta todas as tabelas em uma só

# Confira: quantas linhas foram lidas e como é o início da tabela
nrow(processo)
head(processo)

# #####################################################################
# ITEM (parte 1 do enunciado)
# Fitness registrado no último evento "Start of neighborhood" anterior ao evento que provocou a melhoria.
#
# O QUE FOI FEITO PARA ATENDER O ITEM (parte 1 do enunciado):
#   Dentro de cada arquivo, copiamos o fitness para a coluna 'fit_inicio' só nas linhas
#   'Start of neighborhood' (as outras ficam NA). Depois, fill(.direction = "down") repete
#   esse valor para as linhas seguintes. Assim, em qualquer linha, 'fit_inicio' guarda o fitness
#   do último Start anterior.
# #####################################################################

# PASSO 2a - fitness do último "Start of neighborhood" anterior
com_inicio <- processo %>%
  group_by(instancia, rodada) %>%             # trata cada arquivo separadamente
  arrange(linha, .by_group = TRUE) %>%
  mutate(fit_inicio = ifelse(evento == "Start of neighborhood", fitness, NA)) %>%
  fill(fit_inicio, .direction = "down") %>%   # repete o último valor conhecido
  ungroup()

# #####################################################################
# ITEM (parte 2 do enunciado)
# Fitness depois da aplicação do operador em eventos "Found better in current neighborhood" e ganho = diferença entre os dois.
#
# O QUE FOI FEITO PARA ATENDER O ITEM (parte 2 do enunciado):
#   Filtramos só os eventos 'Found better in current neighborhood'. O fitness dessa linha é o
#   valor DEPOIS do operador. O ganho é:  ganho = fitness - fit_inicio.
# #####################################################################

# PASSO 2b - ganho de cada melhoria
melhorias <- com_inicio %>%
  filter(evento == "Found better in current neighborhood") %>%
  mutate(ganho = fitness - fit_inicio)         # <- ganho de fitness

head(melhorias)

# #####################################################################
# ITEM (parte 3 do enunciado)
# As colunas de operadores indicam o ganho de fitness provocado por cada operador.
#
# O QUE FOI FEITO PARA ATENDER O ITEM (parte 3 do enunciado):
#   Para cada instância e operador calculamos a MÉDIA dos ganhos (mean). Mostramos os 8
#   operadores mais frequentes (os mesmos da Tabela 12). Combinações que nunca ocorreram
#   ficam vazias (NA), gravadas como '-' no arquivo. A última coluna 'Fitness' é o melhor
#   fitness da busca local da instância (mesmo valor do item 1b).
# #####################################################################

# PASSO 3 - os 8 operadores mais frequentes (mesmos da Tabela 12)
ranking <- melhorias %>%
  count(operador, sort = TRUE)     # sort = TRUE ordena do maior para o menor

top8 <- head(ranking$operador, 8)

# PASSO 4 - ganho médio por instância e operador
ganhos <- melhorias %>%
  filter(operador %in% top8) %>%
  group_by(instancia, operador) %>%
  summarise(ganho_medio = round(mean(ganho, na.rm = TRUE), 1), .groups = "drop") %>%
  pivot_wider(names_from = operador, values_from = ganho_medio)   # vazio = NA

# se algum dos 8 operadores nunca aparecer, cria a coluna vazia
for (coluna in top8) {
  if (!(coluna %in% names(ganhos))) {
    ganhos[[coluna]] <- NA_real_
  }
}

# PASSO 5 - melhor fitness da busca local (última linha de cada arquivo; maior das rodadas)
melhor_fitness <- processo %>%
  group_by(instancia, rodada) %>%
  arrange(linha, .by_group = TRUE) %>%
  summarise(final = last(fitness), .groups = "drop") %>%
  group_by(instancia) %>%
  summarise(Fitness = max(final), .groups = "drop")

# PASSO 6 - juntar, ordenar e salvar
ordem <- instancias %>% select(instancia = instance, order)

tabela13 <- ganhos %>%
  left_join(melhor_fitness, by = "instancia") %>%
  left_join(ordem, by = "instancia") %>%
  arrange(order) %>%
  select(instancia, all_of(top8), Fitness)

print(tabela13, n = 20, width = Inf)

write_delim(tabela13, file.path(pasta_saida, "tabela13.csv"), delim = ";", na = "-")
