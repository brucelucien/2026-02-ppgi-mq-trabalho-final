################################################################################
# QUESTÃO 3 - TABELA 11
#
# ENUNCIADO (Regex Golf - Trabalho Final.pdf):
#   3) Gerar as análises que produzem os resultados da Tabela 11 com base nos arquivos do redutor.
#   a. COUNT: número de eventos de encerramento de shrink
#   b. EFFECTIVE: número de eventos de encerramento de shrink com melhoria de qualidade
#   c. % EFFEC: EFFECTIVE dividido por COUNT
#   d. Colunas de operadores: número de vezes que os operadores foram utilizados
#
# ONDE CADA ITEM É ATENDIDO NESTE ARQUIVO:
#   Item a -> PASSO 2 (coluna 'Count')
#   Item b -> PASSO 3 (coluna 'Effective')
#   Item c -> PASSO 4 (coluna 'Pct_Effec')
#   Item d -> PASSO 5 (colunas CR, RD, SN, SR)
#   PASSO 6 junta tudo na Tabela 11 e salva em saida/tabela11.csv
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
# PASSO 1 - LER OS ARQUIVOS DO REDUTOR (ex.: abba_shrinker.csv ...)
# =====================================================================
lista <- list()

for (rodada in rodadas) {
  for (nome in instancias$instance) {
    
    # o arquivo pode se chamar "abba.shrinker.csv" ou "abba_shrinker.csv":
    # testamos os dois e usamos o que existir
    candidatos <- file.path(pasta_resultados, rodada,
                            paste0(nome, c(".shrinker.csv", "_shrinker.csv")))
    arquivo <- candidatos[file.exists(candidatos)][1]
    
    if (is.na(arquivo)) {
      message("Arquivo do redutor não encontrado para: ", nome, " (rodada ", rodada, ")")
      next
    }
    
    d <- read_delim(arquivo, delim = ";",
                    col_types = cols(.default = col_character()))
    
    d <- d %>%
      rename(evento = `Event Name`, operacao = `Operation Name`,
             resultado = `Result Is Better`) %>%
      mutate(# Conforme o enunciado: "vazio igual a false".
        # Só vira TRUE quando o texto for "true".
        melhorou  = tolower(resultado) %in% "true",
        instancia = nome,
        rodada    = rodada) %>%
      select(instancia, rodada, evento, operacao, melhorou)
    
    lista[[length(lista) + 1]] <- d
  }
}

shrink <- bind_rows(lista)

nrow(shrink)
head(shrink)

# #####################################################################
# ITEM A
# COUNT: número de eventos de encerramento de shrink.
#
# O QUE FOI FEITO PARA ATENDER O ITEM A:
#   No arquivo do redutor, o encerramento aparece como evento 'Shink finished'
#   (o erro de digitação 'Shink' é do próprio arquivo).
#   Filtramos esse evento e contamos as linhas (n) por instância e rodada.
#   Depois tiramos a média das rodadas, como na Tabela 11 do artigo.
#   Rodadas sem nenhum shrink contam como 0 (função complete).
# #####################################################################

# PASSO 2 - COUNT
encerramentos <- shrink %>%
  filter(evento == "Shink finished")          # só os eventos de encerramento

contagem_por_rodada <- encerramentos %>%
  group_by(instancia, rodada) %>%
  summarise(count = n(), .groups = "drop") %>%                 # <- nº de encerramentos
  complete(instancia = instancias$instance, rodada = rodadas,  # inclui rodadas sem eventos
           fill = list(count = 0))

count_medio <- contagem_por_rodada %>%
  group_by(instancia) %>%
  summarise(Count = mean(count), .groups = "drop")             # média das rodadas

# #####################################################################
# ITEM B
# EFFECTIVE: número de eventos de encerramento de shrink com melhoria de qualidade.
#
# O QUE FOI FEITO PARA ATENDER O ITEM B:
#   Entre os eventos 'Shink finished', ficamos só com os que têm melhorou == TRUE
#   (coluna 'Result Is Better' igual a 'true'; vazio é tratado como false, como pede o enunciado).
#   Contamos por instância e rodada e tiramos a média das rodadas.
# #####################################################################

# PASSO 3 - EFFECTIVE
efetivos_por_rodada <- encerramentos %>%
  filter(melhorou) %>%                                         # só os que melhoraram
  group_by(instancia, rodada) %>%
  summarise(efetivos = n(), .groups = "drop") %>%              # <- nº de encerramentos efetivos
  complete(instancia = instancias$instance, rodada = rodadas,
           fill = list(efetivos = 0))

effective_medio <- efetivos_por_rodada %>%
  group_by(instancia) %>%
  summarise(Effective = mean(efetivos), .groups = "drop")

# #####################################################################
# ITEM C
# % EFFEC: EFFECTIVE dividido por COUNT.
#
# O QUE FOI FEITO PARA ATENDER O ITEM C:
#   Dividimos Effective por Count e multiplicamos por 100 para ter porcentagem.
#   Se Count for 0 (instância sem nenhum shrink) o resultado fica NA.
# #####################################################################

# PASSO 4 - % EFFEC
efetividade <- count_medio %>%
  left_join(effective_medio, by = "instancia") %>%
  mutate(Pct_Effec = ifelse(Count > 0, 100 * Effective / Count, NA))   # <- Effective / Count

# #####################################################################
# ITEM D
# Colunas de operadores: número de vezes que os operadores foram utilizados.
#
# O QUE FOI FEITO PARA ATENDER O ITEM D:
#   Cada uso de um operador do redutor aparece como uma linha de evento 'Shink operation'.
#   Damos uma sigla a cada operação (CR, RD, SN, SR) e contamos as linhas por instância.
#   Aqui é a SOMA de todas as rodadas (número de vezes que o operador foi usado).
#   Usamos pivot_wider para transformar cada sigla em uma coluna.
# #####################################################################

# PASSO 5 - colunas de operadores
#   CR = Convert to repetition     RD = Remove Duplicate Values
#   SN = Simplify Negation         SR = Simplify Ranges
operadores <- shrink %>%
  filter(evento == "Shink operation") %>%
  mutate(sigla = case_when(
    str_detect(tolower(operacao), "repetition") ~ "CR",
    str_detect(tolower(operacao), "duplicate")  ~ "RD",
    str_detect(tolower(operacao), "negation")   ~ "SN",
    str_detect(tolower(operacao), "range")      ~ "SR",
    TRUE                                        ~ "OUTRO"
  ))

# Confira: nenhuma operação deveria ter ficado como "OUTRO"
count(operadores, operacao, sigla)

usos <- operadores %>%
  count(instancia, sigla) %>%                                     # <- nº de usos
  pivot_wider(names_from = sigla, values_from = n, values_fill = 0)

# se algum operador nunca foi usado, a coluna não existe: criamos com zeros
for (coluna in c("CR", "RD", "SN", "SR")) {
  if (!(coluna %in% names(usos))) {
    usos[[coluna]] <- 0
  }
}

# =====================================================================
# PASSO 6 - MONTAR A TABELA 11 FINAL E SALVAR
# =====================================================================
ordem <- instancias %>% select(instancia = instance, order)

tabela11 <- efetividade %>%                      # Count, Effective, Pct_Effec (itens a, b, c)
  left_join(usos,  by = "instancia") %>%         # CR, RD, SN, SR            (item d)
  left_join(ordem, by = "instancia") %>%
  arrange(order) %>%
  select(instancia, Count, Effective, Pct_Effec, CR, RD, SN, SR)

print(tabela11, n = 20, width = Inf)

write_delim(tabela11, file.path(pasta_saida, "tabela11.csv"), delim = ";")
