# Carregamento do ecossistema tidyverse.
install.packages("tidyverse")
library(tidyverse)

# Indicação do diretório raiz dos registros.
diretorio_base <- "/Users/tamds/Google Drive/My Drive/03-TIC/03.02-doutorado/03.02.02-métodos quantitativos de pesquisa/trabalho final/Regex Golf - Instancias"

# Importa a lista oficial das 15 instâncias com sua ordem de apresentação no artigo.
caminho_instances <- "/Users/tamds/Google Drive/My Drive/03-TIC/03.02-doutorado/03.02.02-métodos quantitativos de pesquisa/trabalho final/Regex Golf - Instancias/instances.csv"
dados_instancias <- read_csv2(caminho_instances) %>%
  select(instance,order)
# Estabelecimento do conjunto das 50 execuções independentes (rodadas 0 a 49).
rodadas <- 0:49

# Cruza as 50 rodadas com as 15 instâncias para montar os caminhos completos dos 
# arquivos '<instancia>.csv', desconsiderando os arquivos auxiliares '.shrinker.csv'.
grade_arquivos <- expand_grid( rodada = rodadas, instance = dados_instancias$instance ) %>%
  mutate(caminho = file.path(diretorio_base, as.character(rodada), paste0(instance, ".csv")))

# Lê cada log e filtra somente as aplicações de operadores que efetivamente melhoraram
# a solução ("Found better in current neighborhood").
ler_melhorias_processo <- function(caminho_arquivo, nome_instancia, num_rodada) {
  read_csv2(caminho_arquivo, col_select = c("Event Name", "Operator Name")) %>%
    filter(`Event Name` == "Found better in current neighborhood") %>%
    mutate(instance = nome_instancia, rodada = num_rodada)
}

# Aplica a função "ler_melhorias_processo" para cada combinação de arquivo, instância 
# e rodada, empilhando os eventos de melhoria em um único tibble.
eventos_melhoria <- pmap_dfr(
  list(
    caminho_arquivo = grade_arquivos$caminho,
    nome_instancia  = grade_arquivos$instance,
    num_rodada      = grade_arquivos$rodada
  ),
  ler_melhorias_processo
)

# Traduz os nomes dos 12 operadores de vizinhança para as siglas oficiais.
eventos_padronizados <- mutate(eventos_melhoria,
                               Operador = case_when(
                                `Operator Name` == "Extraction"      ~ "EXT",
                                `Operator Name` == "Swap"            ~ "SW",
                                `Operator Name` == "Range"           ~ "RNG",
                                `Operator Name` == "Dot"             ~ "DOT",
                                `Operator Name` == "Star quantifier" ~ "STAR",
                                `Operator Name` == "Concatenation"   ~ "CC",
                                `Operator Name` == "Negation"        ~ "NEG",
                                `Operator Name` == "Back reference"  ~ "BR",
                                `Operator Name` == "Plus quantifier" ~ "PLUS", 
                                `Operator Name` == "Start anchor"    ~ "START",
                                `Operator Name` == "Optional"        ~ "OPT",
                                `Operator Name` == "End anchor"      ~ "END"
                               )
                              )

# Conta as aplicações bem-sucedidas ao longo das 50 rodadas por instância e operador.
contagem_por_instancia <- count(eventos_padronizados, instance, Operador, name = "qtd_aplicacoes")
# Calcula o total geral do experimento (denominador dos percentuais).
total_geral_todos_operadores <- sum(contagem_por_instancia$qtd_aplicacoes)
# Fixa as colunas do artigo.
todos_operadores <- c("EXT","SW","RNG","DOT","STAR","CC","NEG","BR","PLUS","START","OPT","END")

# Estrutura matricialmente por instância, ordenando os operadores como colunas,
# ordena as 15 instâncias na sequência oficial e aplica os rótulos formatados da Tabela 12.
tabela_12_instancias <- pivot_wider(contagem_por_instancia,
                                     names_from  = Operador,
                                     values_from = qtd_aplicacoes,
                                     values_fill = 0) %>%
  right_join(dados_instancias, by = "instance") %>%
  arrange(order) %>%
  mutate(Instance = case_when(instance == "warmup"       ~ "Plain strings",
                              instance == "anchors"      ~ "Anchors",
                              instance == "ranges"       ~ "Ranges",
                              instance == "backrefs"     ~ "Backrefs",
                              instance == "abba"         ~ "Abba",
                              instance == "aman-aplan"   ~ "A man, a plan",
                              instance == "prime"        ~ "Prime",
                              instance == "four"         ~ "Four",
                              instance == "order"        ~ "Order",
                              instance == "triples"      ~ "Triples",
                              instance == "glob"         ~ "Glob",
                              instance == "balance"      ~ "Balance",
                              instance == "powers"       ~ "Powers",
                              instance == "long-count"   ~ "Long count",
                              instance == "alphabetical" ~ "Alphabetical"
                            )
         ) %>%
  select(Instance, all_of(todos_operadores))

# Totaliza cada operador verticalmente.
linha_total <- summarise(tabela_12_instancias,
                         Instance = "Total",
                         across(all_of(todos_operadores), sum)
                        )
# Calcula a participação percentual.
linha_percentual <- mutate(linha_total,
                           Instance = "Total (%)",
                           across(all_of(todos_operadores), ~ round((.x / total_geral_todos_operadores) * 100, 2))
                          )
# Empilha as 15 instâncias com as linhas de rodapé.
tabela_12_final <- bind_rows(
  tabela_12_instancias,
  linha_total,
  linha_percentual
)

cat("======================================================================\n")
cat("TABELA 12 COMPLETA - TODOS OS 12 OPERADORES DE VIZINHANÇA (50 RODADAS)\n")
cat("======================================================================\n")
print(as.data.frame(tabela_12_final), row.names = FALSE)

arquivo_saida <- file.path(diretorio_base, "resultado_item4_tabela12.csv")
write_csv2(tabela_12_final, arquivo_saida)
cat("\nArquivo salvo em:", arquivo_saida, "\n")