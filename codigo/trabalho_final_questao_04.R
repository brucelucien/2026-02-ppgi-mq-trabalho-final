# ==============================================================================
# Exercício de replicação de análise estatística
# 4) Gerar as análises que produzem os resultados da Tabela 12 com base nos arquivos 
# do processo. As colunas de operadores indicam o número de vezes que os operadores 
# foram utilizados nas linhas que o evento é “Found better in current neighborhood”.
# ==============================================================================

# ==============================================================================
# O Tidyverse é um conjunto de pacotes para a linguagem R projetado para ajudar 
# o desenvolvedor a importar, organizar, manipular, visualizar e comunicar 
# informações tabulares.
# ==============================================================================
install.packages("tidyverse")

# ==============================================================================
# carrega o pacote Tidyverse para a memória do R, tornando suas funções  
# disponíveis para utilização no ambiente
# ==============================================================================
library(tidyverse)

# ==============================================================================
# local onde estão as 50 pastas das rodadas
# ==============================================================================
diretorio_base <- "/Users/tamds/Google Drive/My Drive/03-TIC/03.02-doutorado/03.02.02-métodos quantitativos de pesquisa/trabalho final/Regex Golf - Instancias"

# ==============================================================================
# local onde está o arquivo instances.csv
# ==============================================================================
caminho_instances <- "/Users/tamds/Google Drive/My Drive/03-TIC/03.02-doutorado/03.02.02-métodos quantitativos de pesquisa/trabalho final/Regex Golf - Instancias/instances.csv"

# ==============================================================================
# Leitura do arquivo base e mapeamento dos nomes das instâncias.
# 
# A função read_csv2() lê arquivos de texto delimitados .csv, reconhecendo o 
# ponto e vírgula (;) como o caractere delimitador que separa cada coluna do arquivo.
# 
# O read_csv2() entrega o tibble resultante para o operador pipe (%>%), que o injeta 
# como entrada no comando seguinte "select(Instance, Order)".
# ==============================================================================
dados_instancias <- read_csv2(caminho_instances) %>%
  
  # ==============================================================================
  # O operador pipe %>% inclui o tibble resultante do "read_csv2" como o primeiro 
  # argumento implícito no "select()", bastando informar os nomes das colunas sem 
  # precisar repetir o nome da tabela ou usar cifrão ($).
  # 
  # A função "select(instance,order)" realiza uma seleção de colunas, mantendo 
  # no tibble resultante apenas as colunas Instance e Order e descartando todas 
  # as demais. 
  # 
  # O arquivo instances.csv possui originalmente 9 colunas. Como a replicação da
  # Tabela 12 precisa apenas do nome oficial de cada instância (instance) e da sua 
  # posição de exibição (order), o select() reduz o tibble de 15x9 para 15x2, deixando 
  # apenas o necessário para o cruzamento de dados nas etapas seguintes.
  # ==============================================================================
  select(instance,order)

# ==============================================================================
# Cria um vetor numérico de 50 elementos contendo a sequência de números 
# inteiros de 0 até 49 (incrementados de 1 em 1).
# Esse vetor representa os nomes dos 50 diretórios do experimento.
# ==============================================================================
rodadas <- 0:49

# ==============================================================================
# Esse bloco constrói um tibble contendo o endereço completo de todos os arquivos 
# do experimento (50 rodadas x 15 instâncias). Na prática, ele substitui os laços 
# "for" aninhados do R tradicional por uma estrutura vetorial pronta para ser percorrida 
# de uma só vez na etapa de leitura.
# 
# A função expand_grid() gera um tibble contendo todas as combinações possíveis, 
# o produto cartesiano, entre os vetores ou tabelas informados como argumentos.
# Como rodadas possui 50 elementos (0 a 49) e dados_instancias$instance possui 15 
# nomes de instâncias, ela gera automaticamente todas as 50 x 15 = 750 combinações 
# possíveis em um tibble de duas colunas (rodada e instance).
# 
# O operador pipe %>% encaminha o tibble do expand_grid() como primeiro argumento 
# implícito para a função mutate().
# ==============================================================================
grade_arquivos <- expand_grid( rodada = rodadas, instance = dados_instancias$instance ) %>%
  
  # ==============================================================================
  # A função mutate() adiciona uma terceira coluna chamada caminho ao tibble de 750 
  # linhas vindo do expand_grid(), montando de forma vetorizada o endereço completo 
  # para cada arquivo CSV do experimento. Como o comando está encadeado após o pipe 
  # %>% , recebe automaticamente a tabela com rodada e instance como entrada e anexa 
  # caminho à direita delas.
  # 
  # A função file.path() une pedaços de texto para formar um endereço de arquivo. Ela 
  # insere automaticamente o separador de pastas adequado (/ no macOS e Linux, 
  # ou \ no Windows), evitando erros de barra dupla (//) ou barra faltando ao concatenar caminhos.
  # 
  # O argumento as.character(rodada) converte os números inteiros da coluna rodada (0 a 49) 
  # para o tipo texto ("0" a "49"), para que o nome da subpasta seja manipulado como string.
  # 
  # A função "paste0(instance, ".csv")" cola textos, para cada linha, ela pega o texto 
  # da coluna instance e anexa a extensão ".csv".
  # ==============================================================================
  mutate(caminho = file.path(diretorio_base, as.character(rodada), paste0(instance, ".csv")))

# ==============================================================================
# Declaração da função "ler_melhorias_processo", responsável por ler um arquivo 
# de processo e filtrar apenas os eventos em que houve melhoria na vizinhança atual 
# ("Found better in current neighborhood").
#
# A função recebe 3 parâmetros obrigatórios: o caminho físico do CSV, o nome da 
# instância e o número da rodada, retornando um tibble filtrado com as ocorrências 
# válidas daquele arquivo.
# ==============================================================================
ler_melhorias_processo <- function(caminho_arquivo, nome_instancia, num_rodada) {

  # ==============================================================================
  # Lê apenas as colunas necessárias para o Objetivo 4.
  # 
  # A função read_csv2() lê arquivos de texto delimitados .csv, reconhecendo o ponto 
  # e vírgula (;) como o caractere delimitador que separa cada coluna do arquivo.
  # 
  # A função c() cria um vetor de texto com os nomes de duas colunas utilizadas 
  # do arquivo de processo ("Event Name" e "Operator Name"), passado ao parâmetro 
  # col_select para ignorar as demais colunas durante a importação.
  # ==============================================================================
  read_csv2(caminho_arquivo, col_select = c("Event Name", "Operator Name")) %>%

  # ==============================================================================
  # Filtra as linhas de melhoria.
  # 
  # O primeiro operador pipe %>% encaminha o tibble resultante da função read_csv2()  
  # como o primeiro argumento implícito para a função filter(), dispensando a repetição 
  # do nome da tabela ou do operador $.
  # 
  # A função "filter(...)" está selecionando apenas as linhas em que a coluna 
  # "Event Name" seja "Found better in current neighborhood".
  # ==============================================================================
  filter(`Event Name` == "Found better in current neighborhood") %>%
  
  # ==============================================================================   
  # Anexa os identificadores da instância e rodada.
  # 
  # O segundo pipe %>% pega o subconjunto reduzido de linhas devolvido pelo filter() 
  # e o encaminha diretamente como entrada para a função mutate().
  # 
  # A função mutate(...) cria novas colunas ao final do tibble. O parâmetro 
  # instance = nome_instancia cria uma nova coluna chamada instance e a preenche 
  # com o valor recebido no parâmetro nome_instancia. O parâmetro rodada = num_rodada
  # cria uma nova coluna chamada rodada e replica o número inteiro recebido em num_rodada.
  # 
  # O resultado de todo o encadeamento (read_csv2() -> filter() -> mutate()) é entregue  
  # diretamente como o retorno da função ler_melhorias_processo().
  # ==============================================================================
  mutate(instance = nome_instancia, rodada = num_rodada)
}

# ==============================================================================
# Leitura em lote dos 750 arquivos e empilhamento em um único tibble, substituindo 
# loop tradicional de "for" aninhados.
# 
# A função pmap_dfr(.l, .f) resume 3 ações realizadas de uma única vez:
#  - p (parallel): Percorre múltiplos vetores de entrada em paralelo, linha por 
#    linha (índice 1 com índice 1, índice 2 com índice 2, até o índice 750).
#  - map: Aplica repetidamente uma mesma função (.f) sobre cada conjunto de elementos 
#    extraídos de cada posição.
#  - _dfr (data frame row-binding): Empilha automaticamente pelas linhas (row-bind) 
#    os 750 pequenos tibbles devolvidos por cada chamada da função em um único tibble.
# 
# A função list() agrupa vetores sob uma única estrutura, e aqui está sendo reunido
# 3 vetores de 750 posições extraídos de grade_arquivos pelo operador $. Os nomes 
# definidos à esquerda do sinal = (caminho_arquivo, nome_instancia e num_rodada) 
# correspondem aos nomes dos três parâmetros declarados na assinatura de ler_melhorias_processo(), 
# garantindo o casamento dos argumentos a cada iteração.
# 
# A função ler_melhorias_processo é passada como objeto (sem parênteses no final, 
# pois ela não está sendo executada ali, mas sim entregue ao pmap_dfr() para que 
# ele a invoque 750 vezes internamente).
# ==============================================================================
eventos_melhoria <- pmap_dfr(
 list(
  caminho_arquivo = grade_arquivos$caminho,
  nome_instancia  = grade_arquivos$instance,
  num_rodada      = grade_arquivos$rodada
 ),
 ler_melhorias_processo
)

# ==============================================================================
# Padronização das siglas dos operadores. É criado um tibble a partir de eventos_melhoria, 
# normalizando os nomes textuais registrados na coluna `Operator Name` e convertendo-os 
# nas siglas dos 12 operadores de vizinhança adotadas na Tabela 12.
# 
# A variável eventos_padronizados recebe o tibble final devolvido pela função mutate().
#
# A função mutate() adiciona a nova coluna 'Operador' ao final do tibble de 
# forma vetorizada. A criação da coluna Operador é necessária para converter os 
# nomes completos registrados nos arquivos CSV brutos nas siglas padronizadas 
# que formam as colunas da Tabela 12.
# 
# O primeiro parâmetro declarado na função mutate() é o tibble eventos_melhroria
# que será transformado.
# 
# A função case_when() é uma estrutura condicional vetorizada equivalente a uma 
# cadeia de if / else if / else que avalia múltiplas condições lógicas linha por 
# linha e atribui um valor correspondente para formar a nova coluna Operador.
# A sintaxe é baseada no operador fórmula ~ , onde cada linha interna do case_when() 
# é dividida pelo til ~ em duas partes:
#  - À esquerda do ~ (condição lógica): Um teste vetorial que retorna TRUE ou FALSE 
#    para cada linha da tabela que verifica se o texto da coluna Operation Name é igual.
#  - À direita do ~ (valor de saída): O resultado gravado na coluna Operador para 
#    todas as linhas em que o teste da esquerda foi verdadeiro.
# O case_when() testa as regras na ordem em que foram escritas, de cima para baixo, 
# e assim que uma linha satisfaz uma condição, ela recebe a sigla definida e não 
# é mais testada nas regras seguintes.
# ==============================================================================
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

# ==============================================================================
# Agrupa-se os registros de melhoria por instância e por operador de vizinhança, 
# contabilizando o total acumulado de aplicações bem-sucedidas ao longo das 50 rodadas 
# para cada par.
# 
# Na variável contagem_por_instancia é armazenado o novo tibble resumido, preservando 
# o tibble eventos_padronizados intacto na memória.
# 
# A função count() agrupa um tibble pelas variáveis informadas e conta quantas linhas 
# existem em cada grupo.
# O primeiro argumento explícito "eventos_padronizados" indica o tibble de entrada 
# que contém os eventos de melhoria com as siglas padronizadas na coluna Operador.
# Os argumentos "instance" e "Operador" são as colunas usadas como chave de agrupamento.
# Como a coluna rodada é omitida, o count() soma automaticamente todas as ocorrências 
# registradas nas 50 rodadas para cada par único de "instance" e "Operador".
# O parâmetro "name = "qtd_aplicacoes" " define o nome da nova coluna onde serão 
# gravadas as contagens.
# ==============================================================================
contagem_por_instancia <- count(eventos_padronizados, instance, Operador, name = "qtd_aplicacoes")

# ==============================================================================
# É feita a soma de todas as aplicações bem-sucedidas dos 12 operadores de vizinhança
# ao longo das 50 rodadas e das 15 instâncias, armazenando esse valor na variável 
# total_geral_todos_operadores.
# 
# Essa variável funciona como o denominador na criação da linha_percentual por meio 
# da fórmula round((.x / total_geral_todos_operadores) * 100, 2).
# ==============================================================================
total_geral_todos_operadores <- sum(contagem_por_instancia$qtd_aplicacoes)

# ==============================================================================
# Construção da Tabela 12 completa com todos os 12 operadores.
# A função c() cria um vetor de texto com as siglas dos operadores.
# ==============================================================================
todos_operadores <- c("EXT","SW","RNG","DOT","STAR","CC","NEG","BR","PLUS","START","OPT","END")

# ==============================================================================
# Transformação do tibble "contagem_por_instancia" para a estrutura matricial da 
# Tabela 12, organizando linhas e colunas como no artigo e armazenando o tibble 
# resultante (15 linhas × 13 colunas) em tabela_12_instancias.
# 
# A função "pivot_wider()" recebe "contagem_por_instancia" como primeiro argumento 
# explícito e reorganiza o tibble pelos critérios:
# - names_from = Operador: Converte cada sigla distinta da coluna Operador em um 
#   cabeçalho de coluna.
# - values_from = qtd_aplicacoes: Preenche as células sob essas novas colunas com 
#   as respectivas contagens acumuladas.
# - values_fill = 0: Substitui por 0 qualquer célula vazia (NA) nos casos em que 
#   um operador nunca produziu melhoria em determinada instância.
# ==============================================================================
tabela_12_instancias <-  pivot_wider(contagem_por_instancia,
                                     names_from  = Operador,
                                     values_from = qtd_aplicacoes,
                                     values_fill = 0) %>%

# ==============================================================================
# O primeiro pipe %>% injeta o tibble pivoteado como argumento inicial implícito 
# de right_join(), que a cruza com dados_instancias usando "instance" como "chave primária".
# 
# A função right_join() faz com que as 15 instâncias listadas à direita no tibble 
# dados_instancias permaneçam no resultado e traz a coluna numérica "order" para 
# a tabela (que será suprimida posteriormente pela função select() ).
# ==============================================================================
                         right_join(dados_instancias, by = "instance") %>%

# ==============================================================================
# O segundo pipe %>% injeta o tibble resultante do right_join() como argumento inicial 
# implícito de arrange().
# 
# A função arrange() ordena as 15 linhas de forma crescente pela coluna order (de 
# 1 a 15), reproduzindo a sequência exata de apresentação das instâncias na Tabela 
# 12 do artigo.
# ==============================================================================
                         arrange(order) %>%

# ==============================================================================
# O terceiro pipe %>% injeta o tibble resultante do arrange() como argumento inicial 
# implícito de mutate().
# 
# A função mutate() cria a coluna Instance, utilizando case_when() para traduzir 
# os identificadores técnicos dos arquivos ("warmup", "aman-aplan" etc.) nos rótulos 
# formatados publicados no estudo ("Plain strings", "A man, a plan", etc.).
# ==============================================================================
                         mutate(Instance = case_when(
                                            instance == "warmup"       ~ "Plain strings",
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

# ==============================================================================
# O quarto pipe %>% injeta o tibble resultante do mutate() como argumento inicial 
# implícito de select().
# O pipe %>% envia o tibble intermediário, que nesse momento possui 15 colunas:
# instance, as 12 siglas criadas pelo pivot_wider(), order trazida pelo right_join(), 
# e Instance criada pelo mutate()).
# 
# A função select() mantém apenas as 13 colunas declaradas dentro dos parênteses, 
# eliminando automaticamente "instance" e "order" que já cumpriram seus papéis nas
# etapas anteriores.
# O primeiro argumento Instance informa o nome literal da coluna que ficará como
# primeira coluna à esquerda do tibble.
# O segundo argumento all_of(todos_operadores) instrui a expandir o vetor todos_operadores
# como uma lista de nomes de colunas.
# ==============================================================================
  select(Instance, all_of(todos_operadores))

# ==============================================================================
# Criação do tibble com 1 linha × 13 colunas contendo a soma vertical de cada um 
# dos 12 operadores de vizinhança ao longo das 15 instâncias presentes em "tabela_12_instancias".
# 
# A função summarise() condensa as 15 linhas em uma linha consolidada.
# O primeiro argumento "tabela_12_instancias" indica explicitamente o tibble de 
# entrada que será sumarizado, contendo os valores das 15 instâncias distribuídos 
# nas 12 colunas de operadores.
# O segundo argumento "Instance = "Total" " define a primeira coluna do novo tibble 
# com o nome Instance e o texto constante "Total".
# O terceiro argumento "across()" aplica uma mesma função simultaneamente a um conjunto
# de colunas, no caso a função "sum" soma verticalmente os 15 valores numéricos 
# de todas as 12 colunas descritas no vetor "todos_operadores".
# ==============================================================================
linha_total <- summarise(tabela_12_instancias,
                         Instance = "Total",
                         across(all_of(todos_operadores), sum)
                        )

# ==============================================================================
# Conversão da soma de cada um dos 12 operadores para seu percentual de participação 
# sobre o total geral de melhorias do experimento (total_geral_todos_operadores), 
# com arredondamento de duas casas decimais.
# 
# A função mutate() por ter o tibble linha_total passada explicitamente como primeiro 
# e já possui as colunas Instance e as 12 siglas de operadores, o mutate() sobrescreve 
# os valores dessas colunas mantendo exatamente a mesma estrutura de 1 linha × 13 colunas.
# O segundo parâmetro Instance = "Total (%)" substitui o texto "Total" da primeira 
# coluna (Instance) pelo rótulo "Total (%)", identificando a linha de percentuais 
# na Tabela 12.
# O terceiro parâmetro across() aplica a mesma fórmula simultaneamente às 12 colunas
# de operadores, evitando repetir o cálculo manualmente para cada sigla e preservando 
# os nomes originais dos cabeçalhos.
# A função all_of(todos_operadores) indica quais colunas serão transformadas, expandindo 
# o vetor de texto todos_operadores ("EXT" até "END").
# 
# O til ~ declara uma função interna dentro do across() dispensando a escrita formal de function(x).
# O .x é um marcador da coluna atual para representar o valor numérico de cada coluna 
# de operador sendo processada naquele instante (EXT, depois SW, até END).
# O 2 arredonda o percentual resultante para 2 casas decimais, reproduzindo a apresentação 
# numérica da última linha da Tabela 12.
# ==============================================================================
linha_percentual <- mutate(linha_total,
                           Instance = "Total (%)",
                           across(all_of(todos_operadores), ~ round((.x / total_geral_todos_operadores) * 100, 2))
                          )

# ==============================================================================
# A função bind_rows() empilha verticalmente múltiplos tibbles um abaixo do outro, 
# consolidando tabela_12_instancias, linha_total e linha_percentual em um único tibble 
# final de 17 linhas × 13 colunas armazenada em tabela_12_final.
# ==============================================================================
tabela_12_final <- bind_rows(
  tabela_12_instancias,
  linha_total,
  linha_percentual
)

# ==============================================================================
# Exibição dos resultados no console e exportação para CSV
# ==============================================================================
cat("======================================================================\n")
cat("TABELA 12 COMPLETA - TODOS OS 12 OPERADORES DE VIZINHANÇA (50 RODADAS)\n")
cat("======================================================================\n")
print(as.data.frame(tabela_12_final), row.names = FALSE)

arquivo_saida <- file.path(diretorio_base, "resultado_item4_tabela12.csv")
write_csv2(tabela_12_final, arquivo_saida)
cat("\nArquivo salvo em:", arquivo_saida, "\n")
