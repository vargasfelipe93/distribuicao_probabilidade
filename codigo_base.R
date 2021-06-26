#################################################################################################################
#################################################################################################################
# Comentários :)
# Os códigos não estão as coisas mais bonitas do mundo
# Daria para trocar os loops por map, que aumentaria muito a performance, mas ainda bato um pouco de cabeça
# E na estimator_comparator aceitar mais de uma combinação por vez, em tese isso é de boas
# como já estava tudo rodado quando pensei nisso, deixo como melhoria.
# Falta fazer alguns testes unitários para as funções funcionarem perfeitamente
# Exemplo os dados tem que estar entre 0 e 1.
#################################################################################################################
#################################################################################################################


#################################################################################################################
#################################################################################################################
# Bibliotecas e utilitários
#################################################################################################################
#################################################################################################################
##Funcao para instalar/chamar os pacotes necessarios
usePackage <- function(p) {
  newPackages <- p[!(p %in% installed.packages()[, "Package"])]
  if(length(newPackages))
    install.packages(newPackages, dependencies = TRUE)
  cat("Packages successfully loaded:\n")
  sapply(p, require, character.only = TRUE, quietly = TRUE)
}
#Chamando os pacotes necessários
usePackage("ggplot2")
usePackage("tidyverse")
usePackage("rootSolve")

#################################################################################################################
#################################################################################################################
# Densidade e funcao acumulada de PB(p)
#################################################################################################################
#################################################################################################################
#Criando uma data frame vazio
#Armzenará os valores da densidade
base_density <- data.frame()

#Criando a função base
#space_of_sequence: espaçamento que o X variará entre 0 e 1
#p: parâmetro da distribuição
density_sqrt <- function(space_of_sequence = 0.01, p = 5, reset_df = F) {

  #Caso insira space_of_sequence que apresentará problemanas
  if(space_of_sequence > 1){
    stop("space_of_sequence deve ser um valor entre (0, 1), visto que X varia entre (0,1)")
  }   
  
  #Caso insira um p negativo  
  if(p <= 0){
    stop("p é um número > 0")
  }  
    
  #Caso usuário queira resetar o data_frame
  if(reset_df == T) {
    base_density <- data.frame()
  }

#Vetor com o espaçamento de X
x <- seq(0,1,space_of_sequence)
#Aplicar X na função de densidade
f_x <- p*(p+1)*(1 - sqrt(x))^(p-1)/(2)
#Aplicar X na função acumulada
F_x <- ((-p*sqrt(x)-1)*exp(p*log(1-sqrt(x))))+1
#Transformando p em string (facilitá o gráfico)
param <- rep(as.character(p),length(x))
#Agrupar as colunas 
base <- bind_cols(x = x , f = f_x, f_upper = F_x, p = param)
#Inserir resultado no data frame vazio
base_density <<- bind_rows(base, base_density)

}

#"Densidade para casos em que p > 1"
density_sqrt(space_of_sequence = 0.001, p = 2, reset_df = T)
density_sqrt(space_of_sequence = 0.001, p = 5)
density_sqrt(space_of_sequence = 0.001, p = 10)

base_density <- base_density %>%
  filter( x > 0 & x < 1)

ggplot(base_density, aes(x=x, y=f, group = p)) +
  geom_line(aes(color=p))  +
  labs(y = "f(x)") + 
  scale_color_brewer(palette="Paired") +
  ggtitle("Densidade para casos em que p > 1") +
  theme_bw()

ggplot(base_density, aes(x=x, y=f_upper, group = p)) +
  geom_line(aes(color=p))  +
  labs(y = "F(x)") + 
  scale_color_brewer(palette="Paired") +
  ggtitle("Função acumulada para casos em que p > 1") +
  theme_bw()

#"Densidade para casos em que p < 1"
density_sqrt(space_of_sequence = 0.001, p = 0.01, reset_df = T)
density_sqrt(space_of_sequence = 0.001, p = 0.1)
density_sqrt(space_of_sequence = 0.001, p = 0.5)
density_sqrt(space_of_sequence = 0.001, p = 0.99)



ggplot(base_density, aes(x=x, y=f_upper, group = p)) +
  geom_line(aes(color=p))  +
  labs(y = "F(x)") + 
  scale_color_brewer(palette="Paired") +
  ggtitle("Função acumulada para casos em que p < 1") +
  theme_bw()


#"Densidade para casos em que p < 1"
density_sqrt(space_of_sequence = 0.001, p = 0.01, reset_df = T)
density_sqrt(space_of_sequence = 0.001, p = 0.05)
density_sqrt(space_of_sequence = 0.001, p = 0.1)
density_sqrt(space_of_sequence = 0.001, p = 0.2)
density_sqrt(space_of_sequence = 0.001, p = 0.5)
density_sqrt(space_of_sequence = 0.001, p = 0.8)
density_sqrt(space_of_sequence = 0.001, p = 0.99)

base_density <- base_density %>%
  filter( x > 0 & x < 1)

ggplot(base_density, aes(x=x, y=f, group = p)) +
  geom_line(aes(color=p))  +
  labs(y = "f(x)") + 
  scale_color_brewer(palette="Paired") +
  ggtitle("Densidade para casos em que p < 1") +
  theme_bw()

#"Densidade para casos em que p = 1"
density_sqrt(space_of_sequence = 0.00001, p = 1, reset_df = T)

ggplot(base_density, aes(x=x, y=f_upper, group = p)) +
  geom_line(aes(color=p))  +
  labs(y = "F(x)") + 
  scale_color_brewer(palette = "Dark2") +
  ggtitle("Função acumulada para casos em que p = 1") +
  theme_bw()





#################################################################################################################
#################################################################################################################
# Comportamento da esperança de X
#################################################################################################################
#################################################################################################################
#Vetor com o espaçamento de p
p <- seq(0,10,0.01)
#Aplicar p na E(X)
e_x <- 6/((p+2)*(p+3))

#União dos vetores
base <- bind_cols(e_x = e_x , p = p)

#Remoção do ponto crítico p
base <- base %>%
  filter(p > 0)

#Realização do gráfico
ggplot(base, aes(x=p, y=e_x)) +
  geom_line()  +
  labs(y = "E(x)") + 
  scale_color_brewer(palette="Paired") +
  ggtitle("Variação de E(X) conforme o p") +
  theme_bw()


#################################################################################################################
#################################################################################################################
# Geracao de variaveis aleatorias
#################################################################################################################
#################################################################################################################
##Funcao para geracao de amostra aleatoria de PB(p)
# p = parâmetro p  a qual a amostra sera gerada
# n = tamanho da amostra aleatoria gerada
# min_range = range inicial de procura das raizes
# max_range = range maximo de procura das raizes
# set_seed = semente aleatoria a ser usada
random_generator <- function(p = 5, n = 10, min_range = -10, max_range = 10, set_seed = 100) {
  
  ##Gerar vetor de tamanho amostral para armazenar os resultados
  random_number <- rep(0, n)
  
  ##Funcao para encontrar as raizes de x, cujo a raiz sera um valor de uma PB(p)
  roots_generate_random_number <- function(x) {((-p*sqrt(x)-1)*exp(p*log(1-sqrt(x)))) + (1 - y)}
  
  ##Validacao das condicoes iniciais
  if(p <= 0){
    stop("p deve ser uma valor > 0")
  }
  
  ##Validacao das condicoes iniciais
  if(max_range < min_range){
    stop("Reveja os limites, máximo menor que o mínimo")
  }
  
  ##Validacao das condicoes iniciais
  if(max_range < 0){
    stop("Limite máximo de busca menor que 0, coloque um valor maior que 1")
  }
  
  ##Loop (alterar para map) em que simula-se valores da PB(p)
  ##Receita para gerar a amostra
  #Setar um set.seed
  #Gerar um valor de uma U(0,1)
  #Encontrar as raizes da funcao em (1)
  for(i in 1:n){
    
    set.seed(set_seed+set_seed*i)
    y = runif(1)
    roots_random_number <- uniroot.all(roots_generate_random_number, c(min_range, max_range))
    random_number[i] <- roots_random_number
    
    
  }
  
  ##Salvar os resultados da amotra no vetor random_sample
  random_sample <<- random_number
  
}


#################################################################################################################
#################################################################################################################
# Estimador de MLE
#################################################################################################################
#################################################################################################################
##Funcao para calculo do estimador de maxima verossimilhanca
# x = dados a serem analisados
# min_range = range inicial de procura das raizes
# max_range = range maximo de procura das raizes
# plot_true = grafico das raizes do estimador de maxima verossimilhanca
# coments_true = comentario sobre as estimativas de maxima verossimilhanca
estimator_mle <- function(x = dados, min_range = -100, max_range = 100, plot_true = F, coments_true = F) {
  
  ##Validacao das condicoes iniciais
  if(max_range < min_range){
    stop("Reveja os limites, máximo menor que o mínimo")
  }
  
  ##Validacao das condicoes iniciais
  if(max_range < 0){
    stop("Limite máximo de busca menor que 0, coloque um valor maior que 1")
  }
  
  ##Calculo do tamanho amostral
  n = length(x)
  
  ##Funcao do somatorio do log de 1 sqrt(x)
  prod_log = sum(log((1 - sqrt(x))))
  
  ##Funcao para encontrar as raizes de p, cuja a raiz sera o MLE
  mle_estimator <- function(x) {prod_log*x^2 + (prod_log + 2*n)*x + n}
  roots_mle <- uniroot.all(mle_estimator, c(min_range, max_range))
  
  ##Por ser um polinomio de segundo grau havera 2 raizes, seleciona-se somente a > 0
  ##Salve-se ela em real_mle
  real_mle <<- roots_mle[roots_mle>0]
  
  ##Se comentario = TRUE, entao apresenta as seguintes informacoes
  if(coments_true){
    cat("Os pontos de máximo serão dados por:",roots_mle[1], "e ", roots_mle[2])
    cat("\n")
    cat("Como p varia entre 0 e infinito, o estimador será:", real_mle)
  }
  
  ##Se plot = TRUE, entao apresenta o local das raizes do polinomio
  if(plot_true) {
    curve(mle_estimator(x), min_range, max_range
          , main = "Raízes do método da máxima verossimilhança"
          , xlab= "Range de procura das raízes"
          , ylab= "Função de máxima verossimilhança")
    points(roots_mle, y = rep(0, length(roots_mle)), pch = 16, cex = 2)
  }
}

#################################################################################################################
#################################################################################################################
# Estimador do MME
#################################################################################################################
#################################################################################################################
##Funcao para calculo do estimador do metodo dos momentos
# x = dados a serem analisados
# min_range = range inicial de procura das raizes
# max_range = range maximo de procura das raizes
# plot_true = grafico das raizes do metodo dos momentos
# coments_true = comentario sobre as estimativas do metodo dos momentos
estimator_of_moment <- function(x = dados, min_range = -10, max_range = 10, plot_true = T, coments_true = T) {
  
  ##Validacao das condicoes iniciais
  if(max_range < min_range){
    stop("Reveja os limites, máximo menor que o mínimo")
  }
  
  ##Validacao das condicoes iniciais
  if(max_range < 0){
    stop("Limite máximo de busca menor que 0, coloque um valor maior que 1")
  }
  
  ##Media das observacoes
  mean_of_data = mean(x)
  
  ##Funcao para encontrar as raizes de p, cuja a raiz sera o MME
  moment_estimator <- function(x) {x^2 + 5*x + (6 - 6/mean_of_data)}
  roots_moment <- uniroot.all(moment_estimator, c(min_range, max_range))
  
  ##Por ser um polinomio de segundo grau havera 2 raizes, seleciona-se somente a > 0
  ##Salve-se ela em real_moment
  real_moment <<- roots_moment[roots_moment>0]
  
  ##Se comentario = TRUE, entao apresenta as seguintes informacoes
  if(coments_true){
    cat("Os pontos de máximo serão dados por:",roots_moment[1], "e ", roots_moment[2])
    cat("\n")
    cat("Como p varia entre 0 e infinito, o estimador será:", real_moment)
  }
  
  ##Se plot = TRUE, entao apresenta o local das raizes do polinomio
  if(plot_true) {
    curve(moment_estimator(x), min_range, max_range, main = "Raízes do método dos momentos"
          , xlab= "Range de procura das raízes"
          , ylab= "Função em relação ao método dos momentos")
    points(roots_moment, y = rep(0, length(roots_moment)), pch = 16, cex = 2)
  }
}

#################################################################################################################
#################################################################################################################
# Simulações de Monte Carlo (Comparação dos estimadores)
#################################################################################################################
#################################################################################################################
##Funcao para gerar comparar os estimadores do metódo dos momentos e máxima verossimilhança
# p = parâmetro p a qual a amostra sera gerada
# n = tamanho da amostra aleatoria gerada
# re = número de replicacoes
# min_range = range inicial de procura das raizes
# max_range = range maximo de procura das raizes
# save_sample = salvar as amostrar geradas nas n replicacoes
# coments_true = comentario sobre as estimativas dos modelos
estimator_comparator <- 
  function(p = 1, n = 1000, re = 1000, min_range = -50, max_range = 50, save_sample = T, coments_true = T){
    
    ##Criar lista vazia para armazenar as amostras caso necessario
    samples <- list()  
    ##Criar um vetor vazio para armazenar os resultados das replicacoes para MLE
    mle_results <- rep(0, re)
    ##Criar um vetor vazio para armazenar os resultados das replicacoes para MME
    mme_results <- rep(0, re)
    
    ##Loop para as replicacoes  
    for(j in 1:re){
      
      ##Somente para saber em qual interacao esta
      cat("\n", j)
      
      ##Gerar uma amostra de tamanho n e parametro p
      random_sample <- random_generator(p = p, n = n, set_seed = 100*j)
      
      ##Salva o resultado em samples
      samples[[j]] <- random_sample
      
      ##Calcula o estimador de MME
      estimator_of_moment(x = samples[[j]], plot_true = F, coments_true = F)
      ##Calcula o estimador de MLE
      estimator_mle(x = samples[[j]], plot_true = F, coments_true = F)
      
      ##Tratamento para casos de nao convergencia
      ##Se nao convergir atribui NA ao resultado
      if(length(real_moment) == 0){
        mme_results[j] <- NA
      } else {
        mme_results[j] <- real_moment
      }
      
      ##Tratamento para casos de nao convergencia
      ##Se nao convergir atribui NA ao resultado
      if(length(real_mle) == 0){
        mle_results[j] <- NA
      } else {
        mle_results[j] <- real_mle
      }
      
    }
    
    ##Se save_sample = T, salva os resultados das amostras em save_sample
    if(save_sample){
      samples <<- samples
    }
    
    ##Salvar as estimativas de MME
    mme_results <<- mme_results
    ##Salvar as estimativas de MLE
    mle_results <<- mle_results
    
    ##Se comentario = TRUE, entao apresenta as seguintes informacoes
    if(coments_true){
      cat("\n")
      cat("A media do estimador de MME é:",mean(mme_results, na.rm = T))
      cat("\n")
      cat("Desses:", sum(is.na(mle_results)), "nao convergiram")
      cat("\n")
      cat("\n")
      cat("\n")
      cat("\n")
      cat("\n")
      cat("A media do estimador de MLE é:",mean(mle_results, na.rm = T))
      cat("\n")
      cat("Desses:", sum(is.na(mle_results)), "nao convergiram")
    }
    
  }