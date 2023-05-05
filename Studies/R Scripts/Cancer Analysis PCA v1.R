#################################################################
# Cancer Analysis using PCA                                     #
# By Luis A. Urso                                               #
# Objective: Use PCA to group cancer factors                    #
#################################################################


## Install Packages 


pkges <- c("plotly", #plataforma gráfica
           "tidyverse", #carregar outros pacotes do R
           "ggrepel", #geoms de texto e rótulo para 'ggplot2' que ajudam a
           #evitar sobreposição de textos
           "knitr", "kableExtra", #formatação de tabelas
           "reshape2", #função 'melt'
           "PerformanceAnalytics", #função 'chart.Correlation' para plotagem
           "psych", #elaboração da fatorial e estatísticas
           "ltm", #determinação do alpha de Cronbach pela função 'cronbach.alpha'
           "Hmisc", # matriz de correlações com p-valor
           "readxl") # importar arquivo Excel

options(rgl.debug = TRUE)

if(sum(as.numeric(!pkges %in% installed.packages())) != 0){
  installer <- pkges[!pkges %in% installed.packages()]
  for(i in 1:length(installer)) {
    install.packages(installer, dependencies = T)
    break()}
  sapply(pkges, require, character = T) 
} else {
  sapply(pkges, require, character = T) 
}

## Load the Dataset 

cancer <- read.csv("cancer_data.csv")

cancer$diagnosis <- ifelse(cancer$diagnosis=="M",1,0)
cancer$diagnosis <- as.factor(cancer$diagnosis)


## Bartellet Test (p-value <= 0.05)

cortest.bartlett(cancer[, 3:32])

## Make the PCA 

pca_cancer <- principal(cancer[, 3:32],
                      nfactors = length(cancer[, 3:32]),
                      rotate = "none",
                      scores = TRUE)
pca_cancer

# Eigenvalues (autovalores)

eigenvalues <- round(pca_cancer$values, 5)

eigenvalues

# Verify the Eigenvalues (Result should be equal to the amount of variables)

round(sum(eigenvalues), 2)

# Shared Variance of each autovalue
shared_variance <- as.data.frame(pca_cancer$Vaccounted) %>% 
  slice(1:3)

rownames(shared_variance) <- c("Autovalores",
                                       "Prop. da Variância",
                                       "Prop. da Variância Acumulada")
## View

round(shared_variance, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 20)

## Factorial Scores Calc

scores_fatoriais <- as.data.frame(pca_cancer$weights)

## View 

round(scores_fatoriais, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 20)

## Factors Calulation 

fatores <- as.data.frame(pca_cancer$scores)

View(fatores)

## Coeficientes de correlação de Pearson para cada par de fatores (ortogonais)
rho <- rcorr(as.matrix(fatores), type="pearson")
round(rho$r, 4)

# Cálculo das cargas fatoriais
cargas_fatoriais <- as.data.frame(unclass(pca_cancer$loadings))

# Visualização das cargas fatoriais
round(cargas_fatoriais, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 20)

# Cálculo das comunalidades
comunalidades <- as.data.frame(unclass(pca_cancer$communality)) %>%
  rename(comunalidades = 1)

# Visualização das comunalidades (aqui são iguais a 1 para todas as variáveis)
# Foram extraídos 4 fatores neste primeiro momento
round(comunalidades, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 20)

##
##
### CRITERIO DE KAISER - AUTOVALORES > 1 
##
##
### Elaboração da Análise Fatorial por Componentes Principais ###
### Fatores extraídos a partir de autovalores maiores que 1 ###

# Definição da quantidade de fatores com eigenvalues maiores que 1
k <- sum(eigenvalues > 1)
print(k)

pca_cancer2 <- principal(cancer[, 3:32],
                       nfactors = k,
                       rotate = "none",
                       scores = TRUE)
pca_cancer2

# Cálculo das comunalidades com apenas os 'k' ('k' = 2) primeiros fatores
comunalidades2 <- as.data.frame(unclass(pca_cancer2$communality)) %>%
  rename(comunalidades = 1)


# Visualização das comunalidades com apenas os 'k' ('k' = 2) primeiros fatores
round(comunalidades2, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 20)

# Loading plot com as cargas dos 'k' ('k' = 2) primeiros fatores
cargas_fatoriais[, 1:2] %>% 
  data.frame() %>%
  rownames_to_column("variáveis") %>%
  ggplot(aes(x = PC1, y = PC2, label = variáveis)) +
  geom_point(color = "darkorchid",
             size = 3) +
  geom_text_repel() +
  geom_vline(aes(xintercept = 0), linetype = "dashed", color = "orange") +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "orange") +
  expand_limits(x= c(-1.25, 0.25), y=c(-0.25, 1)) +
  theme_bw()

# Adicionando os fatores extraídos no banco de dados original
cancer <- bind_cols(cancer,
                    "fator 1" = fatores$PC1, 
                    "fator 2" = fatores$PC2)

# Criação de um ranking Critério da soma ponderada e ordenamento)
cancer$ranking <- fatores$PC1 * shared_variance$PC1[2] +
  fatores$PC2 * shared_variance$PC2[2]

# Visualizando o ranking final
cancer %>%
  arrange(desc(ranking)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 17)

## See the Clustering PC1 x PC2 + Diagnosis

cancer %>% ggplot(aes(cancer$`fator 1`,cancer$`fator 2`)) +
  geom_point(aes(color=diagnosis,shape=diagnosis),size=2)
