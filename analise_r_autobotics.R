library(ggplot2)
library(jsonlite)

df <- dados_hardware

#Visão macro do data frame
summary(df)

#A mediana de uso de CPU é de 11%
median(df$cpu)

#O máximo de uso de CPU atingido foi de 100%
max(df$cpu)

#Distribuição de Uso de CPU em Porcentagem
ggplot(df, aes(x = "", y = cpu)) +
  geom_boxplot(fill = "yellow") +
  labs(title = "Distribuição do Uso de CPU (%)", y = "CPU (%)") +
  theme_minimal()

#Analisando esse gráfico, fica notável a concentração das capturas por volta de 15%, porém
#também é presente várias outliers, que mostram que o uso de CPU passa por "picos" e necessita ser monitorado para
#identificar se pode futuramente apresentar problemas por estar com uso alto (sobrecarga, impactar
#desempenho dos controladores)

#A mediana de uso de RAM é de 75.4%
median(df$ramUsada)


#O máximo de uso de RAM atingido foi de 100%
max(df$ramUsada)

#Distribuição de Uso de RAM em Porcentagem
ggplot(df, aes(x = "", y = ramUsada)) +
  geom_boxplot(fill = "orange") +
  labs(title = "Distribuição do Uso de RAM (%)", y = "RAM (%)")

#A Memória RAM tem uma média de uso naturalmente alta, e a longo prazo pode ser
# um problema, por isso devemos verificar a constância do uso através de alertas.

#A mediana de uso de disco é de 56.1%
median(df$discoUsado)

#O máximo de uso de disco atingido foi de 91.9%
max(df$discoUsado)

#Distribuição de Uso de disco em Porcentagem
ggplot(df, aes(x = "", y = discoUsado)) +
  geom_boxplot(fill = "purple") +
  labs(title = "Distribuição do Uso de Disco (%)", y = "Disco (%)")

#Alguns controladores possuem maior utilização de disco do que outros e isso se dá pelo armazenamento
#de logs e buffers de processo ou alguns arquivos temporários, porém em casos de utilização maior como
# de 90% já necessita de investigação. Diante de RAM e CPU, o disco é o componente que possui a utilização,
#mais constante ao logo do tempo.

#fazer os alertas de CPU, RAM e Disco

# alerta de cpu - nivel medio, 60% a 85%
# alerta de cpu - nivel critico, acima de 85%
df$alerta_cpu <- ifelse(df$cpu > 85, "crítico",
                              ifelse(df$cpu >= 60, "médio", "estável"))


#alerta de ram - nivel medio, 75 a 90%
#alerta de ram - nivel critico, acima de 90%
df$alerta_ram <- ifelse(df$ramUsada > 90, "crítico",
                              ifelse(df$ramUsada >= 75, "médio", "estável"))

# alerta de disco nível médio: 70% a 85%
# nível crítico: acima de 85%
df$alerta_disco <- ifelse(df$discoUsado > 85, "crítico",
                          ifelse(df$discoUsado >= 70, "médio", "estável"))


barplot(
  rbind(
    table(df$alerta_cpu),
    table(df$alerta_ram),
    table(df$alerta_disco)
  ),
  beside = TRUE,
  col = c("yellow", "orange", "purple"),  # cores para CPU, RAM e Disco
  legend.text = c("CPU", "RAM", "Disco"),
  main = "Alertas por Tipo",
  xlab = "Nível de Alerta",
  ylab = "Quantidade"
)


#Ao todo, a maioria dos alertas são de níveis estável, com cpu ultrapassando 1000, ram e disco próximo de 600

#Em nível médio a RAM e Disco se destaca diante da cpu, visto que seus usos são normalmente maiores, por volta
#de 600 alertas desse nível. Já a cpu, possui por volta de 20 alertas desse nível, reforçando a ideia
#que o componente passar por problemas de picos de uso

#No nível crítico, a cpu comprova mais "picos" enquanto a ram possui uma maior "constância" 
#pelo fato de ter uma média de uso naturalmente mais alta (como dito anteriormente), 
#sendo assim a cpu possui cerca de 35 alertas e ram por volta de 80.

#Já o caso de disco, deve ser trabalhado de forma diferente, enquanto o uso de CPU e RAM flutua de acordo 
#com a carga de processamento e tarefas em execução, o uso de disco normalmente cresce de forma gradual, 
#acumulando dados e arquivos de log. Por isso, seus alertas devem priorizar a tendência de crescimento no tempo,
#em vez de variações momentâneas. 
