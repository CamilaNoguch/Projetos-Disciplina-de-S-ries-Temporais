# Projeto 1 - Temperatura média mensal de Brasília - SARIMA
# Referência: Cryer & Chan (2008), slides e orientações do professor
# Camila Lima 

###### Pacotes ######

if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  rmet,
  dplyr,
  lubridate,
  ggplot2,
  forecast,
  tseries,
  moments
)


###### Diretórios de saída ######

dir.create("tabelas", showWarnings = FALSE)
dir.create("figuras", showWarnings = FALSE)


###### Obtenção dos dados - Estação A001 Brasília ######

ls("package:rmet")

estacoes <- inmet_stations()
View(estacoes)

subset(estacoes, station_code == "A001")

anos <- 2000:2026

inmet_download(
  years = anos,
  quiet = FALSE
)

dados <- inmet_read(
  years    = anos,
  stations = "A001"
)

names(dados)
head(dados)
tail(dados)
summary(dados)


###### Seleção da variável de interesse ######

dados_temp <- dados %>%
  mutate(
    data_hora = as.POSIXct(datetime),
    ano       = year(data_hora),
    mes       = month(data_hora)
  ) %>%
  select(
    data_hora,
    ano,
    mes,
    temperatura_bulbo_seco = temp_dry_c
  )

head(dados_temp)
tail(dados_temp)
summary(dados_temp)


###### Valores faltantes ######

sum(is.na(dados_temp$temperatura_bulbo_seco))

faltantes_mensais <- dados_temp %>%
  group_by(ano, mes) %>%
  summarise(
    n_obs          = n(),
    n_faltantes    = sum(is.na(temperatura_bulbo_seco)),
    prop_faltantes = n_faltantes / n_obs,
    .groups        = "drop"
  )

head(faltantes_mensais)
tail(faltantes_mensais)

faltantes_mensais %>%
  filter(n_faltantes > 0)

faltantes_mensais %>%
  filter(prop_faltantes > 0.10) %>%
  arrange(desc(prop_faltantes))


###### Construção da temperatura média mensal ######

temperatura_mensal <- dados_temp %>%
  group_by(ano, mes) %>%
  summarise(
    temp_media     = mean(temperatura_bulbo_seco, na.rm = TRUE),
    n_obs          = n(),
    n_faltantes    = sum(is.na(temperatura_bulbo_seco)),
    prop_faltantes = n_faltantes / n_obs,
    .groups        = "drop"
  ) %>%
  arrange(ano, mes)

head(temperatura_mensal)
tail(temperatura_mensal)
dim(temperatura_mensal)
summary(temperatura_mensal$temp_media)


###### Série temporal mensal ######

inicio_serie <- c(temperatura_mensal$ano[1], temperatura_mensal$mes[1])

serie <- ts(
  temperatura_mensal$temp_media,
  start     = inicio_serie,
  frequency = 12
)

plot(
  serie,
  main = "Temperatura média mensal de Brasília",
  ylab = "Temperatura média mensal (°C)",
  xlab = "Ano"
)


###### Análise exploratória ######

nomes_meses <- c(
  "Jan", "Fev", "Mar", "Abr", "Mai", "Jun",
  "Jul", "Ago", "Set", "Out", "Nov", "Dez"
)

temperatura_mensal <- temperatura_mensal %>%
  mutate(
    data      = as.Date(paste(ano, mes, "01", sep = "-")),
    mes_abrev = factor(nomes_meses[mes], levels = nomes_meses)
  )

temperatura_mensal


# Período da série

periodo_serie <- temperatura_mensal %>%
  summarise(
    data_inicial = min(data),
    data_final   = max(data),
    n_meses      = n()
  )

periodo_serie


# Estatísticas descritivas gerais

estatisticas_gerais <- temperatura_mensal %>%
  summarise(
    n             = n(),
    media         = mean(temp_media),
    mediana       = median(temp_media),
    desvio_padrao = sd(temp_media),
    variancia     = var(temp_media),
    coef_variacao = sd(temp_media) / mean(temp_media),
    minimo        = min(temp_media),
    q1            = quantile(temp_media, 0.25),
    q3            = quantile(temp_media, 0.75),
    maximo        = max(temp_media),
    amplitude     = max(temp_media) - min(temp_media)
  )

estatisticas_gerais


# Estatísticas descritivas por mês

estatisticas_mensais <- temperatura_mensal %>%
  group_by(mes, mes_abrev) %>%
  summarise(
    n             = n(),
    media         = mean(temp_media),
    mediana       = median(temp_media),
    desvio_padrao = sd(temp_media),
    minimo        = min(temp_media),
    q1            = quantile(temp_media, 0.25),
    q3            = quantile(temp_media, 0.75),
    maximo        = max(temp_media),
    amplitude     = max(temp_media) - min(temp_media),
    cv            = 100 * sd(temp_media) / mean(temp_media),
    .groups       = "drop"
  )

estatisticas_mensais

estatisticas_mensais %>% select(mes_abrev, n)


# Meses mais quentes e mais frios

mes_mais_quente <- estatisticas_mensais %>%
  arrange(desc(media)) %>%
  slice(1)

mes_mais_frio <- estatisticas_mensais %>%
  arrange(media) %>%
  slice(1)

mes_mais_quente
mes_mais_frio

amplitude_sazonal_media <- mes_mais_quente$media - mes_mais_frio$media
amplitude_sazonal_media


# Anos incompletos

meses_por_ano <- temperatura_mensal %>%
  group_by(ano) %>%
  summarise(
    n_meses           = n(),
    meses_disponiveis = paste(mes, collapse = ", "),
    .groups           = "drop"
  )

meses_por_ano

anos_incompletos <- meses_por_ano %>%
  filter(n_meses < 12)

anos_incompletos


# Estatísticas por ano

estatisticas_anuais <- temperatura_mensal %>%
  group_by(ano) %>%
  summarise(
    n_meses         = n(),
    media_anual     = mean(temp_media),
    mediana_anual   = median(temp_media),
    dp_anual        = sd(temp_media),
    minimo_anual    = min(temp_media),
    maximo_anual    = max(temp_media),
    amplitude_anual = max(temp_media) - min(temp_media),
    .groups         = "drop"
  )

estatisticas_anuais


# Extremos históricos

meses_mais_quentes <- temperatura_mensal %>%
  arrange(desc(temp_media)) %>%
  slice(1:5)

meses_mais_frios <- temperatura_mensal %>%
  arrange(temp_media) %>%
  slice(1:5)

meses_mais_quentes
meses_mais_frios


# Outliers com z-score

limites_boxplot <- boxplot.stats(temperatura_mensal$temp_media)$stats
lim_inf         <- limites_boxplot[1]
lim_sup         <- limites_boxplot[5]

media_geral <- mean(temperatura_mensal$temp_media)
dp_geral    <- sd(temperatura_mensal$temp_media)

outliers_mensais <- temperatura_mensal %>%
  filter(temp_media < lim_inf | temp_media > lim_sup) %>%
  select(ano, mes, mes_abrev, data, temp_media) %>%
  mutate(z_score = (temp_media - media_geral) / dp_geral)

lim_inf
lim_sup
outliers_mensais


###### Gráficos exploratórios ######

# Série temporal com tendência suavizada

g_serie <- ggplot(temperatura_mensal, aes(x = data, y = temp_media)) +
  geom_line(linewidth = 0.4) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 0.8) +
  labs(
    title    = "Temperatura média mensal de Brasília",
    subtitle = "Estação automática A001/INMET — maio de 2000 a junho de 2026",
    x        = "Ano",
    y        = "Temperatura média mensal (°C)"
  ) +
  theme_minimal()

g_serie


# Média móvel de 12 meses

temperatura_mensal$media_movel_12 <- as.numeric(
  stats::filter(
    temperatura_mensal$temp_media,
    filter = rep(1 / 12, 12),
    sides  = 2
  )
)

g_media_movel <- ggplot(temperatura_mensal, aes(x = data)) +
  geom_line(aes(y = temp_media), linewidth = 0.35, alpha = 0.6) +
  geom_line(aes(y = media_movel_12), linewidth = 0.9) +
  labs(
    title = "Temperatura média mensal e média móvel de 12 meses",
    x     = "Ano",
    y     = "Temperatura média mensal (°C)"
  ) +
  theme_minimal()

g_media_movel


# Temperatura média anual

g_media_anual <- ggplot(estatisticas_anuais, aes(x = ano, y = media_anual)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Temperatura média anual em Brasília",
    x     = "Ano",
    y     = "Temperatura média anual (°C)"
  ) +
  theme_minimal()

g_media_anual


# Variabilidade anual

g_dp_anual <- ggplot(estatisticas_anuais, aes(x = ano, y = dp_anual)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Variabilidade anual da temperatura média mensal",
    x     = "Ano",
    y     = "Desvio-padrão anual (°C)"
  ) +
  theme_minimal()

g_dp_anual


# Tendência linear exploratória

modelo_tendencia <- lm(temp_media ~ data, data = temperatura_mensal)
summary(modelo_tendencia)


# Histograma e densidade

g_hist <- ggplot(temperatura_mensal, aes(x = temp_media)) +
  geom_histogram(aes(y = after_stat(density)), bins = 25) +
  geom_density(linewidth = 0.8) +
  labs(
    title = "Distribuição da temperatura média mensal",
    x     = "Temperatura média mensal (°C)",
    y     = "Densidade"
  ) +
  theme_minimal()

g_hist


# Boxplot geral

g_box_geral <- ggplot(temperatura_mensal, aes(y = temp_media)) +
  geom_boxplot() +
  labs(
    title = "Boxplot da temperatura média mensal",
    y     = "Temperatura média mensal (°C)"
  ) +
  theme_minimal()

g_box_geral


# Boxplot por mês

g_box_mes <- ggplot(temperatura_mensal, aes(x = mes_abrev, y = temp_media)) +
  geom_boxplot() +
  labs(
    title = "Distribuição da temperatura média mensal por mês",
    x     = "Mês",
    y     = "Temperatura média mensal (°C)"
  ) +
  theme_minimal()

g_box_mes


# Perfil sazonal médio

g_perfil_sazonal <- ggplot(
  estatisticas_mensais,
  aes(x = mes_abrev, y = media, group = 1)
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  labs(
    title = "Perfil sazonal médio da temperatura em Brasília",
    x     = "Mês",
    y     = "Temperatura média histórica (°C)"
  ) +
  theme_minimal()

g_perfil_sazonal


# Gráfico sazonal por ano

png("figuras/fig_seasonplot.png", width = 900, height = 500, res = 100)
ggseasonplot(
  serie,
  year.labels = FALSE,
  main        = "Perfil sazonal anual da temperatura média mensal",
  ylab        = "Temperatura média mensal (°C)",
  xlab        = "Mês"
)
dev.off()


# Subséries mensais

png("figuras/fig_subseries.png", width = 900, height = 500, res = 100)
ggsubseriesplot(
  serie,
  main = "Subséries mensais da temperatura média de Brasília",
  ylab = "Temperatura média mensal (°C)"
)
dev.off()


# Heatmap ano x mês

g_heatmap <- ggplot(
  temperatura_mensal,
  aes(x = mes_abrev, y = factor(ano), fill = temp_media)
) +
  geom_tile() +
  labs(
    title = "Mapa de calor da temperatura média mensal",
    x     = "Mês",
    y     = "Ano",
    fill  = "Temp. média (°C)"
  ) +
  theme_minimal()

g_heatmap


# ACF e PACF da série original

png("figuras/fig_acf_pacf_original.png", width = 900, height = 600, res = 100)
ggtsdisplay(serie, main = "Série original, ACF e PACF")
dev.off()

Acf(serie,  lag.max = 60, main = "ACF da série original")
Pacf(serie, lag.max = 60, main = "PACF da série original")


# Autocorrelações nos lags sazonais

acf_valores <- acf(serie, lag.max = 60, plot = FALSE)

acf_sazonal <- data.frame(
  lag = round(as.numeric(acf_valores$lag) * frequency(serie)),
  acf = as.numeric(acf_valores$acf)
) %>%
  filter(lag %in% c(12, 24, 36, 48, 60))

acf_sazonal


###### Decomposição STL ######

decomp <- stl(serie, s.window = "periodic")

png("figuras/fig_decomposicao_stl.png", width = 900, height = 700, res = 100)
plot(decomp, main = "Decomposição STL — Temperatura média mensal de Brasília")
dev.off()


# Componentes da decomposição

decomp_df <- data.frame(
  data         = temperatura_mensal$data,
  observado    = as.numeric(serie),
  tendencia    = as.numeric(decomp$time.series[, "trend"]),
  sazonalidade = as.numeric(decomp$time.series[, "seasonal"]),
  resto        = as.numeric(decomp$time.series[, "remainder"])
)

head(decomp_df)


# Gráficos dos componentes STL

g_tendencia <- ggplot(decomp_df, aes(x = data, y = tendencia)) +
  geom_line() +
  labs(
    title = "Componente de tendência estimada por STL",
    x     = "Ano",
    y     = "Tendência (°C)"
  ) +
  theme_minimal()

g_sazonalidade <- ggplot(decomp_df, aes(x = data, y = sazonalidade)) +
  geom_line() +
  labs(
    title = "Componente sazonal estimada por STL",
    x     = "Ano",
    y     = "Sazonalidade (°C)"
  ) +
  theme_minimal()

g_resto <- ggplot(decomp_df, aes(x = data, y = resto)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title = "Componente irregular estimada por STL",
    x     = "Ano",
    y     = "Resto (°C)"
  ) +
  theme_minimal()

g_tendencia
g_sazonalidade
g_resto


###### Testes de estacionariedade ######

adf.test(serie)
kpss.test(serie)


###### Modelagem SARIMA ######

# Separação treino e teste
# Treino: maio/2000 a dezembro/2024
# Teste : janeiro/2025 a junho/2026

serie_treino <- window(serie, end   = c(2024, 12))
serie_teste  <- window(serie, start = c(2025,  1))

length(serie_treino)
length(serie_teste)

plot(
  serie_treino,
  main = "Série de treinamento",
  ylab = "Temperatura média mensal (°C)",
  xlab = "Ano"
)

plot(
  serie_teste,
  main = "Série de teste",
  ylab = "Temperatura média mensal (°C)",
  xlab = "Ano"
)


# Identificação preliminar no treino

png("figuras/fig_acf_pacf_treino.png", width = 900, height = 600, res = 100)
ggtsdisplay(serie_treino, main = "Série de treinamento, ACF e PACF")
dev.off()

Acf(serie_treino,  lag.max = 60, main = "ACF da série de treinamento")
Pacf(serie_treino, lag.max = 60, main = "PACF da série de treinamento")

adf.test(serie_treino)
kpss.test(serie_treino)


# Diferenciação sazonal

serie_treino_diff_saz <- diff(serie_treino, lag = 12, differences = 1)

plot(
  serie_treino_diff_saz,
  main = "Série de treinamento com diferença sazonal",
  ylab = "Diferença sazonal",
  xlab = "Ano"
)

png("figuras/fig_acf_pacf_diff_saz.png", width = 900, height = 600, res = 100)
ggtsdisplay(serie_treino_diff_saz, main = "Série com diferença sazonal, ACF e PACF")
dev.off()

adf.test(serie_treino_diff_saz)
kpss.test(serie_treino_diff_saz)


# Ajuste automático como referência (busca exaustiva, critério BIC)

modelo_auto <- auto.arima(
  serie_treino,
  seasonal      = TRUE,
  stepwise      = FALSE,
  approximation = FALSE,
  ic            = "bic",
  trace         = TRUE
)

summary(modelo_auto)
checkresiduals(modelo_auto)


# Modelos SARIMA candidatos

modelo_1 <- Arima(serie_treino, order = c(0, 0, 1), seasonal = c(0, 1, 1), include.constant = FALSE)
modelo_2 <- Arima(serie_treino, order = c(1, 0, 0), seasonal = c(0, 1, 1), include.constant = FALSE)
modelo_3 <- Arima(serie_treino, order = c(1, 0, 1), seasonal = c(0, 1, 1), include.constant = FALSE)
modelo_4 <- Arima(serie_treino, order = c(0, 0, 1), seasonal = c(1, 1, 0), include.constant = FALSE)
modelo_5 <- Arima(serie_treino, order = c(1, 0, 1), seasonal = c(1, 1, 0), include.constant = FALSE)
modelo_6 <- modelo_auto

lista_modelos <- list(
  "SARIMA(0,0,1)(0,1,1)[12]" = modelo_1,
  "SARIMA(1,0,0)(0,1,1)[12]" = modelo_2,
  "SARIMA(1,0,1)(0,1,1)[12]" = modelo_3,
  "SARIMA(0,0,1)(1,1,0)[12]" = modelo_4,
  "SARIMA(1,0,1)(1,1,0)[12]" = modelo_5,
  "Auto ARIMA"               = modelo_6
)


# Comparação por AIC e BIC

comparacao_modelos <- data.frame(
  modelo = names(lista_modelos),
  AIC    = sapply(lista_modelos, AIC),
  BIC    = sapply(lista_modelos, BIC),
  logLik = sapply(lista_modelos, logLik)
) %>%
  arrange(BIC)

comparacao_modelos


# Validação preditiva de todos os modelos

avaliar_modelo <- function(modelo, serie_teste) {
  prev <- forecast(modelo, h = length(serie_teste))
  acc  <- accuracy(prev, serie_teste)
  data.frame(
    MAE  = acc["Test set", "MAE"],
    RMSE = acc["Test set", "RMSE"],
    MAPE = acc["Test set", "MAPE"]
  )
}

metricas_modelos <- lapply(lista_modelos, avaliar_modelo, serie_teste = serie_teste)

comparacao_preditiva <- bind_rows(metricas_modelos, .id = "modelo") %>%
  arrange(RMSE)

comparacao_preditiva


# Diagnóstico residual de todos os modelos

avaliar_residuos <- function(modelo) {
  res  <- residuals(modelo)
  lb24 <- Box.test(res, lag = 24, type = "Ljung-Box", fitdf = length(coef(modelo)))
  data.frame(
    ljung_box_24          = as.numeric(lb24$statistic),
    p_valor_ljung_24      = lb24$p.value,
    residuos_ruido_branco = ifelse(lb24$p.value > 0.05, "Sim", "Não")
  )
}

diagnostico_modelos <- lapply(lista_modelos, avaliar_residuos)

comparacao_diagnostico <- bind_rows(diagnostico_modelos, .id = "modelo")
comparacao_diagnostico


# Comparação final: ajuste + previsão + diagnóstico

comparacao_final <- comparacao_modelos %>%
  left_join(comparacao_preditiva, by = "modelo") %>%
  left_join(comparacao_diagnostico, by = "modelo") %>%
  arrange(BIC, RMSE)

comparacao_final


###### Seleção do modelo final ######

# O modelo SARIMA(0,0,1)(1,1,0)[12] apresentou o menor RMSE no teste,
# mas seus resíduos permaneceram autocorrelacionados (Ljung-Box p < 0,001).
# Por isso, não foi adotado como modelo final.
#
# O SARIMA(1,0,1)(0,1,2)[12] (Auto ARIMA) apresentou menor BIC,
# erros preditivos competitivos e resíduos sem autocorrelação significativa.
# Foi selecionado como modelo final.

melhor_nome  <- "Auto ARIMA"
modelo_final <- lista_modelos[[melhor_nome]]

summary(modelo_final)


###### Diagnóstico dos resíduos do modelo final ######

residuos <- residuals(modelo_final)

png("figuras/fig_checkresiduals_treino.png", width = 900, height = 600, res = 100)
checkresiduals(modelo_final)
dev.off()

plot(
  residuos,
  main = "Resíduos do modelo selecionado",
  ylab = "Resíduos",
  xlab = "Ano"
)

Acf(residuos,  lag.max = 60, main = "ACF dos resíduos")
Pacf(residuos, lag.max = 60, main = "PACF dos resíduos")

hist(residuos, breaks = 20, main = "Histograma dos resíduos", xlab = "Resíduos")

qqnorm(residuos)
qqline(residuos)


# Testes de autocorrelação residual

ljung_24 <- Box.test(residuos, lag = 24, type = "Ljung-Box", fitdf = length(coef(modelo_final)))
ljung_36 <- Box.test(residuos, lag = 36, type = "Ljung-Box", fitdf = length(coef(modelo_final)))

ljung_24
ljung_36


# Teste de heterocedasticidade (McLeod-Li)
# Rejeição sugeriria estrutura GARCH — limitação a mencionar no relatório

Box.test(residuos^2, lag = 12, type = "Ljung-Box")


# Testes de normalidade

shapiro_res <- shapiro.test(as.numeric(residuos))
jb_res      <- jarque.test(as.numeric(residuos))

shapiro_res
jb_res


# Tabela de coeficientes

coeficientes <- data.frame(
  parametro   = names(coef(modelo_final)),
  estimativa  = coef(modelo_final),
  erro_padrao = sqrt(diag(modelo_final$var.coef))
)

coeficientes


###### Validação preditiva do modelo final ######

previsao_teste <- forecast(
  modelo_final,
  h     = length(serie_teste),
  level = c(80, 95)
)

plot(
  previsao_teste,
  main = "Previsão no conjunto de teste",
  ylab = "Temperatura média mensal (°C)",
  xlab = "Ano"
)

lines(serie_teste, col = "red", lwd = 2)

accuracy(previsao_teste, serie_teste)

metricas_teste <- data.frame(
  MAE  = accuracy(previsao_teste, serie_teste)["Test set", "MAE"],
  RMSE = accuracy(previsao_teste, serie_teste)["Test set", "RMSE"],
  MAPE = accuracy(previsao_teste, serie_teste)["Test set", "MAPE"]
)

metricas_teste


# Tabela observado x previsto no teste

datas_teste <- seq(
  from       = as.Date(paste(start(serie_teste)[1], start(serie_teste)[2], "01", sep = "-")),
  by         = "month",
  length.out = length(serie_teste)
)

validacao_teste <- data.frame(
  data      = datas_teste,
  observado = as.numeric(serie_teste),
  previsto  = as.numeric(previsao_teste$mean),
  erro      = as.numeric(serie_teste) - as.numeric(previsao_teste$mean),
  erro_abs  = abs(as.numeric(serie_teste) - as.numeric(previsao_teste$mean))
)

validacao_teste


# Gráfico observado x previsto com intervalo de confiança

g_validacao <- ggplot(validacao_teste, aes(x = data)) +
  geom_ribbon(
    aes(
      ymin = as.numeric(previsao_teste$lower[, 2]),
      ymax = as.numeric(previsao_teste$upper[, 2])
    ),
    alpha = 0.2
  ) +
  geom_line(aes(y = observado), linewidth = 0.8) +
  geom_line(aes(y = previsto),  linewidth = 0.8, linetype = "dashed") +
  labs(
    title    = "Temperatura observada e prevista no conjunto de teste",
    subtitle = "Intervalo de 95% de confiança",
    x        = "Data",
    y        = "Temperatura média mensal (°C)"
  ) +
  theme_minimal()

g_validacao


###### Reajuste com a série completa e previsão até dezembro de 2026 ######

# Reestimação dos parâmetros mantendo a estrutura identificada no treino

modelo_final_completo <- Arima(
  serie,
  order    = modelo_final$arma[c(1, 6, 2)],
  seasonal = list(
    order  = modelo_final$arma[c(3, 7, 4)],
    period = 12
  ),
  include.constant = FALSE
)

summary(modelo_final_completo)
checkresiduals(modelo_final_completo)

residuos_completo <- residuals(modelo_final_completo)


# Previsão para julho a dezembro de 2026

previsao_2026 <- forecast(
  modelo_final_completo,
  h     = 6,
  level = c(80, 95)
)

plot(
  previsao_2026,
  main = "Previsão da temperatura média mensal até dezembro de 2026",
  ylab = "Temperatura média mensal (°C)",
  xlab = "Ano"
)

previsao_2026


# Tabela de previsão para julho a dezembro de 2026

tabela_previsao_2026 <- data.frame(
  data          = seq(as.Date("2026-07-01"), as.Date("2026-12-01"), by = "month"),
  previsao      = as.numeric(previsao_2026$mean),
  limite_80_inf = as.numeric(previsao_2026$lower[, 1]),
  limite_80_sup = as.numeric(previsao_2026$upper[, 1]),
  limite_95_inf = as.numeric(previsao_2026$lower[, 2]),
  limite_95_sup = as.numeric(previsao_2026$upper[, 2])
)

tabela_previsao_2026


# Intervalos empíricos com base nos quantis dos resíduos

quantis_residuos <- quantile(
  residuos_completo,
  probs = c(0.025, 0.10, 0.90, 0.975),
  na.rm = TRUE
)

quantis_residuos

tabela_previsao_2026_empirica <- tabela_previsao_2026 %>%
  mutate(
    limite_emp_80_inf = previsao + quantis_residuos[2],
    limite_emp_80_sup = previsao + quantis_residuos[3],
    limite_emp_95_inf = previsao + quantis_residuos[1],
    limite_emp_95_sup = previsao + quantis_residuos[4]
  )

tabela_previsao_2026_empirica


# Gráfico final da previsão — série completa

g_previsao_2026 <- autoplot(previsao_2026) +
  labs(
    title = "Previsão da temperatura média mensal de Brasília até dezembro de 2026",
    x     = "Ano",
    y     = "Temperatura média mensal (°C)"
  ) +
  theme_minimal()

g_previsao_2026


# Gráfico de previsão com zoom — últimos 36 meses + previsão

dados_zoom <- temperatura_mensal %>%
  filter(data >= max(data) %m-% months(35)) %>%
  select(data, temp_media)

datas_prev <- seq(as.Date("2026-07-01"), as.Date("2026-12-01"), by = "month")

previsao_df <- data.frame(
  data     = datas_prev,
  previsto = as.numeric(previsao_2026$mean),
  lwr80    = as.numeric(previsao_2026$lower[, 1]),
  upr80    = as.numeric(previsao_2026$upper[, 1]),
  lwr95    = as.numeric(previsao_2026$lower[, 2]),
  upr95    = as.numeric(previsao_2026$upper[, 2])
)

g_previsao_2026_zoom <- ggplot() +
  geom_ribbon(
    data = previsao_df,
    aes(x = data, ymin = lwr95, ymax = upr95),
    fill = "steelblue", alpha = 0.15
  ) +
  geom_ribbon(
    data = previsao_df,
    aes(x = data, ymin = lwr80, ymax = upr80),
    fill = "steelblue", alpha = 0.25
  ) +
  geom_line(
    data = dados_zoom,
    aes(x = data, y = temp_media),
    linewidth = 0.5
  ) +
  geom_line(
    data = previsao_df,
    aes(x = data, y = previsto),
    linewidth = 0.8, linetype = "dashed", color = "steelblue"
  ) +
  geom_point(
    data = previsao_df,
    aes(x = data, y = previsto),
    size = 1.8, color = "steelblue"
  ) +
  labs(
    title    = "Previsão da temperatura média mensal de Brasília até dezembro de 2026",
    subtitle = "Intervalos de confiança de 80% e 95%",
    x        = "Data",
    y        = "Temperatura média mensal (°C)"
  ) +
  theme_minimal()

g_previsao_2026_zoom

