# Projeto 2 - Mudança estrutural no comércio varejista brasileiro após a COVID-19
# PMC - Número-índice (2022 = 100) | Índice de volume de vendas no comércio varejista
# Referência: Cryer & Chan (2008), slides e orientações do professor
# Camila Lima


###### Pacotes ######

if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  sidrar,
  dplyr,
  tidyr,
  purrr,
  tibble,
  lubridate,
  ggplot2,
  forecast,
  tseries,
  moments,
  strucchange
)



###### Variáveis auxiliares ######

nomes_meses <- c(
  "Jan", "Fev", "Mar", "Abr", "Mai", "Jun",
  "Jul", "Ago", "Set", "Out", "Nov", "Dez"
)


###### Obtenção dos dados - PMC/SIDRA ######

dados_brutos <- get_sidra(
  api = "/t/8880/n1/all/v/7169/p/all/c11046/56734/d/v7169%201"
)

names(dados_brutos)
head(dados_brutos)
tail(dados_brutos)
summary(dados_brutos)


###### Tratamento inicial dos dados ######

pmc <- dados_brutos %>%
  mutate(
    periodo_codigo = as.character(`Mês (Código)`),
    ano = as.integer(substr(periodo_codigo, 1, 4)),
    mes = as.integer(substr(periodo_codigo, 5, 6)),
    data = as.Date(paste(ano, mes, "01", sep = "-")),
    indice_volume = as.numeric(Valor),
    mes_abrev = factor(nomes_meses[mes], levels = nomes_meses),
    periodo_covid = case_when(
      data <= as.Date("2019-12-01") ~ "Pré-COVID",
      data >= as.Date("2020-01-01") ~ "Pós-COVID"
    ),
    periodo_covid = factor(periodo_covid, levels = c("Pré-COVID", "Pós-COVID"))
  ) %>%
  select(data, ano, mes, mes_abrev, periodo_covid, indice_volume) %>%
  arrange(data)

head(pmc)
tail(pmc)
dim(pmc)
summary(pmc)


###### Valores faltantes ######

faltantes_pmc <- pmc %>%
  summarise(
    n = n(),
    n_faltantes = sum(is.na(indice_volume)),
    prop_faltantes = n_faltantes / n
  )

faltantes_pmc

pmc %>%
  filter(is.na(indice_volume))


###### Série temporal mensal ######

inicio_serie <- c(pmc$ano[1], pmc$mes[1])

serie_pmc <- ts(
  pmc$indice_volume,
  start = inicio_serie,
  frequency = 12
)

plot(
  serie_pmc,
  main = "Índice de volume de vendas no comércio varejista",
  ylab = "Número-índice (2022 = 100)",
  xlab = "Ano"
)


###### Análise exploratória ######

periodo_serie <- pmc %>%
  summarise(
    data_inicial = min(data),
    data_final = max(data),
    n_meses = n()
  )

periodo_serie

tamanho_periodos <- pmc %>%
  group_by(periodo_covid) %>%
  summarise(
    data_inicial = min(data),
    data_final = max(data),
    n_meses = n(),
    n_anos_aprox = n_meses / 12,
    .groups = "drop"
  )

tamanho_periodos

estatisticas_gerais <- pmc %>%
  summarise(
    n = n(),
    media = mean(indice_volume, na.rm = TRUE),
    mediana = median(indice_volume, na.rm = TRUE),
    desvio_padrao = sd(indice_volume, na.rm = TRUE),
    variancia = var(indice_volume, na.rm = TRUE),
    coef_variacao = 100 * sd(indice_volume, na.rm = TRUE) / mean(indice_volume, na.rm = TRUE),
    minimo = min(indice_volume, na.rm = TRUE),
    q1 = quantile(indice_volume, 0.25, na.rm = TRUE),
    q3 = quantile(indice_volume, 0.75, na.rm = TRUE),
    maximo = max(indice_volume, na.rm = TRUE),
    amplitude = max(indice_volume, na.rm = TRUE) - min(indice_volume, na.rm = TRUE)
  )

estatisticas_gerais

estatisticas_periodo <- pmc %>%
  group_by(periodo_covid) %>%
  summarise(
    n = n(),
    media = mean(indice_volume, na.rm = TRUE),
    mediana = median(indice_volume, na.rm = TRUE),
    desvio_padrao = sd(indice_volume, na.rm = TRUE),
    variancia = var(indice_volume, na.rm = TRUE),
    coef_variacao = 100 * sd(indice_volume, na.rm = TRUE) / mean(indice_volume, na.rm = TRUE),
    minimo = min(indice_volume, na.rm = TRUE),
    q1 = quantile(indice_volume, 0.25, na.rm = TRUE),
    q3 = quantile(indice_volume, 0.75, na.rm = TRUE),
    maximo = max(indice_volume, na.rm = TRUE),
    amplitude = max(indice_volume, na.rm = TRUE) - min(indice_volume, na.rm = TRUE),
    .groups = "drop"
  )

estatisticas_periodo

estatisticas_mensais_periodo <- pmc %>%
  group_by(periodo_covid, mes, mes_abrev) %>%
  summarise(
    n = n(),
    media = mean(indice_volume, na.rm = TRUE),
    mediana = median(indice_volume, na.rm = TRUE),
    desvio_padrao = sd(indice_volume, na.rm = TRUE),
    minimo = min(indice_volume, na.rm = TRUE),
    q1 = quantile(indice_volume, 0.25, na.rm = TRUE),
    q3 = quantile(indice_volume, 0.75, na.rm = TRUE),
    maximo = max(indice_volume, na.rm = TRUE),
    amplitude = max(indice_volume, na.rm = TRUE) - min(indice_volume, na.rm = TRUE),
    cv = 100 * sd(indice_volume, na.rm = TRUE) / mean(indice_volume, na.rm = TRUE),
    .groups = "drop"
  )

estatisticas_mensais_periodo

estatisticas_anuais <- pmc %>%
  group_by(ano) %>%
  summarise(
    n_meses = n(),
    media_anual = mean(indice_volume, na.rm = TRUE),
    mediana_anual = median(indice_volume, na.rm = TRUE),
    dp_anual = sd(indice_volume, na.rm = TRUE),
    minimo_anual = min(indice_volume, na.rm = TRUE),
    maximo_anual = max(indice_volume, na.rm = TRUE),
    amplitude_anual = max(indice_volume, na.rm = TRUE) - min(indice_volume, na.rm = TRUE),
    .groups = "drop"
  )

estatisticas_anuais

meses_por_ano <- pmc %>%
  group_by(ano) %>%
  summarise(
    n_meses = n(),
    meses_disponiveis = paste(mes, collapse = ", "),
    .groups = "drop"
  )

meses_por_ano

anos_incompletos <- meses_por_ano %>%
  filter(n_meses < 12)

anos_incompletos

meses_maiores_indices <- pmc %>%
  arrange(desc(indice_volume)) %>%
  slice(1:10)

meses_menores_indices <- pmc %>%
  arrange(indice_volume) %>%
  slice(1:10)

meses_maiores_indices
meses_menores_indices


###### Gráficos exploratórios ######

g_serie_completa <- ggplot(pmc, aes(x = data, y = indice_volume)) +
  geom_line(linewidth = 0.4) +
  geom_vline(xintercept = as.Date("2020-01-01"), linetype = "dashed", linewidth = 0.8) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 0.8) +
  labs(
    title = "Índice de volume de vendas no comércio varejista",
    subtitle = "PMC/SIDRA — número-índice (2022 = 100), com corte em janeiro de 2020",
    x = "Ano",
    y = "Índice de volume de vendas"
  ) +
  theme_minimal()

g_serie_completa

pmc$media_movel_12 <- as.numeric(
  stats::filter(pmc$indice_volume, filter = rep(1 / 12, 12), sides = 2)
)

g_media_movel <- ggplot(pmc, aes(x = data)) +
  geom_line(aes(y = indice_volume), linewidth = 0.35, alpha = 0.6) +
  geom_line(aes(y = media_movel_12), linewidth = 0.9) +
  geom_vline(xintercept = as.Date("2020-01-01"), linetype = "dashed", linewidth = 0.8) +
  labs(
    title = "Índice de volume de vendas e média móvel de 12 meses",
    subtitle = "Linha vertical indica janeiro de 2020",
    x = "Ano",
    y = "Índice de volume de vendas"
  ) +
  theme_minimal()

g_media_movel

g_media_anual <- ggplot(estatisticas_anuais, aes(x = ano, y = media_anual)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 2020, linetype = "dashed", linewidth = 0.8) +
  labs(
    title = "Média anual do índice de volume de vendas",
    x = "Ano",
    y = "Índice médio anual"
  ) +
  theme_minimal()

g_media_anual

g_dp_anual <- ggplot(estatisticas_anuais, aes(x = ano, y = dp_anual)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 2020, linetype = "dashed", linewidth = 0.8) +
  labs(
    title = "Variabilidade anual do índice de volume de vendas",
    x = "Ano",
    y = "Desvio-padrão anual"
  ) +
  theme_minimal()

g_dp_anual

g_box_periodo <- ggplot(pmc, aes(x = periodo_covid, y = indice_volume)) +
  geom_boxplot() +
  labs(
    title = "Distribuição do índice de volume de vendas por período",
    x = "Período",
    y = "Índice de volume de vendas"
  ) +
  theme_minimal()

g_box_periodo

g_hist_periodo <- ggplot(pmc, aes(x = indice_volume)) +
  geom_histogram(aes(y = after_stat(density)), bins = 25) +
  geom_density(linewidth = 0.8) +
  facet_wrap(~ periodo_covid, scales = "free_y") +
  labs(
    title = "Distribuição do índice de volume de vendas por período",
    x = "Índice de volume de vendas",
    y = "Densidade"
  ) +
  theme_minimal()

g_hist_periodo

g_box_mes_periodo <- ggplot(pmc, aes(x = mes_abrev, y = indice_volume, fill = periodo_covid)) +
  geom_boxplot() +
  labs(
    title = "Distribuição mensal do índice de volume de vendas",
    subtitle = "Comparação entre períodos pré-COVID e pós-COVID",
    x = "Mês",
    y = "Índice de volume de vendas",
    fill = "Período"
  ) +
  theme_minimal()

g_box_mes_periodo

perfil_sazonal_periodo <- pmc %>%
  group_by(periodo_covid, mes, mes_abrev) %>%
  summarise(
    media_mensal = mean(indice_volume, na.rm = TRUE),
    .groups = "drop"
  )

g_perfil_sazonal_periodo <- ggplot(
  perfil_sazonal_periodo,
  aes(x = mes_abrev, y = media_mensal, group = periodo_covid, linetype = periodo_covid)
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  labs(
    title = "Perfil sazonal médio do comércio varejista",
    subtitle = "Comparação entre períodos pré-COVID e pós-COVID",
    x = "Mês",
    y = "Índice médio de volume de vendas",
    linetype = "Período"
  ) +
  theme_minimal()

g_perfil_sazonal_periodo

g_serie_periodo <- ggplot(pmc, aes(x = data, y = indice_volume, color = periodo_covid)) +
  geom_line(linewidth = 0.5) +
  geom_vline(xintercept = as.Date("2020-01-01"), linetype = "dashed", linewidth = 0.8) +
  labs(
    title = "Índice de volume de vendas no comércio varejista",
    subtitle = "Corte pré-COVID e pós-COVID",
    x = "Ano",
    y = "Índice de volume de vendas",
    color = "Período"
  ) +
  theme_minimal()

g_serie_periodo


###### Detecção descritiva de quebra estrutural ######

tempo_pmc <- 1:length(serie_pmc)

bp_nivel <- breakpoints(
  as.numeric(serie_pmc) ~ 1,
  h = 0.10
)

summary(bp_nivel)

plot(bp_nivel, main = "Seleção do número de quebras (BIC e RSS)")

indices_quebra <- breakpoints(bp_nivel)$breakpoints
n_quebras_otimo <- length(indices_quebra)
datas_quebra <- pmc$data[indices_quebra]

indices_quebra
n_quebras_otimo
datas_quebra

ic_quebras <- confint(bp_nivel)

datas_ic_quebras <- data.frame(
  quebra = seq_along(indices_quebra),
  data_inferior = pmc$data[ic_quebras$confint[, 1]],
  data_estimada = pmc$data[ic_quebras$confint[, 2]],
  data_superior = pmc$data[ic_quebras$confint[, 3]]
)

datas_ic_quebras

bp_tendencia <- breakpoints(
  as.numeric(serie_pmc) ~ tempo_pmc,
  h = 0.10
)

summary(bp_tendencia)
breakdates(bp_tendencia)

indices_quebra_tend <- breakpoints(bp_tendencia)$breakpoints
datas_quebra_tend <- pmc$data[indices_quebra_tend]

indices_quebra_tend
datas_quebra_tend

g_quebra_estrutural <- ggplot(pmc, aes(x = data, y = indice_volume)) +
  geom_line(linewidth = 0.4) +
  geom_vline(
    xintercept = as.Date("2020-01-01"),
    linetype = "dashed",
    color = "black",
    linewidth = 0.8
  ) +
  geom_vline(
    xintercept = datas_quebra,
    linetype = "solid",
    color = "red",
    linewidth = 0.8
  ) +
  labs(
    title = "Índice de volume de vendas com quebra(s) estrutural(is) estimada(s)",
    subtitle = "Tracejada: corte a priori (jan/2020) | Vermelha: quebra estimada (Bai-Perron)",
    x = "Ano",
    y = "Índice de volume de vendas"
  ) +
  theme_minimal()

g_quebra_estrutural


###### Construção das séries pré e pós-COVID ######

pmc_pre <- pmc %>%
  filter(data <= as.Date("2019-12-01"))

pmc_pos <- pmc %>%
  filter(data >= as.Date("2020-01-01"))

serie_pre <- ts(
  pmc_pre$indice_volume,
  start = c(pmc_pre$ano[1], pmc_pre$mes[1]),
  frequency = 12
)

serie_pos <- ts(
  pmc_pos$indice_volume,
  start = c(pmc_pos$ano[1], pmc_pos$mes[1]),
  frequency = 12
)

length(serie_pre)
length(serie_pos)

plot(
  serie_pre,
  main = "Série pré-COVID",
  ylab = "Índice de volume de vendas",
  xlab = "Ano"
)

plot(
  serie_pos,
  main = "Série pós-COVID",
  ylab = "Índice de volume de vendas",
  xlab = "Ano"
)


###### ACF, PACF, STL e estacionariedade ######

ggtsdisplay(serie_pmc, main = "Série completa, ACF e PACF")

ggtsdisplay(serie_pre, main = "Série pré-COVID, ACF e PACF")

ggtsdisplay(serie_pos, main = "Série pós-COVID, ACF e PACF")

Acf(serie_pre, lag.max = 60, main = "ACF da série pré-COVID")
Pacf(serie_pre, lag.max = 60, main = "PACF da série pré-COVID")
Acf(serie_pos, lag.max = min(60, length(serie_pos) - 1), main = "ACF da série pós-COVID")
Pacf(serie_pos, lag.max = min(60, length(serie_pos) - 1), main = "PACF da série pós-COVID")

series_acf <- list("Pré-COVID" = serie_pre, "Pós-COVID" = serie_pos)

acf_sazonal_comparacao <- imap_dfr(
  series_acf,
  ~ {
    acf_obj <- acf(.x, lag.max = min(60, length(.x) - 1), plot = FALSE)

    data.frame(
      periodo = .y,
      lag = round(as.numeric(acf_obj$lag) * frequency(.x)),
      acf = as.numeric(acf_obj$acf)
    )
  }
) %>%
  filter(lag %in% c(12, 24, 36, 48, 60))

acf_sazonal_comparacao

decomp_completa <- stl(serie_pmc, s.window = "periodic")
decomp_pre <- stl(serie_pre, s.window = "periodic")
decomp_pos <- stl(serie_pos, s.window = "periodic")

plot(decomp_completa, main = "Decomposição STL — Comércio varejista")

plot(decomp_pre, main = "Decomposição STL — Período pré-COVID")

plot(decomp_pos, main = "Decomposição STL — Período pós-COVID")

series_estacionariedade <- list(
  "Completa" = serie_pmc,
  "Pré-COVID" = serie_pre,
  "Pós-COVID" = serie_pos
)

testes_estacionariedade_final <- imap_dfr(
  series_estacionariedade,
  ~ {
    adf_serie <- adf.test(.x)
    kpss_nivel <- kpss.test(.x, null = "Level")
    kpss_tendencia <- kpss.test(.x, null = "Trend")

    data.frame(
      serie = .y,
      teste = c("ADF", "KPSS nível", "KPSS tendência"),
      estatistica = c(
        as.numeric(adf_serie$statistic),
        as.numeric(kpss_nivel$statistic),
        as.numeric(kpss_tendencia$statistic)
      ),
      p_valor = c(
        adf_serie$p.value,
        kpss_nivel$p.value,
        kpss_tendencia$p.value
      )
    )
  }
)

testes_estacionariedade_final


###### Identificação SARIMA ######

serie_completa_d1_D1 <- diff(diff(serie_pmc, differences = 1), lag = 12, differences = 1)
serie_pre_d1_D1 <- diff(diff(serie_pre, differences = 1), lag = 12, differences = 1)
serie_pos_d1_D1 <- diff(diff(serie_pos, differences = 1), lag = 12, differences = 1)
serie_pos_D1 <- diff(serie_pos, lag = 12, differences = 1)

adf_diferencas <- bind_rows(
  data.frame(serie = "Completa", transformacao = "Original", estatistica = as.numeric(adf.test(serie_pmc)$statistic), p_valor = adf.test(serie_pmc)$p.value),
  data.frame(serie = "Completa", transformacao = "d=1, D=1", estatistica = as.numeric(adf.test(serie_completa_d1_D1)$statistic), p_valor = adf.test(serie_completa_d1_D1)$p.value),
  data.frame(serie = "Pré-COVID", transformacao = "Original", estatistica = as.numeric(adf.test(serie_pre)$statistic), p_valor = adf.test(serie_pre)$p.value),
  data.frame(serie = "Pré-COVID", transformacao = "d=1, D=1", estatistica = as.numeric(adf.test(serie_pre_d1_D1)$statistic), p_valor = adf.test(serie_pre_d1_D1)$p.value),
  data.frame(serie = "Pós-COVID", transformacao = "Original", estatistica = as.numeric(adf.test(serie_pos)$statistic), p_valor = adf.test(serie_pos)$p.value),
  data.frame(serie = "Pós-COVID", transformacao = "d=1, D=1", estatistica = as.numeric(adf.test(serie_pos_d1_D1)$statistic), p_valor = adf.test(serie_pos_d1_D1)$p.value),
  data.frame(serie = "Pós-COVID", transformacao = "D=1", estatistica = as.numeric(adf.test(serie_pos_D1)$statistic), p_valor = adf.test(serie_pos_D1)$p.value)
)

adf_diferencas

comparacao_diferencas_pos <- data.frame(
  transformacao = c("d=1, D=1", "D=1"),
  variancia = c(var(serie_pos_d1_D1), var(serie_pos_D1))
)

comparacao_diferencas_pos

ggtsdisplay(serie_completa_d1_D1, main = "Completa: diferença regular + sazonal")
ggtsdisplay(serie_pre_d1_D1, main = "Pré-COVID: diferença regular + sazonal")
ggtsdisplay(serie_pos_d1_D1, main = "Pós-COVID: diferença regular + sazonal")
ggtsdisplay(serie_pos_D1, main = "Pós-COVID: apenas diferença sazonal")

auto_completa <- auto.arima(serie_pmc, stepwise = FALSE, approximation = FALSE)
auto_pre <- auto.arima(serie_pre, stepwise = FALSE, approximation = FALSE)
auto_pos <- auto.arima(serie_pos, stepwise = FALSE, approximation = FALSE)

summary(auto_completa)
summary(auto_pre)
summary(auto_pos)


###### Modelos SARIMA candidatos ######

mod_completa_1 <- Arima(serie_pmc, order = c(0, 1, 4), seasonal = list(order = c(0, 1, 1), period = 12))
mod_completa_A <- Arima(serie_pmc, order = c(1, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12))
mod_completa_B <- Arima(serie_pmc, order = c(0, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12))
mod_completa_C <- Arima(serie_pmc, order = c(0, 1, 2), seasonal = list(order = c(0, 1, 1), period = 12))
mod_completa_D <- Arima(serie_pmc, order = c(1, 1, 2), seasonal = list(order = c(0, 1, 1), period = 12))

mod_pre_1 <- Arima(serie_pre, order = c(1, 1, 3), seasonal = list(order = c(0, 1, 1), period = 12))
mod_pre_A <- Arima(serie_pre, order = c(2, 1, 0), seasonal = list(order = c(0, 1, 1), period = 12))
mod_pre_B <- Arima(serie_pre, order = c(0, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12))
mod_pre_C <- Arima(serie_pre, order = c(1, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12))

mod_pos_1 <- Arima(serie_pos, order = c(4, 0, 0), seasonal = list(order = c(0, 1, 0), period = 12), include.drift = TRUE)
mod_pos_2 <- Arima(serie_pos, order = c(1, 0, 0), seasonal = list(order = c(0, 1, 0), period = 12), include.drift = TRUE)
mod_pos_3 <- Arima(serie_pos, order = c(2, 0, 0), seasonal = list(order = c(0, 1, 0), period = 12), include.drift = TRUE)

lista_completa <- list(
  "SARIMA(0,1,4)(0,1,1)[12] [auto]" = mod_completa_1,
  "SARIMA(1,1,1)(0,1,1)[12]" = mod_completa_A,
  "SARIMA(0,1,1)(0,1,1)[12]" = mod_completa_B,
  "SARIMA(0,1,2)(0,1,1)[12]" = mod_completa_C,
  "SARIMA(1,1,2)(0,1,1)[12]" = mod_completa_D
)

lista_pre <- list(
  "SARIMA(1,1,3)(0,1,1)[12] [auto]" = mod_pre_1,
  "SARIMA(2,1,0)(0,1,1)[12]" = mod_pre_A,
  "SARIMA(0,1,1)(0,1,1)[12]" = mod_pre_B,
  "SARIMA(1,1,1)(0,1,1)[12]" = mod_pre_C
)

lista_pos <- list(
  "SARIMA(4,0,0)(0,1,0)[12] com drift [auto]" = mod_pos_1,
  "SARIMA(1,0,0)(0,1,0)[12] com drift" = mod_pos_2,
  "SARIMA(2,0,0)(0,1,0)[12] com drift" = mod_pos_3
)

listas_modelos <- list(
  "Completa" = lista_completa,
  "Pré-COVID" = lista_pre,
  "Pós-COVID" = lista_pos
)

comparacao_modelos_candidatos <- imap_dfr(
  listas_modelos,
  ~ tibble(
    periodo = .y,
    modelo = names(.x),
    AIC = map_dbl(.x, AIC),
    AICc = map_dbl(.x, ~ .x$aicc),
    BIC = map_dbl(.x, BIC),
    logLik = map_dbl(.x, ~ as.numeric(logLik(.x)))
  )
) %>%
  arrange(periodo, AICc, BIC)

comparacao_modelos_candidatos


###### Diagnóstico residual ######

diagnostico_candidatos <- bind_rows(
  diagnostico_modelo(mod_completa_1, "Completa - SARIMA(0,1,4)(0,1,1)[12]", 24, "completa_auto"),
  diagnostico_modelo(mod_completa_C, "Completa - SARIMA(0,1,2)(0,1,1)[12]", 24, "completa_c"),
  diagnostico_modelo(mod_completa_D, "Completa - SARIMA(1,1,2)(0,1,1)[12]", 24, "completa_d"),
  diagnostico_modelo(mod_pre_1, "Pré - SARIMA(1,1,3)(0,1,1)[12]", 24, "pre_auto"),
  diagnostico_modelo(mod_pre_A, "Pré - SARIMA(2,1,0)(0,1,1)[12]", 24, "pre_a"),
  diagnostico_modelo(mod_pos_1, "Pós - SARIMA(4,0,0)(0,1,0)[12] com drift", 12, "pos_auto"),
  diagnostico_modelo(mod_pos_2, "Pós - SARIMA(1,0,0)(0,1,0)[12] com drift", 12, "pos_a")
)

diagnostico_candidatos


###### Modelos finais ######

mod_completa_final <- mod_completa_1
mod_pre_final <- mod_pre_A
mod_pos_final <- mod_pos_2

summary(mod_completa_final)
summary(mod_pre_final)
summary(mod_pos_final)

modelos_finais <- list(
  "Tradicional - completa" = mod_completa_final,
  "Pré-COVID" = mod_pre_final,
  "Pós-COVID" = mod_pos_final
)

coeficientes_finais <- imap_dfr(
  modelos_finais,
  ~ data.frame(
    modelo = .y,
    parametro = names(coef(.x)),
    estimativa = as.numeric(coef(.x)),
    erro_padrao = sqrt(diag(.x$var.coef))
  )
)

coeficientes_finais

comparacao_modelos_finais <- data.frame(
  periodo = c("Completa", "Pré-COVID", "Pós-COVID"),
  interpretacao = c("Modelo tradicional", "Regime pré-pandemia", "Regime pós-pandemia"),
  modelo = c(
    "SARIMA(0,1,4)(0,1,1)[12]",
    "SARIMA(2,1,0)(0,1,1)[12]",
    "SARIMA(1,0,0)(0,1,0)[12] com drift"
  ),
  AIC = c(AIC(mod_completa_final), AIC(mod_pre_final), AIC(mod_pos_final)),
  AICc = c(mod_completa_final$aicc, mod_pre_final$aicc, mod_pos_final$aicc),
  BIC = c(BIC(mod_completa_final), BIC(mod_pre_final), BIC(mod_pos_final)),
  logLik = c(
    as.numeric(logLik(mod_completa_final)),
    as.numeric(logLik(mod_pre_final)),
    as.numeric(logLik(mod_pos_final))
  )
)

comparacao_modelos_finais

diagnostico_modelos_finais <- tibble(
  modelo = modelos_finais,
  nome = names(modelos_finais),
  lag_ljung = c(24, 24, 12)
) %>%
  pmap_dfr(
    ~ {
      residuos <- residuals(..1)

      ljung <- Box.test(
        residuos,
        lag = ..3,
        type = "Ljung-Box",
        fitdf = length(coef(..1))
      )

      mcleod_li <- Box.test(
        residuos^2,
        lag = min(12, ..3),
        type = "Ljung-Box"
      )

      shapiro <- shapiro.test(as.numeric(residuos))
      jb <- moments::jarque.test(as.numeric(residuos))

      data.frame(
        modelo = ..2,
        lag_ljung = ..3,
        fitdf = length(coef(..1)),
        ljung_box = as.numeric(ljung$statistic),
        p_valor_ljung = ljung$p.value,
        mcleod_li = as.numeric(mcleod_li$statistic),
        p_valor_mcleod_li = mcleod_li$p.value,
        shapiro_wilk = as.numeric(shapiro$statistic),
        p_valor_shapiro = shapiro$p.value,
        jarque_bera = as.numeric(jb$statistic),
        p_valor_jarque_bera = jb$p.value
      )
    }
  )

diagnostico_modelos_finais

checkresiduals(mod_completa_final)
checkresiduals(mod_pre_final)
checkresiduals(mod_pos_final)

qqnorm(residuals(mod_completa_final), main = "QQ-plot - modelo tradicional")
qqline(residuals(mod_completa_final))

qqnorm(residuals(mod_pre_final), main = "QQ-plot - modelo pré-COVID")
qqline(residuals(mod_pre_final))

qqnorm(residuals(mod_pos_final), main = "QQ-plot - modelo pós-COVID")
qqline(residuals(mod_pos_final))


###### Falha preditiva: pré-COVID prevê pós-COVID ######

h_pos <- length(serie_pos)

set.seed(190104040)

prev_falha_preditiva <- forecast(
  mod_pre_final,
  h = h_pos,
  bootstrap = TRUE,
  npaths = 2000
)

erro_falha_preditiva <- accuracy(prev_falha_preditiva, serie_pos)
erro_falha_preditiva

df_falha_preditiva <- data.frame(
  data = pmc_pos$data,
  observado = as.numeric(serie_pos),
  previsto = as.numeric(prev_falha_preditiva$mean),
  li_80 = as.numeric(prev_falha_preditiva$lower[, 1]),
  ls_80 = as.numeric(prev_falha_preditiva$upper[, 1]),
  li_95 = as.numeric(prev_falha_preditiva$lower[, 2]),
  ls_95 = as.numeric(prev_falha_preditiva$upper[, 2])
) %>%
  mutate(
    erro = observado - previsto,
    erro_abs = abs(erro),
    dentro_ic95 = observado >= li_95 & observado <= ls_95
  )

prop_fora_ic95 <- mean(!df_falha_preditiva$dentro_ic95)
vies_medio_falha <- mean(df_falha_preditiva$erro)

prop_fora_ic95
vies_medio_falha

tabela_falha_preditiva <- data.frame(
  metrica = c("MAE", "RMSE", "MAPE", "Viés médio", "Proporção fora do IC 95%"),
  valor = c(
    erro_falha_preditiva["Test set", "MAE"],
    erro_falha_preditiva["Test set", "RMSE"],
    erro_falha_preditiva["Test set", "MAPE"],
    vies_medio_falha,
    prop_fora_ic95
  )
)

tabela_falha_preditiva

g_falha_preditiva <- ggplot(df_falha_preditiva, aes(x = data)) +
  geom_ribbon(aes(ymin = li_95, ymax = ls_95), fill = "grey80", alpha = 0.6) +
  geom_ribbon(aes(ymin = li_80, ymax = ls_80), fill = "grey60", alpha = 0.6) +
  geom_line(aes(y = previsto), color = "blue", linewidth = 0.8) +
  geom_line(aes(y = observado), color = "black", linewidth = 0.8) +
  labs(
    title = "Falha preditiva: modelo pré-COVID prevendo o período pós-COVID",
    subtitle = "Preta: observado | Azul: previsto | Faixas: IC empírico 80%/95% (bootstrap)",
    x = "Ano",
    y = "Índice de volume de vendas"
  ) +
  theme_minimal()

g_falha_preditiva


###### Holdout recente ######

h_holdout <- 12
n_pos <- length(serie_pos)
n_completa <- length(serie_pmc)

serie_pos_treino <- window(serie_pos, end = time(serie_pos)[n_pos - h_holdout])
serie_pos_teste <- window(serie_pos, start = time(serie_pos)[n_pos - h_holdout + 1])
serie_completa_treino <- window(serie_pmc, end = time(serie_pmc)[n_completa - h_holdout])

mod_completa_holdout <- Arima(
  serie_completa_treino,
  order = c(0, 1, 4),
  seasonal = list(order = c(0, 1, 1), period = 12)
)

mod_pos_holdout <- Arima(
  serie_pos_treino,
  order = c(1, 0, 0),
  seasonal = list(order = c(0, 1, 0), period = 12),
  include.drift = TRUE
)

prev_completa_holdout <- forecast(
  mod_completa_holdout,
  h = h_holdout,
  bootstrap = TRUE,
  npaths = 2000
)

prev_pos_holdout <- forecast(
  mod_pos_holdout,
  h = h_holdout,
  bootstrap = TRUE,
  npaths = 2000
)

erro_completa_holdout <- accuracy(prev_completa_holdout, serie_pos_teste)
erro_pos_holdout <- accuracy(prev_pos_holdout, serie_pos_teste)

comparacao_holdout <- data.frame(
  modelo = c("Tradicional (toda a história)", "Proposto (só pós-COVID)"),
  RMSE = c(
    erro_completa_holdout["Test set", "RMSE"],
    erro_pos_holdout["Test set", "RMSE"]
  ),
  MAE = c(
    erro_completa_holdout["Test set", "MAE"],
    erro_pos_holdout["Test set", "MAE"]
  ),
  MAPE = c(
    erro_completa_holdout["Test set", "MAPE"],
    erro_pos_holdout["Test set", "MAPE"]
  )
)

comparacao_holdout

df_holdout <- data.frame(
  data = tail(pmc_pos$data, h_holdout),
  observado = as.numeric(serie_pos_teste),
  previsto_tradicional = as.numeric(prev_completa_holdout$mean),
  previsto_proposto = as.numeric(prev_pos_holdout$mean)
)

df_holdout

g_holdout <- ggplot(df_holdout, aes(x = data)) +
  geom_line(aes(y = observado, color = "Observado"), linewidth = 1) +
  geom_line(aes(y = previsto_tradicional, color = "Tradicional"), linewidth = 0.8, linetype = "dashed") +
  geom_line(aes(y = previsto_proposto, color = "Proposto (pós-COVID)"), linewidth = 0.8, linetype = "dashed") +
  labs(
    title = "Holdout dos últimos 12 meses: tradicional vs. proposto",
    x = "Ano",
    y = "Índice de volume de vendas",
    color = ""
  ) +
  theme_minimal()

g_holdout

