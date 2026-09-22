#descarga local de los datos
getwd()
setwd("D:/UTDT/2. SEGUNDO TRIMESTRE/PROGRAMACIÓN ESTADÍSTICA AVANZADA - PYTHON Y R/finales/R")
getwd()


library(haven)

pwt <- read_dta("D:/UTDT/2. SEGUNDO TRIMESTRE/PROGRAMACIÓN ESTADÍSTICA AVANZADA - PYTHON Y R/finales/R/pwt110.dta")
#--------------------

#descarga en linea de los datos
# URL de descarga directa del archivo 
url_pwt <- "https://dataverse.nl/api/access/datafile/554030"

# Descargar solo si el archivo no existe
if (!file.exists("pwt110.dta")) {
  download.file(url_pwt, destfile = "pwt110.dta", mode = "wb")
}

library(haven)
pwt <- read_dta("pwt110.dta")
#-------------------------
library(dplyr)
library(tidyr)
library(fixest)
library(ggplot2)
library(broom)

#exploramos la data
glimpse(pwt)
range(pwt$year)
length(unique(pwt$country))

# Valores faltantes
vars_clave <- c("rgdpo", "rgdpna", "emp", "pop", "hc", "rnna", "labsh",
                "csh_i", "csh_g", "csh_c", "csh_x", "csh_m", "pl_i")

na_tabla <- pwt %>%
  summarise(across(all_of(vars_clave), ~ mean(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "prop_na") %>%
  arrange(desc(prop_na))
print(na_tabla)

ggplot(na_tabla, aes(x = reorder(variable, prop_na), y = prop_na)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Proporción de datos faltantes por variable", x = NULL, y = "% de NA") +
  theme_minimal()

# ¿Cuántos países quedan afuera por no tener el panel completo 1960-2023?
cobertura <- pwt %>%
  filter(year >= 1960, year <= 2023) %>%
  group_by(country) %>%
  summarise(n_validos = sum(!is.na(rgdpo) & !is.na(pop))) %>%
  mutate(completo = n_validos == 64)
table(cobertura$completo)
# Decisión: nos quedamos solo con países con panel completo.
# Los que se caen son en general países chicos o que no existían como tales
# en 1960 -> no es aleatorio, se pierde


#filtramos los datos
pwt1 <- pwt %>%  
  filter(year >= 1960 & year <= 2023) %>%
  group_by(country, countrycode) %>%
  summarise(
    n_validos = sum(!is.na(rgdpo) & !is.na(pop)),
    .groups = "drop"
  ) %>%
  filter(n_validos == 64) %>%   # 1960 a 2023 = 64 años
  pull(country)

length(pwt1) 
print(pwt1)  

#filtramos en un dataframe
pwt2 <- pwt %>%
  filter(country %in% pwt1,
         year >= 1960, year <= 2023)

glimpse(pwt2)
length(pwt2) 
print(pwt2)  

# Variables base
d1 <- pwt2 %>%
  mutate(y = rgdpna / emp,          # producto por trabajador
         ky = rnna / rgdpna,        # razón capital-producto
         alpha = 1 - labsh) %>%     # participación del capital
  filter(year >= 1960, !is.na(y), y > 0)

print(d1)
#estadisticos descriptivos
d2 <- pwt2 %>%
  mutate(y = rgdpna / emp) %>%            # producto por trabajador
  filter(year >= 1960, year <= 2023, !is.na(y), y > 0)

est_b <- d2 %>%
  filter(year %in% c(1960, 2023)) %>%
  group_by(year) %>%
  summarise(
    n        = n(),
    media    = mean(y),
    mediana  = median(y),
    sd       = sd(y),
    minimo   = min(y),
    maximo   = max(y),
    sd_log   = sd(log(y)),
    p10      = quantile(y, 0.10),
    p90      = quantile(y, 0.90),
    ratio_p90_p10 = quantile(y, 0.90) / quantile(y, 0.10)
  )

print(est_b)

# Histograma en niveles (y queda muy sesgado a la derecha)
d2 %>%
  filter(year %in% c(1960, 2023)) %>%
  ggplot(aes(x = y)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  facet_wrap(~ year, scales = "free_x") +
  labs(title = "Distribución del producto por trabajador, 1960 vs 2023",
       x = "Producto por trabajador (USD PPA, precios encadenados)",
       y = "Cantidad de países") +
  theme_minimal()

# Histograma en logaritmos (más informativo para comparar dispersión relativa)
d2 %>%
  filter(year %in% c(1960, 2023)) %>%
  ggplot(aes(x = log(y))) +
  geom_histogram(bins = 20, fill = "darkorange", color = "white") +
  facet_wrap(~ year, scales = "free_x") +
  labs(title = "Distribución del log(producto por trabajador), 1960 vs 2023",
       x = "log(Producto por trabajador)",
       y = "Cantidad de países") +
  theme_minimal()


# Cambios en el ordenamiento (ranking) de países
# Sólo países presentes en ambos años, para que el ranking sea comparable
paises_ambos <- d2 %>%
  filter(year %in% c(1960, 2023)) %>%
  count(countrycode) %>%
  filter(n == 2) %>%
  pull(countrycode)

ranking <- d2 %>%
  filter(year %in% c(1960, 2023), countrycode %in% paises_ambos) %>%
  select(country, year, y) %>%
  group_by(year) %>%
  mutate(ranking = rank(-y)) %>%   # rank(-x): rank 1 = mayor producto por trabajador
  ungroup()

ranking_wide <- ranking %>%
  select(country, year, ranking) %>%
  pivot_wider(names_from = year, values_from = ranking, names_prefix = "rank_") %>%
  mutate(cambio_ranking = rank_1960 - rank_2023)
# cambio_ranking > 0  -> el país SUBIÓ en el ranking (mejoró su posición relativa)
# cambio_ranking < 0  -> el país BAJÓ en el ranking (empeoró su posición relativa)

# Los que más subieron
ranking_wide %>% arrange(desc(cambio_ranking)) %>% head(10)

# Los que más bajaron
ranking_wide %>% arrange(cambio_ranking) %>% head(10)

# Top 10 más productivos en cada año
d2 %>% filter(year == 1960) %>% arrange(desc(y)) %>% select(country, y) %>% head(10)
d2 %>% filter(year == 2023) %>% arrange(desc(y)) %>% select(country, y) %>% head(10)

#-------------------------------
# Panel de períodos quinquenales
panel <- d1 %>%
  mutate(periodo = 1960 + 5 * floor((year - 1960) / 5)) %>%
  group_by(countrycode, periodo) %>%
  summarise(ln_y0 = log(y[which.min(year)]),
            ln_yT = log(y[which.max(year)]),
            span  = max(year) - min(year),
            ln_inv = log(mean(csh_i, na.rm = TRUE)),
            ln_hc  = log(mean(hc, na.rm = TRUE)),
            .groups = "drop") %>%
  filter(span >= 4) %>%
  mutate(g = (ln_yT - ln_y0) / span)   # crecimiento anualizado

# Outliers en el crecimiento
ggplot(panel, aes(x = "", y = g)) +
  geom_boxplot(outlier.colour = "firebrick") +
  labs(title = "Crecimiento anualizado por período (revisión de outliers)",
       x = NULL, y = "Crecimiento anualizado") +
  theme_minimal()

panel %>% arrange(desc(abs(g))) %>%
  select(countrycode, periodo, g) %>% head(10)
# Decisión: son crecimientos altos pero plausibles en promedios de 5 años
# (recuperaciones post-crisis, booms de países chicos), no errores de carga.
# Se dejan en la muestra. Si algún caso fuera claramente un error, se saca
# con: panel <- panel %>% filter(!(countrycode == "XXX" & periodo == YYYY))


# Modelos: de convergencia absoluta a convergencia condicional con EF
m1 <- feols(g ~ ln_y0, panel, cluster = ~countrycode)
m2 <- feols(g ~ ln_y0 + ln_inv + ln_hc, panel, cluster = ~countrycode)
m3 <- feols(g ~ ln_y0 + ln_inv + ln_hc | periodo, panel, cluster = ~countrycode)
m4 <- feols(g ~ ln_y0 + ln_inv + ln_hc | countrycode + periodo, panel, cluster = ~countrycode)

etable(m1, m2, m3, m4,
       headers = c("Absoluta", "Condicional", "EF año", "EF país+año"))

# Gráfico del resultado principal: convergencia beta
ggplot(panel, aes(x = ln_y0, y = g)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "lm", color = "black", se = TRUE) +
  labs(title = "Convergencia beta: nivel inicial vs. crecimiento",
       subtitle = "Cada punto es un país en un período de 5 años",
       x = "log(producto por trabajador) al inicio del período",
       y = "Crecimiento anualizado") +
  theme_minimal()

# Velocidad de convergencia implícita:  beta = -(1 - exp(-lambda*5))/5
lambda <- -log(1 + coef(m4)["ln_y0"] * 5) / 5
cat("Velocidad anual:", round(lambda, 4),
    "| Vida media:", round(log(2) / lambda, 1), "años\n")

da <- d1 %>%
  filter(!is.na(labsh), ky > 0, hc > 0) %>%
  mutate(cap = (alpha / (1 - alpha)) * log(ky),   # contribución del capital
         hum = log(hc),                           # contribución del cap. humano
         ptf = log(y) - cap - hum)                # residuo: PTF

# ¿Cuánto de la desigualdad entre países explican K y H, y cuánto la PTF?
da %>% group_by(year) %>% filter(n() >= 50) %>%
  summarise(explicado_por_factores = var(cap + hum) / var(log(y))) %>%
  tail(10)

# ¿Por qué canal opera la convergencia?
canal <- da %>%
  mutate(periodo = 1960 + 5 * floor((year - 1960) / 5)) %>%
  group_by(countrycode, periodo) %>%
  summarise(ln_y0 = log(y[which.min(year)]),
            span = max(year) - min(year),
            g_cap = (cap[which.max(year)] - cap[which.min(year)]) / span,
            g_hum = (hum[which.max(year)] - hum[which.min(year)]) / span,
            g_ptf = (ptf[which.max(year)] - ptf[which.min(year)]) / span,
            .groups = "drop") %>%
  filter(span >= 4)

etable(feols(g_cap ~ ln_y0 | countrycode + periodo, canal, cluster = ~countrycode),
       feols(g_hum ~ ln_y0 | countrycode + periodo, canal, cluster = ~countrycode),
       feols(g_ptf ~ ln_y0 | countrycode + periodo, canal, cluster = ~countrycode),
       headers = c("Capital", "Capital humano", "PTF"))

#---------------------------------------------------
library(glmnet)
set.seed(0232)

# Predictores: promedios 1980-2023 por país
X <- d1 %>%
  filter(year >= 1980) %>%
  group_by(countrycode) %>%
  summarise(inv      = mean(csh_i, na.rm = TRUE),
            gobierno = mean(csh_g, na.rm = TRUE),
            consumo  = mean(csh_c, na.rm = TRUE),
            apertura = mean(csh_x - csh_m, na.rm = TRUE),
            hc       = mean(hc, na.rm = TRUE),
            labsh    = mean(labsh, na.rm = TRUE),
            precio_i = mean(pl_i, na.rm = TRUE),
            ln_pop   = log(mean(pop, na.rm = TRUE)),
            tasa_emp = mean(emp / pop, na.rm = TRUE),
            .groups  = "drop")

# Variable dependiente: crecimiento de largo plazo + ingreso inicial
Y <- d1 %>%
  filter(year %in% c(1980, max(year))) %>%
  group_by(countrycode) %>%
  filter(n() == 2) %>%
  summarise(ln_y_ini = log(y[which.min(year)]),
            g = (log(y[which.max(year)]) - log(y[which.min(year)])) /
              (max(year) - min(year)))

cs <- inner_join(Y, X, by = "countrycode") %>% na.omit()
cat("Países:", nrow(cs), "\n")

xmat <- as.matrix(select(cs, -countrycode, -g))
cv <- cv.glmnet(xmat, cs$g, alpha = 1, standardize = TRUE)
plot(cv)

# Variables que sobreviven la selección
coef(cv, s = "lambda.1se")

# Post-LASSO: OLS con las variables seleccionadas
sel <- rownames(coef(cv, s = "lambda.1se"))[which(coef(cv, s = "lambda.1se") != 0)][-1]
if (length(sel) == 0) sel <- "ln_y_ini"   # evita que explote si no queda ninguna variable
m_lasso <- lm(reformulate(sel, "g"), data = cs)

# Comparación con el modelo elegido "a mano" (Jones: inversión + capital humano)
m_mano <- lm(g ~ ln_y_ini + inv + hc, data = cs)

summary(m_mano)
summary(m_lasso)

#-----------------------------------------
library(urca)

# Para comparar NIVELES entre países hay que usar rgdpo, no rgdpna
niveles <- pwt %>%
  filter(year >= 1960, !is.na(emp), emp > 0) %>%
  mutate(ln_y = log(rgdpo / emp))

usa <- niveles %>% filter(countrycode == "USA") %>% select(year, ln_y_us = ln_y)

paises <- c("PER", "KOR", "CHL", "ARG", "MEX")

gap <- niveles %>%
  filter(countrycode %in% paises) %>%
  inner_join(usa, by = "year") %>%
  mutate(gap = ln_y - ln_y_us) %>%
  arrange(countrycode, year)

ggplot(gap, aes(year, gap, colour = countrycode)) +
  geom_line() + geom_hline(yintercept = 0, linetype = 2) +
  labs(title = "Brecha de producto por trabajador vs. EE.UU.",
       y = "log(y_i) - log(y_US)", x = NULL,
       colour = "País")

# ADF: H0 = raíz unitaria (shocks permanentes, NO hay convergencia)
# ZA : igual, pero permite un quiebre estructural endógeno
tests <- lapply(paises, function(p) {
  x   <- gap$gap[gap$countrycode == p]
  adf <- ur.df(x, type = "drift", selectlags = "AIC")
  za  <- ur.za(x, model = "both", lag = 2)
  data.frame(pais = p,
             adf      = round(adf@teststat[1], 2),
             adf_cv5  = adf@cval[1, "5pct"],
             za       = round(za@teststat, 2),
             za_cv5   = za@cval[2],
             quiebre  = gap$year[gap$countrycode == p][za@bpoint])
})

do.call(rbind, tests)

# Regla de lectura: si el estadístico es MENOR que el valor crítico al 5%,
# se rechaza la raíz unitaria => hay convergencia estocástica.
# Si ADF no rechaza pero ZA sí, la "divergencia" era un quiebre estructural.

# Detalle de un caso
summary(ur.za(gap$gap[gap$countrycode == "PER"], model = "both", lag = 2))


