# Trabajo-Pr-ctico-Integrador---M-dulo-R

Este trabajo estudia si los países más pobres están **achicando la brecha de productividad laboral** que tienen respecto a Estados Unidos, y qué factores explican esa convergencia (o la falta de ella).

Se usa la base de datos **Penn World Table (PWT) 11.0**, que tiene información económica de más de 180 países desde 1950 hasta la actualidad (PBI, empleo, capital, capital humano, precios, etc.).

## Se realizo lo siguiente:

1. **Análisis exploratorio**: se revisaron los datos en busca de valores faltantes, valores atípicos y cómo se distribuyen las variables principales.
2. **Modelo de panel con efectos fijos**: se estimó si los países con menor productividad inicial crecen más rápido (convergencia), controlando por inversión y capital humano.
3. **Selección de variables con LASSO**: se usó esta técnica para elegir, de forma automática, qué variables explican mejor la brecha con EE.UU., y se comparó contra el modelo elegido "a mano".
4. **Series de tiempo**: se analizó si la brecha de productividad de cada país tiene raíz unitaria (es decir, si es un desequilibrio permanente o algo que tiende a corregirse con el tiempo).

## Contenido del repositorio

- `TP_Integrador-R.R` — script de R con todo el análisis, de punta a punta.

## Cómo correr el código

1. Abrir `TP_Integrador-R.R` en RStudio.
2. Instalar los paquetes que pide el script (`dplyr`, `ggplot2`, `fixest`, `glmnet`, `plm`, `urca`, etc.).
3. Correr el script completo. Va a descargar o leer la base de datos, generar los gráficos en la carpeta `figuras/` y mostrar los resultados en la consola.

## Fuente de los datos

Penn World Table versión 11.0 — https://www.rug.nl/ggdc/productivity/pwt/
