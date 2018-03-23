library(tidyverse)
library(scales)
library(ggthemes)


dir <- getwd()
setwd(dir)
venta <- read.csv("venta_2018-03-21_Madrid.csv")
alquiler <- read.csv("alquiler_2018-03-20_Madrid.csv")

venta <- unique(venta)
alquiler <- unique(alquiler)


currency_format <- function(symbol_currency = "$", symbol_position = "before", symbol_spacing = "none", separator_thousand = ",", separator_thousand_interval = 3, separator_decimal = ".", separator_decimal_interval = 3, largest_with_cents = 100000, nsmall = 0L, trim = TRUE, scientific = FALSE, digits = 1L, drop0trailing = TRUE, currency_unit = "", negative_parentheses = FALSE) {
  function(x) {
    # format numeric axis labels
    x <- plyr::round_any(x, 0.01)
    if (max(x, na.rm = TRUE) < largest_with_cents & 
      !all(x == floor(x), na.rm = TRUE)) {
      nsmall <- 2L
    } else {
      x <- plyr::round_any(x, 1)
      nsmall <- 0L
    }
    labels_format <- format(x, nsmall = nsmall, trim = trim, scientific = scientific, digits = digits, drop0trailing = drop0trailing, big.mark = separator_thousand, big.interval = separator_thousand_interval, decimal.mark = separator_decimal, small.interval = separator_decimal_interval)
    # add currency symbol to labels and position according to style
    if (symbol_spacing == "none" & symbol_position == "after")
      labels <- paste0(labels_format, symbol_currency)
    if (symbol_spacing == "single" & symbol_position == "before")
      labels <- paste0(symbol_currency, " ", labels_format)
    if (symbol_spacing == "single" & symbol_position == "after")
      labels <- paste0(labels_format, " ", symbol_currency)
    if (symbol_spacing == "none" & symbol_position == "before")
      labels <- paste0(symbol_currency, labels_format)
    # millions
    if (currency_unit == "million_us")  # overrules label/symbol positions
      labels <- paste0(symbol_currency, labels_format, "M")
    if (currency_unit == "million_uk")  # overrules label/symbol positions
      labels <- paste0(symbol_currency, labels_format, "m")
    if (currency_unit == "million_french")  # overrules label/symbol positions
      labels <- paste0(labels_format, " Mio ", symbol_currency)
    # billions
    if (currency_unit == "billion_us")  # overrules label/symbol positions
      labels <- paste0(symbol_currency, labels_format, "B")
    if (currency_unit == "billion_uk")  # overrules label/symbol positions
      labels <- paste0(symbol_currency, labels_format, "bn")
    if (currency_unit == "billion_french")  # overrules label/symbol positions
      labels <- paste0(labels_format, " Mrd ", symbol_currency)
    
    return(labels)
  }
}
euro_french_format <- function(x, ...) currency_format(symbol_currency = "€", symbol_position = "after", symbol_spacing = "single", separator_thousand = ".", separator_decimal = ",")
euro_french <- euro_french_format()
euro_french_code_format <- function(x, ...) currency_format(symbol_currency = "EUR", symbol_position = "after", symbol_spacing = "single", separator_thousand = ".", separator_decimal = ",")
euro_french_code <- euro_french_code_format()



#### Medias de compraventa

medianas_venta <- venta %>% group_by(Distrito) %>% summarise(N = n(), media = median(Precio, na.rm = TRUE), media_m2 = median(Precio_m2, na.rm = TRUE), m2 = median(Superficie, na.rm = TRUE))
medias_venta <- venta %>% group_by(Distrito) %>% summarise(N = n(), media = mean(Precio, na.rm = TRUE), media_m2 = mean(Precio_m2, na.rm = TRUE), m2 = mean(Superficie, na.rm = TRUE))

distris <- medias_venta$Distrito

for (p in distris) {
  mean <- mean(venta$Precio_m2[venta$Distrito == p], na.rm = TRUE)
  sd <- sd(venta$Precio_m2[venta$Distrito == p], na.rm = TRUE)
  print(mean)
  
  venta$fuera[venta$Distrito == p & venta$Precio_m2 > mean+sd] <- TRUE
  venta$fuera[venta$Distrito == p & venta$Precio_m2 <= mean+sd] <- FALSE
}

venta_fuera <- subset(venta, fuera == TRUE)
venta_dentro <- subset(venta, fuera == FALSE)

medias_fuera_v <- venta_fuera %>% group_by(Distrito) %>% summarise(N = n(), media = median(Precio, na.rm = TRUE), media_m2 = median(Precio_m2, na.rm = TRUE), m2 = median(Superficie, na.rm = TRUE))
medias_dentro_v <- venta_dentro %>% group_by(Distrito) %>% summarise(N = n(), media = median(Precio, na.rm = TRUE), media_m2 = median(Precio_m2, na.rm = TRUE), m2 = median(Superficie, na.rm = TRUE))




#### Medias de alquiler

medianas_alquiler <- alquiler %>% group_by(Distrito) %>% summarise(N = n(), media = median(Precio, na.rm = TRUE), media_m2 = median(Precio_m2, na.rm = TRUE), m2 = median(Superficie, na.rm = TRUE))
medias_alquiler <- alquiler %>% group_by(Distrito) %>% summarise(N = n(), media = mean(Precio, na.rm = TRUE), media_m2 = mean(Precio_m2, na.rm = TRUE), m2 = mean(Superficie, na.rm = TRUE))

distris <- medias_alquiler$Distrito

for (p in distris) {
  mean <- mean(alquiler$Precio_m2[alquiler$Distrito == p], na.rm = TRUE)
  sd <- sd(alquiler$Precio_m2[alquiler$Distrito == p], na.rm = TRUE)
  print(mean)
  
  alquiler$fuera[alquiler$Distrito == p & alquiler$Precio_m2 > mean+sd] <- TRUE
  alquiler$fuera[alquiler$Distrito == p & alquiler$Precio_m2 <= mean+sd] <- FALSE
}

alquiler_fuera <- subset(alquiler, fuera == TRUE)
alquiler_dentro <- subset(alquiler, fuera == FALSE)

medias_fuera_a <- alquiler_fuera %>% group_by(Distrito) %>% summarise(N = n(), media = median(Precio, na.rm = TRUE), media_m2 = median(Precio_m2, na.rm = TRUE), m2 = median(Superficie, na.rm = TRUE))
medias_dentro_a <- alquiler_dentro %>% group_by(Distrito) %>% summarise(N = n(), media = median(Precio, na.rm = TRUE), media_m2 = median(Precio_m2, na.rm = TRUE), m2 = median(Superficie, na.rm = TRUE))


medias_dentro_a$alquiler12 <- medias_dentro_a$media*12
medias_dentro_a$alquiler12_m2 <- medias_dentro_a$media_m2*12

#medias_dentro_a <- medias_dentro_a[medias_dentro_a$N > 80,]
#medias_dentro_v <- medias_dentro_v[medias_dentro_v$N > 80,]

medias_dentro_a <- subset(medias_dentro_a, select = c(1,2,6,7))


# Fusiono los datos de alquiler y venta
df <- merge(medias_dentro_a, medias_dentro_v, by = "Distrito")


# Calculo la rentabilidad
df$rentabilidad <- df$alquiler12/df$media
df$rentabilidad_m2 <- df$alquiler12_m2/df$media_m2


df$Distrito <- as.character(df$Distrito)

df$Distrito[df$Distrito == "Fuencarral"] <- "Fuencarral-El Pardo"
df$Distrito[df$Distrito == "San Blas"] <- "San Blas-Canillejas"
df$Distrito[df$Distrito == "Moncloa"] <- "Moncloa-Aravaca"


renta <- read_delim("~/Dropbox/MASTER/DATOS/R/IDEALISTA/renta y alquiler/renta_barrios.csv", 
                    ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                grouping_mark = "."), trim_ws = TRUE)

q <- str_split(renta$barrio, pattern = "\\.", simplify = TRUE)

q <- as.data.frame(q)

renta <- bind_cols(q, renta)

colnames(renta)[1] <- "cod"
colnames(renta)[2] <- "barrio_"

renta$barrio_ <- as.character(renta$barrio_)

renta[1,1] <- NA
renta[1,2] <- "Madrid"

renta <- renta[-152,]
renta <- renta[-151,]

renta$tipo[nchar(as.character(renta$cod)) == 2] <- "Distrito"
renta$tipo[nchar(as.character(renta$cod)) == 3] <- "Barrio"
renta$tipo[is.na(renta$cod)] <- "Ciudad"

renta$barrio_ <- str_trim(renta$barrio_, "left")

renta <- subset(renta, tipo == "Distrito")


df <- merge(renta, df, by.x = "barrio_", by.y = "Distrito", all = TRUE)




## Plot

p <- ggplot(df, aes(x = reorder(barrio_, rentabilidad_m2), y = rentabilidad_m2)) + geom_col(aes(fill = rentabilidad_m2), show.legend = FALSE) + 
  coord_flip() + 
  scale_y_continuous(labels = percent) +
  labs(title = "Rentabilidad de la inversión inmobiliaria para dedicarla al alquiler",
       subtitle = "La tasa de rentabilidad es anual y está calculada sobre el metro cuadrado.",
       x = NULL,
       y = NULL,
       caption = "Fuente: Idealista y Ayuntamiento de Madrid      //      @hmeleiros") + 
  theme_minimal(base_family = "Roboto Condensed", base_size = 13) +
  theme(axis.title.x = element_text(margin = margin(20,0,0,0)),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.2),
        plot.caption = element_text(margin = margin(20,0,0,0)), 
        axis.text.y = element_text(hjust = 0.5)) +
  geom_text(aes(label = percent(round(rentabilidad_m2, digits = 3)), 
            family = "Roboto Condensed"), 
            nudge_y = -0.005) + 
  scale_fill_gradient2_tableau(palette = "Light Red-Green")

p2 <- ggplot(df, aes(x = reorder(barrio_, rentabilidad_m2), y = renta)) + geom_col(aes(fill = renta), show.legend = FALSE) + 
  coord_flip() + 
  labs(title = "Renta media de los hogares",
       subtitle = "Datos de 2014.",
       x = NULL,
       y = NULL,
       caption = "") + 
  theme_minimal(base_family = "Roboto Condensed", base_size = 13) +
  theme(axis.title.x = element_text(margin = margin(20,0,0,0)),
        plot.title = element_text(face = "bold", hjust = 0.93),
        plot.subtitle = element_text(hjust = 0.95),
        plot.caption = element_text(margin = margin(20,0,0,0))) + 
  scale_y_reverse(labels = euro_french) + 
  scale_x_discrete(labels = NULL) +
  geom_text(aes(label = euro_french(round(renta, digits = 1))), 
            family = "Roboto Condensed", 
            nudge_y = 6000) + 
  scale_fill_gradient2_tableau(palette = "Light Red-Green")


grid.arrange(p2,p,ncol=2)




## Calculo la rentabilidad de cada inmueble en función del precio medio del alquiler en el distrito
venta$rentabilidad
venta <- merge(venta, medias_dentro_a, by = "Distrito")
venta$rentabilidad_m2 <- venta$alquiler12_m2/venta$Precio_m2
