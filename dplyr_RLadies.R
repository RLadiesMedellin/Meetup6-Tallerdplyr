# cargar funciones
library(dplyr)
library(lubridate)

# cargar base de datos
url <- file.choose()
dt <- read.csv2(url)

# primeras 6 observaciones
head(dt)

# organizando la base de datos
# %>% 

# ¿cómo funciona Pipe?
rename(dt, educacion = educa, grupo_edad = grupo.de.edad,
       comuna = Comuna, tipo_intervencion = Tipos.de.Intervencion)

# rename
dt %>% 
  rename(educacion = educa, grupo_edad = grupo.de.edad,
         comuna = Comuna, tipo_intervencion = Tipos.de.Intervencion) -> dt 

# mutate
dt %>% 
  mutate(fecha_recepcion = ymd(dmy(fecha_recepcion))) -> dt

# separar por año
dt %>% 
  mutate(a�o = year(fecha_recepcion)) -> dt

# separar por mes
dt %>% 
  mutate(mes = month(fecha_recepcion, label = TRUE)) -> dt

# select
dt %>% 
  select(-fecha_recepcion, -tipo_seguim, -hora_recepcion, -hora_finalizacion) -> dt

# reorganizar la base de datos

# relocate
dt %>% 
  relocate(a�o) -> dt

# .after y .before
dt %>% 
  relocate(mes, .after = a�o) -> dt

# omitiendo na's
dt <- na.omit(dt)


# conociendo la base de datos

# grupo_edad
dt %>% 
  group_by(grupo_edad) %>% 
  summarise(conteo = n()) %>% 
  arrange(by_group = desc(conteo))

dt %>% 
  mutate(sexo = toupper(sexo)) -> dt

# sexo
dt %>% 
  group_by(sexo) %>% 
  summarise(conteo = n()) %>% 
  arrange(by_group = desc(conteo))

# ocupacion
dt %>% 
  group_by(ocupacion) %>% 
  summarise(conteo = n()) %>% 
  arrange(by_group = desc(conteo))

# comuna
dt %>% 
  group_by(comuna) %>% 
  summarise(conteo = n()) %>% 
  arrange(by_group = desc(conteo))

# case_when
dt %>% 
  mutate(
    educacion = case_when(
    educacion %in% c("PRIMARIA COMPLETA", "SECUNDARIA INCOMPLETA",
                     "PRIMARIA INCOMPLETA") ~ "EDUCACION BASICA",
    educacion %in% c("MEDIA TECNICA", "SECUNDARIA COMPLETA",
                     "NORMALISTA") ~ "EDUCACION MEDIA",
    educacion %in% c("PROFESIONAL", "POSTGRADO", "TECNOLOGO") ~ "EDUCACION SUPERIOR",
    educacion %in% c("SD", "SIN INFORMACION") ~ "SD"
  )
  ) -> dt


#######################################################

# filtrar mujeres por estado civil
dt %>% 
  filter(sexo == "F") %>% 
  count(estado_civil)

# ¿cuál es el tema más consultado?
dt %>% 
  filter(sexo == "F" & estado_civil == "CASADO") %>% 
  count(componente) %>% 
  arrange(by_group = desc(n))

# ¿cuál es el rango de edad?
dt %>% 
  filter(sexo == "F" & estado_civil == "CASADO" & componente == "SALUD FAMILIAR") %>% 
  group_by(grupo_edad) %>% 
  summarise(conteo = n())
  arrange(by_group = conteo)


# ¿cuál es la ocupación de este perfil?
  dt %>% 
    filter(sexo == "F" & estado_civil == "CASADO" & componente == "SALUD FAMILIAR", edad >55) %>% 
    count(ocupacion)

  
# comparaciones aumento de llamadas
  dt %>% 
    filter(sexo == "F", mes %in% c("en","feb"), a�o == 2020, dur_llamada > "0:10") %>% 
    summarise(conteo = n()) -> y
  
  dt %>% 
    filter(sexo == "F", mes %in% c("mar","abr","may","jun","jul"), a�o == 2020, dur_llamada > "0:10") %>% 
    summarise(conteo = n()) -> x

  # siendo objetivos
  dt %>% 
    filter(sexo == "F", mes %in% c("mar","abr","may","jun","jul"), a�o == 2019, dur_llamada > "0:10") %>% 
    summarise(conteo = n()) -> z

  
  
      