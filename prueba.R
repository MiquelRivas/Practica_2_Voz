```{r}
table(datos$higiene_vocal)

data <- datos %>% 
  dplyr::mutate(higiene_vocal = case_when(
    higiene_vocal == "Bec molta d'aigua" ~ "Mucha agua.",
    higiene_vocal == "Bec molta d'aigua, No seguesc cap rutina per a la veu" ~ "Mucha agua, no rutina.",
    higiene_vocal == "Escalf la veu abans de començar la feina, Faig repòs vocal després de la feina" ~ "Calienta y reposo.",
    higiene_vocal == "Escalf la veu abans de començar la feina, Faig repòs vocal després de la feina, Bec molta d'aigua" ~ "Calienta, reposo y mucha agua.",
    higiene_vocal == "Faig repòs vocal després de la feina" ~ "Reposo.",
    higiene_vocal == "Faig repòs vocal després de la feina, Bec molta d'aigua" ~ "Reposo y mucha agua.",
    higiene_vocal == "Faig repòs vocal després de la feina, Bec molta d'aigua, No seguesc cap rutina per a la veu" ~ "Reposo y mucha agua, no rutina.",
    higiene_vocal == "No seguesc cap rutina per a la veu" ~ "No rutina.",
    TRUE ~ higiene_vocal  # Para mantener las categorías que no coincidan
  ))


table(data$higiene_vocal)

data$VHI = datos$VHI

resumen <- data %>% 
  group_by(higiene_vocal) %>% 
  summarise(
    media_VHI = mean(VHI),
    mediana_VHI = median(VHI),
    desviacion_VHI = sd(VHI),
    n = n()
  )

ggplot(data = data, aes(x = higiene_vocal, y = VHI, fill = higiene_vocal)) +
  geom_boxplot() +
  labs(title = "Distribución del Score VHI por Rutina de Higiene Vocal",
       x = "Higiene Vocal", y = "Score VHI") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7.5),legend.position = "none") 
```


Para facilitarnos el trabajo a la hora de hacer el test de inferencia, que en este caso será de correlación, consideramos que este es el test de inferencia más adecuado ya que nos dirá a que nivel encontramos relación entre estas variables (que es lo que se quiere responder en estas preguntas). Para ello, convertiremos las respuestas (cualitativas) en valores numéricos:
  
  Lo que haremos será asignar una variable numérica a cada respuesta posible ordenada de menor a mayor según lo buena o efectiva que es la rutina que siguen (orden de prioridad). Así, para no evitar contradicciones con las respuestas lo que haremos será considerar solo 6 de estas respuestas, que son: "Mucha agua", "Calienta y reposo", "Calienta, reposo y mucha agua", "Reposo", "No rutina", "Reposo y mucha agua".

Para realizarlo, nos compararemos estas respuestas con las de otra pregunta. La pregunta será: "Ha tengut problemes de veu importants i que li han dificultat la feina al llarg del
curs 2022-2023?". 

Lo haremos por frecuencias, es decir, calcularemos con que frecuencias alguien que no hace rutina ha tenido problemas y mediante estas frecuencias determinaremos el orden de prioridad. 
```{r}
data_seleccionado <- datos[, c("problema_curso22_23", "higiene_vocal")]


frecuencias <- data_seleccionado %>%
  count(problema_curso22_23, higiene_vocal) %>% # Contar las combinaciones
  
  
  # Mostrar la tabla de frecuencias relativas
  print(frecuencias)

```
Ahora, debemos calcular la probabilidad de no tener enfermedad con cada una de las respuestas. 

```{r}
probabilidades <- frecuencias %>%
  group_by(higiene_vocal) %>%                       
  mutate(Probabilidad = n / sum(n)) %>% 
  ungroup()

# Mostrar la tabla con las probabilidades
print(probabilidades)
```

Por tanto, lo mejor es lo que en problema_curso22_23 tenga mayor probabilidad.
1-No rutina
2-Hace reposo
3-Hace reposo y bebe mucha agua
4-Bebe mucha agua
5-Calienta, hace reposo y bebe mucha agua
6-Escalf la veu abans de començar la feina

Notemos que este podium no tiene sentido lógico ya que por intuición se espera que tener una rutina sea lo mejor para la voz pero no es así. Esto se puede deber a la falta de respuestas (ya que solo hay dos observaciones de los tratamientos aparentemente mejores). 
Es por ello, que haremos dos pruebas de inferencia, una con los resultados obtenidos en la clasificación y el otro con nuestra clasificación.

**DATOS OBTENIDOS**
  ```{r}
datos <- data %>%
  mutate(higiene_vocal_num = case_when(
    higiene_vocal == "Mucha agua" ~ 2,
    higiene_vocal == "Calienta y reposo" ~ 0,
    higiene_vocal == "Calienta, reposo y mucha agua" ~ 1,
    higiene_vocal == "Reposo" ~ 4,
    higiene_vocal == "No rutina" ~ 5,
    higiene_vocal == "Reposo y mucha agua" ~ 3,
    TRUE ~ NA_real_  # Para manejar posibles valores desconocidos
  ))
```

Ahora realizamos el test:
  
  ```{r}
resultado_cor <- datos %>%
  summarise(cor_test = list(cor.test(VHI, higiene_vocal_num, method = "pearson", use = "complete.obs"))) %>%
  pull(cor_test)
print(resultado_cor)
```

```{r}
datos <- data %>%
  mutate(higiene_vocal_num = case_when(
    higiene_vocal == "Mucha agua" ~ 1,
    higiene_vocal == "Calienta y reposo" ~ 3,
    higiene_vocal == "Calienta, reposo y mucha agua" ~ 5,
    higiene_vocal == "Reposo" ~ 2,
    higiene_vocal == "No rutina" ~ 0,
    higiene_vocal == "Reposo y mucha agua" ~ 4,
    TRUE ~ NA_real_  # Para manejar posibles valores desconocidos
  ))
```

Ahora realizamos el test:
  
  ```{r}
resultado_cor <- datos %>%
  summarise(cor_test = list(cor.test(VHI, higiene_vocal_num, method = "pearson", use = "complete.obs"))) %>%
  pull(cor_test)
print(resultado_cor)
```

Como podemos ver, al ser el resultado del test una correlación del 0.31295. Pese a ser relativamente moderada, se puede afirmar que las rutinas de higiene vocal tienen correlación con el VHI. Así, podemos responder a la primera parte de la pregunta planteada.

**Respuesta a la pregunta 7:**
  
  Dado que la correlación es notabe entre el cuidado de la voz y el mejoramiento de la misma, respondemos de forma afirmativa a esta pregunta.  