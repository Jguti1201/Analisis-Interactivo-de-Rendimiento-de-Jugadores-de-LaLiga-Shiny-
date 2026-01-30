# ======================
# LIBRERÍAS
# ======================
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(readr)
library(RColorBrewer)

# ======================
# CARGA DE DATOS
# ======================
datos <- read_csv("stats_la_liga_shots.csv", locale = locale(encoding = "UTF-8"))

# Normalizar nombres
names(datos) <- make.names(names(datos))

# Calcular Minutos Jugados = Sh * Sh.90
datos <- datos %>%
  mutate(Minutos_jugados = Sh * Sh.90)

# Seleccionar solo columnas necesarias
datos <- datos %>%
  select(Player, Squad, Pos, Gls, Minutos_jugados)

# ======================
# UI
# ======================
ui <- fluidPage(
  
  titlePanel("Análisis de Rendimiento de Jugadores"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("Filtros"),
      
      selectInput(
        "posicion",
        "Selecciona posición:",
        choices = c("Todas", sort(unique(datos$Pos))),
        selected = "Todas"
      ),
      
      selectInput(
        "equipo",
        "Selecciona equipo:",
        choices = c("Todos", sort(unique(datos$Squad))),
        selected = "Todos"
      ),
      
      selectInput(
        "jugador",
        "Selecciona jugador:",
        choices = c("Todos", sort(unique(datos$Player))),
        selected = "Todos"
      ),
      
      hr(),
      
      h4("Métrica a graficar"),
      selectInput(
        "metrica",
        "Selecciona métrica:",
        choices = c(
          "Goles" = "Gls",
          "Minutos jugados" = "Minutos_jugados"
        ),
        selected = "Gls"
      ),
      
      hr(),
      
      h4("Visualización"),
      selectInput(
        "paleta",
        "Paleta de colores:",
        choices = rownames(brewer.pal.info),
        selected = "Set2"
      ),
      
      checkboxInput(
        "mostrar_leyenda",
        "Mostrar leyenda",
        value = TRUE
      )
    ),
    
    mainPanel(
      tabsetPanel(
        
        tabPanel(
          "Gráficos",
          plotlyOutput("grafico", height = "500px")
        ),
        
        tabPanel(
          "Tabla de datos",
          DTOutput("tabla")
        )
      )
    )
  )
)

# ======================
# SERVER
# ======================
server <- function(input, output, session) {
  
  # Datos filtrados según filtros del usuario
  datos_filtrados <- reactive({
    
    df <- datos
    
    if (input$posicion != "Todas") {
      df <- df %>% filter(Pos == input$posicion)
    }
    
    if (input$equipo != "Todos") {
      df <- df %>% filter(Squad == input$equipo)
    }
    
    if (input$jugador != "Todos") {
      df <- df %>% filter(Player == input$jugador)
    }
    
    df
  })
  
  # Gráfico interactivo
  output$grafico <- renderPlotly({
    
    df <- datos_filtrados()
    
    n_cat <- length(unique(df$Pos))
    max_col <- brewer.pal.info[input$paleta, "maxcolors"]
    paleta_final <- brewer.pal(min(max_col, max(3, n_cat)), input$paleta)
    
    g <- ggplot(
      df,
      aes(
        x = reorder(Player, .data[[input$metrica]]),
        y = .data[[input$metrica]],
        fill = Pos
      )
    ) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = paleta_final) +
      labs(
        title = paste("Comparación de", input$metrica),
        x = "Jugador",
        y = input$metrica
      ) +
      theme_minimal()
    
    if (!input$mostrar_leyenda) {
      g <- g + theme(legend.position = "none")
    }
    
    ggplotly(g)
  })
  
  # Tabla dinámica
  output$tabla <- renderDT({
    datatable(
      datos_filtrados(),
      extensions = "Buttons",
      options = list(
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel", "pdf"),
        pageLength = 10
      ),
      filter = "top"
    )
  })
}

# ======================
# RUN APP
# ======================
shinyApp(ui = ui, server = server)
