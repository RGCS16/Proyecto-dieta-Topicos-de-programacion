library(shiny)
library(DT)
library(lpSolve)

# Categorías de alimentos
categorias <- list(
  "Proteínas" = c("🥚 Huevo", "🐟 Atún", "🥩 Carne", "🍗 Pollo", "🐟 Salmón", "🌭 Salchicha", "🍖 Jamón"),
  "Lácteos" = c("🥛 Leche", "🧀 Queso"),
  "Cereales" = c("🍚 Arroz", "🍞 Pan Integral", "🌮 Tortillas de Maíz"),
  "Legumbres" = c("🫘 Frijoles"),
  "Verduras" = c("🍅 Jitomate", "🥦 Brócoli", "🥕 Zanahoria", "🥒 Pepino", "🥦 Espinaca", "🥒 Chayote", "🌶️ Chile", "🧅 Cebolla", "🎃 Calabaza", "🥔 Papa"),
  "Frutas" = c("🍎 Manzana", "🍌 Plátano", "🍊 Naranjas", "🍓 Fresas", "🍋 Limón", "🍉 Sandía"),
  "Frutos secos" = c("🥜 Almendras", "🌰 Nueces"),
  "Grasas y aceites" = c("🥑 Aguacate", "🥥 Coco")
)

# Obtener lista de alimentos
alimentos <- unlist(categorias, use.names = FALSE)

# Obtener precios simulados por porción con semilla fija
get_latest_prices <- function() {
  set.seed(123)
  precios_kg <- runif(length(alimentos), min = 10, max = 100)
  porcion_g <- sample(50:150, length(alimentos), replace = TRUE) # Porciones más razonables
  precios_porcion <- (precios_kg / 1000) * porcion_g
  return(data.frame(Alimento = alimentos, Costo = round(precios_porcion, 2), Porcion_g = porcion_g))
}

# Obtener requerimientos nutricionales con rangos prácticos
get_nutritional_requirements <- function(tipo_persona) {
  requerimientos <- list(
    "Bebé" = c(300, 150, 8, 4, 500, 250, 12, 10), 
    "Niño" = c(400, 400, 12, 8, 600, 700, 22, 18),
    "Adolescente" = c(600, 900, 30, 22, 1000, 1200, 55, 33),
    "Adulto" = c(800, 900, 45, 24, 1200, 2500, 58, 40),
    "Adulto Mayor" = c(800, 1100, 45, 28, 1200, 1300, 58, 42)
  )
  valores <- requerimientos[[tipo_persona]]
  return(data.frame(
    Nutriente = c("Vitaminas", "Minerales", "Proteínas", "Fibra"),
    Mínimo = valores[1:4],
    Máximo = valores[5:8]
  ))
}

# Interfaz de usuario
ui <- fluidPage(
  titlePanel("Optimización de Dieta - Porciones Prácticas"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("tipo_persona", "Selecciona el tipo de persona:", 
                   choices = c("Bebé", "Niño", "Adolescente", "Adulto", "Adulto Mayor")),
      checkboxGroupInput("selected_alimentos", "Selecciona los alimentos:", choices = alimentos),
      sliderInput("min_porciones", "Mínimo de porciones por alimento:", 
                  min = 0.5, max = 2, value = 1, step = 0.5),
      sliderInput("max_porciones", "Máximo de porciones por alimento:", 
                  min = 3, max = 10, value = 5, step = 1),
      actionButton("optimize", "Optimizar Dieta"),
      helpText("Selecciona varios alimentos de diferentes categorías para mejores resultados.")
    ),
    mainPanel(
      h4("Costos por porción:"),
      DTOutput("tabla_costos"),
      h4("Requerimientos nutricionales:"),
      DTOutput("tabla_requerimientos"),
      h4("Contenido nutricional por alimento (mg por porción):"),
      DTOutput("tabla_mg"),
      h4("Plan de alimentación recomendado:"),
      DTOutput("tabla_resultados"),
      h4("Resumen nutricional:"),
      DTOutput("tabla_nutrientes"),
      verbatimTextOutput("resultado_optimizacion")
    )
  )
)

server <- function(input, output, session) {
  precios <- get_latest_prices()
  
  datos_costos <- reactiveVal(precios)
  datos_requerimientos <- reactiveVal(NULL)
  matriz_mg <- reactiveVal(NULL)
  
  # Generar matriz nutricional más realista
  generate_nutrition_matrix <- function(selected_foods) {
    set.seed(123)
    matrix_data <- matrix(0, nrow = 4, ncol = length(selected_foods))
    
    for(i in seq_along(selected_foods)) {
      food <- selected_foods[i]
      
      # Asignar valores basados en categorías de alimentos
      if(grepl("🥚|🐟|🥩|🍗|🌭|🍖", food)) {
        # Alto en proteínas
        matrix_data[1, i] <- runif(1, 5, 15)  # Vitaminas
        matrix_data[2, i] <- runif(1, 2, 8)   # Minerales
        matrix_data[3, i] <- runif(1, 15, 25) # Proteínas
        matrix_data[4, i] <- runif(1, 0.5, 3) # Fibra
      } 
      else if(grepl("🥛|🧀", food)) {
        # Lácteos
        matrix_data[1, i] <- runif(1, 5, 10)
        matrix_data[2, i] <- runif(1, 10, 20)
        matrix_data[3, i] <- runif(1, 5, 10)
        matrix_data[4, i] <- runif(1, 0, 1)
      }
      else if(grepl("🍚|🍞|🌮", food)) {
        # Cereales
        matrix_data[1, i] <- runif(1, 1, 5)
        matrix_data[2, i] <- runif(1, 2, 5)
        matrix_data[3, i] <- runif(1, 3, 8)
        matrix_data[4, i] <- runif(1, 2, 6)
      }
      else if(grepl("🍅|🥦|🥕|🥒|🌶|🧅|🎃|🥔", food)) {
        # Verduras
        matrix_data[1, i] <- runif(1, 15, 30)
        matrix_data[2, i] <- runif(1, 5, 15)
        matrix_data[3, i] <- runif(1, 1, 5)
        matrix_data[4, i] <- runif(1, 3, 8)
      }
      else if(grepl("🍎|🍌|🍊|🍓|🍋|🍉", food)) {
        # Frutas
        matrix_data[1, i] <- runif(1, 20, 40)
        matrix_data[2, i] <- runif(1, 3, 10)
        matrix_data[3, i] <- runif(1, 0.5, 3)
        matrix_data[4, i] <- runif(1, 2, 5)
      }
      else {
        # Valores por defecto
        matrix_data[1, i] <- runif(1, 1, 10)
        matrix_data[2, i] <- runif(1, 1, 5)
        matrix_data[3, i] <- runif(1, 1, 8)
        matrix_data[4, i] <- runif(1, 0.5, 3)
      }
    }
    
    dimnames(matrix_data) <- list(c("Vitaminas", "Minerales", "Proteínas", "Fibra"), selected_foods)
    return(matrix_data)
  }
  
  observe({
    req(input$selected_alimentos, input$tipo_persona)
    
    precios_seleccionados <- precios[precios$Alimento %in% input$selected_alimentos, ]
    requerimientos <- get_nutritional_requirements(input$tipo_persona)
    matriz_nutricional <- generate_nutrition_matrix(input$selected_alimentos)
    
    datos_costos(precios_seleccionados)
    datos_requerimientos(requerimientos)
    matriz_mg(as.data.frame(matriz_nutricional))
  })
  
  output$tabla_costos <- renderDT({
    datatable(datos_costos(), options = list(pageLength = 5))
  })
  
  output$tabla_requerimientos <- renderDT({
    datatable(datos_requerimientos(), options = list(dom = 't'))
  })
  
  output$tabla_mg <- renderDT({
    datatable(matriz_mg(), options = list(pageLength = 5))
  })
  
  observeEvent(input$optimize, {
    req(datos_costos(), datos_requerimientos(), matriz_mg())
    
    costos <- datos_costos()$Costo
    A <- as.matrix(matriz_mg())
    b_min <- datos_requerimientos()$Mínimo
    b_max <- datos_requerimientos()$Máximo
    n_alimentos <- length(costos)
    
    # Configurar restricciones con los valores del slider
    const_mat <- rbind(
      A,       # Para nutrientes mínimos
      -A,      # Para nutrientes máximos
      diag(n_alimentos),  # Límite superior porciones
      -diag(n_alimentos)  # Límite inferior porciones
    )
    
    const_dir <- c(
      rep(">=", 4),  # Nutrientes mínimos
      rep("<=", 4),  # Nutrientes máximos
      rep("<=", n_alimentos),  # Máximo porciones
      rep(">=", n_alimentos)   # Mínimo porciones
    )
    
    const_rhs <- c(
      b_min * 0.8,  # Aceptamos 80% del mínimo requerido
      -b_max * 1.2, # Aceptamos 120% del máximo requerido
      rep(input$max_porciones, n_alimentos),  # Máximo del slider
      rep(input$min_porciones, n_alimentos)   # Mínimo del slider
    )
    
    # Primero intentamos con solución entera
    result <- lp(
      direction = "min",
      objective.in = costos,
      const.mat = const_mat,
      const.dir = const_dir,
      const.rhs = const_rhs,
      all.int = TRUE  # Forzamos porciones enteras
    )
    
    # Si no encontramos solución, intentamos con decimales pero con porciones prácticas
    if(result$status != 0) {
      result <- lp(
        direction = "min",
        objective.in = costos,
        const.mat = const_mat,
        const.dir = const_dir,
        const.rhs = const_rhs,
        all.int = FALSE
      )
      
      # Redondeamos a medias porciones (0.5) para hacerlo práctico
      porciones <- pmax(round(result$solution * 2) / 2, input$min_porciones)
    } else {
      porciones <- result$solution
    }
    
    # Aseguramos que tenemos una solución práctica
    porciones <- pmax(pmin(porciones, input$max_porciones), input$min_porciones)
    total_cost <- round(sum(porciones * costos), 2)
    
    # Crear tabla de resultados
    resultados <- data.frame(
      Alimento = datos_costos()$Alimento,
      Porciones = porciones,
      "Gramos Totales" = round(porciones * datos_costos()$Porcion_g),
      "Costo por porción" = datos_costos()$Costo,
      "Costo total" = round(porciones * datos_costos()$Costo, 2)
    )
    
    # Calcular nutrientes obtenidos
    nutrientes_obtenidos <- data.frame(
      Nutriente = datos_requerimientos()$Nutriente,
      Obtenido = round(A %*% porciones, 1),
      Mínimo = datos_requerimientos()$Mínimo,
      Máximo = datos_requerimientos()$Máximo,
      Cumplimiento = ifelse(
        round(A %*% porciones, 1) >= datos_requerimientos()$Mínimo & 
          round(A %*% porciones, 1) <= datos_requerimientos()$Máximo,
        "✅", "⚠️"
      )
    )
    
    output$tabla_resultados <- renderDT({
      datatable(resultados, options = list(dom = 't')) %>%
        formatCurrency(c("Costo.por.porción", "Costo.total"), '$') %>%
        formatRound("Porciones", 1)
    })
    
    output$tabla_nutrientes <- renderDT({
      datatable(nutrientes_obtenidos, options = list(dom = 't')) %>%
        formatRound(c("Obtenido", "Mínimo", "Máximo"), 1)
    })
    
    output$resultado_optimizacion <- renderPrint({
      paste("Costo total diario estimado: $", total_cost, "\n",
            "Total de porciones: ", round(sum(porciones), 1), "\n",
            "Gramos totales: ", round(sum(resultados$Gramos.Totales)), "\n",
            "Recomendación: Distribuir estas porciones en 3-5 comidas al día")
    })
  })
}

shinyApp(ui, server)