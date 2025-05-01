library(shiny)
library(shinyWidgets)
library(DT)
library(lpSolve)
library(dplyr)
library(ggplot2)

alimentos <- data.frame(
  id = 1:20,
  nombre = c("Manzana 🍎", "Plátano 🍌", "Pollo 🍗", "Pescado 🐟", "Arroz 🍚", 
             "Frijoles 🦘", "Huevo 🥚", "Leche 🥛", "Pan 🍞", "Queso 🧀", 
             "Zanahoria 🥕", "Brócoli 🥦", "Naranja 🍊", "Aguacate 🥑", "Papa 🥔",
             "Carne de res 🥩", "Yogur 🍶", "Avena 🌾", "Espinaca 🍃", "Atún 🐟"),
  grupo = c("Frutas", "Frutas", "Proteínas", "Proteínas", "Granos", 
            "Granos", "Proteínas", "Lácteos", "Granos", "Lácteos", 
            "Verduras", "Verduras", "Frutas", "Grasas", "Verduras",
            "Proteínas", "Lácteos", "Granos", "Verduras", "Proteínas"),
  calorias = c(52, 89, 165, 206, 130, 127, 155, 42, 265, 402, 
               41, 55, 47, 160, 77, 250, 59, 389, 23, 144),
  proteinas = c(0.3, 1.1, 31, 22, 2.7, 8.9, 13, 3.4, 9, 25, 
                0.9, 3.7, 0.9, 2, 2, 26, 3.5, 17, 2.9, 23),
  carbohidratos = c(14, 23, 0, 0, 28, 22, 1.1, 4.8, 49, 1.3, 
                    10, 11, 12, 9, 17, 0, 4.7, 66, 3.6, 0),
  grasas = c(0.2, 0.3, 3.6, 12, 0.3, 0.5, 11, 1, 3.2, 33, 
             0.2, 0.6, 0.1, 15, 0.1, 17, 1.5, 7, 0.4, 5),
  fibra = c(2.4, 2.6, 0, 0, 0.4, 7.5, 0, 0, 2.7, 0, 
            2.8, 2.6, 2.4, 7, 2.2, 0, 0, 10.6, 2.2, 0),
  porcion = c("1 mediana (182g)", "1 mediano (118g)", "100g", "100g", "1 taza (200g)", 
              "1 taza (177g)", "1 grande (50g)", "1 taza (240ml)", "1 rebanada (28g)", "30g", 
              "1 mediana (61g)", "1 taza (91g)", "1 mediana (131g)", "1/2 unidad (68g)", "1 mediana (173g)",
              "100g", "1 envase (170g)", "1 taza (81g)", "1 taza (30g)", "1 lata (165g)"),
  precio_mxn = c(3.5, 2.8, 25, 35, 8, 12, 3, 12, 15, 45, 
                 6, 10, 4, 18, 7, 80, 10, 20, 8, 15)
)

# Define nutritional requirements by age group
age_group_reqs <- list(
  "Bebé" = data.frame(
    Nutriente = c("Calorías", "Proteínas", "Carbohidratos", "Grasas", "Fibra"),
    Min = c(800, 13, 95, 30, 19),
    Max = c(1200, 20, 130, 50, 30)
  ),
  "Niño" = data.frame(
    Nutriente = c("Calorías", "Proteínas", "Carbohidratos", "Grasas", "Fibra"),
    Min = c(1200, 19, 130, 45, 25),
    Max = c(1800, 34, 200, 75, 35)
  ),
  "Adolescente" = data.frame(
    Nutriente = c("Calorías", "Proteínas", "Carbohidratos", "Grasas", "Fibra"),
    Min = c(1800, 46, 130, 55, 26),
    Max = c(2800, 75, 300, 95, 45)
  ),
  "Adulto" = data.frame(
    Nutriente = c("Calorías", "Proteínas", "Carbohidratos", "Grasas", "Fibra"),
    Min = c(2000, 50, 130, 65, 30),
    Max = c(2500, 150, 300, 100, 50)
  ),
  "Adulto mayor" = data.frame(
    Nutriente = c("Calorías", "Proteínas", "Carbohidratos", "Grasas", "Fibra"),
    Min = c(1600, 50, 130, 50, 25),
    Max = c(2200, 120, 250, 80, 40)
  )
)

ui <- fluidPage(
  titlePanel("Generador de Dietas Optimizadas por Costo"),
  sidebarLayout(
    sidebarPanel(
      pickerInput("grupo_edad", "Selecciona el grupo de edad (referencia):",
                  choices = c("Bebé", "Niño", "Adolescente", "Adulto", "Adulto mayor"),
                  selected = "Adulto"),
      checkboxGroupInput("alimentos_seleccionados", "Alimentos:",
                         choices = setNames(alimentos$id, paste(alimentos$nombre, "-", alimentos$porcion)),
                         selected = alimentos$id),
      sliderInput("porciones_min", "Porciones mínimas por alimento:",
                  min = 0.5, max = 1, value = 0.5, step = 0.1),
      sliderInput("porciones_max", "Porciones máximas por alimento:",
                  min = 5, max = 10, value = 5, step = 0.5),
      actionButton("optimizar", "Generar Dieta Optimizada", class = "btn-primary")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Dieta Recomendada",
                 h3("Grupo de edad seleccionado:"),
                 textOutput("grupo_edad_seleccionado"),
                 br(),
                 h4("Personaliza los requerimientos nutricionales:"),
                 DTOutput("tabla_reqs"),
                 br(),
                 h4("Alimentos y porciones:"),
                 DTOutput("tabla_dieta"),
                 h5("*Las porciones se muestran en unidades estándar (ver descripción en la lista de alimentos)"),
                 h4("Resumen nutricional:"),
                 DTOutput("resumen_nutricional"),
                 h4("Costo total diario:"),
                 tags$h3(textOutput("costo_total"), style = "color:#006400; font-weight:bold; font-size: 24px;"),
                 plotOutput("grafico_nutrientes")),
        tabPanel("Todos los Alimentos", 
                 h4("Lista completa de alimentos con tamaños de porción:"),
                 DTOutput("tabla_alimentos"))
      )
    )
  )
)

server <- function(input, output, session) {
  output$tabla_alimentos <- renderDT({
    datatable(alimentos %>% select(-id), 
              options = list(pageLength = 10), 
              rownames = FALSE,
              caption = "Todos los alimentos disponibles con sus tamaños de porción estándar")
  })
  
  # Initialize with default requirements for adult
  req_data <- reactiveVal(age_group_reqs[["Adulto"]])
  
  # Update requirements when age group changes
  observeEvent(input$grupo_edad, {
    req_data(age_group_reqs[[input$grupo_edad]])
  })
  
  output$tabla_reqs <- renderDT({
    datatable(
      req_data(),
      editable = list(target = "cell", disable = list(columns = 0)),
      rownames = FALSE,
      options = list(dom = 't')
    )
  })
  
  observeEvent(input$tabla_reqs_cell_edit, {
    info <- input$tabla_reqs_cell_edit
    if(info$col %in% 1:2) {
      df <- req_data()
      df[info$row, info$col + 1] <- as.numeric(info$value)
      req_data(df)
    }
  })
  
  dieta_optimizada <- eventReactive(input$optimizar, {
    alimentos_disponibles <- alimentos %>% filter(id %in% input$alimentos_seleccionados)
    reqs <- req_data()
    names(reqs$Min) <- reqs$Nutriente
    names(reqs$Max) <- reqs$Nutriente
    
    f.obj <- alimentos_disponibles$precio_mxn
    
    f.con <- matrix(c(
      alimentos_disponibles$calorias,
      alimentos_disponibles$proteinas,
      alimentos_disponibles$carbohidratos,
      alimentos_disponibles$grasas,
      alimentos_disponibles$fibra,
      rep(1, nrow(alimentos_disponibles))
    ), nrow = 6, byrow = TRUE)
    
    f.dir <- rep(">=", 6)
    f.rhs <- c(reqs$Min[1:5], 3)
    
    n <- nrow(alimentos_disponibles)
    f.con <- rbind(f.con, diag(n), -diag(n))
    f.dir <- c(f.dir, rep(">=", n), rep("<=", n))
    f.rhs <- c(f.rhs, rep(input$porciones_min, n), rep(input$porciones_max, n))
    
    solucion <- lp("min", f.obj, f.con, f.dir, f.rhs)
    
    if (solucion$status == 0) {
      resultado <- alimentos_disponibles %>%
        mutate(Porciones = round(solucion$solution, 2)) %>%
        filter(Porciones > 0) %>%
        mutate(
          Porcion_Desc = paste0(round(Porciones, 2), " x ", porcion),
          Calorias_total = calorias * Porciones,
          Proteinas_total = proteinas * Porciones,
          Carbohidratos_total = carbohidratos * Porciones,
          Grasas_total = grasas * Porciones,
          Fibra_total = fibra * Porciones,
          Costo_total = precio_mxn * Porciones
        )
      list(
        dieta = resultado,
        costo_total = sum(resultado$Costo_total),
        resumen_nutricional = data.frame(
          Nutriente = reqs$Nutriente,
          Requerido = reqs$Min,
          Obtenido = c(
            sum(resultado$Calorias_total),
            sum(resultado$Proteinas_total),
            sum(resultado$Carbohidratos_total),
            sum(resultado$Grasas_total),
            sum(resultado$Fibra_total)
          )
        )
      )
    } else {
      NULL
    }
  })
  
  output$grupo_edad_seleccionado <- renderText({
    input$grupo_edad
  })
  
  output$tabla_dieta <- renderDT({
    dieta <- dieta_optimizada()
    if (!is.null(dieta)) {
      datatable(dieta$dieta %>%
                  select(nombre, grupo, Porcion_Desc, Costo_total) %>%
                  rename(Alimento = nombre, 
                         Grupo = grupo,
                         "Porciones" = Porcion_Desc,
                         "Costo (MXN)" = Costo_total),
                options = list(pageLength = 10), 
                rownames = FALSE)
    }
  })
  
  output$resumen_nutricional <- renderDT({
    dieta <- dieta_optimizada()
    if (!is.null(dieta)) {
      datatable(dieta$resumen_nutricional, 
                options = list(dom = 't'), 
                rownames = FALSE)
    }
  })
  
  output$costo_total <- renderText({
    dieta <- dieta_optimizada()
    if (!is.null(dieta)) {
      paste0("$", round(dieta$costo_total, 2), " MXN por día")
    } else {
      "No se pudo encontrar una solución que cumpla con los requerimientos."
    }
  })
  
  output$grafico_nutrientes <- renderPlot({
    dieta <- dieta_optimizada()
    if (!is.null(dieta)) {
      df <- dieta$resumen_nutricional
      colores <- c("#E41A1C", "#fccd03", "#50dc18", "#510982", "#ff6700")
      
      ggplot(df, aes(x = Nutriente, y = Obtenido, fill = Nutriente)) +
        geom_bar(stat = "identity") +
        geom_hline(aes(yintercept = Requerido, color = Nutriente), size = 1.2, show.legend = FALSE) +
        scale_fill_manual(values = colores) +
        scale_color_manual(values = colores) +
        labs(title = "Comparación de nutrientes obtenidos vs requeridos",
             y = "Cantidad", x = "") +
        theme_minimal() +
        theme(legend.position = "none")
    }
  })
}

shinyApp(ui, server)