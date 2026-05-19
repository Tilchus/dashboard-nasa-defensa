# =========================================================
# PROYECTO: NEXO INTEL - Inteligencia de Defensa Planetaria
# ESTILO: Código Limpio Moderno (|> , [[ ]]) + Storytelling
# MODIFICACIÓN: CONTROL RESPONSIVO OPTIMIZADO PARA VIEWER INTERNO DE R
# =========================================================

library(shiny)
library(httr)
library(jsonlite, warn.conflicts = FALSE)
library(ggplot2)
library(dplyr, warn.conflicts = FALSE)
library(dotenv)
library(bslib, warn.conflicts = FALSE)
library(bsicons)
library(plotly) 
library(DT)     
library(urltools) # Requerida para codificar el texto de la traducción

# 1. Cargar clave de seguridad
try(load_dot_env(".env"), silent = TRUE)

ui <- page_navbar(
  title = "NEXO: Inteligencia de Defensa Planetaria",
  
  # CSS Avanzado para menú colapsado permanente a pantalla completa y KPIs compactos
  theme = bs_theme(version = 5, bootswatch = "darkly") |> 
    bs_add_rules("
      /* ====================================================
         1. ROMPER CONTENEDOR Y ALINEAR BOTÓN HAMBURGUESA
         ==================================================== */
      .navbar > .container-fluid {
        max-width: 100% !important;
        padding-left: 20px !important;
        padding-right: 20px !important;
        display: flex !important;
        justify-content: space-between !important; 
        align-items: center !important;
      }
      .navbar-expand-lg .navbar-toggler {
        display: block !important;
      }
      .navbar-expand-lg .navbar-collapse {
        display: none !important; 
      }
      .navbar-expand-lg .navbar-collapse.show {
        display: block !important; 
      }
      .navbar-toggler {
        border-color: #00bc8c !important; 
        background-color: #2c3e50 !important; 
        padding: 6px 10px !important;
      }
      .navbar-toggler-icon {
        filter: invert(61%) sepia(85%) saturate(443%) hue-rotate(114deg) brightness(94%) contrast(101%) !important; 
      }
      
      /* ====================================================
         2. MENÚ DESPLEGABLE A PANTALLA COMPLETA REAL
         ==================================================== */
      .navbar-collapse {
        position: absolute !important;
        top: 56px !important;    
        left: 0 !important;      
        width: 100vw !important; 
        background-color: #1a1a1a !important; 
        padding: 25px 40px !important;
        border-bottom: 2px solid #333 !important;
        border-top: 1px solid #2b2b2b !important;
        border-left: none !important;
        border-right: none !important;
        border-radius: 0px !important; 
        box-shadow: 0 15px 35px rgba(0,0,0,0.8) !important;
        z-index: 9999 !important;
      }
      .navbar-collapse .nav-link {
        color: #ffffff !important;
        font-weight: 500 !important;
        font-size: 1.1rem !important;
        padding: 14px 20px !important;
        border-bottom: 1px solid #2b2b2b;
        display: block !important;
        width: 100% !important;
      }
      .navbar-collapse .nav-link:hover, 
      .navbar-collapse .nav-link.active {
        color: #00bc8c !important; 
        background-color: #222222 !important; 
        border-radius: 4px;
        padding-left: 30px !important; 
        transition: all 0.2s ease-in-out;
      }

      /* ====================================================
         3. REGLAS MANTENIDAS PARA KPIS Y TABLAS
         ==================================================== */
      .value-box { 
        box-shadow: 0 4px 12px rgba(0,0,0,0.4); 
        border: 1px solid #333; 
        min-height: 80px !important;
        height: 95px !important;
        padding: 0.4rem !important; 
      }
      .value-box .value-box-title { 
        font-size: 0.8rem !important; 
        margin-bottom: 2px !important;
      }
      .value-box .value-box-value { 
        font-size: 1.4rem !important; 
        font-weight: bold !important;
      }
      .value-box .value-box-showcase {
        margin-right: 8px !important;
      }
      table.dataTable dataTables_wrapper {
        color: #fff !important;
      }
      thead th {
        background-color: #2c3e50 !important;
        color: #00bc8c !important;
        font-weight: bold !important;
        border-bottom: 2px solid #00bc8c !important;
        text-align: left !important;
      }
      
      /* AJUSTE PARA VENTANA CHICA: Si la pantalla es menor a 992px (como el Viewer por defecto),
         rompe las columnas paralelas y las apila de forma fluida al 100% de ancho */
      @media (max-width: 992px) {
        .responsive-grid {
          display: block !important;
        }
        .responsive-grid > .card {
          width: 100% !important;
          margin-bottom: 20px !important;
        }
      }
    "),
  
  # PESTAÑA PRINCIPAL: EL "WAR ROOM"
  nav_panel("Centro de Control NEO",
            layout_sidebar(
              sidebar = sidebar(
                title = "Parámetros de Radar",
                dateInput("fecha", "Fecha de Observación:", value = Sys.Date(), language = "es"),
                helpText("Escaneando Objetos Cercanos a la Tierra (NEOs) en trayectoria de aproximación."),
                hr(),
                markdown("**Guía de Análisis:** Los asteroides en la zona superior derecha del gráfico representan el mayor riesgo cinético debido a su masa y velocidad.")
              ),
              
              # NIVEL 1: Indicadores Clave (KPIs) - Ultra Compactos
              layout_column_wrap(
                width = 1/3,
                heights_equal = "row",
                value_box(
                  title = "Objetos en Radar", 
                  value = textOutput("total_ast"), 
                  showcase = bsicons::bs_icon("radar", size = "1.1em"), 
                  theme = "primary"
                ),
                value_box(
                  title = "Índice de Riesgo (%)", 
                  value = textOutput("riesgo_prom"), 
                  showcase = bsicons::bs_icon("exclamation-octagon", size = "1.1em"), 
                  theme = "warning"
                ),
                value_box(
                  title = "Máxima Cercanía (km)", 
                  value = textOutput("min_dist"), 
                  showcase = bsicons::bs_icon("shield-shaded", size = "1.1em"), 
                  theme = "danger"
                )
              ),
              
              br(), 
              
              # NIVEL 2: La Narrativa Dinámica
              uiOutput("narrativa_alerta"),
              
              br(),
              
              # NIVEL 3: Contenedor con comportamiento responsivo e inteligente
              div(class = "responsive-grid",
                  layout_column_wrap(
                    width = 1/2,
                    card(
                      card_header("Matriz de Threat: Tamaño vs Velocidad"), 
                      plotlyOutput("grafico_interactivo", height = "450px") 
                    ),
                    card(
                      card_header("Registro Detallado de Avistamientos"), 
                      card_body(
                        DTOutput("tabla_pro")
                      ),
                      card_body(
                        fill = FALSE,
                        style = "background-color: #1e2b37; border-radius: 6px; padding: 12px; margin-top: 5px;",
                        div(style = "font-size: 0.9rem; color: #e0e0e0; font-weight: 500;",
                            icon("info-circle"), " Protocolo Operativo: Utilice el motor de búsqueda integrado arriba para aislar y analizar asteroides específicos por su designación oficial de manera inmediata."
                        )
                      )
                    )
                  )
              )
            )
  ),
  
  # PESTAÑA MULTIMEDIA (APOD)
  nav_panel("Exploración APOD",
            card(
              card_header(textOutput("titulo_foto")),
              div(style = "text-align: center; padding: 20px; background-color: #000; border-radius: 10px;",
                  uiOutput("media_nasa")
              ),
              card_footer(textOutput("desc_foto"))
            )
  )
)

server <- function(input, output) {
  
  api_key <- reactive({
    key <- Sys.getenv("NASA_API_KEY")
    if (key == "") return("DEMO_KEY")
    return(key)
  })
  
  # Pipeline de Datos de Asteroides
  data_nasa <- reactive({
    url <- paste0("https://api.nasa.gov/neo/rest/v1/feed?start_date=", 
                  input$fecha, "&end_date=", input$fecha, "&api_key=", api_key())
    res <- GET(url)
    if (res$status_code != 200) return(NULL)
    
    raw_data <- fromJSON(content(res, "text"))
    df_raw <- raw_data[["near_earth_objects"]][[1]]
    
    data.frame(
      nombre = as.character(df_raw[["name"]]),
      diam_max = as.numeric(df_raw[["estimated_diameter"]][["kilometers"]][["estimated_diameter_max"]]),
      peligroso = as.logical(df_raw[["is_potentially_hazardous_asteroid"]]),
      distancia = as.numeric(sapply(df_raw[["close_approach_data"]], \(x) x[["miss_distance"]][["kilometers"]])),
      velocidad = as.numeric(sapply(df_raw[["close_approach_data"]], \(x) x[["relative_velocity"]][["kilometers_per_hour"]]))
    ) |> 
      arrange(distancia) 
  })
  
  # --- Renderizados del Centro de Control ---
  output$total_ast <- renderText({ req(data_nasa()); nrow(data_nasa()) })
  
  output$riesgo_prom <- renderText({ 
    req(data_nasa())
    df <- data_nasa()
    total_peligrosos <- sum(df[["peligroso"]], na.rm = TRUE)
    total_objetos <- nrow(df)
    if (total_objetos > 0) {
      porcentaje <- (total_peligrosos / total_objetos) * 100
      return(paste0(round(porcentaje, 1), "%"))
    } else { return("0%") }
  })
  
  output$min_dist <- renderText({ 
    req(data_nasa())
    valores_dist <- data_nasa()[["distancia"]]
    if(length(valores_dist) > 0) {
      min(valores_dist, na.rm = TRUE) |> round(0) |> format(big.mark = ".")
    } else { "N/D" }
  })
  
  output$narrativa_alerta <- renderUI({
    req(data_nasa())
    amenazas <- data_nasa() |> filter(peligroso == TRUE)
    if(nrow(amenazas) > 0) {
      top_peligro <- amenazas |> slice(1)
      layout_column_wrap(
        width = 1,
        value_box(
          title = "INFORME DE AMENAZA CRÍTICA",
          value = top_peligro[["nombre"]],
          subtitle = paste("Mayor riesgo del día. Trayectoria calculada a solo", 
                           format(round(top_peligro[["distancia"]], 0), big.mark = "."), 
                           "km de la Tierra."),
          showcase = bsicons::bs_icon("exclamation-triangle-fill"),
          theme = "danger"
        )
      )
    } else {
      layout_column_wrap(
        width = 1,
        value_box(
          title = "ESTADO DE DEFENSA",
          value = "Sin Amenazas",
          subtitle = "No hay objetos peligrosos detectados en este cuadrante temporal.",
          showcase = bsicons::bs_icon("shield-check"),
          theme = "success"
        )
      )
    }
  })
  
  output$grafico_interactivo <- renderPlotly({
    req(data_nasa())
    gg <- ggplot(data_nasa(), aes(x = diam_max, y = velocidad, color = nombre,
                                  text = paste("Nombre:", nombre,
                                               "<br>Velocidad:", format(round(velocidad, 0), big.mark = "."), "km/h",
                                               "<br>Diámetro:", round(diam_max, 3), "km",
                                               "<br>Distancia:", format(round(distancia, 0), big.mark = "."), "km"))) +
      geom_point(aes(size = diam_max), alpha = 0.8) +
      theme_minimal() +
      labs(x = "Diámetro Estimado Máx (km)", y = "Velocidad Relativa (km/h)") +
      theme(legend.position = "none") 
    ggplotly(gg, tooltip = "text") |> layout(showlegend = FALSE) 
  })
  
  output$tabla_pro <- renderDT({
    req(data_nasa())
    df_mostrar <- data_nasa() |> 
      select(Nombre = nombre, `Diámetro (km)` = diam_max, `Peligroso` = peligroso)
    
    datatable(df_mostrar, 
              options = list(
                pageLength = 5, 
                scrollX = TRUE, 
                autoWidth = TRUE, 
                dom = 'tp',
                # Mantiene la altura perfecta y balanceada
                initComplete = JS(
                  "function(settings, json) {",
                  "  $(this).closest('.dataTables_wrapper').css({'height': '340px', 'display': 'flex', 'flex-direction': 'column', 'justify-content': 'space-between'});",
                  "  $(this).css({'width': '100%', 'height': '100%'});",
                  "}"
                )
              ), 
              rownames = FALSE) |> 
      formatRound(columns = c('Diámetro (km)'), digits = 3)
  })
  
  # =========================================================
  # 4. LÓGICA MULTIMEDIA CON TRADUCTOR POR BLOQUES
  # =========================================================
  
  apod_data <- reactive({
    url_foto <- paste0("https://api.nasa.gov/planetary/apod?api_key=", api_key(), "&date=", input$fecha)
    res <- GET(url_foto)
    if (res$status_code == 200) fromJSON(content(res, "text")) else NULL
  })
  
  traducir_texto <- function(texto) {
    if (is.null(texto) || texto == "") return("")
    
    if (nchar(texto) <= 450) {
      texto_codificado <- url_encode(texto)
      url_traductor <- paste0("https://api.mymemory.translated.net/get?q=", 
                              texto_codificado, "&langpair=en|es")
      res <- try(GET(url_traductor), silent = TRUE)
      if (inherits(res, "try-error") || res$status_code != 200) return(texto)
      res_json <- fromJSON(content(res, "text", encoding = "UTF-8"))
      return(res_json[["responseData"]][["translatedText"]])
    }
    
    oraciones <- unlist(strsplit(texto, "(?<=\\.)\\s+", perl = TRUE))
    texto_traducido_total <- c()
    bloque_actual <- ""
    
    for (oracion in oraciones) {
      if (nchar(bloque_actual) + nchar(oracion) + 1 > 450) {
        if (bloque_actual != "") {
          texto_codificado <- url_encode(bloque_actual)
          url_traductor <- paste0("https://api.mymemory.translated.net/get?q=", 
                                  texto_codificado, "&langpair=en|es")
          res <- try(GET(url_traductor), silent = TRUE)
          if (!inherits(res, "try-error") && res$status_code == 200) {
            res_json <- fromJSON(content(res, "text", encoding = "UTF-8"))
            texto_traducido_total <- c(texto_traducido_total, res_json[["responseData"]][["translatedText"]])
          } else {
            texto_traducido_total <- c(texto_traducido_total, bloque_actual) 
          }
        }
        bloque_actual <- oracion
      } else {
        if (bloque_actual == "") {
          bloque_actual <- oracion
        } else {
          bloque_actual <- paste(bloque_actual, oracion)
        }
      }
    }
    
    if (bloque_actual != "") {
      texto_codificado <- url_encode(bloque_actual)
      url_traductor <- paste0("https://api.mymemory.translated.net/get?q=", 
                              texto_codificado, "&langpair=en|es")
      res <- try(GET(url_traductor), silent = TRUE)
      if (!inherits(res, "try-error") && res$status_code == 200) {
        res_json <- fromJSON(content(res, "text", encoding = "UTF-8"))
        texto_traducido_total <- c(texto_traducido_total, res_json[["responseData"]][["translatedText"]])
      } else {
        texto_traducido_total <- c(texto_traducido_total, bloque_actual)
      }
    }
    
    return(paste(texto_traducido_total, collapse = " "))
  }
  
  output$titulo_foto <- renderText({ 
    req(apod_data())
    traducir_texto(apod_data()[["title"]]) 
  })
  
  output$desc_foto <- renderText({ 
    req(apod_data())
    traducir_texto(apod_data()[["explanation"]]) 
  })
  
  output$media_nasa <- renderUI({
    req(apod_data())
    if(apod_data()[["media_type"]] == "image") {
      tags$img(src = apod_data()[["url"]], style = "max-width: 100%; max-height: 500px; border-radius: 10px;")
    } else {
      tags$iframe(src = apod_data()[["url"]], width = "100%", height = "400px", frameborder = "0")
    }
  })
}

shinyApp(ui, server)