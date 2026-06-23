

ui <- page_navbar(
  title = "NEXO: Inteligencia de Defensa Planetaria",
  theme = bs_theme(version = 5, bootswatch = "darkly"),
  header = tags$head(
    tags$style(HTML("
      /* LECTURA Y LIENZO GENERAL CON SCROLL GLOBAL */
      html, body {
        height: auto !important;
        overflow-y: auto !important;
        background-color: #121212;
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif !important;
      }
      
      .container-fluid {
        height: auto !important;
        overflow-y: visible !important;
        padding-bottom: 40px !important;
      }
      
      .navbar-brand { font-size: 1.4rem !important; font-weight: 800 !important; letter-spacing: -0.5px; color: #ffffff !important; }
      
      /* CONTENEDOR DE METRICAS (KPIs) RESPONSIVO */
      .kpi-grid-container {
        display: grid !important;
        grid-template-columns: repeat(auto-fit, minmax(240px, 1fr)) !important;
        gap: 16px !important;
        margin-bottom: 30px !important;
        width: 100% !important;
      }
      
      /* TARJETAS KPI (VALUE BOXES) OPTIMIZADAS */
      .bslib-value-box, 
      #narrativa_alerta .bslib-value-box {
        min-height: 110px !important;
        height: auto !important;
        border-radius: 12px !important;
        border: 1px solid rgba(255, 255, 255, 0.08) !important;
        position: relative !important;
        overflow: hidden !important;
        box-shadow: 0 4px 12px rgba(0,0,0,0.5) !important;
      }
      
      .bslib-value-box .card-body,
      #narrativa_alerta .bslib-value-box .card-body {
        display: flex !important;
        flex-direction: column !important;
        justify-content: center !important;
        align-items: flex-start !important;
        padding: 16px 18px !important;
        z-index: 2 !important;
      }
      
      /* ENCABEZADOS DE LOS KPIs */
      .bslib-value-box .card-title,
      #narrativa_alerta .bslib-value-box .card-title {
        font-size: 0.78rem !important;
        text-transform: uppercase !important;
        letter-spacing: 1px !important;
        font-weight: 700 !important;
        color: rgba(255, 255, 255, 0.6) !important;
        margin-bottom: 6px !important;
        line-height: 1 !important;
      }
      
      /* DATOS NUMÉRICOS Y TEXTOS PRINCIPALES DE KPIs */
      .bslib-value-box .value,
      #narrativa_alerta .bslib-value-box .value {
        font-size: 1.5rem !important;
        font-weight: 800 !important;
        color: #ffffff !important;
        line-height: 1.1 !important;
        letter-spacing: -0.5px !important;
        word-break: break-word !important;
        white-space: normal !important;
        max-width: 85% !important;
      }
      
      /* INTEGRACIÓN DE ÍCONOS EN FONDO (MARCA DE AGUA) */
      .bslib-value-box .value-box-showcase,
      #narrativa_alerta .bslib-value-box .value-box-showcase {
        position: absolute !important;
        right: 12px !important;
        bottom: 4px !important;
        top: auto !important;
        transform: none !important;
        font-size: 3.2rem !important;
        opacity: 0.12 !important;
        color: #ffffff !important;
        pointer-events: none !important;
        z-index: 1 !important;
      }
      
      /* COMPONENTES GENERALES DE TARJETAS Y DESCRIPCIONES DE GRÁFICOS */
      .bloque-filtro-sidebar { margin-bottom: 25px !important; }
      .card { height: auto !important; margin-bottom: 20px !important; border-radius: 10px; box-shadow: 0 6px 12px rgba(0,0,0,0.3); }
      .card-body { padding: 16px !important; }
      
      .descripcion-tecnica-grafico {
        font-size: 0.88rem !important;
        color: rgba(255, 255, 255, 0.7) !important;
        line-height: 1.4 !important;
        margin-bottom: 12px !important;
        display: block !important;
      }
    "))
  ),

  nav_panel("Centro de Control NEO",
            layout_sidebar(
              sidebar = sidebar(
                title = "Filtros del Sistema",
                
                div(class = "bloque-filtro-sidebar",
                    dateInput("fecha", "Fecha de análisis:", value = Sys.Date(), language = "es")
                ),
                
                div(class = "bloque-filtro-sidebar",
                    sliderInput("filtro_diam", "Diámetro Mínimo Requerido (km):", min = 0, max = 0.5, value = 0, step = 0.01)
                ),
                
                div(class = "bloque-filtro-sidebar",
                    selectInput("filtro_peligro", "Espectro de Riesgo:", 
                                choices = c("Todos los cuerpos" = "TODOS", 
                                            "Solo Potencialmente Peligrosos" = "PELIGROSOS",
                                            "Solo Objetos Seguros" = "SEGUROS"))
                ),
                
                div(class = "bloque-filtro-sidebar",
                    uiOutput("filtro_nombre_ui")
                )
              ),
              
              # CONTENEDOR PRINCIPAL
              div(
                # NIVEL 1: KPIs RESUMEN CON DISEÑO PREMIUM RESPONSIVO
                div(class = "kpi-grid-container",
                    value_box(title = "Objetos Detectados", value = textOutput("total_ast"), showcase = bsicons::bs_icon("eye"), theme = "primary"),
                    value_box(title = "Riesgo Promedio", value = textOutput("riesgo_prom"), showcase = bsicons::bs_icon("percent"), theme = "warning"),
                    value_box(title = "Mínima Cercanía", value = textOutput("min_dist"), showcase = bsicons::bs_icon("geo-alt"), theme = "info"),
                    uiOutput("narrativa_alerta")
                ),
                
                # NIVEL 2: MATRIZ DE THREAT REAL Y REGISTRO TÉCNICO
                fluidRow(
                  column(width = 7,
                         card(
                           card_header("Matriz de Threat Real: Intersección Masa-Velocidad y Cálculo MOID"), 
                           card_body(
                             span(class = "descripcion-tecnica-grafico", 
                                  "Modelado cinético que cruza la masa estimada del bólido con su velocidad relativa. El tamaño de los nodos pondera el MOID mínimo calculado respecto a la órbita terrestre."),
                             plotlyOutput("grafico_interactivo", height = "450px")
                           ) 
                         )
                  ),
                  column(width = 5,
                         card(
                           card_header("Registro Técnico de Avistamientos"),
                           card_body(
                             span(class = "descripcion-tecnica-grafico", 
                                  "Telemetría cruda y segmentación paramétrica de los objetos detectados por el radar dentro de la ventana de escaneo activa."),
                             DTOutput("tabla_pro")
                           )
                         )
                  )
                ),
                
                # NIVEL NUEVOS GRÁFICOS: HISTOGRAMA Y TORTA
                fluidRow(
                  column(width = 6,
                         card(
                           card_header("Gráfico 2: Distribución Frecuencial de Diámetros (Histograma)"),
                           card_body(
                             span(class = "descripcion-tecnica-grafico", 
                                  "Clasificación estadística que agrupa los objetos según su envergadura métrica para evaluar densidades de población macro/micro espacial, preservando el espectro global de control para contextualizar búsquedas individuales."),
                             plotlyOutput("grafico_histograma", height = "280px")
                           )
                         )
                  ),
                  column(width = 6,
                         card(
                           card_header("Gráfico 3: Proporción del Espectro Operativo (Gráfico de Torta)"),
                           card_body(
                             span(class = "descripcion-tecnica-grafico", 
                                  "Representación porcentual del nivel de criticidad y clasificación de peligro de la totalidad de cuerpos celestes bajo observación."),
                             plotlyOutput("grafico_torta", height = "280px")
                           )
                         )
                  )
                ),
                
                # NIVEL 3: GRÁFICO 4 QUINCENAL
                fluidRow(
                  column(width = 12,
                         card(
                           card_header("Fase 1: Ventana de Mediano Plazo (Espectro Acumulado de 14 Días)", style = "background-color: #1a252f; border-bottom: 2px solid #00bc8c;"),
                           card_body(
                             span(class = "descripcion-tecnica-grafico", 
                                  "Monitoreo secuencial proyectado a las próximas dos semanas. Identifica picos operativos de aproximación y acumulación de amenazas en ventanas críticas de tránsito."),
                             plotlyOutput("grafico_barras_criticos", height = "320px")
                           )
                         )
                  )
                ),
                
                # NIVEL 4: GRÁFICO 5 SIMULACIÓN FUTURA
                fluidRow(
                  column(width = 12,
                         card(
                           card_header("Fase 2: Evolución Orbital a Largo Plazo (Simulación de Retornos Cíclicos)", style = "background-color: #1a252f; border-bottom: 2px solid #3498db;"),
                           card_body(
                             span(class = "descripcion-tecnica-grafico", 
                                  "Simulación computacional basada en las ecuaciones de perturbación orbital. Proyecta las recurrencias de aproximación a largo plazo para coordinar alertas tempranas multiperíodo."),
                             plotlyOutput("grafico_retornos_futuros", height = "320px")
                           )
                         )
                  )
                )
              ) # Fin del div contenedor principal
            ) # Fin de layout_sidebar
  ),
  
  nav_panel("Exploración APOD", 
            card(
              card_header(textOutput("titulo_foto")), 
              card_body(
                uiOutput("media_nasa"),
                hr(),
                tags$h5("Descripción de la NASA:", style = "color: #00bc8c; font-weight: bold;"),
                tags$div(
                  style = "padding: 15px; background-color: #2c3e50; border-radius: 8px; color: #ffffff; font-size: 1rem; line-height: 1.5;",
                  textOutput("desc_foto")
                )
              )
            )
  )
)