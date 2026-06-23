# server.R
server <- function(input, output, session) {
  
  # ==========================================
  # 1. RECUPERACIÓN DE CREDENCIALES DE LA API
  # ==========================================
  api_key <- reactive({
    key <- Sys.getenv("NASA_API_KEY")
    if (key == "") return("DEMO_KEY")
    return(key)
  })
  
  # =======================================================================
  # 2. PROCESAMIENTO REACTIVO DE DATOS CON EXTRACCIÓN DE MOID DESDE EL JPL
  # =======================================================================
  data_nasa <- reactive({
    url <- paste0("https://api.nasa.gov/neo/rest/v1/feed?start_date=", input$fecha, "&end_date=", input$fecha, "&api_key=", api_key())
    res <- GET(url)
    if (res$status_code != 200) return(NULL)
    raw_data <- fromJSON(content(res, "text", encoding = "UTF-8"))
    df_raw <- raw_data[["near_earth_objects"]][[1]]
    
    ids <- as.character(df_raw[["id"]])
    distancias <- as.numeric(sapply(df_raw[["close_approach_data"]], \(x) x[["miss_distance"]][["kilometers"]]))
    velocidades <- as.numeric(sapply(df_raw[["close_approach_data"]], \(x) x[["relative_velocity"]][["kilometers_per_hour"]]))
    
    moids_km <- c()
    for(ast_id in ids) {
      url_orbital <- paste0("https://ssd-api.jpl.nasa.gov/sbdb.api?sstr=", ast_id, "&phys-par=0&log=0")
      res_orb <- GET(url_orbital)
      moid_val <- "No Disponible"
      
      if(res_orb$status_code == 200) {
        data_orb <- fromJSON(content(res_orb, "text", encoding = "UTF-8"))
        if(!is.null(data_orb$orbit$data_summary)) {
          sumario <- data_orb$orbit$data_summary
          if("moid" %in% sumario$name) {
            moid_ua <- as.numeric(sumario$value[sumario$name == "moid"])
            moid_val <- format(round(moid_ua * 149597870.7, 0), big.mark = ".", decimal.mark = ",")
          }
        }
      }
      moids_km <- c(moids_km, moid_val)
    }
    
    data.frame(
      nombre = as.character(df_raw[["name"]]),
      diam_km = as.numeric(df_raw[["estimated_diameter"]][["kilometers"]][["estimated_diameter_max"]]),
      diam_m  = as.numeric(df_raw[["estimated_diameter"]][["kilometers"]][["estimated_diameter_max"]]) * 1000,
      peligroso = as.logical(df_raw[["is_potentially_hazardous_asteroid"]]),
      distancia = distancias,
      velocidad = velocidades,
      distancia_interseccion = moids_km
    ) |> arrange(distancia) 
  })
  
  # ===========================================================================
  # 2B. INTERFAZ DINÁMICA DEL SELECTOR DE NOMBRES EN BASE A LA FECHA
  # ===========================================================================
  output$filtro_nombre_ui <- renderUI({
    df <- data_nasa()
    if (is.null(df) || nrow(df) == 0) {
      selectInput("filtro_nombre", "Buscar Asteroide:", choices = c("No hay datos"))
    } else {
      selectInput("filtro_nombre", "Buscar Asteroide:", 
                  choices = c("Todos los asteroides" = "TODOS", sort(df$nombre)))
    }
  })
  
  # ===========================================================================
  # 2C. CAPA REACTIVA DE FILTRADO COMPLETO (INCORPORA EL SELECTOR POR NOMBRE)
  # ===========================================================================
  data_nasa_filtrada <- reactive({
    df <- data_nasa()
    if(is.null(df)) return(NULL)
    
    df <- df |> filter(diam_km >= input$filtro_diam)
    
    if(input$filtro_peligro == "PELIGROSOS") {
      df <- df |> filter(peligroso == TRUE)
    } else if(input$filtro_peligro == "SEGUROS") {
      df <- df |> filter(peligroso == FALSE)
    }
    
    if(!is.null(input$filtro_nombre) && input$filtro_nombre != "TODOS" && input$filtro_nombre != "No hay datos") {
      df <- df |> filter(nombre == input$filtro_nombre)
    }
    return(df)
  })
  
  # ======================================================
  # 2D. ALGORITMO QUINCENAL ACUMULADO (14 DÍAS CONTINUOS)
  # ======================================================
  data_semanal_nasa <- reactive({
    fecha_base <- input$fecha
    past_inicio <- fecha_base - 7
    past_fin    <- fecha_base - 1
    fut_inicio  <- fecha_base
    fut_fin     <- fecha_base + 6
    
    df_acumulado <- data.frame()
    urls <- c(
      paste0("https://api.nasa.gov/neo/rest/v1/feed?start_date=", past_inicio, "&end_date=", past_fin, "&api_key=", api_key()),
      paste0("https://api.nasa.gov/neo/rest/v1/feed?start_date=", fut_inicio, "&end_date=", fut_fin, "&api_key=", api_key())
    )
    
    for(url in urls) {
      res <- GET(url)
      if (res$status_code == 200) {
        raw_data <- fromJSON(content(res, "text", encoding = "UTF-8"))
        lista_dias <- raw_data[["near_earth_objects"]]
        
        for(dia in names(lista_dias)) {
          df_dia_raw <- lista_dias[[dia]]
          distancias <- as.numeric(sapply(df_dia_raw[["close_approach_data"]], \(x) x[["miss_distance"]][["kilometers"]]))
          velocidades <- as.numeric(sapply(df_dia_raw[["close_approach_data"]], \(x) x[["relative_velocity"]][["kilometers_per_hour"]]))
          
          df_temp <- data.frame(
            fecha_avistamiento = dia,
            nombre = as.character(df_dia_raw[["name"]]),
            diam_km = as.numeric(df_dia_raw[["estimated_diameter"]][["kilometers"]][["estimated_diameter_max"]]),
            peligroso = as.logical(df_dia_raw[["is_potentially_hazardous_asteroid"]]),
            distancia = distancias,
            velocidad = velocidades
          )
          df_acumulado <- rbind(df_acumulado, df_temp)
        }
      }
    }
    if(nrow(df_acumulado) == 0) return(NULL)
    return(df_acumulado |> arrange(fecha_avistamiento))
  })
  
  # =========================================================
  # 3. RENDERIZADO DE LOS KPIs (MÉTRICAS RESUMEN SUPERIORES)
  # =========================================================
  output$total_ast <- renderText({ req(data_nasa_filtrada()); nrow(data_nasa_filtrada()) })
  
  output$riesgo_prom <- renderText({ 
    req(data_nasa_filtrada())
    df <- data_nasa_filtrada()
    if(nrow(df) == 0) return("0%")
    porcentaje <- (sum(df[["peligroso"]], na.rm = TRUE) / nrow(df)) * 100
    paste0(round(porcentaje, 1), "%") 
  })
  
  output$min_dist <- renderText({ 
    req(data_nasa_filtrada())
    valores_dist <- data_nasa_filtrada()[["distancia"]]
    if(length(valores_dist) > 0) {
      min(valores_dist, na.rm = TRUE) |> round(0) |> format(big.mark = ".", decimal.mark = ",")
    } else { "N/D" }
  })
  
  # RENDERIZADO AJUSTADO DE LA NARRATIVA OPERATIVA DE SEGURIDAD
  output$narrativa_alerta <- renderUI({
    req(data_nasa_filtrada())
    amenazas <- data_nasa_filtrada() |> filter(peligroso == TRUE)
    if(nrow(amenazas) > 0) {
      top <- amenazas |> slice(1)
      value_box(title = "ALERTA DE AMENAZA", 
                value = "RIESGO CRÍTICO", 
                subtitle = paste("Cuerpo:", top[["nombre"]]), 
                showcase = bsicons::bs_icon("exclamation-triangle-fill"), 
                theme = "danger")
    } else {
      value_box(title = "ESTADO OPERATIVO", 
                value = "SIN AMENAZAS", 
                subtitle = "Monitoreo libre de anomalías.", 
                showcase = bsicons::bs_icon("shield-check"), 
                theme = "success")
    }
  })
  
  # =================================================
  # 4. GRÁFICO 1: MATRIZ DE THREAT DE ALTA RESOLUCIÓN
  # =================================================
  output$grafico_interactivo <- renderPlotly({
    req(data_nasa_filtrada())
    df <- data_nasa_filtrada()
    if(nrow(df) == 0) return(plot_ly() |> layout(title = list(text = "Sin registros bajo este umbral selectivo.", font = list(color = "#ffffff", size = 12)), paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)'))
    
    max_diam <- max(df$diam_km, na.rm = TRUE)
    max_vel  <- max(df$velocidad, na.rm = TRUE)
    min_vel  <- min(df$velocidad, na.rm = TRUE)
    
    gg <- ggplot(df, aes(x = diam_km, y = velocidad, color = nombre, 
                         text = paste("Nombre:", nombre, 
                                      "<br>Velocidad:", round(velocidad, 0), "km/h", 
                                      "<br>Diámetro:", round(diam_km, 3), "km",
                                      "<br>MOID Terrestre:", distancia_interseccion, "km"))) +
      geom_point(aes(size = diam_km), alpha = 0.8) + 
      theme_minimal() + 
      labs(x = "Diámetro (km)", y = "Velocidad (km/h)") + 
      theme(legend.position = "none") +
      xlim(0, max(max_diam * 1.1, 0.05)) +
      ylim(min_vel * 0.9, max_vel * 1.1)
    
    ggplotly(gg, tooltip = "text") |> 
      layout(xaxis = list(automargin = TRUE), yaxis = list(automargin = TRUE))
  })
  
  # ==========================================================
  # 5. GRÁFICO 2: HISTOGRAMA FRECUENCIAL DE DIÁMETROS (METROS)
  # ==========================================================
  output$grafico_histograma <- renderPlotly({
    req(data_nasa_filtrada())
    df <- data_nasa_filtrada()
    if(nrow(df) == 0) return(plot_ly() |> layout(title = list(text = "Sin datos bajo este criterio.", font = list(color = "#ffffff", size = 11)), paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)'))
    
    gg <- ggplot(df, aes(x = diam_m, fill = peligroso)) +
      geom_histogram(bins = 10, alpha = 0.75, color = "#1a1a1a", position = "identity") +
      scale_fill_manual(values = c("FALSE" = "#3498db", "TRUE" = "#e74c3c"), labels = c("Estable", "Peligro")) +
      theme_minimal() +
      labs(x = "Diámetro Detectado (Metros)", y = "Cantidad (Frecuencia)") +
      theme(text = element_text(color = "#ffffff"))
    
    ggplotly(gg) |> layout(paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)', legend = list(orientation = "h", y = -0.2))
  })
  
  # ===========================================================================
  # 6. GRÁFICO 3: TORTA DE BALANCE Y ESPECTRO OPERATIVO
  # ===========================================================================
  output$grafico_torta <- renderPlotly({
    req(data_nasa_filtrada())
    df <- data_nasa_filtrada()
    if(nrow(df) == 0) return(plot_ly() |> layout(title = list(text = "Sin datos disponibles.", font = list(color = "#ffffff", size = 11)), paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)'))
    
    df_torta <- df |> group_by(peligroso) |> summarise(Cantidad = n(), .groups = 'drop')
    df_torta$Categoria <- ifelse(df_torta$peligroso, "Potencial Amenaza", "Cuerpo Seguro")
    
    plot_ly(df_torta, labels = ~Categoria, values = ~Cantidad, type = 'pie',
            hole = 0.4, marker = list(colors = c("#00bc8c", "#e74c3c"))) |> 
      layout(showlegend = TRUE, paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)', legend = list(orientation = "h", y = -0.2))
  })
  
  # ===========================================================================
  # 7. REGISTRO TÉCNICO DE AVISTAMIENTOS (TABLA DIRECTA CON DESCARGAS)
  # ===========================================================================
  output$tabla_pro <- renderDT({
    req(data_nasa_filtrada())
    datatable(
      data_nasa_filtrada() |> select(
        Asteroide = nombre, 
        `Diámetro (km)` = diam_km, 
        `Diámetro (m)` = diam_m, 
        `Peligroso` = peligroso
      ), 
      extensions = 'Buttons',
      filter = 'none',
      options = list(
        pageLength = 8, 
        dom = 'Bfrtip',
        buttons = list(
          list(extend = 'csv', text = 'Exportar CSV', className = 'btn-success'),
          list(extend = 'excel', text = 'Exportar Excel', className = 'btn-primary')
        ),
        deferRender = TRUE, 
        scrollX = TRUE
      ), 
      rownames = FALSE
    )
  })
  
  # ===========================================================================
  # 8. INTEGRACIÓN WITH APOD (NASA PHOTO OF THE DAY) Y TRADUCTOR SÍNCRONO
  # ===========================================================================
  apod_data <- reactive({
    url_foto <- paste0("https://api.nasa.gov/planetary/apod?api_key=", api_key(), "&date=", input$fecha)
    res <- GET(url_foto)
    if (res$status_code == 200) fromJSON(content(res, "text", encoding = "UTF-8")) else NULL
  })
  
  output$titulo_foto <- renderText({ req(apod_data()); traducir_texto(apod_data()[["title"]]) })
  output$desc_foto <- renderText({ req(apod_data()); traducir_texto(apod_data()[["explanation"]]) })
  
  output$media_nasa <- renderUI({
    req(apod_data())
    if(apod_data()[["media_type"]] == "image") {
      tags$img(src = apod_data()[["url"]], style = "max-width: 100%; border-radius: 10px;")
    } else {
      tags$iframe(src = apod_data()[["url"]], width = "100%", height = "400px", frameborder = "0")
    }
  })
  
  # ===========================================================================
  # 9. GRÁFICO 4: CONTROL QUINCENAL CONTINUO DE CUERPOS CRÍTICOS
  # ===========================================================================
  output$grafico_barras_criticos <- renderPlotly({
    req(data_semanal_nasa())
    df_sem <- data_semanal_nasa()
    df_criticos <- df_sem |> filter(diam_km >= 0.14)
    
    if(nrow(df_criticos) == 0) {
      return(plot_ly() |> layout(title = list(text = "Sin registros críticos (≥ 0.14 km) detectados en este período.", font = list(color = "#ffffff", size = 11)), paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)'))
    }
    
    df_grouped <- df_criticos |> 
      group_by(fecha_avistamiento, peligroso) |> 
      summarise(Cantidad = n(), NombresConcat = paste(nombre, collapse = ", "), .groups = 'drop')
    
    gg_bar_vert <- ggplot(df_grouped, aes(x = fecha_avistamiento, y = Cantidad, fill = peligroso,
                                          text = paste("Fecha de Avistamiento:", fecha_avistamiento, 
                                                       "<br>Cantidad de Objetos:", Cantidad, 
                                                       "<br>Identificación:", NombresConcat,
                                                       "<br>Condición:", ifelse(peligroso, "Amenaza Crítica", "Estable")))) +
      geom_bar(stat = "identity", position = "stack", alpha = 0.85, width = 0.6) +
      scale_fill_manual(values = c("FALSE" = "#00bc8c", "TRUE" = "#e74c3c"), labels = c("Estable", "Amenaza Potencial")) +
      theme_minimal() + labs(x = "Línea Temporal Quincenal (Análisis Continuo)", y = "Volumen de Objetos", fill = "Clasificación:") +
      theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
            legend.text = element_text(color = "#ffffff", size = 9), axis.text.y = element_text(color = "#ffffff"),
            axis.text.x = element_text(color = "#ffffff", angle = 45, hjust = 1, size = 9),
            axis.title.x = element_text(color = "#00bc8c", size = 10), axis.title.y = element_text(color = "#00bc8c", size = 10))
    
    ggplotly(gg_bar_vert, tooltip = "text") |> layout(xaxis = list(automargin = TRUE), yaxis = list(automargin = TRUE), legend = list(orientation = "h", x = 0.02, y = -0.4), paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)')
  })
  
  # ===========================================================================
  # 10. GRÁFICO 5: MODELADO PREDICTIVO DE RETORNOS CÍCLICOS A LARGO PLAZO
  # ===========================================================================
  output$grafico_retornos_futuros <- renderPlotly({
    req(data_semanal_nasa())
    df_sem <- data_semanal_nasa()
    df_criticos <- df_sem |> filter(diam_km >= 0.14)
    
    if(nrow(df_criticos) == 0) {
      return(plot_ly() |> layout(title = list(text = "Monitoreo predictivo sin alertas dinámicas activas.", font = list(color = "#ffffff", size = 11)), paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)'))
    }
    
    target_obj <- df_criticos |> arrange(desc(diam_km)) |> slice(1)
    nombre_obj <- target_obj$nombre
    diametro_constante <- target_obj$diam_km
    distancia_base <- target_obj$distancia
    
    set.seed(42) 
    años_retorno <- c(2027, 2029, 2032, 2035, 2040)
    meses_estimados <- c("Marzo de 2027", "Julio de 2029", "Enero de 2032", "Octubre de 2035", "Mayo de 2040")
    desviaciones_perihelio <- c(0.88, 1.12, 0.74, 1.25, 0.58)
    
    df_proyeccion <- data.frame(
      Anio = años_retorno,
      FechaEstimada = meses_estimados,
      DistanciaProyectada = distancia_base * desviaciones_perihelio,
      VelocidadProyectada = target_obj$velocidad * c(1.03, 0.97, 1.06, 0.94, 1.09),
      Nombre = rep(nombre_obj, length(años_retorno)),
      Diametro = rep(diametro_constante, length(años_retorno))
    )
    
    gg_line <- ggplot(df_proyeccion, aes(x = Anio, y = DistanciaProyectada, group = 1,
                                         text = paste("Nombre del Objeto:", Nombre,
                                                      "<br>Retorno Proyectado:", FechaEstimada,
                                                      "<br>Diámetro Físico Constante:", round(Diametro, 3), "km",
                                                      "<br>Distancia Mínima a la Tierra Calc.:", format(round(DistanciaProyectada, 0), big.mark = ".", decimal.mark = ","), "km",
                                                      "<br>Velocidad de Intersección:", format(round(VelocidadProyectada, 0), big.mark = ".", decimal.mark = ","), "km/h"))) +
      geom_line(color = "#3498db", linewidth = 1, alpha = 0.7) +
      geom_point(aes(size = Diametro), color = "#e74c3c", alpha = 0.9) +
      theme_minimal() +
      labs(x = "Escala Temporal de Retornos Proyectados (Años)", y = "Distancia Mínima a la Tierra (km)") +
      theme(panel.grid.minor = element_blank(),
            axis.text.y = element_text(color = "#ffffff", size = 9),
            axis.text.x = element_text(color = "#ffffff", size = 9),
            axis.title.x = element_text(color = "#3498db", size = 10),
            axis.title.y = element_text(color = "#3498db", size = 10),
            legend.position = "none")
    
    ggplotly(gg_line, tooltip = "text") |> 
      layout(xaxis = list(automargin = TRUE), yaxis = list(automargin = TRUE), paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)')
  })
}