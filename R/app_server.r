#' Server side logic of the application
#'
#' @param input shiny input components
#' @param output shiny output components
#' @param session the shiny session parameter
#'
#' @return ignored
#' @export
app_server <- function(input, output, session) {
    healthatlas_data <- get_data() # nolint
    config <- get_config() # nolint

    select_server("test1",
                  language = reactive(input$language),
                  healthatlas_data = healthatlas_data,
                  config = config
                  )

    if (!exists("git_hash")) {
      git_hash <- NULL
    }

    if (!exists("github_repo")) {
      github_repo <- NULL
    }

    if (isTRUE(getOption("shiny.testmode"))) {
      # Load static/dummy data if this is a test run
      healthatlas_data <- helseatlas::testdata
      healthatlas_map <- helseatlas::testmap
    }

    output$pick_language <- shiny::renderUI({
        shinyWidgets::radioGroupButtons(
          size = "sm",
          direction = "vertical",
          status = "default",
          inputId = "language",
          label = "",
          choices = c("NO" = "nb",
                      "EN" = "en"
          ),
          selected = "nb"
        )
    })

    atlas_data <- shiny::reactive({
      shiny::req(input$language)
      if (input$language == "nb") {
        num_atlas <- 1
      } else if (input$language == "en") {
        num_atlas <- 2
      }

      # Return the data for a given atlas.
      if (!is.data.frame(healthatlas_data)) {
        if (is.null(input$atlas)) {
          return(NULL)
        } else {
          return(healthatlas_data[[input$atlas]][[num_atlas]])
        }
      } else {
        return(healthatlas_data)
      }
    })

    atlas_map <- shiny::reactive({
      if (!exists("healthatlas_map")) {
        if (is.null(input$atlas)) {
          return(NULL)
        } else {
          # using UTM 33 for now (32633)
          return(sf::st_transform(healthatlas_data[[input$atlas]][["map"]], 32633))
        }
      } else {
        return(sf::st_transform(healthatlas_map, 32633))
      }
    })


    output$git_version <- shiny::renderUI({
      if (!is.null(git_hash)) {
        if (is.null(github_repo)) {
          version_num <- substr(git_hash, 1, 8)
        } else {
          version_num <- paste0("<a href='https://github.com/",
                                github_repo,
                                "/tree/",
                                git_hash,
                                "'>",
                                substr(git_hash, 1, 8),
                                "</a>")
        }
        # Hash on web page, if given
        return(shiny::HTML(paste0("Version: ", version_num)))
      }
    })

    output$title <- shiny::renderUI({
      shiny::req(input$language)
      if (!exists("webpage_title") || is.null(webpage_title)) {
        # Define the atlas title, if not defined
        webpage_title <- config$title[[input$language]]
      }

      return(shiny::HTML(paste0("<h1>", webpage_title, "</h1>")))
    })

    output$plot_map <- leaflet::renderLeaflet({
      if (is.null(input$menu_level1) | isTRUE(getOption("shiny.testmode"))) {
        return(NULL)
      }
      filtered_data <- helseatlas::filter_out(atlas_data(),
                                            filter1 = input$menu_level1,
                                            filter2 = input$menu_level2,
                                            filter3 = input$menu_level3)

      if (is.null(nrow(filtered_data)) || nrow(filtered_data) == 0) {
        # Return null if data in invalid
        return(NULL)
      }

      map <- helseatlas::make_map(map = atlas_map(), data = filtered_data)
      return(map)
    }
    )

    output$plot_histogram <- shiny::renderPlot({
      filtered_data <- helseatlas::filter_out(atlas_data(),
                                            filter1 = input$menu_level1,
                                            filter2 = input$menu_level2,
                                            filter3 = input$menu_level3)

      plot <- helseatlas::plot_variation(
        input_data = filtered_data,
        xlab = config$plot$xlab[[input$language]],
        ylab = input$menu_level1
      )
      return(plot)
    }
    , height = 800, width = 600)

    output$make_table <- shiny::renderTable({
      if (is.null(input$menu_level1)) {
        return(NULL)
      }
      filtered_data <- helseatlas::filter_out(atlas_data(),
                                            filter1 = input$menu_level1,
                                            filter2 = input$menu_level2,
                                            filter3 = input$menu_level3)

      # Return null if data in invalid
      if (is.null(nrow(filtered_data)) || nrow(filtered_data) == 0) {
        return(NULL)
      }

      tabular_data <- data.frame(filtered_data$area_name)
      colnames(tabular_data) <- c(config$plot$xlab[[input$language]])
      value_name <- as.character(unique(filtered_data$type))
      tabular_data[value_name] <- filtered_data$value
      numerator_name <- as.character(unique(filtered_data$numerator_name))
      tabular_data[numerator_name] <- filtered_data$numerator
      denominator_name <- as.character(unique(filtered_data$denominator_name))
      tabular_data[denominator_name] <- filtered_data$denominator
      # Sort data
      tabular_data <- tabular_data[order(tabular_data[, 2], na.last = TRUE, decreasing = TRUE), ]
      # Format numbers
      tabular_data[, -1] <- sapply(tabular_data[, -1],
                                   FUN = function(x) format(x,
                                                            digits = 2,
                                                            decimal.mark = config$num$decimal[[input$language]],
                                                            big.mark = config$num$big[[input$language]]
                                                            )
                                   )
      return(tabular_data)
    }
    , align = "lrrr")

  output$app_info <- shiny::renderUI({
    shiny::actionButton(
      inputId = "app_info",
      label = "",
      icon = shiny::icon("info")
    )
  })

  shiny::observeEvent(input$app_info, {
    shinyalert::shinyalert(title = config$info$title[[input$language]],
                           text = version_info(),
                           type = "",
                           closeOnEsc = TRUE, closeOnClickOutside = TRUE,
                           html = TRUE,
                           confirmButtonText =
                             sample(config$info$action_button$no_opt_out_ok[[input$language]], 1))
  })
}
