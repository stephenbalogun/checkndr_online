
options(shiny.maxRequestSize = 500 * 1024^2)


ui <- fluidPage(
    useShinyFeedback(),
    theme = caritas_theme(),
    titlePanel("Recency Data Quality Checker"),
    sidebarLayout(
        sidebarPanel(
            fileInput(
                inputId = "upload",
                label = "Upload line-list",
                accept = c(".csv", ".xlsx")
            ),
            pickerInput(
                inputId = "partner",
                label = "Implementing Partner",
                choices = NULL,
                multiple = TRUE,
                options = list(
                    `actions-box` = TRUE,
                    dropAuto = TRUE,
                    size = "auto",
                    `live-search` = TRUE,
                    `live-search-normalize` = TRUE
                )
            ),
            pickerInput(
                inputId = "state",
                label = "State",
                choices = NULL,
                multiple = TRUE,
                options = list(
                    `actions-box` = TRUE,
                    dropAuto = TRUE,
                    size = "auto",
                    `live-search` = TRUE,
                    `live-search-normalize` = TRUE
                )
            ),
            pickerInput(
                inputId = "lga",
                label = "LGA",
                choices = NULL,
                multiple = TRUE,
                options = list(
                    `actions-box` = TRUE,
                    dropAuto = TRUE,
                    size = "auto",
                    `live-search` = TRUE,
                    `live-search-normalize` = TRUE
                )
            ),
            pickerInput(
                inputId = "facility",
                label = "Facility",
                choices = NULL,
                multiple = TRUE,
                options = list(
                    `actions-box` = TRUE,
                    dropAuto = TRUE,
                    # mobile = TRUE,
                    size = "auto",
                    `live-search` = TRUE,
                    `live-search-normalize` = TRUE
                )
            ),
            dateRangeInput(
                "period",
                "Period",
                min = lubridate::ymd("2020-03-01"),
                format = "yyyy-mm-dd",
                language = "en"
            ),
            actionButton(
                inputId = "update",
                label = "Update",
                icon = icon("refresh"),
                class = "btn-success"
            ),
            br(),
            hr(),
            br(),
            pickerInput(
                inputId = "indicator",
                label = "Download missing or wrong entries",
                choices = recency_download_opts(),
                multiple = TRUE,
                options = list(
                    `actions-box` = TRUE,
                    dropAuto = TRUE,
                    size = "auto",
                    `live-search` = TRUE,
                    `live-search-normalize` = TRUE
                )
            ),
            downloadButton(
                "download",
                label = "download",
                icon = icon("download")
            )
        ),
        mainPanel(
            uiOutput("summarybox"),
            fluidRow(
                dataTableOutput("table")
            )
        )
    )
)



# build server component ----------------------------------------------------------------------------------------------------

server <- function(input, output, session) {
    thematic::thematic_shiny()


    # load data -----------------------------------------------------------------------------------------------------------------

    data <- reactive({
        req(input$upload)

        ext <- tools::file_ext(input$upload$name)

        shinyFeedback::feedbackDanger(
            "upload",
            !ext %in% c("csv", "xlsx"), "Please supply a line-list with csv or xlsx extension"
        )

        load_file(input$upload$name, input$upload$datapath, recency_var_names())
    })

    # update date range using data period ---------------------------------------------------------------------------------------

    observeEvent(data(), {
        range <- range(data()$visit_date, na.rm = TRUE)

        updateDateRangeInput(
            session = session,
            inputId = "period",
            min = ymd("2020-01-01"),
            max = Sys.Date(),
            start = range[[1]],
            end = range[[2]]
        )
    })


    # Populate choices of implementing partners ---------------------------------------------------------------------------------

    observeEvent(data(), {
        partners <- sort(as.character(unique(data()$ip)))

        updatePickerInput(session = session, inputId = "partner", choices = partners, clearOptions = TRUE)
    })

    partners <- reactive({
        data()[data()$ip %in% input$partner, ]
    })

    # Populate choices of states ------------------------------------------------------------------------------------------------

    observeEvent(input$partner, {

        states <- sort(as.character(unique(partners()$facility_state)))

        lgas <- sort(as.character(unique(partners()$facility_lga)))

        facilities <- sort(as.character(unique(partners()$facility)))

        updatePickerInput(session = session, inputId = "state", choices = states, selected = states, clearOptions = TRUE)

        updatePickerInput(session = session, inputId = "lga", choices = lgas, selected = lgas, clearOptions = TRUE)

        updatePickerInput(session = session, inputId = "facility", choices = facilities, selected = facilities, clearOptions = TRUE)
    })


    states <- reactive({
        partners()[partners()$facility_state %in% input$state, ]
    })


    # Populate choices of LGAs ------------------------------------------------------------------------------------------------

    observeEvent(input$state, {
        lgas <- sort(unique(as.character(states()$facility_lga)))

        facilities <- sort(as.character(unique(states()$facility)))

        updatePickerInput(session = session, inputId = "lga", choices = lgas, selected = lgas)

        updatePickerInput(session = session, inputId = "facility", choices = facilities, selected = facilities)
    })

    lgas <- reactive({
        states()[states()$facility_lga %in% input$lga, ]
    })


    # Populate choices of facilities --------------------------------------------------------------------------------------------

    observeEvent(input$lga, {
        facilities <- sort(unique(as.character(lgas()$facility)))

        updatePickerInput(session = session, inputId = "facility", choices = facilities, selected = facilities)
    })

    facilities <- reactive({
        lgas()[lgas()$facility %in% input$facility, ]
    })



    # display table -------------------------------------------------------------------------------------------------------------

    hts_pos <- eventReactive(input$update, {
        data() |>
            filter(
                ip %in% input$partner,
                facility_state %in% input$state,
                facility_lga %in% input$lga,
                facility %in% input$facility,
                between(visit_date, input$period[[1]], input$period[[2]])
            )
    })

    recency_test <- reactive({
        filter(
            hts_pos(),
            !is.na(opt_out) | is.na(opt_out) & !is.na(recency_test_name)
        )
    })

    # dt <- eventReactive(input$update, {
    #   data() |>
    #     dplyr::filter(
    #       ip %in% input$partner,
    #       facility_state %in% input$state,
    #       facility_lga %in% input$lga,
    #       facility %in% input$facility,
    #       dplyr::between(visit_date, input$period[[1]], input$period[[2]]),
    #       !is.na(opt_out)
    #     )
    # })


    valid_entries <- reactive({
        valid_cases(recency_test())
    })

    percent_valid <- reactive({
        nrow(valid_entries()) / nrow(recency_test())
    })

    output$summarybox <- renderUI({
        fluidRow(
            summaryBox2(
                "positive 15+ years",
                comma(nrow(hts_pos())),
                width = 3,
                icon = "fas fa-person",
                style = "primary"
            ),
            summaryBox2(
                "recency test",
                comma(nrow(recency_test())),
                width = 3,
                icon = "fas fa-clipboard-list",
                style = "info"
            ),
            summaryBox2(
                "valid recency entries",
                comma(nrow(valid_entries())),
                width = 3,
                icon = "fas fa-vial",
                style = "success"
            ),
            summaryBox2(
                "valid entries",
                percent(percent_valid(), accuracy = 0.1),
                width = 3,
                icon = "fas fa-yin-yang",
                style = case_when(
                    percent_valid() < 0.95 ~ "danger",
                    between(percent_valid(), 0.95, 0.999) ~ "warning",
                    percent_valid() >= 0.999 ~ "success"
                )
            )
        )
    })


    table_pos <- reactive({
        dqa_pos(hts_pos())
    })

    table_recency <- reactive({
        dqa_recency(recency_test())
    })

    output$table <- renderDataTable({
        bind_rows(table_pos(), table_recency())
    })


    # download outputs ----------------------------------------------------------------------------------------------------------

    download_one <- reactive({
        if (any(opts_one() %in% input$indicator)) {
            recency_line_list(hts_pos(), opts_one()[opts_one() %in% input$indicator])
        }
    })

    download_two <- reactive({
        if (any(opts_two() %in% input$indicator)) {
            recency_line_list(recency_test(), opts_two()[opts_two() %in% input$indicator])
        }
    })

    download_data <- shiny::reactive({
        c(download_one(), download_two())
    })

    output$download <- downloadHandler(
        filename = function() {
            paste(Sys.Date(), " ", paste(input$indicator, collapse = "_"),
                  " line-listing", ".xlsx",
                  sep = ""
            )
        },
        content = function(file) {
            write_results(download_data(), f_name = file)
        }
    )

}



# run app -------------------------------------------------------------------------------------------------------------------

shinyApp(ui, server)
