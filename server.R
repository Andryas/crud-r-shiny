shinyServer(function(input, output, session) {
    iv <- shinyvalidate::InputValidator$new()
    iv$add_rule(
        "area",
        shinyvalidate::compose_rules(
            shinyvalidate::sv_required("Required"),
            shinyvalidate::sv_gt(10, message_fmt = "Minimum value 10")
        )
    )
    iv$add_rule(
        "count",
        shinyvalidate::compose_rules(
            shinyvalidate::sv_required("Required"),
            shinyvalidate::sv_integer("Must be an integer"),
            shinyvalidate::sv_gt(0, "Must be great than 0")
        )
    )
    iv$add_rule(
        "price_per_sqm",
        shinyvalidate::compose_rules(
            shinyvalidate::sv_required("Required"),
            shinyvalidate::sv_gt(100, message_fmt = "Minimum is 100")
        )
    )
    iv$add_rule(
        "exchange",
        shinyvalidate::compose_rules(
            shinyvalidate::sv_required("Required"),
            shinyvalidate::sv_integer("Must be an integer"),
            shinyvalidate::sv_gte(0, "Must be an integer great or equal 0")
        )
    )
    iv$add_rule(
        "name",
        shinyvalidate::compose_rules(
            shinyvalidate::sv_required("Required"),
            shinyvalidate::sv_regex("^[a-zA-Z]", "Only alphanumeric characters are allowed for names.")
        )
    )

    # CREATE
    shiny::observeEvent(input$create_modal, {
        shiny::showModal(
            shiny::modalDialog(
                title = "",
                size = "xl",
                easyClose = TRUE,
                footer = NULL,
                bslib::page_fluid(
                    bslib::layout_column_wrap(
                        shinyWidgets::autonumericInput(
                            inputId = "area",
                            label = "Area",
                            value = 0,
                            decimalPlaces = 2,
                            align = "center",
                            digitGroupSeparator = ".",
                            decimalCharacter = ",",
                            currencySymbolPlacement = "s",
                            currencySymbol = " m²"
                        ),
                        shinyWidgets::pickerInput(
                            inputId = "unit",
                            label = "Unit type",
                            choices = c("Residential" = "residential", "Commercial" = "commercial", "Non-Residential" = "non-residential")
                        )
                    ),
                    bslib::layout_column_wrap(
                        shinyWidgets::autonumericInput(
                            inputId = "count",
                            label = "Quantity",
                            value = 0,
                            decimalPlaces = 0,
                            align = "center",
                            digitGroupSeparator = ".",
                            decimalCharacter = ",",
                            currencySymbolPlacement = "p"
                        ),
                        shinyWidgets::autonumericInput(
                            inputId = "price_per_sqm",
                            label = HTML("Price m<sup>2</sup>"),
                            value = 0,
                            decimalPlaces = 2,
                            align = "center",
                            digitGroupSeparator = ".",
                            decimalCharacter = ",",
                            currencySymbolPlacement = "p",
                            currencySymbol = "R$ "
                        ),
                        shinyWidgets::autonumericInput(
                            inputId = "exchange",
                            label = "Exchange",
                            value = 0,
                            decimalPlaces = 0,
                            align = "center",
                            digitGroupSeparator = ".",
                            decimalCharacter = ",",
                            currencySymbolPlacement = "p",
                        )
                    ),
                    bslib::layout_column_wrap(
                        height = "100%",
                        justify_content = "space-between",
                        padding = "0px 0px 22px 0px",
                        shiny::textInput(
                            inputId = "name",
                            label = "Typology name",
                            value = "",
                            placeholder = "Write here the typology name"
                        ),
                        shiny::column(
                            12,
                            align = "center",
                            shiny::actionButton(
                                inputId = "create_bttn",
                                label = "Submit",
                                icon = icon("plus"),
                                class = "btn-primary",
                                width = "auto",
                                style = "margin-top: 17px;"
                            )
                        )
                    )
                )
            )
        )
    })

    shiny::observeEvent(input$create_bttn, {
        iv$enable()

        if (iv$is_valid()) {
            tb <- typology()

            tb <- tb |>
                dplyr::bind_rows(
                    tibble::tibble(
                        name = trimws(input$name),
                        area = input$area,
                        count = input$count,
                        price_per_sqm = input$price_per_sqm,
                        exchange = input$exchange,
                        unit = input$unit
                    )
                )

            text <- tolower(tb$name)
            text <- stringr::str_replace_all(text, "[-'`\"]+", " ")
            text <- stringi::stri_trans_general(text, "Latin-ASCII")
            text <- stringr::str_replace_all(text, "\\s+", " ")
            dedup <- duplicated(text)

            typology(tb[which(!dedup), ])

            iv$disable()

            shiny::removeModal()
        }
    })

    # READ
    typology <- shiny::reactiveVal(
        tibble::tibble(
            name = c("A", "B"),
            area = c(100, 50),
            count = c(4, 15),
            price_per_sqm = c(15000, 18000),
            exchange = c(0, 4),
            unit = c("residential", "residential")
        )
    )

    output$table <- reactable::renderReactable({
        req(typology())

        typology <- typology()

        typology |>
            dplyr::mutate(
                update = NA,
                delete = NA
            ) |>
            reactable::reactable(
                defaultColDef = reactable::colDef(
                    align = "center"
                ),
                columns = list(
                    update = reactable::colDef(
                        name = "",
                        align = "center",
                        cell = reactable.extras::button_extra("update", label = "Edit", class = "btn btn-warning btn-sm"),
                    ),
                    delete = reactable::colDef(
                        name = "",
                        align = "center",
                        cell = reactable.extras::button_extra("delete", label = "Delete", class = "btn btn-danger btn-sm"),
                    )
                )
            )
    })

    # UPDATE
    output$update <- shiny::renderUI({
        shiny::req(input$update$row)

        if (input$update$row == 0) {
            iv$disable()
            return(NULL)
        }

        data <- typology()[input$update$row, ]

        bslib::card(
            bslib::card_header(
                class = "d-flex justify-content-between",
                data$name,
                shiny::actionButton(
                    inputId = "update_close",
                    label = "",
                    icon = shiny::icon("times"),
                    class = "btn-danger"
                )
            ),
            bslib::card_body(
                bslib::page_fluid(
                    bslib::layout_column_wrap(
                        shinyWidgets::autonumericInput(
                            inputId = "area",
                            label = "Area",
                            value = data$area,
                            decimalPlaces = 2,
                            align = "center",
                            digitGroupSeparator = ".",
                            decimalCharacter = ",",
                            currencySymbolPlacement = "s",
                            currencySymbol = " m²"
                        ),
                        shinyWidgets::pickerInput(
                            inputId = "unit",
                            label = "Unit type",
                            choices = c("Residential" = "residential", "Commercial" = "commercial", "Non-Residential" = "non-residential"),
                            selected = data$unit
                        )
                    ),
                    bslib::layout_column_wrap(
                        shinyWidgets::autonumericInput(
                            inputId = "count",
                            label = "Quantity",
                            value = data$count,
                            decimalPlaces = 0,
                            align = "center",
                            digitGroupSeparator = ".",
                            decimalCharacter = ",",
                            currencySymbolPlacement = "p"
                        ),
                        shinyWidgets::autonumericInput(
                            inputId = "price_per_sqm",
                            label = HTML("Price m<sup>2</sup>"),
                            value = data$price_per_sqm,
                            decimalPlaces = 2,
                            align = "center",
                            digitGroupSeparator = ".",
                            decimalCharacter = ",",
                            currencySymbolPlacement = "p",
                            currencySymbol = "R$ "
                        ),
                        shinyWidgets::autonumericInput(
                            inputId = "exchange",
                            label = "Exchange",
                            value = data$exchange,
                            decimalPlaces = 0,
                            align = "center",
                            digitGroupSeparator = ".",
                            decimalCharacter = ",",
                            currencySymbolPlacement = "p",
                        )
                    ),
                    bslib::layout_column_wrap(
                        height = "100%",
                        justify_content = "space-between",
                        padding = "0px 0px 22px 0px",
                        shiny::textInput(
                            inputId = "name",
                            label = "Typology name",
                            value = data$name,
                            placeholder = "Write here the typology name"
                        ),
                        shiny::column(
                            12,
                            align = "center",
                            shiny::actionButton(
                                inputId = "update_modal_bttn",
                                label = "Submit",
                                icon = icon("plus"),
                                class = "btn-primary",
                                width = "auto",
                                style = "margin-top: 17px;"
                            )
                        )
                    )
                )
            )
        )
    })

    shiny::observeEvent(input$update_close, {
        shinyjs::runjs(
            sprintf(
                "Shiny.setInputValue('%s', null, {priority: 'event' })",
                "update"
            )
        )
    })

    shiny::observeEvent(input$update_modal_bttn, {
        iv$enable()

        if (iv$is_valid()) {
            tb <- typology()

            tb[input$update$row, ] <- tibble::tibble(
                name = trimws(input$name),
                area = input$area,
                count = input$count,
                price_per_sqm = input$price_per_sqm,
                exchange = input$exchange,
                unit = input$unit
            )

            typology(tb)

            iv$disable()

             shinyjs::runjs(
                sprintf(
                    "Shiny.setInputValue('%s', null, {priority: 'event' })",
                    "update"
                )
            )
        }
    })

    # DELETE
    shiny::observeEvent(input$delete, {
        typology(typology()[-input$delete$row, ])
        shinyjs::runjs(
            sprintf(
                "Shiny.setInputValue('%s', null, {priority: 'event' })",
                "delete"
            )
        )
    })
})
