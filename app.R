library("dplyr")
library("DT")
library("MASS")
library("plotly")
library("rcdd")
library("shiny")
library("shinyalert")
library("shinyBS")
library("shinycssloaders")
library("shinyjs")
library("shinyvalidate")
library("stringr")
library("tidyr")
library("volesti")
library("waiter")
library("spsComps")
library("shinyWidgets")

options(warn = -1)

ui <- shinyUI(fluidPage(
  tags$head(tags$style(
    "
    .grey-out {
        background-color: #eee;
        opacity: 0.8;
    }
    "
  )),
  navbarPage(
    id = "tabs",
    position = "fixed-top",
    inverse = T,
    windowTitle = "Model representations",
    tags$style(
      type = "text/css",
      ".navbar { background-color: #000000;
                                               font-family: Arial;
                                               font-size: 13px;
                                               color: #FF0000; }",
      ".navbar-dropdown { background-color: #262626;
                                                    font-family: Arial;
                                                    font-size: 13px;
                                                    color: #FF0000; }",
      "body {padding-top: 60px;padding-bottom: 95px;}"
    ),
    tabPanel(
      "Input",
      sidebarPanel(
        style = "position:fixed;width:13%",
        width = 2,
        fluidRow(column(
          12,
          offset = 0,
          helpText("Compute H- and V-representation")
        )),
        fluidRow(column(12, offset = 0, actionButton("go_v_h", "Go"))),
        fluidRow(
          column(12,
            offset = 0,
            helpText("Probabilities")
          ),
          column(
            12,
            offset = 0,
            actionButton("rm_btn", "", icon = icon("fa-regular fa-square-minus")),
            actionButton("add_btn", "", icon = icon("fa-regular fa-square-plus"))
          ),
        ),
        fluidRow(
          column(12,
            offset = 0,
            helpText("Models")
          ),
          column(
            12,
            offset = 0,
            actionButton("rm_ie", "", icon = icon("fa-regular fa-square-minus")),
            actionButton("add_ie", "", icon = icon("fa-regular fa-square-plus"))
          )
        ),
        fluidRow(
          column(12,
            offset = 0,
            helpText("+/- range for approximate equalities")
          ),
          column(
            12,
            offset = 0,
            numericInput(
              min = 0,
              max = 1,
              step = .01,
              inputId = "approx_equal",
              label = NULL,
              value = 0
            )
          )
        ),
        fluidRow(column(12,
          offset = 0,
          helpText("Download and upload model specifications")
        )),
        fluidRow(column(
          12,
          offset = 0,
          downloadButton("download", ""),
        )),
        fluidRow(column(
          12,
          offset = 0,
          fileInput("upload", "",
            multiple = FALSE,
            accept = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv"
            )
          ),
        )),
      ),
      mainPanel(
        shinyBS::bsTooltip("textbox_ui_rel",
          'Use +, -, *, fractional and decimal numbers, p1, p2, p3, <, >, and =. Separate constraints with ";" such as "p1 < p2; p2 < p3". <br><br> Indicate intersections with "inter()" such as "inter(m1,m2)", and mixtures with mix() such as "mix(m1,m2)".<br><br>"2p1 + 3p2 < 1" will not work, "2 * p1 + 3 * p2 < 1" will work. <br><br>"(p1 + p2)/2 < .4" will not work, ".5 * p1 + .5 * p2 < .4" will work. <br><br> "p1 < {p2,3*p3}" is a shortcut for "p1 < p2; p1 < 3 * p3".',
          placement = "top", trigger = "hover"
        ),
        shinyBS::bsTooltip("textbox_ui_check",
          "Build V-representation (on/off).",
          placement = "top", trigger = "hover"
        ),
        shinyBS::bsTooltip("approx_equal",
          "+/- .05 means... <br><br> ... p1 = .5 will be set to p1 < .55 and p1 > .45, <br> ...  p1 = 1 will be set to p1 < 1 and p1 > .95,  <br> ... p1 = p2 will be set to p1 - p2 < .05 and  - p1 + p2 < .05.",
          placement = "bottom", trigger = "hover"
        ),
        waiter::use_waiter(),
        fluidRow(
          column(
            4,
            offset = 0,
            checkboxInput("check_name", "use this", value = FALSE),
            uiOutput("textbox_ui_name"),
            style = "background-color:#E8E8E8;"
          ),
          column(
            4,
            checkboxInput("check_min", "use this", value = TRUE),
            uiOutput("textbox_ui_min"),
            style = "background-color:#F8F8F8;"
          ),
          column(
            4,
            checkboxInput("check_max", "use this", value = TRUE),
            uiOutput("textbox_ui_max"),
            style = "background-color:#E8E8E8;"
          )
        ),
        fluidRow(
          column(
            12,
            offset = 0,
            checkboxInput("check_relations", "use this", value = TRUE)
          ),
        ),
        fluidRow(
          useShinyjs(),
          column(2, uiOutput("textbox_ui_name_rel")),
          column(8,  uiOutput("textbox_ui_rel")),
          column(1,fluidRow(htmlOutput("textbox_ui_check")),offset=1),
          heightMatcher("textbox_ui_check","textbox_ui_rel")
        )
      )
    ),
    tabPanel(
      "H-representation",
      sidebarPanel(
        style = "position:fixed;width:13%",
        width = 2,
        fluidRow(
          column(12,
            offset = 0,
            helpText("Download LaTeX File of H-Representation")
          ),
          column(12,
            offset = 0,
            downloadButton("d_latex", ""),
          ),
          column(12,
            offset = 0,
            helpText("Download H-repressentation for QTEST")
          ),
          column(12,
            offset = 0,
            downloadButton("d_h", "")
          )
        )
      ),
      mainPanel(
        id = "outP", fluidPage(
          withMathJax(),
          uiOutput("h")
        ),
        tags$script(
          '
            $("#go_v_h").click(function(){
                            $("#outP").removeClass("grey-out");
                        });
             $("#approx_equal").click(function(){
                            $("#outP").addClass("grey-out");
                        });
        $("#add_ie").click(function(){
                            $("#outP").addClass("grey-out");
                        });
           $("#rm_btn").click(function(){
                            $("#outP").addClass("grey-out");
                        });
        $("#add_btn").click(function(){
                            $("#outP").addClass("grey-out");
                        });
                        '
        )
      )
    ),
    tabPanel(
      "V-representation",
      sidebarPanel(
        style = "position:fixed;width:13%",
        width = 2,
        fluidRow(
          column(
            12,
            offset = 0,
            helpText("Download V-repressentation for QTEST")
          ),
          column(12,
            offset = 0,
            downloadButton("d_v", "")
          )
        ),
      ),
      mainPanel(
        fluidRow(column(
          id = "v_rep",
          12, uiOutput("v_representation_table")
        )),
        tags$script(
          '
         $("#go_v_h").click(function(){
                            $("#v_rep").removeClass("grey-out");
                        });
             $("#approx_equal").click(function(){
                            $("#v_rep").addClass("grey-out");
                        });
                        $("#rm_ie").click(function(){
                            $("#v_rep").addClass("grey-out");
                        });
        $("#add_ie").click(function(){
                            $("#v_rep").addClass("grey-out");
                        });
           $("#rm_btn").click(function(){
                            $("#v_rep").addClass("grey-out");
                        });
        $("#add_btn").click(function(){
                            $("#v_rep").addClass("grey-out");
                        });
                        '
        )
      )
    ),
    # navbarPage
    tabPanel(
      "Parsimony",
      sidebarPanel(
        style = "position:fixed;width:15%",
        width = 2,
        fluidRow(
          column(12,
            offset = 0,
            helpText("Choose algorithm")
          ),
          column(
            12,
            offset = 0,
            checkboxInput("CB", "CoolingBodies (CB)", value = T),
            checkboxInput("SoB", "SequenceOfBalls (SoB)", value = T),
            checkboxInput("CG", "CoolingGaussian (CG)", value = T)
          ),
          column(
            12,
            offset = 0,
            helpText("Number of repetitions, results are averaged")
          ),
          column(
            12,
            offset = 0,
            numericInput(
              "repetitions",
              "",
              value = 100,
              min = 1,
              step = 1
            )
          ),
          column(12,
            offset = 0,
            helpText("Compute (hyper-) volume")
          ),
          column(12,
            offset = 0,
            actionButton("go", "Go")
          ),
          column(12,
            offset = 0,
            helpText("Download results as csv-table")
          ),
          column(12,
            offset = 0,
            downloadButton("download_volume", "")
          )
        )
      ),
      mainPanel(
        fluidRow(
          column(
            id = "parsim_gr_out",
            12, div(uiOutput("parsimony_spinner"), align = "center")
          )
        ),
        fluidRow(column(
          id = "parsim_out",
          12, div(uiOutput("parsimony_spinner_table"), align = "center")
        )),
        tags$script(
          '
            $("#go_v_h").click(function(){
                            $("#parsim_table").removeClass("grey-out");
                        });
              $("#approx_equal").click(function(){
                            $("#parsim_table").addClass("grey-out");
                        });
        $("#add_ie").click(function(){
                            $("#parsim_table").addClass("grey-out");
                        });
           $("#rm_btn").click(function(){
                            $("#parsim_table").addClass("grey-out");
                        });
        $("#add_btn").click(function(){
                            $("#parsim_table").addClass("grey-out");
                        });


                    $("#go_v_h").click(function(){
                            $("#parsim_gr_out").removeClass("grey-out");
                        });
        $("#add_ie").click(function(){
                            $("#parsim_gr_out").addClass("grey-out");
                        });
           $("#rm_btn").click(function(){
                            $("#parsim_gr_out").addClass("grey-out");
                        });
        $("#add_btn").click(function(){
                            $("#parsim_gr_out").addClass("grey-out");
                        });
                        '
        )
      )
    ),
    tabPanel(
      "Plot",
      sidebarPanel(
        style = "position:fixed;width:15%",
        width = 2,
        fluidRow(column(
          12,
          offset = 0,
          helpText("Select probabilities")
        )),
        fluidRow(column(
          12,
          offset = 0,
          selectInput("dim_1", "p #",
            list(1, 2, 3),
            selected = 1
          )
        )),
        fluidRow(column(
          12,
          offset = 0,
          selectInput("dim_2", "p #",
            list(1, 2, 3),
            selected = 2
          )
        )),
        fluidRow(column(
          12,
          offset = 0,
          selectInput("dim_3", "p #",
            list(1, 2, 3),
            selected = 3
          )
        )),
        fluidRow(column(
          12,
          offset = 0,
          textAreaInput(
            inputId = "name_model_plot",
            label = "Input field for model names",
            value = "",
            width = "100%",
            height = "95px"
          )
        )),
        fluidRow(
          column(12,
            offset = 0,
            helpText("Remove/add all models")
          ),
          column(
            12,
            offset = 0,
            actionButton("subtr_models", "", icon = icon("fa-regular fa-square-minus")),
            actionButton("add_models", "", icon = icon("fa-regular fa-square-plus"))
          )
        )
      ),
      mainPanel(fluidRow(column(
        12, div(

          plotlyOutput("plot"),
          align = "center"
        )
      )))
    ),
    tags$footer(
      column(3, "Pre-publication version"),
      column(
        2,
        offset = 7,
        tags$a(
          href = "mailto:mjekel@uni-koeln.de",
          tags$b("Email feedback"),
          class = "externallink",
          style = "color: white",
          icon = icon("fa-duotone fa-envelope")
        )
      ),
      style = "
   position:fixed;
   text-align:center;
   left: 0;
   bottom:0;
   width:100%;
   z-index:1000;
   height:25px; /* Height of the footer */
   color: white;
   padding: 5px;
   background-color: #000200"
    )
  )
))

server <- shinyServer(function(input, output, session) {
  hide("hidden_b")
  hide("open_b")

  hideTab(inputId = "tabs", target = "H-representation")
  hideTab(inputId = "tabs", target = "V-representation")
  hideTab(inputId = "tabs", target = "Parsimony")
  hideTab(inputId = "tabs", target = "Plot")

  ###### Global Variables ####

  ineq_eq_left_reactive <- reactiveValues(value = NA)
  ineq_eq_right_reactive <- reactiveValues(value = NA)
  numb_p_reactive <- reactiveValues(value = NA)
  outs_reactive <- reactiveValues(value = NA)
  all_h_rep_in_matrix_reactive <- reactiveValues(value = NA)
  v_representation_plot_all_reactive <- reactiveValues(value = NA)
  all_h_rep_in_list_converted_to_v_reactive <- reactiveValues(value = NA)
  names_already_converted_v_reactive <- reactiveValues(value = NA)
  names_available_models_reactive <- reactiveValues(value = NA)
  all_operators_reactive <- reactiveValues(value = NA)
  all_v_rep_in_list_reactive <- reactiveValues(value = NA)

  ####** global variables new ####

  input_user_reactive <- reactiveValues(value = NA)

  h_representation_reactive <- reactiveValues(value = NULL)
  all_h_rep_in_matrix_reactive <- reactiveValues(value = NULL)

  v_representation_reactive <- reactiveValues(value = NULL)
  names_models_reactive <- reactiveValues(value = NULL)
  formula_h_all_reactive <- reactiveValues(value = NULL)

  # Track the number of input boxes to render
  counter <- reactiveValues(n = 3)
  counter_ie <- reactiveValues(n = 1)

  AllInputs <- reactive({
    x <- reactiveValuesToList(input)
  })


  output$download <- downloadHandler(
    filename = function() {
      paste0("user_input", ".csv")
    },
    content = function(file) {
      numb_rows_outs <- ifelse(counter$n > counter_ie$n, counter$n, counter_ie$n)

      if (input$check_relations == T) {
        outs <- matrix("", ncol = 7, nrow = numb_rows_outs)
      } else {
        outs <- matrix("", ncol = 5, nrow = counter$n)
      }

      outs <- data.frame(outs)

      for (loop in 1:counter$n) {
        outs[loop, 1:5] <- c(
          paste("p_", loop, sep = ""),
          eval(parse(
            text = str_replace_all(paste("unlist(AllInputs()$textin_name_",
              loop, ")",
              sep = ""
            ), fixed(" "), "_")
          )),
          eval(parse(
            text = paste("unlist(AllInputs()$textin_min_", loop, ")", sep = "")
          )),
          eval(parse(
            text = paste("unlist(AllInputs()$textin_max_", loop, ")", sep = "")
          )),
          input$approx_equal
        )
      }

      if (input$check_relations == T) {
        for (loop in 1:counter_ie$n) {
          outs[loop, 6:7] <- c(
            str_replace_all(eval(parse(
              text = paste(
                "unlist(AllInputs()$textin_relations_name",
                loop,
                ")",
                sep = ""
              )
            )), fixed(" "), ""),
            eval(parse(
              text = paste(
                "unlist(AllInputs()$textin_relations_",
                loop,
                ")",
                sep = ""
              )
            ))
          )
        }

        colnames(outs) <- c(
          "variable",
          "p_name",
          "p_min",
          "p_max",
          "approx_equal",
          "model_name",
          "in/equalities"
        )
      } else {
        colnames(outs) <- c("variable", "p_name", "p_min", "p_max", "approx_equal")
      }

      write.csv(outs, file)
    }
  )


  #### Button Events ####

  observeEvent(input$add_btn, {
    counter$n <- counter$n + 1

    input_user_reactive$value <- NA
  })

  observeEvent(input$add_ie, {
    counter_ie$n <- counter_ie$n + 1
  })

  observeEvent(input$rm_btn, {
    if (counter$n > 2) {
      counter$n <- counter$n - 1
    }

    input_user_reactive$value <- NA
  })

  observeEvent(input$rm_ie, {
    if (counter_ie$n > 1) {
      counter_ie$n <- counter_ie$n - 1
    }
  })

  observeEvent(input$add_models, {
    names_available_models <- isolate(names_available_models_reactive$value)

    models_to_plot <- input$name_model_plot
    models_to_plot <- unlist(str_split(models_to_plot, ";"))
    models_to_plot <- str_replace_all(models_to_plot, " ", "")
    models_to_plot <- unlist(models_to_plot)

    models_to_plot <- unique(c(models_to_plot, names_available_models))
    models_to_plot <- models_to_plot[models_to_plot != ""]

    updateTextAreaInput(
      inputId = "name_model_plot",
      value = paste(models_to_plot, collapse = ";")
    )
  })

  observeEvent(input$subtr_models, {
    models_to_plot <- input$name_model_plot
    models_to_plot <- unlist(str_split(models_to_plot, ";"))
    models_to_plot <- str_replace_all(models_to_plot, " ", "")
    models_to_plot <- unlist(models_to_plot)

    models_to_plot <- ""

    updateTextAreaInput(
      inputId = "name_model_plot",
      value = paste(models_to_plot, collapse = ";")
    )
  })

  #### Checkbox Events ####

  textboxes_min <- reactive({
    n <- counter$n

    if (n > 0) {
      isolate({
        lapply(seq_len(n), function(i) {
          numericInput(
            min = 0,
            max = 1,
            step = .01,
            inputId = paste0("textin_min_", i),
            label = paste0("Minimum of p", i),
            value = ifelse(is.null(AllInputs()[[paste0("textin_min_", i)]]) == TRUE,
              0, AllInputs()[[paste0("textin_min_", i)]]
            )
          )
        })
      })
    }
  })

  check_box_row <- reactive({
    n <- counter$n

    if (n > 0) {
      isolate({
        lapply(seq_len(n), function(i) {
          checkboxInput(
            inputId = paste0("check_row_", i),
            label = "use this",
            value = FALSE
          )
        })
      })
    }
  })

  textboxes_max <- reactive({
    n <- counter$n

    if (n > 0) {
      isolate({
        lapply(seq_len(n), function(i) {
          numericInput(
            min = 0,
            max = 1,
            step = .01,
            inputId = paste0("textin_max_", i),
            label = paste0("Maximum of p", i),
            value = ifelse(is.null(AllInputs()[[paste0("textin_max_", i)]]) == TRUE,
              1, AllInputs()[[paste0("textin_max_", i)]]
            )
          )
        })
      })
    }
  })

  textboxes_names <- reactive({
    n <- counter$n

    if (n > 0) {
      isolate({
        lapply(seq_len(n), function(i) {
          textInput(
            inputId = paste0("textin_name_", i),
            label = paste0("Name of p", i),
            value = ifelse(
              is.null(AllInputs()[[paste0("textin_name_", i)]]) == TRUE,
              paste0("p", i),
              AllInputs()[[paste0("textin_name_", i)]]
            )
          )
        })
      })
    }
  })

  textboxes_relations <- reactive({
    n <- counter_ie$n

    if (n > 0) {
      isolate({
        lapply(seq_len(n), function(i) {
          textAreaInput(
            inputId = paste0("textin_relations_", i),
            label = paste0("Model Specification"),
            value = AllInputs()[[paste0("textin_relations_", i)]],
            width = "100%",
            height = "95px"
          )
        })
      })
    }
  })

  textboxes_check <- reactive({
    n <- counter_ie$n

    if (n > 0) {
      isolate({
        lapply(seq_len(n), function(i) {
          
          materialSwitch(paste0("textin_check_", i),
                         HTML(paste("V",i,br(),br(),br(),br(),br(),br(),sep="")),
                         status="success",
                      value = AllInputs()[[paste0("textin_check_", i)]],
                      right = T)
          
        })
      })
    }
  })


  textboxes_relations_name <- reactive({
    n <- counter_ie$n

    if (n > 0) {
      isolate({
        lapply(seq_len(n), function(i) {
          textAreaInput(
            inputId = paste0("textin_relations_name", i),
            label = paste0("Model Name"),
            value = ifelse(
              is.null(AllInputs()[[paste0("textin_relations_name", i)]]) == TRUE,
              paste0("m", i),
              AllInputs()[[paste0("textin_relations_name", i)]]
            ),
            width = "100%",
            height = "95px"
          )
        })
      })
    }
  })


  output$textbox_ui_min <- renderUI({
    textboxes_min()
  })
  output$textbox_ui_name <- renderUI({
    textboxes_names()
  })
  output$textbox_ui_max <- renderUI({
    textboxes_max()
  })
  output$textbox_ui_rel <- renderUI({
    textboxes_relations()
  })
  output$textbox_ui_check <- renderUI({
    textboxes_check()
  })
  output$checkbox_ui_row <- renderUI({
    check_box_row()
  })
  output$textbox_ui_name_rel <-
    renderUI({
      textboxes_relations_name()
    })

  #### Download ####

  output$download <- downloadHandler(
    filename = function() {
      paste0("user_input", ".csv")
    },
    content = function(file) {
      outs <- matrix("", ncol = 3, nrow = counter_ie$n)


      outs <- data.frame(outs)

      for (loop in 1:counter_ie$n) {
        outs[loop, 1:3] <- c(
          str_replace_all(eval(parse(
            text = paste(
              "unlist(AllInputs()$textin_relations_name",
              loop,
              ")",
              sep = ""
            )
          )), fixed(" "), ""),
          eval(parse(
            text = paste(
              "unlist(AllInputs()$textin_relations_",
              loop,
              ")",
              sep = ""
            )
          )),
          ifelse((eval(parse(
            text = paste(
              "unlist(AllInputs()$textin_check_",
              loop,
              ")",
              sep = ""
            )
          ))) == "TRUE", 1, 0)
        )
      }


      colnames(outs) <- c(
        "model_name",
        "in/equalities",
        "V-representation"
      )


      write.csv(outs, file)
    }
  )

  #### Upload ####


  observeEvent(input$upload, {
    dats <- read.csv(input$upload$datapath)
    dats <- dats[, 2:4]



    counter_ie$n <- nrow(dats)

    for (loop_upload in 1:length(dats$in.equalities)) {
      updateTextAreaInput(session,
        inputId = paste0("textin_relations_", loop_upload),
        value = (dats$in.equalities[loop_upload])
      )


      updateMaterialSwitch(session, paste0("textin_check_", loop_upload),
        value = ifelse(dats$V.representation[loop_upload] == 1, T, F)
      )

      updateTextAreaInput(session,
        inputId = paste0("textin_relations_name", loop_upload),
        value = (dats$model_name[loop_upload])
      )
    }
  })


  ####### Validation

  observe({
    n <- counter$n
    iv <- InputValidator$new()

    for (loop_val in 1:n) {
      iv$add_rule(paste0("textin_max_", loop_val), sv_between(0, 1))
    }

    for (loop_val in 1:n) {
      comp_max <- AllInputs()[[paste0("textin_max_", loop_val)]]
      comp_min <- AllInputs()[[paste0("textin_min_", loop_val)]]

      iv$add_rule(paste0("textin_min_", loop_val), sv_between(0, 1))

      if (is.null(comp_max) == F) {
        if (comp_max >= 0 & comp_max <= 1 & is.na(comp_max) == F) {
          iv$add_rule(paste0("textin_min_", loop_val), sv_lte(comp_max))
        }
      }

      if (is.null(comp_min) == F) {
        if (comp_min >= 0 & comp_min <= 1 & is.na(comp_min) == F) {
          iv$add_rule(paste0("textin_max_", loop_val), sv_gte(comp_min))
        }
      }
    }
    iv$enable()
  })

  #### Function LaTeX ####

  latex <- function(latex_object) {
    formula_h <- ""

    latex_object[, 3:ncol(latex_object)] <- (-1 * latex_object[, 3:ncol(latex_object)])
    h_representation_pl_minus <- latex_object

    sign_p <- ifelse(data.frame(h_representation_pl_minus[, 3:ncol(h_representation_pl_minus)]) < 0, "-", "+")
    sign_p <- ifelse(data.frame(h_representation_pl_minus[, 3:ncol(h_representation_pl_minus)]) == 0, "", sign_p)

    h_representation_pl_minus[, 3:ncol(h_representation_pl_minus)] <- abs(h_representation_pl_minus[, 3:ncol(h_representation_pl_minus)])

    for (loop_pl in 1:nrow(latex_object)) {
      formula_h_act <- ((as.character(h_representation_pl_minus[loop_pl, ])))

      sign_p_act <- sign_p[loop_pl, ]

      sign_pl <- ifelse(formula_h_act[1] == "1", "=", "\\leq")


      col_names_p <- colnames(latex_object)
      col_names_p <- ifelse(formula_h_act[3:(length(formula_h_act))] != "0", col_names_p[3:length(col_names_p)], "")

      p_with_factors <- paste(formula_h_act[3:(length(formula_h_act))], " \\times  ", col_names_p,
        sep =
          ""
      )

      p_with_factors <- paste(sign_p_act, " & ", p_with_factors, sep = "")


      p_with_factors <- ifelse(col_names_p == "", " & ", p_with_factors)


      p_with_factors <- paste(p_with_factors, " & ", collapse = "")

      p_with_factors <- paste(p_with_factors, sign_pl, " & ", formula_h_act[2],
        collapse =
          ""
      )

      p_with_factors <- paste(paste(p_with_factors, collapse = " "), " \\\\",
        collapse =
          " "
      )


      formula_h <- paste(c(formula_h, p_with_factors), collapse = " ")
    }

    formula_h
  }

  #### Function Plot ####

  function_plot <- function(pl_obj, n_submodels) {
    output$plot <- renderPlotly({
      all_v_rep_in_list <- pl_obj

      models_to_plot <- input$name_model_plot
      models_to_plot <- unlist(str_split(models_to_plot, ";"))
      models_to_plot <- str_replace_all(models_to_plot, " ", "")
      models_to_plot <- unlist(models_to_plot)

      names_available_models <- rep(NA, n_submodels)

      matrix_pl_all <- numeric()

      for (loop_pl in 1:(n_submodels)) {
        extract_name <- all_v_rep_in_list[[loop_pl]]
        names_available_models[loop_pl] <- extract_name[1, 1]

        all_plot_actual <- all_v_rep_in_list[[loop_pl]]

        all_plot_actual <- all_plot_actual[, 2:ncol(all_plot_actual)]
        colnames_v_representation_plot_all <- colnames(all_plot_actual)

        all_plot_actual <- matrix(as.character(d2q(unlist((all_plot_actual)))), ncol = ncol(all_plot_actual))

        matrix_pl_all <- rbind(
          matrix_pl_all,
          data.frame(names_available_models[loop_pl], (all_plot_actual))
        )
      }

      colnames(matrix_pl_all) <- c("models", colnames_v_representation_plot_all)

      names_available_models_reactive$value <- names_available_models


      select_models_to_plot <- names_available_models[names_available_models %in% models_to_plot]

      matrix_pl_all <- matrix_pl_all[matrix_pl_all[, 1] %in% select_models_to_plot, ]

      if (length(select_models_to_plot) > 0) {
        for (loop_pl in 1:length(select_models_to_plot)) {
          all_plot_actual <- matrix_pl_all[matrix_pl_all[, 1] == select_models_to_plot[loop_pl], 2:ncol(matrix_pl_all)]

          mixture_v_plot_names <- colnames(all_plot_actual)

          name_model <- select_models_to_plot[loop_pl]
          all_plot_actual <- matrix(as.character((unlist(
            all_plot_actual
          ))), ncol = ncol(all_plot_actual))
          matrix_pl <- q2d(all_plot_actual)

          select_plot <- as.numeric(c(input$dim_1, input$dim_2, input$dim_3))

          updateSelectInput(
            inputId = "dim_1",
            choices = (1:ncol(matrix_pl))[-select_plot[c(2, 3)]],
            selected = input$dim_1
          )
          updateSelectInput(
            inputId = "dim_2",
            choices = (1:ncol(matrix_pl))[-select_plot[c(1, 3)]],
            selected = input$dim_2
          )
          updateSelectInput(
            inputId = "dim_3",
            choices = (1:ncol(matrix_pl))[-select_plot[c(1, 2)]],
            selected = input$dim_3
          )

          matrix_pl <- matrix_pl[, select_plot]

          if (is.null(dim(matrix_pl)) != TRUE) {
            matrix_pl <- data.frame(matrix(as.numeric(c(
              matrix_pl
            )), ncol = ncol(matrix_pl)))
          } else {
            matrix_pl <- t(data.frame(matrix(as.numeric(
              c(matrix_pl)
            ))))
          }


          dim_names <- mixture_v_plot_names[c(
            as.numeric(input$dim_1),
            as.numeric(input$dim_2),
            as.numeric(input$dim_3)
          )]

          colnames(matrix_pl) <- dim_names


          axx <- list(
            nticks = .1,
            range = c(0, 1),
            title = dim_names[1]
          )

          axy <- list(
            nticks = .1,
            range = c(0, 1),
            title = dim_names[2]
          )

          axz <- list(
            nticks = .1,
            range = c(0, 1),
            title = dim_names[3]
          )

          ###

          trace1 <- list(
            mode = "markers",
            type = "scatter3d",
            x = (matrix_pl[, 1]),
            y = (matrix_pl[, 2]),
            z = (matrix_pl[, 3])
          )

          trace2 <- list(
            type = "mesh3d",
            x = (matrix_pl[, 1]),
            y = (matrix_pl[, 2]),
            z = (matrix_pl[, 3]),
            opacity = 0.05,
            alphahull = 0,
            colors = colorRamp(
              c("blue", "lightblue", "chartreuse3", "yellow", "red")
            )
          )

          ### check dimensionality using principal component analysis

          prcomp_sol <- summary(prcomp(matrix_pl))$importance[2, ]
          shape_point <- ifelse(sum(prcomp_sol != 0) == 1, 1, 0)
          shape_point <- ifelse(nrow(matrix_pl) == 1, 1, shape_point)
          shape_cube <- ifelse(sum(prcomp_sol != 0) == 3, 1, 0)
          shape_cube <- ifelse(shape_point == 1, 0, shape_cube)

          if (shape_cube == 1) {
            trace2 <- list(
              type = "mesh3d",
              x = (matrix_pl[, 1]),
              y = (matrix_pl[, 2]),
              z = (matrix_pl[, 3]),
              opacity = 0.05,
              alphahull = 0,
              colors = colorRamp(
                c(
                  "blue",
                  "lightblue",
                  "chartreuse3",
                  "yellow",
                  "red"
                )
              )
            )
          }

          if (loop_pl == 1) {
            p <- plot_ly(
              width = 750,
              height = 750,
              name = name_model
            )
          }

          if (shape_point != 1) {
            p <-
              add_trace(
                p,
                mode = trace1$mode,
                type = trace1$type,
                x = trace1$x,
                y = trace1$y,
                z = trace1$z,
                name = name_model
              )

            if (shape_cube == 1) {
              p <-
                add_trace(
                  p,
                  type = trace2$type,
                  x = trace2$x,
                  y = trace2$y,
                  z = trace2$z,
                  opacity = trace2$opacity,
                  alphahull = trace2$alphahull,
                  delaunayaxis = trace2$delaunayaxis,
                  name = name_model
                )
            } else {
              p <-
                add_trace(
                  p,
                  type = trace2$type,
                  x = trace2$x,
                  y = trace2$y,
                  z = trace2$z,
                  opacity = .05,
                  alphahull = -1,
                  delaunayaxis = "x",
                  name = name_model
                )
              p <-
                add_trace(
                  p,
                  type = trace2$type,
                  x = trace2$x,
                  y = trace2$y,
                  z = trace2$z,
                  opacity = .05,
                  alphahull = -1,
                  delaunayaxis = "y",
                  name = name_model
                )
              p <-
                add_trace(
                  p,
                  type = trace2$type,
                  x = trace2$x,
                  y = trace2$y,
                  z = trace2$z,
                  opacity = .05,
                  alphahull = -1,
                  delaunayaxis = "z",
                  name = name_model
                )
            }
          } else {
            p <-
              add_trace(
                p,
                x = trace1$x,
                y = trace1$y,
                z = trace1$z,
                type = "scatter3d",
                mode = "lines+markers",
                opacity = 1,
                line = list(width = 4),
                name = name_model
              )
          }
        }

        p %>% layout(
          scene = list(
            xaxis = axx,
            yaxis = axy,
            zaxis = axz,
            aspectmode = "manual",
            aspectratio = list(x = 1, y = 1, z = 1),
            camera = list(eye = list(
              x = 2, y = 2, z = 2
            ))
          )
        )
      }
    })
  }

  #### Function to extract In/equalities from Input ####

  extract_info <- function(test = input_relations$rel1) {
    outs <- data.frame()

    for (loop in 1:counter$n) {
      outs <- rbind(
        outs,
        c(
          paste("p_", loop, sep = ""),
          str_replace_all(eval(parse(
            text = paste("unlist(AllInputs()$textin_name_", loop, ")", sep = "")
          )), fixed(" "), "_"),
          eval(parse(
            text = paste("unlist(AllInputs()$textin_min_", loop, ")", sep = "")
          )),
          eval(parse(
            text = paste("unlist(AllInputs()$textin_max_", loop, ")", sep = "")
          ))
        )
      )
    }

    ####

    colnames(outs) <- c("variable", "name", "p_min", "p_max")
    min_values <- as.numeric(outs$p_min)
    max_values <- as.numeric(outs$p_max)

    if (input$check_relations == TRUE) {
      ####* split and remove ####

      test <- unlist(str_split(test, ";"))
      test <- str_replace_all(test, " ", "")
      test <- unlist(test)

      start_short <- str_locate_all(test, fixed("{"))
      end_short <- str_locate_all(test, fixed("}"))

      test_new <- numeric()

      for (loop_short in 1:length(start_short)) {
        act_start_short <- start_short[[loop_short]][1]
        act_end_short <- end_short[[loop_short]][1]

        if (is.na(act_start_short) == FALSE) {
          if (act_start_short > 1) {
            single_part <- str_sub(test[loop_short], 1, (act_start_short - 1))
            multiple_part <- str_sub(test[loop_short], act_start_short, act_end_short)
            multiple_part <- str_replace_all(multiple_part, fixed("{"), "")
            multiple_part <- str_replace_all(multiple_part, fixed("}"), "")
            multiple_part <- unlist(str_split(multiple_part, ","))

            test_new <- c(
              test_new,
              paste(single_part, multiple_part, sep = "")
            )
          }

          if (act_start_short == 1) {
            single_part <- str_sub(test[loop_short], act_end_short + 1, nchar(test[loop_short]))
            multiple_part <- str_sub(test[loop_short], 1, act_end_short)
            multiple_part <- str_replace_all(multiple_part, fixed("{"), "")
            multiple_part <- str_replace_all(multiple_part, fixed("}"), "")
            multiple_part <- unlist(str_split(multiple_part, ","))

            test_new <- c(
              test_new,
              paste(multiple_part, single_part, sep = "")
            )
          }
        } else {
          test_new <- c(test_new, test[loop_short])
        }
      }

      test <- test_new

      ####* extract structure ####

      loc_above <- str_locate_all(test, ">")
      loc_equal <- str_locate_all(test, "=")
      loc_below <- str_locate_all(test, "<")

      loc_add <- str_locate_all(test, fixed("+"))
      loc_subtract <- str_locate_all(test, "-")
      loc_divide <- str_locate_all(test, "/")
      loc_multiply <- str_locate_all(test, fixed("*"))

      loc_digits <- str_locate_all(test, "[:digit:]")
      loc_prob <- str_locate_all(test, fixed("p"))

      loc_paren_open <- str_locate_all(test, fixed("("))
      loc_paren_closed <- str_locate_all(test, fixed(")"))
      loc_point <- str_locate_all(test, fixed("."))

      #** loop here

      loop_all <- 1
      all_extracted <- list()

      for (loop_bit in 1:length(test)) {
        test_actual <- test[loop_bit]
        test_actual_single <- unlist(str_split(test_actual, ""))

        pos <- rbind(
          "above" = data.frame(loc_above[loop_bit]),
          "below" = data.frame(loc_below[loop_bit]),
          "equal" = data.frame(loc_equal[loop_bit]),
          "add" = data.frame(loc_add[loop_bit]),
          "subtract" = data.frame(loc_subtract[loop_bit]),
          "multiply" = data.frame(loc_multiply[loop_bit]),
          "divide" = data.frame(loc_divide[loop_bit]),
          "digit" = data.frame(loc_digits[loop_bit]),
          "p" = data.frame(loc_prob[loop_bit]),
          "point" = data.frame(loc_point[loop_bit]),
          "open" = data.frame(loc_paren_open[loop_bit]),
          "closed" = data.frame(loc_paren_closed[loop_bit])
        )
        pos <- pos[order(pos[, 1]), ]

        length_str <- str_length(test_actual)

        pos <- data.frame(t(pos))

        info_type <- colnames(pos)

        info_type <- str_replace_all(info_type, fixed("."), "")
        info_type <- str_replace_all(info_type, "[:digit:]", "")

        #####* extract sub inequalities and equalities

        ineq_equ_parts_loc <- which(info_type == "above" |
          info_type == "below" |
          info_type == "equal")

        ineq_equ_parts_loc <- c(1, ineq_equ_parts_loc, length(info_type))

        loc_subparts <- numeric()

        for (loop_loc in 1:(length(ineq_equ_parts_loc) - 2)) {
          loc_act <- ineq_equ_parts_loc[c(1, 3) + (loop_loc - 1)]

          if (loop_loc == 1) {
            loc_act[2] <- loc_act[2] - 1
          }

          if (loop_loc < (length(ineq_equ_parts_loc) - 2) &
            loop_loc != 1) {
            loc_act[1] <- loc_act[1] + 1
            loc_act[2] <- loc_act[2] - 1
          }

          if (loop_loc == (length(ineq_equ_parts_loc) - 2)) {
            loc_act[1] <- loc_act[1] + 1
            loc_act[2] <- loc_act[2]
          }

          loc_subparts <- c(loc_subparts, loc_act)
        }

        loc_subparts <- t(matrix(loc_subparts, nrow = 2))

        subparts <- list()

        if (nrow(loc_subparts) == 1) {
          subparts[[paste0("part", loop_loc)]] <-
            rbind(
              info_type[1:length(info_type)],
              test_actual_single[1:length(info_type)]
            )
        } else {
          for (loop_loc in 1:nrow(loc_subparts)) {
            subparts[[paste0("part", loop_loc)]] <-
              rbind(
                info_type[loc_subparts[loop_loc, 1]:loc_subparts[loop_loc, 2]],
                test_actual_single[loc_subparts[loop_loc, 1]:loc_subparts[loop_loc, 2]]
              )
          }
        }

        ###* loop over subparts

        for (loop_sub_part in 1:length(subparts)) {
          subparts_actual <- data.frame(subparts[loop_sub_part])


          pos_p <- which(subparts_actual[1, ] == "p")
          pos_before_p <- pos_p - 1

          if (pos_before_p[1] == 0) {
            subparts_actual <- cbind(cbind(c("add", "+"), c("digit", "1"), c("multiply", "*")), subparts_actual)
          }

          if (subparts_actual[1, 1] == "digit") {
            subparts_actual <- cbind(cbind(c("add", "+")), subparts_actual)
          }

          ####

          loc_after_relation <- which(
            subparts_actual[1, ] == "below" |
              subparts_actual[1, ] == "above" |
              subparts_actual[1, ] == "equal"
          ) + 1

          if (subparts_actual[1, loc_after_relation] == "digit" |
            subparts_actual[1, loc_after_relation] == "point") {
            subparts_actual <- cbind(
              subparts_actual[, 1:(loc_after_relation - 1)],
              c("add", "+"),
              subparts_actual[, loc_after_relation:ncol(subparts_actual)]
            )
          }


          add_multiply_function <- function() {
            pos_p <- which(subparts_actual[1, ] == "p")
            pos_before_p <- pos_p - 1

            add_multiply <- ifelse(
              subparts_actual[1, pos_before_p] == "add" |
                subparts_actual[1, pos_before_p] ==
                  "below" |
                subparts_actual[1, pos_before_p] ==
                  "above" |
                subparts_actual[1, pos_before_p] ==
                  "equal",
              1,
              0
            )

            add_multiply <- ifelse(
              subparts_actual[1, pos_before_p] == "below" |
                subparts_actual[1, pos_before_p] ==
                  "above" |
                subparts_actual[1, pos_before_p] ==
                  "equal",
              2,
              add_multiply
            )

            add_multiply <- ifelse(subparts_actual[1, pos_before_p] == "subtract", -1,
              add_multiply
            )


            fix_factors <- pos_before_p[add_multiply != 0]
            add_multiply <- add_multiply[add_multiply != 0]

            return(list(fix_factors, add_multiply))
          }

          res_cadd_multiply_function <- add_multiply_function()

          fix_factors <- unlist(res_cadd_multiply_function[1])
          add_multiply <- unlist(res_cadd_multiply_function[2])

          if (length(add_multiply) > 0) {
            for (loop_add_multiply in 1:length(add_multiply)) {
              if (add_multiply[1] == -1) {
                subparts_actual <-
                  cbind(
                    subparts_actual[, 1:fix_factors[1]],
                    cbind(c("digit", "1"), c("multiply", "*")),
                    subparts_actual[, (fix_factors[1] + 1):ncol(subparts_actual)]
                  )
              }

              if (add_multiply[1] == 1) {
                subparts_actual <-
                  cbind(
                    subparts_actual[, 1:fix_factors[1]],
                    cbind(c("digit", "1"), c("multiply", "*")),
                    subparts_actual[, (fix_factors[1] + 1):ncol(subparts_actual)]
                  )
              }

              if (add_multiply[1] == 2) {
                subparts_actual <-
                  cbind(
                    subparts_actual[, 1:fix_factors[1]],
                    cbind(
                      c("add", "+"),
                      c("digit", "1"),
                      c("multiply", "*")
                    ),
                    subparts_actual[, (fix_factors[1] + 1):ncol(subparts_actual)]
                  )
              }

              res_cadd_multiply_function <- add_multiply_function()
              fix_factors <- unlist(res_cadd_multiply_function[1])
              add_multiply <- unlist(res_cadd_multiply_function[2])
            }
          }

          #####* extract p ####

          keep <- numeric()

          for (loop_extract in 1:ncol(subparts_actual)) {
            if (subparts_actual[1, loop_extract] == "p") {
              keep <- c(keep, 1)
            }

            if (loop_extract > 1 &
              subparts_actual[1, loop_extract] != "p") {
              if (subparts_actual[1, loop_extract] != "p" &
                subparts_actual[1, loop_extract] == "digit" &
                keep[loop_extract - 1] == 1) {
                keep <- c(keep, 1)
              } else {
                keep <- c(keep, 0)
              }
            }

            if (loop_extract == 1 &
              subparts_actual[1, loop_extract] != "p") {
              keep <- c(keep, 0)
            }
          }

          extract_p <- subparts_actual[2, keep == 1]
          extract_p <- paste0(extract_p, collapse = "")
          extract_p <- unlist(str_split(extract_p, "p"))
          extract_p <- extract_p[2:length(extract_p)]
          extract_p <- as.numeric(extract_p)

          ####* extract relation ####

          location_relation <- which(
            subparts_actual[1, ] == "below" |
              subparts_actual[1, ] == "above" |
              subparts_actual[1, ] == "equal"
          )

          relation_subpart <- subparts_actual[1, location_relation]
          relation_subpart <- unlist(relation_subpart)

          ####* extract factors ####

          left_side <- data.frame(subparts_actual[, 1:(location_relation -
            1)])
          right_side <- data.frame(subparts_actual[, (location_relation +
            1):ncol(subparts_actual)])


          location_add_subtract_left <- which(left_side[1, ] == "subtract" |
            left_side[1, ] == "add")

          location_p_left <- which(left_side[1, ] == "p")


          location_add_subtract_right <- which(right_side[1, ] == "subtract" |
            right_side[1, ] == "add")

          location_p_right <- which(right_side[1, ] == "p")


          ####** left side

          factor_left <- numeric()

          if (length(location_p_left) == 0) {
            factor_left <- paste0(left_side[2, ], collapse = "")
            extract_p <- c("numb", extract_p)
          } else {
            for (loop_loc in 1:length(location_add_subtract_left)) {
              factor_left <- c(
                factor_left,
                paste0(left_side[2, location_add_subtract_left[loop_loc]:(location_p_left[loop_loc] -
                  2)], collapse = "")
              )
            }
          }

          ####** right side

          factor_right <- numeric()

          if (length(location_p_right) == 0) {
            factor_right <- paste0(right_side[2, ], collapse = "")

            extract_p <- c(extract_p, "numb")
          } else {
            for (loop_loc in 1:length(location_add_subtract_right)) {
              factor_right <- c(
                factor_right,
                paste0(right_side[2, location_add_subtract_right[loop_loc]:(location_p_right[loop_loc] -
                  2)], collapse = "")
              )
            }
          }

          ####** combine extract

          extracted <- rbind(
            extract_p,
            c(
              rep("left", length(
                factor_left
              )),
              rep("right", length(
                factor_right
              ))
            ),
            c(factor_left, factor_right),
            relation_subpart
          )

          row.names(extracted) <- c("p", "left/right", "factor", "relation")
          colnames(extracted) <- paste("sub_", 1:ncol(extracted), sep = "")
          extracted <- data.frame(extracted)


          all_extracted[[paste0("line", loop_all)]] <- extracted

          loop_all <- loop_all + 1
        }
      }
    }


    if (input$check_relations == FALSE) {
      all_extracted <- list()
      numb_p <- 0
    }


    if (length(all_extracted) > 0) {
      numb_p <- numeric()

      for (loop_numb_p in 1:length(all_extracted)) {
        numb_p <- c(numb_p, (unlist(data.frame(
          all_extracted[loop_numb_p]
        )[1, ])))
      }

      numb_p <- numb_p[numb_p != "numb"]
      numb_p <- as.numeric(numb_p)
      numb_p <- max(numb_p)
    } else {
      all_extracted <- list()
      numb_p <- 0
    }


    ###

    if (input$check_min == TRUE) {
      numb_p_min <- length(min_values)
      numb_p <- ifelse(numb_p > numb_p_min, numb_p, numb_p_min)

      extend_all_extracted_numb <- length(all_extracted) + 1

      for (loop_min_max in 1:numb_p_min) {
        actual_min <- min_values[loop_min_max]

        extend <- (rbind(
          "p" = c(loop_min_max, "numb"),
          "left/right" = c("left", "right"),
          "factor" = c(+1, actual_min),
          "relation" = c("above", "above")
        ))

        colnames(extend) <- c("sub_1", "sub_2")


        all_extracted[[paste0("line", extend_all_extracted_numb)]] <-
          extend


        extend_all_extracted_numb <- extend_all_extracted_numb + 1
      }
    }

    ###

    if (input$check_max == TRUE) {
      numb_p_max <- length(max_values)
      numb_p <- ifelse(numb_p > numb_p_max, numb_p, numb_p_max)

      if (exists("extend_all_extracted_numb") == FALSE) {
        extend_all_extracted_numb <- length(all_extracted) + 1
      }


      for (loop_min_max in 1:numb_p_max) {
        actual_max <- max_values[loop_min_max]

        extend <- (rbind(
          "p" = c(loop_min_max, "numb"),
          "left/right" = c("left", "right"),
          "factor" = c(+1, actual_max),
          "relation" = c("below", "below")
        ))

        colnames(extend) <- c("sub_1", "sub_2")


        all_extracted[[paste0("line", extend_all_extracted_numb)]] <-
          extend

        extend_all_extracted_numb <- extend_all_extracted_numb + 1
      }
    }

    ###

    numb_equl_ineq <- length(all_extracted)

    ineq_eq_left <- matrix(NA, ncol = numb_p, nrow = numb_equl_ineq)
    ineq_eq_right <- rep(NA, numb_equl_ineq)
    all_operators <- rep(NA, numb_equl_ineq)

    ###

    for (loop_relations in 1:length(all_extracted)) {
      actual_relations <- data.frame(all_extracted[loop_relations])

      extract_factors <- numeric()

      for (loop_factors in 1:ncol(actual_relations)) {
        extract_factors <- c(extract_factors, eval(parse(text = actual_relations[3, loop_factors])))
      }

      actual_relations[3, ] <- (extract_factors)

      actual_relations_left <- actual_relations[, actual_relations[2, ] == "left"]
      actual_relations_right <- actual_relations[, actual_relations[2, ] == "right"]
      actual_relations_operator <- actual_relations[4, 1]

      actual_relations_left <- data.frame(actual_relations_left)
      actual_relations_right <- data.frame(actual_relations_right)

      numb_left <- which(actual_relations_left[1, ] == "numb")
      numb_right <- which(actual_relations_right[1, ] == "numb")

      non_numb_left <- which(actual_relations_left[1, ] != "numb")
      non_numb_right <- which(actual_relations_right[1, ] != "numb")

      #### change signs

      if (actual_relations_operator == "equal" &
        length(non_numb_right) > 0 & length(non_numb_left) > 0) {
        actual_relations_right[3, non_numb_right] <- -1 * as.numeric(actual_relations_right[3, non_numb_right])
      }

      if (actual_relations_operator == "below" &
        length(non_numb_right) > 0) {
        actual_relations_right[3, non_numb_right] <- -1 * as.numeric(actual_relations_right[3, non_numb_right])
        actual_relations_left[3, numb_left] <- -1 * as.numeric(actual_relations_left[3, numb_left])
      }

      if (actual_relations_operator == "above" &
        length(non_numb_left) > 0) {
        actual_relations_left[3, non_numb_left] <- -1 * as.numeric(actual_relations_left[3, non_numb_left])
        actual_relations_right[3, numb_right] <- -1 * as.numeric(actual_relations_right[3, numb_right])
        actual_relations_operator <- "below"
        actual_relations_left[4, ] <- "below"
        actual_relations_right[4, ] <- "below"
      }

      ## do fractions

      actual_relations_left <- data.frame(actual_relations_left)
      actual_relations_right <- data.frame(actual_relations_right)

      actual_relations_left[3, ] <- (d2q(as.numeric(actual_relations_left[3, ])))
      actual_relations_right[3, ] <- (d2q(as.numeric(actual_relations_right[3, ])))


      ## translate into RCDD input

      total_rcdd <- data.frame(actual_relations_left, actual_relations_right)

      total_rcdd_leftside <- data.frame(total_rcdd[, total_rcdd[1, ] != "numb"])
      total_rcdd_rightside <- data.frame(total_rcdd[, total_rcdd[1, ] == "numb"])

      if (length(total_rcdd_rightside) == 0) {
        total_rcdd_rightside <- t(data.frame("numb", "right", "0", actual_relations_operator))
      }

      ###

      ineq_eq_left[loop_relations, as.numeric(unlist(total_rcdd_leftside[1, ]))] <- unlist(total_rcdd_leftside[3, ])
      ineq_eq_left[loop_relations, -as.numeric(unlist(total_rcdd_leftside[1, ]))] <- "0"
      ineq_eq_right[loop_relations] <- total_rcdd_rightside[3, ]
      all_operators[loop_relations] <- actual_relations_operator
    }

    #### approx equal

    tune_knob <- input$approx_equal

    add_ineq_eq_left <- numeric()
    add_ineq_eq_right <- numeric()
    add_all_operators <- numeric()

    if (tune_knob != 0) {
      change_knob <- ifelse(rowSums(ineq_eq_left != "0") > 1, 1, 0)


      for (loop_tune in 1:length(all_operators)) {
        if (all_operators[loop_tune] == "equal") {
          if (change_knob[loop_tune] == 1) {
            add_ineq_eq_left <- rbind(add_ineq_eq_left, d2q(q2d(ineq_eq_left[loop_tune, ])))
            add_ineq_eq_left <- rbind(add_ineq_eq_left, d2q(-1 * q2d(ineq_eq_left[loop_tune, ])))

            add_ineq_eq_right <- c(
              add_ineq_eq_right,
              q2d(ineq_eq_right[loop_tune]) + tune_knob,
              (q2d(ineq_eq_right[loop_tune]) + tune_knob)
            )

            add_all_operators <- c(add_all_operators, rep("below", 2))
          } else {
            add_ineq_eq_left <- rbind(add_ineq_eq_left, d2q(q2d(ineq_eq_left[loop_tune, ])))
            add_ineq_eq_left <- rbind(add_ineq_eq_left, d2q(-1 * q2d(ineq_eq_left[loop_tune, ])))

            add_ineq_eq_right <- c(
              add_ineq_eq_right,
              q2d(ineq_eq_right[loop_tune]) + tune_knob,
              (-(
                q2d(ineq_eq_right[loop_tune]) - tune_knob
              ))
            )

            add_all_operators <- c(add_all_operators, rep("below", 2))
          }
        }
      }

      add_ineq_eq_right <- d2q(add_ineq_eq_right)

      ineq_eq_left <- ineq_eq_left[all_operators != "equal", ]
      ineq_eq_right <- ineq_eq_right[all_operators != "equal"]
      all_operators <- all_operators[all_operators != "equal"]

      ineq_eq_left <- rbind(ineq_eq_left, add_ineq_eq_left)
      ineq_eq_right <- c(ineq_eq_right, add_ineq_eq_right)
      all_operators <- c(all_operators, add_all_operators)
    }


    all_operators_reactive$value <- all_operators
    ineq_eq_left_reactive$value <- ineq_eq_left
    ineq_eq_right_reactive$value <- ineq_eq_right
    numb_p_reactive$value <- numb_p
    outs_reactive$value <- outs
  }

  #### Function for Representations ####

  ####* H-representation ####

  observeEvent(input$approx_equal, {
    for (loop_approx in 1:length(input_user_reactive$value)) {
      if (grepl("=", input_user_reactive$value[loop_approx]) == T) {
        h_representation_reactive$value[loop_approx] <- NULL
        v_representation_reactive$value[loop_approx] <- NULL
      }
    }
  })

  observeEvent(input$go_v_h, {
    outs <- isolate(outs_reactive$value)

    ####** check what to convert ####

    input_options <- list()

    ####*** add h when v is selected ####

    for (loop in 1:counter_ie$n) {
      opt <-
        eval(parse(
          text = paste("unlist(AllInputs()$textin_check_", loop, ")", sep = "")
        ))

      if (opt == TRUE) {
        opt <- unique(c("V", "H"))
      }

      if (is.null(opt) == F) {
        input_options[[paste0("opt", loop)]] <- opt
      } else {
        input_options[[paste0("opt", loop)]] <- ""
      }
    }

    ####*** check if input is the same ####

    input_user <- numeric()

    for (loop in 1:counter_ie$n) {
      input_user <- c(
        input_user,
        eval(parse(
          text = paste("unlist(AllInputs()$textin_relations_", loop, ")", sep = "")
        ))
      )
    }

    was_input_before <- numeric()
    h_available <- numeric()
    v_available <- numeric()

    for (loop_check_input in 1:length(input_user)) {
      was_input_before <- c(
        was_input_before,
        ifelse(input_user[loop_check_input] ==
          input_user_reactive$value[loop_check_input], 1, 0)
      )

      h_available <- c(h_available, ifelse(is.null(
        unlist(h_representation_reactive$value[loop_check_input])
      ) == FALSE, 1, 0))

      v_available <- c(v_available, ifelse(is.null(
        unlist(v_representation_reactive$value[loop_check_input])
      ) == FALSE, 1, 0))
    }

    was_input_before <- ifelse(is.na(was_input_before) == T,
      0, was_input_before
    )

    input_user_reactive$value <- input_user


    ####*** check whether v is selected ####


    index_convert_to_v <- numeric()

    for (loop_check in 1:length(input_options)) {
      index_convert_to_v <- c(
        index_convert_to_v,
        ifelse(sum(unlist(
          input_options[loop_check]
        ) %in% "V" == T) == 1, 1, 0)
      )
    }

    ####** check whether intersections or mixtures are included ####

    inter_ind <- numeric()
    mix_ind <- numeric()

    n_models <- length(input_user_reactive$value)
    inter_input <- list()
    inter_input[1:n_models] <- (NA)
    mix_input <- list()
    mix_input[1:n_models] <- (NA)

    for (loop_inter_mix in 1:length(input_user_reactive$value)) {
      inter_ind <- c(
        inter_ind,
        ifelse(grepl("inter", unlist(input_user_reactive$value[loop_inter_mix])),
          1, 0
        )
      )

      mix_ind <- c(
        mix_ind,
        ifelse(grepl("mix", unlist(input_user_reactive$value[loop_inter_mix])),
          1, 0
        )
      )

      ####*** intersection ####

      if (inter_ind[loop_inter_mix] == 1) {
        inp <- unlist(input_user_reactive$value[loop_inter_mix])

        inp <- substr(
          inp, 7,
          str_length(inp) - 1
        )

        inp <- unlist(str_split(inp, ","))


        inter_input[loop_inter_mix] <- list(inp)

        if (sum(was_input_before[which(inp %in% names_models_reactive$value)]) <
          length(was_input_before[which(inp %in% names_models_reactive$value)])) {
          was_input_before[loop_inter_mix] <- 0
        }
      }

      ####*** mixture ####

      if (mix_ind[loop_inter_mix] == 1) {
        inp <- unlist(input_user_reactive$value[loop_inter_mix])

        inp <- str_replace_all(inp, " ", "")

        inp <- substr(
          inp, 5,
          str_length(inp) - 1
        )

        inp <- str_split(inp, ",")

        mix_input[loop_inter_mix] <- list(inp)

        if (sum(was_input_before[which(inp %in% names_models_reactive$value)]) <
          length(was_input_before[which(inp %in% names_models_reactive$value)])) {
          was_input_before[loop_inter_mix] <- 0
        }
      }
    }

    ####

    all_input_models_names <- numeric()

    for (loop_numb_models in 1:length(was_input_before)) {
      all_input_models_names <- c(
        all_input_models_names,
        str_replace_all(eval(parse(
          text = paste(
            "unlist(AllInputs()$textin_relations_name",
            loop_numb_models,
            ")",
            sep = ""
          )
        )), fixed(" "), "_")
      )
    }

    names_models_reactive$value <- all_input_models_names

    models_for_mixture <- unlist(mix_input)
    models_for_mixture <- models_for_mixture[is.na(models_for_mixture) == FALSE]

    alert_mix_v <- 0

    if (sum(index_convert_to_v[all_input_models_names %in% models_for_mixture]) !=
      length(index_convert_to_v[all_input_models_names %in% models_for_mixture])) {
      alert_mix_v <- 1

      index_convert_to_v[all_input_models_names %in% models_for_mixture] <- 1
      
      ind_add =  which(all_input_models_names %in% models_for_mixture)
      ind_add = c(ind_add,which(mix_ind == 1))
      
      for(loopAdd in ind_add){
        updateMaterialSwitch(session, paste0("textin_check_", loopAdd),
                             value = 1)
        
      }
      
    
      
      
      
    }

    numb_models <- isolate(counter_ie$n)

    ####** start conversion ####

    if (sum(input_user_reactive$value == "") == 0 & (
      sum(index_convert_to_v) != 0 |
        sum(h_available) < length(h_available) |
        sum(was_input_before) < length(was_input_before))) {
      showTab(inputId = "tabs", target = "Parsimony")
      showTab(inputId = "tabs", target = "Plot")
      showTab(inputId = "tabs", target = "V-representation")
      showTab(inputId = "tabs", target = "H-representation")

      ####** function variables ####


      all_h_rep_in_list <- isolate(h_representation_reactive$value)
      all_h_rep_in_matrix <- isolate(all_h_rep_in_matrix_reactive$value)


      all_input_models <- numeric()


      ####

      input_relations <- list()


      for (loop in 1:counter_ie$n) {
        input_relations[[paste0("rel", loop)]] <-
          eval(parse(
            text = paste(
              "unlist(AllInputs()$textin_relations_",
              loop,
              ")",
              sep = ""
            )
          ))
      }


      ##


      for (loop_numb_models in 1:numb_models) {
        all_input_models <- c(all_input_models, (eval(parse(
          text = paste("input_relations$rel",
            loop_numb_models,
            sep =
              ""
          )
        ))))
      }

      ####

      for (loop_numb_models in 1:length(inter_ind)) {
        if (inter_ind[loop_numb_models] == 1) {
          pick_for_inter <- unlist(inter_input[loop_numb_models])

          input_relations[loop_numb_models] <- paste(unlist(input_relations[all_input_models_names %in%
            pick_for_inter]), collapse = ";")
        }
      }


      ####** loop models ####

      formula_h_all <- isolate(formula_h_all_reactive$value)

      for (loop_numb_models in 1:numb_models) {
        if ((h_available[loop_numb_models] == 0 & was_input_before[loop_numb_models] == 0 |
          h_available[loop_numb_models] == 0 & was_input_before[loop_numb_models] == 1 |
          h_available[loop_numb_models] == 1 & was_input_before[loop_numb_models] == 0)
        ) {
          if (mix_ind[loop_numb_models] == 0) {
            extract_info(eval(parse(
              text = paste("input_relations$rel",
                loop_numb_models,
                sep = ""
              )
            )))

            outs <- isolate(outs_reactive$value)

            ineq_eq_left <- isolate(ineq_eq_left_reactive$value)
            ineq_eq_right <- isolate(ineq_eq_right_reactive$value)

            all_operators <- isolate(all_operators_reactive$value)

            if (sum(all_operators != "equal") > 0 &
              sum(all_operators == "equal") > 0) {
              h_representation <- makeH(
                ineq_eq_left[all_operators != "equal", ],
                ineq_eq_right[all_operators != "equal"],
                ineq_eq_left[all_operators == "equal", ],
                ineq_eq_right[all_operators == "equal"]
              )
            }


            if (sum(all_operators != "equal") == 0 &
              sum(all_operators == "equal") > 0) {
              h_representation <- makeH(
                a2 = ineq_eq_left,
                b2 = ineq_eq_right
              )
            }

            if (sum(all_operators != "equal") > 0 &
              sum(all_operators == "equal") == 0) {
              h_representation <- makeH(
                ineq_eq_left,
                ineq_eq_right
              )
            }

            if (nrow(h_representation) > 1) {
              h_representation <- redundant(h_representation)$output
            } else {
              h_representation <- redundant(h_representation)$output
            }

            all_h_rep_in_list[[loop_numb_models]] <- h_representation

            ###

            current_name <- all_input_models_names[loop_numb_models]


            h_representation_pl <- q2d(h_representation)
            h_representation_pl <- fractions(h_representation_pl)


            if (input$check_name == T) {
              colnames(h_representation_pl) <- c(
                "ineq/eq",
                "right",
                paste(" \\text{ ", outs$name, " }", sep = "")
              )
            } else {
              colnames(h_representation_pl) <- c(
                "ineq/eq",
                "right",
                paste("p_{", 1:numb_p_reactive$value, "}", sep = "")
              )
            }

            begin_eq <- paste(current_name, " $$\\begin{eqnarray} ", sep = "")
            end_eq <- " \\end{eqnarray}$$"
            equation_all <- paste(begin_eq, latex(h_representation_pl), end_eq,
              sep =
                ""
            )

            formula_h_all[loop_numb_models] <- unlist(equation_all)
          } else {
            if (is.null(isolate(formula_h_all_reactive$value[loop_numb_models])) == FALSE) {
              if (length(isolate(formula_h_all_reactive$value)) == loop_numb_models) {
                formula_h_all[loop_numb_models] <- isolate(formula_h_all_reactive$value[loop_numb_models])
              }
            }
          }
        }

        formula_h_all_reactive$value[loop_numb_models] <- formula_h_all[loop_numb_models]


        h_representation_reactive$value <- all_h_rep_in_list

        ####* V-representation ####

        all_v_rep_in_list <- isolate(v_representation_reactive$value)
        error_null_inter <- 0
      }

      if (sum(index_convert_to_v) > 0) {
        for (loop_numb_models in 1:length(names_models_reactive$value)) {
          if (((index_convert_to_v[loop_numb_models] == 1 & v_available[loop_numb_models] == 0 & was_input_before[loop_numb_models] == 0) | (index_convert_to_v[loop_numb_models] == 1 & v_available[loop_numb_models] == 1 & was_input_before[loop_numb_models] == 0) | (index_convert_to_v[loop_numb_models] == 1 & v_available[loop_numb_models] == 0 & was_input_before[loop_numb_models] == 1)) & mix_ind[loop_numb_models] == 0) {
            waiter <- waiter::Waiter$new(
              html = spin_solar(),
              fadeout = 1000
            )
            waiter$show()
            on.exit(waiter$hide())



            current_name <- all_input_models_names[loop_numb_models]

            h_representation <- all_h_rep_in_list[[loop_numb_models]]

            v_representation <- scdd(h_representation)

            v_representation <- matrix(
              sapply(v_representation$output, function(x) {
                eval(parse(text = x))
              }),
              ncol = ncol(v_representation$output)
            )

            v_representation_plot <- data.frame(v_representation)
            v_representation_plot <- v_representation_plot[, 3:ncol(v_representation_plot)]

            if (input$check_name == T) {
              colnames(v_representation_plot) <- outs$name
            } else {
              colnames(v_representation_plot) <- paste("p_", 1:ncol(v_representation_plot),
                sep =
                  ""
              )
            }


            if (nrow(v_representation_plot) > 0) {
              row.names(v_representation_plot) <- paste("V_",
                1:nrow(v_representation_plot),
                "_",
                current_name,
                sep = ""
              )

              v_representation_plot <- cbind("model_name" = current_name, v_representation_plot)
            } else {
              error_null_inter <- 1
              shinyalert("Error", paste("Intersection as defined in ", current_name, " has volume = 0. Please change or remove the model.", collapse = ""),
                type = "error", closeOnClickOutside = T
              )
            }



            all_v_rep_in_list[loop_numb_models] <-
              list(v_representation_plot)
          }
        }

        if (error_null_inter == 0) {
          v_representation_reactive$value <- all_v_rep_in_list

          ####** create table ####

          if (sum(index_convert_to_v) > 0) {
            showTab(inputId = "tabs", target = "V-representation")
            showTab(inputId = "tabs", target = "Plot")

            v_table <- bind_rows(all_v_rep_in_list)

            v_table[, 2:ncol(v_table)] <-
              as.character(fractions(matrix(
                ((unlist(v_table[, 2:ncol(v_table)]))
                ),
                ncol =
                  ncol(v_table) - 1
              )))
          }
        }
      } else {
        hideTab(inputId = "tabs", target = "V-representation")
        hideTab(inputId = "tabs", target = "Plot")
      }
    } else {
      if (sum(input_user_reactive$value == "") > 0) {
        shinyalert("Error", "Please type in all model specifications.", type = "error", closeOnClickOutside = T)
      }
    }

    ####* Mixture ####

    for (loop_mix in which(mix_ind == 1)) {
      if (was_input_before[loop_mix] != 1) {
        waiter <- waiter::Waiter$new(
          html = spin_solar(),
          fadeout = 1000
        )
        waiter$show()
        on.exit(waiter$hide())

        act_mix <- unlist(mix_input[loop_mix])

        v_as_data_frame <- as.data.frame(do.call(rbind, all_v_rep_in_list[names_models_reactive$value %in% act_mix]))

        v_all <- v_as_data_frame[v_as_data_frame[, 1] %in% act_mix, ]
        v_all <- v_all[, 2:ncol(v_all)]

        v_all <- matrix(as.numeric(unlist(v_all)), ncol = ncol(v_all))
        v_all <- v_all[!duplicated(v_all), ]

        v_all <- d2q(v_all)
        mixture_v <- makeV(v_all)
        dim_mixture <- dim(mixture_v)[1]

        if (dim_mixture > 1) {
          mixture_v <- redundant(mixture_v)
        }

        all_v_rep_in_list[loop_mix] <- list(mixture_v$output)

        if (dim_mixture > 1) {
          mixture_h <- scdd(mixture_v$output)
          mixture_h <- redundant(mixture_h$output)
        } else {
          mixture_h <- scdd(mixture_v)
        }

        mixture_h <- (mixture_h$output)

        h_representation_pl <- fractions(q2d(mixture_h))

        all_h_rep_in_list[loop_mix] <- list(h_representation_pl)

        h_representation_reactive$value[loop_mix] <- list(mixture_h)

        ####

        if (input$check_name == T) {
          colnames(h_representation_pl) <- c(
            "ineq/eq",
            "right",
            paste(" \\text{ ", outs$name, " }", sep = "")
          )
        } else {
          colnames(h_representation_pl) <- c(
            "ineq/eq",
            "right",
            paste("p_{", 1:numb_p_reactive$value, "}", sep = "")
          )
        }

        begin_eq <- paste(unlist(names_models_reactive$value[loop_mix]), " $$\\begin{eqnarray} ", sep = "")
        end_eq <- " \\end{eqnarray}$$"
        equation_all <- paste(begin_eq, latex(h_representation_pl), end_eq,
          sep = ""
        )

        formula_h_all[loop_mix] <- unlist(equation_all)

        formula_h_all_reactive$value[loop_mix] <- formula_h_all[loop_mix]

        ####

        all_h_rep_in_list[loop_mix] <- list(mixture_h)

        ####

        if (dim_mixture > 1) {
          mixture_v <- matrix(
            sapply(mixture_v$output, function(x) eval(parse(text = x))),
            ncol = ncol(mixture_v$output)
          )
        } else {
          mixture_v <- matrix(
            sapply(mixture_v, function(x) eval(parse(text = x))),
            ncol = ncol(mixture_v)
          )
        }

        mixture_v_plot <- data.frame(mixture_v)
        mixture_v_plot <- mixture_v_plot[, 3:ncol(mixture_v_plot)]

        mixture_v_plot <- (fractions(matrix(unlist(mixture_v_plot), ncol = ncol(mixture_v_plot))))

        mixture_v_plot <- data.frame(unlist(names_models_reactive$value[loop_mix]), mixture_v_plot)

        colnames(mixture_v_plot) <- c("model_name", paste("p_", 1:(ncol(mixture_v_plot) - 1), sep = ""))


        all_v_rep_in_list[loop_mix] <- list(mixture_v_plot)

        v_representation_reactive$value[loop_mix] <- list(mixture_v_plot)
      }
    }

    ####* Formula H-Representation ####

    if (sum(input_user_reactive$value == "") == 0) {
      names_p <- numeric()

      for (loop in 1:counter$n) {
        names_p <- rbind(
          names_p,
          c(
            paste("p_{", loop, "}", sep = ""),
            eval(parse(
              text = paste("unlist(AllInputs()$textin_name_", loop, ")", sep = "")
            ))
          )
        )
      }

      name_model <- numeric()

      for (loop in 1:numb_models) {
        name_model <- c(
          name_model,
          str_replace_all(eval(parse(text = paste("unlist(AllInputs()$textin_relations_name", loop, ")", sep = ""))), fixed(" "), "_")
        )
      }

      names_models_reactive$value <- name_model

      formula_h_all <- isolate(formula_h_all_reactive$value)
      formula_h_all <- paste(formula_h_all, collapse = "")

      if (input$check_name == T) {
        for (loop_rewrite_formula in 1:nrow(names_p)) {
          if (names_p[loop_rewrite_formula, 2] != paste("p", loop_rewrite_formula, sep = "")) {
            formula_h_all <- str_replace_all(
              formula_h_all,
              fixed(names_p[loop_rewrite_formula, 1]),
              names_p[loop_rewrite_formula, 2]
            )
          }
        }
      }

      for (loop_rewrite_formula in 1:length(name_model)) {
        formula_h_all <- str_replace_all(
          formula_h_all,
          fixed(paste("m", loop_rewrite_formula, sep = "")),
          name_model[loop_rewrite_formula]
        )
      }


      output$h <- renderUI({
        withMathJax(helpText({
          formula_h_all
        }))
      })

      ####* Tables V-representation ####

      show_table <- which(index_convert_to_v != 0)

      if (sum(index_convert_to_v) > 0) {
        for (loop in show_table) {
          all_v_rep_in_list[[loop]][, 1] <- name_model[loop]

          rownames(all_v_rep_in_list[[loop]]) <-
            paste("V_", 1:nrow(all_v_rep_in_list[[loop]]), sep = "")

          if (input$check_name == T) {
            colnames(all_v_rep_in_list[[loop]]) <-
              c("model_name", names_p[, 2])
          }
        }


        output$v_representation_table <- renderUI({
          lapply(as.list(seq_len(length(all_v_rep_in_list))), function(i) {
            id <- paste0("v_representation_table", i)
            DT::dataTableOutput(id)
          })
        })


        for (loop_table in seq_len(length(all_v_rep_in_list))) {
          local({
            id <- paste0("v_representation_table", loop_table)
            pl_t <- all_v_rep_in_list[[loop_table]]
            output[[id]] <- DT::renderDataTable(pl_t)
          })
        }

        ####* Plot ####

        plot_v <- all_v_rep_in_list

        remove_pl <- numeric()

        for (loop_pl in 1:length(plot_v)) {
          remove_pl <- c(remove_pl, ifelse(is.null(unlist(plot_v[loop_pl])) == T, 1, 0))
        }

        plot_v <- plot_v[remove_pl == 0]
        name_model <- name_model[remove_pl == 0]

        for (loop in 1:length(plot_v)) {
          plot_v[[loop]][, 1] <- name_model[loop]
        }

        function_plot(plot_v, length(plot_v))
      }

      ####* Warning ####

      if (alert_mix_v == 1) {
        shinyalert("Info", "V-representations of models for the mixtures were added.",
          type = "info", closeOnClickOutside = T
        )
      }
    }
  })

  #### Parsimony ####

  observeEvent(input$go, {
    output$plot_parsimony <- renderPlotly({
      setClass(
        "model_s4",
        representation(
          A = "matrix", b = "numeric",
          type = "character"
        )
      )

      all_h_rep_in_list <- isolate(h_representation_reactive$value)
      model_name <- isolate(names_models_reactive$value)

      n_rep <- isolate(input$repetitions)
      parsim_rep <- numeric()

      withProgress(message = "Repetition", value = 0, {
        for (loop_repeat_parsimony in 1:n_rep) {
          incProgress(1 / n_rep,
            detail = paste("#", loop_repeat_parsimony, sep = "")
          )


          parsim <- numeric()
          act_dim <- numeric()

          for (loop_parsimony in 1:length(all_h_rep_in_list)) {
            act_pars <- (all_h_rep_in_list[[loop_parsimony]])
            act_pars <- matrix(as.character(act_pars), ncol = ncol(act_pars))
            not_full_dim <- ifelse(sum(q2d(act_pars)[, 1]) > 0, 1, 0)

            if (not_full_dim == 0) {
              left_pars <- q2d((act_pars[, 3:ncol(act_pars)]))
              right_pars <- q2d((act_pars[, 2]))

              model_s4 <- new("model_s4",
                A = left_pars,
                b = right_pars,
                type = "Hpolytope"
              )

              if (isolate(input$CB) == TRUE) {
                act_vol_CB <- volume(model_s4,
                  settings =
                    list("algorithm" = "CB")
                )
              } else {
                act_vol_CB <- NA
              }


              if (isolate(input$SoB) == TRUE) {
                act_vol_SoB <- volume(model_s4,
                  settings =
                    list("algorithm" = "SOB")
                )
              } else {
                act_vol_SoB <- NA
              }

              if (isolate(input$CG) == TRUE) {
                act_vol_CG <- volume(model_s4,
                  settings =
                    list("algorithm" = "CG")
                )
              } else {
                act_vol_CG <- NA
              }


              act_dim <- c(act_dim, "Full-dimensional")
            } else {
              if (isolate(input$CB) == TRUE) {
                act_vol_CB <- 0
              } else {
                act_vol_CB <- NA
              }


              if (isolate(input$SoB) == TRUE) {
                act_vol_SoB <- 0
              } else {
                act_vol_SoB <- NA
              }


              if (isolate(input$CG) == TRUE) {
                act_vol_CG <- 0
              } else {
                act_vol_CG <- NA
              }

              act_dim <- c(act_dim, "Not full-dimensional")
            }

            parsim <- c(parsim, c(act_vol_CB, act_vol_SoB, act_vol_CG))
          }


          parsim <- data.frame(
            "Model" = rep(as.factor(model_name), each = 3),
            "Algorithm" = as.factor(rep(
              c("CB", "SoB", "CG"), length(model_name)
            )),
            "Volume" = parsim,
            "Dimensionality" = rep(act_dim, each = 3)
          )


          parsim <- parsim[complete.cases(parsim) == T, ]

          parsim_rep <- rbind(parsim_rep, parsim)
        }
      })


      parsim <- parsim_rep %>%
        group_by(Model, Algorithm, Dimensionality) %>%
        summarize(Volume = mean(Volume))
      parsim_wide <- spread(parsim, Algorithm, Volume)
      parsim_wide <- cbind(parsim_wide, "Average of algorithms" = signif(rowMeans(parsim_wide[, 3:ncol(parsim_wide)]), 2))
      parsim_wide[, 3:ncol(parsim_wide)] <- signif(parsim_wide[, 3:ncol(parsim_wide)], 2)

      output$parsim_table <- DT::renderDataTable(parsim_wide)

      output$download_volume <- downloadHandler(
        filename = function() {
          paste("volume_",
            str_replace_all(Sys.Date(), "-", "_"),
            ".csv",
            sep = ""
          )
        },
        content = function(file) {
          write.csv(parsim_wide, file)
        }
      )

      fig <-
        plot_ly(
          parsim_wide,
          x = ~Model,
          y = ~`Average of algorithms`,
          type = "bar",
          hoverinfo = "none",
          text = ~`Average of algorithms`
        )

      fig <-
        fig %>% layout(
          yaxis = list(
            title = "Occupied hyperspace (average of algorithms)",
            range = c(0, 1)
          ),
          xaxis = list(title = "Model", tickangle = 45),
          barmode = "group"
        )
    })

    output$parsimony_spinner_table <- renderUI({
      DT::dataTableOutput("parsim_table")
    })

    output$parsimony_spinner <- renderUI({
      withSpinner(
        plotlyOutput("plot_parsimony"),
        type = 8,
        color = "black",
        color.background = "black"
      )
    })
  })

  ##### Download Functions ####

  #####* H-representation ####

  output$d_h <- downloadHandler(
    filename = function() {
      paste("h_representation_",
        str_replace_all(Sys.Date(), "-", "_"),
        ".zip",
        sep = ""
      )
    },
    content = function(file) {
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      files <- NULL


      all_h_rep_in_matrix <- numeric()

      for (loop_qtest_h in 1:length(h_representation_reactive$value)) {
        all_h_rep_in_matrix <- rbind(
          all_h_rep_in_matrix,
          data.frame(isolate(names_models_reactive$value[loop_qtest_h]), isolate(h_representation_reactive$value[[loop_qtest_h]]))
        )
      }

      col_names_h <- c("model name", "equ/ineq", "right", paste("p", 1:(ncol(all_h_rep_in_matrix) - 3), sep = ""))

      colnames(all_h_rep_in_matrix) <- col_names_h


      unique_v <- unique(all_h_rep_in_matrix$`model name`)
      n_v <- length(unique_v)

      for (loop_v in unique_v) {
        act_v <- all_h_rep_in_matrix[all_h_rep_in_matrix$`model name` == loop_v, ]
        act_v_ineq <- act_v[act_v$`equ/ineq` == 0, ]

        header_v <- dim(act_v_ineq) - c(0, 3)

        act_v_left <- act_v_ineq[, 4:ncol(act_v_ineq)]
        act_v_left <- -matrix(q2d(unlist(act_v_left)), ncol = ncol(act_v_left))

        act_v_right <- q2d(unlist(act_v_ineq[, 3]))
        act_v_right <- data.frame(act_v_right)

        act_v_eq <- act_v[act_v$`equ/ineq` == 1, ]


        total_file <- paste(
          paste(as.character(header_v), collapse = " "),
          "\n",
          "\n",
          paste(apply((act_v_left), 1, paste, collapse = " "), collapse = "\n"),
          "\n",
          "\n",
          paste(apply((act_v_right), 1, paste, collapse = " "), collapse = "\n"),
          sep = ""
        )

        file_name_addendum <- ""

        if (dim(act_v_eq)[1] > 0) {
          act_v_eq_left <- act_v_eq[, 4:ncol(act_v_eq)]
          act_v_eq_left <- -matrix(q2d(unlist(act_v_eq_left)), ncol = ncol(act_v_eq_left))

          act_v_eq_right <- (act_v_eq[, 3])
          act_v_eq_right <- q2d(unlist(act_v_eq_right))
          act_v_eq_right <- data.frame(act_v_eq_right)

          header_eq_v <- dim(act_v_eq_left)

          total_file <-
            paste(
              total_file,
              "\n",
              "\n",
              "Equalities",
              "\n",
              "\n",
              paste(as.character(header_eq_v), collapse = " "),
              "\n",
              "\n",
              paste(apply(((act_v_eq_left)
              ), 1, paste, collapse = " "), collapse = "\n"),
              "\n",
              "\n",
              paste(apply((act_v_eq_right), 1, paste, collapse = " "), collapse = "\n"),
              "\n",
              "\n",
              "Needs to be edited here; if you do not know how to edit the file, download the V-representation of the model and let QTEST create the H-representation",
              sep = ""
            )

          file_name_addendum <- "_needs_editing_for_qtest"
        }

        fileName <-
          paste(loop_v, file_name_addendum, ".txt", sep = "")

        write.table(
          total_file,
          fileName,
          quote = FALSE,
          col.names = FALSE,
          row.names = FALSE
        )


        files <- c(fileName, files)
      }

      zip::zip(file, files)
    }
  )


  #####* V-representation ####

  output$d_v <- downloadHandler(
    filename = function() {
      paste("v_representation_",
        str_replace_all(Sys.Date(), "-", "_"),
        ".zip",
        sep = ""
      )
    },
    content = function(file) {
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      files <- NULL

      names_models <- names_models_reactive$value
      v_representation_reactive$value

      for (loop_v_qtest in 1:length(v_representation_reactive$value)) {
        csv_file <- v_representation_reactive$value[[loop_v_qtest]]

        if (is.null(csv_file) == F) {
          csv_file <- csv_file[, 2:ncol(csv_file)]

          csv_file <- t(csv_file)

          header_v <- paste("V", 1:ncol(csv_file), sep = "")
          line_2 <- rep(1, ncol(csv_file))

          total_file <- paste(
            paste((header_v), collapse = ","),
            "\n",
            paste((line_2), collapse = ","),
            "\n",
            "\n",
            paste(apply((csv_file), 1, paste, collapse = ","), collapse = "\n"),
            sep = ""
          )

          fileName <-
            paste(names_models[loop_v_qtest], ".csv", sep = "")

          write.table(
            total_file,
            fileName,
            quote = FALSE,
            col.names = FALSE,
            row.names = FALSE
          )

          files <- c(fileName, files)
        }
      }

      zip::zip(file, files)
    }
  )

  ####* LaTeX-file of H-representation ####

  output$d_latex <- downloadHandler(
    filename = function() {
      paste("models_",
        str_replace_all(Sys.Date(), "-", "_"),
        ".tex",
        sep = ""
      )
    },
    content = function(file) {
      header_latex <- (
        "\\documentclass{article}
\\usepackage{amsmath}
\\begin{document}
\\section*{Models}
"
      )

      footer_latex <- ("
\\end{document}")

      formula_h_all <- unlist(formula_h_all_reactive$value)
      formula_h_all <- paste(formula_h_all, collapse = "")

      formula_h_all_download <- str_replace_all(formula_h_all, "eqnarray", "align*")
      formula_h_all_download <- str_replace_all(formula_h_all_download, "\\$", "")

      formula_h_all_download <- paste(header_latex,
        formula_h_all_download,
        footer_latex,
        sep = ""
      )

      write.table(
        formula_h_all_download,
        file,
        quote = FALSE,
        col.names = FALSE,
        row.names = FALSE
      )
    }
  )
})

shinyApp(ui, server)
