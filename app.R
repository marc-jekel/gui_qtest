library("shiny")
library("shinyvalidate")
library("rcdd")
library("DT")
library("MASS")
library("stringr")
library("plotly")
library("dplyr")

## LOG

## Check whether fractions can be displayesd better for mixture tab

##

ui <- shinyUI(fluidPage(
  
  navbarPage(
    position = "fixed-top",
    inverse = T,
    windowTitle = "Model representations",
    tags$style(type = 'text/css', '.navbar { background-color: #000000;
                                               font-family: Arial;
                                               font-size: 13px;
                                               color: #FF0000; }',
               
               '.navbar-dropdown { background-color: #262626;
                                                    font-family: Arial;
                                                    font-size: 13px;
                                                    color: #FF0000; }',
               
               'body {padding-top: 60px;padding-bottom: 80px;}'),
    
    tabPanel("Input",
             
             sidebarPanel(
               style = "position:fixed;width:13%",
               width=2,
               fluidRow(
                 column(12,offset=0,
                        helpText("Download input"))
               ),
               fluidRow(
                 
                 column(1,offset=0,downloadButton("download", "",
                                                  icon=icon("fa-thin fa-cloud-arrow-down")))
                 
               ),
               
               fluidRow(
                 column(12,offset=0,
                        helpText("Probabilities")
                 ),
                 column(12,offset=0,
                        actionButton("rm_btn", "",icon=icon("fa-regular fa-square-minus")),
                        actionButton("add_btn", "",icon=icon("fa-regular fa-square-plus"))
                        
                        
                 ),
                 
                 
               ),
               fluidRow(
                 column(12,offset=0,
                        helpText("Models")
                 ),
                 column(12,offset=0,
                        actionButton("rm_ie", "",icon=icon("fa-regular fa-square-minus")),
                        actionButton("add_ie", "",icon=icon("fa-regular fa-square-plus"))
                        
                        
                 )
               ),
               fluidRow(
                 column(12,offset=0,
                        helpText("Threshold for approximate equalities")
                 ),
                 column(12,offset=0,
                        
                        numericInput(min=0,max=1,step=.01,inputId = "approx_equal",
                                     label = NULL, 
                                     value = 0)
                 )
               )
               
             ),
             mainPanel(
               
               fluidRow(
                 column(4,offset = 0,checkboxInput("check_name", "use this", value = FALSE),uiOutput("textbox_ui_name"), style = "background-color:#E8E8E8;"),
                 column(4,checkboxInput("check_min", "use this", value = TRUE),uiOutput("textbox_ui_min"), style = "background-color:#F8F8F8;"),
                 column(4,checkboxInput("check_max", "use this", value = TRUE),uiOutput("textbox_ui_max"), style = "background-color:#E8E8E8;")
               ),
               fluidRow(
                 column(12, offset = 0,checkboxInput("check_relations", "use this", value = TRUE),  style = "background-color:#FFFFF2;"),
                 
               ),
               fluidRow(
                 column(4, offset = 0,uiOutput("textbox_ui_name_rel"),  style = "background-color:#FFFFF2;"),
                 column(8,uiOutput("textbox_ui_rel"),  style = "background-color:#FFFFF2;"),
               ),
               fluidRow(
                 column(12,offset = 0,checkboxInput("check_mix_inter", "use this", value = FALSE),
                        textAreaInput("text_mix_inter", "Input field for the intersection and/or mixture of models", value = "",
                                      width='100%',height='100px'),style = "background-color:#F8F8F8;")
               )
             )
             
    ),
    
    
    tabPanel("H-representation",
             
             sidebarPanel(
               style = "position:fixed;width:13%",
               width=2,
               fluidRow(
                 column(12,offset=0,
                        helpText("Download for QTEST"))
               )),
             mainPanel(
               fluidPage(
                 withMathJax(),
                 uiOutput('h')
               )
               
             )
             
    ),
    tabPanel("V-representation",
             
             sidebarPanel(
               style = "position:fixed;width:13%",
               width=2,
               fluidRow(
                 column(12,offset=0,
                        helpText("Download for QTEST"))
               )),
             mainPanel(
               
               fluidRow(
                 column(12,    DT::dataTableOutput("all_v_plot"))
               )
               
             )
             
             
    ), #Bracket  navbarPage
    tabPanel("Plot",
             sidebarPanel(
               style = "position:fixed;width:15%",
               width=2,
               fluidRow(
                 column(12,offset=0,
                        helpText("Select probabilities"))),
               fluidRow(
                 column(12,offset=0,
                        selectInput("dim_1", "p #",
                                    list(1,2,3),
                                    selected = 1
                        )
                 )
               ),
               fluidRow(
                 column(12,offset=0,
                        selectInput("dim_2", "p #",
                                    list(1,2,3),
                                    selected = 2
                        )
                 )
               ),
               fluidRow(
                 column(12,offset=0,
                        selectInput("dim_3", "p #",
                                    list(1,2,3),
                                    selected = 3
                        )
                 )
               ),
               fluidRow(
                 
                 column(12,offset=0,
                        textAreaInput(inputId = "name_model_plot",
                                      label = "Input field for model names", 
                                      value = "",
                                      width="100%",height="100px"
                        )
                 )
               ),
               fluidRow(
                 column(12,offset=0,
                        helpText("Remove/add all models")),
                 column(12,offset=0,
                        actionButton("subtr_models", "",icon=icon("fa-regular fa-square-minus")),
                        actionButton("add_models", "",icon=icon("fa-regular fa-square-plus")))
                 
               )
               
               
             ),
             
             mainPanel(
               fluidRow(
                 column(12,div(plotlyOutput("plot"), align = "center"))
               ))
    ),
    tabPanel("Parsimony",
             mainPanel(
               fluidRow(
                 column(12,div(plotlyOutput("plot_parsimony"), align = "center"))
               ))
    ),
    tags$footer(column(3, "Pre-publication version"), 
                column(2,offset=7, tags$a(href="mailto:mjekel@uni-koeln.de", tags$b("Email feedback"), 
                                          class="externallink", style = "color: white",
                                          icon = icon("fa-duotone fa-envelope"))),
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
  
  # Track the number of input boxes to render
  counter <- reactiveValues(n = 3)
  counter_ie <- reactiveValues(n=1)
  # Track all user inputs
  AllInputs <- reactive({
    x <- reactiveValuesToList(input)
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste0("user_input", ".csv")
    },
    content = function(file) {
      
      numb_rows_outs = ifelse(counter$n > counter_ie$n,counter$n,counter_ie$n)
      
      if(input$check_relations == T){
        
        outs = matrix("",ncol=7,nrow=numb_rows_outs)
        
      }else{
        
        outs = matrix("",ncol=5,nrow=counter$n)
        
      }
      
      outs = data.frame(outs)
      
      for(loop in 1 : counter$n){
        
        outs[loop,1:5] = c(paste("p_",loop,sep=""),
                           eval(parse(text=paste("unlist(AllInputs()$textin_name_",loop,")",sep=""))),
                           eval(parse(text=paste("unlist(AllInputs()$textin_min_",loop,")",sep=""))),
                           eval(parse(text=paste("unlist(AllInputs()$textin_max_",loop,")",sep=""))),
                           input$approx_equal
        )
        
      }
      
      if(input$check_relations == T){
        
        for(loop in 1 : counter_ie$n){
          
          outs[loop,6:7] = c(
            eval(parse(text=paste("unlist(AllInputs()$textin_relations_name",loop,")",sep=""))),
            eval(parse(text=paste("unlist(AllInputs()$textin_relations_",loop,")",sep=""))))
          
        }  
        
        colnames(outs) = c("variable","p_name","p_min","p_max","approx_equal","model_name","in/equalities")
        
      }else{
        
        colnames(outs) = c("variable","p_name","p_min","p_max","approx_equal")
        
        
      }
      
      write.csv(outs, file)
    }
  )
  
  #### button events ####
  
  observeEvent(input$add_btn, {counter$n <- counter$n + 1})
  observeEvent(input$add_ie, {counter_ie$n <- counter_ie$n + 1})
  
  observeEvent(input$rm_btn, {
    if (counter$n > 2) counter$n <- counter$n - 1
  })
  
  observeEvent(input$rm_ie, {
    if (counter_ie$n > 1) counter_ie$n <- counter_ie$n - 1
  })
  
  observeEvent(input$add_models, {
    
    models_to_plot = input$name_model_plot
    models_to_plot = unlist(str_split(models_to_plot,";"))
    models_to_plot = str_replace_all(models_to_plot," ","")
    models_to_plot = unlist(models_to_plot)
    
    models_to_plot = unique(c(models_to_plot,names_available_models))
    models_to_plot = models_to_plot[models_to_plot!=""]
    
    updateTextAreaInput(inputId = "name_model_plot",  
                        value = paste(models_to_plot,collapse=";"))
    
  })
  
  observeEvent(input$subtr_models, {
    
    models_to_plot = input$name_model_plot
    models_to_plot = unlist(str_split(models_to_plot,";"))
    models_to_plot = str_replace_all(models_to_plot," ","")
    models_to_plot = unlist(models_to_plot)
    
    models_to_plot = ""
    
    updateTextAreaInput(inputId = "name_model_plot",  
                        value = paste(models_to_plot,collapse=";"))
    
  })
  
  #### checkbox events ####
  
  textboxes_min <- reactive({
    
    n <- counter$n
    
    if (n > 0) {
      
      isolate({
        lapply(seq_len(n), function(i) {
          
          
          numericInput(min=0,max=1,step=.01,inputId = paste0("textin_min_", i),
                       label = paste0("Minimum of p", i), 
                       value = ifelse(is.null(AllInputs()[[paste0("textin_min_", i)]]) == TRUE,
                                      0,AllInputs()[[paste0("textin_min_", i)]])
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
          
          checkboxInput(inputId = paste0("check_row_", i),label= "use this", value = FALSE)
          
        })
      })
    }
    
  })  
  
  textboxes_max <- reactive({
    
    n <- counter$n
    
    if (n > 0) {
      isolate({
        lapply(seq_len(n), function(i) {
          numericInput(min=0,max=1,step=.01,inputId = paste0("textin_max_", i),
                       label = paste0("Maximum of p", i), 
                       value = ifelse(is.null(AllInputs()[[paste0("textin_max_", i)]]) == TRUE,
                                      1,AllInputs()[[paste0("textin_max_", i)]])
                       
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
          textInput(inputId = paste0("textin_name_", i),
                    label = paste0("Name of p", i), 
                    value = ifelse(is.null(AllInputs()[[paste0("textin_name_", i)]]) == TRUE,
                                   paste0("p",i),AllInputs()[[paste0("textin_name_", i)]])
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
          textAreaInput(inputId = paste0("textin_relations_", i),
                        label = paste0("Input field for equalities and inequalities"), 
                        value = AllInputs()[[paste0("textin_relations_", i)]],
                        width="100%",height="100px"
          )
        })
      })
    }
    
    
  })
  
  textboxes_relations_name <- reactive({
    
    n <- counter_ie$n
    
    if (n > 0) {
      isolate({
        lapply(seq_len(n), function(i) {
          textAreaInput(inputId = paste0("textin_relations_name", i),
                        label = paste0("Name of model"), 
                        value = ifelse(is.null(AllInputs()[[paste0("textin_relations_name", i)]]) == TRUE,
                                       paste0("m",i),AllInputs()[[paste0("textin_relations_name", i)]]),
                        width="100%", height = "100px"
          )
        })
      })
    }
  })
  
  
  output$textbox_ui_min <- renderUI({ textboxes_min() })
  output$textbox_ui_name <- renderUI({ textboxes_names() })
  output$textbox_ui_max <- renderUI({ textboxes_max() })
  output$textbox_ui_rel <- renderUI({ textboxes_relations() }) 
  output$checkbox_ui_row <-renderUI({ check_box_row() }) 
  output$textbox_ui_name_rel <- renderUI({ textboxes_relations_name() }) 
  
  ####### Validation
  
  observe({
    
    n = counter$n
    iv <- InputValidator$new()
    
    for(loop_val in 1 : n){
      
      iv$add_rule(paste0("textin_max_", loop_val), sv_between(0, 1))
    }
    
    for(loop_val in 1 : n){
      
      comp_max = AllInputs()[[paste0("textin_max_", loop_val)]]
      comp_min = AllInputs()[[paste0("textin_min_", loop_val)]]
      
      iv$add_rule(paste0("textin_min_", loop_val), sv_between(0, 1))
      
      if(is.null(comp_max)==F){
        if(comp_max >= 0 & comp_max <= 1 & is.na(comp_max) ==F){
          iv$add_rule(paste0("textin_min_", loop_val), sv_lte(comp_max))
        }
        
      }
      
      if(is.null(comp_min) == F){
        if(comp_min >= 0 & comp_min <= 1 & is.na(comp_min) ==F){
          iv$add_rule(paste0("textin_max_", loop_val), sv_gte(comp_min))
        }
      }
    }
    iv$enable()
    
  })
  
  #### LaTeX
  
  latex = function(latex_object){
    
    formula_h = ""
    
    latex_object[,3:ncol(latex_object)] = (-1 * latex_object[,3:ncol(latex_object)])
    h_representation_pl_minus = latex_object
    
    sign_p = ifelse(data.frame(h_representation_pl_minus[,3:ncol(h_representation_pl_minus)]) < 0,"-","+")
    sign_p = ifelse(data.frame(h_representation_pl_minus[,3:ncol(h_representation_pl_minus)] ) == 0,"",sign_p)
    
    h_representation_pl_minus[,3:ncol(h_representation_pl_minus)] = abs(h_representation_pl_minus[,3:ncol(h_representation_pl_minus)])
    
    for(loop_pl in 1 : nrow(latex_object)){
      
      formula_h_act = ((as.character(h_representation_pl_minus[loop_pl,])))
      
      sign_p_act = sign_p[loop_pl,]
      
      sign_pl = ifelse(formula_h_act[1] == "1", "=","\\leq")
      
      
      col_names_p = colnames(latex_object)
      col_names_p = ifelse(formula_h_act[3:(length(formula_h_act))]!="0",col_names_p[3:length(col_names_p)],"")
      
      p_with_factors = paste(formula_h_act[3:(length(formula_h_act))]," \\times  ", col_names_p, sep ="")
      
      p_with_factors = paste(sign_p_act," & ",p_with_factors,sep="")
      
      
      p_with_factors = ifelse(col_names_p == ""," & ",p_with_factors)
      
      
      p_with_factors = paste(p_with_factors," & ", collapse = "")
      
      p_with_factors = paste(p_with_factors,sign_pl," & ",formula_h_act[2], collapse="")
      
      p_with_factors = paste(paste(p_with_factors,collapse=" "), " \\\\", collapse=" ")
      
      
      formula_h = paste(c(formula_h,p_with_factors),collapse=" ")
      
    }
    
    formula_h
    
  }
  
  #### plot
  
  function_plot = function(v_representation_plot_all_list,n_submodels){
    
    output$plot = renderPlotly({
      
      models_to_plot = input$name_model_plot
      models_to_plot = unlist(str_split(models_to_plot,";"))
      models_to_plot = str_replace_all(models_to_plot," ","")
      models_to_plot = unlist(models_to_plot)
      
      names_available_models = rep(NA,n_submodels)
      
      matrix_pl_all = numeric()
      
      for(loop_pl in 1 : (n_submodels)){
        
        extract_name = v_representation_plot_all_list[[loop_pl]]
        names_available_models[loop_pl] =  extract_name[1,1]
        
        all_plot_actual = v_representation_plot_all_list[[loop_pl]]
        
        all_plot_actual = all_plot_actual[,2:ncol(all_plot_actual)]
        colnames_v_representation_plot_all = colnames(all_plot_actual)
        
        all_plot_actual = matrix(as.character(d2q(unlist((all_plot_actual)))),ncol=ncol(all_plot_actual))
        
        matrix_pl_all = rbind(matrix_pl_all,data.frame(names_available_models[loop_pl] ,(all_plot_actual)))
      }
      
      colnames(matrix_pl_all) = c("models",colnames_v_representation_plot_all)
      
      names_available_models <<- names_available_models
      
      
      select_models_to_plot = names_available_models[names_available_models %in% models_to_plot]
      
      matrix_pl_all = matrix_pl_all[matrix_pl_all[,1] %in% select_models_to_plot, ]
      
      if(length(select_models_to_plot) > 0){
        
        for(loop_pl in 1 : length(select_models_to_plot)){
          
          all_plot_actual = matrix_pl_all[matrix_pl_all[,1]==select_models_to_plot[loop_pl] ,2:ncol(matrix_pl_all)]
          
          mixture_v_plot_names = colnames(all_plot_actual)
          
          name_model = select_models_to_plot[loop_pl]
          all_plot_actual = matrix(as.character((unlist(all_plot_actual))),ncol = ncol(all_plot_actual))
          matrix_pl = q2d(all_plot_actual)
          
          select_plot = as.numeric(c(input$dim_1,input$dim_2,input$dim_3))
          
          updateSelectInput(inputId="dim_1",choices=(1:ncol(matrix_pl))[-select_plot[c(2,3)]],
                            selected = input$dim_1)
          updateSelectInput(inputId="dim_2",choices=(1:ncol(matrix_pl))[-select_plot[c(1,3)]],
                            selected = input$dim_2)
          updateSelectInput(inputId="dim_3",choices=(1:ncol(matrix_pl))[-select_plot[c(1,2)]],
                            selected = input$dim_3)
          
          matrix_pl = matrix_pl[,select_plot]
          
          if(is.null(dim(matrix_pl)) != TRUE){
            
            matrix_pl = data.frame(matrix(as.numeric(c(matrix_pl)),ncol=ncol(matrix_pl)))
            
          }else{
            
            matrix_pl = t(data.frame(matrix(as.numeric(c(matrix_pl)))))
            
            
          }
          
          
          dim_names = mixture_v_plot_names[c(as.numeric(input$dim_1),
                                             as.numeric(input$dim_2),
                                             as.numeric(input$dim_3))]
          
          colnames(matrix_pl) = dim_names
          
          
          axx <- list(
            nticks = .1,
            range = c(0,1),
            title = dim_names[1]
            
          )
          
          axy <- list(
            nticks = .1,
            range = c(0,1),
            title = dim_names[2]
          )
          
          axz <- list(
            nticks = .1,
            range = c(0,1),
            title = dim_names[3]
          )
          
          ###
          
          trace1 <- list(
            mode = "markers", 
            type = "scatter3d", 
            x = (matrix_pl[,1]),
            y = (matrix_pl[,2]),
            z = (matrix_pl[,3])
            
          )
          
          trace2 <- list(
            type = "mesh3d", 
            x = (matrix_pl[,1]),
            y = (matrix_pl[,2]),
            z = (matrix_pl[,3]),
            opacity = 0.05, 
            alphahull = 0,
            colors = colorRamp(c("blue", "lightblue", "chartreuse3", "yellow", "red"))
          ) 
          
          ### check dimensionality using principal component analysis
          
          prcomp_sol = summary(prcomp(matrix_pl))$importance[2,]
          shape_point = ifelse(sum(prcomp_sol!=0) == 1,1,0)
          shape_point = ifelse(nrow(matrix_pl) == 1,1,shape_point)
          shape_cube =  ifelse(sum(prcomp_sol!=0) == 3,1,0)
          shape_cube = ifelse(shape_point == 1,0,shape_cube)
          
          if(shape_cube==1){
            
            trace2 <- list(
              type = "mesh3d", 
              x = (matrix_pl[,1]),
              y = (matrix_pl[,2]),
              z = (matrix_pl[,3]),
              opacity = 0.05, 
              alphahull = 0,
              colors = colorRamp(c("blue", "lightblue", "chartreuse3", "yellow", "red"))
            ) 
          }
          
          if(loop_pl == 1){
            
            p <- plot_ly(width = 750, height = 750, name = name_model)
            
          }
          
          if(shape_point != 1){  
            
            p <- add_trace(p, mode=trace1$mode, type=trace1$type, x=trace1$x, y=trace1$y, z=trace1$z,
                           name = name_model)
            
            if(shape_cube == 1){
              
              p <- add_trace(p, type=trace2$type, x=trace2$x, y=trace2$y, z=trace2$z, 
                             opacity=trace2$opacity, alphahull=trace2$alphahull,
                             delaunayaxis=trace2$delaunayaxis, name = name_model)
              
            }else{
              
              p <- add_trace(p, type=trace2$type, x=trace2$x, y=trace2$y, z=trace2$z, 
                             opacity=.05, alphahull=-1,
                             delaunayaxis="x", name = name_model)
              p <- add_trace(p, type=trace2$type, x=trace2$x, y=trace2$y, z=trace2$z, 
                             opacity=.05, alphahull=-1,
                             delaunayaxis="y", name = name_model)
              p <- add_trace(p, type=trace2$type, x=trace2$x, y=trace2$y, z=trace2$z, 
                             opacity=.05, alphahull=-1,
                             delaunayaxis="z", name = name_model)
              
            }
            
          }else{
            
            p <- add_trace(p,x=trace1$x, y=trace1$y, z=trace1$z, type = 'scatter3d', mode = 'lines+markers',
                           opacity = 1, line = list(width = 4), name = name_model)
            
          }
          
        }
        
        p %>% layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz,aspectmode = "manual", 
                                  aspectratio = list(x=1, y=1, z=1),
                                  camera = list(eye = list(x=2, y=2, z = 2)))) 
        
        
      }
    })  
    
    
    
  }
  
  ### extract in/equalities from input
  
  extract_info = function(test=input_relations$rel1){
    
    outs = data.frame()
    
    for(loop in 1 : counter$n){
      
      outs = rbind(outs,
                   
                   c(paste("p_",loop,sep=""),
                     eval(parse(text=paste("unlist(AllInputs()$textin_name_",loop,")",sep=""))),
                     eval(parse(text=paste("unlist(AllInputs()$textin_min_",loop,")",sep=""))),
                     eval(parse(text=paste("unlist(AllInputs()$textin_max_",loop,")",sep="")))
                   )
      )
    }
    
    ####
    
    colnames(outs) = c("variable","name","p_min","p_max")
    min_values = as.numeric(outs$p_min)
    max_values = as.numeric(outs$p_max)   
    
    ####* RCDD #####
    
    if(input$check_relations == TRUE){
      
      ####* split and remove ####
      
      test = unlist(str_split(test,";"))
      test = str_replace_all(test," ","")
      test = unlist(test)
      
      ####* extract structure ####
      
      loc_above = str_locate_all(test,">")
      loc_equal = str_locate_all(test,"=")
      loc_below = str_locate_all(test,"<")
      
      loc_add = str_locate_all(test,fixed("+"))
      loc_subtract = str_locate_all(test,"-")
      loc_divide = str_locate_all(test,"/")
      loc_multiply = str_locate_all(test,fixed("*"))
      
      loc_digits = str_locate_all(test,"[:digit:]")
      loc_prob = str_locate_all(test,fixed("p"))
      
      loc_paren_open = str_locate_all(test,fixed("("))
      loc_paren_closed = str_locate_all(test,fixed(")"))
      loc_point = str_locate_all(test,fixed("."))
      
      #** loop here
      
      loop_all = 1 
      all_extracted = list()
      
      for(loop_bit in 1 : length(test)){
        
        test_actual = test[loop_bit]
        test_actual_single = unlist(str_split(test_actual,""))
        
        pos = rbind("above"=data.frame(loc_above[loop_bit]),
                    "below" =  data.frame(loc_below[loop_bit]),
                    "equal"  = data.frame(loc_equal[loop_bit]),
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
        pos = pos[order(pos[,1]),]
        
        length_str = str_length(test_actual)
        
        pos = data.frame(t(pos))
        
        info_type = colnames(pos)
        
        info_type = str_replace_all(info_type,fixed("."),"")
        info_type = str_replace_all(info_type,"[:digit:]","")
        
        #####* extract sub inequalities and equalities
        
        ineq_equ_parts_loc = which(info_type == "above" | 
                                     info_type == "below" |
                                     info_type == "equal")
        
        ineq_equ_parts_loc = c(1,ineq_equ_parts_loc,length(info_type))
        
        loc_subparts = numeric()
        
        for(loop_loc in 1 : (length(ineq_equ_parts_loc)-2)){
          
          
          loc_act = ineq_equ_parts_loc[c(1,3)+(loop_loc-1)]
          
          if(loop_loc == 1){
            
            loc_act[2] = loc_act[2] - 1
          }
          
          if(loop_loc < (length(ineq_equ_parts_loc)-2) & loop_loc != 1){
            
            loc_act[1] = loc_act[1] + 1
            loc_act[2] = loc_act[2] - 1
          }
          
          if(loop_loc == (length(ineq_equ_parts_loc)-2)){
            
            loc_act[1] = loc_act[1] + 1
            loc_act[2] = loc_act[2] 
          }
          
          loc_subparts = c(loc_subparts,loc_act)
        }
        
        loc_subparts = t(matrix(loc_subparts,nrow=2))
        
        subparts = list()
        
        if(nrow(loc_subparts) == 1){
          
          subparts[[paste0("part", loop_loc)]]=
            rbind(info_type[1:length(info_type)],
                  test_actual_single[1:length(info_type)])
          
          
        }else{
          
          for(loop_loc in 1 : nrow(loc_subparts)){
            
            subparts[[paste0("part", loop_loc)]]=
              rbind(info_type[loc_subparts[loop_loc,1]:loc_subparts[loop_loc,2]],
                    test_actual_single[loc_subparts[loop_loc,1]:loc_subparts[loop_loc,2]])
            
          }
          
        }
        
        ###* loop over subparts 
        
        for(loop_sub_part in 1 : length(subparts)){
          
          subparts_actual = data.frame(subparts[loop_sub_part])
          
          
          pos_p = which(subparts_actual[1,] == "p")
          pos_before_p = pos_p-1
          
          if(pos_before_p[1] == 0){
            
            subparts_actual = cbind(cbind(c("add","+"),c("digit","1"),c("multiply","*")),subparts_actual)
            
          }
          
          if(subparts_actual[1,1] == "digit"){
            
            subparts_actual = cbind(cbind(c("add","+")),subparts_actual)
            
          }
          
          ####
          
          loc_after_relation = which(subparts_actual[1,]=="below" | 
                                       subparts_actual[1,]=="above" | 
                                       subparts_actual[1,]=="equal") + 1
          
          if(subparts_actual[1,loc_after_relation] == "digit" | subparts_actual[1,loc_after_relation] == "point"){
            
            subparts_actual = cbind(subparts_actual[,1:(loc_after_relation-1)],
                                    c("add","+"),
                                    subparts_actual[,loc_after_relation:ncol(subparts_actual)])
            
          }
          
          
          add_multiply_function = function(){
            
            pos_p = which(subparts_actual[1,] == "p")
            pos_before_p = pos_p-1
            
            add_multiply = ifelse(subparts_actual[1,pos_before_p]=="add" | 
                                    subparts_actual[1,pos_before_p] =="below"| 
                                    subparts_actual[1,pos_before_p] =="above" |
                                    subparts_actual[1,pos_before_p] =="equal",1,0)
            
            add_multiply = ifelse( subparts_actual[1,pos_before_p] =="below"| 
                                     subparts_actual[1,pos_before_p] =="above" |
                                     subparts_actual[1,pos_before_p] =="equal",2,
                                   add_multiply)
            
            add_multiply = ifelse(subparts_actual[1,pos_before_p]=="subtract",-1,
                                  add_multiply)
            
            
            fix_factors = pos_before_p[add_multiply != 0]   
            add_multiply = add_multiply[add_multiply != 0]
            
            return(list(fix_factors,add_multiply))
          }
          
          res_cadd_multiply_function = add_multiply_function()
          
          fix_factors = unlist(res_cadd_multiply_function[1])
          add_multiply = unlist(res_cadd_multiply_function[2])
          
          if(length(add_multiply) > 0){
            
            for(loop_add_multiply in 1: length(add_multiply)){
              
              if(add_multiply[1] == -1){
                
                subparts_actual =
                  cbind(subparts_actual[,1:fix_factors[1]],
                        cbind(c("digit","1"),c("multiply","*")),
                        subparts_actual[,(fix_factors[1]+1):ncol(subparts_actual)])
                
              }
              
              if(add_multiply[1] == 1){
                
                subparts_actual =
                  cbind(subparts_actual[,1:fix_factors[1]],
                        cbind(c("digit","1"),c("multiply","*")),
                        subparts_actual[,(fix_factors[1]+1):ncol(subparts_actual)])
                
              }
              
              if(add_multiply[1] == 2){
                
                subparts_actual =
                  cbind(subparts_actual[,1:fix_factors[1]],
                        cbind(c("add","+"),c("digit","1"),c("multiply","*")),
                        subparts_actual[,(fix_factors[1]+1):ncol(subparts_actual)])
                
              }
              
              res_cadd_multiply_function = add_multiply_function()
              fix_factors = unlist(res_cadd_multiply_function[1])
              add_multiply = unlist(res_cadd_multiply_function[2])
              
            }
            
          }
          
          #####* extract p ####
          
          keep = numeric()
          
          for(loop_extract in 1 : ncol(subparts_actual)){
            
            if(subparts_actual[1,loop_extract] == "p"){
              
              keep = c(keep,1)
              
            }
            
            if(loop_extract > 1 & subparts_actual[1,loop_extract] != "p"){
              
              if(subparts_actual[1,loop_extract] != "p" & 
                 subparts_actual[1,loop_extract] == "digit" &
                 keep[loop_extract-1] == 1 ){
                
                keep = c(keep,1)
                
              }else{
                
                keep = c(keep,0)
                
              }
            }
            
            if(loop_extract == 1 & subparts_actual[1,loop_extract] != "p"){
              
              keep = c(keep,0)
              
            }
          }
          
          extract_p = subparts_actual[2,keep==1]
          extract_p = paste0(extract_p,collapse="")
          extract_p = unlist(str_split(extract_p,"p"))
          extract_p = extract_p[2:length(extract_p)]
          extract_p = as.numeric(extract_p)
          
          ####* extract relation ####
          
          location_relation = which(subparts_actual[1,] == "below" | 
                                      subparts_actual[1,] == "above" | 
                                      subparts_actual[1,] == "equal")
          
          relation_subpart = subparts_actual[1,location_relation]
          relation_subpart = unlist(relation_subpart)
          
          ####* extract factors ####
          
          left_side = data.frame(subparts_actual[,1:(location_relation-1)])
          right_side =  data.frame(subparts_actual[,(location_relation+1):ncol(subparts_actual)] )
          
          
          location_add_subtract_left = which(left_side[1,] == "subtract" | 
                                               left_side[1,] == "add")
          
          location_p_left =  which(left_side[1,] == "p")
          
          
          location_add_subtract_right = which(right_side[1,] == "subtract" | 
                                                right_side[1,] == "add")
          
          location_p_right =  which(right_side[1,] == "p")
          
          
          ####** left side 
          
          factor_left = numeric()
          
          if(length(location_p_left)==0){
            
            factor_left = paste0(left_side[2,],collapse="")
            extract_p = c("numb",extract_p)
            
          }else{
            
            
            for(loop_loc in 1 : length(location_add_subtract_left)){
              
              factor_left = c(factor_left,
                              paste0(left_side[2,location_add_subtract_left[loop_loc]:
                                                 (location_p_left[loop_loc]-2)],collapse=""))
              
            }
            
          }
          
          ####** right side
          
          factor_right = numeric()
          
          if(length(location_p_right)==0){
            
            factor_right = paste0(right_side[2,],collapse="")
            
            extract_p = c(extract_p,"numb")
            
          }else{
            
            for(loop_loc in 1 : length(location_add_subtract_right)){
              
              factor_right= c(factor_right,
                              paste0(right_side[2,location_add_subtract_right[loop_loc]:
                                                  (location_p_right[loop_loc]-2)],collapse=""))
              
              
            }
            
          }
          
          ####** combine extract 
          
          extracted = rbind(extract_p,
                            c(rep("left",length(factor_left)),
                              rep("right",length(factor_right))),
                            c(factor_left,factor_right),
                            relation_subpart)
          
          row.names(extracted) = c("p","left/right","factor","relation")
          colnames(extracted) = paste("sub_",1:ncol(extracted),sep="")
          extracted = data.frame(extracted)
          
          
          all_extracted[[paste0("line", loop_all)]] = extracted
          
          loop_all = loop_all + 1
          
        }
      }
    }
    
    
    if(input$check_relations == FALSE){
      
      
      all_extracted = list()
      numb_p = 0
      
    }
    
    
    if(length(all_extracted) > 0){
      
      numb_p = numeric()
      
      for(loop_numb_p in 1 : length(all_extracted)){
        
        numb_p = c(numb_p,(unlist(data.frame(all_extracted[loop_numb_p])[1,])))
        
      }
      
      numb_p = numb_p[numb_p!="numb"]
      numb_p = as.numeric(numb_p)
      numb_p = max(numb_p)
      
    }else{
      
      
      all_extracted = list()
      numb_p = 0
      
    }
    
    
    ###
    
    if(input$check_min == TRUE){
      
      numb_p_min = length(min_values)
      numb_p = ifelse(numb_p > numb_p_min,numb_p,numb_p_min)
      
      extend_all_extracted_numb = length(all_extracted) + 1
      
      for(loop_min_max in 1 : numb_p_min){
        
        actual_min = min_values[loop_min_max]
        
        extend = (rbind(
          "p"=c(loop_min_max, "numb"),
          "left/right" = c("left","right"),
          "factor" = c(+1,actual_min),
          "relation" = c("above","above")
        ))
        
        colnames(extend) = c("sub_1","sub_2")
        
        
        all_extracted[[paste0("line", extend_all_extracted_numb)]] =
          extend
        
        
        extend_all_extracted_numb = extend_all_extracted_numb + 1
      }
      
    }
    
    ###
    
    if(input$check_max == TRUE){
      
      numb_p_max = length(max_values)
      numb_p = ifelse(numb_p > numb_p_max,numb_p,numb_p_max)
      
      if(exists("extend_all_extracted_numb") == FALSE){
        
        extend_all_extracted_numb = length(all_extracted) + 1
      }
      
      
      for(loop_min_max in 1 : numb_p_max){
        
        actual_max = max_values[loop_min_max]
        
        extend = (rbind(
          "p"=c(loop_min_max, "numb"),
          "left/right" = c("left","right"),
          "factor" = c(+1,actual_max),
          "relation" = c("below","below")
        ))
        
        colnames(extend) = c("sub_1","sub_2")
        
        
        all_extracted[[paste0("line", extend_all_extracted_numb)]] =
          extend
        
        extend_all_extracted_numb = extend_all_extracted_numb + 1
      }
      
    }
    
    ###
    
    numb_equl_ineq = length(all_extracted)
    
    ineq_eq_left = matrix(NA,ncol=numb_p,nrow=numb_equl_ineq)
    ineq_eq_right = rep(NA,numb_equl_ineq)
    all_operators = rep(NA,numb_equl_ineq)
    
    ### 
    
    for(loop_relations in 1 : length(all_extracted)){
      
      actual_relations = data.frame(all_extracted[loop_relations])
      
      extract_factors = numeric() 
      
      for(loop_factors in 1 : ncol(actual_relations)){
        
        extract_factors = c(extract_factors, eval(parse(text = actual_relations[3,loop_factors])))
        
      }
      
      actual_relations[3,] = (extract_factors)
      
      actual_relations_left = actual_relations[,actual_relations[2,] == "left"]
      actual_relations_right= actual_relations[,actual_relations[2,] == "right"]
      actual_relations_operator = actual_relations[4,1]
      
      actual_relations_left = data.frame(actual_relations_left)
      actual_relations_right = data.frame(actual_relations_right)
      
      numb_left = which(actual_relations_left[1,] == "numb")
      numb_right = which(actual_relations_right[1,] == "numb")
      
      non_numb_left = which(actual_relations_left[1,] != "numb")
      non_numb_right = which(actual_relations_right[1,] != "numb")
      
      #### change signs
      
      if(actual_relations_operator == "equal" & length(non_numb_right) > 0 & length(non_numb_left) > 0){
        
        actual_relations_right[3,non_numb_right] = -1 * as.numeric(actual_relations_right[3,non_numb_right])
        
        
      }
      
      if(actual_relations_operator == "below" & length(non_numb_right) > 0){
        
        actual_relations_right[3,non_numb_right] = -1 * as.numeric(actual_relations_right[3,non_numb_right])
        actual_relations_left[3,numb_left] = -1 * as.numeric(actual_relations_left[3,numb_left])
        
      }
      
      if(actual_relations_operator == "above" & length(non_numb_left) > 0){
        
        actual_relations_left[3,non_numb_left] = -1 * as.numeric(actual_relations_left[3,non_numb_left])
        actual_relations_right[3,numb_right] = -1 * as.numeric(  actual_relations_right[3,numb_right])
        actual_relations_operator = "below"
        actual_relations_left[4,] = "below"
        actual_relations_right[4,] = "below"
        
      }
      
      ## do fractions
      
      actual_relations_left = data.frame(actual_relations_left)
      actual_relations_right = data.frame(actual_relations_right)
      
      actual_relations_left[3,] = (d2q(as.numeric(actual_relations_left[3,])))
      actual_relations_right[3,] = (d2q(as.numeric(actual_relations_right[3,])))
      
      
      ## translate into RCDD input
      
      total_rcdd = data.frame(actual_relations_left,actual_relations_right)
      
      total_rcdd_leftside = data.frame(total_rcdd[,total_rcdd[1,] != "numb"])
      total_rcdd_rightside = data.frame(total_rcdd[,total_rcdd[1,] == "numb"])
      
      if(length(total_rcdd_rightside) == 0){
        
        
        total_rcdd_rightside = t(data.frame("numb","right","0",actual_relations_operator))
        
      }
      
      ###
      
      ineq_eq_left[loop_relations,as.numeric(unlist(total_rcdd_leftside[1,]))] = unlist(total_rcdd_leftside[3,])
      ineq_eq_left[loop_relations,-as.numeric(unlist(total_rcdd_leftside[1,]))] = "0"  
      ineq_eq_right[loop_relations] = total_rcdd_rightside[3,]
      all_operators[loop_relations]  = actual_relations_operator
      
    }
    
    #### approx equal
    
    tune_knob = input$approx_equal
    
    add_ineq_eq_left = numeric()
    add_ineq_eq_right = numeric()
    add_all_operators = numeric()
    
    if(tune_knob != 0){
      
      change_knob = ifelse(rowSums(ineq_eq_left!="0") >1,1,0)
      
      
      for(loop_tune in 1 : length(all_operators)){
        
        if(all_operators[loop_tune]=="equal"){
          
          if(change_knob[loop_tune] == 1){
            
            add_ineq_eq_left = rbind(add_ineq_eq_left,d2q(q2d(ineq_eq_left[loop_tune,]) ))
            add_ineq_eq_left = rbind(add_ineq_eq_left,d2q(-1 * q2d(ineq_eq_left[loop_tune,])))
            
            add_ineq_eq_right = c(add_ineq_eq_right,q2d(ineq_eq_right[loop_tune]) + tune_knob,
                                  (q2d(ineq_eq_right[loop_tune]) + tune_knob))
            
            add_all_operators = c(add_all_operators,rep("below",2))
            
          }else{
            
            add_ineq_eq_left = rbind(add_ineq_eq_left,d2q(q2d(ineq_eq_left[loop_tune,]) ))
            add_ineq_eq_left = rbind(add_ineq_eq_left,d2q(-1 * q2d(ineq_eq_left[loop_tune,])))
            
            add_ineq_eq_right = c(add_ineq_eq_right,q2d(ineq_eq_right[loop_tune]) + tune_knob,
                                  (-(q2d(ineq_eq_right[loop_tune]) - tune_knob)))
            
            add_all_operators = c(add_all_operators,rep("below",2))
            
            
          }
          
          
          
        }
      }
      
      add_ineq_eq_right = d2q(add_ineq_eq_right)
      
      ineq_eq_left = ineq_eq_left[all_operators!="equal" ,]
      ineq_eq_right = ineq_eq_right[all_operators!="equal"]
      all_operators = all_operators[all_operators!="equal"]
      
      ineq_eq_left = rbind(ineq_eq_left,add_ineq_eq_left)
      ineq_eq_right = c(ineq_eq_right,add_ineq_eq_right)
      all_operators = c(all_operators,add_all_operators)
      
    }
    
    all_operators <<- all_operators
    ineq_eq_left <<- ineq_eq_left
    ineq_eq_right <<- ineq_eq_right
    numb_p <<- numb_p
    outs <<- outs
    
  }
  
  #### h and v-representations #### 
  
  observe({
    
    output$h <- renderUI({
      
      all_h_rep_in_matrix = numeric()
      
      numb_models = counter_ie$n
      
      v_representation_plot_all = numeric()
      
      withProgress(message = 'In progress',value = 0, expr= {
        
        input_relations = list()
        
        for(loop in 1 : counter_ie$n){
          
          input_relations[[paste0("rel", loop)]] =    
            eval(parse(text=paste("unlist(AllInputs()$textin_relations_",loop,")",sep="")))
          
        }
        
        input_relations <<-  input_relations
        #### * single models ####
        
        formula_h_all = ""
        
        all_input_models = numeric()
        all_input_models_names = numeric()
        
        for(loop_numb_models in 1 : numb_models){
          
          all_input_models = c(all_input_models,(eval(parse(text=paste("input_relations$rel",
                                                                       loop_numb_models,sep="")))))
          
          all_input_models_names = c(all_input_models_names,
                                     eval(parse(text=paste("unlist(AllInputs()$textin_relations_name",
                                                           loop_numb_models,")",sep=""))))
        }
        
        input_mix_inter = input$text_mix_inter
        
        input_mix_inter = unlist(str_split(input_mix_inter,";"))
        input_mix_inter = str_replace_all(input_mix_inter," ","")
        input_mix_inter = unlist(input_mix_inter)
        input_inter = input_mix_inter[str_detect(input_mix_inter,"inter")]
        input_mix = input_mix_inter[str_detect(input_mix_inter,"mix")]
        
        inter_models_list = list()
        
        for(loop_inter in 1 : length(input_inter)){
          
          input_inter[loop_inter] = 
            substr(input_inter[loop_inter],7,
                   str_length(input_inter[loop_inter]) -1)
          
          inter_models_list[loop_inter] = list( (str_split( input_inter[loop_inter],",")))
          
        }
        
        mix_models_list = list()
        
        for(loop_mix in 1 : length(input_mix)){
          
          input_mix[loop_mix] = 
            substr(input_mix[loop_mix],5,
                   str_length(input_mix[loop_mix]) -1)
          
          mix_models_list[loop_mix] = list( (str_split( input_mix[loop_mix],",")))
          
        }
        
        reference_list = data.frame("rel_numb" = names(input_relations),
                                    "mod_name" = (all_input_models_names))
        
        
        #### * build intersection models ####
        
        if(unlist(AllInputs()$check_mix_inter)==TRUE){
          
          if(is.na(unlist(inter_models_list[1])[1]) == FALSE){
            
            for(loop_inter in 1 : length(inter_models_list)){
              
              sel_model = which(reference_list$mod_name %in% 
                                  unlist(inter_models_list[loop_inter]))
              
              input_relations[[paste0("rel", numb_models + loop_inter)]] = paste(all_input_models[sel_model],collapse=";")
              
              all_input_models_names = c(all_input_models_names,paste("inter_",str_replace_all(input_inter[loop_inter],",","_"),sep= ""))
              
            }
            
            numb_models = numb_models + loop_inter
            
          }
          
        }
        
        #### v- and h-representation models and intersection models ####
        
        for(loop_numb_models in 1 : numb_models){
          
          incProgress(1/(2*numb_models))
          
          extract_info(eval(parse(text=paste("input_relations$rel",
                                             loop_numb_models,sep=""))))
          
          
          if(sum(all_operators != "equal") > 0 &  sum(all_operators == "equal") > 0){
            
            h_representation = makeH(ineq_eq_left[all_operators != "equal",], 
                                     ineq_eq_right[all_operators != "equal"],
                                     ineq_eq_left[all_operators == "equal",],
                                     ineq_eq_right[all_operators == "equal"])
            
            
          }
          
          
          if(sum(all_operators != "equal") == 0 &  sum(all_operators == "equal") > 0){
            
            h_representation = makeH(a2 = ineq_eq_left,
                                     b2 = ineq_eq_right) 
            
            
          }
          
          if(sum(all_operators != "equal") > 0 &  sum(all_operators == "equal") == 0){
            
            h_representation = makeH(ineq_eq_left, 
                                     ineq_eq_right) 
            
            
          }
          
          if(nrow(h_representation)>1){
            
            h_representation = redundant(h_representation)$output
            
          }else{
            
            h_representation = redundant(h_representation)$output
          }
          
          
          current_name =  all_input_models_names[loop_numb_models]
          
          all_h_rep_in_matrix = rbind(all_h_rep_in_matrix,
                                      data.frame( current_name,
                                                  h_representation))
          
          
          #### * v-rep #####
          
          v_representation =  scdd(h_representation)
          
          v_representation = matrix(
            sapply(v_representation$output, function(x) eval(parse(text=x))),
            ncol = ncol(v_representation$output))
          
          v_representation_plot = data.frame(v_representation) 
          v_representation_plot = v_representation_plot[,3:ncol(v_representation_plot)]
          
          
          if(input$check_name == T){
            
            colnames(v_representation_plot) = outs$name
            
            
          }else{
            
            colnames(v_representation_plot) = paste("p_",1:ncol(v_representation_plot),sep="")
            
          }
          
          row.names(v_representation_plot) = paste(paste("V_",loop_numb_models,"_",sep=""),1:nrow(v_representation_plot),sep="")
          
          
          v_representation_plot = cbind("model_name" = current_name,v_representation_plot)
          
          v_representation_plot_all = rbind(v_representation_plot_all,v_representation_plot)
          
          h_representation_pl = q2d(h_representation)
          h_representation_pl = fractions(h_representation_pl)
          
          if(input$check_name == T){
            
            colnames(h_representation_pl) = c("ineq/eq","right",paste(" \\text{ ",outs$name," }",sep=""))
            
          }else{
            
            colnames(h_representation_pl) = c("ineq/eq","right",paste("p_{",1:numb_p,"}",sep=""))
            
          }
          
          begin_eq = paste(current_name," $$\\begin{eqnarray} ",sep="")
          end_eq = " \\end{eqnarray}$$"
          equation_all = paste(begin_eq,latex(h_representation_pl),end_eq,sep="")
          
          formula_h_all = paste(formula_h_all,equation_all,collapse="")
        }
        
        
        col_names_h = c("model name","equ/ineq","right",paste("p",1:(ncol(all_h_rep_in_matrix)-3),sep=""))
        
        colnames(all_h_rep_in_matrix) = col_names_h
        
        #### * v- and h-representation of mixtures ####
        
        if(unlist(AllInputs()$check_mix_inter)==TRUE){
          
          list_mixture_h = list()
          
          if(is.na(unlist(mix_models_list[1])[1]) == FALSE){
            
            all_mixture_v_plot = numeric()
            
            for(loop_model_mix in 1 : length(mix_models_list)){
              
              model_name_mix = unlist(mix_models_list[loop_model_mix])
              
              
              v_all = (v_representation_plot_all)[v_representation_plot_all[,1] %in% model_name_mix,2:ncol(v_representation_plot_all)]
              
              v_all = matrix(as.numeric(unlist(v_all)),ncol=ncol(v_all))
              v_all = v_all[!duplicated(v_all), ]
              
              v_all = d2q(v_all)
              
              mixture_v = makeV(v_all)
              
              dim_mixture=dim(mixture_v)[1]
              
              
              if(dim_mixture > 1){
                
                mixture_v = redundant(mixture_v)
                
              }
              
              
              if(dim_mixture > 1){ 
                
                mixture_h = scdd(mixture_v$output)
                mixture_h = q2d(mixture_h$output)
                mixture_h = redundant(mixture_h)
                
                
              }else{
                
                mixture_h = scdd(mixture_v)
                mixture_h = q2d(mixture_h$output)
                
                
              }
              
              list_mixture_h[loop_model_mix] = list(mixture_h)
              
              
              ####
              
              if(dim_mixture> 1){ 
                
                mixture_v = matrix(
                  sapply(mixture_v$output, function(x) eval(parse(text=x))),
                  ncol = ncol(mixture_v$output))
                
              }else{
                
                mixture_v = matrix(
                  sapply(mixture_v, function(x) eval(parse(text=x))),
                  ncol = ncol(mixture_v))
                
              }
              
              mixture_v_plot = data.frame(mixture_v)
              mixture_v_plot = mixture_v_plot[,3:ncol(mixture_v_plot)]
              
              mixture_v_plot = (fractions(matrix(unlist(mixture_v_plot),ncol=ncol(mixture_v_plot)))) #new
              
              if(input$check_name == T){
                
                colnames(mixture_v_plot) = outs$name
                
              }else{
                
                colnames(mixture_v_plot) = paste("p_",1:ncol(mixture_v_plot),sep="")
                
              }
              
              row.names(mixture_v_plot) = paste("V_",1:nrow(mixture_v_plot),sep="")
              
              all_mixture_v_plot = rbind(all_mixture_v_plot,data.frame("model_name"= 
                                                                         paste("mix_",paste(model_name_mix,collapse="_"),sep=""),
                                                                       mixture_v_plot))
              
              mix_h_rep_in_matrix = data.frame(paste("mix_",paste(model_name_mix,collapse="_"),sep=""),mixture_h$output)
              colnames(mix_h_rep_in_matrix) = col_names_h
              
              all_h_rep_in_matrix = 
                rbind(all_h_rep_in_matrix,mix_h_rep_in_matrix)
              
              
            }
          }  
          
        }
        
        all_h_rep_in_matrix <<- all_h_rep_in_matrix
        
        ####* all v-representations ####
        
        if(is.na(unlist(mix_models_list[1])[1]) == FALSE & 
           unlist(AllInputs()$check_mix_inter)==TRUE){
          
          
          all_v_plot = rbind(v_representation_plot_all,all_mixture_v_plot)
          
        }else{
          
          all_v_plot = v_representation_plot_all
          
        }
        
        output$all_v_plot =  DT::renderDataTable(all_v_plot)
        
        names_submodels = unique(all_v_plot[,1])
        n_submodels = length(unique(names_submodels))
        
        v_representation_plot_all_list = list()
        
        for(loop_sub_models in 1 : n_submodels){
          
          v_representation_plot_all_list[[loop_sub_models]] = 
            all_v_plot[all_v_plot[,1] ==  
                         names_submodels[loop_sub_models],1:ncol(all_v_plot)]
          
        }
        
        #### * plot models based on v-representations ####
        
        function_plot(v_representation_plot_all_list,n_submodels)
        
        #### * prepare h-representation of mixture ####
        
        if(unlist(AllInputs()$check_mix_inter)==TRUE &
           is.na(unlist(mix_models_list[1])[1]) == FALSE){
          
          
          
          for(loop_mix_print in 1 : length(list_mixture_h)){
            
            actual_mix = list_mixture_h[[loop_mix_print]]
            
            if(dim_mixture > 1){ 
              
              mixture_h_pl = (actual_mix$output)
              
            }else{
              
              mixture_h_pl = (actual_mix)
              
            }
            
            mixture_h_pl = fractions(mixture_h_pl)
            
            numb_p = ncol(mixture_h_pl)-2
            
            
            if(input$check_name == T){
              
              colnames(mixture_h_pl) = c("ineq/eq","right",paste("\\text{ ",outs$name," }",sep=""))
              
            }else{
              
              colnames(mixture_h_pl) = c("ineq/eq","right",paste("p_{",1:numb_p,"}",sep=""))
              
            }
            
            ind_mix = ifelse(rowSums(abs(mixture_h_pl[,3:ncol(mixture_h_pl)])) > 0,1,0)
            
            mixture_h_pl = mixture_h_pl[ind_mix==1,]
            model_name_mix = unlist(mix_models_list[loop_mix_print])
            
            
            begin_eq = paste( paste("mix_",paste(model_name_mix,collapse="_"),sep="")
                              ," $$\\begin{eqnarray} ",sep="")
            end_eq = " \\end{eqnarray}$$"
            
            equation_all = c(paste(begin_eq,latex(mixture_h_pl),end_eq,sep=""))
            
            
            
            formula_h_all = paste(formula_h_all,equation_all,collapse="")
            
            
            
          }}
        
        incProgress(numb_models)
        
        withMathJax(helpText({ 
          formula_h_all
        }))
        
      })
      
      
    })
  })
  
  
  #### parsimony ####
  
  output$plot_parsimony = renderPlotly({
    
      n_samples = 100000
      
      n_rows = nrow(all_h_rep_in_matrix)
      numb_p = ncol(all_h_rep_in_matrix)-3
    
      sample_p = matrix(runif(numb_p * n_samples),ncol = numb_p )
      
      numb_ineq = data.frame(table(all_h_rep_in_matrix$`model name`))
      colnames(numb_ineq) = c("model.name","numb")

      all_h_rep_in_matrix = 
        do.call(rbind, replicate(n_samples, all_h_rep_in_matrix, 
                                 simplify=FALSE))
      
      sample_p = sample_p[rep(1:nrow(sample_p), each = n_rows), ]
      
      all_h_rep_in_matrix = data.frame(
        "sim" = rep(1:n_samples,each = n_rows),
        all_h_rep_in_matrix)

      all_h_rep_in_matrix_p = all_h_rep_in_matrix[,(ncol(all_h_rep_in_matrix) - (numb_p-1)) :ncol(all_h_rep_in_matrix)]
        
      all_h_rep_in_matrix_p = matrix(q2d(unlist(all_h_rep_in_matrix_p)),ncol=ncol(all_h_rep_in_matrix_p))
 
      all_h_rep_in_matrix_p = rowSums(all_h_rep_in_matrix_p*sample_p*-1)
 
      all_h_rep_in_matrix_p = data.frame(all_h_rep_in_matrix[,1:2],
                                         "equal.inequal"=q2d(all_h_rep_in_matrix[,3]),
                                         "right"=q2d(all_h_rep_in_matrix[,4]),  
                                         "value" = all_h_rep_in_matrix_p)
   
      all_h_rep_in_matrix_p$fulfilled = NA
      
      all_h_rep_in_matrix_p[all_h_rep_in_matrix_p$equal.inequ == 1,ncol(all_h_rep_in_matrix_p)]  = 
      ifelse(round(all_h_rep_in_matrix_p[all_h_rep_in_matrix_p$equal.inequ == 1,ncol(all_h_rep_in_matrix_p)-1],5) == 
        round(all_h_rep_in_matrix_p[all_h_rep_in_matrix_p$equal.inequ == 1,ncol(all_h_rep_in_matrix_p)-2],5),1,0)
      
      all_h_rep_in_matrix_p[all_h_rep_in_matrix_p$equal.inequ == 0,ncol(all_h_rep_in_matrix_p)]  = 
        ifelse(round(all_h_rep_in_matrix_p[all_h_rep_in_matrix_p$equal.inequ == 0,ncol(all_h_rep_in_matrix_p)-1],5) <=
                 round(all_h_rep_in_matrix_p[all_h_rep_in_matrix_p$equal.inequ == 0,ncol(all_h_rep_in_matrix_p)-2],5),1,0)

      sim_summarize = all_h_rep_in_matrix_p %>%
        group_by(sim, model.name) %>%
        summarize(sum_model = sum(fulfilled))
      
      sim_summarize = left_join(sim_summarize,numb_ineq,"model.name")
        
      sim_summarize$within = ifelse(sim_summarize$sum_model == sim_summarize$numb, 1, 0)
      
      sim_summarize = sim_summarize %>%
        group_by(model.name) %>%
        summarize(hyperspace = 100*mean(within))
      

      
      fig <- plot_ly(sim_summarize, x = ~model.name, y = ~hyperspace, type = 'bar')

      fig <- fig %>% layout(yaxis = list(title = 'Percentage of occupied hyperspace'
                                         , range = c(0,100)), 
                            xaxis = list(title = 'Model'),
                            barmode = 'group')

      fig

  })
  
})

shinyApp(ui, server)