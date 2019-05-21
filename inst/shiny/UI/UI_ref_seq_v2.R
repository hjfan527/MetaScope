library(shiny)
library(taxize)

# Define UI for app that draws a histogram ----
load
superkingdom_list <- class_table.all[which(class_table.all$rank == 'superkingdom'),]$name

ui <- fluidPage(
  
  fluid=TRUE,
  theme = "bootstrap.min.css",
  
  tabPanel(title = "Library Generation",
           mainPanel(# the following lines could be uncommented when the download ref seq can
             # work on the rest of the kingdoms
             # radioButtons("kingdom", "Choose a kingdom:",
             #              c("Archaea" = "archaea",
             #                "Bacteria" = "bacteria",
             #                "Fungi" = "fungi",
             #                "Invertebrate" = "invertebrate",
             #                "Plant" = "plant",
             #                "Protozoa" = "protozoa",
             #                "Vertebrate" = "vertibrate",
             #                "Vertebrate other" = "vertibrate_other",
             #                "Virus" = "viral")
             #              ),
             
             # create checkbox input for representative library and reference library
             checkboxInput("representative", "representative", value = TRUE, width = NULL),
             checkboxInput("reference", "reference", value = FALSE, width = NULL),
             
             
             actionButton("downloadref","Download Ref_Seq")
           )
  ),
  mainPanel(
    tabsetPanel(
      type = "tabs",
      
      tabPanel("Superkingdom",
               radioButtons("superkingdomGroup", label = "Choose a superkingdom:", 
                            choices = superkingdom_list),
               hr(),
               actionButton("superkingdom_update","Update")
      ),
      
      tabPanel(title = uiOutput("kingdom_panel"),
               checkboxGroupInput('kingdomGroup', label = "Choose a kingdom:"),
               actionLink("kingdom_selectall","select all"),
               hr(),
               actionButton("kingdom_update","Update")
      ),
      
      tabPanel(title = uiOutput("phylum_panel"),
               checkboxGroupInput("phylumGroup", label = "Choose a phylum:"),
               actionLink("phylum_selectall","select all"),
               hr(),
               actionButton("phylum_update","Update")
      ),
      
      tabPanel(title = uiOutput("class_panel"),
               checkboxGroupInput("classGroup", label = "Choose a class:"),
               actionLink("class_selectall","select all"),
               hr(),
               actionButton("class_update","Update")
      ),
      
      tabPanel(title = uiOutput("order_panel"),
               checkboxGroupInput("orderGroup", label = "Choose an order:"),
               actionLink("order_selectall","select all"),
               hr(),
               actionButton("order_update","Update")
      ),
      
      tabPanel(title = uiOutput("family_panel"),
               checkboxGroupInput("familyGroup", label = "Choose a family:"),
               actionLink("family_selectall","select all"),
               hr(),
               actionButton("family_update","Update")
      ),
      
      tabPanel(title = uiOutput("genus_panel"),
               checkboxGroupInput("genusGroup", label = "Choose a genus:"),
               actionLink("genus_selectall","select all"),
               hr(),
               actionButton("genus_update","Update")
      ),
      
      tabPanel(title = uiOutput("species_panel"),
               checkboxGroupInput("speciesGroup", label = "Choose a species:"),
               actionLink("species_selectall","select all"),
               hr(),
               actionButton("species_update","Update")
      )
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  output$value <- renderPrint({ input$kingdomGroup })

  k_list.sk <- reactiveVal(character(0))
  p_list.sk <- reactiveVal(character(0))
  c_list.sk <- reactiveVal(character(0))
  o_list.sk <- reactiveVal(character(0))
  f_list.sk <- reactiveVal(character(0))
  g_list.sk <- reactiveVal(character(0))
  s_list.sk <- reactiveVal(character(0))
    
  p_list.k <- reactiveVal(character(0))
  c_list.k <- reactiveVal(character(0))
  o_list.k <- reactiveVal(character(0))
  f_list.k <- reactiveVal(character(0))
  g_list.k <- reactiveVal(character(0))
  s_list.k <- reactiveVal(character(0))
  
  c_list.p <- reactiveVal(character(0))
  o_list.p <- reactiveVal(character(0))
  f_list.p <- reactiveVal(character(0))
  g_list.p <- reactiveVal(character(0))
  s_list.p <- reactiveVal(character(0))
  
  o_list.c <- reactiveVal(character(0))
  f_list.c <- reactiveVal(character(0))
  g_list.c <- reactiveVal(character(0))
  s_list.c <- reactiveVal(character(0))
  
  f_list.o <- reactiveVal(character(0))
  g_list.o <- reactiveVal(character(0))
  s_list.o <- reactiveVal(character(0))
  
  g_list.f <- reactiveVal(character(0))
  s_list.f <- reactiveVal(character(0))
  
  s_list.g <- reactiveVal(character(0))
  
  output$kingdom_panel = renderText("Kingdom")
  output$phylum_panel = renderText("Phylum")
  output$class_panel = renderText("Class")
  output$order_panel = renderText("Order")
  output$family_panel = renderText("Family")
  output$genus_panel = renderText("Genus")
  output$species_panel = renderText("Species")

  observeEvent(input$superkingdom_update,{
    sk_input <- input$superkingdomGroup
    
    k_list.new <- character(0)
    p_list.new <- character(0)
    c_list.new <- character(0)
    o_list.new <- character(0)
    f_list.new <- character(0)
    g_list.new <- character(0)
    s_list.new <- character(0)
    
    children_list <- class_table.all[which(class_table.all$parent_taxon %in% sk_input),]
    k_list.new <- children_list[which(children_list$rank == 'kingdom'),]$name
    p_list.new <- children_list[which(children_list$rank == 'phylum'),]$name
    c_list.new <- children_list[which(children_list$rank == 'class'),]$name
    o_list.new <- children_list[which(children_list$rank == 'order'),]$name
    f_list.new <- children_list[which(children_list$rank == 'family'),]$name
    g_list.new <- children_list[which(children_list$rank == 'genus'),]$name
    s_list.new <- children_list[which(children_list$rank == 'species'),]$name
    
    output$kingdom_panel <- renderText(paste("Kingdom (",length(k_list.new),")",sep=""))
    output$phylum_panel <- renderText(paste("Phylum (",length(p_list.new),")",sep=""))
    output$class_panel <- renderText(paste("Class (",length(c_list.new),")",sep=""))
    output$order_panel <- renderText(paste("Order (",length(o_list.new),")",sep=""))
    output$family_panel <- renderText(paste("Family (",length(f_list.new),")",sep=""))
    output$genus_panel <- renderText(paste("Genus (",length(g_list.new),")",sep=""))
    output$species_panel <- renderText(paste("Species (",length(s_list.new),")",sep=""))
    
    updateCheckboxGroupInput(session, "kingdomGroup", choices = sort(k_list.new), selected = k_list.new)
    updateCheckboxGroupInput(session, "phylumGroup", choices = sort(p_list.new), selected = p_list.new)
    updateCheckboxGroupInput(session, "classGroup", choices = sort(c_list.new), selected = c_list.new)
    updateCheckboxGroupInput(session, "orderGroup", choices = sort(o_list.new), selected = o_list.new)
    updateCheckboxGroupInput(session, "familyGroup", choices = sort(f_list.new), selected = f_list.new)
    updateCheckboxGroupInput(session, "genusGroup", choices = sort(g_list.new), selected = g_list.new)
    updateCheckboxGroupInput(session, "speciesGroup", choices = sort(s_list.new), selected = s_list.new)
    
    k_list.sk(k_list.new)
    p_list.sk(p_list.new)
    c_list.sk(c_list.new)
    o_list.sk(o_list.new)
    f_list.sk(f_list.new)
    g_list.sk(g_list.new)
    s_list.sk(s_list.new)
  })
  
  observeEvent(input$kingdom_update,{
    k_input <- input$kingdomGroup
    
    p_list.new <- character(0)
    c_list.new <- character(0)
    o_list.new <- character(0)
    f_list.new <- character(0)
    g_list.new <- character(0)
    s_list.new <- character(0)
    
    children_list <- class_table.all[which(class_table.all$parent_taxon %in% k_input),]
    p_list.new <- children_list[which(children_list$rank == 'phylum'),]$name
    c_list.new <- children_list[which(children_list$rank == 'class'),]$name
    o_list.new <- children_list[which(children_list$rank == 'order'),]$name
    f_list.new <- children_list[which(children_list$rank == 'family'),]$name
    g_list.new <- children_list[which(children_list$rank == 'genus'),]$name
    s_list.new <- children_list[which(children_list$rank == 'species'),]$name

    phylum_choices <- unique(c(p_list.new,
                               p_list.sk()))    
    class_choices <- unique(c(c_list.new,
                              c_list.sk()))
    order_choices <- unique(c(o_list.new,
                              o_list.sk()))
    family_choices <- unique(c(f_list.new,
                               f_list.sk()))
    genus_choices <- unique(c(g_list.new,
                              g_list.sk()))
    species_choices <- unique(c(s_list.new,
                                s_list.sk()))
    
    updateCheckboxGroupInput(session, "phylumGroup", choices = sort(phylum_choices), selected = phylum_choices)
    updateCheckboxGroupInput(session, "classGroup", choices = sort(class_choices), selected = class_choices)
    updateCheckboxGroupInput(session, "orderGroup", choices = sort(order_choices), selected = order_choices)
    updateCheckboxGroupInput(session, "familyGroup", choices = sort(family_choices), selected = family_choices)
    updateCheckboxGroupInput(session, "genusGroup", choices = sort(genus_choices), selected = genus_choices)
    updateCheckboxGroupInput(session, "speciesGroup", choices = sort(species_choices), selected = species_choices)
 
    output$phylum_panel <- renderText(paste("Phylum (",length(phylum_choices),")",sep=""))   
    output$class_panel <- renderText(paste("Class (",length(class_choices),")",sep=""))
    output$order_panel <- renderText(paste("Order (",length(order_choices),")",sep=""))
    output$family_panel <- renderText(paste("Family (",length(family_choices),")",sep=""))
    output$genus_panel <- renderText(paste("Genus (",length(genus_choices),")",sep=""))
    output$species_panel <- renderText(paste("Species (",length(species_choices),")",sep=""))
    
    p_list.k(p_list.new)
    c_list.k(c_list.new)
    o_list.k(o_list.new)
    f_list.k(f_list.new)
    g_list.k(g_list.new)
    s_list.k(s_list.new)
  })
  
  observeEvent(input$phylum_update,{
    p_input <- input$phylumGroup
    
    c_list.new <- character(0)
    o_list.new <- character(0)
    f_list.new <- character(0)
    g_list.new <- character(0)
    s_list.new <- character(0)
    
    children_list <- class_table.all[which(class_table.all$parent_taxon %in% p_input),]
    c_list.new <- children_list[which(children_list$rank == 'class'),]$name
    o_list.new <- children_list[which(children_list$rank == 'order'),]$name
    f_list.new <- children_list[which(children_list$rank == 'family'),]$name
    g_list.new <- children_list[which(children_list$rank == 'genus'),]$name
    s_list.new <- children_list[which(children_list$rank == 'species'),]$name
    
    class_choices <- unique(c(c_list.new,
                              c_list.sk(),
                              c_list.k()))
    order_choices <- unique(c(o_list.new,
                              o_list.sk(),
                              o_list.k()))
    family_choices <- unique(c(f_list.new,
                               f_list.sk(),
                               f_list.k()))
    genus_choices <- unique(c(g_list.new,
                              g_list.sk(),
                              g_list.k()))
    species_choices <- unique(c(s_list.new,
                                s_list.sk(),
                                s_list.k()))
    
    updateCheckboxGroupInput(session, "classGroup", choices = sort(class_choices), selected = class_choices)
    updateCheckboxGroupInput(session, "orderGroup", choices = sort(order_choices), selected = order_choices)
    updateCheckboxGroupInput(session, "familyGroup", choices = sort(family_choices), selected = family_choices)
    updateCheckboxGroupInput(session, "genusGroup", choices = sort(genus_choices), selected = genus_choices)
    updateCheckboxGroupInput(session, "speciesGroup", choices = sort(species_choices), selected = species_choices)
    
    output$class_panel <- renderText(paste("Class (",length(class_choices),")",sep=""))
    output$order_panel <- renderText(paste("Order (",length(order_choices),")",sep=""))
    output$family_panel <- renderText(paste("Family (",length(family_choices),")",sep=""))
    output$genus_panel <- renderText(paste("Genus (",length(genus_choices),")",sep=""))
    output$species_panel <- renderText(paste("Species (",length(species_choices),")",sep=""))
    
    c_list.p(c_list.new)
    o_list.p(o_list.new)
    f_list.p(f_list.new)
    g_list.p(g_list.new)
    s_list.p(s_list.new)
  })
  
  observeEvent(input$class_update,{
    c_input <- input$classGroup
    
    o_list.new <- character(0)
    f_list.new <- character(0)
    g_list.new <- character(0)
    s_list.new <- character(0)
    
    children_list <- class_table.all[which(class_table.all$parent_taxon %in% c_input),]
    o_list.new <- children_list[which(children_list$rank == 'order'),]$name
    f_list.new <- children_list[which(children_list$rank == 'family'),]$name
    g_list.new <- children_list[which(children_list$rank == 'genus'),]$name
    s_list.new <- children_list[which(children_list$rank == 'species'),]$name
    
    order_choices <- unique(c(o_list.new,
                              o_list.sk(),
                              o_list.k(),
                              o_list.p()))
    family_choices <- unique(c(f_list.new,
                               f_list.sk(),
                               f_list.k(),
                               f_list.p()))
    genus_choices <- unique(c(g_list.new,
                              g_list.sk(),
                              g_list.k(),
                              g_list.p()))
    species_choices <- unique(c(s_list.new,
                                s_list.sk(),
                                s_list.k(),
                                s_list.p()))
    
    updateCheckboxGroupInput(session, "orderGroup", choices = sort(order_choices), selected = order_choices)
    updateCheckboxGroupInput(session, "familyGroup", choices = sort(family_choices), selected = family_choices)
    updateCheckboxGroupInput(session, "genusGroup", choices = sort(genus_choices), selected = genus_choices)
    updateCheckboxGroupInput(session, "speciesGroup", choices = sort(species_choices), selected = species_choices)
    
    output$order_panel <- renderText(paste("Order (",length(order_choices),")",sep=""))
    output$family_panel <- renderText(paste("Family (",length(family_choices),")",sep=""))
    output$genus_panel <- renderText(paste("Genus (",length(genus_choices),")",sep=""))
    output$species_panel <- renderText(paste("Species (",length(species_choices),")",sep=""))
    
    o_list.c(o_list.new)
    f_list.c(f_list.new)
    g_list.c(g_list.new)
    s_list.c(s_list.new)
  })
  
  observeEvent(input$order_update,{
    o_input <- input$orderGroup
    
    f_list.new <- character(0)
    g_list.new <- character(0)
    s_list.new <- character(0)
    
    children_list <- class_table.all[which(class_table.all$parent_taxon %in% o_input),]
    f_list.new <- children_list[which(children_list$rank == 'family'),]$name
    g_list.new <- children_list[which(children_list$rank == 'genus'),]$name
    s_list.new <- children_list[which(children_list$rank == 'species'),]$name
    
    family_choices <- unique(c(f_list.new,
                               f_list.sk(),
                               f_list.k(),
                               f_list.p(),
                               f_list.c()))
    genus_choices <- unique(c(g_list.new,
                              g_list.sk(),
                              g_list.k(),
                              g_list.p(),
                              g_list.c()))
    species_choices <- unique(c(s_list.new,
                                s_list.sk(),
                                s_list.k(),
                                s_list.p(),
                                s_list.c()))
    
    updateCheckboxGroupInput(session, "familyGroup", choices = sort(family_choices), selected = family_choices)
    updateCheckboxGroupInput(session, "genusGroup", choices = sort(genus_choices), selected = genus_choices)
    updateCheckboxGroupInput(session, "speciesGroup", choices = sort(species_choices), selected = species_choices)
    
    output$family_panel <- renderText(paste("Family (",length(family_choices),")",sep=""))
    output$genus_panel <- renderText(paste("Genus (",length(genus_choices),")",sep=""))
    output$species_panel <- renderText(paste("Species (",length(species_choices),")",sep=""))
    
    
    f_list.o(f_list.new)
    g_list.o(g_list.new)
    s_list.o(s_list.new)
  })
  
  observeEvent(input$family_update,{
    f_input <- input$familyGroup
    
    g_list.new <- character(0)
    s_list.new <- character(0)
    
    children_list <- class_table.all[which(class_table.all$parent_taxon %in% f_input),]
    g_list.new <- children_list[which(children_list$rank == 'genus'),]$name
    s_list.new <- children_list[which(children_list$rank == 'species'),]$name
    
    genus_choices <- unique(c(g_list.new,
                              g_list.sk(),
                              g_list.k(),
                              g_list.p(),
                              g_list.c(),
                              g_list.o()))
    species_choices <- unique(c(s_list.new,
                                s_list.sk(),
                                s_list.k(),
                                s_list.p(),
                                s_list.c(),
                                s_list.o()))
    
    updateCheckboxGroupInput(session, "genusGroup", choices = sort(genus_choices), selected = genus_choices)
    updateCheckboxGroupInput(session, "speciesGroup", choices = sort(species_choices), selected = species_choices)
    
    output$genus_panel <- renderText(paste("Genus (",length(genus_choices),")",sep=""))
    output$species_panel <- renderText(paste("Species (",length(species_choices),")",sep=""))
    
    g_list.f(g_list.new)
    s_list.f(s_list.new)
  })
  
  observeEvent(input$genus_update,{
    g_input <- input$genusGroup
    
    s_list.new <- character(0)
    
    children_list <- class_table.all[which(class_table.all$parent_taxon %in% g_input),]
    s_list.new <- children_list[which(children_list$rank == 'species'),]$name
    
    species_choices <- unique(c(s_list.new,
                                s_list.sk(),
                                s_list.k(),
                                s_list.p(),
                                s_list.c(),
                                s_list.o(),
                                s_list.f()))
    
    updateCheckboxGroupInput(session, "speciesGroup", choices = sort(species_choices), selected = species_choices)
    
    output$species_panel <- renderText(paste("Species (",length(species_choices),")",sep=""))
    
    s_list.g(s_list.new)
  })
  
  # set action for "select all" actionLinks
  observeEvent(input$kingdom_selectall,{
    if(input$kingdom_selectall%%2 == 1){
      updateCheckboxGroupInput(session,
                               "kingdomGroup",
                               choices=sort(unique(k_list.sk()))
      )
    }
    else{
      updateCheckboxGroupInput(session,
                               "kingdomGroup",
                               choices=sort(unique(k_list.sk())),
                               selected=sort(unique(k_list.sk()))
      )
    }
  })
  observeEvent(input$phylum_selectall,{
    if(input$phylum_selectall%%2 == 1){
      updateCheckboxGroupInput(session,
                               "phylumGroup",
                               choices=sort(unique(c(p_list.sk(),p_list.k())))
      )
    }
    else{
      updateCheckboxGroupInput(session,
                               "phylumGroup",
                               choices=sort(unique(c(p_list.sk(),p_list.k()))),
                               selected=sort(unique(c(p_list.sk(),p_list.k())))
      )
    }
  })
  observeEvent(input$class_selectall,{
    if(input$class_selectall%%2 == 1){
      updateCheckboxGroupInput(session,
                               "classGroup",
                               choices=sort(unique(c(c_list.sk(),c_list.k(),c_list.p())))
      )
    }
    else{
      updateCheckboxGroupInput(session,
                               "classGroup",
                               choices=sort(unique(c(c_list.sk(),c_list.k(),c_list.p()))),
                               selected=sort(unique(c(c_list.sk(),c_list.k(),c_list.p())))
      )
    }
  })
  observeEvent(input$order_selectall,{
    if(input$order_selectall%%2 == 1){
      updateCheckboxGroupInput(session,
                               "orderGroup",
                               choices=sort(unique(c(o_list.sk(),o_list.k(),o_list.p(),o_list.c())))
      )
    }
    else{
      updateCheckboxGroupInput(session,
                               "orderGroup",
                               choices=sort(unique(c(o_list.sk(),o_list.k(),o_list.p(),o_list.c()))),
                               selected=sort(unique(c(o_list.sk(),o_list.k(),o_list.p(),o_list.c())))
      )
    }
  })
  observeEvent(input$family_selectall,{
    if(input$family_selectall%%2 == 1){
      updateCheckboxGroupInput(session,
                               "familyGroup",
                               choices=sort(unique(c(f_list.sk(),f_list.k(),f_list.p(),f_list.c(),f_list.o())))
      )
    }
    else{
      updateCheckboxGroupInput(session,
                               "familyGroup",
                               choices=sort(unique(c(f_list.sk(),f_list.k(),f_list.p(),f_list.c(),f_list.o()))),
                               selected=sort(unique(c(f_list.sk(),f_list.k(),f_list.p(),f_list.c(),f_list.o())))
      )
    }
  })
  observeEvent(input$genus_selectall,{
    if(input$genus_selectall%%2 == 1){
      updateCheckboxGroupInput(session,
                               "genusGroup",
                               choices=sort(unique(c(g_list.sk(),g_list.k(),g_list.p(),g_list.c(),g_list.o(),g_list.f())))
      )
    }
    else{
      updateCheckboxGroupInput(session,
                               "genusGroup",
                               choices=sort(unique(c(g_list.sk(),g_list.k(),g_list.p(),g_list.c(),g_list.o(),g_list.f()))),
                               selected=sort(unique(c(g_list.sk(),g_list.k(),g_list.p(),g_list.c(),g_list.o(),g_list.f())))
      )
    }
  })
  observeEvent(input$species_selectall,{
    if(input$species_selectall%%2 == 1){
      updateCheckboxGroupInput(session,
                               "speciesGroup",
                               choices=sort(unique(c(s_list.sk(),s_list.k(),s_list.p(),s_list.c(),s_list.o(),s_list.f(),s_list.g())))
      )
    }
    else{
      updateCheckboxGroupInput(session,
                               "speciesGroup",
                               choices=sort(unique(c(s_list.sk(),s_list.k(),s_list.p(),s_list.c(),s_list.o(),s_list.f(),s_list.g()))),
                               selected=sort(unique(c(s_list.sk(),s_list.k(),s_list.p(),s_list.c(),s_list.o(),s_list.f(),s_list.g())))
      )
    }
  })
  
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
