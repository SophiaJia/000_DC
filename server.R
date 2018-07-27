library(shiny)
library(tidyverse)
library(openxlsx)
library(janitor)
library(stringdist)
library(lubridate)
library(tableone)
library(shinyBS)
library(shinyjs)
library(DT)
library(plotly)
library(readxl)
# This function will create the buttons for the datatable, they will be unique
shinyInput <- function(FUN, len, id, ...) {inputs <- character(len)
for (i in seq_len(len)) {
  inputs[i] <- as.character(FUN(paste0(id, i), ...))}
inputs
}

allLevel <- function(x){
  x  %>% table %>% names %>% paste(., collapse="; ")
}

# marker objects (con hist)
m <- list(color = toRGB("black"))
m2 <- list(color = toRGB("black", 0.2),
           line = list(color = 'black', width = 0.3))

server <- function(input, output, session) {
  options(shiny.maxRequestSize=30*1024^2)
  options(DT.options = list(pageLength = 15))

  ###0 input                                                               ####
  inpD <- reactive({
    req(input$file1)
    ext <- tools::file_ext(input$file1$name)
    if (ext=="csv"){
      Org <- read.csv(input$file1$datapath,stringsAsFactors = FALSE)
    } else {
      Org <- read.xlsx(input$file1$datapath)
    }
    return(Org)
  }
  )

  inter_data = reactiveValues()
  outD <- reactive({
    Org=inpD()
    id  <- as.numeric(input$varID)
    Org <- Org[!Org[,id] == "",]# If ID = "" then delete this row
    #Org <- Org[!is.na(Org[,id])]
    NewDat <- FuzzyClean2(Org)

    # fix date as number when read from excel(may have better solutions)
    #NewDat <- NewDat %>% mutate_all(IS.Date_xlsx)
    inter_data[['afterfuzzy']] = NewDat
    return(NewDat)
  })

  ###2 con                                                               ####
  output$inpContinuous <- renderDataTable({
    di <- inter_data[['afterfuzzy']]
    d_con <- di[sapply(di, class) == "numeric" | sapply(di, class) == "integer"]

    contable <- cbind(
      #`ID` = c(1:length(colnames(d_con))),
      #`Variable name` = colnames(d_con),
      `Mean` = sapply(d_con, mean, na.rm=TRUE) %>% round(digits = 2),
      `Median` = apply(d_con, 2, median, na.rm=TRUE),
      `Minimum` = apply(d_con, 2, min, na.rm=TRUE),
      `Maximum` = apply(d_con, 2, max, na.rm=TRUE),
      `Missing No` =  apply(d_con, 2,function(x) sum(is.na(x)))
    )
    contable
  },selection = 'single')

  ## con data checking
  ## will first modify the checking table then the dataset

  # create the checking table
  # make it an reactiveValue for update
   con_check_data = reactiveValues(
     datarow = 2222
   )

   observeEvent(input$con_check,{
    di <- isolate(inter_data[['afterfuzzy']])
    `tmp ID` <-seq.int(nrow(di))
    di <- cbind(di,`tmp ID`)
    d_con <- di[sapply(di, class) == "numeric" | sapply(di, class) == "integer"]
    tmp <- d_con[(d_con[,input$inpContinuous_rows_selected] < as.numeric(input$con_min) | d_con[,input$inpContinuous_rows_selected] > as.numeric(input$con_max))&!is.na(d_con[,input$inpContinuous_rows_selected]) ,  ]
    out <- tmp[,c(as.numeric(input$varID),input$inpContinuous_rows_selected,length(tmp))]
    con_check_data[['table']] = out
 })

   # make it the DT output
   output$con_aftercheck<-DT::renderDataTable({
     #as.data.frame(con_check_data[['datarow']])
     #iris
     con_check_data[['table']][,c(1,2)]
   },
   editable = TRUE,
   selection = 'none',
   rownames = FALSE)

   # make DT proxy
   proxy_con = dataTableProxy('con_aftercheck')

   # update the value while edited

   observeEvent(input$con_aftercheck_cell_edit, {
     info = input$con_aftercheck_cell_edit
     str(info)
     i <- info$row
     j <- info$col + 1  # column index offset by 1
     v <- info$value
     tmp <- con_check_data[['table']]
     tmp[i, j] <- DT::coerceValue(v, tmp[i, j])
     rep <- tmp
     con_check_data[['table']] <- tmp
    # DT::replaceData(proxy, rep, resetPaging = FALSE, rownames = FALSE)
   })

   observeEvent(input$refresh_con,{
     update <- con_check_data[['table']]
     di     <- inter_data[['afterfuzzy']]
     `tmp ID` <-seq.int(nrow(di))
     di <- cbind(di,`tmp ID`)
     for (i in update[,3]){
       di[`tmp ID` ==  i, colnames(update)[2]] <- update[`tmp ID` ==  i, 2]
     }
     inter_data[['afterfuzzy']] <- di
   })

  # histogram with bins
  # convenience function for computing xbin/ybin object given a number of bins
  compute_bins <- function(x, n) {
    list(
      start = min(x, na.rm = T),
      end = max(x, na.rm = T),
      size = (max(x, na.rm = T) - min(x, na.rm = T)) / n
    )
  }

  # the histogram
  output$con_hist <- renderPlotly({
    di <- outD()
    d_con <- di[sapply(di, class) == "numeric" | sapply(di, class) == "integer"]
    index <- input$inpContinuous_rows_selected
    if(is.null(input$inpContinuous_rows_selected)){
      index = as.numeric(input$varID)
    }

    names <- colnames(d_con)[index]
    x  <- d_con[,index]
    xbins <- compute_bins(x, input$xbins)
    p <- plot_ly(x = x, type = "histogram", autobinx = F,
                 xbins = xbins, marker = m2)
    # obtain plotlyjs selection
    s <- event_data("plotly_selected")
    # if points are selected, subset the data, and highlight
    if (length(s$x) > 0) {
      p <- add_trace(p, x = s$x, type = "histogram", autobinx = F,
                     xbins = xbins, marker = m)
    }
    p %>%
      config(displayModeBar = F, showLink = F) %>%
      layout(showlegend = F, barmode = "overlay", yaxis = list(title = "count"),
             xaxis = list(title = names , showticklabels = T))
  })

  ###3 char                                                               ####

  output$inpCategorical1 <- renderDataTable({
    di <- inter_data[['afterfuzzy']]
    lev <- sapply(di[sapply(di, mode) == "character"| sapply(di, class) == "factor"], table)
    lev2<- mapply(length,lev)
    leveleq1 <- lev2[lev2 == 1] %>% names

    d_cat <- di[leveleq1]


    ta <- cbind(
      `Variable Name`  = colnames(d_cat),
      `Levels` = apply(d_cat, 2, allLevel),
      `Missing No` =  apply(d_cat, 2,function(x) sum(is.na(x))))
    rownames(ta) <- NULL
    ta

  }, escape=FALSE,
  options = list(sDom  = '<"top">lrt<"bottom">ip'))

  output$inpCategorical234 <- renderDataTable({
    di <- inter_data[['afterfuzzy']]
    lev <- sapply(di[sapply(di, mode) == "character"| sapply(di, class) == "factor"], table)
    lev2<- mapply(length,lev)
    leveleq2 <- lev2[lev2< 5& lev2 > 1] %>% names

    d_cat <- di[leveleq2]

    ta <- cbind(
      `Variable Name`  = colnames(d_cat),
      `Levels` = apply(d_cat, 2, allLevel),
      `Missing No` =  apply(d_cat, 2,function(x) sum(is.na(x))))
    rownames(ta) <- NULL
    ta

  }, escape=FALSE,
  options = list(sDom  = '<"top">lrt<"bottom">ip'))


  ## more than 5
  cat_pre <- reactive({
    di <- inter_data[['afterfuzzy']]
    lev <- sapply(di[sapply(di, mode) == "character"], table)
    lev2<- mapply(length,lev)
    levelgt5 <- lev2[ lev2 > 4] %>% names

    d_cat <- di[levelgt5]
    #d_cat <- di[sapply(di, mode) == "character"]
    d_cat

  })

  cat5re <- reactive({
    d_cat <- cat_pre()
    ta <- cbind(
      `Variable Name`  = colnames(d_cat),
      `Levels` = apply(d_cat, 2, allLevel),
      `Missing No` =  apply(d_cat, 2,function(x) sum(is.na(x))))
    rownames(ta) <- NULL
    as.data.frame(
      cbind(
        ta,
        View = shinyInput(actionButton,
                          nrow(ta),
                          'button_',
                          label = "View",
                          onclick = 'Shiny.onInputChange(\"select_button\",  this.id)' )))

  })

  output$inpCategorical5 <- renderDataTable({
    cat5re()
  },
  selection = 'single',options = list(searching = FALSE,pageLength = 10),server = FALSE, escape = FALSE,rownames= FALSE
  )

  SelectedRow <- eventReactive(input$select_button,{
    as.numeric(strsplit(input$select_button, "_")[[1]][2])
  })

  observeEvent(input$select_button, {
    toggleModal(session, "Catgt5", "open")
  })

  SelectedRowName <- eventReactive(input$select_button,{
    di <- colnames(cat_pre())
    di[SelectedRow()]
  })

  values = reactiveValues(
    datarow = 2222
  )
  values_save = reactiveValues(
    datarow = 2222
  )


  DataRow <- eventReactive(input$select_button,{
    di <- cat_pre()
    tmp <- table(di[,SelectedRow()])
    output_table <- as.data.frame(cbind(rownames(tmp), table(di[,SelectedRow()]), "UNCHANGED"))
    colnames(output_table) <- c("Type","Freq","Target Category")
    rownames(output_table) <- NULL
    output_table

  })

   observeEvent(input$select_button,{
     values[[SelectedRowName()]] = DataRow()
   })

   observeEvent(input$action, {
     updateTextInput(session,"cat_gt5_text", value="")
     mydata <- isolate(values[[SelectedRowName()]] )
     if(input$cat_gt5_text!="" && !is.null(input$datatmp_rows_selected) && input$action>0){
       mydata[,3] <- as.character(mydata[,3])
       mydata[input$datatmp_rows_selected,3] <- input$cat_gt5_text
     }
     values[[SelectedRowName()]] <- mydata
   })

   observeEvent(input$cat_pop_submit, {
     values_save[["aaa"]] <- values[[SelectedRowName()]]

   })

   observeEvent(input$refresh, {
     mydata <- isolate(inter_data[['afterfuzzy']])
     myvar  <- values_save[["aaa"]]
     myvar2 <- myvar[!grepl("UNCHANGED",myvar[,3]),]

     for (i in 1:nrow(myvar2)){
           mydata[mydata[,SelectedRowName()] == myvar2[i,1] & !is.na(mydata[,SelectedRowName()]),SelectedRowName()] <- myvar2[i,3]
       }
     mydata[,SelectedRowName()] = fixchar_col(mydata[,SelectedRowName()])
     inter_data[['afterfuzzy']] <- mydata
   })


  output$datatmp <- renderDataTable(values[[SelectedRowName()]],
                                        extensions = 'Scroller', options = list(deferRender = TRUE,scrollY = 400,scroller = TRUE)
  )

  output$popup <- renderUI({
    bsModal("Catgt5",
            paste0("Variable Names: ",SelectedRowName()), "", size = "large",
            column(12,
                   dataTableOutput("datatmp"),
                   div(style = "position:absolute;right:1em;",
                       actionButton('select_all',  'Select All'),
                       actionButton('select_none', 'Unselect')),
                   textInput("cat_gt5_text", label = h4("Target Categories"), value = " "),
                   actionButton("action", label = "Update"),
                   div(style = "position:absolute;right:5em;",
                       actionButton('cat_pop_submit',  'Save'))
                   )

    )
  })

  proxy = dataTableProxy('datatmp')
  observeEvent(input$select_none, {
    proxy %>% selectRows(NULL)
  })
  observeEvent(input$select_all, {
    proxy %>% selectRows(input$datatmp_rows_current)
  })

  # refresh the table  (butten; cat_refresh)
  ## update the table
  ## clean up Org
  ## ready for download



  ###4 Date                                                              ####
  date_check_data = reactiveValues(
    datarow = 2222
  )

  out <- reactive({
    di <- inter_data[['afterfuzzy']]
    d_date<- di[sapply(di, class) == "Date"]
    out <- cbind(`ID` = di[,as.numeric(input$varID)],d_date)
  }
  )

  observe({
    date_check_data[['table']] = out()
  })


  output$inpDate <- renderDataTable({
    date_check_data[['table']]
  },
  rownames = FALSE,
  selection = list(target = 'column')#,
  #extensions = c('ColReorder', 'Buttons'),
  #options = list( dom = 'Bfrtip', buttons = I('colvis'),
  #                colReorder = TRUE)
  #extensions = c('Buttons'),
  #options = list( dom = 'Bfrtip', buttons = I('colvis'))
  )

  observeEvent(input$date_notshow,{
    di <- date_check_data[['table']]
    var_ids <- input$inpDate_columns_selected + 1
    di <- di[ , -c(var_ids)]
    date_check_data[['table']] <- di
  })

  observeEvent(input$date_exchage,{
    di <- date_check_data[['table']]
    var_ids <- input$inpDate_columns_selected + 1
    a <- sort(var_ids)
    b <- c(a[2],a[1])
    var_id_org <- c(1:ncol(di))
    var_id_exchange <- replace(var_id_org, a, b)
    di <- di[,var_id_exchange]
    date_check_data[['table']] <- di
  })

  observeEvent(input$date_reset,{
    date_check_data[['table']] <- out()
  })

  date_wrong <- eventReactive(input$date_check,{
    di <- date_check_data[['table']]
    n = ncol(di)

    for (i in 2:n){
      varname  <- paste0("col",i)
      di <-
        di %>% mutate(!!varname := 0)
    }

    for (i in 2:(n-1)){
      varname1  <- paste0("col",i)
      varname2  <- paste0("col",i+1)
      di[[varname1]][di[,i] > di[,i+1]] <- 1
      di[[varname2]][di[,i] > di[,i+1]] <- 1
    }

    t <- (di %>% select(starts_with("col")) %>% rowSums > 0 )
    do <- di[t,]
    date_check_data[['show']] <- colnames(di)[2:n]
    date_check_data[['hind']] <- di %>% select(starts_with("col")) %>% colnames()
    date_check_data[['hind_con']] <- c((n+1):length(di))
    do
  })
  #output$date_wrong_out = renderDataTable(date_wrong())

  output$date_wrong_out = renderDataTable({
    datatable(date_wrong(),
              editable = TRUE,
              selection = 'none',
              #rownames = FALSE,
              # Hide logical columns
               options=list(
               columnDefs = list(list(targets=date_check_data[['hind_con']],
                                                   visible=F)),
               sDom  = '<"top">lrt<"bottom">ip')
               ) %>%
  # Format data columns based on the values of hidden logical columns
  formatStyle(columns = date_check_data[['show']],
              valueColumns = date_check_data[['hind']],
              color = styleInterval(0, c('black','orange')),
              backgroundColor = styleInterval(0, c( 'white','gray'))
  )
  }
  )


  ###5 afterclean                                                              ####
  output$afterclean <- renderDataTable({
    di <- outD()
    di
  },
  selection = list(target = 'column'),
  rownames= FALSE)

  output$wide <- renderDataTable({
    di <- outD()
    var_id <- input$afterclean_columns_selected + 1
    id_name  <- colnames(di)[as.numeric(input$varID)]
    Dwild <- long2wide(di, ID = id_name , var_base = di[,var_id] %>%  colnames())
    Dwild
  },
  rownames= FALSE)



  output$inpSummary <- renderText({
    d1 <- inpD()
    rbind(c("Number of Patient:", length(d1[,1])))

  })


  output$table1 <- renderDataTable({
    di=inter_data[['afterfuzzy']]
    table1out <- TheTable1(di)
    table1out
  }, rownames= FALSE,
  extensions = 'Scroller', options = list(
    deferRender = TRUE,
    scrollY = 400,
    scroller = TRUE)
  )

  output$downloadData <- downloadHandler(
    filename = function() {
      "processed.csv"
    },
    content = function(file) {
      write.csv(inter_data[['afterfuzzy']],file, row.names = FALSE)
    }
  )


}
