# 
function(input, output, session) { 
  ###MDLT V3
  
  #Parcel Selection page------------
  options(shiny.maxRequestSize = 1050*1024^2)
  
  #Uploading file: Reads-in shapefile function
  # Read_Shapefile<-function(shp_path) {
  #   infiles<- shp_path$datapath #gets the location of files
  #   dir<- unique(dirname(infiles)) #gets the directory
  #   outfiles<- file.path(dir, shp_path$name) #creates new path name
  #   name<- strsplit(shp_path$name[1], "\\.")[[1]][1] #strip name
  #   purrr::walk2(infiles, outfiles, ~file.rename(.x, .y)) #rename files
  #   x<- read_sf(file.path(dir, paste0(name, ".shp"))) #read-in shapefile
  #   return(x)
  # }
  # 
  ##Working code for cutting shape of file from Study Area 
  observeEvent(input$printParcels2, {
    req(input$shp)  # Ensure that a shapefile is uploaded
    
    # Attempt to read the shapefile safely
    tryCatch({
      uploaded_shapefile <- vect(input$shp$datapath[1])
      #print(class(uploaded_shapefile))
      
      # Check if the CRS is set and transform if necessary
      if (!is.null(st_crs(uploaded_shapefile)) && st_crs(uploaded_shapefile) != st_crs(parcels.shp)) {
        #uploaded_shapefile <- st_transform(uploaded_shapefile, 'epsg:26911')
        uploaded_shapefile <- project(uploaded_shapefile, crs=crs('epsg:26911'))
        
      }
      
      # Perform the spatial operation to cut the shape
      if (!is.null(uploaded_shapefile) && nrow(uploaded_shapefile) > 0) {
       # miniparcels.utm <- st_intersection(parcels.shp,uploaded_shapefile)
        miniparcels.utm <<- intersect(parcels.shp,uploaded_shapefile)
        #miniparcels.utm <<- vect(miniparcels.utm)
        print('ready for new tab')
        newtab<- switch(input$tabs,
                        "parcels"= "data")
        updateTabItems(session, "tabs", newtab)
      } else {
        print("Uploaded shapefile is empty or invalid.")
      }
    }, error = function(e) {
      print(paste("Error reading shapefile:", e$message))
    })
  })
  
  
  #Leaflet map
  output$m<-renderLeaflet({
    leaflet(map) %>%
      addPolygons(fillOpacity = 0) %>%
      addTiles() %>%
      addDrawToolbar(targetGroup='drawGroup',
                     polylineOptions=F,
                     markerOptions= F,
                     circleOptions= F,
                     circleMarkerOptions = F,
                     editOptions = editToolbarOptions(edit=TRUE))
  })
  
  observeEvent(input$toggleDraw, {
    zzz<-  leafletProxy("m") %>%
      toggleGroup("drawGroup")
  })
  
  observeEvent(input$printParcels, {
    #print("in parcels")
    #  req(input$m)        # Ensure that a shape is chosen
    # req(input$m_draw_new_feature)
    
    draw.data<- input$m_draw_new_feature
    # print(str(draw.data))
    mycoords <- matrix(c(unlist(lapply(draw.data$geometry$coordinates[[1]],`[`,1)),unlist(lapply(draw.data$geometry$coordinates[[1]],`[`,2))), ncol=2, byrow = F)
    #print(mycoords)
    #Rest of working code for cutting shape
    drawn.shape.sf<- sf::st_sf(geometry= sf::st_sfc(sf::st_polygon(list(mycoords))), crs='epsg:4326')
    
    drawn.shape.sf <- st_transform(drawn.shape.sf,'epsg:26911')
    # print(st_crs(drawn.shape.sf))
    drawn.shape.v <- vect(drawn.shape.sf)
    #miniparcels.utm<-st_intersection(parcels.shp,drawn.shape.sf)
    print('intersecting')
    miniparcels.utm <<-terra::intersect(parcels.shp,drawn.shape.v)
    print('intersect complete')
    #miniparcels.utm <<- vect(miniparcels.utm)
    #print("done w miniparcels")
    newtab<- switch(input$tabs,
                    "parcels"= "data")
    updateTabItems(session, "tabs", newtab)
    
  })
  
  #Action button to go from Parcel Selection Page to Data Selection Page
  # observeEvent(input$switchtab, {
  #   newtab<- switch(input$tabs,
  #                   "parcels"= "data")
  #   updateTabItems(session, "tabs", newtab)
  # })
  
  # Data Selection page--------------
  output$rasterOptions <- renderUI({
    optionsList <- lapply(unique(datatab$category), function(category) {
      categoryData <- datatab[datatab$category == category, ]
      
      conditionalPanel(
        condition = paste0("input.categorySelector == '", category, "'"),
        tagList(
          lapply(1:nrow(categoryData), function(i) {
            rasternum <- categoryData$id[i]
            rasterName <- categoryData$Layer.name[i]
            description <- categoryData$Description[i]
            inputId <- paste0("classification_", rasternum)
            
            tags$div(
              rasterName,
              bsButton(paste0("popoverBtn_", rasternum), icon("info-circle", lib = "font-awesome"), style = "default", size = "extra-small", class = "btn-gray"),
              radioButtons(
                inputId = inputId,
                label = NULL,
                inline = T,
                choiceNames = c("Positive Layer", "Negative Layer", "NA"),
                choiceValues = c(1, 2, 3),
                selected = 3
              ),
              bsPopover(id = paste0("popoverBtn_", rasternum), title = "Description", content = description, placement = "right")
            )
          })
        )
      )
    })
    
    categorySelector <- selectInput("categorySelector", "Select Category", choices = unique(datatab$category))
    tagList(categorySelector, do.call(tagList, optionsList))
  })
  
  
  
  # Function to get the selected classification for each raster
  getSelectedClassifications <- reactive({
    classifications <- lapply(raster_data$RasterName, function(rasterName) {
      inputId <- paste0("classification_", rasterName)
      inputValue <- input[[inputId]]
      list(rasterName = rasterName, classification = inputValue)
    })
    classifications
  })
  
  
  #parcel layer selection (old code?)
  #  output$out3<- renderPrint(input$in3)
  
  
  
  #Action button (old code?)
  #?  output$value<- renderPrint({input$action})
  
  scaledmodel <- reactiveVal()  # Define scaledmodel as a reactiveVal
  
  
  ##Model Weight page----------------  
  observeEvent(input$makemodel, {    # if(input$TDIinv ==T){
    print('fixing params')
    
    model_params <- paramz()
   # print(model_params)
    # 
    model_params <- list.rbind(model_params)
    totlayerz <- numpos$data + numneg$data
    model_params <- model_params[1:totlayerz,]
    # print('params df')
    # print(model_params)
     paramz(model_params)
    
     print('making model')
    
    # }
    aside.r <- rast(prw_data())
   print('a side is ')
   print(aside.r)
    ### "b" side 
   # bside <- list()
    # nrw_data <- nrw()
    # for(i in 1:numneg$data){
    #   bside[[i]] <- nrw[[i]]
    # }
    bside.r <- rast(nrw_data())
    print('b side is ')
    print(bside.r)
    # ## weighted calculation of Positive-Negative Layers--------
    a<- tr.rescale(sum(aside.r, na.rm = T), 0, 1) 
    print('a is ')
print(a)
    b<- tr.rescale(sum(bside.r, na.rm=T), 0, 1) 
    print('b is ')
print(b)
    scaledmodel(a - b)  # Set the value of scaledmodel using reactiveVal
    # print(class(scaledmodel))
    # dev.off()
    # plot(scaledmodel)
    # print('plot complete')
    # b<- tr.rescale(TDI.r*TDIwt2 + Transmissionlines.r*Transmissionlineswt2, 0, 1) 
    # 
    # a-b 
    updateTabItems(session, "tabs", selected = "parcel")
    
  })
  
  ##Results page--------------------  
  rastervals <- reactive({
    this.r <- scaledmodel()
    samplez <- spatSample(this.r, 1500, method = 'random')
    samplez <- data.frame(x=samplez[!is.na(samplez)])
  })
  
  selectedData <- reactive({
    
    
    this.r <- scaledmodel()
    miniparcels.utm$ModelScore <- round(terra::extract(this.r, miniparcels.utm, fun = mean, na.rm = T)[,2],1)
    miniparcels.utm$Selected <- 0
    miniparcels.utm$Selected[miniparcels.utm$ModelScore > input$sitewt] <- 1
    miniparcels.utm$SelectedF <- factor(ifelse(miniparcels.utm$Selected == 1,'Yes','No'))
    miniparcels.utm
  }) 
  
  # Input data from Shape parameters used to build the graph----------------
  
  
  ### betadata function #####
  betadata <- function(prefix, xx) {
    # for(xx in 1:x){
    
    #print('in betadata')
    bdparamlist <- paramz()
    #print(str(bdparamlist))
    #print(bdparamlist[[1]]$wt)
    pbweight <- input[[paste0(prefix,'_weight_',xx)]]
    pblevelz <- input[[paste0(prefix,'_LevelzInput_',xx)]]
    pbshape1 <-  sdf$s1[sdf$l == pblevelz]
    pbshape2 <-  sdf$s2[sdf$l == pblevelz]
    # pbshape1 <- input[[paste0(prefix,'_shape_input1_',xx)]]
    # pbshape2 <- input[[paste0(prefix,'_shape_input2_',xx)]]
   # pbinv <- input[[paste0(prefix, "_inverse_", xx)]]
   
     if(prefix == 'pos'){
    bdparamlist[[xx]]$wt <-  pbweight
    bdparamlist[[xx]]$levelz <-  pblevelz
   # bdparamlist[[xx]]$s2 <-  pbshape2
    #bdparamlist[[xx]]$inv <-  pbinv
    } else {
      bdparamlist[[xx+numpos$data]]$wt <-  pbweight
      bdparamlist[[xx+numpos$data]]$levelz <-  pblevelz
      
      # bdparamlist[[xx+numpos$data]]$s1 <-  pbshape1
      # bdparamlist[[xx+numpos$data]]$s2 <-  pbshape2
      # bdparamlist[[xx+numpos$data]]$inv <-  pbinv
      
    }
    paramz(bdparamlist)
    print('bdparamlist')
    print(bdparamlist)
    print('paramz done')
    # print(paste0('inbetadata iteration- ', xx))
    # print(paste0('weight is ', pbweight))
    # print(paste0('shape1 is- ', pbshape1))
    # print(paste0('shape1 is- ', pbshape1))
    # 
    # Compute pb based on your logic
    ### old section here 
    # if(input[[paste0(prefix, "_inverse_", xx)]]) {
    #   pb <- 1 - pbeta(plotseq, pbshape1, pbshape2) * pbweight
    # } else {
    #   pb <- pbeta(plotseq, pbshape1, pbshape2) * pbweight
    # }
    #    }
    # Return pb
    ## end old section
    print('pblevelz is ')
    print(pblevelz)
    if(pblevelz ==1){
      pb = plotseq * pbweight
    } else if(pblevelz ==7){
      pb = (-plotseq+1) * pbweight
    } else {
      # shp1 <- sdf$s1[sdf$l == input$levelz]
      # shp2 <- sdf$s2[sdf$l == input$levelz]
      
      pb <- rescale(dbeta(plotseq, shape1=pbshape1, shape2=pbshape2),c(0,1))* pbweight
    }
    return(pb)
  }
  
  terra.pbeta <- function(r,prefix, xx){
    print(' in terra pbeta')
    print(paste('class r is ', class(r)))
   rmax <- as.numeric(global(r, 'max', na.rm=T))
   print(paste('rmax is', rmax))
  if(is.nan(rmax)){
      print('it is nan')
      r[is.na(r)] <- 0
  }
   
    print(r)
    w <- input[[paste0(prefix,'_weight_',xx)]]
    levelz <- input[[paste0(prefix,'_LevelzInput_',xx)]]
    rshape1 <-  sdf$s1[sdf$l == levelz]
    rshape2 <-  sdf$s2[sdf$l == levelz]
    # s1 <- input[[paste0(prefix,'_shape_input1_',xx)]]
    # s2 <- input[[paste0(prefix,'_shape_input2_',xx)]]
    # i1 <- input[[paste0(prefix, "_inverse_", xx)]]
    print('terra.pbeta levelz')
    print(levelz)
    if(levelz ==1 ){
      tmp.r = r * w
    } else if(levelz ==7){
      tmp.r = 1 - r * w
    } else {
      # shp1 <- sdf$s1[sdf$l == input$levelz]
      # shp2 <- sdf$s2[sdf$l == input$levelz]
      
      tpb <- dbeta(values(r)[,1], rshape1, rshape2)*w
      tmp.r <- r
      values(tmp.r) <- tpb
      
    }
   
    # if(i1 == 1){
    #   tmp.r <- 1-tmp.r
    # }
    return(tmp.r)
  }
  
  ntdf <- reactiveValues(data = NULL)
  numpos <- reactiveValues(data = NULL)
  numneg <- reactiveValues(data = NULL)
  pos.rstrz <-  reactiveValues(data = NULL)
  neg.rstrz <-  reactiveValues(data = NULL)
  pos.rstrz.obj <-  reactiveValues(data = NULL)
  neg.rstrz.obj <-  reactiveValues(data = NULL)
  pos.rstrz.src <-  reactiveValues(data = NULL)
  neg.rstrz.src <-  reactiveValues(data = NULL)
  tmprsts <- reactiveVal(NULL)
  paramz <-  reactiveVal(list(NULL))
  rasterobjects <-  reactiveValues(data = NULL)
  
  #Action button for Data Selection page to move into Model Weights page-------
  observeEvent(input$goButton, {
    # print("Inside observeEvent - goButton")
    # print(dim(datatab))
    # lapply(1:42, function(x) print(input[[paste0("classification_",x)]]))
    ntdf$data <- data.frame(
      id = datatab$id,
      raster = datatab$Layer.name,
      raster.obj = datatab$Filename,
      raster.source = datatab$source,
      class = sapply(1:dim(datatab)[1], function(i) input[[paste0("classification_", datatab$id[i])]])
      
    )
    #  print('ntdf class check')
    #  print(head(ntdf$data$class))
    # 
    #   print("somebody pressed go")
    numpos$data <- sum(ntdf$data$class == 1)
    numneg$data <- sum(ntdf$data$class == 2)
    pos.rstrz$data <- ntdf$data$raster[ntdf$data$class ==1]
    neg.rstrz$data <- ntdf$data$raster[ntdf$data$class ==2]
    pos.rstrz.obj$data <- ntdf$data$raster.obj[ntdf$data$class ==1]
    neg.rstrz.obj$data <- ntdf$data$raster.obj[ntdf$data$class ==2]
    
    pos.rstrz.src$data <- ntdf$data$raster.source[ntdf$data$class ==1]
    neg.rstrz.src$data <- ntdf$data$raster.source[ntdf$data$class ==2]
    rasterstoload <- c(pos.rstrz.src$data,neg.rstrz.src$data)
    rasterobjects <- c(pos.rstrz.obj$data,neg.rstrz.obj$data)
   
    print('making paramz')
    print(numpos$data)
    print(pos.rstrz.obj$data)
    print(numneg$data)
    print(neg.rstrz.obj$data)
    
    paramz_list <- list()
    for(i in 1:(numpos$data)){
      paramz_list[[i]] <- data.frame(raster = pos.rstrz.obj$data[i], class = 'pos', wt=0, s1 = 0, s2 = 0, inv=99)
    }
    for(i in (numpos$data + 1):(numpos$data+numneg$data)){
      print('line 312')
      print(i)
      paramz_list[[i]] <- data.frame(raster = neg.rstrz.obj$data[i-numpos$data], class = 'neg', wt=0, s1 = 0, s2 = 0, inv=99)
    }
    
    paramz(paramz_list)
    print('paramz first build')
    print(paramz_list)
    # print('rasters to load') 
    # print(rasterstoload)
    # print('rasters to load w path') 
    # print((paste0(rasterpath,rasterstoload)))
    # print(rasterobjects)
    # 
    # #  load Rasters
    #print('Loading Rasters')
    #print(dim(neg.rstrz.obj$data))
    
    # print(dim(neg.rstrz.src$data))
    tmprsts(rast(paste0(rasterpath,rasterstoload)))
    
    
    #print(tmprsts())
    #print(names(tmprsts()))
    tmprsts_data <- tmprsts()
    names(tmprsts_data) <- rasterobjects # reassign names
    
    #crop to miniparcels
    tmprsts_data <- crop(tmprsts_data, miniparcels.utm)
    #print(names(tmprsts_data))
    tmprsts(tmprsts_data)
    globaltmprsts(tmprsts_data)
    # names(tmprsts()) <- rasterobjects
    
    #print(names(tmprsts()))
    #print(tmprsts$data)
    # print(tmprsts)
    
    updateTabItems(session, "tabs", selected = "weights")
    # print(paste('postitive rasters',pos.rstrz.obj$data))
    # print(paste('negative rasters',neg.rstrz.obj$data))
    
    #  print(paste("numpos$data:", numpos$data))
    # print(paste("numneg$data:", numneg$data))
  })
  
  ##End of action button on Data Selection page 
  output$environmentrasters <- renderText({
    # Access the globaltmprsts reactive value here
    # For example:
    tmprsts_data <- tmprsts()
    tmprsts_names <- (names(tmprsts_data))
    print('environmental')
    print(tmprsts_names)
    # Perform any necessary operations on myGlobalValue
    # Return the value to be displayed in the text output
    return(tmprsts_names)
  })
  # 
  # observeEvent({
  #   # Get the values from output$environmentrasters
  #   print('in obeserve tmprsts names')
  #   print(output$environmentrasters())
  #   tmprsts_names <- strsplit(output$environmentrasters(), ", ")[[1]]
  #   
  #   # Update the choices of the dropdown
  #   updateSelectInput(session, "whichlayer", choices = tmprsts_names)
  # })
  tmprsts_names <- reactive({
    req(tmprsts())  # Make sure 'tmprsts' is not NULL
    names(tmprsts())
  })
  observe({
    updateSelectInput(session, "whichlayer", choices = tmprsts_names())
  })
  ##Back to model weights page----------------
  generateDynamicRows <- function(x, prefix) {
    # Initialize an empty list to store UI elements
    optionsList <- list()
    
    if(prefix == 'pos'){
      rn <- pos.rstrz$data
      header.main <- 'Positive influences'
    }
    if(prefix == 'neg'){
      rn <- neg.rstrz$data
      header.main <- 'Negative influences'
    }
    #optionsList$posnegtitle <- div(class = "item-header",header.main)
    #boldTitle <- tags$h3(header.main, style = "font-weight: bold;")
    optionsList$posnegtitle <- tags$h3(header.main, style = "font-weight: bold;")
    
    # Loop through the number of rows and generate UI elements dynamically
    for (i in 1:x) {
      # Generate input IDs for each element
      sliderId <- paste0(prefix, "_weight_", i)
      LevelzInput <-  paste0(prefix, "_LevelzInput_", i)
      #shapeInputId1 <- paste0(prefix, "_shape_input1_", i)
      #shapeInputId2 <- paste0(prefix, "_shape_input2_", i)
      #checkboxId <- paste0(prefix, "_inverse_", i)
      # Create UI elements for each row
      
      #  rn <- get(paste0(prefix,'.rstr.namez$data'))
      sliderblocktitle <-  tags$h4( rn[i], style = "font-weight: bold;") ### Here I need to carry the raster selected 
      sliderInput <- div(style="height: 90px; margin-left: 20px;", sliderInput(inputId = sliderId, label = paste("Weight", i), min = 0, max = 1, value = 1))
      sliderInput2 <-  div(style="height: 90px; margin-left: 20px;", sliderInput(inputId = LevelzInput, label = paste("Shape Input", i), min = 1, max = 7,  value = 1))
     # shapeInput1 <-  div(style="height: 90px; margin-left: 20px;", sliderInput(inputId = shapeInputId1, label = paste("Shape Input 1", i), min = 0, max = 5,  value = 5))
      #shapeInput2 <-  div(style="height: 90px; margin-left: 20px;", sliderInput(inputId = shapeInputId2, label = paste("Shape Input 2", i), min = 0, max = 5,  value = 5))
      #checkboxInput <-  div(style="height: 90px; margin-left: 20px;", checkboxInput(inputId = checkboxId, label = paste("Inverse", i), value = FALSE))
      
      # Append UI elements to the optionsList
      optionsList[[i+1]] <- tagList( # adding one for posnegtitle in front
        sliderblocktitle,
        sliderInput,
        sliderInput2
        # shapeInput1,
        # shapeInput2,
        # checkboxInput
      )
    }
    
    # Return the list of UI elements as a tagList
    return(do.call(tagList, optionsList))
  }
  
  
  output$positives <- renderUI({
    generateDynamicRows(numpos$data, 'pos')
  })
  
  output$negatives <- renderUI({
    generateDynamicRows(numneg$data, 'neg')
  })
  
  
  
  
  # output$show_state2 <- renderText({input[['classification_1']]})  
  output$show_state2 <- renderText({input[['classification_1']]})  
  # output$show_state2 <- renderText({input$classification_1})  
  # output$show_state <- renderText({names(input)})  
  #output$show_state <- renderText({input$classification_Roads})  
  #output$show_state <- renderTable({datatab[datatab$trial ==1,]})  
  output$show_beta <- renderText({input[[pos_weight_1]]})
  output$show_state <- renderTable({
    req(ntdf$data)
    ntdf$data
  })
  
  
  #Output plots--------------------------------
  
  #Output plot on Parcel Selection page  
  output$plot1 <- renderPlot({
    # browser()
    this.r <- scaledmodel()
    #   print('plotting model')
    plot(this.r, main = 'Weighted Model')
    plot(selectedData(), border = twocol[as.factor(selectedData()$Selected)], background=NA, lwd = 0.5, add=T)
    print(dim(selectedData()[selectedData()$Selected ==1,]))
    
  })
  
  output$modelp1<-renderLeaflet({
    this.r <- scaledmodel()
    this.r <- project(this.r, crs('epsg:4326'))
    this.r <- raster::raster(this.r)
    #print(class(this.r))
    
    text <- paste0("<b>Parcel_Num %s</b>")
    theseparcels.sf <- st_as_sf(selectedData())
    theseparcels.sf <- st_transform(theseparcels.sf, 'epsg:4326')
    pal.at <- seq(0,1,by=0.1)
    pal.leg <- colorBin("Spectral",values(this.r), bins = pal.at)
    #leaflet(theseparcels.sf) %>%
    if(input$showparcels ==T){
    leaflet(theseparcels.sf) %>%
      addTiles()%>%
      addRasterImage(this.r, opacity = 0.6)  %>%
      addPolygons(fillOpacity = 0,popup = ~sprintf(   text, htmlEscape(Parcel_Num)), weight=1, color =  twocol[as.factor(selectedData()$Selected)]) %>%
      addLegend(pal=pal.leg, values= values(this.r))
    } else{
      leaflet(theseparcels.sf) %>%
        addTiles()%>%
        addRasterImage(this.r, opacity = 0.6)  %>%
        #addPolygons(fillOpacity = 0,popup = ~sprintf(   text, htmlEscape(Parcel_Num)), weight=1, color =  twocol[as.factor(selectedData()$Selected)]) %>%
        addLegend(pal=pal.leg, values= values(this.r))
      }
  })
  
  #Output Histograms on DIAGNOSTIC PAGE
  output$plot2 <- renderPlot({
    ggh <- ggplot() + geom_histogram(data=as.data.frame(selectedData()), aes(x=ModelScore, fill = SelectedF)) + xlab('Model Score') + ylab('Count') + ggtitle('Selected Parcels') +labs(fill='Selected')
    gg_b <- ggplot_build(
      ggplot() + geom_histogram(aes(x = rastervals()[,1]), binwidth=.1)
    )
    nu_bins <- dim(gg_b$data[[1]])[1]    
    gmh <- ggplot() + geom_histogram(data=rastervals(), aes(x=x, fill = ..x..))+ggtitle('Model Scores') + xlab('Model Score') + ylab('Count') + scale_fill_viridis_c(direction=-1, option = "turbo")+labs(fill='Parcel Value')
    
    ggh + gmh
  })
  
  #Output plot on Environmental Layers page
  output$plot3 <- renderLeaflet({0
    # browser()
    #print('in plot 3')
     req(tmprsts())
    tmpp2 <- tmprsts()
    # print(tmpp2)
    thisr <- tmpp2[input$whichlayer]
   # print(input$whichlayer)
   # thisraster <- ()
   # plot(thisr, main = input$whichlayer, zlim = c(0,1))
  #  plot(selectedData(), border = twocol[as.factor(selectedData()$Selected)],
   #      background=NA, lwd = 0.5, add=T)  
    text <- paste0("<b>Parcel_Num %s</b>")
    theseparcels.sf <- st_as_sf(selectedData())
    theseparcels.sf <- st_transform(theseparcels.sf, 'epsg:4326')
    leaflet(theseparcels.sf) %>%
      addTiles()%>%
      addRasterImage(thisr, opacity = 0.6)  %>%
      addPolygons(fillOpacity = 0,popup = ~sprintf(   text, htmlEscape(Parcel_Num)), weight=1, color =  twocol[as.factor(selectedData()$Selected)])
  })
  
  
  
  output$betashapeplotpos <- renderPlot({
    print("Inside renderPlot - betashapeplotpos")
    # Create an empty list to store plots
    plot_list <- list()
    zz <- numpos$data
    posplotsize <<- paste0(zz*100+1000,'px')
    
    #print("posplot size is ",posplotsize)
    # Iterate through positive inputs and compute betadata for each
    for (i in 1:zz) {
      # print(paste("Inside loop - i:", i))
      betadata_plot <- betadata('pos', i) # Compute betadata
      #print(str(betadata_plot)) # Check the structure and content of betadata_plot
      bpdf <- data.frame(b.x = plotseq, b.y = betadata_plot)
      
      thisplot.title <- pos.rstrz$data[i]
      
      #   print(head(bpdf))
      # Store the plot in the list
      options(repr.plot.width =3, repr.plot.height =3) 
      plot_list[[i]] <- ggplot(bpdf, aes(x=b.x, y=b.y)) + 
        geom_line() +
        xlab('value') + ylab('weight') + xlim(0, 1) + ylim(0, 1) +
        ggtitle(thisplot.title)
      #  print(str(plot_list[[i]])) # Check the structure and content of plot_list
    } 
    # Arrange plots in a grid
    plot_grid(plotlist = plot_list, ncol = 1)
  })
  
  
  
  output$betashapeplotneg <- renderPlot({
    # print("Inside renderPlot - betashapeplotneg")
    
    # Create an empty list to store plots
    plot_list_neg <- list()
    zzz <- numneg$data
    negplotsize <<- zzz*100+1000
    # Iterate through positive inputs and compute betadata for each
    for (i in 1:zzz) {
      # print(paste("Inside neg loop - i:", i))
      betadata_plot_n <- betadata('neg', i) # Compute betadata
      #print(str(betadata_plot)) # Check the structure and content of betadata_plot
      bpdfn <- data.frame(b.x = plotseq, b.y = betadata_plot_n)
      thisplot.title <- neg.rstrz$data[i]
      # Store the plot in the list
      options(repr.plot.width =3, repr.plot.height =3)
      plot_list_neg[[i]] <- ggplot(bpdfn, aes(x=b.x, y=b.y)) + 
        geom_point() +
        xlab('value') + ylab('weight') + xlim(0, 1) + ylim(0, 1) +
        ggtitle(thisplot.title)
      #  print(str(plot_list_neg[[i]])) # Check the structure and content of plot_list
    } 
    # Arrange plots in a grid
    plot_grid(plotlist = plot_list_neg, ncol = 1)
    #print(head(datatab))
    
  })
  
  #prw_list <- reactiveVal(list())
  nrw <- reactiveVal(NULL)
  prw_data <- reactiveVal(list(NULL))
  nrw_data <- reactiveVal(list(NULL))
  
  
  #Raster graphs
  output$minirasterzpos <- renderPlot({
    npz <- numpos$data
    pos.raster.weighted <- list()
    tmprsts_data <- tmprsts()
    prw_data_loader <-list()
    for(rr in 1:npz){
      thisrast <- tmprsts_data[pos.rstrz.obj$data[rr]]
      thisrast.title <- pos.rstrz$data[rr]
      thisrast.weighted <- terra.pbeta(thisrast,'pos',rr)
       
      #prw_data_loader <- prw_data()  # Get the current value of prw_list
       prw_data_loader[[rr]] <- thisrast.weighted  # Assign the value to the correct index
       #prw_data(prw_data_loader)
       
       #print(str(prw_data_loader))
      
      lp <- levelplot(thisrast.weighted, margin=F, col.regions = rev(terrain.colors(20)),main= thisrast.title, at=zat)
      pos.raster.weighted[[rr]] <- lp
    }
    prw_data(prw_data_loader)
    plot_grid(plotlist = pos.raster.weighted, ncol = 1)
  })
  
  output$minirasterzneg <- renderPlot({
    nng <- numneg$data
    print(paste('number of negatives is ',nng))
    neg.raster.weighted <- list()
    tmprsts_data <- tmprsts()
    nrw_data_loader <-list()
    
    for(ss in 1:nng){
      # print(paste('assigning negatives number ',ss))
      #  print(neg.rstrz.obj$data[ss])
      thisrast_n <- tmprsts_data[neg.rstrz.obj$data[ss]]
      # print(paste("un weighted", thisrast_n))
      thisrast.title_n <- neg.rstrz$data[ss]
      thisrast.weighted_n <- terra.pbeta(thisrast_n,'neg',ss)
      nrw_data_loader[[ss]] <- thisrast.weighted_n  # Assign the value to the correct index
      # nrw_data <- nrw()  # Get the current value of prw
      # nrw_data[[rr]] <- thisrast.weighted_n  # Assign the value to the correct index
      # nrw(nrw_data)     #  print(nrw[[ss]])
      #  print(paste("weighted", thisrast.weighted_n))
      lp <- levelplot(thisrast.weighted_n, margin=F, col.regions = rev(terrain.colors(20)),main= thisrast.title_n, at=zat)
      neg.raster.weighted[[ss]] <- lp
    }
    nrw_data(nrw_data_loader)
    
    plot_grid(plotlist = neg.raster.weighted, ncol = 1)
    
  })
  
  
  ##Downloads for Parcel Selection Page-----------------------
  
  output$dls <- downloadHandler(
    # filename = function() {
    #   "Selected_Sites.zip"
    # },
    filename = function() {
      paste0("Selected_Sites_", Sys.Date(), ".gpkg")
    },
    content = function(file) {
      data <- selectedData() # selected data from model
      writeVector(data, file, overwrite = TRUE)   
      }
    # content = function(file) {
    #   data <- selectedData() # selected data from model
    #   
    #   # Create a temporary directory to store shapefiles
    #   temp_dir <- tempdir()
    #   #print(paste("Temporary directory:", temp_dir))
    #   
    #   # Write shapefiles to the temporary directory
    #   gpkg_file <- file.path(temp_dir, "Selected_Sites.gpkg")
    #   writeVector(data, gpkg_file, overwrite = TRUE)
    #   print(paste("File written to:", gpkg_file))
    #   
    #   # Zip shapefile directory
    #   zip_file <- file.path(temp_dir, "Selected_Sites.zip")
    #   zip::zipr(zip_file, gpkg_file)
    #   print(paste("Zip file created at:", zip_file))
    #   
    #   # Copy the zip file to the specified file
    #   file.copy(zip_file, file)
    #   print(paste("Zip file copied to:", file))
    #   
    #   # Remove temporary files
    #   # Comment out this line temporarily for debugging
    #   # unlink(temp_dir, recursive = TRUE)
    # }
  )
  
  
  output$dlm <- downloadHandler(
    r <-   scaledmodel(),
    filename = function() {
      paste('Model-', Sys.Date(), ".tif", sep="")
    },
    content = function(file) {
      writeRaster(r, file)
    }
  )

  
  
  output$dlp <- downloadHandler(
    
    filename = function() {
      paste('Parameters-', Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(paramz(), file, row.names = F)
    }
  )
}

