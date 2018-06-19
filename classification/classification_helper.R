############################################################
#
# by Dominique Weber, HAFL, BFH
# 
############################################################

library(raster)
library(rgdal)

############################################################
# Create stack list: One stack per date
get_stack_list <- function(stack_path, stack_dates, layer_names, layer_indices) {
  
  # prepare stackList
  stackList = list(
    stacks = vector(),
    dates = vector(),
    bands = vector()
  )
  
  # filter .tif files
  fileNames = list.files(stack_path)
  fileNames = fileNames[grep("tif$", fileNames)]
  
  # load stacks and add them to stacklist
  lapply(stack_dates, function(date) {
    tmp_stk <-  stack(paste(
      stack_path, 
      fileNames[grep(date, fileNames)][[1]],
      sep=""
    ))
    
    stackList$stacks        <<- c(stackList$stacks, tmp_stk)
    stackList$dates  <<- c(stackList$dates, date)
    stackList$bands <<- layer_names
  })
  
  # check if stackList.lenght == dates.lenght
  try(if (length(stackList$stacks) != length(stackList$dates)) 
    stop("length of stack not equal to lenght of datess") 
  )
  
  # apply names and select bands
  for(i in 1:length(stackList$stacks)) {
    # select relevant bands
    stackList$stacks[[i]] = stackList$stacks[[i]][[layer_indices]]
    # name stacks
    names(stackList$stacks[[i]]) = c(layer_names)
  }
  return(stackList)
}

############################################################
# Extract values from raster stack list at given spatial locations
extract_time_series <- function(stack_list, sp, response_variable) {
  # load multi core libraries
  library(foreach)
  library(doParallel)
  
  # get band names
  band_names = stack_list$bands
  
  # prepare multi-core
  print("Prepare multi-core processing...")
  start_time <- Sys.time()
  cl = makeCluster(detectCores() -1)
  registerDoParallel(cl)

  # start raster value extraction
  df <- data.frame()
  for(i in 1:length(stack_list$stacks)) {
    print(paste("Start processing stack", i, "of", length(stack_list$stacks)))
    stk <- stack_list$stacks[[i]]
    colStart <- stack_list$dates[[i]]
    
    # extract spectral values and calculate VIs
    df_tmp = foreach(j=1:length(sp), .packages = c("raster"), .combine = "rbind") %dopar% {
      extract(stk, sp[j,], fun=median, na.rm=T, df=T)[,-1]
    }
    df_tmp$NDVI = (df_tmp$B8 - df_tmp$B4)/(df_tmp$B8 + df_tmp$B4)
    df_tmp$IRECI = (df_tmp$B7-df_tmp$B4)/(df_tmp$B5/df_tmp$B6)
    df_tmp$NDI45 = (df_tmp$B5-df_tmp$B4)/(df_tmp$B5+df_tmp$B4)
    df_tmp$MCARI = ((df_tmp$B5-df_tmp$B4)-0.2*(df_tmp$B5-df_tmp$B3))*(df_tmp$B5-df_tmp$B4)
    df_tmp$GNDVI = (df_tmp$B7-df_tmp$B3)/(df_tmp$B7+df_tmp$B3)
    
    # add date to colname
    colnames(df_tmp) = paste(colStart, colnames(df_tmp), sep = "_")
    
    # copy target variable only in first subset
    if (i==1) {
      df_tmp = cbind(sp@data[,response_variable], df_tmp)
      colnames(df_tmp)[1] = response_variable
      df = df_tmp
    } else {
      df = cbind(df,df_tmp)
    }
  }
  
  #end cluster
  stopCluster(cl)

  print(paste("processing time:", Sys.time() - start_time))
  return(df)
}

############################################################
# Create prediction data set
create_prediction_stack <- function(stack_list, extent) {
  # CREATE RASTER STACK FOR PREDICTION
  start_time <- Sys.time()
  
  # prepare data
  print("Prepare raster data for prediction")
  predictionStack <- NA
  
  for(i in 1:length(stack_list$stack)) {
    print(paste("Creating stack for prediction:", i, "of", length(stack_list$stack)))
    
    pred <- stack_list$stacks[[i]]
    colStart <- stack_list$dates[[i]]
    names(pred) = stack_list$bands
    
    # crop
    print("cropping...")
    pred = crop(pred, extent)
    
    # calculate vegetation indices
    print("calculate VIs...")
    pred$NDVI = (pred$B8 - pred$B4)/(pred$B8 + pred$B4)
    pred$IRECI = (pred$B7-pred$B4)/(pred$B5/pred$B6)
    pred$NDI45 = (pred$B5-pred$B4)/(pred$B5+pred$B4)
    pred$MCARI = ((pred$B5-pred$B4)-0.2*(pred$B5-pred$B3))*(pred$B5-pred$B4)
    pred$GNDVI = (pred$B7-pred$B3)/(pred$B7+pred$B3)
    
    # add prefix to every column
    names(pred) = paste(colStart, names(pred), sep = "_")
    #add stack to final stack
    if (i == 1) 
      predictionStack = pred
    else
      predictionStack = stack(predictionStack, pred)
  }
  print(paste("processing time:", Sys.time() - start_time))

  return(predictionStack)
}

############################################################
# Create prediction data set
write_additional_model_outputs <- function(stack_list, training_data, rf.mdl, rf.prediction) {
  # probabilities
  x <- predict(rf.mdl, df, type = "prob")
  write.csv(x, "class_probabilities.csv")
  
  # overview
  png(filename = "map_samples.png", width = 1000, height = 800)
  par(mar=c(4,5,3,2))
  plotRGB(stack_list$stack[[1]][[4:2]], stretch="hist", axes=T, ext=extent(training_data))
  plot(training_data, add=T, col="red", border="red", lwd=3)
  dev.off()
  
  png(filename = "map_prediction.png", width = 1000, height = 800)
  plot(rf.prediction)
  dev.off()
  
  png(filename = "variable_importance.png", width = 500, height = 700)
  varImpPlot(rf.mdl, type=1)
  dev.off()
  
  capture.output(rf.mdl, file = "stats.txt")
}
