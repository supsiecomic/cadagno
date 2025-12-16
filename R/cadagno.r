###############################################################
#
#                       CADAGNO
#                    
# description: utiliy functions to prepare, clean and analyse
#              lake Cadagno data
#
# author:       Daniel Romero Mujalli
# email:        daniel.romero@supsi.ch
#
# last update:  20251216
#
###############################################################
###############################################################
#' read_TOB
#'
#' DESCRIPTION
#' this function reads .TOB data file and returns a dataframe
#'
#' PARAMETERS
#' @param fname file path/name to open for reading
#'
#' @param showunits column names with parameter units
#'
#' @param outdir character output directory.
#' Default FALSE: no file is created
#'
#' @param DEBUG code maintenance only
#'
#' OUTPUT
#' @return a data frame
#'
#' @export
read_TOB <- function(
  fname,
  showunits = FALSE,
  outdir = FALSE,
  DEBUG = FALSE
) {
  
  # establish connection
  con = file(
    description = fname,
    open = "r" # read mode, see ?file
  )

  i = 1 # track lines of interest
  curr_line <- 1000
  while ( TRUE ) { # file reading loop starts here
    line = readLines(con, n = 1, warn = FALSE)
    # remove non UTF-8 characters from line
    line <- iconv(x = line, from = "UTF-8", to = "UTF-8", sub = "")
    #print(line)
    # break the loop is file end was reached
    if ( length(line) == 0 ) {
      break
    }

    # store date information
    if(i == 3)
      filedate <- extract_date(line)

    # find column names (header) using pattern matching
    if(grepl(pattern = "Datasets", x = line))
    {
        # split the line to get the names
        names <- unlist(strsplit(x = line, split = "    "))
        # clean unnecessary characters
        names <- gsub(pattern = " ", replacement = "", x = names)
        names <- sub(pattern = ";", replacement = "", x = names)
        # remove redundant information: 
        # name == "Datasets" since it gives the row numbers
        names <- names[-1]

        # update old variable names
        names[names %in% "Oxy"] <- "sat"
        names[names %in% "O2_mg"] <- "DO_mg"

        # get the current line number
        curr_line <- i

        # DEBUG
        if(DEBUG)
        { 
            print(line)
            print(names)
        }
    }

    # attached units to names, if requested
    if(showunits && i == curr_line + 1)
    {
        # split the line to get the units
        units <- unlist(strsplit(x = line, split = "  "))
        # clean unnecessary characters
        units <- units[-c(1:6)]
        units <- gsub(pattern = " ", replacement = "", x = units)
        units[units == "%]"] <- "xx" # backup before removing nonalphanumerics
        units <- gsub(pattern = "[^[:alnum:] ]", replacement = "", x = units)
        units[units == "xx"] <- "[%]"
        units <- sub(pattern = "E", replacement = "muE", x = units)
        units <- sub(pattern = "gL", replacement = "mug/l", x = units)
        units <- units[units != ""]
        units <- sub(pattern = "mScm", replacement = "mS/cm", x = units)
        units <- sub(pattern = "mgl", replacement = "mg/l", x = units)
        
        # concatenate units to names
        names <- paste0(names,".",units)
    
        # DEBUG
        if(DEBUG){
            print(units)
            print(names)
        }
    }

    # read values
    if(i >= curr_line + 3)
    {
        # split the line to get the values
        values <- unlist(strsplit(x = line, split = " "))
        # append AM|PM to end of time values
        if(sum(grepl("AM|PM", values)))
        {
          index <- grep(pattern = "AM|PM", values)
          values[index - 1] <- paste(values[index - 1], values[index]
                                    ,sep = ""
                                    )
          # remove AM|PM column
          values <- values[-index]
        }
        values <- gsub(pattern = " ", replacement = "", x = values)
        values <- values[values != ""]
        values <- values[-1]

        # store the information into a dataframe
        if(i == curr_line + 3)
        { 
            x <- data.frame(t(values)) 
        } else { 
                x <- rbind(x, data.frame(t(values))) 
        }
    
        # DEBUG   
        if(DEBUG)
            print(values)
    }
    # update line counter
    i = i + 1   
  } # while loop ends here

  # set the column names based on names
  colnames(x) <- names

  # convert measurements to numeric data type
  foo <- suppressWarnings(sapply(na.omit(x), as.numeric))
  selection <- unique(is.na(foo))
  x[, !selection] <- sapply(X = x[, !selection], FUN = as.numeric)

  # DEBUG
  if(DEBUG)
    print(head(x))
  
  # close connection
  close(con)

  # bind filedate
  filedate <- rep(filedate, times = nrow(x))
  x <- cbind(filedate,x)
  names(x)[1] <- "date"

  # write data to file, if requested
  if(outdir != FALSE)
  {
    fname <- sub(".TOB","",fname)
    fname <- sub("tob/","",fname)
    fname <- sub(" ","",fname)
    write.table(
      x = x,
      file = file.path(outdir,paste0(filedate,fname,".csv")),
      quote = FALSE,
      row.names = FALSE,
      sep = ";"
    )
  }
  
  # return object
  return(x)
}

###############################################################
#' clean_TOB
#'
#' DESCRIPTION
#' this function clean previously read .TOB data file.
#' The cleaning consists of three steps:
#' 0)  removal of measurements when the probe is ascending
#' i)  reduction from multiple measurements per depth to single 
#'     measurement per depth value
#' ii) reording the dataset based on depth
#'
#' PARAMETERS
#' @param x dataframe obtained from calling read_TOB()
#'
#' @param selection_method method to treat the multiple
#' measurements per depth value. Either by "random" sample or
#' by taking the "mean"
#'
#' @param outfile character output file name
#' Default FALSE: no file is created
#'
#' OUTPUT
#' @return a data frame
#'
#' @export
clean_TOB <- function(
  x,
  selection_method = FALSE,
  outfile = FALSE
  ){
    # backup file date
    filedate <- x$date
    # remove date column. Then bind it back at the end of file
    # processing
    x <- x[,-1]

    # remove negative values of depth since this means that
    # the probe was still out of water
    x <- x[x$Press > 0,]
    # remove values that were taking when the probe was
    # ascending
    x <- x[1:which(x$Press == max(x$Press))[1], ]
    # get unique depth values
    values <- unique(x$Press)
    # simplify the dataset to consider measurements
    # per unique value of depth
    for(k in 1:length(values)){
        # set of values with same depth value
        set <- which(x$Press == values[k])

        # i)   selection_method: first value
        if(selection_method == FALSE){
          if(length(set) > 1){
            foo <- x[set[1], ]
          } else{
            foo <- x[set, ]
          }
        }

        # ii)  selection_method: random selection
        if(selection_method == "random"){
          if(length(set) > 1){
            foo <- x[sample(x = set, size = 1), ]
          } else{
            foo <- x[set, ]
          }
        }
          
        # iii) selection_method = mean
        if(selection_method == "mean"){
          # indentify and exclude non-numeric columns
          dummy <- suppressWarnings(sapply(na.exclude(x)
                                          ,as.numeric
                                          )
                                   )
          selection <- unique(is.na(dummy))
          foo <- sapply(X = x[set, !selection]
                       ,FUN = mean
                       )
          foo <- as.data.frame(cbind(x[set[1], selection]
                                    ,t(foo)
                                    )
                              )
        }
        
        # create data frame
        if(k < 2){
          y <- foo
        } else {
          y <- rbind(y, foo)
        }
    }

    # reorder data based on depth
    y <- y[order(y$Press),]

    # change Press to Depth and add 
    # Depth_par = Depth - 0.53 variable
    # some measurements are taken by a sensor located 0.53m 
    # heigher than the sensor that register depth
    names(y)[names(y) == "Press"] <- "Depth"
    y$Depth_par <- y$Depth - 0.53
    # reorder variable names according to template
    # Values not included in template will be placed at the end
    template <- c("Depth", "Depth_par", "Cond", "Temp", "sat"
    ,"DO_mg", "Turb", "PAR", "BGAPC", "Chl_A", "IntD", "IntT"
    ,"pH", "Redox", "H2S", "T_H2S", "SOUND"
    )
    dummy <- template[template %in% names(y)]
    yummy <- names(y)[!names(y) %in% template]
    y <- y[,c(dummy, yummy)]

    # adjust precision / number of digits of data variables
    for (var in names(y))
    {
      precision <- get_precision(var)
      if(precision)
        y[, var] <- round(x = y[, var], digits = precision)
    }

    # bind file date to y
    y <- cbind(filedate[1:nrow(y)], y)
    names(y)[1] <- "date"

    # write data to file, if requested
    if(outfile != FALSE)
    {
      write.table(x = y
               ,file = outfile
               ,quote = FALSE
               ,row.names = FALSE
               ,sep = ";"
               )
    }
    # return object
    return(y)
}

#'#############################################################
#'		     		GET PRECISION			
#' DESCRIPTION 
#' returns the precision (number of digits) after the comma for
#' the variable of interest
# 
#' PARAMETERS
#' @param var variable (as defined in precision.txt)
#'
#' OUTPUT
#' @return number of digits after the comman for the variable
#' of interest. If the variable is not found in precision.txt
#' FALSE is returned instead
#'
get_precision <- function (var)
{
    # read precision from file
    # more info in "precision/precision.txt"
    df <- data.frame(
    var = c("Depth", "Depth_par", "Cond", "Temp", "sat", "DO_mg"
           ,"Turb", "PAR", "BGAPC", "Chl_A", "pH", "Redox", "H2S"),
    digits = c(2, 2, 4, 1, 1, 2, 1, 2, 1, 4, 1, 1, 1)  
    )
    
    precision <- ifelse(sum(grepl(var, df[,1]))
                       ,yes = df[grep(var,df[,1])[1],2]
                       ,no  = FALSE
                       )
    return (precision)
}

###############################################################
#' plot_var
#'
#' DESCRIPTION
#' plot the profile of the specified variable against depth
#' using simple moving average (?TTR::SMA)
#'
#' PARAMETERS
#'
#' @param var variable (vector)
#'
#' @param depth depth values (vector)
#'
#' @param ... additional parameter to plot()
#'
#' OUTPUT
#' nothing
#'
#' @export
plot_var <- function(
  var,
  depth,
  xlab = "var",
  linecolor = "cyan4",
  lwd = 2, 
  ...
)
{
  # get simple moving average
  sma <- TTR::SMA(
    x = var,
    n = 10 # number of periods to average over
  ) 
  # create the plot
  plot(
    x = var, 
    y = -1 * depth,
    las = 1,xlab = "",
    ylab = "Depth",
    col = "white",
    ylim = range(0, -next_ten(max(var))),
    bty = "n",
    xaxt = "n"
  )
  # y-axis   
  # x-axis on top
  axis(side = 3)
  # add axis label
  mtext(text = xlab, side = 3, line = 2)
  # add the moving average to plot  
  lines(
    x = sma, 
    y = -1 * depth,
    col = linecolor,
    lwd = lwd, 
    ...
  )
}

###############################################################
#' plot_set
#'
#' DESCRIPTION
#' plot a set of measurements of interest against depth, using
#' moving average. All variables are displayed in the same plot
#'
#' PARAMETERS
#' @param x cleaned .TOB dataframe
#'
#' @param set set of variables of interest (vector).
#' Otherwise,  
#' "abio", displays Temp, Cond, DO_mg and Turb in a single plot
#' "bio", PAR, Chl_A and BGAPC into single plot.
#'
#' @param ... additional parameter to plot()
#'
#' OUTPUT
#' nothing
#'
#' @export
plot_set <- function(
  x,
  BL = "abio", # bacterial layer variable set: "bio" or "abio"
  lwd = 1,
  Rbrewer_colorname = "RdBu",
  zoomIn = FALSE, 
  ...
)
{
  ##  TO DO:
  # MAKE IT WORK WITH THE ZOOMIN (DONE)
  # THEN, CORRECT PAR VARIABLE BY SUBSTRACTING 63
  # REPORT MEAN OF PAR AT SURFACE (THOSE NAGATIVE AFTER SUBSTRACTION)

  # variable selection
  if(BL == "abio")
    set <- c("Temp", "Cond", "DO_mg", "Turb")
  if(BL == "bio")
    set <- c("Chl_A", "BGAPC", "sat", "Turb", "PAR")

  foo <- x[, names(x) %in% set]
  #selection <- unlist(sapply(X = set, FUN = grep, names(x)))
  #foo <- x[, selection]
  # standardize values so that they can be plotted in similar
  # scale
  foo <- sapply(X = foo, FUN = function(x) {
    x <- (x - min(x)) / (max(x) - min(x))
  })
  foo <- as.data.frame(foo)

  # adjust range of ylim and
  # subset dataset if zoomIn is requested
  # default zoom_selection: all depths selected (i.e., no zoom)
  zoom_selection <- rep(TRUE,times = nrow(x))
  y_range <- range(0, -next_ten(max(x$Depth)))
  if(BL == "bio" && zoomIn)
  {
    sd <- 3
    ref <- x$Depth[x$Turb == max(x$Turb)]
    
    y_range <- c(ref - sd, ref + sd)
    # use only variable values within y_range
    #x <- x[x$Depth >= y_range[1] & x$Depth <= y_range[2],]
    zoom_selection <- x$Depth >= y_range[1] & x$Depth <= y_range[2]
    # adjust y_range to fit plotting requirements:
    # depth: negative numbers. Therefore -1 * y_range
    y_range <- -1 * y_range
    y_range <- y_range[order(y_range)]   
  }
  # subset data to zoom_selection
  foo <- foo[zoom_selection, ]
  x   <-   x[zoom_selection, ]

  # modify plot margins to fit multiple x-axes
  # default margin values "mar" starting from bottom,
  # then going clockwise
  # $mar = c(5.1, 4.1, 4.1, 2.1)
  custom_mar <- c(
    2.0, # bottom
    4.1, # left
    #4.1 + round(ncol(x) * 0.5),
    2.1 + round(length(set) * 2), # top
    5 # right
  )
  par(mar = custom_mar)
  # create empty plot
  # take as reference for x axis, the variable with
  # highest value
  dummy <- sapply(X = foo, FUN = max)
  index <- which(dummy == max(dummy))[1]
  plot(
    x = foo[,index],#[zoom_selection, 1],
    y = -1 * x$Depth,#[zoom_selection],
    las = 1,
    xlab = "",
    ylab = "Depth",
    col = "white",
    ylim = y_range,
    bty = "n",
    xaxt = "n"
  )
  # create color palette
  n <- dim(foo)[2]
  #mypalette <- viridis::magma(n)
  mypalette <- RColorBrewer::brewer.pal(n, name = Rbrewer_colorname)
  # replace light colors with colors from another palette
  mypalette[mypalette == "#F7F7F7"] <- "#762A83"
  mypalette[mypalette == "#D1E5F0"] <- "#1B7837"
  # plot the variables in set (i.e., names(foo))
  # keep in mind that names(foo) does not necessarily contain all
  # names in set 
  i <- 1
  for (var in names(foo)){ # in set
    sma <- TTR::SMA(
      x = foo[,var],
      n = 10 # number of periods to average over
    )
    # add x-axis on top
    # at: four levels c(0, 25%, 50%, 75%, next_ten)
    v <- x[,var]
    #min_v <- round(min(v), digits = get_precision(var))
    #max_v <- round(max(v), digits = get_precision(var))
    min_v <- round(min(v), digits = 0)
    max_v <- next_ten(max(v))
    dummy <- (max_v - min_v) / 4
    labels <- c(
      min_v,
      min_v + 1 * dummy,
      min_v + 2 * dummy,
      min_v + 3 * dummy,
      max_v
    )
    thisline <- 1 + (2*i - 1)
    axis(
      side = 3,
      at = c(0, 0.25, 0.5, 0.75, 1.0),
      labels = labels,
      line = thisline,
      col = mypalette[i],
      #col.ticks = mypalette[i],
      col.axis = mypalette[i]
    )
    # add axis label
    mtext(
      text = var, side = 3, line = thisline,
      adj = 1,
      at = 1.2,
      col = mypalette[i]
    )
    # add the moving average to plot  
    lines(
      x = sma,#[zoom_selection], 
      y = -1 * x$Depth,#[zoom_selection],
      col = mypalette[i],
      lwd = lwd#, 
      #...
    )
    # update counter
    i <- i + 1
  } 
}


###############################################################
#'		     		            NEXT TEN			
#' description: returns the next ten of the number x
#'              If x is a vector v, then x = max(v)
# 
#' PARAMETERS
#' @param x numeric or vector
#'
#' OUTPUT
#' @return the next ten of x (e.g., if x = 41, then return 50)
#'
#'
next_ten <- function (x)
{
    # catch exeption: returns unity if the value is less than 1
    if(max(x) < 1)
        return(1)
    y <- floor(max(x)) + seq(0,9,1)
    return (y[y %% 10 == 0])
}


###############################################################
#'		     		            EXTRACT DATE			
#' description: extract date information from the string input
# 
#' PARAMETERS
#' @param s character string
#'
#' OUTPUT
#' @return character date object
#'
extract_date <- function(s){
  # split s based on "," "." or empty space " "
  x <- unlist(strsplit(x = s, split = "[,|. ]"))
  # remove unnecessary elements
  x <- x[!c(nchar(x) < 1 | grepl("AM|PM|[:]", x))]
  # remove the first element as it is always the week day
  x <- tolower(x[-1])
  # create date class object
  dateformat <- "%b,%d,%Y"
  y <- get_month(x)
  # is the month was in italian, then adjust date format
  if(!identical(x,y))
    dateformat <- "%d,%b,%Y"
  x <- y
  date <- as.Date(paste(x, collapse = ","), format = dateformat)
  # return date object
  return (date)
}


###############################################################
#'		     		            GET MONTH			
#' description: identify and translate the month name in x
# 
#' PARAMETERS
#' @param x character vector
#'
#' OUTPUT
#' @return x with english month name
#'
get_month <- function(x){
  
  month <- data.frame(
    en = c(
    "january",
    "february",
    "march",
    "april",
    "may",
    "june",
    "july",
    "august",
    "september",
    "october",
    "november",
    "december"
    ), 
    it = c(
    "gennaio",
    "febbraio",
    "marzo",
    "aprile",
    "maggio",
    "giugno",
    "luglio",
    "agosto",
    "settembre",
    "ottobre",
    "novembre",
    "dicembre"
    )
  )
  
  for(i in 1:nrow(month))
  {
    m <- grepl(paste(month[i,],collapse = "|"), tolower(x))
    if(sum(m) > 0)
      return(sub(x[m],month[i, "en"],x))
  }
  return (NA)
}