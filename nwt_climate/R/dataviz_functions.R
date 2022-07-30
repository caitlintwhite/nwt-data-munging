# dataviz functions

require(tidyverse)
require(lubridate)
theme_set(theme_test())

# show available data and missing by time sequence, for as many vars as want
# > could later make it work to show multiple stations at once by plotting list

available_dataviz <- function(dat, timecol, id = NULL, mets, allvars = T, plotNA = T, scales = "free_y", ...){
  
  if(allvars){
    # collapse meteorological columns into searchable string
    keepcols <- paste0("^", mets, "($|_)")
    keepcols <- stringr::str_flatten(keepcols, collapse = "|")
    # update mets
    mets <- names(dat)[grepl(keepcols, names(dat))]
  }
  
  # subset data to plot
  plotdat <- dat[names(dat) %in% c(timecol, mets)]
  # note any column that is completely empty
  excludemets <- names(plotdat)[sapply(plotdat, function(x) all(is.na(x)))]
  # gather all metrics to facet wrap panels
  plotdat <- gather(plotdat, metric, value, all_of(mets))
  # remove mets that are empty
  plotdat <- subset(plotdat, !metric %in% excludemets)
  
  # create df to mark missing data due to date breaks (e.g., gchnd data)
  # determine expected time interval based on most recent data
  timeinterval <- plotdat[[timecol]][nrow(plotdat)] - plotdat[[timecol]][nrow(plotdat)-1]
  # create expected time series based on time class
  if(class(plotdat[[timecol]]) == "Date"){
    missing_times <- seq.Date(from = min(plotdat[[timecol]]), to = max(plotdat[[timecol]]), by = timeinterval)
  }else{
    # assume it's POSIX format if not Date
    missing_times <- seq.POSIXt(from = min(plotdat[[timecol]]), to = max(plotdat[[timecol]]), by = timeinterval)
  }
  # create df of expected times and data values
  expectedtime_df <- data.frame(missing_times)
  expectedtime_df <- cbind(expectedtime_df, 
                           matrix(nrow = length(missing_times), ncol = length(unique(plotdat$metric)),
                                  dimnames = list(NULL, unique(plotdat$metric))))
  expectedtime_df <- gather(expectedtime_df, metric, value, unique(plotdat$metric))
  # drop empty value col then merge data
  expectedtime_df <- expectedtime_df[c("missing_times", "metric")]
  names(expectedtime_df)[1] <- timecol
  expectedtime_df <- left_join(expectedtime_df, cbind(plotdat, present = 1), by = c(timecol, "metric"))
  
  # plot
  if(plotNA){
    p <- ggplot(plotdat, aes(get(timecol), is.na(value))) +
    geom_jitter(alpha = 0.5, height = 0.25, width = 0) +
    geom_jitter(data = subset(expectedtime_df, is.na(present)), alpha = 0.5,  height = 0.12, width = 0, col = "orchid") +
    labs(x = NULL, y = "Value missing?", subtitle = id) +
    facet_wrap(~metric)
  }
  if(!plotNA){
    p <- ggplot(plotdat, aes(get(timecol), value), ...) +
      geom_line(alpha = 0.8, na.rm = T) +
      labs(x = NULL, subtitle = id) +
      facet_wrap(~metric, scales = scales)
  }
  # print plot
  print(p)
}

plot_all_list <- function(listobject, ...){
  for(i in 1:length(listobject)){
    available_dataviz(listobject[[i]], id = names(listobject)[i], ...)
  }
}

plot_all_groups <- function(dat, groupvars, ...){
  for(i in unique(dat[[groupvars]])){
    print(paste("Processing", i))
    available_dataviz(subset(dat, get(groupvars) == i), id = i, ...)
  }
}
