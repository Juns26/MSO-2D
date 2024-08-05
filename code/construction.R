library(dplyr)
ES_helper <- function(current_job, start_job, current_max_duration,activity_col,predecessor_col,duration_col) {
  #check for the first job which has NA predecessors
  if (is.na(current_job) | current_job == start_job) {
    return(current_max_duration)
  } 
  #check jobs, get its predecessors
  else {
    current_index <- which(current_job == activity_col)
    predecessors <- predecessor_col[current_index]
    if (is.na(predecessors)) {
      return(current_max_duration)
    } #in case predecessors is empty
    
    #get current job's predecessors
    predecessors <- unlist(strsplit(predecessors, ','))
    
    #store the parameter first, to be used in recursive function
    max_duration <- current_max_duration
    
    #call its predecessors recursively and add to max durations
    for (i in predecessors) {
      if (i != "") {
        previous_duration <- duration_col[which(i == activity_col)]
        max_duration <- max(max_duration, ES_helper(i, start_job, current_max_duration + previous_duration,activity_col,predecessor_col,duration_col))
      }
    }
    return(max_duration)
  }
}

#LF of current job = min {LS(successive jobs)} = min {LF(successive jobs)-successive jobs time}
LF_helper <- function(current_job,final_job,current_LF,activity_col,predecessor_col,duration_col) {

  #base case where current job is final job
  if (current_job==final_job[1]) {
    return (current_LF)
  }
  else {
    current_index <- which(current_job==activity_col)
    current_successors_index <- which(grepl(current_job,predecessor_col))
    current_successors <- activity_col[current_successors_index]
    min_duration <- current_LF
    for (i in current_successors){
      successor_duration <- duration_col[which(i==activity_col)]
      min_duration <- min(min_duration,LF_helper(i,final_job,current_LF-successor_duration,activity_col,predecessor_col,duration_col))
    }
    return (min_duration)
  }
}

input_data <- function(data,activity_header,predecessor_header,duration_header,cost_header,crashduration_header,crashcost_header,resource_header){
  
  headers <- c(activity_header, predecessor_header, duration_header, cost_header, crashduration_header, crashcost_header, resource_header)
  
  # Filter out 'NA' values
  headers <- headers[headers != 'NA']
  
  # Check for duplicate headers
  if (any(duplicated(headers))) {
    return (NULL)
  }
  
  else if (activity_header=='NA' || predecessor_header=='NA' || duration_header =='NA') {
    return(NA)
  }
  
  data <- data[!is.na(data[,duration_header]) & !is.na(data[,activity_header]),]
  
  #change to a common col name for easy reference later on
  #basic data needed for normal cpm
  colnames(data)[colnames(data)==activity_header] <- "Activity"
  colnames(data)[colnames(data)==predecessor_header] <- "Predecessor"
  colnames(data)[colnames(data)==duration_header] <- "Duration"
  headers <- c("Activity","Predecessor","Duration")
  
  #other extra data needed for crashing and leveling
  if (crashduration_header!='NA'){
    colnames(data)[colnames(data)==crashduration_header] <- "Crash Duration"
    
    #check crashing data for any NAs and empty string, replace with 0
    data$`Crash Duration` <- ifelse(is.na(data$`Crash Duration`) | data$`Crash Duration`=="",0,data$`Crash Duration`)
    headers <- c(headers,"Crash Duration")
  }
  if (crashcost_header!='NA'){
    colnames(data)[colnames(data)==crashcost_header] <- "Crash Cost per period"
    data$`Crash Cost per period` <- ifelse(is.na(data$`Crash Cost per period`) | data$`Crash Cost per period`=="",0,data$`Crash Cost per period`)
    headers <- c(headers,"Crash Cost per period")
    if (is.character(data$`Crash Cost per period`)) {
      data$`Crash Cost per period` <- ifelse(grepl("\\$",data$`Crash Cost per period`),gsub("\\$","",data$`Crash Cost per period`),data$`Crash Cost per period`)
      data$`Crash Cost per period` <- ifelse(grepl(",",data$`Crash Cost per period`),gsub(",","",data$`Crash Cost per period`),data$`Crash Cost per period`)
      data$`Crash Cost per period` <- as.numeric(data$`Crash Cost per period`)
    }
  }
  if (resource_header!='NA'){
    colnames(data)[colnames(data)==resource_header] <- "Resource"
    headers <- c(headers,"Resource")
  }
  if (cost_header!='NA'){
    colnames(data)[colnames(data)==cost_header] <- "Cost"
    headers <- c(headers,"Cost")
    #check if cost is of the correct type...
    if (is.character(data$`Cost`)) {
      data$Cost <- ifelse(grepl("\\$",data$Cost),gsub("\\$","",data$Cost),data$Cost)
      data$Cost <- ifelse(grepl(",",data$Cost),gsub(",","",data$Cost),data$Cost)
      data$Cost <- as.numeric(data$Cost)
    }
  }
  return (data[,headers])
}

processed_data <- function(data){
  start_activity <- data$Activity[is.na(data$Predecessor) | data$Predecessor=='-' | data$Predecessor=='']
  
  #If start_activity is none due to all no predecessor force to first activity
  if (length(start_activity) == 0) {
    start_activity <- data$Activity[1]
  } else {
    start_activity <- start_activity[1]
  }
  
  data$ES <- sapply(data$Activity, function(job) {
    tryCatch(
      ES_helper(job, start_activity, 0, data$Activity, data$Predecessor, data$Duration),
      error = function(e) {
        NA # Return NA if there's an error
      }
    )
  })
  if (any(is.na(data$ES))) {
    shiny::showNotification(
      HTML("<span style='font-family: Arial; font-size: 16px; color: red;'>Error in data input</span>"),
      type = "error"
    )
    return() 
  }
  
  data$EF <- data$ES + data$Duration
  
  #Testing what is data$EF to see if causing duration problem
  project_duration <- max(data$EF)
  
  final_job <- data$Activity[which(data$EF==project_duration)]
  # Use tryCatch to handle errors in LF_helper
  data$LF <- sapply(data$Activity, function(job) {
    tryCatch(
      LF_helper(job, final_job, project_duration, data$Activity, data$Predecessor, data$Duration),
      error = function(e) {
        NA # Return NA if there's an error
      }
    )
  })
  
  # Check if there were errors in LF_helper
  if (any(is.na(data$LF))) {
    shiny::showNotification(
      HTML("<span style='font-family: Arial; font-size: 16px; color: red;'>Error in data input</span>"),
      type = "error"
    )
    return() 
  }
  data$LS <- data$LF - data$Duration
  
  #find out which path is critical
  data$cp <- data$EF==data$LF
  return (data)
}

crashing_function <- function(target_duration,data){
  max_duration <- max(data$LF)
  while (max_duration>target_duration){
    if ("Crash Cost per period" %in% colnames(data)) {
      interval_data <- data %>%
        rowwise() %>%
        do(data.frame(
          interval = seq(.$ES, .$EF - 1, by = 1),
          Crash_Cost_per_period = .$`Crash Cost per period`,
          Activity = .$Activity,
          Crash_tracking = ifelse(.$`Crash Duration`>0,1,0)
        )) %>%
        group_by(interval) %>%
        summarize(total_crash_cost = sum(Crash_Cost_per_period),
                  num_activities = n(),
                  crash_tracking = sum(Crash_tracking))
      crashable <- which(interval_data$crash_tracking==interval_data$num_activities)
      if (length(crashable)==0){
        shiny::showNotification(
          HTML("<span style='font-family: Arial; font-size: 16px; color: red;'> Target Duration not possible </span>"),
          type = "error"
        )
        return (data)
      }
      window_start <- interval_data$interval[crashable[which.min(interval_data$crash_tracking[crashable])]]
      index <- which(data$EF>= (window_start+1) & data$ES<=window_start)
    }
    
    #if no crash cost provided, just go with the one with least crash activity
    else {
      interval_data <- data %>%
        rowwise() %>%
        do(data.frame(
          interval = seq(.$ES, .$EF - 1, by = 1),
          Activity = .$Activity,
          Crash_tracking = ifelse(.$`Crash Duration`>0,1,0)
        )) %>%
        group_by(interval) %>%
        summarize(num_activities = n(),
                  crash_tracking = sum(Crash_tracking))
      crashable <- which(interval_data$crash_tracking==interval_data$num_activities)
      if (length(crashable)==0){
        shiny::showNotification(
          HTML("<span style='font-family: Arial; font-size: 16px; color: red;'> Target Duration not possible </span>"),
          type = "error"
        )
        return (data)
      }
      window_start <- interval_data$interval[crashable[which.min(interval_data$crash_tracking[crashable])]]
      index <- which(data$EF>= (window_start+1) & data$ES<=window_start)
    }
    data$`Crash Duration`[index] <- data$`Crash Duration`[index] - 1
    data$Duration[index] <- data$Duration[index] - 1
    if ("Crash Cost per period" %in% colnames(data) & "Cost"%in% colnames(data)) {
      data$Cost[index] <- data$Cost[index] + data$`Crash Cost per period`[index]
      data$`Crash Cost per period` <- ifelse(data$`Crash Duration`==0,0,data$`Crash Cost per period`)
    }
    data <- processed_data(data)
    max_duration<- max(data$LF)
  }
  return (data)
}

#plotting labor-time graph
resource_vs_time <- function(data){
  #if resource leveling fails, data is NA
  if (!is.data.frame(data)){
    return (NA)
  } else{
    max_time <- max(data$EF)
    time_points <- 0:max_time
    resource_usage <- numeric(length(time_points))
    # Calculate the resource usage at each time point
    for (i in 1:nrow(data)){
      resource_usage[(data$ES[i]+1):data$EF[i]] <- resource_usage[(data$ES[i]+1):data$EF[i]]+ data$Resource[i]
    }
    return (data.frame(time_points,resource_usage)) 
  }
}

push_successors <- function(data,activity,end_activity) {
  current_successors_index <- which(grepl(activity,data$Predecessor))
  current_successors <- data$Activity[current_successors_index]
  while (length(current_successors)>0) {
    successor <- current_successors[1]
    current_successors <- current_successors[-1] #remove first one
    
    index <- which(data$Activity==successor)
    data$ES[index] <- data$ES[index]+1
    data$EF[index] <- data$EF[index]+1
    data$LS[index] <- data$LS[index]+1
    data$LF[index] <- data$LF[index]+1
    
    new_successors_index <- which(grepl(successor,data$Predecessor))
    new_successors <- data$Activity[new_successors_index]
    current_successors <- c(current_successors,new_successors)
  }
  return (data)
}

resource_leveling <- function(data,resource_allocated,cpm_resource_cap) {

  resource_usage <- resource_vs_time(data)
  #case1: not even possible for just one queue of activity with max resource
  if (resource_allocated<max(data$Resource)){
    return (NA)
  }

  #case2: resource allocated is >= necessary for existing cpm to work
  else if (resource_allocated>=cpm_resource_cap) {
    return (data)
  }

  else if (resource_allocated>=max(resource_usage$resource_usage)){
    return (data)
  }

  #case3: resource leveling needed
  else {
    end_activity <- data$Activity[which.max(data$EF)]
    for (i in 0:sum(data$Duration)){
      #only possibility of exceed is when a new activity is starting which adds on to the resource
      #check the resource at timestamp i, continue to push back if still exceed allocation
      cp_activities <- data$Activity[data$ES==i & data$cp==TRUE]
      non_cp_activities <- data$Activity[data$ES==i & data$cp==FALSE]
      while (sum(data$Resource[(data$ES<=i & data$EF>i)])>resource_allocated & !identical(non_cp_activities, character(0))) {
          #priority0: non-cp for all the possible slack duration as a form of resource smoothing
          #testing resource up till new ES should be <= resource allocation
          non_cp_data <- data[data$Activity %in% non_cp_activities,]
          for (activity in non_cp_data$Activity){
            index <- which(data$Activity==activity)
            ES <- data$ES[index]
            EF <- data$EF[index]
            resource <- data$Resource[index]
            resource_usage <- resource_vs_time(data)
            resource_usage$resource_usage[ES:EF] <- resource_usage$resource_usage[ES:EF] - resource
            for (timestamp in (i+1):data$LS[index]){
              temp_data <- data
              if (all(resource_usage$resource_usage[timestamp:(EF-ES+timestamp)] + resource<=resource_allocated)){
                temp_data$ES[index] <- timestamp
                temp_data$EF[index] <- timestamp + EF - ES
                temp_data$cp[index] <- temp_data$LS[index]==temp_data$ES[index]
                resource_usage <- resource_vs_time(temp_data)
                if (all(resource_usage$resource_usage<=resource_allocated)){
                  temp_data$LS <- temp_data$ES
                  temp_data$LF <- temp_data$EF
                  temp_data$cp <- temp_data$LS==temp_data$ES
                  max_duration <- max(temp_data$EF)
                  #identify potential gaps to push schedule ahead before crashing
                  for (i in 0:(max_duration - 1)) {
                    lower = i
                    higher= i+1
                    while (!any(temp_data$ES<=lower & temp_data$EF>=higher)){
                      #obtain all the activities after this spare window to push forward
                      activities_index <- which(temp_data$ES>=lower)
                      if (length(activities_index)==0){
                        break
                      }
                      temp_data$ES[activities_index] <- temp_data$ES[activities_index]-1
                      temp_data$EF[activities_index] <- temp_data$EF[activities_index]-1
                      temp_data$LS[activities_index] <- temp_data$LS[activities_index]-1
                      temp_data$LF[activities_index] <- temp_data$LF[activities_index]-1
                      max_duration <- max_duration-1
                    }
                  }
                  return (temp_data)
                }
              }
            }
          }

          #priority1: push non-cp back first, priority to push back for lower resource
          non_cp_activity <- non_cp_data$Activity[which.min(non_cp_data$Resource)]
          index <- which(data$Activity==non_cp_activity)

          #update the new ES and EF, check if activity still critical
          data$ES[index] <- data$ES[index] + 1
          data$EF[index] <- data$EF[index] + 1
          data$cp[index] <- data$EF[index]==data$LF[index]
          #no need to push successor since it eats into its own slacks
          non_cp_activities <- data$Activity[data$ES==i & data$cp==FALSE]
      }
      #priority2: push cp activity with the least resource needed (not sure if this is the best, just my algo)
      while (sum(data$Resource[(data$ES<=i & data$EF>i)])>resource_allocated){
        cp_activities <- data$Activity[data$ES==i & data$cp==TRUE]
        cp_data <- data[data$Activity %in% cp_activities,]
        cp_activity <- cp_data$Activity[which.min(cp_data$Resource)]
        index <- which(data$Activity==cp_activity)
        data$ES[index] <- data$ES[index] + 1
        data$EF[index] <- data$EF[index] + 1
        data$LS[index] <- data$LS[index] + 1
        data$LF[index] <- data$LF[index] + 1

        #push all successors of activity by 1 as well
        data <- push_successors(data,cp_activity,end_activity)
      }
    }
    #remove all the slack, not sure if its necessary
    data$LS <- data$ES
    data$LF <- data$EF
    data$cp <- data$LS==data$ES
    max_duration <- max(data$EF)
    #identify potential gaps to push schedule ahead before crashing
    for (i in 0:(max_duration-1)) {
      lower = i
      higher= i+1
      while (!any(data$ES<=lower & data$EF>=higher)){
        #obtain all the activities after this spare window to push forward
        activities_index <- which(data$ES>=lower)
        if (length(activities_index)==0){
          break
        }
        data$ES[activities_index] <- data$ES[activities_index]-1
        data$EF[activities_index] <- data$EF[activities_index]-1
        data$LS[activities_index] <- data$LS[activities_index]-1
        data$LF[activities_index] <- data$LF[activities_index]-1
        max_duration <- max_duration-1
        }
    }
    return (data)
  }
}

library(ggplot2)
library(plotly)
gantt_chart <- function(data,start_date=NULL,toggle_date = FALSE){
  #when resource leveling fails, data is NA
  if (!is.data.frame(data)){
    return (NA)
  }

  if (toggle_date) {
    vert_line <- Sys.Date()
    scale_x <- scale_x_date(limits = c(min(data$ES),max(data$LF)))
  } else {
    vert_line <- NULL
    scale_x <- scale_x_continuous(limits = c(min(data$ES),max(data$LF)))
  }

  data$Activity <- factor(data$Activity,level=rev(unique(data$Activity)))
  data$hover_text <- paste('Activity:', data$Activity, '<br>ES:', data$ES, '<br>EF:', data$EF, '<br>LS:', data$LS, '<br>LF:', data$LF)
  plot <- ggplot(data, aes(y = Activity, x = ES, xend = LF, text = hover_text)) +
    geom_rect(aes(xmin = ES, xmax = EF, ymin = as.numeric(Activity) - 0.4, ymax = as.numeric(Activity) + 0.4, fill = 'Actual')) +
    geom_rect(aes(xmin = EF, xmax = LF, ymin = as.numeric(Activity) - 0.4, ymax = as.numeric(Activity) + 0.4, fill = 'Slack'), alpha = 0.55) +
    scale_fill_manual(name = "Legend", values = c('Actual' = '#ff8e72', 'Slack' = '#ffbead')) +
    guides(fill = guide_legend(override.aes = list(size = 5, height = 2.5, width = 2.5))) +  # Adjust legend key size
    geom_vline(xintercept = as.numeric(vert_line), color = 'grey', linetype='solid', size = 0.75) +
    labs(title = 'Gantt Chart', x = 'Time', y = 'Activity') +
    theme_minimal()+
    scale_x
  
  # Convert to interactive plot with plotly
  plot_interactive <- ggplotly(plot, tooltip = "text") %>%
    layout(legend = list(orientation = "h", yanchor = "top",xanchor='right',x=1, y = -0.1))
  return(plot_interactive)
}

resource_chart <- function(data,start_date=NULL,toggle_date=FALSE){
  if (toggle_date) {
    temp_final_dataset <-data
    temp_final_dataset$ES <- as.numeric(temp_final_dataset$ES - start_date)
    temp_final_dataset$EF <- as.numeric(temp_final_dataset$EF - start_date)
    temp_final_dataset$LS <- as.numeric(temp_final_dataset$LS - start_date)
    temp_final_dataset$LF <- as.numeric(temp_final_dataset$LF - start_date)
    resource_usage <- resource_vs_time(temp_final_dataset)
    resource_usage$time_points <- as.Date(resource_usage$time_points + start_date)
  } 
  else {
    resource_usage <- resource_vs_time(data)
  }
  if (toggle_date) {
    vert_line <- Sys.Date()
  } else {
    vert_line <-NULL
  }
  
  colnames(resource_usage) <- c("Time","Resource")
  
  gg_plot <- ggplot(data = resource_usage, aes(x = Time, y = Resource)) +
    geom_step(color='#ff851b') +
    labs(x = "Time", y = "Resource") + 
    geom_vline(xintercept = as.numeric(vert_line), color = 'grey', linetype='solid', size = 0.75) +
    theme_minimal()
  
  
  plotly_plot <- ggplotly(gg_plot)
  
  return (plotly_plot)
}

cost_chart <- function(data,start_date=NULL,toggle_date=FALSE){
  if (toggle_date) {
    temp_final_dataset <-data
    temp_final_dataset$ES <- as.numeric(temp_final_dataset$ES - start_date)
    temp_final_dataset$EF <- as.numeric(temp_final_dataset$EF - start_date)
    temp_final_dataset$LS <- as.numeric(temp_final_dataset$LS - start_date)
    temp_final_dataset$LF <- as.numeric(temp_final_dataset$LF - start_date)
    cost_usage <- cost_vs_time(temp_final_dataset)
    cost_usage$time_points <- as.Date(cost_usage$time_points + start_date)
  } 
  else {
    cost_usage <- cost_vs_time(data)
  }
  if (toggle_date) {
    vert_line <- Sys.Date()
  } else {
    vert_line <-NULL
  }
  colnames(cost_usage) <- c("Time", "Cost","Total Cost")
  cost_usage$`Total Cost` <- round(cost_usage$`Total Cost`, 2)
  gg_plot <- ggplot(cost_usage, aes(x = Time, y = `Total Cost`)) +
    geom_line(colour = '#3d9970') +
    labs(x = "Time",y = "Total Cost") +
    geom_vline(xintercept = as.numeric(vert_line), color = 'grey', linetype='solid', size = 0.75) +
    theme_minimal()
  
  plotly_plot <- ggplotly(gg_plot)
  return (plotly_plot)
}

final_data <- function(data,target_resource,target_duration) {
  
  #resource leveling first
  resource_usage <- resource_vs_time(data)
  if (!is.data.frame(resource_usage)){
    return (NA)
  }
  
  cpm_cap <- max(resource_usage$resource_usage)
  leveled_data <- resource_leveling(data,target_resource,cpm_cap)
  
  #remove all the slack, not sure if its necessary
  leveled_data$LS <- leveled_data$ES
  leveled_data$LF <- leveled_data$EF
  leveled_data$cp <- leveled_data$LS==leveled_data$ES
  max_duration <- max(leveled_data$EF)
  resource_usage <- resource_vs_time(leveled_data)
  
  #identify potential gaps to push schedule ahead before crashing
  for (i in 0:(max_duration - 1)) {
    lower = i
    higher= i+1
    while (!any(leveled_data$ES<=lower & leveled_data$EF>=higher)){
      #obtain all the activities after this spare window to push forward
      activities_index <- which(leveled_data$ES>=lower)
      if (length(activities_index)==0){
        break
      }
      leveled_data$ES[activities_index] <- leveled_data$ES[activities_index]-1
      leveled_data$EF[activities_index] <- leveled_data$EF[activities_index]-1
      leveled_data$LS[activities_index] <- leveled_data$LS[activities_index]-1
      leveled_data$LF[activities_index] <- leveled_data$LF[activities_index]-1
      max_duration <- max_duration-1
    }
  }
  
  #crash activity to meet target_duration
  while (max_duration>target_duration) {
    if ("Crash Cost per period" %in% colnames(data)) {
      interval_data <- leveled_data %>%
        rowwise() %>%
        do(data.frame(
          interval = seq(.$ES, .$EF - 1, by = 1),
          Crash_Cost_per_period = .$`Crash Cost per period`,
          Activity = .$Activity,
          Crash_tracking = ifelse(.$`Crash Duration`>0,1,0)
        )) %>%
        group_by(interval) %>%
        summarize(total_crash_cost = sum(Crash_Cost_per_period),
                  num_activities = n(),
                  crash_tracking = sum(Crash_tracking))
      crashable <- which(interval_data$crash_tracking==interval_data$num_activities)
      window_start <- interval_data$interval[crashable[which.min(interval_data$total_crash_cost[crashable])]]
      index <- which(leveled_data$EF>= (window_start+1) & leveled_data$ES<=window_start)
    }
    
    #if no crash cost provided, just go with the one with least crash activity
    else {
      interval_data <- leveled_data %>%
        rowwise() %>%
        do(data.frame(
          interval = seq(.$ES, .$EF - 1, by = 1),
          Activity = .$Activity,
          Crash_tracking = ifelse(.$`Crash Duration`>0,1,0)
        )) %>%
        group_by(interval) %>%
        summarize(num_activities = n(),
                  crash_tracking = sum(Crash_tracking))
      crashable <- which(interval_data$crash_tracking==interval_data$num_activities)
      window_start <- interval_data$interval[crashable[which.min(interval_data$crash_tracking[crashable])]]
      index <- which(leveled_data$EF>= (window_start+1) & leveled_data$ES<=window_start)
    }
    #identify all the activities_index after this activity_index
    activities_index <- which(leveled_data$ES>=window_start+1)
    
    leveled_data$`Crash Duration`[index] <- leveled_data$`Crash Duration`[index] - 1
    leveled_data$Duration[index] <- leveled_data$Duration[index] - 1
    leveled_data$EF[c(index,activities_index)] <- leveled_data$EF[c(index,activities_index)] - 1
    leveled_data$LF[c(index,activities_index)] <- leveled_data$LF[c(index,activities_index)] - 1
    leveled_data$ES[activities_index] <- leveled_data$ES[activities_index] - 1
    leveled_data$LS[activities_index] <- leveled_data$LS[activities_index] - 1
    if ("Crash Cost per period" %in% colnames(data) & "Cost"%in% colnames(data)) {
      leveled_data$Cost[index] <- leveled_data$Cost[index] + leveled_data$`Crash Cost per period`[index]
      leveled_data$`Crash Cost per period` <- ifelse(leveled_data$`Crash Duration`==0,0,leveled_data$`Crash Cost per period`) 
    }
    max_duration <- max_duration - 1
  }
  
  if (target_duration<max(leveled_data$EF)) {
    shiny::showNotification(
      HTML("<span style='font-family: Arial; font-size: 16px; color: red;'> Target Duration not possible </span>"),
      type = "error"
    ) 
  }
  
  #will always return data with duration>=target duration
  return (leveled_data)
}

cost_vs_time <- function(data) {
  max_time <- max(data$EF)
  time_points <- 0:max_time
  cost_usage <- numeric(length(time_points))
  # Calculate the cost usage at each time point
  for (i in 1:max_time){
    activities_index <- which(data$ES<=(i-1) & data$EF>=i)
    cost_usage[i+1] <- sum(data$Cost[activities_index]/data$Duration[activities_index])
  }
  cul_cost <- cumsum(cost_usage)
  return (data.frame(time_points,cost_usage,cul_cost)) 
}


# Prepare the data for networkD3
prepare_network_data <- function(data) {
  # Create nodes dataframe
  nodes <- unique(c(data$Activity, unlist(strsplit(data$Predecessor, ","))))
  nodes <- data.frame(name = nodes, stringsAsFactors = FALSE)
  
  # Label the root node
  nodes$group <- ifelse((nodes$name == "-"), "Root", "Node")
  nodes$name[nodes$name == "-"] <- "Project"
  
  # Create links dataframe
  links <- data %>%
    separate_rows(Predecessor, sep = ",") %>%
    mutate(
      source = match(ifelse((Predecessor == "-" ), "Project", Predecessor), nodes$name) - 1,
      target = match(Activity, nodes$name) - 1
    ) %>%
    select(source, target)
  list(nodes = nodes, links = links)
}
# 
# a <- read.csv('aaa.csv')
# a <- input_data(a,"Activity","Predecessor","Duration","Cost","NA","NA","Resource")
# a <- processed_data(a)
# a
# r <- resource_leveling(a,1,3)
# 
b <- read.csv('Construction.csv')
b <- input_data(b,"Jobs", "Predecessor", "Duration..days.","Cost....","NA","NA","Labor")
b <- processed_data(b)
r1 <- resource_leveling(b,2,4)
