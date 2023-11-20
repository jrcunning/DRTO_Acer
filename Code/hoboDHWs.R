#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"
#' @export
hoboDHWs <- function(path, MMM, anomaly = 1, calc = TRUE, groupingVariable = NA, summary = FALSE) {

  readInRename <- function(hoboFile){
    groupingVariable = groupingVariable

    hoboNames <- as.data.frame(names(hoboFile)) %>%
      tibble::rownames_to_column(., "Index") %>%
      dplyr::mutate(Function = dplyr::case_when(grepl("Date",names(hoboFile), ignore.case = TRUE) == TRUE
                                                & grepl("Time",names(hoboFile), ignore.case = TRUE) == TRUE ~ "DateTime",
                                                grepl("Date",names(hoboFile), ignore.case = TRUE) == TRUE
                                                & grepl("Time",names(hoboFile), ignore.case = TRUE) == FALSE ~ "Date",
                                                grepl("Date",names(hoboFile), ignore.case = TRUE) == FALSE
                                                & grepl("Time",names(hoboFile), ignore.case = TRUE) == TRUE ~ "Time",
                                                grepl("Temp",names(hoboFile), ignore.case = TRUE) == TRUE ~ "Temperature",
                                                grepl("Lux", names(hoboFile), ignore.case = TRUE) == TRUE
                                                | grepl("Light", names(hoboFile), ignore.case = TRUE) == TRUE
                                                | grepl("lum", names(hoboFile), ignore.case = TRUE) == TRUE ~ "Light",
                                                names(hoboFile) == groupingVariable ~ "Group"),
                    Units = dplyr::case_when(Function == "Temperature" &
                                               grepl("F",names(hoboFile), ignore.case = TRUE) == TRUE ~ "°F",
                                             Function == "Temperature" &
                                               grepl("C",names(hoboFile), ignore.case = TRUE) == TRUE ~ "°C",
                                             Function == "Light" &
                                               grepl("lux",names(hoboFile), ignore.case = TRUE) == TRUE ~ "(lux)",
                                             Function == "Light" &
                                               grepl("lum",names(hoboFile), ignore.case = TRUE) == TRUE ~ "(lum)"),
                    Function2 = dplyr::case_when(!is.na(Units) ~ paste(Function,Units),
                                                 TRUE ~ Function))
    names(hoboFile) <- hoboNames$Function
    convert <- subset(hoboNames,Function == "Temperature")$Units
    timeCheck <- ifelse(grepl("AM",hoboFile$DateTime[1], ignore.case = TRUE) == TRUE |
                          grepl("PM",hoboFile$DateTime[1], ignore.case = TRUE) == TRUE,"12Hour",NA)
    light <- nrow(subset(hoboNames,Function == "Light")) != 0
    hoboFile <- hoboFile[as.numeric(subset(hoboNames,!is.na(Function))$Index)]

    hoboFile %<>%
      dplyr::filter(!is.na(Temperature)) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(Temperature = ifelse(convert == "°F",weathermetrics::fahrenheit.to.celsius(Temperature),Temperature)) %>%
      dplyr::rename(`Temperature °C` = Temperature) %>%
      dplyr::ungroup()

    if(is.na(timeCheck) == FALSE) {
      hoboFile %<>%
        dplyr::mutate(DateTime = as.POSIXct(DateTime, format="%m/%d/%y %I:%M:%S %p"))
      }

    if(light == TRUE){
      hoboFile %<>%
        dplyr::mutate(Light = as.integer(Light))
    }
  }

  ####hoboFile Diagnostic####
  if (is.data.frame(path) == TRUE){
    hoboFile <- readInRename(path)
  } else if (dir.exists(path) == TRUE) {
    hoboList <- list.files(path, full.names = TRUE) %>%
      lapply(rio::import) %>%
      lapply(readInRename)
    names(hoboList) <- basename(list.files(path,full.names = TRUE))
    hoboFile <- hoboList %>%
      purrr:::map_dfr(plyr::rbind.fill, .id="Group") %>%
      unique()
    groupingVariable = "Group"
  } else if (file.exists(path) == TRUE){
    hoboFile <- readInRename(rio::import(path))
  } else {print("Import failed.")}


  ####Calculation Math####
  if (calc == TRUE) {
    if (is.na(groupingVariable) == FALSE) {
      calcTemperatures <- hoboFile %>%
        dplyr::group_by(Group) %>%
        dplyr::arrange(DateTime) %>%
        dplyr::mutate(lagTime = dplyr::lag(DateTime,1),
                      diffTime = as.integer(difftime(DateTime,lagTime, units = "mins")),
                      Date = lubridate::date(DateTime)) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(Group, Date) %>%
        dplyr::mutate(sumMinutes = sum(diffTime,na.rm=TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(percentMinutes = diffTime/sumMinutes,
                      diffTemp = ifelse(`Temperature °C` >= (MMM+anomaly), `Temperature °C` - MMM, 0),
                      percentTemp = diffTemp * percentMinutes) %>%
        dplyr::group_by(Group, Date) %>%
        dplyr::arrange(Date) %>%
        dplyr::mutate(dhDay = sum(percentTemp, na.rm=TRUE)) %>%
        dplyr::ungroup()

      hoboSummary <- hoboFile %>%
        dplyr::mutate(Date = lubridate::date(DateTime)) %>%
        dplyr::group_by(Group, Date) %>%
        dplyr::summarise(Temperature_Average = ave(`Temperature °C`,na.rm=TRUE),
                       Temperature_StDev = sd(`Temperature °C`,na.rm=TRUE)) %>%
        unique() %>%
        dplyr::ungroup() %>%
        dplyr::left_join(unique(calcTemperatures[c("Date", "dhDay", "Group")]), by = c("Date", "Group")) %>%
        dplyr::mutate(dhWeek=dhDay*(1/7)) %>%
        dplyr::group_by(Group) %>%
        dplyr::arrange(Date) %>%
        dplyr::mutate(DHWs = zoo::rollapplyr(dhWeek, (7*12), sum, partial = TRUE))

      hoboComplete <- hoboFile %>%
        dplyr::mutate(Date = lubridate::date(DateTime)) %>%
        dplyr::left_join(hoboSummary[c("Date","dhDay","DHWs", "Group")], by = c("Date", "Group")) %>%
        dplyr::select(-c(Date))
    } else {
      calcTemperatures <- hoboFile %>%
        dplyr::arrange(DateTime) %>%
        dplyr::mutate(lagTime = dplyr::lag(DateTime,1),
                      diffTime = as.integer(difftime(DateTime,lagTime, units = "mins")),
                      Date = lubridate::date(DateTime)) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(Date) %>%
        dplyr::mutate(sumMinutes = sum(diffTime,na.rm=TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(percentMinutes = diffTime/sumMinutes,
                      diffTemp = ifelse(`Temperature °C` >= (MMM+anomaly), `Temperature °C` - MMM, 0),
                      percentTemp = diffTemp * percentMinutes) %>%
        dplyr::group_by(Date) %>%
        dplyr::arrange(Date) %>%
        dplyr::mutate(dhDay = sum(percentTemp, na.rm=TRUE)) %>%
        dplyr::ungroup()

      hoboSummary <- hoboFile %>%
        dplyr::mutate(Date = lubridate::date(DateTime)) %>%
        dplyr::group_by(Date) %>%
        dplyr::summarise(Temperature_Average = ave(`Temperature °C`,na.rm=TRUE),
                       Temperature_StDev = sd(`Temperature °C`,na.rm=TRUE)) %>%
        unique() %>%
        dplyr::ungroup() %>%
        dplyr::left_join(unique(calcTemperatures[c("Date", "dhDay")]), by = c("Date")) %>%
        dplyr::mutate(dhWeek=dhDay*(1/7)) %>%
        dplyr::arrange(Date) %>%
        dplyr::mutate(DHWs = zoo::rollapplyr(dhWeek, (7*12), sum, partial = TRUE))

      hoboComplete <- hoboFile %>%
        dplyr::mutate(Date = lubridate::date(DateTime)) %>%
        dplyr::left_join(hoboSummary[c("Date","dhDay","DHWs")], by = "Date") %>%
        dplyr::select(-c(Date))
    }
  } else if (calc == FALSE){
    if (is.na(groupingVariable) == FALSE) {
      hoboSummary <- hoboFile %>%
        dplyr::mutate(Date = lubridate::date(DateTime)) %>%
        dplyr::group_by(Group, Date) %>%
        dplyr::summarise(Temperature_Average = ave(`Temperature °C`,na.rm=TRUE),
                       Temperature_StDev = sd(`Temperature °C`,na.rm=TRUE)) %>%
        unique()

      hoboComplete <- hoboFile
    } else {
      hoboSummary <- hoboFile %>%
        dplyr::mutate(Date = lubridate::date(DateTime)) %>%
        dplyr::group_by(Date) %>%
        dplyr::summarise(Temperature_Average = ave(`Temperature °C`,na.rm=TRUE),
                       Temperature_StDev = sd(`Temperature °C`,na.rm=TRUE)) %>%
        unique()

      hoboComplete <- hoboFile
    }
  }

  if (summary == TRUE){
    hoboSummary %>%
      return(.)
  }else if (summary == FALSE){
    hoboComplete %>%
      return(.)
  }
}
