library(shiny)
library(shinydashboard)
library(tidyverse)
library(reactable)
library(fs)

t_drive <- gsub("\\\\", "/", r"(\\oneabbott.com\dept\ADC\Technical_OPS\Clinical_Affairs\CDM_23238\)")
# study event
event <- dir_ls(gsub("\\\\", "/", r"(\\wf00168p.oneabbott.com\data1\CDM\ADC-US-RES-23238\OpenClinicaExtract\Current)")) |>
         str_extract("(?<=Current/).{3}")

ui <- dashboardPage(
        dashboardHeader(title = "Filename Quality"),
        dashboardSidebar(selectInput("event", "Select Event Number", event),
                         textInput("event1","Input Event Number")
                         ),
        dashboardBody(reactableOutput("table"),waiter::use_waiter())
)

server <- function(input, output) {

  # Directory
  text <- reactive({
          req(input$event)
            waiter <- waiter::Waiter$new()
            waiter$show()
            on.exit(waiter$hide())
            # Sys.sleep(sample(5, 1))
            # runif(1)
          
          file_list <- dir_ls(str_c(t_drive,input$event),recurse = T,glob = "*extracted_realm.zip|*events.csv|*devices.csv")
          # Filter Path
          if (any(str_detect(file_list,"CDM_23238"))){
              file_path <- file_list[!str_detect(file_list,regex("Transfers|Transfer|Archive|Archives",ignore_case = T))]
              data <- tibble(Path = str_extract(str_remove_all(file_path,"[:blank:]"),regex("(?<=Clinical_Affairs/).+|(?<=Upload Data/).+")),
                             Site  = str_extract(Path,regex("(?<=/Apol)[:upper:]{3}|(?<=/Mobi)[:upper:]{3}|(?<=/[:alnum:]{2}_)[:upper:]{3}")),
                            `Subject ID` = str_extract(Path,regex("(?<=/ApolADC)[:digit:]{4}|(?<=/MobiADC)[:digit:]{4}|(?<=/[:alnum:]{2}_ADC)[:digit:]{4}")),
                            `Condition ID` = str_extract(Path,regex("(?<=ApolADC[:digit:]{10}_)[:alnum:]+|(?<=MobiADC[:digit:]{10}_)[:alnum:]+|(?<=[:alnum:]{2}_ADC[:alnum:]{4}_)[:alnum:]+")),
                             Date = ymd(str_extract(Path,regex("(?<=_)[:digit:]{6}"))),
                             Time = hms::as_hms(str_c(
                                str_extract(Path,regex("(?<=_[:digit:]{6}_)[:digit:]{2}")),
                                str_extract(Path,regex("(?<=_[:digit:]{6}_[:digit:]{2})[:digit:]{2}")),
                                str_extract(Path,regex("(?<=_[:digit:]{6}_[:digit:]{4})[:digit:]{2}")),sep = ":")),
                             Type = str_extract(Path,"AUU|UUU|GKS"),
                             Sensor = str_extract(Path,"[:alnum:]{2}(?=_ADC)"),
                             Visit = str_extract(Path,regex("FV|V3|V4|V5|Visit3|Visit4|Visit5|Visit_3|Visit_4|Visit_5|C1|C2|C3|C4|Exit|Round1|Round2",ignore_case = T))) |> 
                        distinct(pick(c(Site:Time)),.keep_all = T) |> 
                        mutate(Count = n(),.by = c(Type,`Subject ID`,`Condition ID`,Visit))
              # Count the most number of sensors  
              most <- data |> 
                      count(Sensor) |> 
                      slice_max(n) |> 
                      pull(Sensor)      
            data <- data |>  
                    mutate(Status = case_when(
                    str_length(`Subject ID`) != median(str_length(`Subject ID`),na.rm = T) ~ "No Good: Please Check Subject ID",
                    str_length(`Condition ID`) != 3 |
                      str_detect(`Condition ID`,"[:lower:]") |
                      str_detect(`Condition ID`,"[:digit:]{3}") ~ "No Good: Please Check Condition ID",
                    is.na(`Subject ID`) | is.na(`Condition ID`) ~ "No Good: Please Check Subject ID and Condition ID",
                    str_extract(Site,"[:digit:]{3}") != str_sub(`Subject ID`,1,3) ~ "No Good: Put files on the wrong Site folders",
                    Sensor != most ~ "No Good: Wrong sensor type",
                    Count != 1 ~ "No Good: Duplicated Uploads",
                    .default = "Good")) |> 
                  arrange(`Subject ID`)}
          
          else {
              file_path <- file_list[!str_detect(file_list,regex("Transfers|Transfer|Archive|Archives|Final",ignore_case = T))]
          
              data <- tibble(Path = str_extract(str_remove_all(file_path,"[:blank:]"),regex("(?<=UploadData/).+")),
                             Site  = str_c(str_extract(Path,regex("(?<=DataFiles/)[:alnum:]+",ignore_case = T)),
                                         str_extract(Path,regex("(?<=DataFiles/[:alnum:]{3,4}[:punct:]{1})[:alnum:]+",ignore_case = T)),
                                         sep = "-"),
                            `Subject ID` = str_extract(Path,regex("(?<=/Mobi)[:digit:]+|(?<=/Atna)[:digit:]+|(?<=/Apol)[:digit:]+")),
                            `Condition ID` = str_extract(Path,regex("(?<=[:punct:]{7})[:alnum:]+")),
                             Date = ymd(str_extract(Path,regex("(?<=_)[:digit:]{6}"))),
                             Time = hms::as_hms(str_c(
                                    str_extract(Path,regex("(?<=_[:digit:]{6}_)[:digit:]{2}")),
                                    str_extract(Path,regex("(?<=_[:digit:]{6}_[:digit:]{2})[:digit:]{2}")),
                                    str_extract(Path,regex("(?<=_[:digit:]{6}_[:digit:]{4})[:digit:]{2}")),sep = ":")),
                             Type = str_extract(Path,"[:alpha:]+"),
                              Visit = str_extract(Path,regex("FV|V3|V4|V5|Visit3|Visit4|Visit5|Visit_3|Visit_4|Visit_5|C1|C2|C3|C4|Exit",ignore_case = T))) |>
              # Remove dualsensors_events.csv and dualsensors_extracted_realm.zip
                        distinct(pick(c(Site:Time)),.keep_all = T) |> 
                        mutate(Count = n(),.by = c(Type,`Subject ID`,`Condition ID`,Visit)) |>
                        mutate(Status = case_when(
                          str_length(`Subject ID`) != median(str_length(`Subject ID`),na.rm = T) ~ "No Good: Please Check Subject ID",
                          str_length(`Condition ID`) != 3 |
                            str_detect(`Condition ID`,"[:lower:]") |
                            str_detect(`Condition ID`,"[:digit:]{3}") ~ "No Good: Please Check Condition ID",
                          is.na(`Subject ID`) | is.na(`Condition ID`) ~ "No Good: Please Check Subject ID and Condition ID",
                          str_extract(Site,"[:digit:]{3}") != str_sub(`Subject ID`,1,3) ~ "No Good: Put files on the wrong Site folders",
                          Count != 1 ~ "No Good: Duplicated Uploads",
                          .default = "Good")) |> 
                        arrange(`Subject ID`)}})
  
          output$table <- renderReactable({reactable(text(),paginationType = "jump", striped = TRUE, highlight = TRUE, searchable = TRUE, filterable = TRUE,defaultPageSize = 6, resizable = TRUE,defaultColDef = colDef(align = "center",minWidth = 115))})
           
      
}

shinyApp(ui, server)

# https://stackoverflow.com/questions/73247832/download-data-into-excel-from-r-shiny-table-created-with-reactable