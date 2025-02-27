library(flowchart)
library(tidyverse)


interviews <- data.frame(
  stringsAsFactors = FALSE,
                          School = c("Rose Stein","Foster","Deane","Weber",
                                     "Secrest","Welchester","Lumberg","Fremont",
                                     "Westgate","Fremont","Eiber","Leawood",
                                     "Stevens","Hutchinson","Deane","Deane",
                                     "Deane","Foster","Westgate","Westgate",
                                     "Weber","Weber","Fremont","Fremont",
                                     "Secrest","Welchester","Stober","Stober",
                                     "Hutchinson","Little","Little",
                                     "Bear Creek K8","Governor’s Ranch","Lukas",
                                     "Van Arsdale","Stober","Governor’s Ranch",
                                     "Little","Weber","Deane","Stott","Stott",
                                     "Shelton","Shelton","Secrest","Fremont",
                                     "Lukas"),
                            Role = c("Teacher","Teacher","Teacher","Teacher",
                                     "Teacher","Teacher","Teacher","Teacher",
                                     "Teacher","Teacher","Teacher","Teacher",
                                     "Teacher","Teacher","Instructional Coach",
                                     "Literacy Interventionist",
                                     "Literacy Interventionist","Instructional Coach",
                                     "Instructional Coach",
                                     "Literacy Interventionist","Instructional Coach",
                                     "Literacy Interventionist","Instructional Coach",
                                     "Literacy Interventionist","Instructional Coach",
                                     "Literacy Interventionist",
                                     "Instructional Coach",NA,"Instructional Coach",
                                     "Instructional Coach","Literacy Interventionist",
                                     "Instructional Coach","Instructional Coach",
                                     "Instructional Coach",
                                     "Instructional Coach","Principal","Principal","Principal",
                                     "Principal","Principal",
                                     "Principal","Instructional Coach","Principal",
                                     "Instructional Coach","Principal",
                                     "Principal","Principal"),
                  Title.I.Funded = c("X",
                                     "X","X",NA,"X","X","X",NA,"X",NA,
                                     "X",NA,NA,NA,"X","X","X","X","X",
                                     "X",NA,NA,NA,NA,"X","X",NA,NA,NA,
                                     "X","X","X",NA,NA,NA,NA,NA,"X",NA,
                                     "X",NA,NA,NA,NA,NA,NA,NA),
                        Response = c(NA,
                                     NA,"26-Feb",NA,NA,NA,"No Show",NA,NA,
                                     NA,NA,NA,NA,NA,NA,NA,"No Show",
                                     "25-Feb",NA,"No Show","14-Feb","14-Feb",
                                     NA,"11-Feb","2/25 ","14-Feb",NA,NA,
                                     "14-Feb",NA,NA,NA,"13-Feb","25-Feb",NA,
                                     NA,"13-Feb","Requested interview",NA,
                                     "26-Feb","12-Feb","12-Feb",NA,NA,NA,
                                     "25-Feb","25-Feb")
              )|>  
  mutate(agreetoPartipate = ifelse(!is.na(Response), 'Agreeed to participate', 'Did not agree to participate')) %>% 
  mutate(participated = ifelse(!is.na(Response) & Response != 'No Show', 'Participated', "Did not participate"))

interviewsChart <- interviews |> 
  as_fc(label = "Inteview Invitations")|>
  fc_filter(!is.na(Role), 
            label = "Current employees", 
            show_exc = TRUE, 
            direction_exc = "left",
            label_exc = "No longer an employee",
            round_digits = 0) |>
  fc_split(Role, 
           round_digits = 0) |>
  fc_split(participated,
           round_digits = 0,
           offset = 0) |>
  # fc_filter(!is.na(participated), 
  #           label = "participated", 
  #           show_exc = F, 
  #           round_digits = 0, 
  #           perc_total = T) |>
  fc_draw(title = "Educator Interview Participation")

interviewsChartSummary <- interviews |> 
  as_fc(label = "Inteview Invitations")|>
  fc_filter(!is.na(Role),
            label = "Current employees",
            show_exc = F,
            direction_exc = "left",
            label_exc = "No longer an employee",
            round_digits = 0) |>
  # fc_split(Role, 
  #          round_digits = 0) |>
  fc_split(participated,
           title = "Participation Status",
           bg_fill_title = 'grey',
           round_digits = 0,
           offset = 0) |>
  # fc_filter(!is.na(participated), 
  #           label = "participated", 
  #           show_exc = F, 
  #           round_digits = 0, 
  #           perc_total = T) |>
  fc_draw(title = "Educator Interview Participation Summary")


schoolSummary <- interviews %>% 
  select(School, participated, Title.I.Funded) %>% 
  mutate(Title.I.Funded = if_else(Title.I.Funded == 'X', 
                                 'Title I Funded School', 
                                 'No', 
                                 missing = "Not Title I Funded School"))

schoolParticipation <- schoolSummary |> 
  as_fc(label = "Inteview Invitations")|>
  # fc_filter(!is.na(School),
  #           label = "Current employees",
  #           show_exc = T,
  #           direction_exc = "left",
  #           label_exc = "No longer an employee",
  #           round_digits = 0) |>
  fc_split(Title.I.Funded, 
           # title = "Title One Funded",
           # bg_fill_title = 'grey',
           round_digits = 0)|>
  # fc_split(School,
  #          round_digits = 0) |>
  fc_split(participated,
           # title = "Participation Status",
           # bg_fill_title = 'grey',
           round_digits = 0,
           offset = 0) |>
  # fc_filter(!is.na(participated),
  #           label = "participated",
  #           show_exc = F,
  #           round_digits = 0,
  #           perc_total = T) |>
  fc_draw(title = "Educator Interview Participation Summary")



