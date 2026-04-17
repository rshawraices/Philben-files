#### MAKING A PRETTY TABLE FOR PUBLICATION - BIOAVAILABILITY PAPER - 8/26/2025 ####

#---------------------------------------------------------------------------------------------------------
#### gt resources: general -- https://rstudio.r-universe.dev/gt/doc/manual.html#cells_title, https://www.rdocumentation.org/packages/gt/versions/1.0.0
##                 location helpers -- https://gt.rstudio.com/reference/cells_summary.html;
##                 making different tables -- https://medium.com/number-around-us/table-it-like-a-pro-print-ready-tables-in-r-ff1856611008

## Obsolete code for replacing NA with " ":
 #datanona<- data %>%
   #mutate(across(everything(), ~ replace(.x, is.na(.x), " "))) #replacing NA values (units) with " "


#---------------------------------------------------------------------------------------------------------
#install packages

install.packages("gt")                   #main table maker
library(gt)
library(ggplot2)                         #other side of making tables
library(devtools)                        #may be unnecessary -- may be from when I was updating R, Rstudio, and Rtools
library(dplyr)                           #basic functions
install.packages("reporter")             #I think this is the one that let's you do sub/superscripts
library(reporter)
install.packages("data.table")           #Install and load to make data tables
library("data.table")
install.packages("tidyverse")            #Install to be able to replace the NAs
library(tidyverse)
install.packages("chromote")             #Need chromote in order to export the table I believe
library(chromote)
#may need to do following if it lost the path to chrome (and correct path if different):
# Sys.setenv(
#   CHROMOTE_CHROME = "C:/Users/rache/AppData/Local/Google/Chrome/Application/chrome.exe"
#   )
# chromote_info()
# find_chrome()                          #unnecessary, but may find usable chromes

#---------------------------------------------------------------------------------------------------------
#making a table out of a .txt file

data<-read.delim("C:/Users/rache/Downloads/work stuff/research/Philben/table3.txt")

                                         #make all the number columns numeric instead of characters (to control decimals)
as.numeric(data$Depth,data$pH,data$C.N,data$TotalOrgAcids,data$TotalInorgN,
           data$X.CSugars,data$X.Glucose,data$X.Rhamnose,data$THAA.N,data$AAIndex,
           data$THAA.C,data$X.Hyp,data$X.Glycine,data$HypC,data$AnaCO2,data$AerCO2,data$CH4)

tablenum<-setDT(data)                    #make the data table
tablenum                                 #just to see the table


#---------------------------------------------------------------------------------------------------------
# Create a gt table to fix decimals

gt_tableunits<-gt(tablenum)%>%
  fmt_number(columns = "pH", decimals=1)%>%
  fmt_number(columns = c("Depth","C.N","TotalOrgAcids","TotalInorgN","X.CSugars",
             "X.Glucose","X.Rhamnose","THAA.N","AAIndex","THAA.C","X.Hyp",
             "X.Glycine","HypC","AnaCO2","AerCO2","CH4"), decimals=2)

gt_tableunits                            #just to test it

#---------------------------------------------------------------------------------------------------------
#turn back into a dataframe to turn into characters (necessary to add units row)

tablechar<-as.data.frame(gt_tableunits)
as.character(tablechar$Depth,tablechar$pH,tablechar$C.N,tablechar$TotalOrgAcids,
             tablechar$TotalInorgN,tablechar$X.CSugars,tablechar$X.Glucose,
             tablechar$X.Rhamnose,tablechar$THAA.N,tablechar$AAIndex,tablechar$THAA.C,
             tablechar$X.Hyp,tablechar$X.Glycine,tablechar$HypC,tablechar$AnaCO2,
             tablechar$AerCO2,tablechar$CH4)
tablechar                                #just to test it
is.character(tablechar$pH)               #test a variable

#---------------------------------------------------------------------------------------------------------
#turn it back into a datatable

setDT(tablechar)
is.data.table(tablechar)                 #see if it worked


#---------------------------------------------------------------------------------------------------------
#create a gt table, add "units" row, and fix the style

gt_table <- gt(tablechar) %>%
  tab_header(
    title = "Table 1",
    subtitle = "Data table of values used in respiration, PCA, and multiple regression analyses using the following environmental and chemical parameters: microtopography, depth, pH, C:N, total organic acid content (TotalOrgAcids), total inorganic N content (TotalInorgN), % of total C as sugars (%CSugars), % of total sugars as glucose (%Glucose), % of total sugars as rhamnose (%Rhamnose), % of total N as amino acids (THAA%N), the amino acid degradation index (AAIndex), % of total C as amino acids (THAA%C), molar % of total amino acids as hydroxyproline (%Hyp), the molar % of glycine (%Glycine), the C-normalized yield of hydroxyproline (HypC), aerobic CO" %p% subsc('2') %p% " production (AerCO" %p% subsc('2') %p%"), anaerobic CO"%p% subsc('2') %p%" production (AnaCO"%p% subsc('2') %p%"), and CH"%p% subsc('4') %p%" production (CH"%p% subsc('4') %p%")."
  ) %>%
  cols_label(
    Depth = "Depth",
    C.N = "C:N",
    TotalOrgAcids = "TotalOrgAcids",
    TotalInorgN = "TotalInorgN",
    X.CSugars = "%CSugars",
    X.Glucose = "%Glucose",
    X.Rhamnose = "%Rhamnose",
    THAA.N = "THAA%N",
    THAA.C = "THAA%C",
    X.Hyp = "%Hyp",
    X.Glycine = "%Glycine",
    AnaCO2 = "AnaCO" %p% subsc('2'),
    AerCO2 = "AerCO" %p% subsc('2'),
    CH4 = "CH"%p% subsc('4')
  )%>%
  rows_add(
    #.data =gt_table,
    Sample = " ",
    Microtopography = " ",
    Depth = "m",
    pH = " ",
    C.N = " ",
    TotalOrgAcids = "µmol/gdw",
    TotalInorgN = "µmol/gdw",
    X.CSugars = " ",
    X.Glucose = " ",
    X.Rhamnose = " ",
    THAA.N = " ",
    AAIndex = " ",
    THAA.C = " ",
    X.Hyp = " ",
    X.Glycine = " ",
    HypC = "nmol/mgC",
    AnaCO2 = "µmol/gdw/day",
    AerCO2 = "µmol/gdw/day",
    CH4 = "nmol/gdw/day",
    .before = c(1)
  )%>%
  tab_style(
    style = list(
      cell_text(align = "center", weight = "bold", font = "TimesNewRoman", size = 12),
      cell_borders(
        sides = "t",
        color = "black",
        weight = px(1.5)
      )
    ),
    locations = cells_column_labels() #controls borders but only if you got rid of the table lines like I did at the end??
  ) %>%
  tab_style(
    style = list(
      cell_text(align = "left", weight = "bold", font = "TimesNewRoman", size = 12)
      ),
    locations = cells_title("title")
  ) %>%
  tab_style(
    style = list(
      cell_text(font = "TimesNewRoman", size = 12, align = "center")
    ),
    locations = cells_body() #this doesn't control line between labels
  ) %>%
  tab_style(
    style = list(
      cell_text(font = "TimesNewRoman", size = 12, align = "center"),
      cell_borders(sides = "b", color = "black", weight=px(1.5))
    ),
    locations = cells_body(columns = everything(), rows = c(1)) #adds a line under the units row
  ) %>%
  tab_style(
    style = list(
      cell_text(font = "TimesNewRoman", size = 12, align = "left")
    ),
    locations = cells_title("subtitle") #this doesn't seem to control line between subtitle and labels
  )%>%
  opt_table_outline(style = "none")%>%     #this gets rid of the outer borders/outlines around whole table
  opt_table_lines(extent="none")           #gets rid of table lines inside the table (not borders apparently??)


#--------------------------------------------------------------------------------------------------------------------------------------
# Print the table
gt_table

#--------------------------------------------------------------------------------------------------------------------------------------
#export the table --> can change ending in filename to change file type (png,docx,html,etc) -- html was only that worked for the size of this table
gtsave(gt_table, filename = "bioavail.table.4.docx", path="C:/Users/rache/Downloads/work stuff/research/Philben")



#--------------------------------------------------------------------------------------------------------------------------------------
#to make a .csv file, need to convert to tibble first -- IN PROGRESS, STUCK ON MAKING THE TIBBLE
install.packages("gtsummary")
library(gtsummary)
install.packages("writexl")
library(writexl)

gt_table %>%
 gtsummary::as_tibble(gt_table, include=everything(),cols_label(TRUE),fmt_missing=FALSE,return_calls=FALSE, validate =NULL, .name_repair = "unique")
#^stuck here, column lengths keep not being same length??

#reference code: https://stackoverflow.com/questions/69400178/how-to-save-a-gt-table-to-either-an-excel-or-word-file
gt_table %>%
 gtsummary::as_tibble() %>% 
  writexl::write_xlsx(., "example_gtsummary1.xlsx")
 
#tried this but not a data.frame so doesn't work
write_excel_csv(gt_table,file="C:/Users/rache/Downloads/work stuff/research/Philben/bioavail.table.csv")

?gtsummary
?as_tibble
install.packages("gtsummary")
library(gtsummary)
install.packages("writexl")
library(writexl)
?write.csv
?write_csv
?write_xlsx
?as_hux_xlsx
?recycle_columns
?tbl_wide_summary
