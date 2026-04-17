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

data.aa<-read.delim("C:/Users/rache/Downloads/work stuff/research/Philben/AAtable2.txt")

table.aa<-setDT(data.aa)                #make the data table
table.aa                                #just to see the table


#---------------------------------------------------------------------------------------------------------
# Create a gt table to fix decimals

gt_tableaa<-gt(table.aa)%>%
  fmt_number(
    decimals=1,
    use_seps = FALSE)%>%
  tab_header(
    title = "Table 2",
    subtitle = "Data table of amino acid values used to calculate the following
    amino acid-based variables: amino acid degradation index (AAIndex), molar 
    % of total amino acids as hydroxyproline (%Hyp), the molar % of glycine (%Glycine), 
    and the C-normalized yield of hydroxyproline (HypC). AAIndex was caluclated
    via principal components analysis excluding the hydroxproline values. %Glycine 
    was also calculated excluding the hydroxyproline values. All amino acid values
    are reported as C-normalized yields with units of nmol/mg C."
  )%>%
  cols_label(
    Aspartic.Acid = "Aspartic Acid",
    Glutamic.acid = "Glutamic Acid",
    Total...Hyp. = "Total (+Hyp)",
    Total...Hyp..1 = "Total (-Hyp)"
  )%>%
  tab_style(
    style = list(
      cell_text(align = "center", weight = "bold", font = "TimesNewRoman", size = 12),
      cell_borders(
        sides = c("t","b"),
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
      cell_text(font = "TimesNewRoman", size = 12, align = "left")
    ),
    locations = cells_title("subtitle") #this doesn't seem to control line between subtitle and labels
  )%>%
  opt_table_outline(style = "none")%>%     #this gets rid of the outer borders/outlines around whole table
  opt_table_lines(extent="none")           #gets rid of table lines inside the table (not borders apparently??)

gt_tableaa                            

#--------------------------------------------------------------------------------------------------------------------------------------
#export the table --> can change ending in filename to change file type (png,docx,html,etc) -- html was only that worked for the size of this table
gtsave(gt_tableaa, filename = "bioavail.tableaa.html", path="C:/Users/rache/Downloads/work stuff/research/Philben")

