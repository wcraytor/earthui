################################################   Setup ##########################################
# Author:       Wm. Bert Craytor
# Location:     243 Clifton Rd., Pacifica, CA 94044, USA
# Date:         07/02/2021
# Description:  Setup script to generate MARS (earth)  analysis for appraisal data using R:earth
################################################ KVEsfr_Utilities.R ##############################
#
# Notes:        1.  This program is free software; you can redistribute it and/or modify
#                   it under the terms of the GNU General Public License as published by
#                   the Free Software Foundation; either version 3 of the License, or
#                   (at your option) any later version.
#
#               2.  This program is distributed in the hope that it will be useful,
#                   but WITHOUT ANY WARRANTY; without even the implied warranty of
#                   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#                   GNU General Public License for more details.
#
#               3.  A copy of the GNU General Public License is available at
#                   http://www.r-project.org/Licenses
#
#               4.   Ref:  https://ycphs.github.io/openxlsx/index.html
#
#
# Purpose:  You need to:
#            1.  Make a copy of the MlsData.xlsx file in your version output folder to "MlsComps.xlsx".  Then rename "Sheet1" to "X".
#            2.  Then rename "Sheet1" to "X".  - As you may wind-up making a lot of other worksheets for other calcalations.  The "X" sheet is to create the
#                "RcaInput" worksheet for input to the Rca URAR form, using Rca's worksheet feature.
#            3.  The first part of the RunModul2.R script is almost the same as the first part of the RunModule1.R script.  Basically it set sup
#                the proj
#
###################################################################################################
# library(readxl)
# library(readr)
# library(openxlsx)
# library(magrittr)
# library(dplyr)
# library(pryr)
# library(tidyverse)

# library(lubridate)
# library(tibble)
# library(ggplotAssist)
# library("writexl")
# library("Formula")
# library("partykit")
# library("MASS")
# library(collections)
# library(Rcpp)
# library(DBI)
# library(RSQLite)
# library(here)

#'  Load the Mappings (or configuation file) to a Data Frame
library(openxlsx)

#' Initial call is here to set up the environment under projEnv
#'  @projEnv Project Environment
#'  @codeFolder codeFolder,
#'  @projectParentFolder Project files parent folder
#'  @projectID Project ID
#'  @mlsComps MLS Data File
#'  @RcaInput Rca Input File
#' Initial call is here to set up the environment under projEnv
#' #export


# Meant to create border around entire projEnv$RcaWB.  But doesn't work as advertised
OutsideBorders <-
  function(wb_,
           sheet_,
           rows_,
           cols_,
           border_col = "#002060",
           border_thickness = "thick") {
    left_col = 1
    right_col = 20
    top_row = 1
    bottom_row = 43
    
    sub_rows <- list(c(bottom_row:top_row),
                     c(bottom_row:top_row),
                     top_row,
                     bottom_row)
    
    sub_cols <- list(left_col,
                     right_col,
                     c(left_col:right_col),
                     c(left_col:right_col))
    
    directions <- list("Left", "Right", "Top", "Bottom")
    
    mapply(function(r_, c_, d) {
      temp_style <- createStyle(border = d,
                                borderColour = border_col,
                                borderStyle = border_thickness)
      addStyle(
        wb_,
        sheet_,
        style = temp_style,
        rows = r_,
        cols = c_,
        gridExpand = TRUE,
        stack = TRUE
      )
      
    }, sub_rows, sub_cols, directions)
  }

 
 
InitDataFrames <-
  function() {
    print("InitDataFrames")
    projEnv$MlsCompsDF <- data.frame()
    projEnv$RcaInputDF <- data.frame()
  }
  
RcaCreateWorksheets <- function() {
  projEnv$RcaWB <- createWorkbook()
  addWorksheet(projEnv$RcaWB, "Comps 1-3")
  addWorksheet(projEnv$RcaWB, "Comps 4-6")
  addWorksheet(projEnv$RcaWB, "Comps 7-9")
  addWorksheet(projEnv$RcaWB, "Comps 10-12")
  projEnv$Sheets <- sheets(projEnv$RcaWB)
}


# Set Sales Grid Workbook Style
RcaWBStyle <- function() {
  print("Set SetprojEnv$RcaWBStyle")
  options("openxlsx.borderStyle" = "thin")
  options("openxlsx.borderColour" = "#002060")
   options("openxlsx.border" = "#TopBottomLeftRight")
     
  options("openxlsx.numFmt" = "$#,##0")
  modifyBaseFont(projEnv$RcaWB, fontSize = 9, fontName = "Arial Narrow")

  # light Blue:  #CCCCFF
  # Dardk blue   #002060
  # Light Green  #E2EFDA
  # Light Gray   #F2F2F2
  hs1 <-
    createStyle(
      fgFill = "#4F81BD",
      halign = "CENTER",
      textDecoration = "Bold",
      border = "Bottom",
      fontColour = "white"
    )

  # headerStyle <- createStyle(
  # fontSize = 14,
  # fontColour = "#FFFFFF",
  # halign = "center",
  # fgFill = "#4F81BD",
  # border = "TopBottom",
  # borderColour = "#4F81BD"
  # )

  colWidths <-
    c(29, 11, 8, 4, 12, 11, 8, 7, 12, 11, 11, 8, 7, 12, 11, 11, 8, 7, 12, 11)
  cols <-
    c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)

  for (k in 1:4) {
  	 
    setColWidths(projEnv$RcaWB, k, cols, widths = colWidths)

    mergeCells(projEnv$RcaWB, k, cols = 1:20, rows = 2:2)
    mergeCells(projEnv$RcaWB, k, cols = 2:5, rows = 3:3)
    mergeCells(projEnv$RcaWB, k, cols = 6:10, rows = 3:3)
    mergeCells(projEnv$RcaWB, k, cols = 11:15, rows = 3:3)
    mergeCells(projEnv$RcaWB, k, cols = 16:20, rows = 3:3)
	
    mergeCells(projEnv$RcaWB, k, cols = 2:5, rows = 4:4)
    mergeCells(projEnv$RcaWB, k, cols = 6:10, rows = 4:4)
    mergeCells(projEnv$RcaWB, k, cols = 11:15, rows = 4:4)
    mergeCells(projEnv$RcaWB, k, cols = 16:20, rows = 4:4)
	
    mergeCells(projEnv$RcaWB, k, cols = 3:4, rows = 5:5)
    mergeCells(projEnv$RcaWB, k, cols = 7:8, rows = 5:5)
	mergeCells(projEnv$RcaWB, k, cols = 9:10, rows = 5:5)  
    mergeCells(projEnv$RcaWB, k, cols = 12:13, rows = 5:5)
	mergeCells(projEnv$RcaWB, k, cols = 14:15, rows = 5:5)  
    mergeCells(projEnv$RcaWB, k, cols = 17:18, rows = 5:5)
	mergeCells(projEnv$RcaWB, k, cols = 19:20, rows = 5:5)  
	
    mergeCells(projEnv$RcaWB, k, cols = 2:4, rows = 6:6)
    mergeCells(projEnv$RcaWB, k, cols = 6:8, rows = 6:6)
    mergeCells(projEnv$RcaWB, k, cols = 11:13, rows = 6:6)
    mergeCells(projEnv$RcaWB, k, cols = 16:18, rows = 6:6)
	  
    mergeCells(projEnv$RcaWB, k, cols = 2:4, rows = 7:7)
    mergeCells(projEnv$RcaWB, k, cols = 6:8, rows = 7:7)
    mergeCells(projEnv$RcaWB, k, cols = 11:13, rows = 7:7)
    mergeCells(projEnv$RcaWB, k, cols = 17:18, rows = 7:7)
	
	mergeCells(projEnv$RcaWB, k, cols = 2:4, rows = 8:8)
    mergeCells(projEnv$RcaWB, k, cols = 6:8, rows = 8:8)
    mergeCells(projEnv$RcaWB, k, cols = 11:13, rows = 8:8)
    mergeCells(projEnv$RcaWB, k, cols = 17:18, rows = 8:8)
    ##########################
	# No merge on 9
    mergeCells(projEnv$RcaWB, k, cols = 3:4, rows = 10:10)
    mergeCells(projEnv$RcaWB, k, cols = 7:8, rows = 10:10)
    mergeCells(projEnv$RcaWB, k, cols = 12:13, rows = 10:10)
    mergeCells(projEnv$RcaWB, k, cols = 17:18, rows = 10:10)
	
    mergeCells(projEnv$RcaWB, k, cols = 3:4, rows = 11:11)
    mergeCells(projEnv$RcaWB, k, cols = 7:8, rows = 11:11)
    mergeCells(projEnv$RcaWB, k, cols = 12:13, rows = 11:11)
    mergeCells(projEnv$RcaWB, k, cols = 17:18, rows = 11:11)
	
    mergeCells(projEnv$RcaWB, k, cols = 3:4, rows = 12:12)
    mergeCells(projEnv$RcaWB, k, cols = 7:8, rows = 12:12)
    mergeCells(projEnv$RcaWB, k, cols = 12:13, rows = 12:12)
    mergeCells(projEnv$RcaWB, k, cols = 17:18, rows = 12:12)
	
    mergeCells(projEnv$RcaWB, k, cols = 3:4, rows = 13:13)
    mergeCells(projEnv$RcaWB, k, cols = 7:8, rows = 13:13)
    mergeCells(projEnv$RcaWB, k, cols = 13:13, rows = 13:13)
    mergeCells(projEnv$RcaWB, k, cols = 17:18, rows = 13:13)
	####################
   

    mergeCells(projEnv$RcaWB, k, cols = 2:4, rows = 14:14)
    mergeCells(projEnv$RcaWB, k, cols = 6:8, rows = 14:14)
    mergeCells(projEnv$RcaWB, k, cols = 11:13, rows = 14:14)
    mergeCells(projEnv$RcaWB, k, cols = 16:18, rows = 14:14)

    mergeCells(projEnv$RcaWB, k, cols = 2:4, rows = 15:15)
    mergeCells(projEnv$RcaWB, k, cols = 6:8, rows = 15:15)
    mergeCells(projEnv$RcaWB, k, cols = 11:13, rows = 15:15)
    mergeCells(projEnv$RcaWB, k, cols = 16:18, rows = 15:15)
    print("A")
    mergeCells(projEnv$RcaWB, k, cols = 2:4, rows = 16:16)
    mergeCells(projEnv$RcaWB, k, cols = 6:8, rows = 16:16)
    mergeCells(projEnv$RcaWB, k, cols = 11:13, rows = 16:16)
    mergeCells(projEnv$RcaWB, k, cols = 16:18, rows = 16:16)
    print("A1")
    mergeCells(projEnv$RcaWB, k, cols = 2:4, rows = 17:17)
    mergeCells(projEnv$RcaWB, k, cols = 6:8, rows = 17:17)
    mergeCells(projEnv$RcaWB, k, cols = 11:13, rows = 17:17)
    mergeCells(projEnv$RcaWB, k, cols = 16:18, rows = 17:17)
    print("A2")
    mergeCells(projEnv$RcaWB, k, cols = 2:4, rows = 18:18)
    mergeCells(projEnv$RcaWB, k, cols = 6:8, rows = 18:18)
    mergeCells(projEnv$RcaWB, k, cols = 11:13, rows = 18:18)
    mergeCells(projEnv$RcaWB, k, cols = 16:18, rows = 18:18)
    print("A3")
    mergeCells(projEnv$RcaWB, k, cols = 2:4, rows = 19:19)
    mergeCells(projEnv$RcaWB, k, cols = 6:8, rows = 19:19)
    mergeCells(projEnv$RcaWB, k, cols = 11:13, rows = 19:19)
    mergeCells(projEnv$RcaWB, k, cols = 16:18, rows = 19:19)
	
    mergeCells(projEnv$RcaWB, k, cols = 2:4, rows = 20:20)
    mergeCells(projEnv$RcaWB, k, cols = 6:8, rows = 20:20)
    mergeCells(projEnv$RcaWB, k, cols = 11:13, rows = 20:20)
    mergeCells(projEnv$RcaWB, k, cols = 16:18, rows = 20:20)
    print("A5")
    mergeCells(projEnv$RcaWB, k, cols = 2:4, rows = 21:21)
    mergeCells(projEnv$RcaWB, k, cols = 6:8, rows = 21:21)
    mergeCells(projEnv$RcaWB, k, cols = 11:13, rows = 21:21)
    mergeCells(projEnv$RcaWB, k, cols = 16:18, rows = 21:21)
    print("A6")
    mergeCells(projEnv$RcaWB, k, cols = 2:4, rows = 22:22)
    mergeCells(projEnv$RcaWB, k, cols = 6:8, rows = 22:22)
    mergeCells(projEnv$RcaWB, k, cols = 11:13, rows = 22:22)
    mergeCells(projEnv$RcaWB, k, cols = 16:18, rows = 22:22)
	
	mergeCells(projEnv$RcaWB, k, cols = 2:4, rows = 23:23)
    mergeCells(projEnv$RcaWB, k, cols = 6:8, rows = 23:23)
    mergeCells(projEnv$RcaWB, k, cols = 11:13, rows = 23:23)
    mergeCells(projEnv$RcaWB, k, cols = 16:18, rows = 23:23)
	
	mergeCells(projEnv$RcaWB, k, cols = 2:4, rows = 24:24)
    mergeCells(projEnv$RcaWB, k, cols = 6:8, rows = 24:24)
    mergeCells(projEnv$RcaWB, k, cols = 11:13, rows = 24:24)
    mergeCells(projEnv$RcaWB, k, cols = 16:18, rows = 24:24)
	
    print("A7")
    mergeCells(projEnv$RcaWB, k, cols = 2:4, rows = 25:25)
    mergeCells(projEnv$RcaWB, k, cols = 6:8, rows = 25:25)
    mergeCells(projEnv$RcaWB, k, cols = 11:13, rows = 25:25)
    mergeCells(projEnv$RcaWB, k, cols = 17:18, rows = 25:25)
    print("B")
    mergeCells(projEnv$RcaWB, k, cols = 3:4, rows = 26:26)
    mergeCells(projEnv$RcaWB, k, cols = 7:8, rows = 26:26)
    mergeCells(projEnv$RcaWB, k, cols = 12:13, rows = 26:26)
    mergeCells(projEnv$RcaWB, k, cols = 17:18, rows = 26:26)

    mergeCells(projEnv$RcaWB, k, cols = 2:4, rows = 27:27)
    mergeCells(projEnv$RcaWB, k, cols = 6:8, rows = 27:27)
    mergeCells(projEnv$RcaWB, k, cols = 11:13, rows = 27:27)
    mergeCells(projEnv$RcaWB, k, cols = 16:18, rows = 27:27)
 
    mergeCells(projEnv$RcaWB, k, cols = 2:4, rows = 28:28)
    mergeCells(projEnv$RcaWB, k, cols = 6:8, rows = 28:28)
    mergeCells(projEnv$RcaWB, k, cols = 11:13, rows = 28:28)
    mergeCells(projEnv$RcaWB, k, cols = 16:18, rows = 28:28)

    mergeCells(projEnv$RcaWB, k, cols = 2:4, rows = 29:29)
    mergeCells(projEnv$RcaWB, k, cols = 6:8, rows = 29:29)
    mergeCells(projEnv$RcaWB, k, cols = 11:13, rows = 29:29)
    mergeCells(projEnv$RcaWB, k, cols = 16:18, rows = 29:29)

    mergeCells(projEnv$RcaWB, k, cols = 2:4, rows = 30:30)
    mergeCells(projEnv$RcaWB, k, cols = 6:8, rows = 30:30)
    mergeCells(projEnv$RcaWB, k, cols = 11:13, rows = 30:30)
    mergeCells(projEnv$RcaWB, k, cols = 16:18, rows = 30:30)

    mergeCells(projEnv$RcaWB, k, cols = 2:4, rows = 31:31)
    mergeCells(projEnv$RcaWB, k, cols = 6:8, rows = 31:31)
    mergeCells(projEnv$RcaWB, k, cols = 11:13, rows = 31:31)
    mergeCells(projEnv$RcaWB, k, cols = 16:18, rows = 31:31)

    mergeCells(projEnv$RcaWB, k, cols = 2:4, rows = 32:32)
    mergeCells(projEnv$RcaWB, k, cols = 6:8, rows = 32:32)
    mergeCells(projEnv$RcaWB, k, cols = 11:13, rows = 32:32)
    mergeCells(projEnv$RcaWB, k, cols = 16:18, rows = 32:32)

    mergeCells(projEnv$RcaWB, k, cols = 2:4, rows = 33:33)
    mergeCells(projEnv$RcaWB, k, cols = 6:8, rows = 33:33)
    mergeCells(projEnv$RcaWB, k, cols = 11:13, rows = 33:33)
    mergeCells(projEnv$RcaWB, k, cols = 16:18, rows = 33:33)

    mergeCells(projEnv$RcaWB, k, cols = 2:4, rows = 34:34)
    mergeCells(projEnv$RcaWB, k, cols = 6:8, rows = 34:34)
    mergeCells(projEnv$RcaWB, k, cols = 11:13, rows = 34:34)
    mergeCells(projEnv$RcaWB, k, cols = 16:18, rows = 34:34)

    mergeCells(projEnv$RcaWB, k, cols = 2:4, rows = 35:35)
    mergeCells(projEnv$RcaWB, k, cols = 6:8, rows = 35:35)
    mergeCells(projEnv$RcaWB, k, cols = 11:13, rows = 35:35)
    mergeCells(projEnv$RcaWB, k, cols = 16:18, rows = 35:35)

    mergeCells(projEnv$RcaWB, k, cols = 2:4, rows = 36:36)
    mergeCells(projEnv$RcaWB, k, cols = 6:8, rows = 36:36)
    mergeCells(projEnv$RcaWB, k, cols = 11:13, rows = 36:36)
    mergeCells(projEnv$RcaWB, k, cols = 16:18, rows = 36:36)

    mergeCells(projEnv$RcaWB, k, cols = 2:4, rows = 37:37)
    mergeCells(projEnv$RcaWB, k, cols = 6:8, rows = 37:37)
    mergeCells(projEnv$RcaWB, k, cols = 11:13, rows = 37:37)
    mergeCells(projEnv$RcaWB, k, cols = 16:18, rows = 37:37)

    mergeCells(projEnv$RcaWB, k, cols = 2:4, rows = 38:38)
    mergeCells(projEnv$RcaWB, k, cols = 6:8, rows = 38:38)
    mergeCells(projEnv$RcaWB, k, cols = 11:13, rows = 38:38)
    mergeCells(projEnv$RcaWB, k, cols = 16:18, rows = 38:38)
    print("D")
    mergeCells(projEnv$RcaWB, k, cols = 2:4, rows = 39:39)
    mergeCells(projEnv$RcaWB, k, cols = 6:8, rows = 39:39)
    mergeCells(projEnv$RcaWB, k, cols = 11:13, rows = 39:39)
    mergeCells(projEnv$RcaWB, k, cols = 16:18, rows = 39:39)

    mergeCells(projEnv$RcaWB, k, cols = 2:4, rows = 40:40)
    mergeCells(projEnv$RcaWB, k, cols = 6:8, rows = 40:40)
    mergeCells(projEnv$RcaWB, k, cols = 11:13, rows = 40:40)
    mergeCells(projEnv$RcaWB, k, cols = 16:18, rows = 40:40)

    mergeCells(projEnv$RcaWB, k, cols = 2:5, rows = 41:42)
    mergeCells(projEnv$RcaWB, k, cols = 6:8, rows = 41:42)
    mergeCells(projEnv$RcaWB, k, cols = 11:13, rows = 41:42)
    mergeCells(projEnv$RcaWB, k, cols = 16:18, rows = 41:42)

   # mergeCells(projEnv$RcaWB, k, cols = 2:4, rows = 42:42)
    # mergeCells(projEnv$RcaWB, k, cols = 6:8, rows = 42:42)
    # mergeCells(projEnv$RcaWB, k, cols = 11:13, rows = 42:42)
    # mergeCells(projEnv$RcaWB, k, cols = 16:18, rows = 42:42)

  
 
  mergeCells(projEnv$RcaWB, k, cols = 1:20, rows = 43:43)
  
 
	
	  line2Style <-
      createStyle(
        border = "TopBottomLeftRight",
        borderColour = "#002060",
        textDecoration = "bold",
        fontColour= "#FFFFFF",
        borderStyle = openxlsx_getOp("borderStyle", "thick"),
        valign = "center",
        halign = "center",
        fgFill = "#002060"
      )
	 
    cellBorderStyle <- 
	 createStyle(
	    border ="TopBottomLeftRight",
		borderColour="#002060",
		borderStyle="thick"
	 )
	 
	line3Style <-
      createStyle(
      
        border = "TopBottomLeftRight",
        textDecoration = "bold",
        borderColour = "#002060",
        borderStyle = openxlsx_getOp("borderStyle", "thin"),
        valign = "center",
        halign = "center",
        fgFill = "#CCCCFF"
      )
	 
	 line3StyleLocked <-
      createStyle(
        border = NULL,
        lock=TRUE,
        textDecoration = "bold",
        borderColour = "#002060",
        borderStyle = openxlsx_getOp("borderStyle", "thin"),
        valign = "center",
        halign = "center",
        fgFill = "#CCCCFF"
      )
	  
	 col1Style <-
      createStyle(
        border = "TopBottomLeftRight",
        borderColour = "#002060",
         textDecoration = "bold",
        
        borderStyle = openxlsx_getOp("borderStyle", "thin"),
        valign = "center",
        halign = "left",
        fgFill = "#CCCCFF"
      )
	  
    line4Style <-
      createStyle(
        border = "TopBottomLeftRight",
        borderColour = "#002060",
        borderStyle = openxlsx_getOp("borderStyle", "thin"),
        valign = "center",
        halign = "left",
        fgFill = "#E2EFDA"
      )
	  
     line4StyleBold <-
      createStyle(
        border = "TopBottomLeftRight",
        borderColour = "#002060",
		borderStyle = openxlsx_getOp("borderStyle", "thin"),
        textDecoration = "bold",
       
        valign = "center",
        halign = "center",
        fgFill = "#E2EFDA"
      )
	  
	 line4StyleBold <-
      createStyle(
        border = "TopBottomLeftRight",
        borderColour = "#002060",
        textDecoration = "bold",
        borderStyle = openxlsx_getOp("borderStyle", "thin"),
        valign = "center",
        halign = "left",
        fgFill = "#E2EFDA"
      )
	  
    line5Style <-
      createStyle(
        border = "TopBottomLeftRight",
        borderColour = "#002060",
        borderStyle = openxlsx_getOp("borderStyle", "thin"),
        valign = "center",
        halign = "center",
        fgFill = "#E2EFDA"
      )
	  
	  
    line6StyleBold <-
      createStyle(
        border = "TopBottomLeftRight",
        borderColour = "#002060",
        textDecoration = "bold",
        borderStyle = openxlsx_getOp("borderStyle", "thin"),
        valign = "center",
        halign = "left",
        fgFill = "#F2F2F2"
      )
	  
   line6Style  <-
      createStyle(
	     numFmt="$#,##0",
        border = "TopBottomLeftRight",
        borderColour = "#002060",
        borderStyle = openxlsx_getOp("borderStyle", "thin"),
        valign = "center",
        halign = "right",
		 bgFill = "#FFFFFF",
        fgFill = "#F2F2F2"
      )
	  
  line40Style <-
      createStyle(
        border = "TopBottomLeftRight",
        borderColour = "#002060",
		borderStyle = openxlsx_getOp("borderStyle", "thin"),
        bgFill = "#CCCCFF",
        valign = "center",
        halign = "center",
        fgFill = "#FFFFFF"
      ) 
   line43Style <-
      createStyle(
        border = "TopBottomLeftRight",
        borderColour = "#002060",
		borderStyle = openxlsx_getOp("borderStyle", "thick"),
     
        valign = "center",
        halign = "center",
        fgFill = "#CCCCFF"
      )
    bodyStyle <-
      createStyle(
        border = "TopBottomLeftRight",
        borderColour = "#002060",
		borderStyle = openxlsx_getOp("borderStyle", "thin"),
        bgFill = "#CCCCFF",
        valign = "center",
        halign = "center",
        fgFill = "#FFFFFF"
      )
	  bodyStyleGreen <-
      createStyle(
        border = "TopBottomLeftRight",
        borderColour = "#002060",
		borderStyle = openxlsx_getOp("borderStyle", "thin"),
        bgFill = "#CCCCFF",
        valign = "center",
        halign = "center",
        fgFill = "#E2EFDA"
      )
	 gisStyle3 <-
	  createStyle(
	     border = "TopBottomLeftRight",
         borderColour = "#002060",
		 borderStyle = openxlsx_getOp("borderStyle", "thin"),
	     numFmt="0.000",
	    
		 halign="center",
		 valign="center")
		 
	line10Style <- createStyle(
	     border = "TopBottomLeftRight",
         borderColour = "#002060",
		 borderStyle = openxlsx_getOp("borderStyle", "thin"),
	     numFmt="#,##0",
		 halign="center",
		 valign="center")
	
    line41Style  <-
      createStyle(
        border = "TopBottomLeftRight",
        textDecoration = "bold",
        borderColour = "#002060",
        borderStyle = openxlsx_getOp("borderStyle", "thin"),
        valign = "center",
        halign = "center",
        fgFill = "#CCCCFF"
      )
	  
	 line42Style  <-
      createStyle(
        border = "Bottom",
        textDecoration = "bold",
        borderColour = "#002060",
        borderStyle = openxlsx_getOp("borderStyle", "thin"),
        valign = "center",
        halign = "center",
        fgFill = "#CCCCFF"
      )
	  
    numStyle <-
	  createStyle(
	     border = "TopBottomLeftRight",
         borderColour = "#002060",
	     borderStyle = openxlsx_getOp("borderStyle", "thin"),
	     numFmt="#,##0",
		   fgFill = "#FFFFFF",
		    indent=0,
		 halign="right",
		 valign="center")
		 
	 numStyleMiles <-
	  createStyle(
	     border = "TopBottomLeftRight",
         borderColour = "#002060",
	     borderStyle = openxlsx_getOp("borderStyle", "thin"),
	     numFmt="#,##0.00",
		 fgFill = "#E2EFDA",
		 halign="center",
		 valign="center")
		 
   numStyleCurr <-
	  createStyle(
	     border = "TopBottomLeftRight",
         borderColour = "#002060",
		 borderStyle = openxlsx_getOp("borderStyle", "thin"),
	     numFmt="$#,##0",
	     fgFill = "#FFFFFF",
		 indent=0,
		 halign="right",
		 valign="center")	 
		 
     numStyleCQA <-
	  createStyle(
	     border = "TopBottomLeftRight",
         borderColour = "#002060",
		 borderStyle = openxlsx_getOp("borderStyle", "thin"),
	     numFmt="0.00",
		 halign="center",
		 valign="center")	
		 
	############################### Add Styles:  #######################
	 addStyle(
      projEnv$RcaWB,
      sheet = k,
      line2Style,
      rows = 2:2,
      cols = 1:20,
      gridExpand = FALSE
    )
	 addStyle(
      projEnv$RcaWB,
      sheet = k,
      line3Style,
      rows = 3:3,
      cols = 1:20,
      stack=TRUE,
      gridExpand = TRUE
    )
	 addStyle(
      projEnv$RcaWB,
      sheet = k,
      line4Style,
      rows = 4,
      cols = 2:20,
	  stack=TRUE,
      gridExpand = TRUE
    )
	 addStyle(
      projEnv$RcaWB,
      sheet = k,
      line4StyleBold,
      rows = 4:5,
      cols = 1:1,
	  stack=TRUE,
      gridExpand = TRUE
    )
	
	
	 
    addStyle(
		  projEnv$RcaWB,
		  sheet = k,
		  line5Style,
		  rows=5:5,
		  cols=2:20,
		  stack=TRUE,
		  gridExpand=TRUE)
 
	  
	  
	# # Line 6 col 1
	 addStyle(
      projEnv$RcaWB,
      sheet = k,
      line6StyleBold,
      rows = 6:6,
      cols = 1:1,
	    stack=TRUE,
      gridExpand = FALSE
    )
	# line 6 cols 2-20
	addStyle(
      projEnv$RcaWB,
      sheet = k,
      line6Style,
      rows = 6:6,
      cols = 2:20,
	  stack=TRUE,
      gridExpand = TRUE
    )
	
	 # for( n in 6:20) {
		  # addStyle(
				  # projEnv$RcaWB,
				  # sheet = k,
				  # numStyleCurr,
				  # rows=6:6,
				  # cols=n,
				  # stack=TRUE,
				  # gridExpand=FALSE)
		  
	   # }
	   
	# Line 7 Cols 1-20
    addStyle(
      projEnv$RcaWB,
      sheet = k,
      line3Style,
      rows = 7:7,
      cols = 1:20,
      gridExpand = FALSE
    )
	#  Col 1, Lines 7:43 Light blue
    addStyle(
      projEnv$RcaWB,
      sheet = k,
      col1Style,
      rows = 7:43,
      cols = 1:1,
	  stack=TRUE,
      gridExpand = TRUE
    )
	
    addStyle(
	 projEnv$RcaWB,
      sheet = k,
	  numStyle,
	  rows=8:24,
	  cols=5:5,
	  stack=TRUE,
	  gridExpand=TRUE
	  )
	  
	 # Center GIS Coordinates & Size Size, Dimensions, Actual Age, Beds, Baths
     for(n in c(2,3, 5, 6, 11,12,16,17)) {
		  addStyle(
			  projEnv$RcaWB,
			  sheet = k,
			  gisStyle3,
			  rows=8:9,
			  cols=n,
			  stack=TRUE,
			  gridExpand=TRUE
	      )
		}  
		
		 
	   # Value Contrib & Adjustment Columns
	    for( n in c(5, 9,10,14,15,19,20)) {
		  addStyle(
				  projEnv$RcaWB,
				  sheet = k,
				  numStyleCurr,
				  rows=8:24,
				  cols=n,
				  stack=TRUE)
				  
		   addStyle(
				  projEnv$RcaWB,
				  sheet = k,
				  numStyleCurr,
				  rows=26:40,
				  cols=n,
				  stack=TRUE)
		   addStyle(
				  projEnv$RcaWB,
				  sheet = k,
				  numStyleCurr,
				  rows=42,
				  cols=n,
				  stack=TRUE)
		# conditionalFormatting(projEnv$RcaWB,sheet=k,cols=n,rows=8:24,rule="!=0",style=numStyleCurr)
	  }
	  
	 
	  for( n in c(2,3,4,6,7,8,11,12,13,16,17,18)) {
		  addStyle(
				  projEnv$RcaWB,
				  sheet = k,
				  line10Style,
				  rows=10:24,
				  cols=n,
				  stack=TRUE,
				  gridExpand=TRUE)
				  
		  addStyle(
				  projEnv$RcaWB,
				  sheet = k,
				  line10Style,
				  rows=24:38,
				  cols=n,
				  stack=TRUE,
				  gridExpand=TRUE)	  
	  }
	  
	    
	 addStyle(
      projEnv$RcaWB,
      sheet = k,
      line3Style,
      rows = 25:25,
      cols = 2:20,
	    stack=TRUE,
      gridExpand = TRUE
    )
	 addStyle(
      projEnv$RcaWB,
      sheet = k,
      line3Style,
      rows = 25:25,
      cols = 2:8,
	    stack=TRUE,
      gridExpand = TRUE
    )
	 addStyle(
      projEnv$RcaWB,
      sheet = k,
      line3Style,
      rows = 25:25,
      cols = 11:13,
	    stack=TRUE,
      gridExpand = TRUE
    )
	 addStyle(
      projEnv$RcaWB,
      sheet = k,
      line3Style,
      rows = 25:25,
      cols = 16:18,
	    stack=TRUE,
      gridExpand = TRUE
    )
	
	# Lines 4:43, cols 2-9, Light Blue
    addStyle(
      projEnv$RcaWB,
      sheet = k,
      line41Style,
      rows = 41:42 ,
      cols = 1:9,
	    stack=TRUE,
      gridExpand = TRUE
    )
   
    addStyle(
      projEnv$RcaWB,
      sheet = k,
      line41Style,
      rows = 41:42,
      cols = 11:14,
	  stack=TRUE,
      gridExpand = TRUE
    )
	 
    addStyle(
      projEnv$RcaWB,
      sheet = k,
      line41Style,
      rows = 41:42 ,
      cols = 16:19,
	  stack=TRUE,
      gridExpand = TRUE
    )
  
	 addStyle(
	 projEnv$RcaWB,
      sheet = k,
	  bodyStyle,
	  rows=43:43,
	  cols=1:20,
	  stack=TRUE,
	  gridExpand=TRUE
	  )   

 
   OutsideBorders(
		  projEnv$RcaWB,
		  sheet = k,
		  rows = 2:43,
		  cols = 1:20
		)
   OutsideBorders(
		  projEnv$RcaWB,
		  sheet = k,
		  rows = 2:42,
		  cols = 1:20
		)
			 
    print("Y")
    }
  
}

# Load Featur Names into RcaInputDF
SetUpFeatures <- function() {
  print("SetUpFeatures")
  projEnv$RcaInputDF[2, "Features"] <- ""
  projEnv$RcaInputDF[3, "Features"] <- "Street, City, State Zip"
  projEnv$RcaInputDF[4, "Features"] <- "APN | MLS# | Subj.Proximity"
  projEnv$RcaInputDF[5, "Features"] <- "Sales Price|Concess.|Net SP"
  projEnv$RcaInputDF[6, "Features"] <- "Regression Features"
  projEnv$RcaInputDF[7, "Features"] <-  "BASE VALUE"
  projEnv$RcaInputDF[8, "Features"] <-  "Sale Date | Close Date | Off Mkt"
  projEnv$RcaInputDF[9, "Features"] <-  "Location:  Longitude | Latitude"
  projEnv$RcaInputDF[10, "Features"] <- "Site Size | Dimensions"
  projEnv$RcaInputDF[11, "Features"] <- "Actual Age | Effective Age"
  projEnv$RcaInputDF[12, "Features"] <- "Beds | Baths"
  projEnv$RcaInputDF[13, "Features"] <- "Legal Living Area Above Gnd"
  projEnv$RcaInputDF[14, "Features"] <- "Legal Living Area Below Gnd"
  projEnv$RcaInputDF[15, "Features"] <- "Non-legal Living Area"
  projEnv$RcaInputDF[16, "Features"] <- "Secondary/ADU"
  projEnv$RcaInputDF[17, "Features"] <- "Garage SF"
  projEnv$RcaInputDF[18, "Features"] <- "Fireplaces"
  projEnv$RcaInputDF[19, "Features"] <- "Stories"
 
  projEnv$RcaInputDF[20, "Features"] <- ""
  projEnv$RcaInputDF[21, "Features"] <- ""
  projEnv$RcaInputDF[22, "Features"] <- ""
  projEnv$RcaInputDF[23, "Features"] <- ""
  projEnv$RcaInputDF[24, "Features"] <- "Residual Features"
  projEnv$RcaInputDF[25, "Features"] <- "CQA | Residual | Remaining"
  projEnv$RcaInputDF[26, "Features"] <- "View"
  projEnv$RcaInputDF[27, "Features"] <- "Design"
  projEnv$RcaInputDF[28, "Features"] <- "Quality of Construction"
  projEnv$RcaInputDF[29, "Features"] <- "Condition"
  projEnv$RcaInputDF[30, "Features"] <- "Functional Utility"
  projEnv$RcaInputDF[31, "Features"] <- "Heating & A/c"
  projEnv$RcaInputDF[32, "Features"] <- "Porch/Patio"
  projEnv$RcaInputDF[33, "Features"] <- "Other"
  projEnv$RcaInputDF[34, "Features"] <- ""
  projEnv$RcaInputDF[35, "Features"] <- ""
  projEnv$RcaInputDF[36, "Features"] <- ""
  projEnv$RcaInputDF[37, "Features"] <- ""
  projEnv$RcaInputDF[38, "Features"] <- ""
  projEnv$RcaInputDF[39, "Features"] <- "Total VC / Net Adjustment"
  projEnv$RcaInputDF[40, "Features"] <- "Adjusted Sale Price"
  projEnv$RcaInputDF[41, "Features"] <- "of Comparables"
  projEnv$RcaInputDF[42, "Features"] <- "Copyright 2022-3, Pacific Vista Net ©"
  print("end")
}


RcaWBStyleUpdate <- function() {
   

  colWidths <-
    c(29, 11, 8, 4, 12, 11, 8, 7, 12, 11, 11, 8, 7, 12, 11, 11, 8, 7, 12, 11)
  cols <-
    c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)

  for (k in 1:4) {
  	 
    
  
 
	
	  line2Style <-
      createStyle(
        border = "TopBottomLeftRight",
        borderColour = "#002060",
        textDecoration = "bold",
        fontColour= "#FFFFFF",
        borderStyle = openxlsx_getOp("borderStyle", "thick"),
        valign = "center",
        halign = "center",
        fgFill = "#002060"
      )
	 
    cellBorderStyle <- 
	 createStyle(
	    border ="TopBottomLeftRight",
		borderColour="#002060",
		borderStyle="thick"
	 )
	 
	line3Style <-
      createStyle(
      
        border = "TopBottomLeftRight",
        textDecoration = "bold",
        borderColour = "#002060",
        borderStyle = openxlsx_getOp("borderStyle", "thin"),
        valign = "center",
        halign = "center",
        fgFill = "#CCCCFF"
      )
	 
	 line3StyleLocked <-
      createStyle(
        border = NULL,
        lock=TRUE,
        textDecoration = "bold",
        borderColour = "#002060",
        borderStyle = openxlsx_getOp("borderStyle", "thin"),
        valign = "center",
        halign = "center",
        fgFill = "#CCCCFF"
      )
	  
	 col1Style <-
      createStyle(
        border = "TopBottomLeftRight",
        borderColour = "#002060",
         textDecoration = "bold",
        
        borderStyle = openxlsx_getOp("borderStyle", "thin"),
        valign = "center",
        halign = "left",
        fgFill = "#CCCCFF"
      )
	  
    line4Style <-
      createStyle(
        border = "TopBottomLeftRight",
        borderColour = "#002060",
        borderStyle = openxlsx_getOp("borderStyle", "thin"),
        valign = "center",
        halign = "left",
        fgFill = "#E2EFDA"
      )
	  
     line4StyleBold <-
      createStyle(
        border = "TopBottomLeftRight",
        borderColour = "#002060",
		borderStyle = openxlsx_getOp("borderStyle", "thin"),
        textDecoration = "bold",
       
        valign = "center",
        halign = "center",
        fgFill = "#E2EFDA"
      )
	  
	 line4StyleBold <-
      createStyle(
        border = "TopBottomLeftRight",
        borderColour = "#002060",
        textDecoration = "bold",
        borderStyle = openxlsx_getOp("borderStyle", "thin"),
        valign = "center",
        halign = "left",
        fgFill = "#E2EFDA"
      )
	  
    line5Style <-
      createStyle(
        border = "TopBottomLeftRight",
        borderColour = "#002060",
        borderStyle = openxlsx_getOp("borderStyle", "thin"),
        valign = "center",
        halign = "center",
        fgFill = "#E2EFDA"
      )
	  
	  
    line6StyleBold <-
      createStyle(
        border = "TopBottomLeftRight",
        borderColour = "#002060",
        textDecoration = "bold",
        borderStyle = openxlsx_getOp("borderStyle", "thin"),
        valign = "center",
        halign = "left",
        fgFill = "#F2F2F2"
      )
	  
   line6Style  <-
      createStyle(
	     numFmt="$#,##0",
        border = "TopBottomLeftRight",
        borderColour = "#002060",
        borderStyle = openxlsx_getOp("borderStyle", "thin"),
        valign = "center",
        halign = "right",
		 bgFill = "#FFFFFF",
        fgFill = "#F2F2F2"
      )
	  
  line40Style <-
      createStyle(
        border = "TopBottomLeftRight",
        borderColour = "#002060",
		borderStyle = openxlsx_getOp("borderStyle", "thin"),
        bgFill = "#CCCCFF",
        valign = "center",
        halign = "center",
        fgFill = "#FFFFFF"
      ) 
   line43Style <-
      createStyle(
        border = "TopBottomLeftRight",
        borderColour = "#002060",
		borderStyle = openxlsx_getOp("borderStyle", "thick"),
     
        valign = "center",
        halign = "center",
        fgFill = "#CCCCFF"
      )
    bodyStyle <-
      createStyle(
        border = "TopBottomLeftRight",
        borderColour = "#002060",
		borderStyle = openxlsx_getOp("borderStyle", "thin"),
        bgFill = "#CCCCFF",
        valign = "center",
        halign = "center",
        fgFill = "#FFFFFF"
      )
	  bodyStyleGreen <-
      createStyle(
        border = "TopBottomLeftRight",
        borderColour = "#002060",
		borderStyle = openxlsx_getOp("borderStyle", "thin"),
        bgFill = "#CCCCFF",
        valign = "center",
        halign = "center",
        fgFill = "#E2EFDA"
      )
	 gisStyle3 <-
	  createStyle(
	     border = "TopBottomLeftRight",
         borderColour = "#002060",
		 borderStyle = openxlsx_getOp("borderStyle", "thin"),
	     numFmt="0.000",
	    
		 halign="center",
		 valign="center")
		 
	line10Style <- createStyle(
	     border = "TopBottomLeftRight",
         borderColour = "#002060",
		 borderStyle = openxlsx_getOp("borderStyle", "thin"),
	     numFmt="#,##0",
		 halign="center",
		 valign="center")
	
    line41Style  <-
      createStyle(
        border = "TopBottomLeftRight",
        textDecoration = "bold",
        borderColour = "#002060",
        borderStyle = openxlsx_getOp("borderStyle", "thin"),
        valign = "center",
        halign = "center",
        fgFill = "#CCCCFF"
      )
	  
	 line42Style  <-
      createStyle(
        border = "Bottom",
        textDecoration = "bold",
        borderColour = "#002060",
        borderStyle = openxlsx_getOp("borderStyle", "thin"),
        valign = "center",
        halign = "center",
        fgFill = "#CCCCFF"
      )
	  
    numStyle <-
	  createStyle(
	     border = "TopBottomLeftRight",
         borderColour = "#002060",
	     borderStyle = openxlsx_getOp("borderStyle", "thin"),
	     numFmt="#,##0 ",
		   fgFill = "#FFFFFF",
		    indent=0,
		 halign="right",
		 valign="center")
		 
	 numStyleMiles <-
	  createStyle(
	     border = "TopBottomLeftRight",
         borderColour = "#002060",
	     borderStyle = openxlsx_getOp("borderStyle", "thin"),
	     numFmt="#,##0.00",
		 fgFill = "#E2EFDA",
		 halign="center",
		 valign="center")
		 
   numStyleCurr <-
	  createStyle(
	     border = "TopBottomLeftRight",
         borderColour = "#002060",
		 borderStyle = openxlsx_getOp("borderStyle", "thin"),
	     numFmt="$#,##0 ",
	     fgFill = "#FFFFFF",
		 indent=0,
		 halign="right",
		 valign="center")	 
		 
     numStyleCQA <-
	  createStyle(
	     border = "TopBottomLeftRight",
         borderColour = "#002060",
		 borderStyle = openxlsx_getOp("borderStyle", "thin"),
	     numFmt="0.00",
		 halign="center",
		 valign="center")	
		 
	############################### Add Styles:  #######################
	 addStyle(
      projEnv$RcaWB,
      sheet = k,
      line2Style,
      rows = 2:2,
      cols = 1:20,
      gridExpand = FALSE
    )
	 addStyle(
      projEnv$RcaWB,
      sheet = k,
      line3Style,
      rows = 3:3,
      cols = 1:20,
      stack=TRUE,
      gridExpand = TRUE
    )
	 addStyle(
      projEnv$RcaWB,
      sheet = k,
      line4Style,
      rows = 4,
      cols = 2:20,
	  stack=TRUE,
      gridExpand = TRUE
    )
	 addStyle(
      projEnv$RcaWB,
      sheet = k,
      line4StyleBold,
      rows = 4:5,
      cols = 1:1,
	  stack=TRUE,
      gridExpand = TRUE
    )
	
	
	 
    addStyle(
		  projEnv$RcaWB,
		  sheet = k,
		  line5Style,
		  rows=5:5,
		  cols=2:20,
		  stack=TRUE,
		  gridExpand=TRUE)
 
	  
	  
	# # Line 6 col 1
	 addStyle(
      projEnv$RcaWB,
      sheet = k,
      line6StyleBold,
      rows = 6:6,
      cols = 1:1,
	    stack=TRUE,
      gridExpand = FALSE
    )
	# line 6 cols 2-20
	addStyle(
      projEnv$RcaWB,
      sheet = k,
      line6Style,
      rows = 6:6,
      cols = 2:20,
	  stack=TRUE,
      gridExpand = TRUE
    )
	
	 # for( n in 6:20) {
		  # addStyle(
				  # projEnv$RcaWB,
				  # sheet = k,
				  # numStyleCurr,
				  # rows=6:6,
				  # cols=n,
				  # stack=TRUE,
				  # gridExpand=FALSE)
		  
	   # }
	   
	# Line 7 Cols 1-20
    addStyle(
      projEnv$RcaWB,
      sheet = k,
      line3Style,
      rows = 7:7,
      cols = 1:20,
      gridExpand = FALSE
    )
	#  Col 1, Lines 7:43 Light blue
    addStyle(
      projEnv$RcaWB,
      sheet = k,
      col1Style,
      rows = 7:43,
      cols = 1:1,
	  stack=TRUE,
      gridExpand = TRUE
    )
	
    addStyle(
	 projEnv$RcaWB,
      sheet = k,
	  numStyle,
	  rows=8:24,
	  cols=5:5,
	  stack=TRUE,
	  gridExpand=TRUE
	  )
	  
	 # Center GIS Coordinates & Size Size, Dimensions, Actual Age, Beds, Baths
     for(n in c(2,3, 5, 6, 11,12,16,17)) {
		  addStyle(
			  projEnv$RcaWB,
			  sheet = k,
			  gisStyle3,
			  rows=8:9,
			  cols=n,
			  stack=TRUE,
			  gridExpand=TRUE
	      )
		}  
		
		 
	   # Value Contrib & Adjustment Columns
	    for( n in c(5, 9,10,14,15,19,20)) {
		  addStyle(
				  projEnv$RcaWB,
				  sheet = k,
				  numStyleCurr,
				  rows=8:24,
				  cols=n,
				  stack=TRUE,
				  gridExpand=TRUE)
				  
		   addStyle(
				  projEnv$RcaWB,
				  sheet = k,
				  numStyleCurr,
				  rows=26:40,
				  cols=n,
				  stack=TRUE,
				  gridExpand=TRUE)
		   addStyle(
				  projEnv$RcaWB,
				  sheet = k,
				  numStyleCurr,
				  rows=42,
				  cols=n,
				  stack=TRUE,
				  gridExpand=TRUE)
	   conditionalFormatting(projEnv$RcaWB,sheet=k,cols=n,rows=8:24,rule="!=0",style=numStyleCurr)
	  }
	  
	 
	  for( n in c(2,3,4,6,7,8,11,12,13,16,17,18)) {
		  addStyle(
				  projEnv$RcaWB,
				  sheet = k,
				  line10Style,
				  rows=10:24,
				  cols=n,
				  stack=TRUE,
				  gridExpand=TRUE)
				  
		  addStyle(
				  projEnv$RcaWB,
				  sheet = k,
				  line10Style,
				  rows=24:38,
				  cols=n,
				  stack=TRUE,
				  gridExpand=TRUE)	  
	  }
	  
	    
	 addStyle(
      projEnv$RcaWB,
      sheet = k,
      line3Style,
      rows = 25:25,
      cols = 2:20,
	    stack=TRUE,
      gridExpand = TRUE
    )
	 addStyle(
      projEnv$RcaWB,
      sheet = k,
      line3Style,
      rows = 25:25,
      cols = 2:8,
	    stack=TRUE,
      gridExpand = TRUE
    )
	 addStyle(
      projEnv$RcaWB,
      sheet = k,
      line3Style,
      rows = 25:25,
      cols = 11:13,
	    stack=TRUE,
      gridExpand = TRUE
    )
	 addStyle(
      projEnv$RcaWB,
      sheet = k,
      line3Style,
      rows = 25:25,
      cols = 16:18,
	    stack=TRUE,
      gridExpand = TRUE
    )
	
	# Lines 4:43, cols 2-9, Light Blue
    addStyle(
      projEnv$RcaWB,
      sheet = k,
      line41Style,
      rows = 41:42 ,
      cols = 1:9,
	    stack=TRUE,
      gridExpand = TRUE
    )
   
    addStyle(
      projEnv$RcaWB,
      sheet = k,
      line41Style,
      rows = 41:42,
      cols = 11:14,
	  stack=TRUE,
      gridExpand = TRUE
    )
	 
    addStyle(
      projEnv$RcaWB,
      sheet = k,
      line41Style,
      rows = 41:42 ,
      cols = 16:19,
	  stack=TRUE,
      gridExpand = TRUE
    )
  
	 addStyle(
	 projEnv$RcaWB,
      sheet = k,
	  bodyStyle,
	  rows=43:43,
	  cols=1:20,
	  stack=TRUE,
	  gridExpand=TRUE
	  )   

 
   OutsideBorders(
		  projEnv$RcaWB,
		  sheet = k,
		  rows = 2:43,
		  cols = 1:20
		)
   OutsideBorders(
		  projEnv$RcaWB,
		  sheet = k,
		  rows = 2:42,
		  cols = 1:20
		)
			 
    print("Y")
    }
  
}
#' Load Comp data from SGPrepDataFile to projEnv$MlsCompsDF
LoadCompsToDataFrameRCA <- function() {
  print("LoadCompsToDataFrameRCA")

  projEnv$MlsCompsDF <-
    read.xlsx(
      projEnv$SGPrepDataFile,
      sheet = "Sheet1",
      colNames = TRUE,
      detectDates = TRUE
    )
}
	
# Initialize projEnv$RcaInputDF with columns of 43 blanks
Initialize_RcaInputDF <- function() {
  print("Initialize_RcaInputDF")
  projEnv$RcaInputDF <- data.frame(
    Features = rep("", 43),
    S1 = rep("", 43),
    S2 = rep("", 43),
    S3 = rep("", 43),
    S4 = c(rep("$0", 38),rep("",5)),
    C1FV1 = rep("", 43),
    C1FV2 = rep("", 43),
    C1FV3 = rep("", 43),
    C1VC1 =  c(rep("$0", 38),rep("",5)),
    C1A1 = rep("", 43),
    C2FV1 = rep("", 43),
    C2FV2 = rep("", 43),
    C2FV3 = rep("", 43),
    C2VC1 = c(rep("$0", 38),rep("",5)),
    C2A1 = rep("", 43),
    C3FV1 = rep("", 43),
    C3FV2 = rep("", 43),
    C3FV3 = rep("", 43),
    C3VC1 = c(rep("$0", 38),rep("",5)),
    C3A1 = rep("", 43)
  )
}



#'  Replace periods in name vector with spaces
#'  @projEnv Name vector
#'  @return  Name vector
RepPeriodWithSpaceColumnNamesRCA <- function(cnms) {
  nc <- length(cnms)
  # Remove spaces, period, parentheses and pound signs from column names
  for (i in 1:nc) {
    cnmsI <- cnms[i]
    # replace period with space
    cnms[i] <- gsub("\\.", " ", cnmsI)
  }
  cnms
}

#'  Compress/clean column names
#'  Replace some characters with more descriptive names and remove unwanted characters such as spaces and periods
#'  @projEnv Name vector
#'  @return  Name vector
CompressColumnNamesRCA <- function(cnms) {
  nc <- length(cnms)

  print(paste("nc: ", nc))

  # Remove spaces, period, parentheses and pound signs from column names
  for (i in 1:nc) {
    cnmsI <- cnms[i]
    # replace pound sign with Nbr
    cnmsI <- gsub("#", "Nbr", cnmsI)
    # remove any characters not in the range A-z
    cnmsI <- gsub("[^A-z0-9]*", "", cnmsI)
    cnms[i] <- cnmsI
  }
  cnms
}

#' We need to convert Data Frame Dates to Char/string for the upload to SQLite to work correctly
#'  @projEnv Name vector
#'  @return  Name vector
ConvertDatesToCharRCA <- function(df) {
  print("ConvertDatesToChar")

  # Get the number of columns
  nc <- ncol(df)

  # Get number of rows
  nr <- nrow(df)

  # Put all column types in array
  t <- sapply(df, class)

  # Find and convert Dates to character for upload into SQLite
  for (i in 1:nc) {
    if (t[i] == "Date") {
      print(paste("i/t[i]: ", i, t[i]))
      asChar <-
        as.character(projEnv$RcaInputDF[, i], format = "%m/%d/%Y")
      df[, i] <- asChar
      print(df[, i])
    }
  }
}

#' We need to convert Data Frame Dates to Char/string for the upload to SQLite to work correctly
#'  @projEnv Project Environment
#'  @return  Project Environment
ConvertDatesToCharAllDFRCA <- function(projEnv) {
  print("ConvertDatesToCharRCA( RcaInputDF")
  ConvertDatesToCharRCA(projEnv$RcaInputDF)
  ConvertDatesToCharRCA(projEnv$MlsCompsDF)
}



ColExistsNumeric <- function(r, nm, digits_=0) {
  a <- 0
  if (nm %in% colnames(projEnv$MlsCompsDF)) {
   
	a <- round(projEnv$MlsCompsDF[r, nm], digits = digits_)
  }
  return(a)
}


ColExists <- function(r, nm) {
  a <- ""
  if (nm %in% colnames(projEnv$MlsCompsDF)) {
    a <- projEnv$MlsCompsDF[r, nm]
  }
  return(a)
}

LoadSubjectData <- function() {
  saleDate <- projEnv$MlsCompsDF[1, "SaleDate"]
  closeDate <- projEnv$MlsCompsDF[1, "CloseDate"]

  saleMonth <-
    str_pad(month(as.POSIXlt(
      as.Date(saleDate, origin = "1899-12-31"),
      format = "%m/%d/%Y"
    )), width = 2, pad = "0")
  closeMonth <-
    str_pad(month(as.POSIXlt(
      as.Date(closeDate, origin = "1899-12-31"),
      format = "%m/%d/%Y"
    )), width = 2, pad = "0")

  saleDay <-
    str_pad(day(as.POSIXlt(
      as.Date(saleDate, origin = "1899-12-31"),
      format = "%m/%d/%Y"
    )), width = 2, pad = "0")
  closeDay <-
    str_pad(day(as.POSIXlt(
      as.Date(closeDate, origin = "1899-12-31"),
      format = "%m/%d/%Y"
    )), width = 2, pad = "0")

  saleYear <-
    str_pad(year(as.POSIXlt(
      as.Date(saleDate, origin = "1899-12-31"),
      format = "%m/%d/%Y"
    )) - 2000, width = 2, pad = "0")
	
  closeYear <-
    str_pad(year(as.POSIXlt(
      as.Date(closeDate, origin = "1899-12-31"),
      format = "%m/%d/%Y"
    )) - 2000, width = 2, pad = "0")


 
  saleDt <- str_c(saleMonth, "/", saleDay, "/", saleYear)
  closeDt <- str_c(closeMonth, "/", closeDay, "/", closeYear)
  projEnv$RcaInputDF[25, "S4"] <- "=C26-SUM(E27:E38)"
  print("abc1")
  
  projEnv$RcaInputDF[3, "S1"] <-
    paste(projEnv$MlsCompsDF[1, "Address"], projEnv$MlsCompsDF[1, "CityStateZip"])
  projEnv$RcaInputDF[4, "S1"] <- "N/A"
   projEnv$RcaInputDF[4, "S4"] <- paste(round(ColExists(1, "Proximity"),digits=2)," miles")
  projEnv$RcaInputDF[5, "S1"] <- ColExists(1,"ParcelNbr")
  projEnv$RcaInputDF[5, "S4"] <- "0 miles"
  projEnv$RcaInputDF[6, "S1"] <- "Factual Value"
  projEnv$RcaInputDF[6, "S4"] <- "Value Contrib."
  projEnv$RcaInputDF[7,  "S1"] <-""
  projEnv$RcaInputDF[7,  "S2"] <-""
  projEnv$RcaInputDF[7,  "S2"] <-""
  projEnv$RcaInputDF[7,  "S4"] <- round(as.numeric(projEnv$BaseValue ), digits=0)
      print("abc2")
  projEnv$RcaInputDF[8, "S1"] <- saleDt
  projEnv$RcaInputDF[8, "S2"] <- closeDt
  projEnv$RcaInputDF[8, "S3"] <- ColExistsNumeric(1, "DaysOffMkt")
  projEnv$RcaInputDF[8, "S4"] <- ColExistsNumeric(1, "Contrib_DaysOffMkt")
  projEnv$RcaInputDF[9, "S1"] <- ColExists(1, "Longitude")
  projEnv$RcaInputDF[9, "S2"] <- ColExists(1, "Latitude")
  projEnv$RcaInputDF[9, "S4"] <- ColExistsNumeric(1, "Contrib_Latitude") + ColExistsNumeric(1, "Contrib_Longitude")
  projEnv$RcaInputDF[10, "S1"] <- paste(ColExists(1, "LotSize"),"sf")
  projEnv$RcaInputDF[10, "S2"] <- "N/A"
  projEnv$RcaInputDF[10, "S4"] <- ColExistsNumeric(1, "Contrib_LotSize")
  projEnv$RcaInputDF[11, "S1"] <- ColExists(1, "Age")
  projEnv$RcaInputDF[11, "S2"] <- "N/A"
  projEnv$RcaInputDF[11, "S4"] <- ColExistsNumeric(1, "Contrib_Age")
  projEnv$RcaInputDF[12, "S1"] <- ColExists(1, "Beds")
  projEnv$RcaInputDF[12, "S2"] <- ColExists(1, "Baths")
  projEnv$RcaInputDF[12, "S4"] <- ColExistsNumeric(1, "Contrib_Beds")+ ColExistsNumeric(1, "Contrib_Baths")
  projEnv$RcaInputDF[13, "S1"] <- paste(ColExists(1, "GLA"),"sf")
  projEnv$RcaInputDF[13, "S4"] <- ColExistsNumeric(1, "Contrib_GLA")
  projEnv$RcaInputDF[14, "S1"] <- paste(ColExists(1, "BGLA"),"sf")
  projEnv$RcaInputDF[14, "S4"] <- ColExistsNumeric(1, "Contrib_BGLA")
  projEnv$RcaInputDF[15, "S1"] <- paste(ColExistsNumeric(1, "ULA"), "sf")
  projEnv$RcaInputDF[15, "S4"] <- ColExistsNumeric(1, "Contrib_ULA")
  projEnv$RcaInputDF[16, "S1"] <- paste(ColExists(1, "ADU"),"sf")
  projEnv$RcaInputDF[16, "S4"] <- ColExistsNumeric(1, "Contrib_ADU")
  projEnv$RcaInputDF[17, "S1"] <- paste(ColExists(1, "GarageSF"),"sf")
  projEnv$RcaInputDF[17, "S4"] <- ColExistsNumeric(1, "Contrib_GarageSF")
  projEnv$RcaInputDF[18, "S1"] <- ColExists(1, "FrPlcNbr")
  projEnv$RcaInputDF[18, "S4"] <- ColExistsNumeric(1, "Contrib_FrPlcNbr")
  projEnv$RcaInputDF[19, "S1"] <- ColExists(1, "Story")
  projEnv$RcaInputDF[19, "S4"] <- ColExistsNumeric(1, "Contrib_Story")
  projEnv$RcaInputDF[19, "S1"] <- ""
  projEnv$RcaInputDF[20, "S1"] <- ""
  projEnv$RcaInputDF[21, "S1"] <- ""
  projEnv$RcaInputDF[22, "S1"] <- ""
  projEnv$RcaInputDF[23, "S1"] <- ""
  projEnv$RcaInputDF[24, "S1"] <- "CQA/Description"
  projEnv$RcaInputDF[24, "S4"] <- "Value Contrib."
  projEnv$RcaInputDF[25, "S1"] <- ColExists(1, "CQA2")
  projEnv$RcaInputDF[25, "S2"] <- ColExists(1, "Residual2")
  projEnv$RcaInputDF[26, "S1"] <- ""
  projEnv$RcaInputDF[27, "S1"] <- ""
  projEnv$RcaInputDF[28, "S1"] <- ""
  projEnv$RcaInputDF[29, "S1"] <- ""
  projEnv$RcaInputDF[30, "S1"] <- ""
  projEnv$RcaInputDF[31, "S1"] <- ""
  projEnv$RcaInputDF[32, "S1"] <- ""
  projEnv$RcaInputDF[33, "S1"] <- ""
  projEnv$RcaInputDF[34, "S1"] <- ""
  projEnv$RcaInputDF[35, "S1"] <- ""
  projEnv$RcaInputDF[36, "S1"] <- ""
  projEnv$RcaInputDF[37, "S1"] <- ""
  projEnv$RcaInputDF[38, "S1"] <- ""
  projEnv$RcaInputDF[39, "S1"] <- ""
  projEnv$RcaInputDF[40, "S1"] <- ""
  projEnv$RcaInputDF[41, "S1"] <- ""
    print("abc9")
}


LoadCompData <- function(r ) {
  print(paste("LoadCompData",r))
  rm1 <- r - 2
  c <- mod(rm1, 3) + 1
  cnfv1 <- paste("C", c, "FV", 1, sep = "")
  cnfv2 <- paste("C", c, "FV", 2, sep = "")
  cnfv3 <- paste("C", c, "FV", 3, sep = "")

  cnvc1 <- paste("C", c, "VC1", sep = "")
  cna1 <- paste("C", c, "A1", sep = "")
  saleDate <- projEnv$MlsCompsDF[r, "SaleDate"]
  closeDate <- projEnv$MlsCompsDF[r, "CloseDate"]

  saleMonth <-
    str_pad(month(as.POSIXlt(
      as.Date(saleDate, origin = "1899-12-31"),
      format = "%m/%d/%Y"
    )), width = 2, pad = "0")
  closeMonth <-
    str_pad(month(as.POSIXlt(
      as.Date(closeDate, origin = "1899-12-31"),
      format = "%m/%d/%Y"
    )), width = 2, pad = "0")

  saleDay <-
    str_pad(day(as.POSIXlt(
      as.Date(saleDate, origin = "1899-12-31"),
      format = "%m/%d/%Y"
    )), width = 2, pad = "0")
  closeDay <-
    str_pad(day(as.POSIXlt(
      as.Date(closeDate, origin = "1899-12-31"),
      format = "%m/%d/%Y"
    )), width = 2, pad = "0")


  saleYear <-
    str_pad(year(as.POSIXlt(
      as.Date(saleDate, origin = "1899-12-31"),
      format = "%m/%d/%Y"
    )) - 2000, width = 2, pad = "0")
  closeYear <-
    str_pad(year(as.POSIXlt(
      as.Date(closeDate, origin = "1899-12-31"),
      format = "%m/%d/%Y"
    )) - 2000, width = 2, pad = "0")

  saleDt <- str_c(saleMonth, "/", saleDay, "/", saleYear)
  closeDt <- str_c(closeMonth, "/", closeDay, "/", closeYear)
   projEnv$RcaInputDF[2,"Features"] <- "Feature"
  projEnv$RcaInputDF[2,"S1"] <- "Subject"
    projEnv$RcaInputDF[2,cnfv1] <- paste("Comparable ",c)
 
  print(paste("Y1"))
  projEnv$RcaInputDF[2, cnfv1] <- paste("Comparable ", r-1,sep="")
  projEnv$RcaInputDF[3, cnfv1] <-  paste(projEnv$MlsCompsDF[r, "Address"], projEnv$MlsCompsDF[r, "CityStateZip"])
  projEnv$RcaInputDF[4, cnfv1] <-  projEnv$MlsCompsDF[r, "ParcelNbr"]
  projEnv$RcaInputDF[4, cnfv2] <-  projEnv$MlsCompsDF[r, "MlsNbr"]
  projEnv$RcaInputDF[4, cnvc1] <-  paste(round(ColExists(r, "Proximity"),digits=2)," miles")

  projEnv$RcaInputDF[5, cnfv1] <-  ColExistsNumeric(r, "ContractSP",1)  
  projEnv$RcaInputDF[5, cnvc1] <-  ColExistsNumeric(r, "Concessions",1)
 # projEnv$RcaInputDF[5, cna1] <-  ColExistsNumeric(r, "SalePrice")
  projEnv$RcaInputDF[6, cnfv1] <- "Factual Value"
  projEnv$RcaInputDF[6, cnvc1] <- "Value Contrib."
  projEnv$RcaInputDF[6, cna1]  <- "Adjustment"
  # projEnv$RcaInputDF[7,  "S1"] <-"Date of Sale | OffMkt | OnMkt"
  projEnv$RcaInputDF[7, cnvc1] <-  round(as.numeric(projEnv$BaseValue) , digits=0)
  projEnv$RcaInputDF[8, cnfv1] <-  saleDt
  projEnv$RcaInputDF[8, cnfv2] <-  closeDt
  projEnv$RcaInputDF[8, cnfv3] <-  ColExistsNumeric(r, "DaysOffMkt")
  projEnv$RcaInputDF[8, cnvc1] <-  ColExistsNumeric(r, "Contrib_DaysOffMkt")
  print(paste("Y2"))
  projEnv$RcaInputDF[9, cnfv1]  <- ColExistsNumeric(r,"Longitude",3) 
  projEnv$RcaInputDF[9, cnfv2]  <- ColExistsNumeric(r,"Latitude",3) 
  projEnv$RcaInputDF[9, cnvc1]  <- ColExistsNumeric(r, "Contrib_Latitude") + ColExistsNumeric(r, "Contrib_Longitude")
  projEnv$RcaInputDF[10, cnfv1] <- paste(ColExistsNumeric(r, "LotSize"),"sf")
  projEnv$RcaInputDF[10, cnfv2] <- "N/A" # Dimensions
  projEnv$RcaInputDF[10, cnvc1] <- ColExistsNumeric(r, "Contrib_LotSize") 
  projEnv$RcaInputDF[11, cnfv1] <- ColExistsNumeric(r, "Age")
  projEnv$RcaInputDF[11, cnfv2] <- "N/A"
  projEnv$RcaInputDF[11, cnvc1] <- paste("$",ColExistsNumeric(r, "Contrib_Age"),sep="")
  projEnv$RcaInputDF[12, cnfv1] <- ColExistsNumeric(r, "Beds")
  projEnv$RcaInputDF[12, cnfv2] <- ColExistsNumeric(r, "Baths")
  projEnv$RcaInputDF[12, cnvc1] <- ColExistsNumeric(r, "Contrib_Beds")+ ColExistsNumeric(r, "Contrib_Baths")
  projEnv$RcaInputDF[13, cnfv1] <- paste(ColExistsNumeric(r, "GLA"),"sf")
  projEnv$RcaInputDF[13, cnvc1] <-  ColExistsNumeric(r, "Contrib_GLA")
  projEnv$RcaInputDF[14, cnfv1] <- paste(ColExistsNumeric(r, "BGLA"),"sf")
  projEnv$RcaInputDF[14, cnvc1] <- ColExistsNumeric(r, "Contrib_BGLA")
  projEnv$RcaInputDF[15, cnfv1] <- paste(ColExistsNumeric(r, "ULA"),"sf")
  projEnv$RcaInputDF[15, cnvc1] <- ColExistsNumeric(r, "Contrib_ULA")
  projEnv$RcaInputDF[16, cnfv1] <- paste(ColExistsNumeric(r, "ADU"),"sf")
  projEnv$RcaInputDF[16, cnvc1] <- ColExistsNumeric(r, "Contrib_ADU")
  projEnv$RcaInputDF[17, cnfv1] <- paste(ColExistsNumeric(r, "GarageSF"),"sf")
  projEnv$RcaInputDF[17, cnvc1] <- ColExistsNumeric(r, "Contrib_GarageSF")
  projEnv$RcaInputDF[18, cnfv1] <- ColExistsNumeric(r, "FrPlcNbr")
  projEnv$RcaInputDF[18, cnvc1] <- ColExistsNumeric(r, "Contrib_FrPlcNbr")
  projEnv$RcaInputDF[19, cnfv1] <- ColExistsNumeric(r, "Story")
  projEnv$RcaInputDF[19, cnvc1] <- ColExistsNumeric(r, "Contrib_Story")
	projEnv$RcaInputDF[20, cnfv1] <- ""
  projEnv$RcaInputDF[20, cnvc1] <- 0
  projEnv$RcaInputDF[21, cnfv1] <- ""
  projEnv$RcaInputDF[21, cnvc1] <- 0
  projEnv$RcaInputDF[22, cnfv1] <- ""
  projEnv$RcaInputDF[22, cnvc1] <- 0
  projEnv$RcaInputDF[23, cnfv1] <- ""
  projEnv$RcaInputDF[23, cnvc1] <- 0
  projEnv$RcaInputDF[24, cnfv1] <- ""
  projEnv$RcaInputDF[24, cnvc1] <- 0
  projEnv$RcaInputDF[25, cnfv1] <- ""
  projEnv$RcaInputDF[25, cnvc1] <- 0
  projEnv$RcaInputDF[26, cnfv1] <- ""
  projEnv$RcaInputDF[26, cnvc1] <- 0
  print(paste("Y3"))
  if (projEnv$ResidualMethod == "Residual") {
    projEnv$RcaInputDF[25, cnfv1] <- projEnv$MlsCompsDF[r, "CQA"]
    projEnv$RcaInputDF[25, cnfv2] <-
      projEnv$MlsCompsDF[r, "Residual"]
  } else {
    projEnv$RcaInputDF[25, cnfv1] <- projEnv$MlsCompsDF[r, "CQA2"]
    projEnv$RcaInputDF[25, cnfv2] <-
      ColExistsNumeric(r, "Residual")
  }
   projEnv$RcaInputDF[25, "S1"] <- ColExists(1, "CQA2")
  projEnv$RcaInputDF[25, "S2"] <- ColExists(1, "Residual2")
 
  print(paste("Y5"))
  # 25: see above
  projEnv$RcaInputDF[26, cnvc1] <- 0
  projEnv$RcaInputDF[27, cnvc1] <- 0
  projEnv$RcaInputDF[28, cnvc1] <- 0
  projEnv$RcaInputDF[29, cnvc1] <- 0
  projEnv$RcaInputDF[30, cnvc1] <- 0
  projEnv$RcaInputDF[31, cnvc1] <- 0
  projEnv$RcaInputDF[32, cnvc1] <- 0
  projEnv$RcaInputDF[33, cnvc1] <- 0
  projEnv$RcaInputDF[34, cnvc1] <- 0
  projEnv$RcaInputDF[35, cnvc1] <- 0
  projEnv$RcaInputDF[36, cnvc1] <- 0
  projEnv$RcaInputDF[37, cnvc1] <- 0
  # 38: see above
  projEnv$RcaInputDF[39, "S1"] <- ""
  #projEnv$RcaInputDF[40, "S1"] <- ""
  projEnv$RcaInputDF[40, cnvc1] <- paste("Net Adj: ",round(ColExistsNumeric(r, "SG_Net")/ColExistsNumeric(r, "SalePrice")*100,digits=1),"%",sep="")
  projEnv$RcaInputDF[41, cnvc1] <- paste("Grs Adj: ",round(ColExistsNumeric(r, "SG_Gross")/ColExistsNumeric(r, "SalePrice")*100,digits=1),"%",sep="")
  
  
  print(paste("Y6"))
}

AbortRun <- FALSE

#' SetUp
#'  @projEnv Project Environment
#'  @codeFolder codeFolder,
#'  @projectParentFolder Project files parent folder
#'  @projectID Project ID
#'  @mlsComps MLS Data File
#'  @RcaInput MLS Data File Sheet Name
#'
#'  @example
#'  Stage_1(projEnv)
#'
#' Out main Stage I function:
RCA_SalesGrid <-
  function() {
    ##  !!!!!!   YOU need to set the Version Folder int RunModuleto Use for the Import (Usually the last one created)
    InitDataFrames()
	RcaCreateWorksheets() 
	Initialize_RcaInputDF() 
    RcaWBStyle ()
    LoadCompsToDataFrameRCA()
	  
   
	 
    # Features and Subject data only has to be set in projEnv$RcaInputDF once
	# This is the first column of the Sales Grid
    SetUpFeatures()
	
	# Load the subject data
    LoadSubjectData()
  
    # now iterate through the top 12 compos 3 at a time, loading their data into the data frame:
    for (i in 2:13) {
	 sheet <- floor((i - 2) / 3) + 1
	 rm1 <- i - 2
     c <- mod(rm1, 3) + 1
	  c1fv1 <- paste("C", c, "FV", 1, sep = "")
	  c1fv2 <- paste("C", c, "FV", 2, sep = "")
	  c1fv3 <- paste("C", c, "FV", 3, sep = "")

	  c1vc1 <- paste("C", c, "VC1", sep = "")
	  c1a1 <- paste("C", c, "A1", sep = "")
  
      print(paste("i: ", i))
      # Set the RCA page title to "Comps <from>-<to>"
      cfirst <-    (floor((i-2)/3)*3 +1)
      clast <-    (cfirst + 2)
      projEnv$RcaInputDF[1, "Features"] <-
        paste("Sales Comparable Grid: ", "Comps ", cfirst, "-", clast,
          sep =
            ""
        )
      print(projEnv$RcaInputDF[1, "Features"])

      # Load the next 3 comps into current page
       LoadCompData(i)
    
      if (mod((i - 1), 3) == 0) {
        k <- floor((i - 2) / 3) + 1
        print(paste("K: ", k))

        writeDataTable(
        projEnv$RcaWB,
       
        xy=NULL,
		sheet=k,
        x=projEnv$RcaInputDF,
		 withFilter = openxlsx_getOp("withFilter", FALSE),
		 tableStyle = openxlsx_getOp("tableStyle", "none"),
        tableName = NULL,
        startCol = 1,
        startRow = 1,
        colNames = TRUE,
        rowNames = FALSE,
		keepNA = openxlsx_getOp("keepNA", FALSE),
        na.string = openxlsx_getOp("na.string"),
		stack=TRUE,
		firstColumn=FALSE 
		 
        )
	 
		writeFormula(projEnv$RcaWB,sheet=k, x= "C26-SUM(E27:E39),0)" , startRow=26,startCol=5)
	    writeFormula(projEnv$RcaWB,sheet=k, x= "SUM(I8:I24)+SUM(I26:I39)" , startRow=40,startCol=5)
	    writeFormula(projEnv$RcaWB,sheet=k, x= "F6-I6" , startRow=6,startCol=10)
		writeFormula(projEnv$RcaWB,sheet=k, x= "K6-N6" , startRow=6,startCol=15)
		writeFormula(projEnv$RcaWB,sheet=k, x= "P6-S6" , startRow=6,startCol=20)
		 
		writeFormula(projEnv$RcaWB,sheet=k, x= "C26-SUM(D27:D39)" , startRow=26,startCol=5)
		writeFormula(projEnv$RcaWB,sheet=k, x= "G26-SUM(I27:I39)" , startRow=26,startCol=9)
		writeFormula(projEnv$RcaWB,sheet=k, x= "L26-SUM(N27:N38)" , startRow=26,startCol=14)
	    writeFormula(projEnv$RcaWB,sheet=k, x= "Q26-SUM(S27:S38)" , startRow=26,startCol=19)
			
		writeFormula(projEnv$RcaWB,sheet=k, x= "SUM(E8:E24)+SUM(E26:E39)" , startRow=40,startCol=5)
		writeFormula(projEnv$RcaWB,sheet=k, x= "SUM(I8:I24)+SUM(I26:I39)" , startRow=40,startCol=9)
		writeFormula(projEnv$RcaWB,sheet=k, x= "SUM(N8:N24)+SUM(N26:N39)" , startRow=40,startCol=14)
		writeFormula(projEnv$RcaWB,sheet=k, x= "SUM(S8:S24)+SUM(S26:S39)" , startRow=40,startCol=19)
		
		writeFormula(projEnv$RcaWB,sheet=k, x= "SUM(J8:J24)+SUM(J26:J39)" , startRow=40,startCol=10)
		writeFormula(projEnv$RcaWB,sheet=k, x= "SUM(O8:O24)+SUM(O26:O39)" , startRow=40,startCol=15)
		writeFormula(projEnv$RcaWB,sheet=k, x= "SUM(T8:T24)+SUM(T26:T39)" , startRow=40,startCol=20)
		
		writeFormula(projEnv$RcaWB,sheet=k, x= "J6+SUM(J8:J24)+SUM(J26:J39)" , startRow=42,startCol=10)
		writeFormula(projEnv$RcaWB,sheet=k, x= "O6+SUM(O8:O24)+SUM(O26:O39)" , startRow=42,startCol=15)
		writeFormula(projEnv$RcaWB,sheet=k, x= "T6+SUM(T8:T24)+SUM(T26:T39)" , startRow=42,startCol=20)
		 
		
		# comp 1 adjustment calcs
		for(n in c(8:24,26:39)) {
		  eFormula <- paste("E",n,"-","I",n,sep="")
		  print(eFormula)
		  writeFormula(projEnv$RcaWB,sheet=k, x= eFormula , startRow=n,startCol=10)
    	 }
	 
		
	 
		for(n in c(8:24,26:38)) {
		  eFormula <- paste("E",n,"-","N",n,sep="")
		  writeFormula(projEnv$RcaWB,sheet=k, x= eFormula , startRow=n,startCol=15)
    	 }
		 

	
	 
		for(n in c(8:24,26:38)) {
		  eFormula <- paste("E",n,"-","S",n,sep="")
		  writeFormula(projEnv$RcaWB,sheet=k, x= eFormula , startRow=n,startCol=20)
    	 }
	 
	#	writeFormula(projEnv$RcaWB,sheet=k, x= "J39/J6" , startRow=41,startCol=9)
		#writeFormula(projEnv$RcaWB,sheet=k, x= "J39/J6" , startRow=41,startCol=9)	  
	#	writeFormula(projEnv$RcaWB,sheet=k, x= "SUM(ABS(J8:J24))+SUM(ABS(J26:J38))" , startRow=42,startCol=9)
		#writeFormula(projEnv$RcaWB,sheet=k, x= "SUM(ABS(O8:O24))+SUM(ABS(O26:O38))" , startRow=42,startCol=14)
	#	writeFormula(projEnv$RcaWB,sheet=k, x= "SUMIF(T8:T24,\">0\"))-SUMIF(T26:T38),\"<0\")" , startRow=42,startCol=19)  
       # writeData(
          # projEnv$RcaWB,
          # k,
          # projEnv$RcaInputDF,
          # startCol = 1,
          # startRow = 1,
          # array = FALSE,
          # xy = NULL,
		  
          # colNames = TRUE,
          # rowNames = FALSE,
          # borders = openxlsx_getOp("borders", "surrounding"),
          # borderColour = openxlsx_getOp("borderColour", "darkblue"),
          # borderStyle = openxlsx_getOp("borderStyle", "thick"),
          # withFilter = openxlsx_getOp("withFilter", FALSE),
          # keepNA = openxlsx_getOp("keepNA", FALSE),
          # na.string = NULL,   #openxlsx_getOp("na.string"),
          # name = NULL,
          # sep = ", "
        # )      
     
	 
      }
    }
   # RcaWBStyleUpdate ()
    saveWorkbook(projEnv$RcaWB,
      projEnv$RcaInputFile,
      overwrite = TRUE   )
  }
