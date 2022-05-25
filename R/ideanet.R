ideanet<-function(){

clcol_15 <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#CDCD00", "#A65628", "#F781BF", "#999999", "#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02") # 15 colors
clcol_grey <- c(clcol_15, "#A9A9A9") # 15 colors + grey
# subset the color palette for each cluster solution so that the color for a cluster number is the same in all plots (e.g. CL#1 is always E41A1C)
clcol_5 <- clcol_grey[c(1:5)]
clcol_6 <- clcol_grey[c(1:6)]
clcol_7 <- clcol_grey[c(1:7)]
clcol_8 <- clcol_grey[c(1:8)]
clcol_9 <- clcol_grey[c(1:9)]
clcol_10 <- clcol_grey[c(1:10)]
clcol_11 <- clcol_grey[c(1:11)]
clcol_12 <- clcol_grey[c(1:12)]
clcol_13 <- clcol_grey[c(1:13)]
clcol_14 <- clcol_grey[c(1:14)]
clcol_15 <- clcol_grey[c(1:15)]

# . . ggplot2::ggplot themes -----
th1 <- theme(
  plot.margin = unit(c(1, 1, 1, 1), "in"),
  panel.background = element_rect(fill = "white"),
  axis.ticks.y = element_blank(), axis.text.y = element_blank(),
  axis.ticks.x = element_blank(), axis.text.x = element_blank(),
  axis.title.y = element_blank(), axis.title.x = element_blank()
)
th1.1 <- theme(
  plot.margin = unit(c(0, 0, 0, 0), "in"),
  panel.background = element_rect(fill = "white"),
  axis.ticks.y = element_blank(), axis.text.y = element_blank(),
  axis.ticks.x = element_blank(), axis.text.x = element_blank(),
  axis.title.y = element_blank(), axis.title.x = element_blank(),
)
th2 <- theme(legend.position = "right") # format plot global settings for the cluster plots
th3 <- geom_vline(xintercept = 2, linetype = "solid", color = "grey", size = 1)
th4 <- geom_vline(xintercept = 1, linetype = "solid", color = "grey", size = 1)
th5 <- theme(
  plot.margin = unit(c(1, 1, 1, 1), "in"), panel.background = element_rect(fill = "white"),
  axis.ticks.y = element_blank(), axis.text.y = element_blank(),
  axis.ticks.x = element_blank(), axis.text.x = element_blank(),
  axis.title.y = element_blank(), axis.title.x = element_blank()
)
th6 <- theme(legend.position = "right") # format plot global settings for the cluster plots
th7 <- theme(legend.position = "none") # remove legend when the cluster names are used as labels
th8 <- theme(
  panel.background = element_rect(fill = "white"),
  axis.ticks.x = element_blank(),
  axis.title.x = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks.y = element_blank(),
  axis.title.y = element_blank()
)
th9 <- theme_void()

# . . fonts for tcltk -----
fontHead <-tcltk::tkfont.create(family = "times", size = 12, weight = "bold")
fontSub <- tcltk::tkfont.create(family = "times", size = 10, weight = "bold")
fontQ <- tcltk::tkfont.create(family = "times", size = 12)
fontQ <- tcltk::tkfont.create(family = "times", size = 10)
fontQ.s <- tcltk::tkfont.create(family = "times", size = 8)

# . . location for graphics on slides -----
# cluster report
ft_location1 <- ph_location(left = 0.25, top = 1, width = 3, height = 4) # location flextable
ft_location2 <- ph_location(left = 5, top = 1, width = 3, height = 4) # location flextable
map_loc1 <- ph_location(left = 2.5, top = 1.5, width = 5, height = 5) # location of map on slide with title only
map_loc2 <- ph_location(left = 4.5, top = 1, width = 5, height = 5) # location map on slide with flextable

# pattern analysis
pa_title <- fpar(ftext(
  "Pattern analysis - Cluster rating map",
  fp_text(color = "black", font.size = 14, bold = TRUE)
))

pa_title_loc <- ph_location(
  left = 2, top = 0.25,
  width = 4, height = 1,
  rotation = 0, bg = "white"
)

pa_layer_loc <- ph_location(left = 1, top = 1, height = 6, width = 7) # pattern analysis layer map
pa_ft_loc <- ph_location(left = 0.5, top = 2, height = 5, width = 7)
pa_choices_loc <- ph_location(left = 1, top = 2, height = 5, width = 5)

# pattern matching
ladder_loc <- ph_location(left = 3, top = 1, height = 4, width = 4)
ft_ladder_left_loc1 <- ph_location(left = 0.5, top = 1, height = 4, width = 2)
ft_ladder_right_loc1 <- ph_location(left = 7.5, top = 1, height = 4, width = 2)
ft_ladder_scale_loc <- ph_location(left = 1, top = 0.5, width = 7)
ft_ladder_left_loc2 <- ph_location(left = .5, top = 5, height = 4, width = 2)
ft_ladder_right_loc2 <- ph_location(left = 7.5, top = 5, height = 4, width = 2)
ladder_rescale_loc <- ph_location(left = 3, top = 1, height = 4, width = 4)

# ********************************************************************************************** -----
# functions used in multiple calls ------
input_wd <- function() {
  input_file_base <- basename(input_file) # extract the file name
  input_dir <- stringr::str_remove(input_file, input_file_base) # remove the filename from the path
  assign("input_dir", input_dir, envir = .GlobalEnv) # assign so available to other functions
  assign("input_file_base", input_file_base, envir = .GlobalEnv) # assign so available to other functions
  setwd(input_dir)
}

output_wd <- function() {
  output_file_base <- basename(output_file) # extract the file name
  assign("output_file_base", output_file_base, envir = .GlobalEnv) # assign so available to other functions
  output_dir <- stringr::str_remove(output_file, output_file_base) # remove the filename from the path
  setwd(output_dir) # set the working dir to be the same location as the input file.
}

get_hulls <- function(output_result) {
  find_hull <- function(output_result) output_result[chull(output_result$dim1, output_result$dim2), ]
  all_hulls <- NULL

  # . . replace plyr with dplyr -----
  for (x in 5:15) {
    clu_name <- paste("CLU", x, sep = "")

    # . . replace plyr with dplyr -----
    hulls <- plyr::ddply(output_result, clu_name, find_hull)
    hulls$cluster <- x
    all_hulls <- rbind(all_hulls, data.frame(hulls))
  }
  assign("all_hulls", all_hulls, envir = .GlobalEnv) # assign to parent environ where called
}

cluster_means <- function(output_result) {

  # start here . . replace plyr with dplyr -----
  # . . calculate the cluster mean for each cluster in the  cluster solution
  clu5_mean <- plyr::ddply(output_result, 'CLU5', summarize, ldim1 = mean(dim1), ldim2 = mean(dim2)) # cluster centers
  clu6_mean <- plyr::ddply(output_result, 'CLU6', summarize, ldim1 = mean(dim1), ldim2 = mean(dim2))
  clu7_mean <- plyr::ddply(output_result, 'CLU7', summarize, ldim1 = mean(dim1), ldim2 = mean(dim2))
  clu8_mean <- plyr::ddply(output_result, 'CLU8', summarize, ldim1 = mean(dim1), ldim2 = mean(dim2))
  clu9_mean <- plyr::ddply(output_result, 'CLU9', summarize, ldim1 = mean(dim1), ldim2 = mean(dim2))
  clu10_mean <- plyr::ddply(output_result, 'CLU10', summarize, ldim1 = mean(dim1), ldim2 = mean(dim2))
  clu11_mean <- plyr::ddply(output_result, 'CLU11', summarize, ldim1 = mean(dim1), ldim2 = mean(dim2))
  clu12_mean <- plyr::ddply(output_result, 'CLU12', summarize, ldim1 = mean(dim1), ldim2 = mean(dim2))
  clu13_mean <- plyr::ddply(output_result, 'CLU13', summarize, ldim1 = mean(dim1), ldim2 = mean(dim2))
  clu14_mean <- plyr::ddply(output_result, 'CLU14', summarize, ldim1 = mean(dim1), ldim2 = mean(dim2))
  clu15_mean <- plyr::ddply(output_result, 'CLU15', summarize, ldim1 = mean(dim1), ldim2 = mean(dim2))

  # change the colnames so df can be rbind
  colnames(clu5_mean)[1] <- "CLU"
  colnames(clu6_mean)[1] <- "CLU"
  colnames(clu7_mean)[1] <- "CLU"
  colnames(clu8_mean)[1] <- "CLU"
  colnames(clu9_mean)[1] <- "CLU"
  colnames(clu10_mean)[1] <- "CLU"
  colnames(clu11_mean)[1] <- "CLU"
  colnames(clu12_mean)[1] <- "CLU"
  colnames(clu13_mean)[1] <- "CLU"
  colnames(clu14_mean)[1] <- "CLU"
  colnames(clu15_mean)[1] <- "CLU"

  # add integer value of cluster for filtering
  clu5_mean$cluster <- 5
  clu6_mean$cluster <- 6
  clu7_mean$cluster <- 7
  clu8_mean$cluster <- 8
  clu9_mean$cluster <- 9
  clu10_mean$cluster <- 10
  clu11_mean$cluster <- 11
  clu12_mean$cluster <- 12
  clu13_mean$cluster <- 13
  clu14_mean$cluster <- 14
  clu15_mean$cluster <- 15

  # rbind dfs for filtering
  all_mean_clu <- rbind(
    clu5_mean, clu6_mean, clu7_mean, clu8_mean, clu9_mean, clu10_mean,
    clu11_mean, clu12_mean, clu13_mean, clu14_mean, clu15_mean
  )
  assign("all_mean_clu", all_mean_clu, envir = .GlobalEnv) # assign to parent environ where called
}

# get source data - sorts and ratings
get_input <- function() {

  # open dialogue to choose file
  input_file <- tcltk::tclvalue(tcltk::tkgetOpenFile(filetypes = "{{Excel Spreadsheet} {.xlsx}}
                                         {{All files} *}"))
  assign("input_file", input_file, envir = .GlobalEnv) # assign so accessible to other functions

  # check if wks is present
  input_names <- getSheetNames(input_file)
  input_sheets <- if (("values" %in% input_names) == FALSE) missing_input_sheet()
  assign("input_names", input_names, envir = .GlobalEnv)

  # read in the wb
  input_result <- read.xlsx(input_file, sheet = "values", skipEmptyCols = FALSE)
  input_values_def <- read.xlsx(input_file,
    sheet = "values_def", startRow = 1,
    skipEmptyRows = FALSE, skipEmptyCols = FALSE
  )

  input_values_def[is.na(input_values_def)] <- " " # replace NA with blank

  # verify that input wks has data nrow>0, ncol=>17
  input_rows <- ifelse(nrow((input_result) > 0), "yes", "no")
  input_cols <- ifelse(ncol((input_result) >= 5), "yes", "no")

  if (input_rows == "no") no_input_data()
  if (input_cols == "no") missing_input_data()

  # if no errors
  assign("input_result", input_result, envir = .GlobalEnv)
  assign("input_values_def", input_values_def, envir = .GlobalEnv)

} # end get_input function

# get worksheet with  multiple cluster solutions for cluster report for a single cluster solution and pattern analysis
get_output <- function() {

  # open dialogue to choose file
  output_file <- tcltk::tclvalue(tcltk::tkgetOpenFile(filetypes = "{{Excel Spreadsheet} {.xlsx}}
                                         {{All files} *}"))

  # check if output wks is present
  output_names <- getSheetNames(output_file)
  output_sheets <- if (("output" %in% output_names) == FALSE) missing_output_sheet()

  # read in the wb
  output_result <- read.xlsx(output_file, sheet = "output")

  # verify that output wks has data nrow>0, ncol=>17
  output_rows <- ifelse(nrow(na.omit(output_result) > 0), "yes", "no")
  output_cols <- ifelse(nrow(na.omit(output_result) >= 17), "yes", "no")

  if (output_rows == "no") no_output_data()
  if (output_cols == "no") missing_output_data()

  # if no errors
  assign("output_file", output_file, envir = .GlobalEnv)
  assign("output_names", output_names, envir = .GlobalEnv)
  output_result[3:13] <- lapply(output_result[3:13], as.factor) # if no errors convert all cluster values to factors for grouping
  assign("output_result", output_result, envir = .GlobalEnv)

  output_wd() # get the path from location of output.xlsx and save pptx in same location
} # end get_output function

# check if top label wks is present
get_labels <- function(output_names, output_file) {
  label_wks <- paste("cluster", clus_chosen, sep = "")
  if ((label_wks %in% output_names) == FALSE) missing_label_sheet()
  top_labels <- read.xlsx(output_file, sheet = label_wks)
  assign("top_labels", top_labels, envir = .GlobalEnv)
} # end get_labels function

select_measure <- function() {
  measure_choice <- tcltk::tclvalue(choose_measure)
  if (measure_choice != "Select a measure") {
    assign("measure_choice", measure_choice, envir = .GlobalEnv)
  }
  if (measure_choice == "Select a measure") tcltk::tk_messageBox(title = "Well...", message = "Select a number of measures")
} # end select_measure

# ********************************************************************************************** -----
# error messages  to user -----
no_ideas <- function() {
  tcltk::tk_messageBox(type = "ok", message = "There is no item text in column B of the ideas worksheet. This program will now stop. Open excel workbook and add items.")
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}
both_sorts <- function() {
  tcltk::tk_messageBox(type = "ok", message = "There is sort data in BOTH the racked and stacked worksheets. Only one type of sort data can be used. This program will now stop. Open excel workbook and remove data from either racked or stacked.")
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}
no_sorts <- function() {
  tcltk::tk_messageBox(type = "ok", message = "There is no sort data. This program will now stop. Open excel workbook and add sort data to either racked or stacked.")
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}
no_output_data <- function() {
  tcltk::tk_messageBox(type = "ok", message = "Excel file does not have data. This program will now stop. Open excel workbook and add items.")
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}
no_input_data <- function() {
  tcltk::tk_messageBox(type = "ok", message = "Excel file does not have values/rating data. This program will now stop. Open excel workbook and add items.")
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}
missing_output_data <- function() {
  tcltk::tk_messageBox(type = "ok", message = "One or more columns of data are missing. Should have 17 columns. This program will now stop. Open excel workbook and add items.")
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}
missing_input_data <- function() {
  tcltk::tk_messageBox(type = "ok", message = "There are no columns of ratinng data.  This program will now stop. Open excel workbook and add items.")
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}
missing_output_sheet <- function() {
  tcltk::tk_messageBox(type = "ok", message = "Output worksheet is missing. This program will now stop.")
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}
missing_label_sheet <- function() {
  tcltk::tk_messageBox(type = "ok", message = "The label worksheet for this cluster solution is missing.")
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}
invalid_value <- function() {
  tcltk::tk_messageBox(type = "ok", message = "The value entered for cluster reports is not valid, value needs to be an integer between 5 and 15 inclusive. Enter a valid value. ")
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}
missing_input_sheet <- function() {
  tcltk::tk_messageBox(type = "ok", message = "values worksheet and/or values_def worksheet are missing from input excel file. This program will now stop.")
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}
invalid_value_measure <- function() {
  tcltk::tk_messageBox(type = "ok", message = "The value entered for number of measures is not valid, value needs to be either 1 or 2. Enter a valid value. ")
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}
invalid_measure_name <- function() {
  tcltk::tk_messageBox(type = "ok", message = "The measure name is either missing or not valid (e.g, not text).")
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}
invalid_scale_value <- function() {
  tcltk::tk_messageBox(type = "ok", message = "The scale value is either missing or is not valid (e.g., not an integer).")
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}
invalid_scale_anchor <- function() {
  tcltk::tk_messageBox(type = "ok", message = "The values for min and max are not valid. Values may be missing or min is greater than max.")
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}
missing_rating_data <- function() {
  tcltk::tk_messageBox(type = "ok", message = "There is no rating data.")
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}
rating_scale_mismatch <- function() {
  tcltk::tk_messageBox(type = "ok", message = "The data is outside of the bounds for the min or max of scale.")
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}
unequal_rating_var <- function(rating_col) {
  unequal_var <- paste("There are ", rating_col, " measurement variables. There are two measures and the number of variables for each measure is unequal.  Please check the rating data. The number of variables for each measure needs to be equal.")
  tcltk::tk_messageBox(type = "ok", message = unequal_var)
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}
# ********************************************************************************************** -----
# create empty workbook formatted for data input -----
create_input_wb <- function() {

  # clears frames from canvas when a new menu item is chosen at top level menu
  reset_canvas <- function() {
    if (exists("lpw.1")) tcltk::tkdestroy(lpw.1)
    if (exists("lpw.2")) tcltk::tkdestroy(lpw.2)
    if (exists("lpw.3")) tcltk::tkdestroy(lpw.3)
    if (exists("lpw.4")) tcltk::tkdestroy(lpw.4)
    if (exists("lpw.5")) tcltk::tkdestroy(lpw.5)
    if (exists("lpw.6")) tcltk::tkdestroy(lpw.6)
    if (exists("lpw.7")) tcltk::tkdestroy(lpw.7)
    if (exists("lpw.8")) tcltk::tkdestroy(lpw.8)
    if (exists("lpw.9")) tcltk::tkdestroy(lpw.9)
    if (exists("lpw.10")) tcltk::tkdestroy(lpw.10)
    if (exists("lpw.11")) tcltk::tkdestroy(lpw.11)
    if (exists("lpw.12")) tcltk::tkdestroy(lpw.12)
    if (exists("lpw.13")) tcltk::tkdestroy(lpw.13)
    if (exists("compute.layer.but")) tcltk::tkdestroy(compute.layer.but)
    if (exists("reset.but")) tcltk::tkdestroy(reset.but)
  } # end reset_canvas
  reset_canvas() # clear canvas

  # . . create wb -----
  input_wb <- createWorkbook()
  addWorksheet(input_wb, "directions")
  addWorksheet(input_wb, "stacked")
  addWorksheet(input_wb, "racked")
  addWorksheet(input_wb, "ideas")
  addWorksheet(input_wb, "values")
  addWorksheet(input_wb, "values_def")

  # . . instructions -----
  df_instruction <- (as.data.frame(matrix(nrow = 35, ncol = 1)))
  names(df_instruction) <- "Directions"
  df_instruction[1, 1] <- "Directions for using this Excel workbook for entering sorting data follow.  If you have rating data to add then see the worksheet labels VALUE DATA DIRECTIONS.  See readme for more detailed directions."
  df_instruction[2, 1] <- ""
  df_instruction[3, 1] <- "Add sorting data"
  df_instruction[4, 1] <- "Determine if your sorting data is in a racked or stacked format. Consult readme for details on each format."
  df_instruction[5, 1] <- "RACKED - If data is entered manually is most likely in the racked forrmat."
  df_instruction[6, 1] <- "     Each row is a group of cards associated with a label and person/sorter."
  df_instruction[7, 1] <- "     An identifier is entered in row 1 column A.  A label for a group of cards is entered in row 1 column B."
  df_instruction[8, 1] <- "     Beginning in row 1 column C the number for each card in the group is entered,"
  df_instruction[9, 1] <- "     Enter one number per cell going from left to right and using as many cells are necessary."
  df_instruction[10, 1] <- "    Then move to row 2 and repeat for the next group of items."
  df_instruction[11, 1] <- ""
  df_instruction[12, 1] <- "STACKED - If data is downloaded from an online sorting program it is most likely in the stacked format."
  df_instruction[13, 1] <- "    The data are arranged vertically.  There may be many columns but only three colums of data are needed."
  df_instruction[14, 1] <- "    Copy and paste columns of data that correspond to (1)sorter identification, (2)item number, (3) group/cards label."
  df_instruction[15, 1] <- ""
  df_instruction[16, 1] <- "IDEA TEXT - On the ideas worksheet, enter the text for each item beginning in row 2 of column B."
  df_instruction[17, 1] <- "The text should also include the item number, for example; 1. text for idea"
  df_instruction[18, 1] <- ""
  df_instruction[19, 1] <- "VALUE/RATING DATA - If your project includes rating data then on the valuse worksheet do the following:"
  df_instruction[20, 1] <- "Add the variable name for each measure beginning in Column E in row 1. For example, if the measure is importance, variable might be imp1, imp2, imp2 and so on."
  df_instruction[21, 1] <- "Do not change other variable labels in column A-D."
  df_instruction[22, 1] <- "For each rating:"
  df_instruction[23, 1] <- "   - enter an identification value for the rater."
  df_instruction[24, 1] <- ""
  df_instruction[25, 1] <- "   - If there are demographic variables, enter these in columns B,C, & D."
  df_instruction[26, 1] <- "       Categorical demographic variables MUST contain text.  Do NOT use integers (e.g, 1,2,3) to indicate group membership."
  df_instruction[27, 1] <- "       Continuous variables must be numeric."
  df_instruction[28, 1] <- "       if there are no demographic values then leave columns B,C & D blank. Otherwise enter data for up to three demographic variables in columns indicated."
  df_instruction[29, 1] <-
  df_instruction[30, 1] <- "   - enter values for measurement variable and note that:"
  df_instruction[31, 1] <- "         The number of measurement variables needs to be equal to the number of items/ideas."
  df_instruction[32, 1] <- "         If there are two measures, the number of variables for each measure should be the same and equal to the number of items/ideas."
  df_instruction[33, 1] <- ""
  df_instruction[34, 1] <- "DATA DEFINITION OF RATING DATA - Leave this worksheet blank.  The app will prompt you for information to complete this worksheet."
  df_instruction[35, 1] <- ""
  df_instruction[36, 1] <- "If you present or publish your work, please use the following information to cite this analytical resource."
  df_instruction[37, 1] <- "McLinden, D. (year). IdeaNet - Open Source software in R for concept mapping. Retrieved from https://github.com/ideanetwork/ideanetR"
  df_instruction[is.na(df_instruction)] <- "" # set all cells without data to empty/blank

  # . . racked -----
  df_racked <- as.data.frame(matrix(nrow = 1, ncol = 4))
  names(df_racked) <- c("sorter", "group", "first_card", "next_card")
  df_racked[is.na(df_racked)] <- "" # set all cells without data to empty/blank'

  # . . stacked -----
  df_stacked <- (as.data.frame(matrix(nrow = 5, ncol = 4)))
  names(df_stacked) <- c("sorter", "item", "group", "directions")
  df_stacked[1, 4] <- "Begin data entry in row 2"
  df_stacked[2, 4] <- "In column A copy and past the identification number for each sort"
  df_stacked[3, 4] <- "In column B copy and past the item number for each sort.  Do not add item text."
  df_stacked[4, 4] <- "In column C copy and past the identification number for each sort"
  df_stacked[5, 4] <- "Do not change the column headings"
  df_stacked[is.na(df_stacked)] <- "" # set all cells without data to empty/blank

  # . . ideas -----
  df_ideas <- (as.data.frame(matrix(nrow = 4, ncol = 3)))
  names(df_ideas) <- c("item", "item_text", "directions")
  df_ideas[1, 3] <- "This workseet is used to record  the text for each idea which is to be used for graphs and tables"
  df_ideas[2, 3] <- "In column B, beginning in row 2, put the text for each item corresponding to the item number in column A."
  df_ideas[3, 3] <- "Ideally in the form of the item number, period and item text (e.g., #. text)."
  df_ideas[4, 3] <- "Leave column A blank"
  df_ideas[is.na(df_ideas)] <- "" # set all cells without data to empty/blank

  # . . rating data ------
  df_values <- as.data.frame((matrix(nrow = 1, ncol = 5)))
  df_values[1, 1] <- "raterID"
  df_values[1, 2] <- "demographic_var1 - Replace this text with name of first demographic variable. If none, leave blank."
  df_values[1, 3] <- "demographic_var2 - Replace this text with name of second demographic variable. If none, leave blank."
  df_values[1, 4] <- "demographic_var3 - Replace this text with name of third demographic variable. If none, leave blank."
  df_values[1, 5] <- "Start in this column (col E) in row 1 and name the measurement variables (e.g., imp1, imp2,...). Note, you will overwrite this message with your first variable name. If there are two measures (e.g., importance, feasibility) name the variables for the first measure using as many columns as necessary and then name the second measure."
  df_values[is.na(df_values)] <- "" # set all cells without data to empty/blank

  # . .  data definition for demographic data -----
  df_demo_def <- as.data.frame((matrix(nrow = 5, ncol = 5)))
  df_demo_def[1, 1] <- "Demographics data definition"
  df_demo_def[2, 1] <- "Variable"
  df_demo_def[2, 2] <- "Variable name"
  df_demo_def[2, 3] <- "Data type"
  df_demo_def[2, 4] <- "Minimum"
  df_demo_def[2, 5] <- "Maximum"
  df_demo_def[3, 1] <- "First demographic variable"
  df_demo_def[4, 1] <- "Second demographic variable"
  df_demo_def[5, 1] <- "Third demographic variable"

  # . .  data definition for rating data -----
  df_data_def <- as.data.frame((matrix(nrow = 15, ncol = 3)))
  df_data_def[1, 1] <- "Measures/rating data definition"
  df_data_def[2, 1] <- "Number of measures (1 or 2)"
  df_data_def[3, 1] <- "Name of first measure beginning in col E of values worksheet"
  df_data_def[4, 1] <- "Maximum value of the rating scale for measure one."
  df_data_def[5, 1] <- "Minimum value of the rating scale for measure one."
  df_data_def[6, 1] <- ""
  df_data_def[7, 1] <- "Name of second measure, if applicable"
  df_data_def[8, 1] <- "Maximum value of the rating scale for measure two (leave blank if none)."
  df_data_def[9, 1] <- "Minimum value of the rating scale for measure two (leave blank if none)."
  df_data_def[is.na(df_data_def)] <- "" # set all cells without data to empty/blank

  # . . styles -----
  style_wrap <- createStyle(fontSize = 10, fontName = "Arial", wrapText = TRUE)

  # . . add data to worksheets -----
  # write directions
  setColWidths(input_wb, "directions", 1, widths = 75)
  addStyle(input_wb, "directions", style_wrap, cols = 1, rows = 1:35)
  writeData(input_wb, "directions", df_instruction,
    startCol = 1,
    startRow = 1, colNames = TRUE, rowNames = FALSE
  )

  # . . write data to wks -----
  # racked
  setColWidths(input_wb, "racked", cols = c(1, 2), widths = 25)
  setColWidths(input_wb, "racked", cols = c(3, 4), widths = 15)
  writeData(input_wb, "racked", df_racked,
    startCol = 1, startRow = 1,
    colNames = TRUE, rowNames = FALSE
  )

  # stacked
  setColWidths(input_wb, "stacked", cols = 3, widths = 50)
  writeData(input_wb, "stacked", df_stacked,
    startCol = 1, startRow = 1,
    colNames = TRUE, rowNames = FALSE
  )

  #  ideas
  setColWidths(input_wb, "ideas", cols = 3, widths = 50)
  addStyle(input_wb, "ideas", style_wrap, cols = 3, rows = 1:4)
  writeData(input_wb, "ideas", df_ideas,
    startCol = 1, startRow = 1,
    colNames = TRUE, rowNames = FALSE
  )

  # values
  setColWidths(input_wb, "values", cols = 5, widths = 50)
  addStyle(input_wb, "values", style_wrap, cols = 1, rows = 1)
  writeData(input_wb, "values", df_values,
    startCol = 1, startRow = 1,
    colNames = FALSE, rowNames = FALSE
  )

  # demographics
  setColWidths(input_wb, "values_def", cols = 1, widths = 50)
  setColWidths(input_wb, "values_def", cols = 2:5, widths = 25)
  addStyle(input_wb, "values_def", style_wrap, cols = 1, rows = 1:5)
  writeData(input_wb, "values_def", df_demo_def,
    startCol = 1, startRow = 1,
    colNames = FALSE, rowNames = FALSE
  )

  # data definition for ratings
  setColWidths(input_wb, "values_def", cols = 1, widths = 50)
  setColWidths(input_wb, "values_def", cols = 2:3, widths = 25)
  addStyle(input_wb, "values_def", style_wrap, cols = 1, rows = 1:15)
  writeData(input_wb, "values_def", df_data_def,
    startCol = 1, startRow = 7,
    colNames = FALSE, rowNames = FALSE
  )

  # . . explain saving .xlsx file extension -----
  tcltk::tk_messageBox(type = "ok", message = "A file for project data has been created. A window will open so you can save this input file.  Give the file a name (e.g., input.xlsx). IMPORTANT: add the file extension for Excel (i.e. .xlsx). Click ok and then save your file.")

  saveWorkbook(input_wb, file = file.choose(), overwrite = TRUE) # user needs to add xlsx extension

  tcltk::tk_messageBox(type = "ok", message = "A excel file for your project data has been saved.")
} # end create_input_wb function

# ********************************************************************************************** -----
# test sort data for errors ------
sortdata_errorcheck <- function() {

  # clears frames from canvas when a new menu item is chosen at top level menu
  reset_canvas <- function() {
    if (exists("lpw.1")) tcltk::tkdestroy(lpw.1)
    if (exists("lpw.2")) tcltk::tkdestroy(lpw.2)
    if (exists("lpw.3")) tcltk::tkdestroy(lpw.3)
    if (exists("lpw.4")) tcltk::tkdestroy(lpw.4)
    if (exists("lpw.5")) tcltk::tkdestroy(lpw.5)
    if (exists("lpw.6")) tcltk::tkdestroy(lpw.6)
    if (exists("lpw.7")) tcltk::tkdestroy(lpw.7)
    if (exists("lpw.8")) tcltk::tkdestroy(lpw.8)
    if (exists("lpw.9")) tcltk::tkdestroy(lpw.9)
    if (exists("lpw.10")) tcltk::tkdestroy(lpw.10)
    if (exists("lpw.11")) tcltk::tkdestroy(lpw.11)
    if (exists("lpw.12")) tcltk::tkdestroy(lpw.12)
    if (exists("lpw.13")) tcltk::tkdestroy(lpw.13)
    if (exists("compute.layer.but")) tcltk::tkdestroy(compute.layer.but)
    if (exists("reset.but")) tcltk::tkdestroy(reset.but)
  } # end reset_canvas
  reset_canvas() # clear canvas

  # . . get data -----
  # open dialogue to choose file
  input_file <- tcltk::tclvalue(tcltk::tkgetOpenFile(filetypes = "{{Excel Spreadsheet} {.xlsx}}
                                       {{All files} *}"))

  # assign input_file to global so that other functions can access and save error data if errors
  assign("input_file", input_file, envir = .GlobalEnv)

  # read in the wb
  stacked <- read.xlsx(input_file, sheet = "stacked")
  racked <- read.xlsx(input_file, sheet = "racked")
  ideas <- read.xlsx(input_file, sheet = "ideas")

  # verify the wks has data - ideas
  ideas_obs <- ifelse(length(na.omit(ideas$item_text) > 0), "yes", "no")
  if (ideas_obs == "no") no_ideas()

  # verify the wks has data - sort data and if data is racked or stacked
  racked_obs <- length(na.omit(racked$sorter))
  stacked_obs <- length(na.omit(stacked$sorter))
  sort_structure <- if (racked_obs == 0 & stacked_obs > 0) {
    "stacked"
  } else
  if (racked_obs > 1 & stacked_obs == 0) {
    "racked"
  } else
  if (racked_obs > 1 & stacked_obs > 1) {
    "both"
  } else
  if (racked_obs == 0 & stacked_obs == 0) "none"

  # call error for no sort data or duplicate sort types and stop quietly
  if (sort_structure == "both") {
    both_sorts()
  } else
  if (sort_structure == "none") no_sorts()

  # rename df so that code the follows can use the sort data regardless of original format or name
  if (sort_structure == "racked") {
    # NOTE ON LABELS: if two labels from two different people use the same words (e.g., communication), these two instances are treated as "unique" values.
    racked$label_ID <- 1:nrow(racked) # add an index value as an identifier for each label so that each label is uniquely identified regardless of character value
    meltsort <- reshape2::melt(racked, id.vars = c("sorter", "group", "label_ID")) # organize data and melt by ID vars of person and label in cols 1 and 2
    meltsort$value <- as.numeric(as.character(meltsort$value)) # melt converts item num to char, this line convert char back to numeric
    meltsort[meltsort == ""] <- NA #  change all missing to NA in order to remove rows with NA or missing
    meltsort <- meltsort[complete.cases(meltsort), ] # remove rows with NA
    names(meltsort)[5] <- "item" # rename the column heading
    meltsort[, c("variable", "label_ID")] <- list(NULL) # delete cols not needed for error check
    meltsort <- with(meltsort, (meltsort[order(sorter, item), ])) # sort by person then by item, not required, make the output easier to view in excel if there are errors
    stacked <- meltsort
    assign("stacked", stacked, envir = .GlobalEnv)
  } else {
    stacked <- stacked
  }

  n_sorters <- stacked %>%
    group_by(sorter) %>%
    dplyr::summarise(count = n_distinct(sorter)) %>%
    ungroup()

  n_items <- length(ideas$item_text)

  # . . error checking sort data -----
  # correct number of cards for each sort
  cards_sorted <- function() {
    n_items <- length(ideas$item_text)
    n_cards <- paste("Number of cards sorted = ", n_items)

    # count the number of items sorted by each person
    stacked$sorter <- as.integer(as.factor(stacked$sorter))

    # . . replace plyr with dplyr -----
    person_sort <- plyr::ddply(stacked, "sorter", summarize, sortedn = length(item))

    # create a dataframe to hold comparison of actual sorted to expected sorted'
    person_sort$maxn <- n_items # add the maximum number of sorted cards as a column in dataframe
    # rename columns
    names(person_sort)[2] <- "respondent sorted"
    names(person_sort)[3] <- "items to be sorted"

    # compare actual sorted to expected sorted
    person_sort$status <- (person_sort[, 2] == person_sort[, 3])
    person_sort$status <- ifelse(person_sort$status == "FALSE", "error", "ok")

    return(person_sort)
  } # end number of cards sorted function

  # missing cards
  find_missing <- function() {
    n_items <- length(ideas$item_text)

    # create a sequence of integer values from 1 to N
    sequence <- as.data.frame(seq(1:n_items))
    colnames(sequence) <- "seq_item"
    sequence$seq_item <- as.integer(sequence$seq_item)

    # add an index for levels of factor, this will be a counter for loops error checking code
    stacked$person_factor <- as.integer(as.factor(stacked$sorter))
    start <- min(stacked$person_factor)
    last_row <- max(stacked$person_factor)
    last_col <- n_items + 1

    missing_cards <- as.data.frame(matrix(nrow = 0, ncol = last_col))
    colnames(missing_cards)[1] <- "sorter"

    # loop through cards for each sorter compare sorter to sort to find missing in each sort, if any
    for (z in seq(start, last_row, 1)) {
      sub_item <- (subset(stacked, stacked$person_factor == z)) # select a subset of item ID numbers based on a persons factor index number
      sub_item$item <- as.integer(sub_item$item) # coerce to integer to ensure all data comparisons are between the same types
      missing <- setdiff(sequence$seq_item, sub_item$item)
      if (identical(missing, integer(0)) == "TRUE") {
        missing <- 0
      } # If not cards are missing convert result to zero
      t_missing <- (t(missing)) # transpose  rows to columns
      missing_data <- as.data.frame(cbind(sub_item$sorter[1], t_missing)) # add sorter id to missing cards
      colnames(missing_data)[1] <- "sorter"
      add_missing <- list(missing_cards, missing_data) # combine dataframes in a list
      missing_cards <- plyr::rbind.fill(missing_cards, missing_data) # add unequal rows to dataframe
      rm(missing_data) # delete dataframe
    } # end for loop

    missing_cards[is.na(missing_cards)] <- "" # rem NA to blank/empty

    return(missing_cards)
  } # end missing cards function

  #  duplicate cards
  dup_cards <- function() {
    n_items <- length(ideas$item_text)

    # add an index for levels of factor, this will be a counter for loops error checking code
    stacked$person_factor <- as.integer(as.factor(stacked$sorter))
    start <- min(stacked$person_factor)
    last_row <- max(stacked$person_factor)
    last_col <- n_items + 1

    dup_cards <- as.data.frame(matrix(nrow = 0, ncol = last_col))
    colnames(dup_cards)[1] <- "sorter"

    # loop through cards for each sorter compare sorter to sort to find dup in each sort, if any
    for (z in seq(start, last_row, 1)) {
      sub_item <- (subset(stacked, stacked$person_factor == z)) # select a subset of item ID numbers based on a persons factor index number
      sub_item$item <- as.integer(sub_item$item) # coerce to integer to ensure all data comparisons are between the same types
      items <- as.data.frame(sub_item$item) # extract column of item numbers
      colnames(items)[1] <- "id"
      dups <- items[duplicated(items), ]
      if (identical(dups, integer(0)) == "TRUE") {
        dups <- 0
      } # If no cards are missing convert result to zero
      t_dups <- (t(dups)) # transpose  rows to columns
      dup_data <- as.data.frame(cbind(sub_item$sorter[1], t_dups)) # add sorter id to duplicate cards
      colnames(dup_data)[1] <- "sorter"
      dup_cards <- plyr::rbind.fill(dup_cards, dup_data) # add unequal rows to dataframe

      rm(dup_data) # delete dataframe
    } # end for loop

    dup_cards[is.na(dup_cards)] <- "" # rem NA to blank/empty

    return(dup_cards)
  } # end duplicate cards

  # cards out of sequence from 1 to N
  card_seq <- function() {

    # get ideas worksheet and count the number of ideas
    n_items <- length(ideas$item_text)

    # add an index for levels of factor, this will be a counter for loops error checking code
    stacked$person_factor <- as.integer(as.factor(stacked$sorter))
    start <- min(stacked$person_factor)
    stop <- max(stacked$person_factor)

    # create an empty dataframe to hold results of error check, dimension nrow=number of sorters
    card_seq <- as.data.frame(matrix(nrow = stop, ncol = 2))
    card_seq$V1 <- as.character(card_seq$V1) # define as a text field.
    card_seq$V2 <- as.character(card_seq$V2)
    names(card_seq) <- c("sorter", "status") # add colnames

    # loop through each sorter's data and compare sorter sequence of card numbers to standard sequence of 1 to N
    for (count in seq(start, stop, 1)) {
      sub_item <- (subset(stacked, stacked$person_factor == count))
      sub_item$item <- as.integer(sub_item$item) # coerce to integer to ensure all data comparisons are between the same types
      max_card <- max(sub_item$item)
      min_card <- min(sub_item$item)

      # checks for zero and card numbers larger than number of cards
      ifelse(min_card == 1, low_card <- "ok", "error")
      ifelse(max_card == n_items, high_card <- "ok", "error")
      ifelse(low_card == "ok" & high_card == "ok", seq_status <- "ok", seq_status <- "error")
      card_seq[count, 1] <- sub_item[1, 1]
      card_seq[count, 2] <- seq_status
    } # end for loop

    return(card_seq)
  } # end card sequence

  # save error checking results
  sort_err <- function() {
    input_wd() # set the working dir to be the same location as the input file.

    # . . create a workbook to hold error checking data
    error_check <- createWorkbook()
    addWorksheet(error_check, "error_summary")
    addWorksheet(error_check, "cards_sorted")
    addWorksheet(error_check, "missing_cards")
    addWorksheet(error_check, "duplicate_cards")
    addWorksheet(error_check, "card_sequence")

    writeData(error_check, "error_summary", error_df,
      startCol = 1, startRow = 1,
      colNames = TRUE, rowNames = FALSE
    ) # write cards sorted to workbook
    writeData(error_check, "cards_sorted", ret_person_sort,
      startCol = 1, startRow = 1,
      colNames = TRUE, rowNames = FALSE
    ) # write cards sorted to workbook
    writeData(error_check, "missing_cards", ret_missing_cards,
      startCol = 1, startRow = 1,
      colNames = TRUE, rowNames = FALSE
    ) # write cards sorted to workbook
    writeData(error_check, "duplicate_cards", ret_dup_cards,
      startCol = 1, startRow = 1,
      colNames = TRUE, rowNames = FALSE
    ) # write cards sorted to workbook
    writeData(error_check, "card_sequence", ret_card_seq,
      startCol = 1, startRow = 1,
      colNames = TRUE, rowNames = FALSE
    ) # write cards sorted to workbook
    saveWorkbook(error_check, "error_check.xlsx", overwrite = TRUE)

    msg1 <- "There are sort errors in the data."
    msg2 <- ""
    msg3 <- "An excel  file, error_check.xlsx, has been saved in the same directory as the sort data. Open the Excel file to review errors."
    msg4 <- ""
    msg5 <- "While it is advisable to correct errors, if the number of errors is small, the mapping algorithm will still run and provide meaningful results."
    msg6 <- ""
    msg7 <- "Before computing maps, if you have rating data then, from the menu choose review data and then select review rating data."

    error_message <- paste(msg1, msg2, msg3, msg4, msg5, msg6, msg7, sep = "\n")
    tcltk::tk_messageBox(type = "ok", message = error_message)
  } # end sort_err function

  no_sort_err <- function() {
    msg1 <- paste("There are", n_sorters, "sorts of", n_items, "items. There are no errors in sort data.", sep = " ")
    msg2 <- ""
    msg3 <- "If you have rating data then, from the menu choose review data and then select review rating data."
    msg4 <- ""
    msg5 <- "If there is no rating data, then proceed to compute maps."

    no_sort_err <- print(paste(msg1, msg2, msg3, msg4, msg5, sep = "\n"))
    tcltk::tk_messageBox(type = "ok", message = no_sort_err)
  }

  # call the functions to check the sort data
  ret_person_sort <- cards_sorted()
  ret_missing_cards <- find_missing()
  ret_dup_cards <- dup_cards()
  ret_card_seq <- card_seq()

  # create a summary of errors in a df
  c_sorted <- ifelse(any(ret_person_sort$status == "error"), "error", "ok")
  m_cards <- ifelse(any(ret_missing_cards$V2 != 0), "error", "ok")
  d_cards <- ifelse(any(ret_dup_cards$V2 != 0), "error", "ok")
  seq_cards <- ifelse(any(ret_card_seq$status == "error"), "error", "ok")
  error_df <- as.data.frame(rbind(c_sorted, m_cards, d_cards, seq_cards))
  error_df$err_type <- row.names(error_df)
  error_df[1, 2] <- "number of cards sorted"
  error_df[2, 2] <- "missing cards"
  error_df[3, 2] <- "duplicate cards"
  error_df[4, 2] <- "cards out of sequence"
  colnames(error_df)[1] <- "status"

  # if any errors else if no errors
  ifelse(any(error_df$status == "error"), sort_err(), no_sort_err())
} # end sort data_error check function

# ********************************************************************************************** -----
# rating data error check & define demographics & scale -----
ratedata_errorcheck <- function() {

  # clears frames from canvas when a new menu item is chosen at top level menu
  reset_canvas <- function() {
    if (exists("lpw.1")) tcltk::tkdestroy(lpw.1)
    if (exists("lpw.2")) tcltk::tkdestroy(lpw.2)
    if (exists("lpw.3")) tcltk::tkdestroy(lpw.3)
    if (exists("lpw.4")) tcltk::tkdestroy(lpw.4)
    if (exists("lpw.5")) tcltk::tkdestroy(lpw.5)
    if (exists("lpw.6")) tcltk::tkdestroy(lpw.6)
    if (exists("lpw.7")) tcltk::tkdestroy(lpw.7)
    if (exists("lpw.8")) tcltk::tkdestroy(lpw.8)
    if (exists("lpw.9")) tcltk::tkdestroy(lpw.9)
    if (exists("lpw.10")) tcltk::tkdestroy(lpw.10)
    if (exists("lpw.11")) tcltk::tkdestroy(lpw.11)
    if (exists("lpw.12")) tcltk::tkdestroy(lpw.12)
    if (exists("lpw.13")) tcltk::tkdestroy(lpw.13)
    if (exists("compute.layer.but")) tcltk::tkdestroy(compute.layer.but)
    if (exists("reset.but")) tcltk::tkdestroy(reset.but)
  } # end reset_canvas

  reset_canvas()

  # save demographic & rating info to excel
  demo_ok <- function() {
    input_wd() # set wd same as source data
    wb <- loadWorkbook(input_file_base)

    writeData(wb,
              sheet = "values_def", demo_data, startCol = 2, startRow = 3,
              colNames = FALSE, rowNames = FALSE, keepNA = FALSE
    )
    saveWorkbook(wb, input_file_base, overwrite = T)
    tcltk::tkmessageBox(title = "Demographic data", message = "Demographic information saved to input data file.")
    tcltk::tkdestroy(demo_ok.but)
  } # end demo_ok function

  # if one measure
  rate1_ok <- function() {

   input_wd() # set the working dir to be the same location as the input file.
    wb <- loadWorkbook(input_file_base)
    rate_data[8:15, 2] <- "" # If changed from two measure to one measure, clear the old measure2 data before writing new data
    writeData(wb,
              sheet = "values_def", rate_data, startCol = 2, startRow = 8,
              colNames = FALSE, rowNames = FALSE, keepNA = FALSE
    )
    saveWorkbook(wb, input_file_base, overwrite = T)
    tcltk::tkmessageBox(
      title = "Rating data",
      message = "Rating data checked and are OK. Information saved to input data file."
    )

    tcltk::tkdestroy(lpw.1)
    tcltk::tkdestroy(lpw.2)
    tcltk::tkdestroy(reset.but)

  } # end rate1_ok function

  # if two measures
  rate2_ok <- function() {

    input_wd() # set the working dir to be the same location as the input file.
    wb <- loadWorkbook(input_file_base)
    writeData(wb,
              sheet = "values_def", rate_data1, startCol = 2, startRow = 8,
              colNames = FALSE, rowNames = FALSE, keepNA = FALSE
    )
    writeData(wb,
              sheet = "values_def", rate_data2, startCol = 2, startRow = 13,
              colNames = FALSE, rowNames = FALSE, keepNA = FALSE
    )
    saveWorkbook(wb, input_file_base, overwrite = T)
    tcltk::tkmessageBox(title = "Rating data", message = "Rating data checked and are OK. Information saved to input data file.")

    tcltk::tkdestroy(lpw.1)
    tcltk::tkdestroy(lpw.2)
    tcltk::tkdestroy(reset.but)
  } # end rate2_ok function

  # define measures
  def_measures <- function() {

    one_measure <- function() {

      if (exists("OK.but")) tcltk::tkdestroy(OK.but)

      xvar <- tcltk::tclVar("")
      yvar <- tcltk::tclVar("")
      zvar <- tcltk::tclVar("")
      x.entry <- tcltk::tkentry(lpw.2, textvariable = xvar)
      y.entry <- tcltk::tkentry(lpw.2, textvariable = yvar)
      z.entry <- tcltk::tkentry(lpw.2, textvariable = zvar)

      submit <- function() {
        xvar <- as.character(tcltk::tclvalue(xvar))
        yvar <- as.numeric(tcltk::tclvalue(yvar))
        zvar <- as.numeric(tcltk::tclvalue(zvar))

        ratings <- (values[, 5:rating_col]) # extract rating values
        max_rate <- max(ratings, na.rm = T)
        min_rate <- min(ratings, na.rm = T)

        if (is.character(xvar) == "FALSE" || is.na(xvar) == "TRUE") invalid_measure_name()
        if (is.numeric(yvar) == "FALSE" || is.na(yvar) == "TRUE") invalid_scale_value()
        if (is.numeric(zvar) == "FALSE" || is.na(zvar) == "TRUE") invalid_scale_value()
        if ((yvar >= zvar) == "TRUE") invalid_scale_anchor()
        if (yvar > min_rate) rating_scale_mismatch()
        if (zvar < max_rate) rating_scale_mismatch()

        rate_data <- as.data.frame(rbind(measure_choice, xvar, yvar, zvar))

        assign("rate_data", rate_data, envir = .GlobalEnv)
        assign("xvar", xvar, envir = .GlobalEnv)
        assign("yvar", yvar, envir = .GlobalEnv)
        assign("zvar", zvar, envir = .GlobalEnv)

        rate1_ok()
      } # end submit function within one measure function

      define_rate.but <- tcltk::tkbutton(lpw.2, text = "Submit rating definition", command = submit)

      tcltk::tkgrid(tcltk::tklabel(lpw.2, text = "Measure name"), x.entry, pady = 5, padx = 5)
      tcltk::tkgrid(tcltk::tklabel(lpw.2, text = "Scale minimum value"), y.entry, pady = 5, padx = 5)
      tcltk::tkgrid(tcltk::tklabel(lpw.2, text = "Scale maximum value"), z.entry, pady = 5, padx = 5)
      tcltk::tkgrid(define_rate.but)
    } # end one_measure function

    two_measure <- function() {
      if (rating_col %% 2 != 0) unequal_rating_var(rating_col) # test if 2 measures, n of variables is equal

      if (exists("OK.but")) tcltk::tkdestroy(OK.but)

      start_col1 <- 5
      end_col1 <- start_col1 + ((rating_col / 2) - 1)
      start_col2 <- end_col1 + 1
      end_col2 <- start_col2 + ((rating_col / 2) - 1)

      x1var <- tcltk::tclVar("")
      y1var <- tcltk::tclVar("")
      z1var <- tcltk::tclVar("")
      x2var <- tcltk::tclVar("")
      y2var <- tcltk::tclVar("")
      z2var <- tcltk::tclVar("")
      x1.entry <- tcltk::tkentry(lpw.2, textvariable = x1var)
      y1.entry <- tcltk::tkentry(lpw.2, textvariable = y1var)
      z1.entry <- tcltk::tkentry(lpw.2, textvariable = z1var)
      x2.entry <- tcltk::tkentry(lpw.2, textvariable = x2var)
      y2.entry <- tcltk::tkentry(lpw.2, textvariable = y2var)
      z2.entry <- tcltk::tkentry(lpw.2, textvariable = z2var)

      submit <- function() {
        x1var <- as.character(tcltk::tclvalue(x1var))
        y1var <- as.numeric(tcltk::tclvalue(y1var))
        z1var <- as.numeric(tcltk::tclvalue(z1var))
        x2var <- as.character(tcltk::tclvalue(x2var))
        y2var <- as.numeric(tcltk::tclvalue(y2var))
        z2var <- as.numeric(tcltk::tclvalue(z2var))

        ratings1 <- (values[, 5:end_col1]) # extract rating values
        ratings2 <- (values[, start_col2:end_col2]) # extract rating values

        max1_rate <- max(ratings1, na.rm = T)
        min1_rate <- min(ratings1, na.rm = T)
        max2_rate <- max(ratings2, na.rm = T)
        min2_rate <- min(ratings2, na.rm = T)

        if (is.character(x1var) == "FALSE" || is.na(x1var) == "TRUE") invalid_measure_name()
        if (is.numeric(y1var) == "FALSE" || is.na(y1var) == "TRUE") invalid_scale_value()
        if (is.numeric(z1var) == "FALSE" || is.na(z1var) == "TRUE") invalid_scale_value()
        if ((y1var >= z1var) == "TRUE") invalid_scale_anchor()
        if (y1var > min1_rate) rating_scale_mismatch()
        if (z1var < max1_rate) rating_scale_mismatch()

        if (is.character(x2var) == "FALSE" || is.na(x2var) == "TRUE") invalid_measure_name()
        if (is.numeric(y2var) == "FALSE" || is.na(y2var) == "TRUE") invalid_scale_value()
        if (is.numeric(z2var) == "FALSE" || is.na(z2var) == "TRUE") invalid_scale_value()
        if ((y2var >= z2var) == "TRUE") invalid_scale_anchor()
        if (y2var > min2_rate) rating_scale_mismatch()
        if (z2var < max2_rate) rating_scale_mismatch()

        rate_data1 <- as.data.frame(rbind(measure_choice, x1var, y1var, z1var))
        rate_data2 <- as.data.frame(rbind(x2var, y2var, z2var))

        assign("rate_data1", rate_data1, envir = .GlobalEnv)
        assign("rate_data2", rate_data2, envir = .GlobalEnv)
        assign("x1var", x1var, envir = .GlobalEnv)
        assign("y1var", y1var, envir = .GlobalEnv)
        assign("z1var", z1var, envir = .GlobalEnv)
        assign("x2var", x2var, envir = .GlobalEnv)
        assign("y2var", y2var, envir = .GlobalEnv)
        assign("z2var", z2var, envir = .GlobalEnv)

        rate2_ok()
      } # end submit function within two measure function

      define_rate2.but <- tcltk::tkbutton(lpw.2, text = "Submit define measures", command = submit)

      tcltk::tkgrid(tcltk2::tk2message(lpw.2, text = "", bg = "aliceblue", width = 400, justify = "left"))
      tcltk::tkgrid(tcltk2::tk2message(lpw.2, text = "Define measure 1.", bg = "aliceblue", width = 400, justify = "left"))
      tcltk::tkgrid(tcltk::tklabel(lpw.2, text = "Measure one name"), x1.entry, pady = 5, padx = 5)
      tcltk::tkgrid(tcltk::tklabel(lpw.2, text = "Scale one minimum value"), y1.entry, pady = 5, padx = 5)
      tcltk::tkgrid(tcltk::tklabel(lpw.2, text = "Scale one maximum value"), z1.entry, pady = 5, padx = 5)
      tcltk::tkgrid(tcltk2::tk2message(lpw.2, text = "", bg = "aliceblue", width = 400, justify = "left"))
      tcltk::tkgrid(tcltk2::tk2message(lpw.2, text = "Define measure 2.", bg = "aliceblue", width = 400, justify = "left"))
      tcltk::tkgrid(tcltk::tklabel(lpw.2, text = "Measure two name"), x2.entry, pady = 5, padx = 5)
      tcltk::tkgrid(tcltk::tklabel(lpw.2, text = "Scale two minimum value"), y2.entry, pady = 5, padx = 5)
      tcltk::tkgrid(tcltk::tklabel(lpw.2, text = "Scale two maximum value"), z2.entry, pady = 5, padx = 5)
      tcltk::tkgrid(define_rate2.but)
    } # end two_measure function

    measure_choice <- tcltk::tclvalue(num_measures)
    if (measure_choice != "Choose number of measures") {
      assign("measure_choice", measure_choice, envir = .GlobalEnv)
    }
    if (measure_choice == 1) one_measure()
    if (measure_choice == 2) two_measure()
    if (measure_choice == "Choose number of measures")tcltk::tkmessageBox(title = "Well...", message = "Select a number of measures")
  } # end define_measures function

  # define each demographic variable, if any. x is demographic column of rating data
  demographics <- function(x) {
    if (all(is.na(values[, x])) == "TRUE") {
      demo_var <- "NA"
      demo_name <- "NA"
      demo_levels <- "NA"
      demo_type <- "NA"
      demo_min <- "NA"
      demo_max <- "NA"
      demo_msg <- "empty, there is no demographic data"
    } else {
      if (is.character(values[, x]) == "TRUE") {
        demo_name <- colnames(values[x])
        demo_var <- (values[, x])
        demo_var <- as.data.frame(demo_var)
        demo_var$demo_var <- as.factor(demo_var$demo_var)
        demo_levels <- nlevels(demo_var$demo_var)
        demo_type <- "categorical"
        demo_min <- "NA"
        demo_max <- "NA"
        demo_msg1 <- "The variable is categorical"
        demo_msg2 <- paste(demo_levels, " levels", sep = "")
        demo_msg <- paste(demo_name, demo_name, demo_msg1, demo_msg2, sep = "\n")
      } else {
        if (is.numeric(values[, x]) == "TRUE") {
          demo_name <- colnames(values[x])
          demo_var <- values[, x]
          demo_var <- as.data.frame(demo_var)
          demo_type <- "continuous"
          demo_levels <- nrow(demo_var)
          demo_min <- min(demo_var)
          demo_max <- max(demo_var)
          demo_msg <- paste(demo_name,
                            ". The variable is continuous. The minimum value is ",
                            demo_min, " and the maximum values is ", demo_max, ".",
                            sep = ""
          )
        } # end if
      } # end else
    } # end if else

    # using if(exists) avoids warning messages if canvas is reset before these vars are defined
    if (exists("demo_levels")) assign("demo_levels", demo_levels, envir = .GlobalEnv)
    if (exists("demo_type")) assign("demo_type", demo_type, envir = .GlobalEnv)
    if (exists("demo_msg")) assign("demo_msg", demo_msg, envir = .GlobalEnv)
    if (exists("demo_name")) assign("demo_name", demo_name, envir = .GlobalEnv)
    if (exists("demo_min")) assign("demo_min", demo_min, envir = .GlobalEnv)
    if (exists("demo_max")) assign("demo_max", demo_max, envir = .GlobalEnv)

  } # end demographics function

  # . . get data -----
  # open dialogue to choose file with rating data
  input_file <- tcltk::tclvalue(tcltk::tkgetOpenFile(filetypes = "{{Excel Spreadsheet} {.xlsx}} {{All files} *}"))
  assign("input_file", input_file, envir = .GlobalEnv) # assign so available to other functions
  values <- read.xlsx(input_file, sheet = "values", skipEmptyCols = FALSE) # get rating data
  values_def <- read.xlsx(input_file, sheet = "values_def", skipEmptyCols = FALSE) # get value definitions
  rating_col <- ncol(values) - 4 # subtract ID and demographic cols 1-4
  n_cols <- ncol(values[1, ])
  if (n_cols <= 5) missing_rating_data() # stop if no data

  # . . call demographics function -----
  x <- 2 # demographic column of rating data
  demographics(x)
  demo1_levels <- demo_levels
  demo1_type <- demo_type
  demo1_name <- demo_name
  demo1_min <- demo_min
  demo1_max <- demo_max
  demo1_msg <- paste("The first demographic variable in column 2 is ", demo_msg, sep = "")
  rm(demo_type)
  rm(demo_levels)
  rm(demo_msg)
  rm(demo_name)
  rm(demo_min)
  rm(demo_max)

  x <- 3
  demographics(x)
  demo2_levels <- demo_levels
  demo2_type <- demo_type
  demo2_name <- demo_name
  demo2_min <- demo_min
  demo2_max <- demo_max
  demo2_msg <- paste("The second demographic variable in column 3 is ", demo_msg, sep = "")
  rm(demo_type)
  rm(demo_levels)
  rm(demo_msg)
  rm(demo_name)
  rm(demo_min)
  rm(demo_max)

  x <- 4
  demographics(x)
  demo3_levels <- demo_levels
  demo3_type <- demo_type
  demo3_name <- demo_name
  demo3_min <- demo_min
  demo3_max <- demo_max
  demo3_msg <- paste("The third demographic variable in column 4 is ", demo_msg, sep = "")
  rm(demo_type)
  rm(demo_levels)
  rm(demo_msg)
  rm(demo_name)

  if (is.null(demo1_name) == "FALSE") {
    demo1_data <- as.data.frame(cbind(demo1_name, demo1_type, demo1_min, demo1_max))
    colnames(demo1_data) <- c("name", "type", "min", "max")
  } else {
    if (is.null(demo1_name) == "TRUE") {
      demo1_name <- "none"
      demo1_type <- "none"
      demo1_min <- "none"
      demo1_min <- "none"
      demo1_data <- as.data.frame(cbind(demo1_name, demo1_type, demo1_min, demo1_max))
      colnames(demo1_data) <- c("name", "type", "min", "max")
    }
  }
  if (is.null(demo2_name) == "FALSE") {
    demo2_data <- as.data.frame(cbind(demo2_name, demo2_type, demo2_min, demo2_max))
    colnames(demo2_data) <- c("name", "type", "min", "max")
  } else {
    if (is.null(demo2_name) == "TRUE") {
      demo2_name <- "none"
      demo2_type <- "none"
      demo2_min <- "none"
      demo2_min <- "none"
      demo2_data <- as.data.frame(cbind(demo2_name, demo2_type, demo2_min, demo2_max))
      colnames(demo2_data) <- c("name", "type", "min", "max")
    }
  }
  if (is.null(demo3_name) == "FALSE") {
    demo3_data <- as.data.frame(cbind(demo3_name, demo3_type, demo3_min, demo3_max))
    colnames(demo3_data) <- c("name", "type", "min", "max")
  } else {
    if (is.null(demo3_name) == "TRUE") {
      demo3_name <- "none"
      demo3_type <- "none"
      demo3_min <- "none"
      demo3_min <- "none"
      demo3_data <- as.data.frame(cbind(demo3_name, demo3_type, demo3_min, demo3_max))
      colnames(demo3_data) <- c("name", "type", "min", "max")
    }
  }

  demo_data <- as.data.frame(rbind(demo1_data, demo2_data, demo3_data))

  # . . create canvas demographics and values data -----
  lpw.1 <-tcltk::tkframe(tt, bg = "aliceblue", width = 400, height = 500)
  lpw.2 <- tcltk::tkframe(tt, bg = "aliceblue", width = 400, height = 500)
  reset.but <- tcltk::tkbutton(tt, text = "Reset", command = reset_canvas)

  # assign to global environ in order for reset_canvas() to clear canvas
  assign("lpw.1", lpw.1, envir = .GlobalEnv)
  assign("lpw.2", lpw.2, envir = .GlobalEnv)
  assign("reset.but", reset.but, envir = .GlobalEnv)

  tcltk::tkpack(reset.but, side = "bottom")
  tcltk::tkpack(lpw.1, lpw.2, side = "left", padx = 15, expand = TRUE, fill = "both")

  # message to user to check demographics
  demo4_msg <- "If the description of the demographic data is correct then click SUBMIT and move on to defining the rating data."
  demo5_msg <- ""
  demo6_msg <- "If the description of demographic data is NOT correct, then click RESET, check the source data in the Excel file and then rerun the data check."
  demo7_msg <- paste(demo4_msg, demo5_msg, demo6_msg, sep = "\n")

  demo_ok.but <- tcltk::tkbutton(lpw.1, text = "Submit demographics", command = demo_ok)

  tcltk::tkgrid(tcltk2::tk2message(lpw.1, text = " ", bg = "aliceblue", width = 400, justify = "left"))
  tcltk::tkgrid(tcltk2::tk2message(lpw.1, text = "NOTE: The input Excel data file needs to be closed. When you click submit, results of this data review will be written to the Excel file.", bg = "aliceblue", width = 400, justify = "left"))
  tcltk::tkgrid(tcltk2::tk2message(lpw.1, text = "----------", bg = "aliceblue", width = 400, justify = "left"))
  tcltk::tkgrid(tcltk2::tk2message(lpw.1,
    bg = "aliceblue", width = 400, justify = "center",
    text = "Demographics data definition"
  ))
  tcltk::tkgrid(tcltk2::tk2message(lpw.1, text = " ", bg = "aliceblue", width = 400, justify = "left"))
  tcltk::tkgrid(tcltk2::tk2message(lpw.1, text = demo1_msg, bg = "aliceblue", width = 400, justify = "left"))
  tcltk::tkgrid(tcltk2::tk2message(lpw.1, text = " ", bg = "aliceblue", width = 400, justify = "left"))
  tcltk::tkgrid(tcltk2::tk2message(lpw.1, text = demo2_msg, bg = "aliceblue", width = 400, justify = "left"))
  tcltk::tkgrid(tcltk2::tk2message(lpw.1, text = " ", bg = "aliceblue", width = 400, justify = "left"))
  tcltk::tkgrid(tcltk2::tk2message(lpw.1, text = demo3_msg, bg = "aliceblue", width = 400, justify = "left"))
  tcltk::tkgrid(tcltk2::tk2message(lpw.1, text = " ", bg = "aliceblue", width = 400, justify = "left"))
  tcltk::tkgrid(tcltk2::tk2message(lpw.1, text = " ", bg = "aliceblue", width = 400, justify = "left"))
  tcltk::tkgrid(tcltk2::tk2message(lpw.1, text = demo7_msg, bg = "aliceblue", width = 400, justify = "left"))
  tcltk::tkgrid(tcltk2::tk2message(lpw.1, text = " ", bg = "aliceblue", width = 400, justify = "left"))
  tcltk::tkgrid(demo_ok.but)
  # end demographics

  # check and define value measures
  rating_msg1 <- paste("\nThere are ", rating_col, " measurement variables.\nDoes this represent one measure or two measures?")

  tcltk::tkgrid(tcltk2::tk2message(lpw.2, text = " ", bg = "aliceblue", width = 400, justify = "left"))
  tcltk::tkgrid(tcltk2::tk2message(lpw.2, text = "NOTE: The input Excel data file needs to be closed. When you click submit, results of this data review will be written to the Excel file.", bg = "aliceblue", width = 400, justify = "left"))
  tcltk::tkgrid(tcltk2::tk2message(lpw.2, text = "----------", bg = "aliceblue", width = 400, justify = "left"))
  tcltk::tkgrid(tcltk2::tk2message(lpw.2, text = "Rating Data", bg = "aliceblue", width = 400, justify = "left"))
  tcltk::tkgrid(tcltk2::tk2message(lpw.2, text = rating_msg1, bg = "aliceblue", width = 400, justify = "left"))


  measures <- c("Choose number of measures", "1", "2")
  num_measures <- tcltk::tclVar("Choose number of measures")
  combo.1 <- tcltk::ttkcombobox(lpw.2,width = 30, values = measures, textvariable = num_measures, state = "readonly")
  OK.but <- tcltk::tkbutton(lpw.2, text = "Submit measures", command = def_measures)
  tcltk::tkgrid(tcltk2::tk2message(lpw.2,
    bg = "aliceblue", width = 400, justify = "left",
    text = "Choose the number of measures from dropdown list then click OK to open fields to further define measurement details.\n"
  ))
  tcltk::tkgrid(combo.1)
  tcltk::tkgrid(OK.but)

} # end ratedata function to check demographics and define rating var

# end ratedata_errorcheck and define demographic and rating data
# ********************************************************************************************** -----

# compute maps and output pptx & xlsx --------
getdata_computemaps <- function() {

  # clears frames from canvas when a new menu item is chosen at top level menu
  reset_canvas <- function() {
    if (exists("lpw.1")) tcltk::tkdestroy(lpw.1)
    if (exists("lpw.2")) tcltk::tkdestroy(lpw.2)
    if (exists("lpw.3")) tcltk::tkdestroy(lpw.3)
    if (exists("lpw.4")) tcltk::tkdestroy(lpw.4)
    if (exists("lpw.5")) tcltk::tkdestroy(lpw.5)
    if (exists("lpw.6")) tcltk::tkdestroy(lpw.6)
    if (exists("lpw.7")) tcltk::tkdestroy(lpw.7)
    if (exists("lpw.8")) tcltk::tkdestroy(lpw.8)
    if (exists("lpw.9")) tcltk::tkdestroy(lpw.9)
    if (exists("lpw.10")) tcltk::tkdestroy(lpw.10)
    if (exists("lpw.11")) tcltk::tkdestroy(lpw.11)
    if (exists("lpw.12")) tcltk::tkdestroy(lpw.12)
    if (exists("lpw.13")) tcltk::tkdestroy(lpw.13)
    if (exists("compute.layer.but")) tcltk::tkdestroy(compute.layer.but)
    if (exists("reset.but")) tcltk::tkdestroy(reset.but)
  } # end reset_canvas

  reset_canvas() # clear canvas


  # . . get data -----
  # open dialogue to choose file
  input_file <- tcltk::tclvalue(tcltk::tkgetOpenFile(filetypes = "{{Excel Spreadsheet} {.xlsx}}
                                       {{All files} *}"))
  assign("input_file", input_file, envir = .GlobalEnv)

  # read in the wb
  stacked <- read.xlsx(input_file, sheet = "stacked")
  racked <- read.xlsx(input_file, sheet = "racked")
  ideas <- read.xlsx(input_file, sheet = "ideas")

  # verify that wks have data - ideas
  idea_count <- length(na.omit(ideas$item_text))
  ideas_obs <- ifelse((idea_count > 0), "yes", "no")
  if (ideas_obs == "no") no_ideas()
  if (ideas_obs == "yes") ideas$item <- 1:idea_count # add integer as ID to each item

  # determine if sort has data and  is racked or stacked
  racked_obs <- length(na.omit(racked$sorter))
  stacked_obs <- length(na.omit(stacked$sorter))
  sort_structure <- if (racked_obs == 0 & stacked_obs > 0) {
    "stacked"
  } else if (racked_obs > 1 & stacked_obs == 0) {
    "racked"
  } else if (racked_obs > 1 & stacked_obs > 1) {
    "both"
  } else if (racked_obs == 0 & stacked_obs == 0) {
    "none"
  }

  # call error for sort data or duplicate sort types and stop quietly
  if (sort_structure == "both") {
    both_sorts()
  } else
  if (sort_structure == "none") no_sorts()

  # if racked call function to restructure sort
  # rename df so that code the follows can use the sort data regardless of original format or name
  if (sort_structure == "racked") {
    # NOTE ON LABELS: if two labels from two different people use the same words (e.g., communication), these two instances are treated as "unique" values.
    racked$label_ID <- 1:nrow(racked) # add an index value as an identifier for each label so that each label is uniquely identified regardless of character value
    meltsort <- reshape2::melt(racked, id.vars = c("sorter", "group", "label_ID")) # organize data and melt by ID vars of person and label in cols 1 and 2
    meltsort$value <- as.numeric(as.character(meltsort$value)) # melt converts item num to char, this line convert char back to numeric
    meltsort[meltsort == ""] <- NA #  change all missing to NA in order to remove rows with NA or missing
    meltsort <- meltsort[complete.cases(meltsort), ] # remove rows with NA
    names(meltsort)[5] <- "item" # rename the column heading
    meltsort[, c("variable", "label_ID")] <- list(NULL) # delete cols not needed for error check
    meltsort <- with(meltsort, (meltsort[order(sorter, item), ])) # sort by person then by item, not required, make the output easier to view in excel if there are errors
    stacked <- meltsort
    assign("stacked", stacked, envir = .GlobalEnv)
    } else
    if (sort_structure == "stacked") {
    stacked <- stacked
  } # end if else

  # add label id
  # NOTE ON LABELS: if two labels from two different people use the same words (e.g., communication), these two or more instances are treated as "unique" values.
  stacked$row.names <- NULL
  stacked$unique <- paste(stacked$sorter, stacked$group) # create a unique occurence for each group label if two or more people use the same words
  stacked$label_ID <- as.numeric(as.factor(stacked$unique)) # create a variable to enumerate each unique occurrence
  stacked$unique <- NULL # remove the pasted column, no longer needed

  # . . compute maps -----
  # create summary matrix
  i <- as.numeric(factor(stacked$item)) # coerce factor to numeric and specify as row index
  j <- as.numeric(factor(stacked$label_ID)) # coerce factor to numeric and specify as col index
  dcast_mat <- dcast(stacked, i ~ j)


  dcast_mat <- as.matrix(dcast_mat[, -1]) # remove column of names
  dcast_mat[is.na(dcast_mat)] <- 0 # set empty cells to zero
  dcast_mat <- apply(dcast_mat, 2, function(x) as.numeric(x > 0)) # set values from label to integer value of factor which is one
  sum_matrix <- dcast_mat %*% t(dcast_mat) # multiply rectangular matrix by the transpose of same to get square matrix
  class(sum_matrix) <- "integer" # change characters to integers
  sum_matrix <- as.data.frame(sum_matrix)

  # MDS
  matrix_dist <- dist(sum_matrix, method = "euclidean") # prepare the data for MDS calc the distance matrix using Euclidean distance
  mds_results <- smacofSym(matrix_dist, ndim = 2, type = "ordinal") # mds on the distances calculated from the summary matrix
  mconf <- as.data.frame(mds_results$conf) # coerce values in object to a dataframe to be used later to create output of this data to excel
  mstress <- as.data.frame(mds_results$stress) # coerce values in object to a dataframe to be used later to create output of this data to excel
  colnames(mstress) <- "Stress value"
  names(mconf) <- c("dim1", "dim2") # column names for x,y coordinates

  # . . cluster analysis -----
  # hierarchical cluster analysis using ward's method on the dissimilarity matrix
  hiclust <- cluster::agnes(mds_results$confdist, diss = TRUE,
    metric = "euclidean", method = "ward"  ) # cluster analysis from cluster package
  hc_range <- 15:5 # cluster solutions to retain for interpretation
  hc_cut <- cutree(hiclust, k = hc_range) # applying cutting so that results include 5 to 15 clusters
  colnames(hc_cut) <- paste("CLU", hc_range, sep = "") # define the column names for clusters

  # combine results into a single df for a single wks
  output_result <- cbind(mconf, hc_cut, ideas[, 1:2]) # create dataframe that combines of x,y coordinates; clusters 5-15, item# and item text

  assign("output_result", output_result, envir = .GlobalEnv)

  # . . label analysis -----
  xyitems <- output_result %>%
    select(dim1, dim2, item, item_text)

  label_items <- stacked %>%
    select(group, item, label_ID)

  xylabels <- merge(label_items, xyitems, by = c("item")) # merge many to one by item

  # compute label means
  mean_labels <- xylabels %>%
                        dplyr::group_by(label_ID, group) %>%
                        dplyr::summarize (ldim1 = mean(dim1), ldim2 = mean(dim2)) %>%
                        ungroup()

  # compute labels closest to cluster center
  dist_calc <- function() {
    for (out_counter in 1:x) { # counter for outer loop is number of columns and is equal to number of clusters
      for (in_counter in 1:inner) { # counter for the inner loop is number of rows and is equal to number of cluster labels by label ID
        # get xy for label, get xy for cluster center and calculate the distance
        x1 <- mean_labels[in_counter, 3]
        y1 <- mean_labels[in_counter, 4]
        x2 <- lab_dim1[out_counter, 1]
        y2 <- lab_dim2[out_counter, 1]
        df[in_counter, out_counter] <- sqrt((sum((x2 - x1)^2) + ((y2 - y1)^2))) # calc dist
      }
    }
    return(df)
  }

  # call the dist_calc function and compute top 5 labels
  inner <- nrow(mean_labels) # counter for inner loop of distance function applies to all clusters

  for (x in 5:15) {
    clu_name <- paste("CLU", x, sep = "")

    # . . replace plyr with dplyr -----
    clu_mean <- plyr::ddply(output_result, clu_name, summarize,
      ldim1 = mean(dim1), ldim2 = mean(dim2) )
    in_counter <- 1 # reset counter to 1
    out_counter <- 1 # reset counter to 1
    df <- as.data.frame(matrix(nrow = inner, ncol = x)) # create a dataframe to hold the distances
    lab_dim1 <- as.data.frame(clu_mean$ldim1) # create a dataframe of value for the function
    lab_dim2 <- as.data.frame(clu_mean$ldim2) # create a dataframe of value for the function
    df <- dist_calc() # run the function
    df <- cbind(mean_labels, df) # create a dataframe to be output to excel

    # create top 5 labels for each cluster solution
    df3 <- NULL
    for (y in 1:x) {
      z <- 4 + y # select the column of distances starting with V1 in col 5

      df2 <- df %>%
        select(label_ID, group, ldim1, ldim2, all_of(z)) %>%
        dplyr::arrange(df[z]) %>%
        slice(1:5)
      colnames(df2)[5] <- "clustdist"
      df2$clus_label <- y
      df3 <- rbind(df3, data.frame(df2))
    }

    df3 <- df3 %>%
      group_by(clus_label) %>%
      dplyr::mutate(rank = 1:n()) %>% # add rank, closest=1
      ungroup()
    colnames(df3) <- c(
      "label_ID", "label_name", "dim1",
      "dim2", "dist_clus_center",
      "cluster_number", "rank"
    ) # rename columns to be easier to read

    assign(paste("cluster", x, sep = ""), data.frame(df3))

    rm(df, df2, df3, lab_dim1, lab_dim2) # remove df from memory and reset with new dimensions in next calculation
  }

  # end compute maps

  # . . create map output (pptx and xlsx) -----
  get_hulls(output_result)
  cluster_means(output_result)

  first_plots <- function() {

    # B&W points
    points_bw <- ggplot2::ggplot(output_result, aes(dim1, dim2)) +
      geom_point(color = "black", size = 3.0) +
      th1
    points_bw <- rvg::dml(ggobj = points_bw) # convert to an editable object
    assign("points_bw", points_bw, envir = .GlobalEnv)

    # B&W points with item numbers as labels
    points_bw_lab <- ggplot2::ggplot(output_result, aes(dim1, dim2)) +
      geom_point(color = "black", size = 3.0) +
      geom_text(aes(label = item), size = 3, position = position_nudge(x = 0.05, y = 0.05)) +
      th1
    points_bw_lab <- rvg::dml(ggobj = points_bw_lab) # convert to an editable object
    assign("points_bw_lab", points_bw_lab, envir = .GlobalEnv)

    # cluster 5 no split
    hull_chosen <- all_hulls %>% dplyr::filter(cluster == 5)
    clus_chosen <- 5
    means_chosen <- all_mean_clu %>% dplyr::filter(cluster == 5) # subset the all_mean_clu

    cluster5_poly_labels <- ggplot2::ggplot(output_result, aes(x = dim1, y = dim2, color = as.factor(CLU5))) +
      geom_point(size = 3.0, shape = 16) +
      geom_text(aes(label = item), size = 3, color = "black", position = position_nudge(x = 0.03, y = 0.03)) +
      scale_color_manual(name = "Cluster #", values = clcol_5, guide = "none") +
      geom_polygon(data = hull_chosen, show.legend = FALSE, fill = NA) +
      th1 +
      th2 +
      annotate(
        geom = "text",
        x = means_chosen[1:clus_chosen, 2],
        y = means_chosen[1:clus_chosen, 3],
        label = means_chosen[1:clus_chosen, 1],
        color = "black", size = 7, fontface = "bold"
      )
    cluster5_poly_labels <- rvg::dml(ggobj = cluster5_poly_labels) # convert to an editable object
    assign("cluster5_poly_labels", cluster5_poly_labels, envir = .GlobalEnv)
  } # end function first_plots

  map_slide_deck <- function(points_bw,
                             points_bw_lab,
                             cluster5_poly_labels) {
    doc2 <- read_pptx() %>%
      # slide, directions
      add_slide(layout = "Title and Content", master = "Office Theme") %>%
      ph_with(ph_location_type(type = "title"),
        value = "Choosing a cluster solution"
      ) %>%
      ph_with(
        value = (unordered_list(
          level_list = c(1, 1, 1, 2, 2, 2, 1, 1, 1),
          style = fp_text(color = "black", font.size = 12),
          str_list = c(
            "This slide deck is helpful for understanding your results and choosing the optimal cluster solution",
            "The first and second maps show the map of the items with and without item numbers",
            "Next are slides which illustrate how the clusters split in going from five to fifteen clusters.",
            "Examine the maps to visualize the change in items membership in a cluster",
            "The slide following the map illustrating each cluster split has tables that list the items in the two cluster that result from the split.",
            "You can also do the same analysis as described here in the output.xlsx file using the ouptput worksheet.  ",
            "These maps can also be edited (e.g. resized, ungrouped) in order to create a visuals for a report",
            "If you present or publish your work, please use the following information to cite this analytical resource.",
            "McLinden, D. (year). IdeaNet - Open Source software in R for concept mapping. Retrieved from https://github.com/ideanetwork/ideanetR"
          ) # end str_list
        ) # end level_list
        ), # end value  =
        location = ph_location_type()
      ) %>% # end ph_with

      # item map without labels
      add_slide(layout = "Title and Content", master = "Office Theme") %>%
      ph_with(value = points_bw, location = ph_location_type(type = "body")) %>%
      ph_with(value = "Map of all items", location = ph_location_type(type = "title")) %>%
      # item map with labels
      add_slide(layout = "Title and Content", master = "Office Theme") %>%
      ph_with(value = points_bw_lab, location = ph_location_type(type = "body")) %>%
      ph_with(value = "Map of all items with item numbers", location = ph_location_type(type = "title")) %>%
      add_slide(layout = "Title and Content", master = "Office Theme") %>%
      ph_with(value = cluster5_poly_labels, location = ph_location_type(type = "body")) %>%
      ph_with(value = "Five cluster map", location = ph_location_type(type = "title"))

    # plots showing the clusters that split
    for (x in 5:14) {
      # equate cluster number to the column that contains that cluster solution in output_results
      cluster_col_A <- if (x == 14) {
        4
      } else
      if (x == 13) {
        5
      } else
      if (x == 12) {
        6
      } else
      if (x == 11) {
        7
      } else
      if (x == 10) {
        8
      } else
      if (x == 9) {
        9
      } else
      if (x == 8) {
        10
      } else
      if (x == 7) {
        11
      } else
      if (x == 6) {
        12
      } else
      if (x == 5) 13

      y <- x + 1 # cluster solution that splits lower cluster number

      cluster_col_B <- cluster_col_A - 1 # cluster col for cluster after the split, higher cluster number col
      cluster_center <- all_mean_clu %>% dplyr::filter(cluster == y) # cluster centers for higher cluster number,
      color_chosen <- clcol_15[1:y] # color for the higher number cluster solution

      output_result <- dplyr::arrange(
        output_result,
        output_result[, cluster_col_A],
        output_result[, 14]
      ) # data must be sorted for cluster comparison to work properly

      means_chosen <- all_mean_clu %>% dplyr::filter(cluster == y) # subset the cluster means for cluster number in plot

      output_result$split <- output_result[, cluster_col_B] - output_result[, cluster_col_A] # subtract columns
      row_split <- (which(output_result$split > 0)) # give the row of the df where split starts
      surround_split <- min(output_result[row_split, cluster_col_A]) # gives the cluster in the current cluster solutions that splits in the next cluster solution

      hull_surround <- all_hulls %>%
        dplyr::filter(cluster == x) %>%
        dplyr::mutate(clu_surround = .[[cluster_col_A]]) %>%
        dplyr::filter(clu_surround == surround_split)

      # get hull for the higher number cluster solution to plot boundaries of the clusters
      hull_chosen <- all_hulls %>%
        dplyr::filter(cluster == y) %>%
        dplyr::mutate(clutemp = .[[cluster_col_B]])

      # set higher cluster solution as a variable that aligns with x,y coordinates for point plot
      output_result <- output_result %>% dplyr::mutate(clutemp = .[[cluster_col_B]])

      cluster_poly_split <- ggplot2::ggplot(output_result, aes(x = dim1, y = dim2, color = as.factor(clutemp))) +
        geom_point(size = 3.0, shape = 16) +
        geom_text(aes(label = item),
          size = 3, color = "black",
          position = position_nudge(x = 0.03, y = 0.03)
        ) +
        scale_color_manual(name = "Cluster #", values = color_chosen) +
        geom_polygon(data = hull_chosen, show.legend = FALSE, fill = NA) +
        geom_polygon(
          data = hull_surround, fill = NA,
          linetype = "dotted", size = 1, color = "black", show.legend = FALSE
        ) +
        annotate(
          geom = "text", x = means_chosen[1:y, 2],
          y = means_chosen[1:y, 3],
          label = means_chosen[1:y, 1],
          color = "black", size = 7, fontface = "bold"
        ) +
        th1 +
        th2

      cluster_poly_split <- rvg::dml(ggobj = cluster_poly_split) # convert to an editable object

      # flextable showing splits
      output_result <- output_result %>% dplyr::mutate(clutemp_surround = .[[cluster_col_A]])

      t_split <- output_result %>%
        dplyr::filter(clutemp_surround == surround_split) %>%
        dplyr::mutate(clu_surround = .[[cluster_col_A]]) %>%
        dplyr::filter(clu_surround == surround_split)

      t_split <- t_split[, c(cluster_col_B, cluster_col_A, 14:15)] # keep relevant columns
      t_split <- dplyr::arrange(t_split, t_split[, 1]) # arrange by higher/splitting cluster

      ft_split <- flextable(data = t_split) %>%
        width(j = 1:3, width = 0.75) %>%
        width(j = 4, width = 6) %>%
        fontsize(size = 12, part = "header") %>%
        fontsize(size = 10, part = "body") %>%
        border_remove() %>%
        theme_vanilla()

      plot_title <- paste(x, "clusters splits into", y, sep = " ") # title for slides

      # splits  plot
      add_slide(doc2, layout = "Title and Content", master = "Office Theme") %>%
        ph_with(value = cluster_poly_split$ggobj, location = ph_location_type(type = "body")) %>%
        ph_with(value = plot_title, location = ph_location_type(type = "title"))

      # splits flextable
      add_slide(doc2, layout = "Title and Content", master = "Office Theme") %>%
        ph_with(value = plot_title, location = ph_location_type(type = "title")) %>%
        ph_with(value = ft_split, location = ph_location(left = 1, top = 1.5, type = "body"))
    } # end loop

    input_wd() # set the working dir to be the same location as the input file.
    print(doc2, target = "output.pptx")
    invisible()
  } # end function to create map_slide_deck to create & save map slide deck

  # . . create dendrogram graphs and slide deck -----

  dendro_slide_deck <- function(hiclust) {

    # adapted from: https://atrebas.github.io/post/2019-06-08-lightweight-dendrograms/
    dendro_data_k <- function(hc, k) {
      hcdata <- ggdendro::dendro_data(hc, type = "rectangle")
      seg <- hcdata$segments
      labclust <- cutree(hc, k)[hc$order]
      segclust <- rep(0L, nrow(seg))
      heights <- sort(hc$height, decreasing = TRUE)
      height <- mean(c(heights[k], heights[k - 1L]), na.rm = TRUE)

      for (i in 1:k) {
        xi <- hcdata$labels$x[labclust == i]
        idx1 <- seg$x >= min(xi) & seg$x <= max(xi)
        idx2 <- seg$xend >= min(xi) & seg$xend <= max(xi)
        idx3 <- seg$yend < height
        idx <- idx1 & idx2 & idx3
        segclust[idx] <- i
      }

      idx <- which(segclust == 0L)
      segclust[idx] <- segclust[idx + 1L]
      hcdata$segments$clust <- segclust
      hcdata$segments$line <- as.integer(segclust < 1L)
      hcdata$labels$clust <- labclust

      hcdata
    } # end dendro_data_k function

    set_labels_params <- function(nbLabels,
                                  direction = c("tb", "bt", "lr", "rl"),
                                  fan = FALSE) {
      if (fan) {
        angle <- 360 / nbLabels * 1:nbLabels + 90
        idx <- angle >= 90 & angle <= 270
        angle[idx] <- angle[idx] + 180
        hjust <- rep(0, nbLabels)
        hjust[idx] <- 1
      } else {
        angle <- rep(0, nbLabels)
        hjust <- 0
        if (direction %in% c("tb", "bt")) {
          angle <- angle + 45
        }
        if (direction %in% c("tb", "rl")) {
          hjust <- 1
        }
      }
      list(angle = angle, hjust = hjust, vjust = 0.5)
    } # end set_labels_param function

    plot_ggdendro <- function(hcdata,
                              k,
                              direction = c("lr", "rl", "tb", "bt"),
                              fan = FALSE,
                              scale.color = NULL,
                              branch.size = 1,
                              label.size = 3,
                              nudge.label = 0.01,
                              expand.y = 0.1) {
      if (k == 1) {
        color_chosen <- c("#000000", "#000000")
      } else {
        if (k >= 5) color_chosen <- c("#000000", clcol_15[1:k])
      } # end if else

      direction <- match.arg(direction) # if fan = FALSE
      ybreaks <- pretty(segment(hcdata)$y, n = 5)
      ymax <- max(segment(hcdata)$y)

      # branches
      p <- ggplot2::ggplot() +
        geom_segment(
          data = segment(hcdata),
          aes(
            x = x,
            y = y,
            xend = xend,
            yend = yend,
            linetype = factor(line),
            colour = factor(clust)
          ),
          lineend = "round",
          show.legend = FALSE,
          size = branch.size
        ) +
        scale_color_manual(values = color_chosen)

      # orientation
      if (fan) {
        p <- p +
          coord_polar(direction = -1) +
          scale_x_continuous(
            breaks = NULL,
            limits = c(0, nrow(label(hcdata)))
          ) +
          scale_y_reverse(breaks = ybreaks)
      } else {
        p <- p + scale_x_continuous(breaks = NULL)
        if (direction %in% c("rl", "lr")) {
          p <- p + coord_flip()
        }
        if (direction %in% c("bt", "lr")) {
          p <- p + scale_y_reverse(breaks = ybreaks)
        } else {
          p <- p + scale_y_continuous(breaks = ybreaks)
          nudge.label <- -(nudge.label)
        }
      } # end if else

      # labels
      labelParams <- set_labels_params(nrow(hcdata$labels), direction, fan)
      hcdata$labels$angle <- labelParams$angle

      p <- p +
        geom_text(
          data = label(hcdata),
          aes(
            x = x,
            y = y,
            label = label,
            colour = factor(clust),
            angle = angle
          ),
          vjust = labelParams$vjust,
          hjust = labelParams$hjust,
          nudge_y = ymax * nudge.label,
          size = label.size,
          show.legend = FALSE
        )

      # colors and limits
      if (!is.null(scale.color)) {
        p <- p + scale_color_manual(values = color_chosen)
      }

      ylim <- -round(ymax * expand.y, 1)
      p <- p + expand_limits(y = ylim)

      p
    } # end plot_dendro function

    k <- 1 # B&W dendrogram
    hc <- dendro_data_k(hiclust, k) # convert CA to dendro_data and add k cuts

    p_dendro <- plot_ggdendro(hc,
      k,
      direction   = "lr",
      expand.y    = 0.2
    )
    p_dendro <- p_dendro +
      th7 +
      th8 +
      th9

    p_dendro <- rvg::dml(ggobj = p_dendro) # convert to an editable object

    doc3 <- read_pptx() %>%
      # slide, directions
      add_slide(layout = "Title and Content", master = "Office Theme") %>%
      ph_with(ph_location_type(type = "title"),
        value = "Choosing a cluster solution"
      ) %>%
      ph_with(
        value = (unordered_list(
          level_list = c(1, 1, 1, 1, 1),
          style = fp_text(color = "black", font.size = 12),
          str_list = c(
            "This slide deck contains dendrograms and may be useful for choosing acluster solution",
            "Use this slide deck along with output.xlsx and output.pptx.",
            "These graphs can also be edited (e.g. resized, ungrouped) in order to create a visuals for a report",
            "If you present or publish your work, please use the following information to cite this analytical resource.",
            "McLinden, D. (year). IdeaNet - Open Source software in R for concept mapping. Retrieved from https://github.com/ideanetwork/ideanetR"
          ) # end str_list
        ) # end unordered_list
        ), # end value=
        location = ph_location_type()
      ) %>% # end ph_with

      # B&W dendogram
      add_slide(layout = "Title and Content", master = "Office Theme") %>%
      ph_with(value = p_dendro, location = ph_location_type(type = "body")) %>%
      ph_with(value = "Dendogram", location = ph_location_type(type = "title"))

    # loop for cluster 5:15 adding color
    for (k in 5:15) {
      hc <- dendro_data_k(hiclust, k) # convert CA to dendro_data and add k cuts
      p_dendro_cl <- plot_ggdendro(hc,
        k,
        direction   = "lr",
        expand.y    = 0.2
      )
      p_dendro_cl <- p_dendro_cl +
        th7 +
        th8 +
        th9

      p_dendro_cl <- rvg::dml(ggobj = p_dendro_cl) # convert to an editable object

      plot_title <- paste("Dendrogram", k, "clusters", sep = " ") # title for slides

      add_slide(doc3, layout = "Title and Content", master = "Office Theme") %>%
        ph_with(value = p_dendro_cl$ggobj, location = ph_location_type(type = "body")) %>%
        ph_with(value = plot_title, location = ph_location_type(type = "title"))
    } # end loop

    # save output_dendrogram.pptx
    print(doc3, target = "output_dendrogram.pptx")
    invisible()

    # create table of items in the same order as the dendrograms
    k <- 1
    hc <- dendro_data_k(hiclust, k) # convert CA to dendro_data and add k cuts
    hc_table <- hc$labels
    hc_table <- subset(hc_table, select = -c(y, clust)) # drop cols not needed
    names(hc_table)[names(hc_table) == "label"] <- "item" # rename for merge
    output_items <- subset(output_result, select = c(item, item_text))
    hc_table <- merge(x = output_items, y = hc_table, by = "item", all = TRUE)
    hc_table <- hc_table %>% dplyr::arrange(x, decreasing = TRUE) # arrange in same order as dendrogram
    colnames(hc_table)[2] <- "Item text in same order as dendrograms"
    colnames(hc_table)[3] <- "Dendrogram order, sort high to low "

    assign("hc_table", hc_table, envir = .GlobalEnv) # object is needed for subsequent functions
  } # end dendro_slide_deck function

  # . . call functions to output -----
  input_wd() # get and set wd

  first_plots() # overall plots

  # slide deck showing cluster splits from 5 to 15
  map_slide_deck(
    # input_dir,
    points_bw,
    points_bw_lab,
    cluster5_poly_labels
  )

  # slide deck of dendograms showing splits from 5 to 15
  dendro_slide_deck(hiclust)

  # save output.xlsx
  # create wb
  output <- createWorkbook()
  addWorksheet(output, "stress")
  addWorksheet(output, "output")
  addWorksheet(output, "dendrogram")
  addWorksheet(output, "cluster5")
  addWorksheet(output, "cluster6")
  addWorksheet(output, "cluster7")
  addWorksheet(output, "cluster8")
  addWorksheet(output, "cluster9")
  addWorksheet(output, "cluster10")
  addWorksheet(output, "cluster11")
  addWorksheet(output, "cluster12")
  addWorksheet(output, "cluster13")
  addWorksheet(output, "cluster14")
  addWorksheet(output, "cluster15")

  #  add wks to wb and save
  writeData(output, "stress", mstress, startCol = 1, startRow = 1,colNames = TRUE, rowNames = FALSE)
  writeData(output, "output", output_result, startCol = 1, startRow = 1, colNames = TRUE, rowNames = FALSE) # write cards sorted to workbook
  writeData(output, "dendrogram", hc_table, startCol = 1, startRow = 1, colNames = TRUE, rowNames = FALSE) # write cards sorted to workbook
  writeData(output, "cluster5", cluster5, startCol = 1, startRow = 1, colNames = TRUE, rowNames = FALSE) # write cards sorted to workbook
  writeData(output, "cluster6", cluster6, startCol = 1, startRow = 1, colNames = TRUE, rowNames = FALSE) # write cards sorted to workbook
  writeData(output, "cluster7", cluster7, startCol = 1, startRow = 1, colNames = TRUE, rowNames = FALSE) # write cards sorted to workbook
  writeData(output, "cluster8", cluster8, startCol = 1, startRow = 1, colNames = TRUE, rowNames = FALSE) # write cards sorted to workbook
  writeData(output, "cluster9", cluster9, startCol = 1, startRow = 1, colNames = TRUE, rowNames = FALSE) # write cards sorted to workbook
  writeData(output, "cluster10", cluster10, startCol = 1, startRow = 1, colNames = TRUE, rowNames = FALSE) # write cards sorted to workbook
  writeData(output, "cluster11", cluster11, startCol = 1, startRow = 1, colNames = TRUE, rowNames = FALSE) # write cards sorted to workbook
  writeData(output, "cluster12", cluster12, startCol = 1, startRow = 1, colNames = TRUE, rowNames = FALSE) # write cards sorted to workbook
  writeData(output, "cluster13", cluster13, startCol = 1, startRow = 1, colNames = TRUE, rowNames = FALSE) # write cards sorted to workbook
  writeData(output, "cluster14", cluster14, startCol = 1, startRow = 1, colNames = TRUE, rowNames = FALSE) # write cards sorted to workbook
  writeData(output, "cluster15", cluster15, startCol = 1, startRow = 1, colNames = TRUE, rowNames = FALSE) # write cards sorted to workbook

  saveWorkbook(output, "output.xlsx", overwrite = TRUE)

  msg1 <- "Analysis is complete. Output files were created (output.pptx, output.xlsx, output_dendrogram.pptx) and saved in the same directory as the source (i.e.input.xlsx) file."
  msg2 <- " "
  msg3 <- "1. output.xlsx contains the stress value (stress tab), cluster membership (output tab), items ordered to match dendrogram pptx file (dendrogram tab), best fitting cluster labels for each cluster solution (multiple tabs)."
  msg4 <- " "
  msg5 <- "2. output.pptx contains maps showing which clusters split for each cluster solution and the items in each split cluster."
  msg6 <- " "
  msg7 <- "3. output_dendrogram.pptx illustrates the splitting of clusters on a dendrogram. Refer to dendrogram tab in output.xlsx to view text of the items."
  msg8 <- " "
  msg9 <- "PLEASE NOTE:  In some cases when you open the output PowerPoint or Excel file you will get a message such as the following - Powerpoint found a problem with content in output... This is is not an R issue nor is there a problem with the Ideanet program. This is a known issue with PowerPoint and Excel. Click on REPAIR, then click on OK, and the file will open and will be fine"

  output_message <- print(paste(msg1, msg2, msg3, msg4, msg5, msg6, msg7, msg8,
    msg9,
    sep = "\n"
  ))
  tcltk::tk_messageBox(type = "ok", message = output_message)
} # end getdata_computemaps function
# End compute maps & create output output.xlsx output_maps.pptx output_dendrogram.pptx

# ********************************************************************************************** -----

# individual report -----


overall_plots <- function(output_result, color_chosen, hull_chosen, means_chosen) {
  points_bw <- ggplot2::ggplot(output_result, aes(dim1, dim2)) +
    geom_point(color = "black", size = 3.0) +
    th1
  points_bw <- rvg::dml(ggobj = points_bw) # convert to an editable object
  assign("points_bw", points_bw, envir = .GlobalEnv)

  # B&W points with item numbers as labels
  points_bw_lab <- ggplot2::ggplot(output_result, aes(dim1, dim2)) +
    geom_point(color = "black", size = 3.0) +
    geom_text(aes(label = item), size = 3, position = position_nudge(x = 0.05, y = 0.05)) +
    th1
  points_bw_lab <- rvg::dml(ggobj = points_bw_lab) # convert to an editable object
  assign("points_bw_lab", points_bw_lab, envir = .GlobalEnv)

  # polygons for clusters with points and item number and without item numbers
  cluster_poly <- ggplot2::ggplot(output_result, aes(x = dim1, y = dim2, color = as.factor(clutemp))) +
    geom_point(size = 3.0, shape = 16) +
    geom_text(aes(label = item),
      size = 3, color = "black",
      position = position_nudge(x = 0.03, y = 0.03)
    ) +
    geom_polygon(data = hull_chosen, alpha = .05, show.legend = FALSE) +
    scale_color_manual(name = "Cluster #", values = color_chosen, guide = "none") +
    th1 +
    th2
  cluster_poly <- rvg::dml(ggobj = cluster_poly) # convert to an editable object
  assign("cluster_poly", cluster_poly, envir = .GlobalEnv)

  cluster_poly_labels <- ggplot2::ggplot(output_result, aes(x = dim1, y = dim2, color = as.factor(clutemp))) +
    geom_point(size = 3.0, shape = 16) +
    geom_text(aes(label = item),
      size = 3, color = "black",
      position = position_nudge(x = 0.03, y = 0.03)
    ) +
    geom_polygon(data = hull_chosen, alpha = .05, show.legend = FALSE) +
    scale_color_manual(name = "Cluster #", values = color_chosen, guide = "none") +
    annotate(
      geom = "text", x = means_chosen[1:clus_chosen, 2],
      y = means_chosen[1:clus_chosen, 3],
      label = means_chosen[1:clus_chosen, 1],
      color = "black", size = 7, fontface = "bold"
    ) +
    th1 +
    th2
  cluster_poly_labels <- rvg::dml(ggobj = cluster_poly_labels) # convert to an editable object
  assign("cluster_poly_labels", cluster_poly_labels, envir = .GlobalEnv)

  # polygons for clusters with clusters filled
  cluster_poly_fill <- ggplot2::ggplot(output_result, aes(x = dim1, y = dim2, color = as.factor(clutemp))) +
    geom_polygon(data = hull_chosen, show.legend = FALSE, aes(fill = as.factor(clutemp))) +
    scale_color_manual(name = "Cluster #", values = color_chosen, guide = "none") +
    annotate(
      geom = "text", x = means_chosen[1:clus_chosen, 2],
      y = means_chosen[1:clus_chosen, 3],
      label = means_chosen[1:clus_chosen, 1],
      color = "black", size = 7, fontface = "bold"
    ) +
    th1 +
    th2
  cluster_poly_fill <- rvg::dml(ggobj = cluster_poly_fill) # convert to an editable object
  assign("cluster_poly_fill", cluster_poly_fill, envir = .GlobalEnv)
} # end  overal_plots function

# plot clusters within a cluster solution and output to PowerPoint
out_clusters <- function(output_file, i, endi, title_text, output_result, output_labels, points_bw, points_bw_lab, cluster_poly, cluster_poly_labels, cluster_poly_fill) {
  doc_num <- read_pptx()
  # first slide, directions
  add_slide(doc_num, layout = "Title and Content", master = "Office Theme") %>%
    ph_with(value = title_text, location = ph_location_type(type = "title")) %>%
    ph_with(
      value = (unordered_list(
        level_list = c(1, 1, 2, 2, 1, 1, 1, 1),
        style = fp_text(color = "black", font.size = 12),
        str_list = c(
          "This report is typically useful after you have chosen the optimal cluster solution",
          "This report provides the overall map for the cluster solution along with:",
          "a map with an individual cluster highlighted along with a table of the items in the highlighted cluster",
          "a table of the items within a cluster along with a table of the labels for that cluster that are nearest to the cluster center and likely candidates for a final name",
          "Use this report as a guide for cluster naming and a resource for creating a presentation slide deck",
          "These maps can edited (e.g. resized, ungrouped) in order to create a visuals for a report",
          "If you present or publish your work, please use the following information to cite this analytical resource.",
          "McLinden, D. (year). IdeaNet - Open Source software in R for concept mapping. Retrieved from https://github.com/ideanetwork/ideanetR"
        ) # end str_list
      ) # end unordered_list
      ), # end value=
      location = ph_location_type("body")
    ) %>% # end ph_with

    # second slide, item map without labels
    add_slide(layout = "Title Only", master = "Office Theme") %>%
    ph_with(value = "Map of all items with item labels", location = ph_location_type(type = "title")) %>%
    ph_with(value = points_bw, location = map_loc1) %>%
    # third slide, item map with labels
    add_slide(layout = "Title Only", master = "Office Theme") %>%
    ph_with(value = points_bw_lab, location = map_loc1) %>%
    ph_with(value = "Map of all items with item labels", location = ph_location_type(type = "title")) %>%
    # next three slides, item cluster map with boundaries around clusters
    add_slide(layout = "Title Only", master = "Office Theme") %>%
    ph_with(value = cluster_poly, location = map_loc1) %>%
    ph_with(value = title_text, location = ph_location_type(type = "title")) %>%
    add_slide(layout = "Title Only", master = "Office Theme") %>%
    ph_with(value = cluster_poly_labels, location = map_loc1) %>%
    ph_with(value = title_text, location = ph_location_type(type = "title")) %>%
    add_slide(layout = "Title Only", master = "Office Theme") %>%
    ph_with(value = cluster_poly_fill, location = map_loc1) %>%
    ph_with(value = title_text, location = ph_location_type(type = "title"))

  #  .  . Create individual plots
  color_vector <- vector(length = endi) # create a vector of length i to endi to hold colors in loops

  # outer loop to create i to endi slides
  for (i in 1:endi) {
    # inner loop to create color for one cluster and grey for all other clusters for a single slide
    for (x in 1:endi) color_vector[x] <- x
    colortemp <- ifelse(color_vector == i, clcol_grey[i], clcol_grey[16])

    find_hull <- function(output_result) output_result[chull(output_result$dim1, output_result$dim2), ]

    # . . replace plyr with dplyr -----
    hulls <- plyr::ddply(output_result, "clutemp", find_hull)

    cl <- paste("Cluster number", i) # text for each slide in the outer loop

    p <- ggplot2::ggplot(output_result, aes(x = dim1, y = dim2, color = as.factor(clutemp))) +
      geom_point(size = 3.0, shape = 16) +
      ggtitle(title_text) +
      geom_text(aes(label = item), size = 3, color = "black", position = position_nudge(x = 0.03, y = 0.03)) +
      geom_polygon(data = hulls, alpha = .05, show.legend = FALSE) +
      scale_color_manual(name = NULL, values = colortemp) +
      th1 +
      th2
    p <- rvg::dml(ggobj = p) # convert to an editable object

    # item text as a flextable
    t1 <- subset(output_result, clutemp == i, select = item_text) # select text for individual cluster
    ft1 <- flextable(data = t1) %>%
      align(align = "left", part = "all") %>%
      fontsize(size = 9, part = "header") %>%
      fontsize(size = 8, part = "body") %>%
      border_remove() %>%
      width(j = 1, width = 4) %>%
      set_header_labels(item_text = "Item Text") %>%
      theme_vanilla()

    # top labels as flextable
    t2 <- subset(output_labels, cluster_number == i, select = label_name) # select text for individual cluster
    ft2 <- flextable(data = t2) %>%
      align(align = "left", part = "all") %>%
      fontsize(size = 9, part = "header") %>%
      fontsize(size = 8, part = "body") %>%
      border_remove() %>%
      width(j = 1, 4) %>%
      set_header_labels(group = "Five labels closest to cluster center") %>%
      theme_vanilla()

    # output slides highlighted cluster and item text
    add_slide(doc_num, layout = "Blank", master = "Office Theme") %>%
      ph_with(value = cl, location = ph_location_type(type = "ftr")) %>%
      ph_with(value = ft1, location = ft_location1) %>%
      ph_with(value = p, location = map_loc2)

    # output slides item text and best fitting cluster labels
    add_slide(doc_num, layout = "Blank", master = "Office Theme") %>%
      ph_with(value = cl, location = ph_location_type(type = "ftr")) %>%
      ph_with(value = ft1, location = ft_location1) %>%
      ph_with(value = ft2, location = ft_location2)
  } # end loop

  # get the path from location of output.xlsx and save pptx in same location
  output_wd()
  target_file <- paste("output", clus_chosen, "cluster report.pptx", sep = " ") # set unique file name

  print(doc_num, target = target_file)
  invisible()
  rm(doc_num)
} # end out_clusters

# function called from tktcl to create individual report
entry_clus_rpt <- function() {

  # clears frames from canvas when a new menu item is chosen at top level menu
  reset_canvas <- function() {
    if (exists("lpw.1")) tcltk::tkdestroy(lpw.1)
    if (exists("lpw.2")) tcltk::tkdestroy(lpw.2)
    if (exists("lpw.3")) tcltk::tkdestroy(lpw.3)
    if (exists("lpw.4")) tcltk::tkdestroy(lpw.4)
    if (exists("lpw.5")) tcltk::tkdestroy(lpw.5)
    if (exists("lpw.6")) tcltk::tkdestroy(lpw.6)
    if (exists("lpw.7")) tcltk::tkdestroy(lpw.7)
    if (exists("lpw.8")) tcltk::tkdestroy(lpw.8)
    if (exists("lpw.9")) tcltk::tkdestroy(lpw.9)
    if (exists("lpw.10")) tcltk::tkdestroy(lpw.10)
    if (exists("lpw.11")) tcltk::tkdestroy(lpw.11)
    if (exists("lpw.12")) tcltk::tkdestroy(lpw.12)
    if (exists("lpw.13")) tcltk::tkdestroy(lpw.13)
    if (exists("compute.layer.but")) tcltk::tkdestroy(compute.layer.but)
    if (exists("reset.but")) tcltk::tkdestroy(reset.but)
  } # end reset_canvas

  reset_canvas()

  clus_report <- function(clus_chosen) {

    # if no errors call functions to make available in this code
    cluster_means(output_result)
    get_labels(output_names, output_file)
    get_hulls(output_result)

    # equate cluster number to the column that contains that cluster solution in output_results
    cluster_col <- if (clus_chosen == 15) {
      3
    } else
    if (clus_chosen == 14) {
      4
    } else
    if (clus_chosen == 13) {
      5
    } else
    if (clus_chosen == 12) {
      6
    } else
    if (clus_chosen == 11) {
      7
    } else
    if (clus_chosen == 10) {
      8
    } else
    if (clus_chosen == 9) {
      9
    } else
    if (clus_chosen == 8) {
      10
    } else
    if (clus_chosen == 7) {
      11
    } else
    if (clus_chosen == 6) {
      12
    } else
    if (clus_chosen == 5) 13

    # set the variable in the temp file equal to the cluster, this is used in function out_clusters
    output_result <- output_result %>% dplyr::mutate(clutemp = .[[cluster_col]])

    hull_chosen <- all_hulls %>% dplyr::filter(cluster == clus_chosen)
    hull_chosen <- hull_chosen %>% dplyr::mutate(clutemp = .[[cluster_col]])

    color_chosen <- clcol_15[1:clus_chosen]
    means_chosen <- all_mean_clu %>% dplyr::filter(cluster == clus_chosen) # subset the all_mean_clu

    overall_plots(
      output_result,
      color_chosen,
      hull_chosen,
      means_chosen
    ) # call function to create overall plots for the cluster solution

    title_text <- paste(clus_chosen, "cluster solution", sep = " ")
    endi <- clus_chosen
    i <- 1
    output_labels <- top_labels
    doc_num <- read_pptx()
    out_clusters(
      output_file,
      i,
      endi,
      title_text,
      output_result,
      output_labels,
      points_bw,
      points_bw_lab,
      cluster_poly,
      cluster_poly_labels,
      cluster_poly_fill
    )

    report_msg <- paste("Report for cluster", clus_chosen, "is complete and is saved in the directory used to read in source data. lick OK to close this message.", sep = " ")

    tcltk::tk_messageBox(type = "ok", message = report_msg)

    tcltk::tkdestroy(lpw.1)

  } # end clus_report function

# create widget to call clus_report()
  lpw.1 <- tcltk::tkframe(tt, bg = "white", width = 800, height = 600)
  tcltk::tkpack(lpw.1, side = "left", padx = 25, expand = TRUE, fill = "both")
  tcltk::tkgrid(tcltk2::tk2message(lpw.1, text = " ", bg = "white", justify = "left"))
  tcltk::tkgrid(tcltk2::tk2message(lpw.1, text = " ", bg = "white", justify = "left"))
  tcltk::tkgrid(tcltk2::tk2message(lpw.1, text = " ", bg = "white", justify = "left"))
  tcltk::tkgrid(tcltk2::tk2message(lpw.1, bg = "white", justify = "center", width = 500, text = "Click button and select output.xlsx cluster data file created from computing maps"))
  button.widget <- tcltk::tkbutton(lpw.1, text = "Select output data file", command = get_output)
  tcltk::tkgrid(button.widget)

  # input field for cluster selection
  tcltk::tkgrid(tcltk2::tk2message(lpw.1, bg = "white", text = " "))
  tcltk::tkgrid(tcltk2::tk2message(lpw.1, bg = "white", width = 400, text = "Enter cluster number between 5 and 15 inclusive and click SUBMIT."))
  tbValue <- tcltk::tclVar("")
  entry.tbValue <- tcltk::tkentry(lpw.1, width = "10", bg = "LightGrey", textvariable = tbValue)
  tcltk::tkgrid(entry.tbValue)

  submit_A <- function() { # function to submit cluster number to create cluster report
    clus_chosen <- as.numeric(tcltk::tclvalue(tbValue))
    clus_chosen <- as.integer(clus_chosen)
    if ((clus_chosen %in% seq.int(from = 5, to = 15, by = 1)) == FALSE) invalid_value() #   validate input  value for cluster chosen
    assign("clus_chosen", clus_chosen, envir = .GlobalEnv)
    clus_report(clus_chosen)
  }
  submit.but_A <- tcltk::tkbutton(lpw.1, text = "submit", command = submit_A)

  tcltk::tkgrid(submit.but_A)

} # end entry_clus_rpt

# ********************************************************************************************** -----
# pattern analysis with cluster rating/layer map -----
pattern_analysis <- function() {

  # clears frames from canvas when a new menu item is chosen at top level menu
  reset_canvas <- function() {
    if (exists("lpw.1")) tcltk::tkdestroy(lpw.1)
    if (exists("lpw.2")) tcltk::tkdestroy(lpw.2)
    if (exists("lpw.3")) tcltk::tkdestroy(lpw.3)
    if (exists("lpw.4")) tcltk::tkdestroy(lpw.4)
    if (exists("lpw.5")) tcltk::tkdestroy(lpw.5)
    if (exists("lpw.6")) tcltk::tkdestroy(lpw.6)
    if (exists("lpw.7")) tcltk::tkdestroy(lpw.7)
    if (exists("lpw.8")) tcltk::tkdestroy(lpw.8)
    if (exists("lpw.9")) tcltk::tkdestroy(lpw.9)
    if (exists("lpw.10")) tcltk::tkdestroy(lpw.10)
    if (exists("lpw.11")) tcltk::tkdestroy(lpw.11)
    if (exists("lpw.12")) tcltk::tkdestroy(lpw.12)
    if (exists("lpw.13")) tcltk::tkdestroy(lpw.13)
    if (exists("compute.layer.but")) tcltk::tkdestroy(compute.layer.but)
    if (exists("reset.but")) tcltk::tkdestroy(reset.but)
  } # end reset_canvas

  reset_canvas()

  # creates and save cluster rating map pptx
  create_layer <- function(cluster_chosen) {

    # check for output pptx & delete file if it exists
    f <- "cluster rating map.pptx"
    if (file.exists(f)) file.remove(f)

    # cluster solution 15 starts in column 3,
    cluster_col <- if (clus_chosen == 15) {
      3
    } else if (clus_chosen == 14) {
      4
    } else if (clus_chosen == 13) {
      5
    } else if (clus_chosen == 12) {
      6
    } else if (clus_chosen == 11) {
      7
    } else if (clus_chosen == 10) {
      8
    } else if (clus_chosen == 9) {
      9
    } else if (clus_chosen == 8) {
      10
    } else if (clus_chosen == 7) {
      11
    } else if (clus_chosen == 6) {
      12
    } else if (clus_chosen == 5) {
      13
    }

    get_clu_name <- as.character(colnames(output_result[cluster_col]))

    # get hulls
    output_result <- output_result %>% dplyr::mutate(clutemp = .[[cluster_col]])
    find_hull <- function(output_result) output_result[chull(output_result$dim1, output_result$dim2), ]
    hulls <- plyr::ddply(output_result, "clutemp", find_hull)

    clu_mean_loc<-output_result %>%
      dplyr::group_by(output_result[,cluster_col]) %>%
      dplyr::summarize(ldim1=mean(dim1), ldim2=mean(dim2))%>%
      ungroup()

    names(clu_mean_loc) <- c(get_clu_name, "ldim1", "ldim2") # rename so that the merge can use common name
    clu_mean_loc[[1]] <- as.numeric(as.character(clu_mean_loc[[1]])) # coerce from factor to numeric in order to join

    # create default cluster names
    clu_text <- (as.data.frame(matrix(nrow = clus_chosen, ncol = 2))) # create a df to hold num and text
    names(clu_text) <- c(get_clu_name, "clu_text") # name cols

    text <- as.data.frame(matrix(nrow = clus_chosen, ncol = 1))
    for (y in 1:clus_chosen) {
      text[y, 1] <- paste("Cluster", y, sep = "")
    }
    for (x in 1:clus_chosen) {
      clu_text[x, 1] <- x
      clu_text[x, 2] <- text[x, 1]
    }

    clu_mean_loc <- full_join(x = clu_mean_loc, y = clu_text, by = get_clu_name) # add cluster text

    # count observation and add to slide deck
    subset_observations <- nrow(subset_data)
    msg_subset_observations <- paste("N = ", subset_observations) # message is ftr in pptx

    # get start and end cols based on number of measures
    rating_col <- ncol(subset_data) - 4 # get rating columns
    if (input_values_def[7, 2] == 1) {
      start_col1 <- 5
      end_col1 <- start_col1 + rating_col - 1
    } else if (input_values_def[7, 2] == 2) {
      start_col1 <- 5
      end_col1 <- (rating_col / 2) + 4
      start_col2 <- end_col1 + 1
      end_col2 <- start_col2 + (rating_col / 2) - 1
    }

    # match measure_choice input by user to data and choose start and stop cols for item means
    if (input_values_def[8, 2] == measure_choice) {
      start_col <- start_col1
      end_col <- end_col1
      item_means <- as.data.frame(colMeans(subset_data[, start_col:end_col]))
    } else if (input_values_def[12, 2] == measure_choice) {
      start_col <- start_col2
      end_col <- end_col2
      item_means <- as.data.frame(colMeans(subset_data[, start_col:end_col]))
    }

    item_means <- item_means %>% rename_with(.cols = 1, ~"item_means")
    item_means$item <- as.integer(seq(1:length(item_means$item_means))) # add sequence to match item variable in subset to merge on

    output_result <- full_join(x = output_result, y = item_means, by = "item") # add subsetted item means to cluster data

    clu_mean_value <- output_result %>%
      dplyr::group_by(.[[cluster_col]]) %>%
      dplyr::summarise(mean = mean(item_means)) %>%
      ungroup()
    names(clu_mean_value) <- c(get_clu_name, "cluster_mean")
    clu_mean_value[[1]] <- as.numeric(as.character(clu_mean_value[[1]])) # coerce from factor to numeric in order to join

    # compute quintiles so that lowest quintile is one layer/hull
    bb <- quantile(clu_mean_value$cluster_mean, probs = seq(0, 1, 0.20))

    # hull_count is the number of layers/hulls that corresponds to the quintile
    clu_mean_value$hull_count <- ifelse(clu_mean_value$cluster_mean <= bb[2], 1,
      ifelse(clu_mean_value$cluster_mean > bb[2] & clu_mean_value$cluster_mean <= bb[3], 2,
        ifelse(clu_mean_value$cluster_mean > bb[3] & clu_mean_value$cluster_mean <= bb[4], 3,
          ifelse(clu_mean_value$cluster_mean > bb[4] & clu_mean_value$cluster_mean <= bb[5], 4,
            ifelse(clu_mean_value$cluster_mean > bb[5], 5, 99)
          )
        )
      )
    )

    # merge data for flextable output
    clu_mean_value <- full_join(clu_mean_value, clu_mean_loc)
    clu_mean_value <- as.data.frame(clu_mean_value) # coerce from tibble to dataframe, required by ggplot2::ggplot

    # compute number of layers for each hull
    hulls$hull_count <- NA # add new columns hull_count to hold value of hull count
    # add layer value to each boundary/hull item
    for (i in 1:nrow(hulls)) {
      for (j in 1:clus_chosen) {
        if (hulls$clutemp[i] == j) {
          hulls$hull_count[i] <- clu_mean_value$hull_count[j]
        }
      }
    }
    my_data <- list()

    cblack <- rep("black", 15)
    for (i in 1:5) {
      my_data[[i]] <- hulls
      my_data[[i]]$dim2 <- my_data[[i]]$dim2 - 0.05 * (i - 1) * (hulls$hull_count > i - 1)
    }

    layer_map <- ggplot2::ggplot(hulls, aes(x = dim1, y = dim2, color = as.factor(clutemp))) +
      scale_color_manual(name = "Cluster #", values = cblack[c(1:clus_chosen)]) +
      scale_fill_manual(name = "Cluster #", values = clcol_grey[c(1:clus_chosen)]) +
      th5 +
      th7

    for (i in 0:4) {
      layer_map <- layer_map + geom_polygon(data = my_data[[5 - i]], aes(fill = as.factor(clutemp)))
    }

    for (i in 1:clus_chosen) {
      layer_map <- layer_map + annotate(
        geom = "text",
        x = clu_mean_value[i, 4],
        y = clu_mean_value[i, 5],
        label = clu_mean_value[i, 6],
        color = "black", size = 3, fontface = "plain"
      )
    }

    layer_map <- rvg::dml(ggobj = layer_map) # convert to an editable object

    names(clu_mean_value) <- c("Cluster#", "Cluster mean", "Number of layers", "dim1", "dim2", "Cluster name")
    names(subset_msg) <- "Subset choices"

    layer_ft <- flextable(data = clu_mean_value) %>%
      width(width = 1.5) %>%
      fontsize(size = 11, part = "header") %>%
      fontsize(size = 11, part = "body") %>%
      theme_vanilla()

    choices_ft <- flextable(data = subset_msg) %>%
      width(width = 7) %>%
      fontsize(size = 10, part = "header") %>%
      fontsize(size = 10, part = "body") %>%
      line_spacing(space = 1) %>%
      theme_vanilla()

    # create pptx file
    my_pres <- read_pptx() %>%
      add_slide(layout = "Blank", master = "Office Theme") %>%
      ph_with(value = pa_title, location = pa_title_loc) %>%
      ph_with(value = msg_subset_observations, location = ph_location_type(type = "ftr")) %>%
      ph_with(value = layer_map, location = pa_layer_loc) %>%
      add_slide(layout = "Blank", master = "Office Theme") %>%
      ph_with(value = pa_title, location = pa_title_loc) %>%
      ph_with(value = msg_subset_observations, location = ph_location_type(type = "ftr")) %>%
      ph_with(value = layer_ft, location = pa_ft_loc) %>%
      add_slide(layout = "Blank", master = "Office Theme") %>%
      ph_with(value = pa_title, location = pa_title_loc) %>%
      ph_with(value = msg_subset_observations, location = ph_location_type(type = "ftr")) %>%
      ph_with(
        value = choices_ft,
        location = pa_choices_loc
      ) %>%
      print(my_pres, target = "cluster rating map.pptx") # save to working directory

    reset_canvas()
  } # end create layer function

  # linked to compute button to create and save cluster rating map calls create_layer function
  compute_layer_map <- function() {
    tcltk::tkdestroy(compute.layer.but)
    create_layer(clus_chosen)

    if (exists("subset_data") == "FALSE") {
      msg_layer1 <- "Demographic data has not been selected."
      msg_layer2 <- "Click the SELECT button even if choosing ALL observations."
      msg_layer <- paste(msg_layer1, msg_layer2, sep = "\n")
      tcltk::tk_messageBox(type = "ok", message = msg_layer)
    } else if (exists("subset_data") == "TRUE") {
      msg_layer1 <- "A cluster rating map has been created - cluster rating map.pptx."
      msg_layer2 <- ""
      msg_layer3 <- "The file is saved in the same directory as the output Excel file."
      msg_layer4 <- ""
      msg_layer5 <- "NOTE: Rename the pptx file if you plan to create additional cluster rating maps, otherwise a new cluster rating map will overwrite the existing pptx file."
      msg_layer6 <- ""
      msg_layer7 <- "Click RESET to create another map."
      msg_layer <- paste(msg_layer1, msg_layer2, msg_layer3, msg_layer4, msg_layer5, msg_layer6, msg_layer7, sep = "\n")
      tcltk::tk_messageBox(type = "ok", message = msg_layer)
    } # end if else
  } # end compute layer function

  # open three left frames to choose demographics and subset data
  choosing_demographics <- function(x, y, var_count, demo_frame) {

    # start if variable is continuous
    if (input_values_def[x, 3] == "continuous") {
      type <- "continuous"
      # select button function to record user input for continuous variable and subset data
      select.cont.demo <- function() {
        continuous.1 <- tcltk::tclvalue(choose_subset1) # first boolean operator
        continuous.2 <- tcltk::tclvalue(choose_subset2) # second boolean operator
        subset1_chosen <- as.numeric(tcltk::tclvalue(subset.value1)) # first value for boolean operator
        subset2_chosen <- as.numeric(tcltk::tclvalue(subset.value2)) # second value for boolean operator

        # validate data
        choice_mismatch <- function() {
          msg_choice_mismatch <- paste(demo_name, "-", "If first choice is ALL then second choice must be NONE.", sep = " ")
          tcltk::tk_messageBox(type = "ok", message = msg_choice_mismatch) # if first choice is ALL then second choice must be NONE
        }
        choice_missing_value <- function() {
          msg_missing_value <- paste(demo_name, "-", "A value must be entered in the field below the operator chosen.", sep = " ")
          tcltk::tk_messageBox(type = "ok", message = msg_missing_value) # choosing an operator but missing the value for subset
        }
        choice_duplicate_operators <- function() {
          msg_duplicate_operators <- paste(demo_name, "-", "The choices are not valid. The operators are the same. Operators need to be different to create a range of values.")
          tcltk::tk_messageBox(type = "ok", message = msg_duplicate_operators) # if the operators are the same then the second is redundant
        }
        choice_invalid_range <- function() {
          msg_invalid_range <- paste(demo_name, "-", "The values for the operators are the same.The values need to be different to create a range of values.")
          tcltk::tk_messageBox(type = "ok", message = msg_invalid_range) # if the operators differ but the values are the same then there is no range
        }
        choice_invalid_value <- function() {
          msg_invalid_value <- paste(demo_name, "-", "If the operator is ALL or NONE, the value in the field needs to be blank.")
          tcltk::tk_messageBox(type = "ok", message = msg_invalid_value) # if the operators differ but the values are the same then there is no range
        }

        # if subset input from canvas choices are valid then subset data
        cont_choice_ok <- function() {

          # if subset_data df exists from prior subset then continue subsetting that df else create df from input
          if (exists("subset_data") == "TRUE") {
            subset_data <- subset_data
          } else
          if (exists("subset_data") == "FALSE") subset_data <- input_result

          # if first option is any  choice and second option is NONE
          one_subset_option <- function() {
            subset_data <- if (continuous.1 == "ALL") {
              subset_data
            } else
            if (continuous.1 == "GT") {
              dplyr::filter(subset_data, subset_data[, x] > subset1_chosen)
            } else
            if (continuous.1 == "GE") {
              dplyr::filter(subset_data, subset_data[, x] >= subset1_chosen)
            } else
            if (continuous.1 == "EQ") {
              dplyr::filter(subset_data, subset_data[, x] == subset1_chosen)
            } else
            if (continuous.1 == "LT") {
              dplyr::filter(subset_data, subset_data[, x] < subset1_chosen)
            } else
            if (continuous.1 == "LE") dplyr::filter(subset_data, subset_data[, x] <= subset1_chosen)

            assign("subset_data", subset_data, envir = .GlobalEnv)

            tcltk::tkdestroy(select.cont.demo.but) # destroy button so no further choices can be made, user needs to user reset button to start over
          } # end one_subset_option function

          # first option is not ALL and second option is not NONE
          two_subset_option <- function() {
            # get the first group
            first_group <- if (continuous.1 == "GT") {
              dplyr::filter(subset_data, subset_data[, x] > subset1_chosen)
            } else
            if (continuous.1 == "GE") {
              dplyr::filter(subset_data, subset_data[, x] >= subset1_chosen)
            } else
            if (continuous.1 == "EQ") {
              dplyr::filter(subset_data, subset_data[, x] == subset1_chosen)
            } else
            if (continuous.1 == "LT") {
              dplyr::filter(subset_data, subset_data[, x] < subset1_chosen)
            } else
            if (continuous.1 == "LE") dplyr::filter(subset_data, subset_data[, x] <= subset1_chosen)

            # get the second group
            second_group <- if (continuous.2 == "GT") {
              dplyr::filter(subset_data, subset_data[, x] > subset2_chosen)
            } else
            if (continuous.2 == "GE") {
              dplyr::filter(subset_data, subset_data[, x] >= subset2_chosen)
            } else
            if (continuous.2 == "EQ") {
              dplyr::filter(subset_data, subset_data[, x] == subset2_chosen)
            } else
            if (continuous.2 == "LT") {
              dplyr::filter(subset_data, subset_data[, x] < subset2_chosen)
            } else
            if (continuous.2 == "LE") dplyr::filter(subset_data, subset_data[, x] <= subset2_chosen)

            # combine the two datasets with dplyr/tidy
            subset_data <- bind_rows(first_group, second_group)

            assign("subset_data", subset_data, envir = .GlobalEnv)

            # destroy button so no further choices can be made, user needs to user reset button to start over
            tcltk::tkdestroy(select.cont.demo.but)
          } # end function to subset data with two choices

          if (continuous.2 == "NONE") one_subset_option()
          if (continuous.2 != "NONE") two_subset_option()

          msg_c0 <- "Data has been subsetted."
          msg_c1 <- paste("Type of variable", type, sep = " ")
          msg_c2 <- paste("Variable name", demo_name, sep = " ")
          msg_c3 <- paste("Variable range from", demo_min, "to", demo_max, sep = " ")
          msg_c4 <- paste("First choice", continuous.1, subset1_chosen, sep = " ")
          msg_c5 <- paste("Second choice", continuous.2, subset2_chosen, sep = " ")
          msg_c6 <- "----------"
          msg_c7 <- "Continue with next variable or if complete, click compute."

          # create msg to add to flextable and output to pptx
          msg_choices <- paste(msg_c1, msg_c2, msg_c3, msg_c4,
            msg_c5,
            sep = "\n"
          )
          demo_count <- x - 1 # set value of index to 1 - 3
          subset_msg[demo_count, 1] <- msg_choices
          assign("subset_msg", subset_msg, envir = .GlobalEnv)

          continuous_choices <- paste(msg_c0, msg_c1, msg_c2, msg_c3, msg_c4,
            msg_c5, msg_c6, msg_c7,
            sep = "\n"
          )
          tcltk::tk_messageBox(type = "ok", message = continuous_choices)
        } # end choice_ok function to subset data

        # call validate data functions if choices not valid else call function to subset data if choices OK
        if (continuous.1 != "ALL" & is.na(subset1_chosen) == "TRUE") {
          choice_missing_value()
        } else if (continuous.1 == "ALL" & is.na(subset1_chosen) == "FALSE") {
          choice_invalid_value()
        } else if (continuous.2 == "NONE" & is.na(subset2_chosen) == "FALSE") {
          choice_invalid_value
        } else if (continuous.1 != "ALL" & is.na(subset1_chosen) == "FALSE" & continuous.2 != "NONE" & is.na(subset2_chosen) == "TRUE") {
          choice_missing_value()
        } else if (continuous.1 == "ALL" & continuous.2 != "NONE") {
          choice_mismatch()
        } else if (continuous.1 == continuous.2) {
          choice_duplicate_operators()
        } else {
          cont_choice_ok()
        }
      } # end select_cont.demo button function to get user input for continuous demographic variable and subset data

      # create widgets for continuous variable
      demo_name <- input_values_def[x, 2]
      demo_min <- input_values_def[x, 4]
      demo_max <- input_values_def[x, 5]
      demo_msg1 <- paste("Min=", demo_min, sep = "")
      demo_msg2 <- paste("Max=", demo_max, sep = "")
      demo_msg <- paste(var_count, demo_name, demo_msg1, demo_msg2, sep = "\n")

      subset1_var <- c("ALL", "GT", "GE", "EQ", "LT", "LE", "NE")
      choose_subset1 <- tcltk::tclVar("ALL") # set default to all
      combo.demo_cont1 <- tcltk::ttkcombobox(demo_frame,
        width = 25, values = subset1_var,
        textvariable = choose_subset1, state = "readonly"
      )
      subset2_var <- c("NONE", "GT", "GE", "EQ", "LT", "LE", "NE")
      choose_subset2 <- tcltk::tclVar("NONE")
      combo.demo_cont2 <- tcltk::ttkcombobox(demo_frame,
        width = 25, values = subset2_var,
        textvariable = choose_subset2, state = "readonly"
      )

      subset.value1 <- tcltk::tclVar("")
      entry.tbValue1 <- tcltk::tkentry(demo_frame, width = "10", bg = "LightGrey", textvariable = subset.value1)
      subset.value2 <- tcltk::tclVar("")
      entry.tbValue2 <- tcltk::tkentry(demo_frame, width = "10", bg = "LightGrey", textvariable = subset.value2)
      select.cont.demo.but <- tcltk::tkbutton(demo_frame,
        text = "Select",
        command = select.cont.demo
      ) # get input data for continuous variable

      # position widgets for continuous variable
      tcltk::tkgrid(tcltk2::tk2message(demo_frame,
        bg = "aliceblue", text = demo_msg,
        pady = 3, width = 300
      ), sticky = "w")
      tcltk::tkgrid(tcltk::tklabel(demo_frame,
        text = "Select ALL or select a subset of data",
        pady = 3, bg = "aliceblue"
      ), sticky = "w")
      tcltk::tkgrid(combo.demo_cont1, sticky = "w")
      tcltk::tkgrid(tcltk::tklabel(demo_frame,
        text = "Enter a value if choosing a subset", bg = "aliceblue",
        pady = 3
      ), sticky = "w")
      tcltk::tkgrid(entry.tbValue1, sticky = "w")
      tcltk::tkgrid(tcltk::tklabel(demo_frame,
        text = "Select a second criteria to subset data", bg = "aliceblue",
        pady = 3
      ), sticky = "w")
      tcltk::tkgrid(combo.demo_cont2, pady = 3, sticky = "w")
      tcltk::tkgrid(tcltk::tklabel(demo_frame, text = "Enter a value if choosing a subset", bg = "aliceblue", pady = 3),
        sticky = "w"
      )
      tcltk::tkgrid(entry.tbValue2, sticky = "w")
      tcltk::tkgrid(select.cont.demo.but, pady = 3)
      # end if continuous variable
    } else {

      # start if categorical variable
      if (input_values_def[x, 3] == "categorical") {
        type <- "categorical"

        # selection button for categorical variable
        select.cat.demo <- function() {
          cat_choice <- choices[as.numeric(tcltk::tkcurselection(demo_cat_lb)) + 1] # number choice in selection choices

          # data not valid message functions called
          all_cat_sub <- function() {
            msg_all_cat_sub <- paste(demo_name, "-", "The value of ALL and a value for a subset were both chosen.  Deselect ALL to subset or deselect other options to use ALL.")
            tcltk::tk_messageBox(type = "ok", message = msg_all_cat_sub) # if the operators differ but the values are the same then there is no range
          }
          missing_cat <- function() {
            msg_missing_cat <- paste(demo_name, "-", "No choices were made. Select ALL or choose to subset data.")
            tcltk::tk_messageBox(type = "ok", message = msg_missing_cat) # if the operators differ but the values are the same then there is no range
          }
          choose_all_cat <- function() {
            msg_choose_all_cat <- paste(demo_name, "-", "All subset values where chosen. Deselect and choose ALL instead.")
            tcltk::tk_messageBox(type = "ok", message = msg_choose_all_cat) # if the operators differ but the values are the same then there is no range
          }

          # data valid
          cat_choice_ok <- function() {
            # if subset_data df exists from prior subset then continue subsetting that df else create df from input
            if (exists("subset_data") == "TRUE") {
              subset_data <- subset_data
            } else
            if (exists("subset_data") == "FALSE") subset_data <- input_result

            # subset the data
            if (cat_choice == "ALL") {
              subset_data <- subset_data
            } else
            if (cat_choice != "ALL") subset_data <- dplyr::filter(input_result, input_result[, y] %in% cat_choice)

            # message to user about choices
            collapse_demo_levels <- paste(demo_levels, collapse = " ") # convert char vectors to single element for messages, otherwise messages repeat for length of vector
            collapse_cat_choice <- paste(cat_choice, collapse = " ")
            msg_c0 <- "Data has been subsetted."
            msg_c1 <- paste("Type of variable", type, sep = " ")
            msg_c2 <- paste("Variable name", demo_name, sep = " ")
            msg_c3 <- paste("Number of levels", demo_n_levels, sep = " ")
            msg_c4 <- paste("Possible choices", collapse_demo_levels, sep = " ")
            msg_c5 <- paste("Choice made", collapse_cat_choice, sep = " ")
            msg_c6 <- "----------"
            msg_c7 <- "Continue with next variable or if complete, click compute."

            # create msg to add to flextable and output to pptx
            msg_choices <- paste(msg_c1, msg_c2, msg_c3, msg_c4,
              msg_c5,
              sep = "\n"
            )
            demo_count <- x - 1 # set value of index to 1 - 3
            subset_msg[demo_count, 1] <- msg_choices
            assign("subset_msg", subset_msg, envir = .GlobalEnv)

            categorical_choices <- paste(msg_c0, msg_c1, msg_c2, msg_c3, msg_c4,
              msg_c5, msg_c6, msg_c7,
              sep = "\n"
            )
            tcltk::tk_messageBox(type = "ok", message = categorical_choices)

            assign("subset_data", subset_data, envir = .GlobalEnv)

            tcltk::tkdestroy(select.cat.demo.but) # destroy button so no further choices can be made, user needs to user reset button to start over
          } # end  the demo_cat_ok function

          # validate categorical choice
          if ((length(cat_choice) > 1) & ("ALL" %in% cat_choice == TRUE)) {
            all_cat_sub() # if choice includes ALL and one other choice then deselect something
          } else if (length(cat_choice) == 0) {
            missing_cat() # if no choices
          } else if (length(cat_choice) == demo_n_levels) {
            choose_all_cat() # if all choices are highlighted, select ALL and deselect others or deselect something
          } else {
            cat_choice_ok() # data is valid, call function to subset data
          } # end if then to validate data
        } # end selection button

        # get categorical values from input data
        demo_name <- input_values_def[x, 2]
        demo_var <- input_result[, y]
        demo_var <- as.data.frame(demo_var)
        demo_var$demo_var <- as.factor(demo_var$demo_var)
        demo_n_levels <- nlevels(demo_var$demo_var)
        demo_levels <- levels(demo_var$demo_var)
        demo_msg1 <- paste(demo_n_levels, " levels", sep = "")
        demo_msg <- paste(var_count, demo_name, demo_msg1, sep = "\n")

        # define widgets for demographics
        demo_cat_lb <- tcltk2::tk2listbox(demo_frame, height = 4, selectmode = "multiple") # demographics listbox allows multiple choices
        choices <- c("ALL", demo_levels)
        for (choice in choices) {
          tcltk::tkinsert(demo_cat_lb, "end", choice)
        }
        tcltk::tkselection.set(demo_cat_lb, 0) # Default is ALL; Indexing starts at zero.
        select.cat.demo.but <- tcltk::tkbutton(demo_frame,
          text = "Select",
          command = select.cat.demo
        ) # get input data for continuous variable

        # position widgets for demographics
        tcltk::tkgrid(tcltk2::tk2message(demo_frame, bg = "aliceblue", text = demo_msg, width = 300, pady = 3), sticky = "w")
        tcltk::tkgrid(tcltk::tklabel(demo_frame,
          text = "Select ALL or select a subset of data", bg = "aliceblue",
          pady = 3
        ), sticky = "w")
        tcltk::tkgrid(tcltk::tklabel(demo_frame,
          text = "Click to deselect or select a variable",
          pady = 3, bg = "aliceblue"
        ), sticky = "w")
        tcltk::tkgrid(tcltk::tklabel(demo_frame,
          text = "Multiple groups may be selected",
          pady = 3, bg = "aliceblue"
        ), sticky = "w")
        tcltk::tkgrid(demo_cat_lb) # , padx = 10, pady = c(5, 10))
        tcltk::tkgrid(select.cat.demo.but, pady = 3)
        # end if categorical variable
      } else {
        # start if no demographic variable
        # if subset_data df exists from prior subset then continue subsetting that df else create df from input
        if (exists("subset_data") == "TRUE") {
          subset_data <- subset_data
        } else
        if (exists("subset_data") == "FALSE") subset_data <- input_result

        assign("subset_data", subset_data, envir = .GlobalEnv)

        no_choices <- "There is no demographic data"

        # create msg to add to flextable and output to pptx
        msg_choices <- no_choices
        demo_count <- x - 1 # set value of index to 1 - 3
        subset_msg[demo_count, 1] <- msg_choices
        assign("subset_msg", subset_msg, envir = .GlobalEnv)
        demo_msg <- paste(var_count, no_choices, sep = "\n")

        tcltk::tkgrid(tcltk2::tk2message(demo_frame,
          bg = "aliceblue", text = demo_msg,
          pady = 3, width = 300
        ), sticky = "w")

        # end if no demographic variable
      } # end last/second else
    } # end first else
  } # end choosing_demographics function

  # loops through input data looking for demographic variables passing information to choosing_demographics()
  input_demographics <- function() {
    # right side frame choose demographic subsets
    for (z in seq(2, 4, 1)) {
      x <- z
      y <- z

      if (z == 2) {
        var_count <- "First demographic variable "
        demo_frame <- lpw.6
      } else if (z == 3) {
        var_count <- "Second demographic variable "
        demo_frame <- lpw.7
      } else if (z == 4) {
        var_count <- "Third demographic variable "
        demo_frame <- lpw.8
      } # end last else

      choosing_demographics(x, y, var_count, demo_frame) # call function to build frames to select demographics
    } # end loop
  } # end input_demographics function

  # select  measure and open remaining frames for demographic choices
  meas_choice <- function() {
    select_meas <- function() {
      measure_choice <- tcltk::tclvalue(choose_measure)
      assign("measure_choice", measure_choice, envir = .GlobalEnv)
      tcltk::tkdestroy(button.widget_meas)

      compute.layer.but <- tcltk::tkbutton(lpw.3, text = "Compute cluster rating map", command = compute_layer_map)
      tcltk::tkpack(compute.layer.but, side = "left", pady = 10)
      assign("compute.layer.but", compute.layer.but, envir = .GlobalEnv)

      input_demographics() # call function to add demographics to remaining frames
    } # end  select_meas function

    # . . . . choose a measure
    if (input_values_def[7, 2] == 1) {
      measures <- input_values_def[8, 2]
    } else {
      if (input_values_def[7, 2] == 2) {
        measures <- c(input_values_def[8, 2], input_values_def[12, 2])
      }
    } # end if else

    first_measure <- measures[1] # populate measures dropdown with default first measure
    label.measure <- tcltk::tklabel(lpw.5, text = "Select a measure")
    choose_measure <- tcltk::tclVar(first_measure)
    assign("choose_measure", choose_measure, envir = .GlobalEnv)
    combo.measure <- tcltk::ttkcombobox(lpw.5,
      width = 25, values = measures,
      textvariable = choose_measure, state = "readonly"
    )

    # select button for measure
    button.widget_meas <- tcltk::tkbutton(lpw.5,
      text = "Save selected data",
      command = select_meas
    )
    tcltk::tkgrid(tcltk2::tk2message(lpw.5,
      bg = "aliceblue", justify = "center", width = 300, pady = 3, font = fontSub,
      text = "Select measure/rating"
    ))
    tcltk::tkgrid(tcltk2::tk2message(lpw.5,
      bg = "aliceblue", justify = "left", width = 300, pady = 3, font = fontQ,
      text = "Choose one measure from dropdown list"
    ))
    tcltk::tkgrid(combo.measure, pady = 3)
    tcltk::tkgrid(button.widget_meas)
  }

  # open bottom half of first frame to choose cluster solution and measure and call measure choice meas_choice()
  clus_choice <- function() {
    # if the data files are chosen then add cluster choice
    # choose a cluster solution widget
    select_clus <- function() {
      # cluster solution chosen
      clus_chosen <- as.numeric(tcltk::tclvalue(tbValue))
      clus_chosen <- as.integer(clus_chosen)
      if ((clus_chosen %in% seq.int(from = 5, to = 15, by = 1)) == FALSE) invalid_value() # validate input  value for cluster chosen
      assign("clus_chosen", clus_chosen, envir = .GlobalEnv)

      # if clus and measure chosen then open demo frames
      clus_exists <- ifelse((exists("clus_chosen")), "yes", "no")

      if (clus_exists == "yes") {
        subset_data <- input_result # subset the data here in case there are non demographics
        assign("subset_data", subset_data, envir = .GlobalEnv)
        tcltk::tkdestroy(button.widget_clus)

        meas_choice()
      } else {
        tcltk::tk_messageBox(type = "ok", message = "Select a cluster solution.")
      } # end else
    } # end function select_clus function

    # select number of clusters widget
    tbValue <- tcltk::tclVar("")
    entry.tbValue <- tcltk::tkentry(lpw.1, width = "10", bg = "LightGrey", textvariable = tbValue)

    button.widget_clus <- tcltk::tkbutton(lpw.1, text = "Save selected data", command = select_clus)

    tcltk::tkgrid(tcltk2::tk2message(lpw.1,
      bg = "aliceblue", justify = "left", width = 300, pady = 3, font = fontSub,
      text = "Select cluster solution"
    ))
    tcltk::tkgrid(tcltk2::tk2message(lpw.1,
      bg = "aliceblue", pady = 3, justify = "left", width = 300, font = fontQ,
      text = "Choose cluster solution: Enter a value between 5 and 15 inclusive."
    ))
    tcltk::tkgrid(entry.tbValue, pady = 3)
    tcltk::tkgrid(tcltk2::tk2message(lpw.1,
      bg = "aliceblue", justify = "center", width = 300, pady = 3, font = fontQ,
      text = "----------"
    ))

    tcltk::tkgrid(button.widget_clus)
    tcltk::tkgrid(tcltk2::tk2message(lpw.1,
      bg = "aliceblue", justify = "left", width = 300, pady = 3, font = fontQ,
      text = "Click to save selected cluster."
    ))

    # dimension an empty data frame to hold messages used to summarize data choices
    # which are input from choice in demographics and then output to layer map as flextables
    subset_msg <- (as.data.frame(matrix(nrow = 3, ncol = 1)))
    subset_msg[[1]] <- as.character(subset_msg[[1]])
    assign("subset_msg", subset_msg, envir = .GlobalEnv)
 } # end clu_choice function

  # .  . create the canvas for cluster rating/layer map -----
    # define canvas for pattern analysis
    lpw.1 <- tcltk::tkframe(tt, bg = "aliceblue")
    lpw.2 <- tcltk::tkframe(tt, bg = "white")
    lpw.3 <- tcltk::tkframe(lpw.2, bg = "white", height = 20)
    lpw.4 <- tcltk::tkframe(lpw.2, bg = "white")
    lpw.5 <- tcltk::tkframe(lpw.4, bg = "aliceblue")
    lpw.6 <- tcltk::tkframe(lpw.4, bg = "aliceblue")
    lpw.7 <- tcltk::tkframe(lpw.4, bg = "aliceblue")
    lpw.8 <- tcltk::tkframe(lpw.4, bg = "aliceblue")

    tcltk::tkpack(lpw.1, lpw.2, side = "left", padx = 3, expand = TRUE, fill = "both")
    tcltk::tkpack(lpw.3, side = "bottom", padx = 3, expand = FALSE, fill = "both")
    tcltk::tkpack(lpw.4, side = "top", padx = 3, expand = TRUE, fill = "both")
    tcltk::tkpack(lpw.5, lpw.6, lpw.7, lpw.8, side = "left", padx = 3, expand = TRUE, fill = "both")
    reset.but <- tcltk::tkbutton(lpw.3, text = "Reset", command = reset_canvas)

    tcltk::tkpack(reset.but, side = "left", padx = 100, pady = 10)

    assign("lpw.1", lpw.1, envir = .GlobalEnv)
    assign("lpw.2", lpw.2, envir = .GlobalEnv)
    assign("lpw.3", lpw.3, envir = .GlobalEnv)
    assign("lpw.4", lpw.4, envir = .GlobalEnv)
    assign("lpw.5", lpw.5, envir = .GlobalEnv)
    assign("lpw.6", lpw.6, envir = .GlobalEnv)
    assign("lpw.7", lpw.7, envir = .GlobalEnv)
    assign("lpw.8", lpw.8, envir = .GlobalEnv)
    assign("reset.but", reset.but, envir = .GlobalEnv)

  # . . open top half of first frame to select data files and then call clus_choice() -----
    # define widgets
    # select data files widgets
    button.widget_input <- tcltk::tkbutton(lpw.1,
                                           text = "Select input data file",
                                           command = get_input)
    button.widget_output <- tcltk::tkbutton(lpw.1,
                                            justify = "left",
                                            text = "Select output data file", command = get_output
    )

    # after selecting data file, button will read in data for measure choices
    select_data_files <- function() {
      cluster_data <- if (exists("output_result") == TRUE) {
        # destroy button in selecting data file for analyze values to avoid clicking twice in error
        if (exists("button.widget_input")) tcltk::tkdestroy(button.widget_input)
        if (exists("button.widget_output")) tcltk::tkdestroy(button.widget_output)
        tcltk::tkdestroy(button.widget_data)
        clus_choice() # call function to open bottom half of frame to select cluster solution and measure
      } else
        if (exists("input_result") == FALSE) {
          tcltk::tk_messageBox(type = "ok",
                               message = "Missing one or both data files. Select input data file with ratings and and measure definition and output data file with cluster membership.")
        } # end if else
    } # end select_data_files function

    button.widget_data <- tcltk::tkbutton(lpw.1, text = "Save selected data", command = select_data_files)

    # layout for data selection left frame
    tcltk::tkgrid(tcltk2::tk2message(lpw.1,
                                        bg = "aliceblue", justify = "left", width = 300, pady = 3, font = fontSub,
                                        text = "Load data"
    ))
    tcltk::tkgrid(tcltk2::tk2message(lpw.1,
                                     bg = "aliceblue", justify = "left", width = 300, pady = 3, font = fontQ,
                                     text = "Each pattern analysis requires two files, input.xlsx containing rating data & output.xlsx containing cluster membership.*"
    ))
    tcltk::tkgrid(tcltk2::tk2message(lpw.1,
                                     bg = "aliceblue", justify = "left", width = 300, pady = 3, font = fontQ,
                                     text = "If you have renamed the excel files, choose the files that correspond to input.xlxs and output.xlsx respectively."
    ))
    tcltk::tkgrid(tcltk2::tk2message(lpw.1,
                                     bg = "aliceblue",
                                     text = "*NOTE: This analysis assumes REVIEW DATA step has been completed and a map has been computed.",
                                     pady = 15, width = 300, font = fontQ
    ))
    tcltk::tkgrid(button.widget_input, pady = 3)
    tcltk::tkgrid(button.widget_output, pady = 3)
    tcltk::tkgrid(button.widget_data, pady = 3)
    tcltk::tkgrid(tk2message(lpw.1,
                             bg = "aliceblue", justify = "center", width = 300, pady = 3, font = fontQ,
                             text = "----------"
    ))

} # end pattern_analysis function

# end pattern analysis using cluster rating/layer map
# ********************************************************************************************** -----

# pattern matching ladders and go zones/quadrant graphs -----
pattern_matching <- function() {

  # clears frames from canvas when a new menu item is chosen at top level menu
  reset_canvas <- function() {
    if (exists("lpw.1")) tcltk::tkdestroy(lpw.1)
    if (exists("lpw.2")) tcltk::tkdestroy(lpw.2)
    if (exists("lpw.3")) tcltk::tkdestroy(lpw.3)
    if (exists("lpw.4")) tcltk::tkdestroy(lpw.4)
    if (exists("lpw.5")) tcltk::tkdestroy(lpw.5)
    if (exists("lpw.6")) tcltk::tkdestroy(lpw.6)
    if (exists("lpw.7")) tcltk::tkdestroy(lpw.7)
    if (exists("lpw.8")) tcltk::tkdestroy(lpw.8)
    if (exists("lpw.9")) tcltk::tkdestroy(lpw.9)
    if (exists("lpw.10")) tcltk::tkdestroy(lpw.10)
    if (exists("lpw.11")) tcltk::tkdestroy(lpw.11)
    if (exists("lpw.12")) tcltk::tkdestroy(lpw.12)
    if (exists("lpw.13")) tcltk::tkdestroy(lpw.13)
    if (exists("compute.layer.but")) tcltk::tkdestroy(compute.layer.but)
    if (exists("reset.but")) tcltk::tkdestroy(reset.but)
  } # end reset_canvas

  reset_canvas()
  pattern_match_complete <- function() {
    if (exists("subset_data_left") == "FALSE" | (exists("subset_data_right") == "FALSE")) {
      msg_layer1 <- "All or some demographic data has NOT been selected."
      msg_layer2 <- "Click the SELECT button even if choosing ALL observations for a variable."
      msg_layer <- paste(msg_layer1, msg_layer2, sep = "\n")
      tcltk::tk_messageBox(type = "ok", message = msg_layer)
    } else if (exists("subset_data_left") == "TRUE" & exists("subset_data_right") == "TRUE") {
      msg_layer1 <- "A ladder graph and go-zones graphs have  been created - pattern_match.pptx"
      msg_layer2 <- ""
      msg_layer3 <- "The file is saved in the same directory as the output Excel file."
      msg_layer4 <- ""
      msg_layer5 <- "NOTE: Rename the pptx file if you plan to create additional graphs, otherwise new pattern matching output will overwrite the existing pptx file."
      msg_layer6 <- ""
      msg_layer7 <- "Click RESET to create another analysis."
      msg_layer <- paste(msg_layer1, msg_layer2, msg_layer3, msg_layer4, msg_layer5, msg_layer6, msg_layer7, sep = "\n")
      tcltk::tk_messageBox(type = "ok", message = msg_layer)

      reset_canvas()
    } # end if else
  } # end pattern_match_complete function

  compute_pattern_match <- function() {

    # NOTE:  left ladder=vertical gozone, right ladder = horizontal gozone

    # remove existing files
    f <- "pattern_match.pptx"
    if (file.exists(f)) { # Check its existence
      file.remove(f)
    } # Delete file if it exists

    # labels ladder and for quadrant dimensions
    left_vert_label <- "Vertical axis"
    right_horiz_label <- "Horizontal axis"

    # cluster solution 15 starts in column 3 of output.xlsx output worksheet
    cluster_col <- if (clus_chosen == 15) {
      3
    } else if (clus_chosen == 14) {
      4
    } else if (clus_chosen == 13) {
      5
    } else if (clus_chosen == 12) {
      6
    } else if (clus_chosen == 11) {
      7
    } else if (clus_chosen == 10) {
      8
    } else if (clus_chosen == 9) {
      9
    } else if (clus_chosen == 8) {
      10
    } else if (clus_chosen == 7) {
      11
    } else if (clus_chosen == 6) {
      12
    } else if (clus_chosen == 5) {
      13
    } # end if else

    get_clu_name <- as.character(colnames(output_result[cluster_col]))

    output_result <- output_result %>% dplyr::mutate(clutemp = .[[cluster_col]]) # create a temp cluster col with the cluster solution selected

    # count observation and add to slide deck
    subset_observations_left <- nrow(subset_data_left)
    subset_observations_right <- nrow(subset_data_right)

    assign("subset_observations_left", subset_observations_left, envir = .GlobalEnv)
    assign("subset_observations_right", subset_observations_right, envir = .GlobalEnv)

    # create info for flextable to describe left/vertical right/horizontal
    measure_left <- paste("Measure:", measure_choice_left, sep = " ")
    measure_left <- as.data.frame(measure_left) # measure for left/vertical
    colnames(measure_left)[1] <- "V1"
    subset_msg_left <- as.data.frame(subset_msg_left) # demographic choices left/vertical
    colnames(subset_msg_left)[1] <- "V1"
    detail_vert <- rbind(measure_left, subset_msg_left)
    colnames(detail_vert) <- "vertical_left"

    measure_right <- paste("Measure:", measure_choice_right, sep = " ")
    measure_right <- as.data.frame(measure_right) # measure for left/vertical
    colnames(measure_right)[1] <- "V1"
    subset_msg_right <- as.data.frame(subset_msg_right) # demographic choices left/vertical
    colnames(subset_msg_right)[1] <- "V1"
    detail_horiz <- rbind(measure_right, subset_msg_right)
    colnames(detail_horiz) <- "horizontal_right"

    # get start and end cols based on number of measures
    rating_col <- ncol(subset_data) - 4 # get rating columns
    if (input_values_def[7, 2] == 1) {
      start_col1 <- 5
      end_col1 <- start_col1 + rating_col - 1
    } else if (input_values_def[7, 2] == 2) {
      start_col1 <- 5
      end_col1 <- (rating_col / 2) + 4
      start_col2 <- end_col1 + 1
      end_col2 <- start_col2 + (rating_col / 2) - 1
    }

    # . . summarize item means -----
    # left side
    # match measure_choice input by user to data and choose start and stop cols for item means
    if (input_values_def[8, 2] == measure_choice_left) {
      start_col <- start_col1
      end_col <- end_col1
      item_means_left <- as.data.frame(colMeans(subset_data_left[, start_col:end_col]))
    } else if (input_values_def[12, 2] == measure_choice_left) {
      start_col <- start_col2
      end_col <- end_col2
      item_means_left <- as.data.frame(colMeans(subset_data_left[, start_col:end_col]))
    }
    item_means_left <- item_means_left %>% rename_with(.cols = 1, ~"item_means_left")
    item_means_left$item <- as.integer(seq(1:length(item_means_left$item_means))) # add sequence to match item variable in subset to merge on

    # right side
    # match measure_choice input by user to data and choose start and stop cols for item means
    if (input_values_def[8, 2] == measure_choice_right) {
      start_col <- start_col1
      end_col <- end_col1
      item_means_right <- as.data.frame(colMeans(subset_data_right[, start_col:end_col]))
    } else if (input_values_def[12, 2] == measure_choice_right) {
      start_col <- start_col2
      end_col <- end_col2
      item_means_right <- as.data.frame(colMeans(subset_data_right[, start_col:end_col]))
    }
    item_means_right <- item_means_right %>% rename_with(.cols = 1, ~"item_means_right")
    item_means_right$item <- as.integer(seq(1:length(item_means_right$item_means))) # add sequence to match item variable in subset to merge on

    left_right_items <- full_join(x = item_means_left, y = item_means_right, by = "item")
    output_result <- full_join(x = output_result, y = left_right_items, by = "item") # add subsetted item means to cluster data

    # . . summarize cluster means -----
    # create default cluster names
    clu_text <- (as.data.frame(matrix(nrow = clus_chosen, ncol = 2))) # create a df to hold num and text
    names(clu_text) <- c(get_clu_name, "clu_text") # name cols

    text <- as.data.frame(matrix(nrow = clus_chosen, ncol = 1))
    for (y in 1:clus_chosen) {
      text[y, 1] <- paste("Cluster", y, sep = "")
    }
    for (x in 1:clus_chosen) {
      clu_text[x, 1] <- x
      clu_text[x, 2] <- text[x, 1]
    }

    # . . compute raw data values for ladder -----
    # left
    clu_mean_value_left <- output_result %>%
      group_by(clutemp) %>%
      dplyr::summarise(Mean = mean(item_means_left))%>%
      ungroup()

    names(clu_mean_value_left) <- c(get_clu_name, "cluster_mean_left")
    clu_mean_value_left[[1]] <- as.numeric(as.character(clu_mean_value_left[[1]])) # coerce from factor to numeric in order to join

    # right
    clu_mean_value_right <- output_result %>%
      group_by(clutemp) %>%
      dplyr::summarise(Mean = mean(item_means_right))%>%
      ungroup()

    names(clu_mean_value_right) <- c(get_clu_name, "cluster_mean_right")
    clu_mean_value_right[[1]] <- as.numeric(as.character(clu_mean_value_right[[1]])) # coerce from factor to numeric in order to join

    left_right_cluster <- full_join(x = clu_mean_value_left, y = clu_mean_value_right, by = get_clu_name)

    clu_mean_value_ladder <- full_join(clu_text, left_right_cluster)

    # compute the correlation ladder
    corr_ladder <- cor(clu_mean_value_ladder$cluster_mean_left,
      clu_mean_value_ladder$cluster_mean_right,
      method = "spearman"
    )
    corr_ladder <- format(round(corr_ladder, 2))

    names('clu_mean_value_ladder') <- c("cluster", "cluster_name", "left_mean", "right_mean")
    clu_mean_value_ladder$left_mean <- round(clu_mean_value_ladder$left_mean, digits = 3)
    clu_mean_value_ladder$right_mean <- round(clu_mean_value_ladder$right_mean, digits = 3)

    # create ladder data in long form
    ladder_stack <- reshape2::melt(clu_mean_value_ladder,
      id.vars = c("cluster", "cluster_name"),
      value.name = "value"
    )

    colnames(ladder_stack)[colnames(ladder_stack) == "variable"] <- "measure"
    ladder_stack$group <- factor(ladder_stack$cluster)
    ladder_stack$measure <- factor(ladder_stack$measure)

    # ladder color
    ladder_color <- clcol_grey[1:clus_chosen]
    ladder_title <- paste(clus_chosen, "Cluster Solution", sep = " ")

    # set ladder scale with min and max,
    scale_min <- min(ladder_stack$value)
    scale_max <- max(ladder_stack$value)
    scale_min <- round(scale_min, digits = 3)
    scale_max <- round(scale_max, digits = 3)

    # expand the min and max to expand the limits on ggplot2::ggplot
    scale_min_expand <- min(ladder_stack$value) - .05
    scale_max_expand <- max(ladder_stack$value) + .05

    scale_values <- as.data.frame(rbind(scale_min, scale_max))
    colnames(scale_values)[1] <- "value" # needed for aes geom_text for labeling laddder axis
    scale_values$measure <- rownames(scale_values)
    scale_values$group <- seq(1, 2)
    scale_values$group <- factor(scale_values$group)
    scale_values$measure <- factor(scale_values$measure)

    # get cluster labels for plot, subset left and right values
    left_label <- ladder_stack %>% dplyr::filter(measure == "left_mean")
    right_label <- ladder_stack %>% dplyr::filter(measure == "right_mean")

    # . . ladder and flextables no change to data -----
    ladder_scale <- ggplot2::ggplot(data = ladder_stack, aes(x = measure,
                                                             y = value,
                                                             group = group,
                                                             colour = group)) +
      geom_line(linetype = "solid", size = 1) +
      geom_point(size = 3) +
      scale_y_continuous(
        name = NULL,
        limits = c(scale_min_expand, scale_max_expand),
        breaks = seq(scale_min, scale_max, 0.25),
        expand = expansion(add = 0.25),
        sec.axis = dup_axis()
      ) +
      scale_x_discrete(name = "Measure comparisons") + # , expand = expansion(add = 0)  )+
      ggtitle(ladder_title) +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_color_manual(values = ladder_color) +
      # add cluster labels to ladder
      geom_text(
        data = left_label,
        aes(label = cluster_name),
        size = 3,
        check_overlap = FALSE,
        nudge_x = -0.2
      ) +
      geom_text(
        data = right_label,
        aes(label = cluster_name),
        size = 3,
        check_overlap = FALSE,
        nudge_x = 0.2
      ) +
      geom_label(
        data = scale_values,
        aes(x = 1, y = value, label = value),
        color = "black",
        size = 3,
        nudge_x = -0.5
      ) +
      geom_label(
        data = scale_values,
        aes(x = 2, y = value, label = value),
        color = "black",
        size = 3,
        nudge_x = 0.5
      ) +
      th1.1 +
      th3 +
      th4 +
      th7

    ladder_scale <- rvg::dml(ggobj = ladder_scale) # convert to an editable object for pptx

    ft_ladder_scale <- flextable(data = clu_mean_value_ladder) %>%
      width(j = 1, width = 1) %>%
      width(j = 2, width = 1.5) %>%
      width(j = 3, width = 2) %>%
      width(j = 4, width = 2) %>%
      fontsize(size = 10, part = "header") %>%
      fontsize(size = 10, part = "body") %>%
      border_remove() %>%
      theme_vanilla()

    ft_ladder_left <- flextable(data = detail_vert) %>%
      width(j = 1, width = 2) %>%
      fontsize(size = 8, part = "header") %>%
      fontsize(size = 8, part = "body") %>%
      border_remove() %>%
      set_header_labels(vertical_left = "Left") %>%
      theme_vanilla()

    ft_ladder_right <- flextable(data = detail_horiz) %>%
      width(j = 1, width = 2) %>%
      fontsize(size = 8, part = "header") %>%
      fontsize(size = 8, part = "body") %>%
      border_remove() %>%
      set_header_labels(horizontal_right = "Right") %>%
      theme_vanilla()

    # . . rescale ladder &  flextables -----
    # set ladder scale with min and max
    rescale_min <- 0
    rescale_max <- 1
    rescale_values <- as.data.frame(rbind(rescale_min, rescale_max))
    colnames(rescale_values)[1] <- "value" # needed for aes geom_text for labeling laddder axis
    rescale_values$measure <- rownames(rescale_values)
    rescale_values$group <- seq(1, 2)
    rescale_values$group <- factor(scale_values$group)
    rescale_values$measure <- factor(scale_values$measure)

    # rescale ladder data with min max equal to scale values rescale left and right to the same metric
    clu_mean_value_left$rescale_left <- scales::rescale(clu_mean_value_left$cluster_mean_left, to = c(0, 1))
    clu_mean_value_right$rescale_right <- scales::rescale(clu_mean_value_right$cluster_mean_right, to = c(0, 1))

    # copy df to new name before dropping variables
    clu_mean_rescale_left <- clu_mean_value_left
    clu_mean_rescale_right <- clu_mean_value_right

    # drop the raw data mean before melting to long form
    clu_mean_rescale_left$cluster_mean_left <- NULL
    clu_mean_rescale_right$cluster_mean_right <- NULL

    # rename cluster column with common name in order to join df
    colnames(clu_mean_rescale_left)[1] <- "cluster"
    colnames(clu_mean_rescale_right)[1] <- "cluster"

    # join
    left_right_rescale <- full_join(x = clu_mean_rescale_left, y = clu_mean_rescale_right, by = "cluster")
    colnames(clu_text)[1] <- "cluster" # rename column for join
    clu_mean_rescale_ladder <- full_join(clu_text, left_right_rescale)
    colnames(clu_mean_rescale_ladder)[2] <- "cluster_name"

    # melt to long form
    ladder_recale_stack <- reshape2::melt(clu_mean_rescale_ladder,
      id.vars = c("cluster", "cluster_name"),
      value.name = "value"
    )

    colnames(ladder_recale_stack)[colnames(ladder_recale_stack) == "variable"] <- "measure"
    ladder_recale_stack$group <- factor(ladder_recale_stack$cluster)
    ladder_recale_stack$measure <- factor(ladder_recale_stack$measure)

    # get cluster labels for plot, subset left and right values
    left_rescale_label <- ladder_recale_stack %>% dplyr::filter(measure == "rescale_left")
    right_rescale_label <- ladder_recale_stack %>% dplyr::filter(measure == "rescale_right")

    rescale_ladder_title <- paste(ladder_title, "- rescaled means", sep = " ")

    # plot resccaled ladder
    ladder_rescale <- ggplot2::ggplot(data = ladder_recale_stack, aes(x = measure, y = value, group = group, colour = group)) +
      geom_line(linetype = "solid", size = 1) +
      geom_point(size = 3) +
      scale_y_continuous(
        name = NULL,
        limits = c(rescale_min, rescale_max),
        breaks = seq(rescale_min, rescale_max, 0.25),
        expand = expansion(add = 0.25),
        sec.axis = dup_axis()
      ) +
      scale_x_discrete(name = "Measure comparisons") +
      ggtitle(rescale_ladder_title) +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_color_manual(values = ladder_color) +
      # add cluster labels to ladder
      geom_text(
        data = left_rescale_label,
        aes(label = cluster_name),
        size = 3,
        check_overlap = FALSE,
        nudge_x = -0.2
      ) +
      geom_text(
        data = right_rescale_label,
        aes(label = cluster_name),
        size = 3,
        check_overlap = FALSE,
        nudge_x = 0.2
      ) +
      geom_label(
        data = rescale_values,
        color = "black",
        size = 3,
        aes(x = 1, y = value, label = value),
        nudge_x = -0.5
      ) +
      geom_label(
        data = rescale_values,
        color = "black",
        size = 3,
        aes(x = 2, y = value, label = value),
        nudge_x = 0.5
      ) +
      th1.1 +
      th3 +
      th4 +
      th7

    ladder_rescale <- rvg::dml(ggobj = ladder_rescale) # convert to an editable object for pptx

    ft_ladder_rescale <- flextable(data = clu_mean_rescale_ladder) %>%
      width(j = 1, width = 1) %>%
      width(j = 2, width = 1.5) %>%
      width(j = 3, width = 2) %>%
      width(j = 4, width = 2) %>%
      fontsize(size = 12, part = "header") %>%
      fontsize(size = 10, part = "body") %>%
      border_remove() %>%
      theme_vanilla()

    # . . create gozones -----
    # scale max min
    if (measure_choice_left == input_values_def[8, 2]) {
      left_min <- input_values_def[9, 2]
      left_max <- input_values_def[10, 2]
    } else if (measure_choice_left == input_values_def[12, 2]) {
      left_min <- input_values_def[13, 2]
      left_max <- input_values_def[14, 2]
    }

    if (measure_choice_right == input_values_def[8, 2]) {
      right_min <- input_values_def[9, 2]
      right_max <- input_values_def[10, 2]
    } else if (measure_choice_right == input_values_def[12, 2]) {
      right_min <- input_values_def[13, 2]
      right_max <- input_values_def[14, 2]
    }

    left_min <- as.numeric(left_min)
    left_max <- as.numeric(left_max)
    right_min <- as.numeric(right_min)
    right_max <- as.numeric(right_max)

    # . . create pptx -----
    # footer msg for ladders
    msg_subset_observations_left_ladder <- paste("Left side ladder",
      "N = ", subset_observations_left,
      sep = " "
    ) # message is ftr in pptx

    msg_subset_observations_right_ladder <- paste("Right side ladder",
      "N =", subset_observations_right,
      sep = " "
    ) # message is ftr in pptx

    corr_ladder_msg <- paste("spearman correlation =", corr_ladder)

    msg_subset_observations_ladder <- paste(corr_ladder_msg,
      msg_subset_observations_left_ladder,
      msg_subset_observations_right_ladder,
      sep = "\n"
    )

    # footer msg for quad plots
    msg_subset_observations_vert_quad <- paste("Vertical axis", "N =", subset_observations_left, sep = " ") # message is ftr in pptx
    msg_subset_observations_horiz_quad <- paste("Horizontal axis", "N =", subset_observations_right, sep = " ") # message is ftr in pptx
    msg_subset_observations_quad <- paste(msg_subset_observations_vert_quad,
      msg_subset_observations_horiz_quad,
      sep = "\n"
    )

    pm_doc <- read_pptx()
    # slide, directions
    add_slide(pm_doc, layout = "Title and Content", master = "Office Theme") %>%
      ph_with(ph_location_type(type = "title"),
        value = "Comparing values"
      ) %>%
      ph_with(ph_location_type(type = "body"),
        value = (unordered_list(
          level_list = c(1, 1, 1, 1, 1, 1),
          style = fp_text(color = "black", font.size = 12),
          str_list = c(
            "This slide deck compares values based on choice of measure(s) and demographic subsets for chosen cluster solution.",
            "The first ladder is based on the mean of the raw score for rating values and axes are set at the min and max of the mean values.",
            "In some cases, measures may be perceived differently such as when feasibility is uniformly rated lower than importance or when one demographic group uses the rating scale differently than the comparison group. The second ladder rescales the data from zero to one in order to compare both sides on common scale and better illustrate alignment and difference in values.",
            "Go-zones or quadrant plots compare items within a cluster based on choice of measure(s) and demographic subsets.",
            "If you present or publish your work, please use the following information to cite this software.",
            "McLinden, D. (year). IdeaNet - Open Source software in R for concept mapping. Retrieved from https://github.com/ideanetwork/ideanetR"
          ) # end str_list
        ) # end level_list
        ) # end unordered_list
      ) # end value=

    add_slide(pm_doc, layout = "Blank", master = "Office Theme") %>%
      ph_with(ladder_scale, location = ladder_loc) %>%
      ph_with(value = ft_ladder_left, location = ft_ladder_left_loc1) %>%
      ph_with(value = ft_ladder_right, location = ft_ladder_right_loc1) %>%
      ph_with(value = msg_subset_observations_ladder, location = ph_location_type(type = "ftr"))

    add_slide(x = pm_doc, layout = "Blank", master = "Office Theme") %>%
      ph_with(ft_ladder_scale, location = ft_ladder_scale_loc) %>%
      ph_with(value = ft_ladder_left, location = ft_ladder_left_loc2) %>%
      ph_with(value = ft_ladder_right, location = ft_ladder_right_loc2) %>%
      ph_with(value = msg_subset_observations_ladder, location = ph_location_type(type = "ftr"))

    add_slide(x = pm_doc, layout = "Blank", master = "Office Theme") %>%
      ph_with(ladder_rescale, location = ladder_rescale_loc) %>%
      ph_with(value = ft_ladder_left, location = ft_ladder_left_loc1) %>%
      ph_with(value = ft_ladder_right, location = ft_ladder_right_loc1) %>%
      ph_with(value = msg_subset_observations_ladder, location = ph_location_type(type = "ftr"))

    add_slide(x = pm_doc, layout = "Blank", master = "Office Theme") %>%
      ph_with(ft_ladder_rescale, location = ft_ladder_scale_loc) %>%
      ph_with(value = ft_ladder_left, location = ft_ladder_left_loc2) %>%
      ph_with(value = ft_ladder_right, location = ft_ladder_right_loc2) %>%
      ph_with(value = msg_subset_observations_ladder, location = ph_location_type(type = "ftr"))


    # . . loop for gozone for each cluster in a chosen cluster solution pptx -----
    count_clusters <- 1

    for (count_clusters in seq(1, clus_chosen, 1)) {
      cluster_n <- (subset(output_result, output_result$clutemp == count_clusters)) # select a subset of item ID numbers based on a persons factor index number

      quad_df <- subset(cluster_n, select = c(item, item_text, item_means_left, item_means_right))

      # compute reference lines
      left_mean <- mean(cluster_n$item_means_left) # left/vertical
      right_mean <- mean(cluster_n$item_means_right) # right/horizontal

      left_mean <- signif(left_mean, 4)
      right_mean <- signif(right_mean, 4)

      # compute the correlation gozone
      corr <- cor(cluster_n$item_means_left, cluster_n$item_means_right, method = "spearman")
      corr <- format(round(corr, 2))
      corr_msg <- paste("spearman correlation =", corr)

      quad_title <- paste("Cluster", count_clusters, "of", clus_chosen, "clusters", sep = " ")
      vert_left_mean <- paste("mean =", left_mean, sep = " ")
      horiz_right_mean <- paste("mean =", right_mean, sep = " ")

      gozone_plot <- ggplot2::ggplot(cluster_n, aes(x = item_means_right, y = item_means_left)) +
        annotate("rect", xmin = right_mean, xmax = Inf, ymin = left_mean, ymax = Inf, fill = "green") +
        annotate("rect", xmin = -Inf, xmax = right_mean, ymin = -Inf, ymax = left_mean, fill = "red") +
        annotate("rect", xmin = -Inf, xmax = right_mean, ymin = left_mean, ymax = Inf, fill = "yellow") + # top left
        annotate("rect", xmin = right_mean, xmax = Inf, ymin = -Inf, ymax = left_mean, fill = "yellow") + # bottom right
        scale_x_continuous(limits = c(right_min, right_max), breaks = waiver()) +
        scale_y_continuous(limits = c(left_min, left_max), breaks = waiver()) +
        geom_point() +
        xlab(right_horiz_label) +
        ylab(left_vert_label) +
        labs(title = quad_title, caption = corr_msg) +
        geom_text(aes(label = item), hjust = 1, vjust = 1.5, size = 3) +
        geom_text(aes(x = left_max, y = left_mean, label = vert_left_mean, vjust = -0.5), ) +
        geom_text(aes(
          x = right_mean, y = right_max, label = horiz_right_mean,
          vjust = -0.25, hjust = 0.5
        )) +
        theme(axis.line = element_line(color = "black", size = 0.5))

      gozone_plot <- rvg::dml(ggobj = gozone_plot) # convert to an editable object for pptx

      # create flextable using quad_df
      gozone_flex <- flextable(data = quad_df) %>%
        align(align = "left", part = "all") %>%
        fontsize(size = 12, part = "header") %>%
        fontsize(size = 10, part = "body") %>%
        border_remove() %>%
        width(j = 2, width = 5) %>%
        width(j = 1, width = 1) %>%
        width(j = 3, width = 1) %>%
        width(j = 4, width = 1) %>%
        set_header_labels(
          item = "Item", item_text = quad_title,
          item_means_left = left_vert_label, item_means_right = right_horiz_label
        ) %>%
        theme_vanilla()

      gozone_detail_vert <- flextable(data = detail_vert) %>%
        align(align = "left", part = "all") %>%
        fontsize(size = 9, part = "header") %>%
        fontsize(size = 9, part = "body") %>%
        border_remove() %>%
        width(j = 1, width = 3) %>%
        set_header_labels(vertical_left = "vertical") %>%
        theme_vanilla()

      gozone_detail_horiz <- flextable(data = detail_horiz) %>%
        align(align = "left", part = "all") %>%
        fontsize(size = 9, part = "header") %>%
        fontsize(size = 9, part = "body") %>%
        border_remove() %>%
        width(j = 1, width = 3) %>%
        set_header_labels(horizontal_right = "horizontal") %>%
        theme_vanilla()

      add_slide(pm_doc) %>%
        ph_with(
          value = gozone_plot,
          location = ph_location_template(
            left = 4, top = 0.25,
            height = 4, width = 4
          )
        ) %>%
        ph_with(
          value = gozone_detail_vert,
          location = ph_location_template(
            left = .5, top = 0.5,
            height = 4, width = 4
          )
        ) %>%
        ph_with(
          value = gozone_detail_horiz,
          location = ph_location_template(
            left = 4, top = 4.5,
            height = 4, width = 5
          )
        ) %>%
        ph_with(value = msg_subset_observations_quad, location = ph_location_type(type = "ftr"))

      add_slide(pm_doc) %>%
        ph_with(pm_doc,
          value = gozone_flex,
          location = ph_location_template(left = 1, top = 1)
        ) %>%
        ph_with(value = msg_subset_observations_quad, location = ph_location_type(type = "ftr"))
    } # end loop for gozone plots and flextables in create gozone quadrants

    output_wd() # save to working dir for output.xlsx

    print(pm_doc, target = "pattern_match.pptx") %>%
      invisible()

    pattern_match_complete() # pop up msg
  } # end compute_pattern_match function to create ladder and go zone graphs


  # open measure and demographic choice frames
  ladder_subset_frames <- function() {

    # . . open three right top demographic frames -----
    # to choose demographics and subset data for left side ladder
    choosing_demographics1 <- function(x, y, var_count, demo_frame) {

      # start if variable is continuous
      if (input_values_def[x, 3] == "continuous") {
        type <- "continuous"
        # select button function to record user input for continuous variable and subset data
        select.cont.demo <- function() {
          continuous.1 <- tcltk::tclvalue(choose_subset1) # first boolean operator
          continuous.2 <- tcltk::tclvalue(choose_subset2) # second boolean operator
          subset1_chosen <- as.numeric(tcltk::tclvalue(subset.value1)) # first value for boolean operator
          subset2_chosen <- as.numeric(tcltk::tclvalue(subset.value2)) # second value for boolean operator

          # validate data
          choice_mismatch <- function() {
            msg_choice_mismatch <- paste(demo_name, "-", "If first choice is ALL then second choice must be NONE.", sep = " ")
            tcltk::tk_messageBox(type = "ok", message = msg_choice_mismatch) # if first choice is ALL then second choice must be NONE
          }
          choice_missing_value <- function() {
            msg_missing_value <- paste(demo_name, "-", "A value must be entered in the field below the operator chosen.", sep = " ")
            tcltk::tk_messageBox(type = "ok", message = msg_missing_value) # choosing an operator but missing the value for subset
          }
          choice_duplicate_operators <- function() {
            msg_duplicate_operators <- paste(demo_name, "-", "The choices are not valid. The operators are the same. Operators need to be different to create a range of values.")
            tcltk::tk_messageBox(type = "ok", message = msg_duplicate_operators) # if the operators are the same then the second is redundant
          }
          choice_invalid_range <- function() {
            msg_invalid_range <- paste(demo_name, "-", "The values for the operators are the same.The values need to be different to create a range of values.")
            tcltk::tk_messageBox(type = "ok", message = msg_invalid_range) # if the operators differ but the values are the same then there is no range
          }
          choice_invalid_value <- function() {
            msg_invalid_value <- paste(demo_name, "-", "If the operator is ALL or NONE, the value in the field needs to be blank.")
            tcltk::tk_messageBox(type = "ok", message = msg_invalid_value) # if the operators differ but the values are the same then there is no range
          }

          # if subset input from canvas choices are valid then subset data
          cont_choice_ok <- function() {

            # if subset_data_left df exists from prior subset then continue subsetting that df else create df from input
            if (exists("subset_data_left") == "TRUE") {
              subset_data_left <- subset_data_left
            } else
            if (exists("subset_data_left") == "FALSE") subset_data_left <- subset_data

            # if first option is any  choice and second option is NONE
            one_subset_option <- function() {
              subset_data_left <- if (continuous.1 == "ALL") {
                subset_data_left
              } else
              if (continuous.1 == "GT") {
                dplyr::filter(subset_data_left, subset_data_left[, x] > subset1_chosen)
              } else
              if (continuous.1 == "GE") {
                dplyr::filter(subset_data_left, subset_data_left[, x] >= subset1_chosen)
              } else
              if (continuous.1 == "EQ") {
                dplyr::filter(subset_data_left, subset_data_left[, x] == subset1_chosen)
              } else
              if (continuous.1 == "LT") {
                dplyr::filter(subset_data_left, subset_data_left[, x] < subset1_chosen)
              } else
              if (continuous.1 == "LE") dplyr::filter(subset_data_left, subset_data_left[, x] <= subset1_chosen)

              assign("subset_data_left", subset_data_left, envir = .GlobalEnv)

              tcltk::tkdestroy(select.cont.demo.but) # destroy button so no further choices can be made, user needs to user reset button to start over
            }

            # first option is not ALL and second option is not NONE
            two_subset_option <- function() {
              # get the first group
              first_group <- if (continuous.1 == "GT") {
                dplyr::filter(subset_data_left, subset_data_left[, x] > subset1_chosen)
              } else
              if (continuous.1 == "GE") {
                dplyr::filter(subset_data_left, subset_data_left[, x] >= subset1_chosen)
              } else
              if (continuous.1 == "EQ") {
                dplyr::filter(subset_data_left, subset_data_left[, x] == subset1_chosen)
              } else
              if (continuous.1 == "LT") {
                dplyr::filter(subset_data_left, subset_data_left[, x] < subset1_chosen)
              } else
              if (continuous.1 == "LE") dplyr::filter(subset_data_left, subset_data_left[, x] <= subset1_chosen)

              # get the second group
              second_group <- if (continuous.2 == "GT") {
                dplyr::filter(subset_data_left, subset_data_left[, x] > subset2_chosen)
              } else
              if (continuous.2 == "GE") {
                dplyr::filter(subset_data_left, subset_data_left[, x] >= subset2_chosen)
              } else
              if (continuous.2 == "EQ") {
                dplyr::filter(subset_data_left, subset_data_left[, x] == subset2_chosen)
              } else
              if (continuous.2 == "LT") {
                dplyr::filter(subset_data_left, subset_data_left[, x] < subset2_chosen)
              } else
              if (continuous.2 == "LE") dplyr::filter(subset_data_left, subset_data_left[, x] <= subset2_chosen)

              # combine the two datasets with dplyr/tidy
              subset_data_left <- bind_rows(first_group, second_group)

              assign("subset_data_left", subset_data_left, envir = .GlobalEnv)

              # destroy button so no further choices can be made, user needs to user reset button to start over
              tcltk::tkdestroy(select.cont.demo.but)
            } # end function to subset data with two choices

            if (continuous.2 == "NONE") one_subset_option()
            if (continuous.2 != "NONE") two_subset_option()

            msg_c0 <- "Data has been subsetted."
            msg_c1 <- paste("Type of variable", type, sep = " ")
            msg_c2 <- paste("Variable name", demo_name, sep = " ")
            msg_c3 <- paste("Variable range from", demo_min, "to", demo_max, sep = " ")
            msg_c4 <- paste("First choice", continuous.1, subset1_chosen, sep = " ")
            msg_c5 <- paste("Second choice", continuous.2, subset2_chosen, sep = " ")
            msg_c6 <- "----------"
            msg_c7 <- "Continue with next variable or if complete, click compute."

            # create msg to add to flextable and output to pptx
            msg_choices1 <- paste("Variable:", demo_name, sep = " ")
            msg_choices2 <- paste("1st choice:", continuous.1, "Range:", subset1_chosen, sep = " ")
            msg_choices3 <- paste("2nd choice:", continuous.2, "Range:", subset2_chosen, sep = " ")
            msg_choices <- paste(msg_choices1, msg_choices2, msg_choices3, sep = "\n")
            demo_count <- x - 1 # set value of index to 1 - 3
            subset_msg_left[demo_count, 1] <- msg_choices
            assign("subset_msg_left", subset_msg_left, envir = .GlobalEnv)

            continuous_choices <- paste(msg_c0, msg_c1, msg_c2, msg_c3, msg_c4,
              msg_c5, msg_c6, msg_c7,
              sep = "\n"
            )
            tcltk::tk_messageBox(type = "ok", message = continuous_choices)
          } # end choice_ok function to subset data

          # call validate data functions if choices not valid else call function to subset data if choices OK
          if (continuous.1 != "ALL" & is.na(subset1_chosen) == "TRUE") {
            choice_missing_value()
          } else if (continuous.1 == "ALL" & is.na(subset1_chosen) == "FALSE") {
            choice_invalid_value()
          } else if (continuous.2 == "NONE" & is.na(subset2_chosen) == "FALSE") {
            choice_invalid_value
          } else if (continuous.1 != "ALL" & is.na(subset1_chosen) == "FALSE" & continuous.2 != "NONE" & is.na(subset2_chosen) == "TRUE") {
            choice_missing_value()
          } else if (continuous.1 == "ALL" & continuous.2 != "NONE") {
            choice_mismatch()
          } else if (continuous.1 == continuous.2) {
            choice_duplicate_operators()
          } else {
            cont_choice_ok()
          }
        } # end select_cont.demo button function to get user input for continuous demographic variable and subset data

        # create widgets for continuous variable
        demo_name <- input_values_def[x, 2]
        demo_min <- input_values_def[x, 4]
        demo_max <- input_values_def[x, 5]
        demo_msg1 <- paste("Min=", demo_min, sep = "")
        demo_msg2 <- paste("Max=", demo_max, sep = "")
        demo_msg <- paste(var_count, demo_name, demo_msg1, demo_msg2, sep = "\n")

        subset1_var <- c("ALL", "GT", "GE", "EQ", "LT", "LE", "NE")
        choose_subset1 <- tcltk::tclVar("ALL") # set default to all
        combo.demo_cont1 <- tcltk::ttkcombobox(demo_frame,
          width = 25, values = subset1_var,
          textvariable = choose_subset1, state = "readonly"
        )
        subset2_var <- c("NONE", "GT", "GE", "EQ", "LT", "LE", "NE")
        choose_subset2 <- tcltk::tclVar("NONE")
        combo.demo_cont2 <- tcltk::ttkcombobox(demo_frame,
          width = 25, values = subset2_var,
          textvariable = choose_subset2, state = "readonly"
        )

        subset.value1 <- tcltk::tclVar("")
        entry.tbValue1 <- tcltk::tkentry(demo_frame, width = "10", bg = "LightGrey", textvariable = subset.value1)
        subset.value2 <- tcltk::tclVar("")
        entry.tbValue2 <- tcltk::tkentry(demo_frame, width = "10", bg = "LightGrey", textvariable = subset.value2)
        select.cont.demo.but <- tcltk::tkbutton(demo_frame,
          text = "Select",
          command = select.cont.demo
        ) # get input data for continuous variable

        # position widgets for continuous variable
        tcltk::tkgrid(tcltk2::tk2message(demo_frame,
          bg = "aliceblue", text = demo_msg,
          pady = 3, width = 300
        ), sticky = "w")
        tcltk::tkgrid(tcltk::tklabel(demo_frame,
          text = "Select ALL or select a subset of data",
          pady = 3, bg = "aliceblue"
        ), sticky = "w")
        tcltk::tkgrid(combo.demo_cont1, sticky = "w")
        tcltk::tkgrid(tcltk::tklabel(demo_frame,
          text = "Enter a value if choosing a subset", bg = "aliceblue",
          pady = 3
        ), sticky = "w")
        tcltk::tkgrid(entry.tbValue1, sticky = "w")
        tcltk::tkgrid(tcltk::tklabel(demo_frame,
          text = "Select a second criteria to subset data", bg = "aliceblue",
          pady = 3
        ), sticky = "w")
        tcltk::tkgrid(combo.demo_cont2, pady = 3, sticky = "w")
        tcltk::tkgrid(tcltk::tklabel(demo_frame, text = "Enter a value if choosing a subset", bg = "aliceblue", pady = 3),
          sticky = "w"
        )
        tcltk::tkgrid(entry.tbValue2, sticky = "w")
        tcltk::tkgrid(select.cont.demo.but, pady = 3)
        # end if continuous variable
      } else {

        # start if categorical variable
        if (input_values_def[x, 3] == "categorical") {
          type <- "categorical"

          # selection button for categorical variable
          select.cat.demo <- function() {
            cat_choice <- choices[as.numeric(tcltk::tkcurselection(demo_cat_lb)) + 1] # number choice in selection choices

            # data not valid message functions called
            all_cat_sub <- function() {
              msg_all_cat_sub <- paste(demo_name, "-", "The value of ALL and a value for a subset were both chosen.  Deselect ALL to subset or deselect other options to use ALL.")
              tcltk::tk_messageBox(type = "ok", message = msg_all_cat_sub) # if the operators differ but the values are the same then there is no range
            }
            missing_cat <- function() {
              msg_missing_cat <- paste(demo_name, "-", "No choices were made. Select ALL or choose to subset data.")
              tcltk::tk_messageBox(type = "ok", message = msg_missing_cat) # if the operators differ but the values are the same then there is no range
            }
            choose_all_cat <- function() {
              msg_choose_all_cat <- paste(demo_name, "-", "All subset values where chosen. Deselect and choose ALL instead.")
              tcltk::tk_messageBox(type = "ok", message = msg_choose_all_cat) # if the operators differ but the values are the same then there is no range
            }

            # data valid
            cat_choice_ok <- function() {
              # if subset_data_left df exists from prior subset then continue subsetting that df else create df from input
              if (exists("subset_data_left") == "TRUE") {
                subset_data_left <- subset_data_left
              } else
              if (exists("subset_data_left") == "FALSE") subset_data_left <- subset_data

              # subset the data
              if (cat_choice == "ALL") {
                subset_data_left <- subset_data_left
              } else
              if (cat_choice != "ALL") subset_data_left <- dplyr::filter(subset_data_left, subset_data_left[, y] %in% cat_choice)

              # message to user about choices
              collapse_demo_levels <- paste(demo_levels, collapse = " ") # convert char vectors to single element for messages, otherwise messages repeat for length of vector
              collapse_cat_choice <- paste(cat_choice, collapse = " ")
              msg_c0 <- "Data has been subsetted."
              msg_c1 <- paste("Type of variable", type, sep = " ")
              msg_c2 <- paste("Variable name", demo_name, sep = " ")
              msg_c3 <- paste("Number of levels", demo_n_levels, sep = " ")
              msg_c4 <- paste("Possible choices", collapse_demo_levels, sep = " ")
              msg_c5 <- paste("Choice made", collapse_cat_choice, sep = " ")
              msg_c6 <- "----------"
              msg_c7 <- "Continue with next variable or if complete, click compute."

              # create msg to add to flextable and output to pptx
              msg_choices1 <- paste("Variable:", demo_name, sep = " ")
              msg_choices2 <- paste("Choice:", collapse_cat_choice, sep = " ")
              msg_choices <- paste(msg_choices1, msg_choices2, sep = "\n")
              demo_count <- x - 1 # set value of index to 1 - 3
              subset_msg_left[demo_count, 1] <- msg_choices
              assign("subset_msg_left", subset_msg_left, envir = .GlobalEnv)

              categorical_choices <- paste(msg_c0, msg_c1, msg_c2, msg_c3, msg_c4,
                msg_c5, msg_c6, msg_c7,
                sep = "\n"
              )
              tcltk::tk_messageBox(type = "ok", message = categorical_choices)

              assign("subset_data_left", subset_data_left, envir = .GlobalEnv)

              tcltk::tkdestroy(select.cat.demo.but) # destroy button so no further choices can be made, user needs to user reset button to start over
            } # end  the demo_cat_ok function

            # validate categorical choice
            if ((length(cat_choice) > 1) & ("ALL" %in% cat_choice == TRUE)) {
              all_cat_sub() # if choice includes ALL and one other choice then deselect something
            } else if (length(cat_choice) == 0) {
              missing_cat() # if no choices
            } else if (length(cat_choice) == demo_n_levels) {
              choose_all_cat() # if all choices are highlighted, select ALL and deselect others or deselect something
            } else {
              cat_choice_ok() # data is valid, call function to subset data
            } # end if then to validate data
          } # end selection button

          # get categorical values from input data
          demo_name <- input_values_def[x, 2]
          demo_var <- input_result[, y]
          demo_var <- as.data.frame(demo_var)
          demo_var$demo_var <- as.factor(demo_var$demo_var)
          demo_n_levels <- nlevels(demo_var$demo_var)
          demo_levels <- levels(demo_var$demo_var)
          demo_msg1 <- paste(demo_n_levels, " levels", sep = "")
          demo_msg <- paste(var_count, demo_name, demo_msg1, sep = "\n")

          # define widgets for demographics
          demo_cat_lb <- tcltk2::tk2listbox(demo_frame, height = 4, selectmode = "multiple") # demographics listbox allows multiple choices
          choices <- c("ALL", demo_levels)
          for (choice in choices) {
            tcltk::tkinsert(demo_cat_lb, "end", choice)
          }
          tcltk::tkselection.set(demo_cat_lb, 0) # Default is ALL; Indexing starts at zero.
          select.cat.demo.but <- tcltk::tkbutton(demo_frame,
            text = "Select",
            command = select.cat.demo
          ) # get input data for continuous variable

          # position widgets for demographics
          tcltk::tkgrid(tcltk2::tk2message(demo_frame, bg = "aliceblue", text = demo_msg, width = 300, pady = 3), sticky = "w")
          tcltk::tkgrid(tcltk::tklabel(demo_frame,
            text = "Select ALL or select a subset of data", bg = "aliceblue",
            pady = 3
          ), sticky = "w")
          tcltk::tkgrid(tcltk::tklabel(demo_frame,
            text = "Click to deselect or select a variable",
            pady = 3, bg = "aliceblue"
          ), sticky = "w")
          tcltk::tkgrid(tcltk::tklabel(demo_frame,
            text = "Multiple groups may be selected",
            pady = 3, bg = "aliceblue"
          ), sticky = "w")
          tcltk::tkgrid(demo_cat_lb) # , padx = 10, pady = c(5, 10))
          tcltk::tkgrid(select.cat.demo.but, pady = 3)
          # end if categorical variable
        } else {
          # start if no demographic variable
          # if subset_data_left df exists from prior subset then continue subsetting that df else create df from input
          if (exists("subset_data_left") == "TRUE") {
            subset_data_left <- subset_data_left
          } else if (exists("subset_data_left") == "FALSE") {
            subset_data_left <- subset_data
          } # end if else

          no_choices <- "No demographic variable"

          # create msg to add to flextable and output to pptx
          msg_choices <- paste("Variable:", no_choices, sep = " ")
          demo_count <- x - 1 # set value of index to 1 - 3
          subset_msg_left[demo_count, 1] <- msg_choices
          assign("subset_msg_left", subset_msg_left, envir = .GlobalEnv)
          demo_msg <- paste(var_count, no_choices, sep = "\n")

          tcltk::tkgrid(tcltk2::tk2message(demo_frame,
            bg = "aliceblue", text = demo_msg,
            pady = 3, width = 300
          ), sticky = "w")

          assign("subset_data_left", subset_data_left, envir = .GlobalEnv)
          # end if no demographic variable
        } # end last/second else
      } # end first else
    } # end choosing_demographics1 function

    # . . open three right bottom demographic frames -----
    # to choose demographics and subset data for right side ladder
    choosing_demographics2 <- function(x, y, var_count, demo_frame) {

      # start if variable is continuous
      if (input_values_def[x, 3] == "continuous") {
        type <- "continuous"
        # select button function to record user input for continuous variable and subset data
        select.cont.demo <- function() {
          continuous.1 <- tcltk::tclvalue(choose_subset1) # first boolean operator
          continuous.2 <- tcltk::tclvalue(choose_subset2) # second boolean operator
          subset1_chosen <- as.numeric(tcltk::tclvalue(subset.value1)) # first value for boolean operator
          subset2_chosen <- as.numeric(tcltk::tclvalue(subset.value2)) # second value for boolean operator

          # validate data
          choice_mismatch <- function() {
            msg_choice_mismatch <- paste(demo_name, "-", "If first choice is ALL then second choice must be NONE.", sep = " ")
            tcltk::tk_messageBox(type = "ok", message = msg_choice_mismatch) # if first choice is ALL then second choice must be NONE
          }
          choice_missing_value <- function() {
            msg_missing_value <- paste(demo_name, "-", "A value must be entered in the field below the operator chosen.", sep = " ")
            tcltk::tk_messageBox(type = "ok", message = msg_missing_value) # choosing an operator but missing the value for subset
          }
          choice_duplicate_operators <- function() {
            msg_duplicate_operators <- paste(demo_name, "-", "The choices are not valid. The operators are the same. Operators need to be different to create a range of values.")
            tcltk::tk_messageBox(type = "ok", message = msg_duplicate_operators) # if the operators are the same then the second is redundant
          }
          choice_invalid_range <- function() {
            msg_invalid_range <- paste(demo_name, "-", "The values for the operators are the same.The values need to be different to create a range of values.")
            tcltk::tk_messageBox(type = "ok", message = msg_invalid_range) # if the operators differ but the values are the same then there is no range
          }
          choice_invalid_value <- function() {
            msg_invalid_value <- paste(demo_name, "-", "If the operator is ALL or NONE, the value in the field needs to be blank.")
            tcltk::tk_messageBox(type = "ok", message = msg_invalid_value) # if the operators differ but the values are the same then there is no range
          }

          # if subset input from canvas choices are valid then subset data
          cont_choice_ok <- function() {

            # if subset_data_right df exists from prior subset then continue subsetting that df else create df from input
            if (exists("subset_data_right") == "TRUE") {
              subset_data_right <- subset_data_right
            } else
            if (exists("subset_data_right") == "FALSE") subset_data_right <- subset_data

            # if first option is any  choice and second option is NONE
            one_subset_option <- function() {
              subset_data_right <- if (continuous.1 == "ALL") {
                subset_data_right
              } else
              if (continuous.1 == "GT") {
                dplyr::filter(subset_data_right, subset_data_right[, x] > subset1_chosen)
              } else
              if (continuous.1 == "GE") {
                dplyr::filter(subset_data_right, subset_data_right[, x] >= subset1_chosen)
              } else
              if (continuous.1 == "EQ") {
                dplyr::filter(subset_data_right, subset_data_right[, x] == subset1_chosen)
              } else
              if (continuous.1 == "LT") {
                dplyr::filter(subset_data_right, subset_data_right[, x] < subset1_chosen)
              } else
              if (continuous.1 == "LE") dplyr::filter(subset_data_right, subset_data_right[, x] <= subset1_chosen)

              assign("subset_data_right", subset_data_right, envir = .GlobalEnv)

              tcltk::tkdestroy(select.cont.demo.but) # destroy button so no further choices can be made, user needs to user reset button to start over
            }

            # first option is not ALL and second option is not NONE
            two_subset_option <- function() {
              # get the first group
              first_group <- if (continuous.1 == "GT") {
                dplyr::filter(subset_data_right, subset_data_right[, x] > subset1_chosen)
              } else
              if (continuous.1 == "GE") {
                dplyr::filter(subset_data_right, subset_data_right[, x] >= subset1_chosen)
              } else
              if (continuous.1 == "EQ") {
                dplyr::filter(subset_data_right, subset_data_right[, x] == subset1_chosen)
              } else
              if (continuous.1 == "LT") {
                dplyr::filter(subset_data_right, subset_data_right[, x] < subset1_chosen)
              } else
              if (continuous.1 == "LE") dplyr::filter(subset_data_right, subset_data_right[, x] <= subset1_chosen)

              # get the second group
              second_group <- if (continuous.2 == "GT") {
                dplyr::filter(subset_data_right, subset_data_right[, x] > subset2_chosen)
              } else
              if (continuous.2 == "GE") {
                dplyr::filter(subset_data_right, subset_data_right[, x] >= subset2_chosen)
              } else
              if (continuous.2 == "EQ") {
                dplyr::filter(subset_data_right, subset_data_right[, x] == subset2_chosen)
              } else
              if (continuous.2 == "LT") {
                dplyr::filter(subset_data_right, subset_data_right[, x] < subset2_chosen)
              } else
              if (continuous.2 == "LE") dplyr::filter(subset_data_right, subset_data_right[, x] <= subset2_chosen)

              # combine the two datasets with dplyr/tidy
              subset_data_right <- bind_rows(first_group, second_group)

              assign("subset_data_right", subset_data_right, envir = .GlobalEnv)

              # destroy button so no further choices can be made, user needs to user reset button to start over
              tcltk::tkdestroy(select.cont.demo.but)
            } # end function to subset data with two choices

            if (continuous.2 == "NONE") one_subset_option()
            if (continuous.2 != "NONE") two_subset_option()

            msg_c0 <- "Data has been subsetted."
            msg_c1 <- paste("Type of variable", type, sep = " ")
            msg_c2 <- paste("Variable name", demo_name, sep = " ")
            msg_c3 <- paste("Variable range from", demo_min, "to", demo_max, sep = " ")
            msg_c4 <- paste("First choice", continuous.1, subset1_chosen, sep = " ")
            msg_c5 <- paste("Second choice", continuous.2, subset2_chosen, sep = " ")
            msg_c6 <- "----------"
            msg_c7 <- "Continue with next variable or if complete, click compute."

            # create msg to add to flextable and output to pptx
            msg_choices1 <- paste("Variable:", demo_name, sep = " ")
            msg_choices2 <- paste("1st choice :", continuous.1, "Range:", subset1_chosen, sep = " ")
            msg_choices3 <- paste("2nd choice", continuous.2, "Range:", subset2_chosen, sep = " ")
            msg_choices <- paste(msg_choices1, msg_choices2, msg_choices3, sep = "\n")
            demo_count <- x - 1 # set value of index to 1 - 3
            subset_msg_right[demo_count, 1] <- msg_choices
            assign("subset_msg_right", subset_msg_right, envir = .GlobalEnv)

            continuous_choices <- paste(msg_c0, msg_c1, msg_c2, msg_c3, msg_c4,
              msg_c5, msg_c6, msg_c7,
              sep = "\n"
            )
            tcltk::tk_messageBox(type = "ok", message = continuous_choices)
          } # end choice_ok function to subset data

          # call validate data functions if choices not valid else call function to subset data if choices OK
          if (continuous.1 != "ALL" & is.na(subset1_chosen) == "TRUE") {
            choice_missing_value()
          } else if (continuous.1 == "ALL" & is.na(subset1_chosen) == "FALSE") {
            choice_invalid_value()
          } else if (continuous.2 == "NONE" & is.na(subset2_chosen) == "FALSE") {
            choice_invalid_value
          } else if (continuous.1 != "ALL" & is.na(subset1_chosen) == "FALSE" & continuous.2 != "NONE" & is.na(subset2_chosen) == "TRUE") {
            choice_missing_value()
          } else if (continuous.1 == "ALL" & continuous.2 != "NONE") {
            choice_mismatch()
          } else if (continuous.1 == continuous.2) {
            choice_duplicate_operators()
          } else {
            cont_choice_ok()
          }
        } # end select_cont.demo button function to get user input for continuous demographic variable and subset data

        # create widgets for continuous variable
        demo_name <- input_values_def[x, 2]
        demo_min <- input_values_def[x, 4]
        demo_max <- input_values_def[x, 5]
        demo_msg1 <- paste("Min=", demo_min, sep = "")
        demo_msg2 <- paste("Max=", demo_max, sep = "")
        demo_msg <- paste(var_count, demo_name, demo_msg1, demo_msg2, sep = "\n")

        subset1_var <- c("ALL", "GT", "GE", "EQ", "LT", "LE", "NE")
        choose_subset1 <- tcltk::tclVar("ALL") # set default to all
        combo.demo_cont1 <- tcltk::ttkcombobox(demo_frame,
          width = 25, values = subset1_var,
          textvariable = choose_subset1, state = "readonly"
        )
        subset2_var <- c("NONE", "GT", "GE", "EQ", "LT", "LE", "NE")
        choose_subset2 <- tcltk::tclVar("NONE")
        combo.demo_cont2 <- tcltk::ttkcombobox(demo_frame,
          width = 25, values = subset2_var,
          textvariable = choose_subset2, state = "readonly"
        )

        subset.value1 <- tcltk::tclVar("")
        entry.tbValue1 <- tcltk::tkentry(demo_frame, width = "10", bg = "LightGrey", textvariable = subset.value1)
        subset.value2 <- tcltk::tclVar("")
        entry.tbValue2 <- tcltk::tkentry(demo_frame, width = "10", bg = "LightGrey", textvariable = subset.value2)
        select.cont.demo.but <- tcltk::tkbutton(demo_frame,
          text = "Select",
          command = select.cont.demo
        ) # get input data for continuous variable

        # position widgets for continuous variable
        tcltk::tkgrid(tcltk2::tk2message(demo_frame,
          bg = "aliceblue", text = demo_msg,
          pady = 3, width = 300
        ), sticky = "w")
        tcltk::tkgrid(tcltk::tklabel(demo_frame,
          text = "Select ALL or select a subset of data",
          pady = 3, bg = "aliceblue"
        ), sticky = "w")
        tcltk::tkgrid(combo.demo_cont1, sticky = "w")
        tcltk::tkgrid(tcltk::tklabel(demo_frame,
          text = "Enter a value if choosing a subset", bg = "aliceblue",
          pady = 3
        ), sticky = "w")
        tcltk::tkgrid(entry.tbValue1, sticky = "w")
        tcltk::tkgrid(tcltk::tklabel(demo_frame,
          text = "Select a second criteria to subset data", bg = "aliceblue",
          pady = 3
        ), sticky = "w")
        tcltk::tkgrid(combo.demo_cont2, pady = 3, sticky = "w")
        tcltk::tkgrid(tcltk::tklabel(demo_frame, text = "Enter a value if choosing a subset", bg = "aliceblue", pady = 3),
          sticky = "w"
        )
        tcltk::tkgrid(entry.tbValue2, sticky = "w")
        tcltk::tkgrid(select.cont.demo.but, pady = 3)
        # end if continuous variable
      } else {

        # start if categorical variable
        if (input_values_def[x, 3] == "categorical") {
          type <- "categorical"

          # selection button for categorical variable
          select.cat.demo <- function() {
            cat_choice <- choices[as.numeric(tcltk::tkcurselection(demo_cat_lb)) + 1] # number choice in selection choices

            # data not valid message functions called
            all_cat_sub <- function() {
              msg_all_cat_sub <- paste(demo_name, "-", "The value of ALL and a value for a subset were both chosen.  Deselect ALL to subset or deselect other options to use ALL.")
              tcltk::tk_messageBox(type = "ok", message = msg_all_cat_sub) # if the operators differ but the values are the same then there is no range
            }
            missing_cat <- function() {
              msg_missing_cat <- paste(demo_name, "-", "No choices were made. Select ALL or choose to subset data.")
              tcltk::tk_messageBox(type = "ok", message = msg_missing_cat) # if the operators differ but the values are the same then there is no range
            }
            choose_all_cat <- function() {
              msg_choose_all_cat <- paste(demo_name, "-", "All subset values where chosen. Deselect and choose ALL instead.")
              tcltk::tk_messageBox(type = "ok", message = msg_choose_all_cat) # if the operators differ but the values are the same then there is no range
            }

            # data valid
            cat_choice_ok <- function() {
              # if subset_data_right df exists from prior subset then continue subsetting that df else create df from input
              if (exists("subset_data_right") == "TRUE") {
                subset_data_right <- subset_data_right
              } else
              if (exists("subset_data_right") == "FALSE") subset_data_right <- subset_data

              # subset the data
              if (cat_choice == "ALL") {
                subset_data_right <- subset_data_right
              } else
              if (cat_choice != "ALL") subset_data_right <- dplyr::filter(subset_data_right, subset_data_right[, y] %in% cat_choice)

              # message to user about choices
              collapse_demo_levels <- paste(demo_levels, collapse = " ") # convert char vectors to single element for messages, otherwise messages repeat for length of vector
              collapse_cat_choice <- paste(cat_choice, collapse = " ")
              msg_c0 <- "Data has been subsetted."
              msg_c1 <- paste("Type of variable", type, sep = " ")
              msg_c2 <- paste("Variable name", demo_name, sep = " ")
              msg_c3 <- paste("Number of levels", demo_n_levels, sep = " ")
              msg_c4 <- paste("Possible choices", collapse_demo_levels, sep = " ")
              msg_c5 <- paste("Choice made", collapse_cat_choice, sep = " ")
              msg_c6 <- "----------"
              msg_c7 <- "Continue with next variable or if complete, click compute."

              # create msg to add to flextable and output to pptx
              msg_choices1 <- paste("Variable:", demo_name, sep = " ")
              msg_choices2 <- paste("Choice:", collapse_cat_choice, sep = " ")
              msg_choices <- paste(msg_choices1, msg_choices2, sep = "\n")
              demo_count <- x - 1 # set value of index to 1 - 3
              subset_msg_right[demo_count, 1] <- msg_choices
              assign("subset_msg_right", subset_msg_right, envir = .GlobalEnv)

              categorical_choices <- paste(msg_c0, msg_c1, msg_c2, msg_c3, msg_c4,
                msg_c5, msg_c6, msg_c7,
                sep = "\n"
              )
              tcltk::tk_messageBox(type = "ok", message = categorical_choices)

              assign("subset_data_right", subset_data_right, envir = .GlobalEnv)

              tcltk::tkdestroy(select.cat.demo.but) # destroy button so no further choices can be made, user needs to user reset button to start over
            } # end  the demo_cat_ok function

            # validate categorical choice
            if ((length(cat_choice) > 1) & ("ALL" %in% cat_choice == TRUE)) {
              all_cat_sub() # if choice includes ALL and one other choice then deselect something
            } else if (length(cat_choice) == 0) {
              missing_cat() # if no choices
            } else if (length(cat_choice) == demo_n_levels) {
              choose_all_cat() # if all choices are highlighted, select ALL and deselect others or deselect something
            } else {
              cat_choice_ok() # data is valid, call function to subset data
            } # end if then to validate data
          } # end selection button

          # get categorical values from input data
          demo_name <- input_values_def[x, 2]
          demo_var <- input_result[, y]
          demo_var <- as.data.frame(demo_var)
          demo_var$demo_var <- as.factor(demo_var$demo_var)
          demo_n_levels <- nlevels(demo_var$demo_var)
          demo_levels <- levels(demo_var$demo_var)
          demo_msg1 <- paste(demo_n_levels, " levels", sep = "")
          demo_msg <- paste(var_count, demo_name, demo_msg1, sep = "\n")

          # define widgets for demographics
          demo_cat_lb <- tcltk2::tk2listbox(demo_frame, height = 4, selectmode = "multiple") # demographics listbox allows multiple choices
          choices <- c("ALL", demo_levels)
          for (choice in choices) {
            tcltk::tkinsert(demo_cat_lb, "end", choice)
          }
          tcltk::tkselection.set(demo_cat_lb, 0) # Default is ALL; Indexing starts at zero.
          select.cat.demo.but <- tcltk::tkbutton(demo_frame,
            text = "Select",
            command = select.cat.demo
          ) # get input data for continuous variable

          # position widgets for demographics
          tcltk::tkgrid(tcltk2::tk2message(demo_frame, bg = "aliceblue", text = demo_msg, width = 300, pady = 3), sticky = "w")
          tcltk::tkgrid(tcltk::tklabel(demo_frame,
            text = "Select ALL or select a subset of data", bg = "aliceblue",
            pady = 3
          ), sticky = "w")
          tcltk::tkgrid(tcltk::tklabel(demo_frame,
            text = "Click to deselect or select a variable",
            pady = 3, bg = "aliceblue"
          ), sticky = "w")
          tcltk::tkgrid(tcltk::tklabel(demo_frame,
            text = "Multiple groups may be selected",
            pady = 3, bg = "aliceblue"
          ), sticky = "w")
          tcltk::tkgrid(demo_cat_lb) # , padx = 10, pady = c(5, 10))
          tcltk::tkgrid(select.cat.demo.but, pady = 3)
          # end if categorical variable
        } else {
          # start if no demographic variable
          # if subset_data_right df exists from prior subset then continue subsetting that df else create df from input
          if (exists("subset_data_right") == "TRUE") {
            subset_data_right <- subset_data_right
          } else
          if (exists("subset_data_right") == "FALSE") subset_data_right <- subset_data

          no_choices <- "No demographic variable"

          # create msg to add to flextable and output to pptx
          msg_choices <- paste("Variable:", no_choices, sep = " ")
          demo_count <- x - 1 # set value of index to 1 - 3
          subset_msg_right[demo_count, 1] <- msg_choices
          assign("subset_msg_right", subset_msg_right, envir = .GlobalEnv)
          demo_msg <- paste(var_count, no_choices, sep = "\n")

          tcltk::tkgrid(tcltk2::tk2message(demo_frame,
            bg = "aliceblue", text = demo_msg,
            pady = 3, width = 300
          ), sticky = "w")

          assign("subset_data_right", subset_data_right, envir = .GlobalEnv)
          # end if no demographic variable
        } # end last/second else
      } # end first else
    } # end choosing_demographics2 function

    # loops through input data looking for demographic variables passing information to choosing_demographics()
    input_demographics <- function() {
      # right side ladder top frame choose demographic subsets
      for (z in seq(2, 4, 1)) {
        x <- z
        y <- z

        if (z == 2) {
          var_count <- "First demographic variable "
          demo_frame <- lpw.6
        } else if (z == 3) {
          var_count <- "Second demographic variable "
          demo_frame <- lpw.7
        } else if (z == 4) {
          var_count <- "Third demographic variable "
          demo_frame <- lpw.8
        } # end last else

        choosing_demographics1(x, y, var_count, demo_frame) # call function to build frames to select demographics
      } # end loop

      # left side ladder bottom frame choose demographic subsets
      for (z in seq(2, 4, 1)) {
        x <- z
        y <- z

        if (z == 2) {
          var_count <- "First demographic variable "
          demo_frame <- lpw.11
        } else if (z == 3) {
          var_count <- "Second demographic variable "
          demo_frame <- lpw.12
        } else if (z == 4) {
          var_count <- "Third demographic variable "
          demo_frame <- lpw.13
        } # end last else

        choosing_demographics2(x, y, var_count, demo_frame) # call function to build frames to select demographics
      } # end loop
    } # end input_demographics function

    meas_choice <- function() {

      # choose a measure
      if (input_values_def[7, 2] == 1) {
        measures <- input_values_def[8, 2]
      } else {
        if (input_values_def[7, 2] == 2) {
          measures <- c(input_values_def[8, 2], input_values_def[12, 2])
        }
      } # end if else

      # left side ladder or vertical quadrant plot
      select_meas_left <- function() {
        measure_choice_left <- tcltk::tclvalue(choose_measure_left)
        assign("measure_choice_left", measure_choice_left, envir = .GlobalEnv)
        tcltk::tkdestroy(button.widget_meas_left)
      } # end  select_meas1 function

      first_measure_left <- measures[1] # populate measures drop down with default first measure
      label.measure_left <- tcltk::tklabel(lpw.5, text = "Select a measure")
      choose_measure_left <- tcltk::tclVar(first_measure_left)
      assign("choose_measure_left", choose_measure_left, envir = .GlobalEnv)
      combo.measure_left <- tcltk::ttkcombobox(lpw.5,
        width = 25, values = measures,
        textvariable = choose_measure_left, state = "readonly"
      )

      # select button for measure 1 left/vertical
      button.widget_meas_left <- tcltk::tkbutton(lpw.5,
        text = "Save selected data",
        command = select_meas_left
      )
      tcltk::tkgrid(tcltk2::tk2message(lpw.5,
        bg = "aliceblue", justify = "center", width = 300, pady = 3, font = fontSub,
        text = "Select measure/rating"
      ))
      tcltk::tkgrid(tcltk2::tk2message(lpw.5,
        bg = "aliceblue", justify = "left", width = 300, pady = 3, font = fontQ,
        text = "Choose one measure from dropdown list"
      ))
      tcltk::tkgrid(combo.measure_left, pady = 3)
      tcltk::tkgrid(button.widget_meas_left)

      # right side ladder horizontal quadrant graph
      select_meas_right <- function() {
        measure_choice_right <- tcltk::tclvalue(choose_measure_right)
        assign("measure_choice_right", measure_choice_right, envir = .GlobalEnv)
        tcltk::tkdestroy(button.widget_meas_right)
      } # end  select_meas2 function

      first_measure_right <- measures[1] # populate measures dropdown with default first measure
      label.measure_right <- tcltk::tklabel(lpw.10, text = "Select a measure")
      choose_measure_right <- tcltk::tclVar(first_measure_right)
      assign("choose_measure_right", choose_measure_right, envir = .GlobalEnv)
      combo.measure_right <- tcltk::ttkcombobox(lpw.10,
        width = 25, values = measures,
        textvariable = choose_measure_right, state = "readonly"
      )

      # select button for measure2 right/horizontal
      button.widget_meas_right <- tcltk::tkbutton(lpw.10,
        text = "Save selected data",
        command = select_meas_right
      )
      tcltk::tkgrid(tcltk2::tk2message(lpw.10,
        bg = "aliceblue", justify = "center", width = 300, pady = 3, font = fontSub,
        text = "Select measure/rating"
      ))
      tcltk::tkgrid(tcltk2::tk2message(lpw.10,
        bg = "aliceblue", justify = "left", width = 300, pady = 3, font = fontQ,
        text = "Choose one measure from dropdown list"
      ))
      tcltk::tkgrid(combo.measure_right, pady = 3)
      tcltk::tkgrid(button.widget_meas_right)
    } # end meas_choice function

    # button to save user input and call function to compute ladder and gozone
    compute.ladder.but <- function() {
      compute.ladder.but <- tcltk::tkbutton(lpw.3, text = "Compute pattern match", command = compute_pattern_match)
      tcltk::tkpack(compute.ladder.but, side = "left", pady = 10)
      assign("compute.ladder.but", compute.ladder.but, envir = .GlobalEnv)
    } # end compute.ladder.but

    # call functions within ladder_subset_frames function create right side frame content
    meas_choice()
    input_demographics()
    compute.ladder.but()
  } # end ladder_subset_frames function

  # open bottom half of first frame to choose cluster solution and measure and call input_demographics
  clus_choice <- function() {
    # if the data files are chosen then add cluster choice
    # choose a cluster solution widget
    select_clus <- function() {
      # cluster solution chosen
      clus_chosen <- as.numeric(tcltk::tclvalue(tbValue))
      clus_chosen <- as.integer(clus_chosen)
      if ((clus_chosen %in% seq.int(from = 5, to = 15, by = 1)) == FALSE) invalid_value() # validate input  value for cluster chosen
      assign("clus_chosen", clus_chosen, envir = .GlobalEnv)

      # if clus and measure chosen then open demo frames
      if (exists("clus_chosen") == TRUE) {
        tcltk::tkdestroy(button.widget_clus)
        subset_data <- input_result # subset the data here in case there are no demographics
        assign("subset_data", subset_data, envir = .GlobalEnv)
        ladder_subset_frames() # call function to populate right side frames for measures and demographics
      } else if (exists("clus_chosen") == FALSE) {
        tcltk::tk_messageBox(type = "ok", message = "Select a cluster solution.")
      } # end if else
    } # end function select_clus function

    # select number of clusters widget
    tbValue <- tcltk::tclVar("")
    entry.tbValue <- tcltk::tkentry(lpw.1, width = "10", bg = "LightGrey", textvariable = tbValue)

    button.widget_clus <- tcltk::tkbutton(lpw.1,
      text = "Save selected data",
      command = select_clus
    )

    tcltk::tkgrid(tcltk2::tk2message(lpw.1,
      bg = "aliceblue", justify = "left", width = 300, pady = 3, font = fontSub,
      text = "Select cluster solution"
    ))
    tcltk::tkgrid(tcltk2::tk2message(lpw.1,
      bg = "aliceblue", pady = 3, justify = "left", width = 300, font = fontQ,
      text = "Choose cluster solution: Enter a value between 5 and 15 inclusive."
    ))
    tcltk::tkgrid(entry.tbValue, pady = 3)
    tcltk::tkgrid(tcltk2::tk2message(lpw.1,
      bg = "aliceblue", justify = "center", width = 300, pady = 3, font = fontQ,
      text = "----------"
    ))

    tcltk::tkgrid(button.widget_clus)
    tcltk::tkgrid(tcltk2::tk2message(lpw.1,
      bg = "aliceblue", justify = "left", width = 300, pady = 3, font = fontQ,
      text = "Click to save selected cluster."
    ))

    # dimension an empty data frame to hold messages used to summarize data choices
    # which are input from choice in demographics and then output to layer map as flextables
    subset_msg_left <- (as.data.frame(matrix(nrow = 3, ncol = 1)))
    subset_msg_left[[1]] <- as.character(subset_msg_left[[1]])
    assign("subset_msg_left", subset_msg_left, envir = .GlobalEnv)

    subset_msg_right <- (as.data.frame(matrix(nrow = 3, ncol = 1)))
    subset_msg_right[[1]] <- as.character(subset_msg_right[[1]])
    assign("subset_msg_right", subset_msg_right, envir = .GlobalEnv)
  } # end clu_choice function


  # . . create the canvas for cluster rating/layer map -----
    # define canvas for pattern analysis
    lpw.1 <- tcltk::tkframe(tt, bg = "aliceblue") # left
    lpw.2 <- tcltk::tkframe(tt, bg = "white") # is white to visually separate multiple panels in aliceblue

    lpw.3 <- tcltk::tkframe(lpw.2, bg = "white", height = 20) # bottom of right for reset and compute button
    lpw.4 <- tcltk::tkframe(lpw.2, bg = "white") # top of right for multiple panels, 4 top and 4 bottom panels
    lpw.5 <- tcltk::tkframe(lpw.4, bg = "aliceblue") # top 4 panels on right side for choosing subset1
    lpw.6 <- tcltk::tkframe(lpw.4, bg = "aliceblue") # top 4 panels on right side for choosing subset1
    lpw.7 <- tcltk::tkframe(lpw.4, bg = "aliceblue") # top 4 panels on right side forightr choosing subset1
    lpw.8 <- tcltk::tkframe(lpw.4, bg = "aliceblue") # top 4 panels on right side for choosing subset1

    lpw.9 <- tcltk::tkframe(lpw.2, bg = "white") # bottom  frame for four panels for subset2
    lpw.10 <- tcltk::tkframe(lpw.9, bg = "aliceblue") # bottom 4 panels to choose subset2
    lpw.11 <- tcltk::tkframe(lpw.9, bg = "aliceblue") # bottom 4 panels to choose subset2
    lpw.12 <- tcltk::tkframe(lpw.9, bg = "aliceblue") # bottom 4 panels to choose subset2
    lpw.13 <- tcltk::tkframe(lpw.9, bg = "aliceblue") # bottom 4 panels to choose subset2

    reset.but <- tcltk::tkbutton(lpw.3, text = "Reset", command = reset_canvas)

    left_msg <- (tcltk2::tk2message(lpw.4,
      bg = "white", justify = "left", width = 200, font = fontQ.s,
      text = "left side of ladder/vertical axis of go-zone"
    ))
    right_msg <- (tcltk2::tk2message(lpw.9,
      bg = "white", justify = "left", width = 300, font = fontQ.s,
      text = "right side of ladder/horizonatal axis of go-zone"
    ))

    tcltk::tkpack(tcltk::tkframe(lpw.4), left_msg, side = "top")
    tcltk::tkpack(tcltk::tkframe(lpw.9), right_msg, side = "top")
    tcltk::tkpack(lpw.1, lpw.2, side = "left", padx = 3, expand = TRUE, fill = "both") # left and right
    tcltk::tkpack(lpw.3, side = "bottom", padx = 3, expand = FALSE, fill = "both") # bottom right for buttons
    tcltk::tkpack(lpw.4, lpw.9, side = "top", padx = 3, pady = 3, expand = TRUE, fill = "both") # top and bottome on right for panels
    tcltk::tkpack(lpw.5, lpw.6, lpw.7, lpw.8, side = "left", padx = 3, expand = TRUE, fill = "both") # right side top panel
    tcltk::tkpack(lpw.10, lpw.11, lpw.12, lpw.13, side = "left", padx = 3, pady = 5, expand = TRUE, fill = "both") # right side bottom panels
    tcltk::tkpack(reset.but, side = "left", padx = 100, pady = 10)

    assign("lpw.1", lpw.1, envir = .GlobalEnv)
    assign("lpw.2", lpw.2, envir = .GlobalEnv)
    assign("lpw.3", lpw.3, envir = .GlobalEnv)
    assign("lpw.4", lpw.4, envir = .GlobalEnv)
    assign("lpw.5", lpw.5, envir = .GlobalEnv)
    assign("lpw.6", lpw.6, envir = .GlobalEnv)
    assign("lpw.7", lpw.7, envir = .GlobalEnv)
    assign("lpw.8", lpw.8, envir = .GlobalEnv)
    assign("lpw.9", lpw.9, envir = .GlobalEnv)
    assign("lpw.10", lpw.10, envir = .GlobalEnv)
    assign("lpw.11", lpw.11, envir = .GlobalEnv)
    assign("lpw.12", lpw.12, envir = .GlobalEnv)
    assign("lpw.13", lpw.13, envir = .GlobalEnv)

    assign("reset.but", reset.but, envir = .GlobalEnv)

    # open top half of first frame to select data files and then call clus_choice()
      # define widgets
      # select data files widgets
      button.widget_input <- tcltk::tkbutton(lpw.1, text = "Select input data file", command = get_input)
      button.widget_output <- tcltk::tkbutton(lpw.1,
                                              justify = "left",
                                              text = "Select output data file", command = get_output
      )

      # after selecting data file, button will read in data for measure choices
      select_data_files <- function() {
        if (exists("output_result") == TRUE & exists("input_result") == TRUE) {
          # destroy button in selecting data file for analyze values to avoid clicking twice in error
          if (exists("button.widget_input")) tcltk::tkdestroy(button.widget_input)
          if (exists("button.widget_output")) tcltk::tkdestroy(button.widget_output)
          tcltk::tkdestroy(button.widget_data)
          clus_choice() # call function to open bottom half of frame to select cluster solution and measure
        } else if (exists("output_result") == FALSE | exists("input_result") == FALSE) {
          tcltk::tk_messageBox(
            type = "ok",
            message = "Missing one or both data files. Select input data file with ratings and and measure definition and output data file with cluster membership."
          )
        }
      } # end select_data_files

      button.widget_data <- tcltk::tkbutton(lpw.1, text = "Save selected data", command = select_data_files)

      # layout for data selection left frame
      tcltk::tkgrid(tcltk2::tk2message(lpw.1,
                                       bg = "aliceblue", justify = "left", width = 300, pady = 3, font = fontSub,
                                       text = "Load data"
      ))
      tcltk::tkgrid(tcltk2::tk2message(lpw.1,
                                       bg = "aliceblue", justify = "left", width = 300, pady = 3, font = fontQ,
                                       text = "Each pattern analysis requires two files, input.xlsx containing rating data & output.xlsx containing cluster membership.*"
      ))
      tcltk::tkgrid(tcltk2::tk2message(lpw.1,
                                       bg = "aliceblue", justify = "left", width = 300, pady = 3, font = fontQ,
                                       text = "If you have renamed the excel files, choose the files that correspond to input.xlxs and output.xlsx respectively."
      ))
      tcltk::tkgrid(tcltk2::tk2message(lpw.1,
                                       bg = "aliceblue",
                                       text = "*NOTE: This analysis assumes REVIEW DATA step has been completed and a map has been computed.",
                                       pady = 15, width = 300, font = fontQ
      ))
      tcltk::tkgrid(button.widget_input, pady = 3)
      tcltk::tkgrid(button.widget_output, pady = 3)
      tcltk::tkgrid(button.widget_data, pady = 3)
      tcltk::tkgrid(tcltk2::tk2message(lpw.1,
                                       bg = "aliceblue", justify = "center", width = 300, pady = 3, font = fontQ,
                                       text = "----------"
      ))
} # end pattern_matching function

# ********************************************************************************************** -----
# mds validity -----
# valid_mds<-function(){
#   tcltk::tk_messageBox(type="ok",message="Validity analysis for the map of points is currently under construction")
# }
# ********************************************************************************************** -----
# cluster validity -----
valid_cluster <- function() {

  # clears frames from canvas when a new menu item is chosen at top level menu
  reset_canvas <- function() {
    if (exists("lpw.1")) tcltk::tkdestroy(lpw.1)
    if (exists("lpw.2")) tcltk::tkdestroy(lpw.2)
    if (exists("lpw.3")) tcltk::tkdestroy(lpw.3)
    if (exists("lpw.4")) tcltk::tkdestroy(lpw.4)
    if (exists("lpw.5")) tcltk::tkdestroy(lpw.5)
    if (exists("lpw.6")) tcltk::tkdestroy(lpw.6)
    if (exists("lpw.7")) tcltk::tkdestroy(lpw.7)
    if (exists("lpw.8")) tcltk::tkdestroy(lpw.8)
    if (exists("lpw.9")) tcltk::tkdestroy(lpw.9)
    if (exists("lpw.10")) tcltk::tkdestroy(lpw.10)
    if (exists("lpw.11")) tcltk::tkdestroy(lpw.11)
    if (exists("lpw.12")) tcltk::tkdestroy(lpw.12)
    if (exists("lpw.13")) tcltk::tkdestroy(lpw.13)
    if (exists("compute.layer.but")) tcltk::tkdestroy(compute.layer.but)
    if (exists("reset.but")) tcltk::tkdestroy(reset.but)
  } # end reset_canvas

  reset_canvas()

  sil_analyis <- function(clus_chosen) {

    # check for silhouette.xlsx & delete file if it exists
    f <- "silhouette.xlsx"
    if (file.exists(f)) file.remove(f)

    # cluster solution 15 starts in column 3,
    cluster_col <- if (clus_chosen == 15) {
      3
    } else if (clus_chosen == 14) {
      4
    } else if (clus_chosen == 13) {
      5
    } else if (clus_chosen == 12) {
      6
    } else if (clus_chosen == 11) {
      7
    } else if (clus_chosen == 10) {
      8
    } else if (clus_chosen == 9) {
      9
    } else if (clus_chosen == 8) {
      10
    } else if (clus_chosen == 7) {
      11
    } else if (clus_chosen == 6) {
      12
    } else if (clus_chosen == 5) {
      13
    }
    sil_item <- output_result %>% dplyr::select(item, item_text)
    sil_dim <- output_result %>% dplyr::select(dim1, dim2)
    sil_k <- as.integer(output_result[, cluster_col]) # convert from factor to integer vector
    clus_dist <- dist(sil_dim, method = "euclidean", diag = T) # recreate the dissimilarity matrix from coordinates, recreate by reading in output.xlsx, avoids saving the dist matrix to xlsx if it can simply recreated

    clus_sil <- silhouette(sil_k, clus_dist) # silhouette analysis

    # create df of silhouette results
    current_cluster <- as.data.frame(clus_sil[, 1])
    neighbor <- as.data.frame(clus_sil[, 2])
    sil_width <- as.data.frame(clus_sil[, 3])
    comb_sil <- cbind(current_cluster, neighbor, sil_width)
    colnames(comb_sil) <- c("current_cluster", "neighbor", "sil_width")
    comb_sil <- cbind(comb_sil, sil_item) # add item and item text to results

    # create excel workbook and write results
    silhouette_analysis <- createWorkbook()
    addWorksheet(silhouette_analysis, "Directions")
    addWorksheet(silhouette_analysis, "sil_results")

    # instructions
    df_directions <- (as.data.frame(matrix(nrow = 15, ncol = 1)))
    names(df_directions) <- "Directions"
    df_directions[1, 1] <- "Once a cluster solution is chosen, in some cases, a review of the map may evoke assertions that a given point may be better placed in a nearby cluster instead of the cluster where the point is currently located. The analysis done here will validate cluster membership.  Once run, silhouette.xlsx will be output."
    df_directions[2, 1] <- "If the sil_width value is high positive, the best fit is in the current cluster."
    df_directions[3, 1] <- "If the sil_width value is near zero, the item is best left in the current cluster but has a strong affinity for items in the neigboring cluster and is a bridging item."
    df_directions[4, 1] <- "If sil_width value is negative, then the map may be improved by placing the item in neighboring cluster."
    df_directions[5, 1] <- "If changing the cluster membership of an item(s), manually make changes to the PowerPoint file and the output Exel file."
    df_directions[6, 1] <- "In PowerPoint, do not move the item, make the change in cluster membership by redrawing the cluster boundary to incorporate item(s) being added."
    df_directions[7, 1] <- "In the output exel file, locate the item that is a candidate to be moved, change the value from the current cluster membership to nearest neighboring cluster as indicated by silhouette.xlsx."
    df_directions[8, 1] <- "There should be minimal changes to cluster membership from the original analysis, possibly one or several, if any. If the analysis suggests changing cluster membership for many items, then considering a different cluster solution may be a better option."
    df_directions[9, 1] <- "If you move items to neighboring clusters rerun COMPUTE CLUSTER REPORT, and if it applies to your data, do the same for PATTERN ANALYSIS, and PATTERN MATCHING."

    # styles
    style_wrap <- createStyle(fontSize = 10, fontName = "Arial", wrapText = TRUE)

    # add data to worksheets
    # write directions
    setColWidths(silhouette_analysis, "Directions", 1, widths = 75)
    addStyle(silhouette_analysis, "Directions", style_wrap, cols = 1, rows = 1:15)
    writeData(silhouette_analysis, "Directions", df_directions,
      startCol = 1,
      startRow = 1, colNames = TRUE, rowNames = FALSE
    )
    writeData(silhouette_analysis, "sil_results", comb_sil,
      startCol = 1, startRow = 1, colNames = TRUE, rowNames = FALSE
    )

    saveWorkbook(silhouette_analysis, "silhouette.xlsx", overwrite = TRUE)

    tcltk::tk_messageBox(type = "ok", message = "silhouette.xlsx has been saved.")

    reset_canvas()
  } # end sil_analyis function

  reset_canvas() # clear canvas

    lpw.1 <- tcltk::tkframe(tt, bg = "aliceblue")
    lpw.2 <- tcltk::tkframe(tt, bg = "aliceblue")
    tcltk::tkpack(lpw.1, lpw.2, side = "left", padx = 3, expand = TRUE, fill = "both")
    reset.but <- tcltk::tkbutton(lpw.1, text = "Reset", command = reset_canvas)
    button.widget_output <- tcltk::tkbutton(lpw.1, justify = "left", text = "Select output data file", command = get_output) # get data file

    assign("lpw.1", lpw.1, envir = .GlobalEnv)
    assign("lpw.2", lpw.2, envir = .GlobalEnv)

    # left side
    # select output.xlsx
    tcltk::tkgrid(tcltk2::tk2message(lpw.1, text = " ", bg = "aliceblue", justify = "left"))
    tcltk::tkgrid(tcltk2::tk2message(lpw.1, text = " ", bg = "aliceblue", justify = "left"))
    tcltk::tkgrid(tcltk2::tk2message(lpw.1, text = " ", bg = "aliceblue", justify = "left"))
    tcltk::tkgrid(tcltk2::tk2message(lpw.1, bg = "aliceblue", justify = "center", width = 500, text = "Click button and select output.xlsx cluster data file created from computing maps"))
    tcltk::tkgrid(button.widget_output)

    # input field for cluster selection
    tcltk::tkgrid(tcltk2::tk2message(lpw.1, bg = "aliceblue", text = " "))
    tcltk::tkgrid(tcltk2::tk2message(lpw.1, bg = "aliceblue", width = 400, text = "Enter cluster number between 5 and 15 inclusive and click SUBMIT."))
    tbValue <- tcltk::tclVar("")
    entry.tbValue <- tcltk::tkentry(lpw.1, width = "10", bg = "LightGrey", textvariable = tbValue)
    tcltk::tkgrid(entry.tbValue)

    submit_A <- function() { # function to submit cluster number to create cluster report
      clus_chosen <- as.numeric(tcltk::tclvalue(tbValue))
      clus_chosen <- as.integer(clus_chosen)
      if ((clus_chosen %in% seq.int(from = 5, to = 15, by = 1)) == FALSE) invalid_value() #   validate input  value for cluster chosen
      assign("clus_chosen", clus_chosen, envir = .GlobalEnv)
      sil_analyis(clus_chosen)
    } # end submit_A

    submit.but_A <- tcltk::tkbutton(lpw.1, text = "Compute cluster validity ", command = submit_A) # select cluster

    tcltk::tkgrid(submit.but_A)
    tcltk::tkgrid(tcltk2::tk2message(lpw.1, text = " ", bg = "aliceblue", justify = "left"))
    tcltk::tkgrid(tcltk2::tk2message(lpw.1, text = " ", bg = "aliceblue", justify = "left"))
    tcltk::tkgrid(tcltk2::tk2message(lpw.1, text = " ", bg = "aliceblue", justify = "left"))
    tcltk::tkgrid(tcltk2::tk2message(lpw.1, text = " ", bg = "aliceblue", justify = "left"))
    tcltk::tkgrid(tcltk2::tk2message(lpw.1, text = " ", bg = "aliceblue", justify = "left"))
    tcltk::tkgrid(tcltk2::tk2message(lpw.1, text = " ", bg = "aliceblue", justify = "left"))
    tcltk::tkgrid(reset.but)

    # right side of canvas
    cl_msg1 <- "Results will be saved as silhouette.xlsx in same directory as output data file."
    cl_msg2 <- "Once a cluster solution is chosen, in some cases, a review of the map may evoke assertions that a given point may be better placed in a nearby cluster instead of the cluster where the point is currently located. The analysis done here will validate cluster membership.  Once run, silhouette.xlsx will be output."
    cl_msg3 <- "The value of sil_width has a range of +1 to -1. If the sil_width value is high positive, the best fit is in the current cluster. If the sil_width value is near zero, the item is best left in the current cluster but has a strong affinity for items in the neigboring cluster and is a bridging item. If sil_width value is negative, then the map may be improved by placing the item in neighboring cluster. The neighboring cluster is listed in the silhouette.xlsx."
    cl_msg4 <- "If changing the cluster membership of an item(s), manually make changes to the output Excel file created from the compute maps menu option (output.xlsx). In the output excel file, locate the item that to be moved, change the value from the current cluster membership to nearest neighboring cluster as indicated by silhouette.xlsx. After you move items to neighboring clusters rerun COMPUTE CLUSTER REPORT, and if it applies to your data, do the same for PATTERN ANALYSIS and PATTERN MATCHING."
    cl_msg5 <- "There should be minimal changes to cluster membership from the original analysis, possibly one or several, if any. If the analysis suggests changing cluster membership for many items, then considering a different cluster solution may be a better option."
    tcltk::tkgrid(tcltk2::tk2message(lpw.2, text = "", bg = "aliceblue", width = 500))
    tcltk::tkgrid(tcltk2::tk2message(lpw.2, text = "", bg = "aliceblue", width = 500))
    tcltk::tkgrid(tcltk2::tk2message(lpw.2, text = cl_msg1, bg = "aliceblue", width = 500, justify = "left"))
    tcltk::tkgrid(tcltk2::tk2message(lpw.2, text = "", bg = "aliceblue", width = 500))
    tcltk::tkgrid(tcltk2::tk2message(lpw.2, text = cl_msg2, bg = "aliceblue", width = 500, justify = "left"))
    tcltk::tkgrid(tcltk2::tk2message(lpw.2, text = "", bg = "aliceblue", width = 500))
    tcltk::tkgrid(tcltk2::tk2message(lpw.2, text = cl_msg3, bg = "aliceblue", width = 500, justify = "left")) # this is indenting
    tcltk::tkgrid(tcltk2::tk2message(lpw.2, text = "", bg = "aliceblue", width = 500))
    tcltk::tkgrid(tcltk2::tk2message(lpw.2, text = cl_msg4, bg = "aliceblue", width = 500, justify = "left"))
    tcltk::tkgrid(tcltk2::tk2message(lpw.2, text = "", bg = "aliceblue", width = 500))
    tcltk::tkgrid(tcltk2::tk2message(lpw.2, text = cl_msg5, bg = "aliceblue", width = 500, justify = "left"))

} # end valid_cluster function

# ********************************************************************************************** -----
# menu functions  ------

about_ideanet <- function() {
  blank_msg <- ""
  about_msg1 <- "Ideanet is software to support concept mapping, a method for crowdsourcing the understanding of and/or the design of interventions for any number of complex challenges. Unlike other group processes which rely on consensus; this process elicits the many and diverse viewpoints on an issue. The methodology relies on brainstorming, sorting, and rating to collect input from the group. Computation creates a visual representation, a map, of the group's thinking."
  about_msg2 <- "Learn more"
  about_msg3 <- "Visit the website: https://ideanetworks.io/"
  about_msg4 <- "There is a vast literature on Concept Mapping. The following articles provide an overview of the state of the art as well as references to the many applications of this methodology."
  about_msg5 <- "Trochim, W. M., & McLinden, D. (2017). Introduction to a special issue on concept mapping. Evaluation and Program Planning, 60, 166-175."
  about_msg6 <- "Trochim, W. M. K. (2017). Hindsight is 20/20: The Evolution of Concept Mapping Over the Past 25 Years. Evaluation and Program Planning, 176-185."
  about_msg7 <- "McLinden, D. (2017). And then the internet happened: Prospective Thoughts about Concept Mapping in the New Millennium. Evaluation and Program Planning 60: 293-300."
  about_msg <- paste(about_msg1, blank_msg, about_msg2, about_msg3, blank_msg, about_msg4, blank_msg, about_msg5, blank_msg,
    about_msg6, blank_msg, about_msg7,
    sep = "\n"
  )
  tcltk::tk_messageBox(type = "ok", message = about_msg)
}
ideanet_license <- function() {
  blank <- ""
  preamble1 <- "IDEANET"
  preamble2 <- "R script for Concept Mapping that computes and visualizes a network of ideas."
  preamble3 <- "The program distributed here is shared in the hope that this will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE."
  preamble4 <- "This work is licensed under the GNU General Public License v3.0 which was downloaded with this program or shared with you. If you do not have the license document, a link to the details of the license can be found at:"
  preamble5 <- "https://www.gnu.org/licenses/gpl-3.0.en.html"
  preamble6 <- "You are free to use  this program, share with others and make and share changes to the code. If you do make changes, the software license requires that the code and any ensuing modifications be made publicly available, allowing the entire community to benefit."
  preamble7 <- "Suggestions, questions, bug reports, code improvements - info@ideanetworks.io"
  license_msg <- paste(preamble1, blank, preamble2, blank, preamble3, blank,
    preamble4, preamble5, blank, preamble6, blank, preamble7,
    sep = "\n"
  )
  tcltk::tk_messageBox(type = "ok", message = license_msg)
}
ideanet_version <- function() {
  ver_msg0 <- ""
  ver_msg1 <- "Ideanet version 0.50."
  ver_msg2 <- "This is a development version. Creating additional functions and testing is still in process."
  ver_msg3 <- "Code was created and tested with R version 4.13"
  version_msg <- paste(ver_msg1, ver_msg0, ver_msg2, ver_msg0, ver_msg3, sep = "\n")
  tcltk::tk_messageBox(type = "ok", message = version_msg)
}
collect_sort_data <- function() {
  sort_blank <- ""
  sort_msg1 <- "Collecting sort data"
  sort_msg2 <- "Sort data - manual data collection: Assumes you can create a stack of cards with the text of one idea per card and each card contains an identification number and the text of the idea.  The numbers used for identifying ideas need be numbered sequentially beginning with the number 1 through to the maximum number of cards.  Creating the stack of cards for a manual sort can involve printing and then cutting sheets.  Do not print a sheet of all of the same items.  This will create the difficult task of sorting all those items into separate stacks, a tedious time-consuming task that is prone to error.  Do create a sheet(s) with the items from 1 to N and then cut and stack those items to create one stack.  A common misconception that causes unnecessary work is trying to create the stack so that cards are in order from 1 to N.  This is not necessary, you only need to be sure that each stack of cards has all of the ideas from 1 to N."
  sort_msg3 <- "Sort data - online data collection:  There are number of online options for sorting.  When adding items to an online program, be sure to add the item number as part of the text of the item (e.g., 1. Item one text)."
  sort_msg <- paste(sort_msg1, sort_blank, sort_msg2, sort_blank, sort_msg3, sep = "\n")
  tcltk::tk_messageBox(type = "ok", message = sort_msg)
}
enter_data <- function() {
  entry_blank <- ""
  entry_msg1 <- "Use the menu option to create a blank template for data entry."
  entry_msg2 <- "The Ideanet software is structured to read data from this template.To create a concept map, the template requires data in two worksheets."
  entry_msg3 <- "1. Sorting data must be added to either the racked or stacked worksheet."
  entry_msg4 <- "2. The text for ideas is entered in the ideas worksheet. Note, the text for each idea should include the number as part of the text of the item (e.g., 1. Item one text)."
  entry_msg5 <- "3(optionally).  If rating data is collected, enter this information on the values worksheet.  The template accomodates up to three demographics that can be continuous (e.g., age) or categorical (e.g., gender) and up to two measures (e.g., importance, feasibility)."
  entry_msg6 <- "Other worksheets will be populated with output from the analysis.  The directions worksheet also contains information about the workbook."
  entry_msg <- paste(entry_msg1, entry_blank, entry_msg2, entry_blank, entry_msg3, entry_blank,
    entry_msg4, entry_blank, entry_msg5, entry_blank,
    entry_msg6sep = "\n"
  )
  tcltk::tk_messageBox(type = "ok", message = entry_msg)
}
sort_data_structure <- function() {
  struct_blank <- ""
  struct_msg1 <- "RACKED - If data is entered manually is most likely in the racked format. The data are arranged horizontally.   Each row is a group of cards associated with a label and a person/sorter. Each person/sorter has multiple rows of data, one row for each sort category they create.   For ideanet program, data needs to be in an Excel worksheet. For each row columnA contains the identifier for the sorter, columnB contains the text label for the group of cards sorted together, and columnC and following columns in the row contain the identification number for the cards in a group with one card number in each cell."
  struct_msg2 <- "STACKED - If data is downloaded from an online sorting program it is most likely in the stacked format. The data are arranged vertically.  There may be many columns of data but only three columns of are needed; (1 )sorter identification, (2) group/category label for the category in whch the item was placed, and (3) item number associated with the category label for the sorter.  Typically, data from an online program can be downloaded and the relevant columns copied and pasted into the template."
  struct_msg <- paste(struct_msg1, struct_blank, struct_msg2, sep = "\n")
  tcltk::tk_messageBox(type = "ok", message = struct_msg)
}
compute_maps <- function() {
  comp_blank <- ""
  comp_msg1 <- "Ideanet program uses Scaling by MAjorizing a COmplicated Function (SMACOF) as the multidimensional scaling (MDS) algorithm to compute the location of points in two dimensions and computes stress assuming ordinal data. For more information see the following reference:"
  comp_msg2 <- "de Leeuw, J., & Mair, P. (2009). Multidimensional scaling using majorization: The R package smacof. Journal of Statistical Software, 31(3), 1-30."
  comp_msg3 <- "The map of items is partitioned with hierarchical cluster analysis using Ward's method."
  comp_msg4 <- "Computing a map will create three files"
  comp_msg5 <- "output.xlsx - contains worksheets with the stress value, cluster membership for 5 to 15 clusters, and top 5 labels for each cluster solution."
  comp_msg6 <- "output.pptx - is a slide deck illustrating cluster membership and top 5 labels for each cluster solution."
  comp_msg7 <- "dendrogram_output.pptx - is a slide deck illustrating cluster membership in dendrogram."
  comp_msg8 <- "Output files are useful for choosing a cluster solution for further analysis."
  comp_msg9 <- "Objects in PowerPoint files all editable and may be resized and ungrouped."
  comp_msg <- paste(comp_msg1, comp_msg2, comp_blank, comp_msg3, comp_blank,
    comp_msg4, comp_blank, comp_msg5, comp_blank, comp_msg6, comp_blank,
    comp_msg7, comp_blank, comp_msg8, comp_blank, comp_msg9,
    sep = "\n"
  )
  tcltk::tk_messageBox(type = "ok", message = comp_msg)
}
choose_cluster <- function() {
  choose_blank <- ""
  choose_msg1 <- "Use any or all of the files from the COMPUTE MAPS to determine the cluster solution that best represents the issue being addressed in this concept mapping project. Once a cluster solution is selected, choose CREATE CLUSTER REPORT from the menu to produce a detailed report for that cluster solution."
  choose_msg2 <- "OUTPUT.XLSX -  open the worksheet OUTPUT. Columns labeled clu15 to clu5  indicate the cluster membership of each item for each cluster solution.  Clu15 is the fifteen cluster solution, clu14 is the 14 cluster solution and so on.  Sort all data based on a cluster column.  Examine the items in each cluster to determine where clusters join in solutions with fewer clusters or split in solutions with more clusters.  Continue reviewing cluster solutions until a final solution is chosen."
  choose_msg3 <- "OUTPUT.PPTX - The PowerPoint file visually illustrates the information in output.xlsx and shows which cluster split as the analysis proceeds from the 5 cluster solution to the 15 cluster solution."
  choose_msg4 <- "OUTPUT_DENDROGRAM.PPTX - The PowerPoint file visualizes the various cluster solutions in the form of a dendrogram.  This may be useful for users with advanced understanding of cluster analysis.  Refer to the dendrogram worksheet in output.xlsx to associate item text with item numbers in the dendrogram."
  choose_msg <- paste(choose_msg1, choose_blank, choose_msg2, choose_blank, choose_msg3, choose_blank, choose_msg4, sep = "\n")
  tcltk::tk_messageBox(type = "ok", message = choose_msg)
}
analyze_values <- function() {
  value_blank <- ""
  value_msg1 <- "PATTERN ANALYSIS - This option creates a cluster rating map (cluster rating map.pptx)  and illustrates values by the number of layers for each cluster.  Five layers for the highest value and one layer for the lowest.  The mean of each item is computed then the mean of each clusters is computed from the item means for items in the cluster.  The range of means from highest to lowest is divided into quintiles and clusters are assigned to one of the five quintiles based on the cluster mean value."
  value_msg2 <- "PATTERN MATCHING - This option can compare patterns  in values between measures (e.g., importance & feasibility) or between demographic groups (e.g., management & staff) and saves output as pattern_match.pptx.  Two types of graphics are included.  A ladder graph compares values at the cluster level and two types of ladder are computed.  The first shows the raw score (i.e, mean) values for each cluster.  Oftentimes the computed means can obscure patterns.  A second ladder rescales each side of the ladder on a scale from zero to one and makes the differences/similarities easier to observe.  The second type of graphic is a bivariate plot comparing the item means within each cluster on the same two dimensions as the ladder graph.  Often referred to a go-zone plots, the mean for items for each comparison is used to divide the plot into four quadrants. (1)values are high in both axes, the go-zone (green). (2) values are low on both axes (red). (3) & (4) values are high on one axis and low on the other (yellow)."
  value_msg3 <- "NOTE"
  value_msg4 <- "1. In addition to choosing a measure, demographic variables  may be used to create to subsets to investigate how patterns differ among different groups.  If there are demographics, be sure to click the select button even if choosing ALL."
  value_msg5 <- "2.  When running multiple analyses, be sure to rename any output files with a different name as the next analysis will overwrite prior analyses."
  value_msg <- paste(value_msg1, value_blank, value_msg2, value_blank, value_msg3, value_msg4, value_blank, value_msg5, sep = "\n")
  tcltk::tk_messageBox(type = "ok", message = value_msg)
}
citation <- function() {
  cite_msg1 <- "If you present or publish your work, please use the following information to cite this software ."
  cite_msg2 <- ""
  cite_msg3 <- "McLinden, D. (year). IdeaNet - Open Source software in R for concept mapping. Retrieved from https://github.com/ideanetwork/ideanetR."
  citation_msg <- paste(cite_msg1, cite_msg2, cite_msg3, sep = "\n")
  tcltk::tk_messageBox(type = "ok", message = citation_msg)
}
contact_us <- function() {
  reset_canvas()
  contact_msg1 <- "Inquiries, bug reports, suggestions, improvements in the code can be sent to:"
  contact_msg2 <- "info@ideanetworks.io"
  contact_msg <- paste(contact_msg1, "", contact_msg2, sep = "\n")
  tcltk::tk_messageBox(type = "ok", message = contact_msg)
}
quit <- function() {
  tcltk::.Tcl("set exit 1")
  tcltk::tkdestroy(tt)
}

# end menu functions
# ********************************************************************************************** -----

# MENU -----
# . . top level canvas -----
tt <-tcltk::tktoplevel(bg = "white")
tcltk::tkwm.title(tt, "Ideanet: Concept Mapping and Pattern analysis")
topMenu <- tcltk::tkmenu(tt) # Create a menu
tcltk::tkconfigure(tt, menu = topMenu) # Add it to the 'tt' window

# . . startup screen -----
lpw.1 <- tcltk::tkframe(tt, bg = "white", width = 800, height = 800)
tcltk::tkpack(lpw.1, side = "left", fill = "both") # expand=TRUE,
assign("lpw.1", lpw.1, envir = .GlobalEnv)

tcltk::tkgrid(tcltk2::tk2message(lpw.1, text = "", width = 800, bg = "white", justify = "left"))
tcltk::tkgrid(tcltk2::tk2message(lpw.1, text = "", width = 800, bg = "white", justify = "left"))
tcltk::tkgrid(tcltk2::tk2message(lpw.1,
  text = "Ideanet: An application in R for Concept Mapping that computes and visualizes a network of ideas",
  bg = "white",  width = 800, justify = "left"))
tcltk::tkgrid(tcltk2::tk2message(lpw.1, text = "----------", width = 500, bg = "white", justify = "left"))
tcltk::tkgrid(tcltk2::tk2message(lpw.1, text = "", bg = "white", justify = "left"))
tcltk::tkgrid(tcltk2::tk2message(lpw.1,
  text = "Click on Help/Using this app for information on how to use this application.",
  width = 800, bg = "white"
)) # ,justify="left"))
tcltk::tkgrid(tcltk2::tk2message(lpw.1, text = "----------", width = 500, bg = "white", justify = "left"))
tcltk::tkgrid(tcltk2::tk2message(lpw.1, text = "", bg = "white", justify = "left"))
tcltk::tkgrid(tcltk2::tk2message(lpw.1, text = "", bg = "white", justify = "left"))
tcltk::tkgrid(tcltk2::tk2message(lpw.1,
  text = "License:  The program distributed here is shared in the hope that this will be useful,  but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.  This work is licensed under the GNU General Public License v3.0 which was downloaded with this program or provided to you when this program was shared with you. If you do not have the license document, then a link to the details of the license can be found at: https://www.gnu.org/licenses/gpl-3.0.en.html You are free to use this program, share with others along with the license and make and share changes to the code.  If you do make changes, the software license requires that the code and any ensuing  modifications be made publicly available allowing the entire community to benefit.  Suggestions, questions, code improvements - info@ideanetworks.io",
  width = 800, bg = "white", justify = "left"
))

# . . create menu options -----
data_entry <- tcltk::tkmenu(topMenu, tearoff = FALSE)
tcltk::tkadd(data_entry, "command",
      label = "Create blank template for data entry",
  command = create_input_wb
)

review_data <- tcltk::tkmenu(topMenu, tearoff = FALSE)
tcltk::tkadd(review_data, "command",
      label = "Check sort data for errors",
  command = sortdata_errorcheck
)
tcltk::tkadd(review_data, "command",
  label = "Review & define demographics & rating data",
  command = ratedata_errorcheck
)

compute_map <- tcltk::tkmenu(topMenu, tearoff = FALSE)
tcltk::tkadd(compute_map, "command", label = "Choose sort data input file", command = getdata_computemaps)

cluster_rpt <- tcltk::tkmenu(topMenu, tearoff = FALSE)
tcltk::tkadd(cluster_rpt, "command", label = "Choose cluster number & output file", command = entry_clus_rpt)

pattern_rpt <- tcltk::tkmenu(topMenu, tearoff = FALSE)
tcltk::tkadd(pattern_rpt, "command", label = "Pattern analysis: Cluster rating map", command = pattern_analysis)
tcltk::tkadd(pattern_rpt, "command", label = "Pattern matching: Ladder & Go zone graphs", command = pattern_matching)

valid_app <- tcltk::tkmenu(topMenu, tearoff = FALSE)
# tcltk::tkadd(valid_app, "command", label = "Validity analysis - MDS", command=valid_mds)
tcltk::tkadd(valid_app, "command", label = "Validity analysis - Clusters", command = valid_cluster)

help_app <- tcltk::tkmenu(topMenu, tearoff = FALSE)
tcltk::tkadd(help_app, "command", label = "About", command = about_ideanet)
tcltk::tkadd(help_app, "command", label = "License", command = ideanet_license)
tcltk::tkadd(help_app, "command", label = "Version", command = ideanet_version)
tcltk::tkadd(help_app, "command", label = "Citation", command = citation)
tcltk::tkadd(help_app, "command", label = "Bug reports,suggestions, code changes, etc", command = contact_us)

# sub-menu for contents in help app
contents_app <- tcltk::tkmenu(help_app, tearoff = FALSE)
tcltk::tkadd(contents_app, "command", label = "Collect sort data", command = collect_sort_data) # can be racked or stacked
tcltk::tkadd(contents_app, "command", label = "Sorting data structures", command = sort_data_structure) # need to use this for sorting, none, 1 or 2 measure and none, 1,2,3 demographics of type cont or cat
tcltk::tkadd(contents_app, "command", label = "Data entry", command = enter_data) # will check sort data, demographics and rating data
tcltk::tkadd(contents_app, "command", label = "Compute maps", command = compute_maps) # can be one or two measure # can be none or up to three demographics of type continuous or categorical
tcltk::tkadd(contents_app, "command", label = "Choose cluster solution & create a report", command = choose_cluster) # can be one or two measure # can be none or up to three demographics of type continuous or categorical
tcltk::tkadd(contents_app, "command", label = "Analyze values", command = analyze_values) # can be one or two measure # can be none or up to three demographics of type continuous or categorical

quit_app <- tcltk::tkmenu(topMenu, tearoff = FALSE)
tcltk::tkadd(quit_app, "command", label = "Close app", command = quit)

# . . create menu -----
tcltk::tkadd(topMenu, "separator")
tcltk::tkadd(topMenu, "cascade", label = "Data entry", menu = data_entry)
tcltk::tkadd(topMenu, "cascade", label = "Review & define data", menu = review_data)
tcltk::tkadd(topMenu, "cascade", label = "Compute maps", menu = compute_map)
tcltk::tkadd(topMenu, "cascade", label = "Create cluster report", menu = cluster_rpt)
tcltk::tkadd(topMenu, "cascade", label = "Analyze values", menu = pattern_rpt)
tcltk::tkadd(topMenu, "cascade", label = "Assess validity", menu = valid_app)
tcltk::tkadd(topMenu, "cascade", label = "Help", menu = help_app)
tcltk::tkadd(help_app, "cascade", label = "Using this app", menu = contents_app) # Add content menu in help for submenu of MDS etc.
tcltk::tkadd(topMenu, "cascade", label = "Quit", menu = quit_app)

# . . wait for input -----
tcltk::.Tcl("set exit 0")
tcltk::.Tcl("vwait exit")

# ********************************************************************************************** -----
} # end ideanet function -----


