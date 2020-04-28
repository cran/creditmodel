#' Data Exploration
#'
#' #'The \code{data_exploration} includes both univariate and bivariate analysis and ranges from univariate statistics and frequency distributions, to correlations, cross-tabulation and characteristic analysis.
#' @param dat A data.frame with x and target.
#' @param save_data Logical. If TRUE, save  files to the specified folder at \code{dir_path}
#' @param dir_path The path for periodically saved outliers analysis file. Default is tempdir().
#' @param file_name The file name for periodically saved outliers analysis file. Default is NULL.
#' @return A list contains both categrory and numeric variable analysis.
#' @examples
#' data_ex = data_exploration(dat = UCICreditCard[1:1000,])
#' @importFrom dplyr group_by mutate summarize  summarise n  count %>% filter mutate_if ungroup count 
#' @importFrom data.table fwrite melt fread dcast as.data.table
#' @export
data_exploration <- function(dat,  save_data = FALSE, file_name = NULL, dir_path = tempdir()) {
  opt = options("warn" = -1, scipen = 100, stringsAsFactors = FALSE, digits = 2) # suppress warnings
  view_char = view_char1 = view_num1 = view_num = Feature =  NULL
  dat1 = dat %>% checking_data() %>% null_blank_na() %>% time_transfer() %>% mutate_if(is.character, as.factor)
  char_x_list = get_names(dat = dat1,
                          types = c('factor', 'character', "ordered"),
                          get_ex = FALSE)
  num_x_list = get_names(dat = dat1,
                         types = c('numeric', 'integer', 'double'),
                         get_ex = FALSE)
  date_x_list = get_names(dat = dat1,
                          types = c("Date",
                                    "POSIXlt", "POSIXct", "POSIXt"),
                          get_ex = FALSE)
  dat1[,date_x_list] = lapply(date_x_list,function(x)as.Date(dat1[,x]))
  
  
  cat_line("--Data summary", col = love_color("dark_green"))
  ex_out = list(Observations = nrow(dat1), Numeric_variables = length(num_x_list),
                Category_variables = length(char_x_list),Date_variables = length(date_x_list))  
  cat_bullet(paste0(format(names(ex_out)), ": ", unname(ex_out)), col = "darkgrey")     
  #numeric varaibles
  num_x_list = c(date_x_list,num_x_list )
  if (length(num_x_list) > 0) {
    #summary
    
    dat_num = summary(dat1[num_x_list])
    dat_num = as.data.frame(dat_num)
    view_num = cbind(as.character(dat_num[, "Var2"]), t(as.data.frame(strsplit(dat_num[, "Freq"], ":|.:| :|: "))))[,1:3]
    view_num = apply(view_num, 2, function(i) gsub(" ", "", i))
    colnames(view_num) = c("Feature", "Summary", "View")
    
    view_num[, "Summary"] = ifelse(is.na(view_num[, "Summary"]) | view_num[, "Summary"] %alike% 
                                     c("NA","NA's", "NA\'s","Other"), "NMiss", view_num[, "Summary"])
    view_num[is.na(view_num)] = 0
    #reshape data
	view_num = as.data.table(view_num)
    view_num = data.table::dcast(view_num, Feature ~ Summary,  value.var = "View")
	view_num = as.data.frame(view_num)
    if (!is.element("NMiss", names(view_num))) {
      view_num$NMiss = 0
    }
    view_num[is.na(view_num)] = 0
    num_var_list = gsub(" ", "", as.character(unlist(view_num[, "Feature"])))
    view_num$Std = round(as.numeric(colSds(dat1[num_var_list], na.rm = TRUE)),2)
    view_num$Miss_Rate = as_percent(as.numeric(view_num$NMiss)/nrow(dat1),4)
    
    names(view_num) = c("Feature", "25%", "75%", "Max", "Mean", "Median", "Min", "NMiss", "Std","Miss_Rate")
    view_num1 = as.data.frame(view_num[, c("Feature", "NMiss","Miss_Rate", "Max", "75%",   "Median","25%", "Min", "Mean", "Std")])
    if (save_data) {
      dir_path = ifelse(!is.character(dir_path), tempdir(), dir_path)
      if (!dir.exists(dir_path)) dir.create(dir_path)
      if (!is.character(file_name)) { file_name = NULL }
      save_data(view_num1, file_name = ifelse(is.null(file_name), "feature_exploration_numeric", 
                                            paste(file_name, "feature_exploration_numeric", sep = ".")),
              dir_path = dir_path, note = TRUE, as_list = FALSE)
    }
  }
  #category varaibles
  if (length(char_x_list) > 0) {
    dat_char = summary(dat1[char_x_list])
    dat_char = as.data.frame(dat_char)
    dat_char = apply(dat_char, 2, function(i) gsub(" ", "", i))
    view_char = cbind(as.character(dat_char[, "Var2"]), as.data.frame(gsub(":", " : ", dat_char[, "Freq"])))
    view_char = as.data.frame(view_char, row.names = NULL)
    names(view_char) = c("Feature", "Value")
    
    n_len = view_char %>% dplyr::group_by(Feature) %>% dplyr::count() %>% dplyr::ungroup()
    v_name = strsplit(paste0("Value", 1:max(n_len$n), sep = "", collapse = ","), ",")
    view_char$ids = rep(unlist(v_name), length(char_x_list))
    #reshape data
	view_char = as.data.table(view_char)
    view_char1 = data.table::dcast(view_char, Feature ~ ids, value.var = c("Value"))
	view_char1 = as.data.frame(view_char1)
    dat1[char_x_list] = merge_category(dat1[char_x_list], m = 10,note = FALSE)
    Corr_dt = data.frame(Feature = char_x_list, NMiss = unlist(lapply(char_x_list, function(x) sum(is.na(dat1[, x])))))
    view_char1 = merge(view_char1, Corr_dt)
    view_char1$Miss_Rate = as_percent(as.numeric(view_char1$NMiss)/nrow(dat1),4)
    view_char1[is.na(view_char1)] = "--"
    view_char1 = as.data.frame(view_char1)
    
    view_char1 = view_char1[, c("Feature", "NMiss",'Miss_Rate', unlist(v_name))]
    if (save_data) {
      dir_path = ifelse(!is.character(dir_path), tempdir(), dir_path)
      if (!dir.exists(dir_path)) dir.create(dir_path)
      if (!is.character(file_name)) { file_name = NULL }
      save_data(view_char1, file_name = ifelse(is.null(file_name), "feature_exploration_character", 
                                             paste(file_name, "feature_exploration_character", sep = ".")),
              dir_path = dir_path, note = TRUE, as_list = FALSE)
    }
    
  }
  
  return(list(num = view_num1,char = view_char1))
  options(opt)
}