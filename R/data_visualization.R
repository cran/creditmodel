#' Plot Independent Variables Distribution
#'
#' You can use the \code{plot_vars} to produce plots that characterize the frequency or the distribution of your data.
#' \code{get_plots} can loop through plots for all specified independent variables.
#' @param dat_train A data.frame with independent variables and target variable.
#' @param dat_test  A data.frame of test data. Default is NULL.
#' @param target The name of target variable.
#' @param x_list Names of independent variables.
#' @param x  The name of an independent variable.
#' @param ex_cols A list of excluded variables. Regular expressions can also be used to match variable names. Default is NULL.
#' @param pos_flag Value of positive class, Default is "1".
#' @param breaks_list A table containing a list of splitting points for each independent variable. Default is NULL.
#' @param breaks Splitting points for an independent variable. Default is NULL.
#' @param g_width  The width of graphs.
#' @param best  Logical, merge initial breaks to get optimal breaks for binning.
#' @param equal_bins  Logical, generates initial breaks for equal frequency or width binning.
#' @param cut_bin A string, if equal_bins is TRUE, 'equal_depth' or 'equal_width', default is 'equal_depth'.
#' @param g  Number of initial breakpoints for equal frequency binning.
#' @param tree_control  Parameters of using Decision Tree to segment initial breaks. See detials: \code{\link{get_tree_breaks}}
#' @param bins_control  Parameters  used to control binning.  See detials: \code{\link{select_best_class}}, \code{\link{select_best_breaks}}
#' @param parallel Logical, parallel computing. Default is FALSE.
#' @param plot_show Logical, show model performance in current graphic device. Default is FALSE.
#' @param save_data Logical, save results in locally specified folder. Default is FALSE.
#' @param dir_path The path for periodically saved graphic files.
#' @param file_name  The name for periodically saved data file. Default is NULL.
#' @examples
#' train_test <- train_test_split(UCICreditCard[1:1000,], split_type = "Random",
#'  prop = 0.8, save_data = FALSE)
#' dat_train = train_test$train
#' dat_test = train_test$test
#' get_plots(dat_train[, c(8, 26)], dat_test = dat_test[, c(8, 26)],
#' target = "default.payment.next.month")
#' @import ggplot2
#' @importFrom dplyr group_by mutate summarize  summarise n  count %>% filter mutate_if distinct ungroup
#' @importFrom data.table melt
#' @export


get_plots <- function(dat_train, dat_test = NULL, x_list = NULL,
					  target = NULL, ex_cols = NULL, breaks_list = NULL,
					  pos_flag = NULL, equal_bins = FALSE, cut_bin = 'equal_depth', best = TRUE,
					  g = 20, tree_control = NULL, bins_control = NULL, plot_show = TRUE,
					  save_data = FALSE, file_name = NULL,
					  parallel = FALSE, g_width = 8, dir_path = tempdir()) {

	opt = options('warn' = -1, scipen = 200, stringsAsFactors = FALSE) #
	if (save_data) {
		dir_path = paste0(dir_path, "/variable_plot/")
		if (!dir.exists(dir_path)) dir.create(dir_path)
		if (dir.exists(dir_path)) { file.remove(list.files(dir_path, recursive = TRUE, full.names = TRUE)) }
		}
	dat_train = checking_data(dat = dat_train, target = target, pos_flag = pos_flag)
	if (is.null(x_list)) {
		if (is.null(x_list)) {
			if (!is.null(breaks_list)) {
				x_list = unique(as.character(breaks_list[, "Feature"]))
			}
			x_list = get_x_list(x_list = x_list,
									dat_train = dat_train,
									dat_test = dat_test,
									ex_cols = ex_cols)
		}
	} else {
		x_list = get_x_list(x_list = x_list,
									dat_train = dat_train,
									dat_test = dat_test,
									ex_cols = ex_cols)
	}
	if (!is.null(dat_test)) {
		dat_test = checking_data(dat = dat_test, target = target, pos_flag = pos_flag)
		x_list = get_x_list(x_list = x_list, dat_train = dat_train, dat_test = dat_test,
							ex_cols = c(target, ex_cols))
		com_list = unique(c(target, x_list))
		dat_train = dat_train[, com_list]
		dat_test = dat_test[, com_list]
		dat_ts = rbind(dat_train, dat_test)
		if (all(unique(dat_ts[, target]) != c("0", "1"))) {
			if (!is.null(pos_flag)) {
				dat_ts$target = ifelse(dat_ts[, target] %in% pos_flag, "1", "0")
			} else {
				pos_flag = list("1", 1, "bad", "positive")
				dat_ts$target = ifelse(dat_ts[, target] %in% pos_flag, "1", "0")
			}
			if (length(unique(dat_ts$target)) == 1) {
				stop("The value of target is unique.\n")
			}
		} else {
			dat_ts$target = as.character(dat_ts[, target])
		}
		nr = nrow(dat_train)
		train_test = train_test_split(dat_ts, split_type = "byRow", prop = nr / nrow(dat_ts),
		   seed = 46, save_data = FALSE, note = FALSE)
		dat_train = train_test$train
		dat_test = train_test$test
	} else {
		if (all(unique(dat_train[, target]) != c("0", "1"))) {
			if (!is.null(pos_flag)) {
				dat_train$target = ifelse(dat_train[, target] %in% pos_flag, "1", "0")
			} else {
				pos_flag = list("1", 1, "bad", "positive")
				dat_train$target = ifelse(dat_train[, target] %in% pos_flag, "1", "0")
			}
			if (length(unique(dat_train$target)) == 1) {
				stop("The value of target is unique.\n")
			}
		} else {
			dat_train$target = as.character(dat_train[, target])
		}
	}

	df_ae_list = loop_function(func = plot_vars, x_list = x_list,
							   args = list(dat_train = dat_train, dat_test = dat_test,
										   target = target, breaks_list = breaks_list,
										   pos_flag = pos_flag,
										   equal_bins = equal_bins,
										   cut_bin = cut_bin,
										   best = best, g = g,
										   tree_control = tree_control,
										   bins_control = bins_control,
										   plot_show = plot_show,
										   g_width = g_width, dir_path = dir_path, save_data = save_data),
							   bind = "rbind", parallel = parallel, as_list = FALSE)

	if (save_data) {
		save_data(df_ae_list, dir_path = dir_path, file_name = ifelse(is.null(file_name), "plot_table", paste(file_name, "plot_table", sep = ".")), append = FALSE, note = FALSE)
	}
	return(df_ae_list)
	options(opt) # reset
}


#' @rdname get_plots
#' @export


plot_vars <- function(dat_train, x, target, dat_test = NULL,
					  g_width = 8, breaks_list = NULL, breaks = NULL,
					  pos_flag = list("1", 1, "bad", "positive"),
					  equal_bins = TRUE, cut_bin = 'equal_depth', best = FALSE,
					  g = 10, tree_control = NULL,
					  bins_control = NULL,
					  plot_show = TRUE,
					  save_data = FALSE,
					  dir_path = tempdir()) {

	dat_train = checking_data(dat = dat_train, target = target)
	dat_train = char_to_num(dat_train, char_list = x, note = FALSE)
	digits_x = min(ifelse(is.numeric(dat_train[, x]), digits_num(dat_train[, x]), 4), 4, na.rm = FALSE)
	opt = options('warn' = -1, scipen = 200, digits = digits_x + 1) #

	dat_train$target = as.character(dat_train[, target])
	xn = NULL
	if (class(dat_train[, x]) %in% c("numeric", "double", "integer") && length(unique(dat_train[, x])) > 5) {

		med <- dat_train %>%
  	  dplyr::mutate(xn = dat_train[, x]) %>%
  	  dplyr::group_by(target) %>%
  	  dplyr::summarise(grp.mean = quantile(xn, 0.5, na.rm = TRUE, type = 3))
		none_na_num <- sum(!is.na(dat_train[, x]))
		tbl_x <- table(dat_train[, x])
		x_unique_value <- as.double(names(tbl_x))
		cum_sum <- cumsum(tbl_x)
		cuts_sum <- approx(cum_sum, x_unique_value, xout = (1:100) * none_na_num / 100,
					   method = "constant", rule = 2, f = 1)$y

		dat_train_sub = dat_train
		if (length(unique(cuts_sum)) > 10) {
			x_cuts <- cuts_sum[length(cuts_sum) - 1]
			dat_train_sub <- subset(dat_train, dat_train[, x] <= x_cuts)
		}
		#desity plot
		plot_1 <- ggplot(dat_train_sub, aes(x = dat_train_sub[, x])) +
  	  geom_density(aes(fill = dat_train_sub$target), alpha = 0.3) +
  	  stat_density(geom = "line", position = "identity", size = 0.6,
				   aes(color = dat_train_sub$target)) +
  	  scale_fill_manual(values = c('1' = love_color("shallow_red"),
								   '0' = love_color("sky_blue"))) +
  	  scale_color_manual(values = c('1' = love_color("shallow_red"),
									'0' = love_color("sky_blue"))) +
  	  geom_vline(data = med, linetype = "dashed", size = 0.7,
				 aes(xintercept = med$grp.mean, color = med$target)) +
  	  xlab(x) +
  	  ggtitle(paste("Density of", x)) +
  	  plot_theme(legend.position = c(.9, .9), title_size = 9,
				 axis_title_size = 8)

	} else {
		#relative frequency histogram
		dat_tr = dat_train
		dat_tr[, x] = as.character(dat_tr[, x])

		#relative frequency histogram
		dat_tr$target = as.character(dat_tr[, target])
		data1 = dat_tr %>%
  	  dplyr::mutate(xn = dat_tr[, x]) %>%
  	  dplyr::group_by(xn) %>% dplyr::count( xn,target) %>%
  	  dplyr::mutate(percent = n / sum(n))
		plot_1 <- ggplot(data1, aes(x = data1$xn, y = data1$percent, fill = reorder(data1$target, n))) +
  	  geom_bar(stat = "identity", position = position_stack()) +
  	  geom_text(aes(label = paste(as_percent(data1$percent, digits = 3))),
				size = ifelse(length(data1$xn) > 10, 2.1,
							  ifelse(length(data1$xn) > 5, 2.5,
									 ifelse(length(data1$xn) > 3, 3, 3.3))), vjust = 1, colour = 'white', position = position_stack()) +
  	  guides(fill = guide_legend(reverse = F)) +
  	  ggtitle(paste("Relative Frequency of", x)) +
  	  ylab("Percent") + xlab(x) +
  	  scale_fill_manual(values = c('0' = love_color("shallow_cyan"),
								   '1' = love_color("deep_grey"))) +
  	  plot_theme(legend.position = "top", title_size = 9,
				 axis_title_size = 8)
	}

	if (!is.null(dat_test) || length(dat_test) > 1) {
		dat_test = char_to_num(dat_test, char_list = x, note = FALSE)
		if (class(dat_test[, x])[1] != class(dat_train[, x])[1]) {
			warning("The types of x in dat_test and dat_train are different!")
		}
		df_ae = get_psi_iv(dat = dat_train, dat_test = dat_test,
					   x = x, target = target, pos_flag = pos_flag,
					   breaks_list = breaks_list,
					   breaks = breaks,
					   equal_bins = equal_bins,
					   cut_bin = cut_bin,
					   tree_control = tree_control,
					   bins_control = bins_control,
					   bins_total = FALSE,
					   best = best, g = g, as_table = TRUE,
					   note = FALSE, bins_no = TRUE)
		ae_total <- data.table::melt(as.data.table(df_ae[c("bins", "%actual", "%expected")]),
								 id.vars = c("bins"),
								 variable.name = "actual_expected",
								 value.name = "value")
		ae_1 <- data.table::melt(as.data.table(df_ae[c("bins", "%actual_1", "%expected_1")]),
							 id.vars = c("bins"),
							 variable.name = "actual_expected",
							 value.name = "value")

		plot_2 <- ggplot(ae_total, aes(x = ae_total$bins,
								   y = ae_total$value,
								   fill = ae_total$actual_expected)) +
  	  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
  	  geom_text(aes(y = ae_total$value,
					label = paste(as_percent(ae_total$value, digits = 3))),
				position = position_dodge(width = 0.7),
				size = ifelse(nrow(ae_total) > 10, 2.6,
							  ifelse(nrow(ae_total) > 5, 2.8,
									 ifelse(nrow(ae_total) > 3, 3, 3.2))), vjust = 1, hjust = 0.3, colour = "white") +
  	  geom_line(aes(x = factor(ae_1[[1]]),
					y = as.numeric(ae_1$value) * max(ae_total$value) * 4,
					color = ae_1$actual_expected,
					linetype = ae_1$actual_expected,
					group = ae_1$actual_expected),
				position = position_dodge(width = 0.5), size = 1) +
  	  geom_point(aes(y = as.numeric(ae_1$value) * max(ae_total$value) * 4,
					 color = ae_1$actual_expected,
					 group = ae_1$actual_expected),
				 position = position_dodge(width = 0.5),
				 fill = 'white', color = love_color("deep_red"), size = 2, shape = 21) +
  	  geom_text(aes(y = as.numeric(ae_1$value) * max(ae_total$value) * 4,
					label = paste(as_percent(ae_1$value, digits = 3))),
				position = position_dodge(width = 0.5),
				colour = 'black',
				size = ifelse(nrow(ae_total) > 10, 2.6,
							  ifelse(nrow(ae_total) > 5, 2.8,
									 ifelse(nrow(ae_total) > 3, 3, 3.2))), vjust = -0.1) +
  	  annotate(geom = 'text',
			   x = dim(ae_total)[1] / 3,
			   y = max(c(ae_total$value, as.numeric(ae_1$value) * max(ae_total$value) * 4)) + 0.09,
			   label = paste(paste("IV:", sum(df_ae$IVi)), paste('PSI:', sum(df_ae$PSIi)), sep = "   ")) +
  	  scale_fill_manual(values = c('%actual' = love_color("deep_grey"),
								   '%expected' = love_color("light_yellow"))) +
  	  scale_color_manual(values = c('%actual_1' = love_color("shallow_red"),
									'%expected_1' = love_color("sky_blue"))) +
  	  ylim(c(-0.01, max(c(ae_total$value, as.numeric(ae_1$value) * max(ae_total$value) * 4)) + 0.1)) +
  	  xlab(x) + ylab("Total Percent") +
  	  ggtitle(paste(x, "Distribution of Train/Expected and Test/Actual")) +
  	  plot_theme(legend.position = "top",
				 title_size = 9,
				 axis_title_size = 8,
				 angle = ifelse(nrow(ae_total) > 10, 60,
								ifelse(nrow(ae_total) > 5, 40,
									   ifelse(nrow(ae_total) > 3, 20, 10))),
				 axis_size_x = ifelse(max(nchar(ae_total$bins)) > 30, 5,
									  ifelse(max(nchar(ae_total$bins)) > 20, 6,
											 ifelse(max(nchar(ae_total$bins)) > 10, 7, 8))))

	} else {

		if (is.null(breaks) & is.null(breaks_list)) {

			breaks = get_breaks(dat = dat_train, dat_test = dat_test,
						  x = x, target = target, pos_flag = pos_flag,
						  equal_bins = equal_bins,
						  cut_bin = cut_bin,
						  tree_control = tree_control,
						  bins_control = bins_control,
						  bins_total = FALSE,
						  best = best, g = g, as_table = FALSE,
						  note = FALSE, bins_no = TRUE)

		}
		df_ae <- get_bins_table(dat = dat_train,
							x = x, target = target, pos_flag = pos_flag,
							breaks_list = breaks_list,
							breaks = breaks,
							bins_total = FALSE,
							note = FALSE)
		tar_var = paste0("%", target)
		plot_2 <- ggplot(df_ae, aes(x = df_ae$bins,
								y = de_percent(df_ae$`%total`, 3)
	)) +
  	  geom_bar(aes(fill = "%total"), stat = "identity", position = position_dodge(width = 0.7)) +
  	  geom_text(aes(y = de_percent(df_ae$`%total`, 3),
					label = paste(df_ae$`%total`)),
				position = position_dodge(width = 0.7),
				size = ifelse(nrow(df_ae) > 10, 2.6,
							  ifelse(nrow(df_ae) > 5, 2.8,
									 ifelse(nrow(df_ae) > 3, 3, 3.2))), vjust = 1, hjust = 0.3, colour = "white") +
  	  geom_line(aes(x = factor(df_ae[['bins']]),
					y = de_percent(df_ae$bad_rate, 3) * max(de_percent(df_ae$`%total`, 3)) * 4,
					color = tar_var
	  ),
	  linetype = 1,
	  group = "%total",
	  position = position_dodge(width = 0.5), size = 1) +
  	  geom_point(aes(y = de_percent(df_ae$bad_rate, 3) * max(de_percent(df_ae$`%total`, 3)) * 4
	  ),
	  color = love_color("deep_orange"),
	  group = 1,
	  position = position_dodge(width = 0.5),
	  fill = 'white', color = love_color("deep_red"), size = 2, shape = 21) +
  	  geom_text(aes(y = de_percent(df_ae$bad_rate, 3) * max(de_percent(df_ae$`%total`, 3)) * 4,
					label = paste(df_ae$bad_rate)),
				position = position_dodge(width = 0.5),
				colour = 'black',
				size = ifelse(nrow(df_ae) > 10, 2.6,
							  ifelse(nrow(df_ae) > 5, 2.8,
									 ifelse(nrow(df_ae) > 3, 3, 3.2))), vjust = -0.1) +
  	  annotate(geom = 'text',
			   x = dim(df_ae)[1] / 3,
			   y = max(c(de_percent(df_ae$`%total`, 3),
						 de_percent(df_ae$bad_rate, 3) * max(de_percent(df_ae$`%total`, 3)) * 4)) + 0.09,
			   label = paste(paste("IV:", sum(df_ae$iv, na.rm = TRUE)))) +
  	  scale_fill_manual(values = c("%total" = love_color("light_cyan"))) +
  	  scale_color_manual(values = c(love_color("deep_orange"))) +
  	  ylim(c(-0.01, max(c(de_percent(df_ae$`%total`, 3),
						  de_percent(df_ae$bad_rate, 3) * max(de_percent(df_ae$`%total`, 3)) * 4)) + 0.05)) +

  	  guides(fill = guide_legend(title = NULL)) +
  	  xlab(x) + ylab("percent") +
  	  ggtitle(paste(x, " Distribution")) +
  	  plot_theme(legend.position = "top",
				 title_size = 9,
				 axis_title_size = 8,
				 angle = ifelse(nrow(df_ae) > 10, 60,
								ifelse(nrow(df_ae) > 5, 40,
									   ifelse(nrow(df_ae) > 3, 20, 10))),
				 axis_size_x = ifelse(max(nchar(df_ae$bins)) > 30, 5,
									  ifelse(max(nchar(df_ae$bins)) > 20, 6,
											 ifelse(max(nchar(df_ae$bins)) > 10, 7, 8))))


	}
	if (save_data) {
		ggsave(paste0(dir_path, paste(x, "png", sep = '.')),
		   plot = multi_grid(grobs = list(plot_1, plot_2), ncol = 2, nrow = 1),
		   width = g_width, height = g_width / 2, dpi = "retina")
	}
	if (plot_show) {
		plot(multi_grid(grobs = list(plot_1, plot_2), ncol = 2, nrow = 1))
	}
	return(df_ae)
	options(opt) # reset
}

#' Plot PSI(Population Stability Index)
#'
#' You can use the \code{psi_plot} to plot PSI of your data.
#' \code{get_psi_plots} can loop through plots for all specified independent variables.
#' @param dat_train A data.frame with independent variables.
#' @param dat_test  A data.frame of test data. Default is NULL.
#' @param x_list Names of independent variables.
#' @param x  The name of an independent variable.
#' @param occur_time  The name of occur time.
#' @param ex_cols A list of excluded variables. Regular expressions can also be used to match variable names. Default is NULL.
#' @param breaks_list A table containing a list of splitting points for each independent variable. Default is NULL.
#' @param breaks Splitting points for a continues variable.
#' @param g_width The width of graphs.
#' @param g  Number of initial breakpoints for equal frequency binning.
#' @param parallel Logical, parallel computing. Default is FALSE.
#' @param plot_show Logical, show model performance in current graphic device. Default is FALSE.
#' @param save_data Logical, save results in locally specified folder. Default is FALSE.
#' @param dir_path The path for periodically saved graphic files.
#' @param file_name  The name for periodically saved data file. Default is NULL.
#' @examples
#' train_test <- train_test_split(UCICreditCard[1:1000,], split_type = "Random",
#'  prop = 0.8, save_data = FALSE)
#' dat_train = train_test$train
#' dat_test = train_test$test
#' get_psi_plots(dat_train[, c(8, 9)], dat_test = dat_test[, c(8, 9)])
#' @import ggplot2
#' @importFrom dplyr group_by mutate summarize  summarise n  count %>% filter mutate_if distinct ungroup
#' @importFrom data.table melt
#' @export

get_psi_plots <- function(dat_train, dat_test = NULL, x_list = NULL,
                      ex_cols = NULL, breaks_list = NULL,occur_time = NULL,
                      g = 10,plot_show = TRUE,
                      save_data = FALSE, file_name = NULL,
                      parallel = FALSE, g_width = 8, dir_path = tempdir()) {

    opt = options('warn' = -1,scipen = 200, stringsAsFactors = FALSE) #
    if (save_data) {
        dir_path = paste0(dir_path, "/psi_plot/")
        if (!dir.exists(dir_path)) dir.create(dir_path)
        if (dir.exists(dir_path)) { file.remove(list.files(dir_path, recursive = TRUE, full.names = TRUE)) }
        }
    dat_train = checking_data(dat = dat_train)
    if (is.null(x_list)) {
        if (is.null(x_list)) {
            if (!is.null(breaks_list)) {
                x_list = unique(as.character(breaks_list[, "Feature"]))
            }
                x_list = get_x_list(x_list = x_list,
                                    dat_train = dat_train,
                                    dat_test = dat_test,
                                    ex_cols = ex_cols)
        }
    } else {
	    x_list = get_x_list(x_list = x_list,
                                    dat_train = dat_train,
                                    dat_test = dat_test,
                                    ex_cols = ex_cols)
    }
    if(is.null(dat_test)){
	train_test = train_test_split(dat_train,split_type = 'OOT',prop = 0.7,
                                    occur_time = occur_time)
      dat_train = train_test$train
      dat_test = train_test$test
	}else{
	dat_test = checking_data(dat = dat_test)
	}

    df_ae_list = loop_function(func = psi_plot, x_list = x_list,
                               args = list(dat_train = dat_train, dat_test = dat_test,
                                           breaks_list = breaks_list,
                                            g = g,
                                           plot_show = plot_show,
                                           g_width = g_width, dir_path = dir_path, save_data = save_data),
                               bind = "rbind", parallel = parallel, as_list = FALSE)

    if (save_data) {
        save_data(df_ae_list, dir_path = dir_path, file_name = ifelse(is.null(file_name), "PSI_table", paste(file_name, "PSI_table", sep = ".")), append = FALSE, note = FALSE)
    }

    return(df_ae_list)
	options(opt) # reset
}


#' @rdname get_psi_plots
#' @export
psi_plot <- function(dat_train, x, dat_test = NULL,occur_time = NULL,
                      g_width = 8,breaks_list = NULL,breaks = NULL,g = 10,
                      plot_show = TRUE,
                      save_data = FALSE,
                      dir_path = tempdir()){

  digits_x = ifelse(is.numeric(dat_train[, x]), digits_num(dat_train[, x]),4)
  opt = options('warn' = -1, scipen = 200, stringsAsFactors = FALSE, digits = digits_x + 1) #
    if(is.null(dat_test)) {
      train_test = train_test_split(dat_train,split_type = 'OOT',prop = 0.7,
                                    occur_time = occur_time)
      dat_train = train_test$train
      dat_test = train_test$test
    }
  dat_train$ae = "Expected"
  dat_test$ae = "Actual"
  xn = ae = NULL

    df_ae <- get_psi(dat = dat_train, dat_test = dat_test,
                        x = x,
                        breaks_list = breaks_list,
                        breaks = breaks,
                         g = g, as_table = TRUE,
                        note = FALSE, bins_no = TRUE)
    plot_1 = plot_table(grid_table = df_ae[,-1],theme = c("cyan"),tile.color = "grey80", title = paste("PSI of", x))
    ae_total <- data.table::melt(as.data.table(df_ae[c("Bins", "Ac_pct", "Ex_pct")]),
                                 id.vars = c("Bins"),
                                 variable.name = "actual_expected",
                                 value.name = "value")


    plot_2 <- ggplot(ae_total, aes(x = ae_total$Bins,
                                   y = de_percent(ae_total$value, 4),
                                   fill = ae_total$actual_expected)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
      geom_text(aes(y = de_percent(ae_total$value, 4),
                    label = paste(ae_total$value)),
                position = position_dodge(width = 0.7),
                size = ifelse(nrow(ae_total) > 10, 2.3,
                              ifelse(nrow(ae_total) > 5, 2.5,
                                     ifelse(nrow(ae_total) > 3, 2.8, 3))), vjust = 1, hjust = 0.3, colour = "white") +
      annotate(geom = 'text',
               x = dim(ae_total)[1] / 3,
               y = max(c(de_percent(ae_total$value, 4), max(de_percent(ae_total$value, 4)) )) + 0.05,
               label = paste('PSI:',sum(df_ae$PSI_i))) +
      scale_fill_manual(values = c('Ac_pct' = love_color("shallow_cyan"),
                                   'Ex_pct' = love_color("light_purple"))) +

      ylim(c(-0.01, max(c(de_percent(ae_total$value, 4), max(de_percent(ae_total$value, 4)) )) + 0.05)) +
      xlab(x) + ylab("Total Percent") +
      ggtitle(paste(x," Distribution of Expected and Actual")) +
      plot_theme(legend.position = "top",
                 title_size = 9,
                 axis_title_size = 8,
                 angle = ifelse(nrow(ae_total) > 10, 60,
                                ifelse(nrow(ae_total) > 5, 40,
                                       ifelse(nrow(ae_total) > 3, 20, 10))),
                 axis_size_x = ifelse(max(nchar(ae_total$Bins)) > 30, 5,
                                      ifelse(max(nchar(ae_total$Bins)) > 20, 6,
                                             ifelse(max(nchar(ae_total$Bins)) > 10, 7 ,8))))
  if (save_data) {
    ggsave(paste0(dir_path, paste(x, "png", sep = '.')),
           plot = multi_grid(grobs = list(plot_1, plot_2), ncol = 2, nrow = 1),
           width = g_width, height = g_width / 2, dpi = "retina")
  }
  if (plot_show) {
    plot(multi_grid(grobs = list(plot_1, plot_2), ncol = 2, nrow = 1))
  }
  return(df_ae)
  options(opt) # reset
}


#' partial_dependence_plot
#'
#' \code{partial_dependence_plot} is for generating a partial dependence plot.
#' \code{get_partial_dependence_plots} is for ploting partial dependence of all vairables in x_list.
#' @param model A data frame of training with predicted prob or score.
#' @param x The name of an independent variable.
#' @param x_list Names of independent variables.
#' @param x_train A data.frame with independent variables.
#' @param n.trees Number of trees for best.iter of gbm.
#' @param parallel Logical, parallel computing. Default is FALSE.
#' @param plot_show Logical, show model performance in current graphic device. Default is FALSE.
#' @param save_data Logical, save results in locally specified folder. Default is FALSE.
#' @param dir_path The path for periodically saved graphic files.
#' @examples
#' sub = cv_split(UCICreditCard, k = 30)[[1]]
#' dat = UCICreditCard[sub,]
#' dat = re_name(dat, "default.payment.next.month", "target")
#' dat = data_cleansing(dat, target = "target", obs_id = "ID",
#' occur_time = "apply_date", miss_values = list("", -1))
#'
#' train_test <- train_test_split(dat, split_type = "OOT", prop = 0.7,
#'                                 occur_time = "apply_date")
#' dat_train = train_test$train
#' dat_test = train_test$test
#' x_list = c("PAY_0", "LIMIT_BAL", "PAY_AMT5", "PAY_3", "PAY_2")
#' Formula = as.formula(paste("target", paste(x_list, collapse = ' + '), sep = ' ~ '))
#' set.seed(46)
#' lr_model = glm(Formula, data = dat_train[, c("target", x_list)], family = binomial(logit))
#' #plot partial dependency of one variable
#' partial_dependence_plot(model = lr_model, x ="LIMIT_BAL", x_train = dat_train)
#' #plot partial dependency of all variables
#' pd_list = get_partial_dependence_plots(model = lr_model, x_list = x_list[1:2],
#'  x_train = dat_train, save_data = FALSE,plot_show = TRUE)
#' @import ggplot2
#' @importFrom dplyr group_by mutate summarize  summarise n  count %>% filter
#' @importFrom data.table melt dcast
#' @export


partial_dependence_plot <- function(model, x, x_train, n.trees = NULL) {
    if (!requireNamespace("pdp", quietly = TRUE)) {
        cat_rule("Package `pdp` needed for partial dependence plot. Use 'require_packages(pdp)' to install and load it, .\n", col = love_color("deep_red"))
    } else {
	 opt = options('warn' = -1,scipen = 200, stringsAsFactors = FALSE, digits = 6) #
     pd = pdp::partial(model,
            pred.var = c(x),
            grid.resolution = NULL,
            train = x_train,
            n.trees = n.trees,
            prob = TRUE,
            plot = FALSE,
            plot.engine = c("ggplot2"),
            .progress = "none",
            chull = TRUE,
            trim.outliers = TRUE)
	yhat = NULL
    pd_pl =  autoplot(pd, aes(x = pd[x], y = pd$yhat)) +
    geom_line(color = love_color("green_cyan"), size = 1) +
    theme_light() +
    theme(legend.title = element_blank(), legend.position = "top",
           plot.title = element_text(face = "bold", size = 11, vjust = 0, hjust = 0)) +
    scale_y_continuous(limits = c(min(pd$yhat), max(pd$yhat)),
    breaks = round(seq(min(pd$yhat), max(pd$yhat), length.out = 10), 4),
    labels = sprintf("%0.3f", round(seq(min(pd$yhat), max(pd$yhat), length.out = 10), digits = 3))) +
    labs(x = x, y = "y_prob", title = paste(x, " - Partial Dependence"))
	return(pd_pl)
	options(opt)
	}
}


#' @rdname partial_dependence_plot
#' @export



get_partial_dependence_plots <- function(model, x_train, x_list, n.trees = NULL,
           dir_path = getwd(),save_data =TRUE, plot_show = FALSE, parallel = FALSE) {
    if (!requireNamespace("pdp", quietly = TRUE)) {
        cat_rule("Package `pdp` needed for partial dependence plot. Use 'require_packages(pdp)' install and load it, .\n", col = love_color("deep_red"))
    } else {
	pd_list = loop_function(func = partial_dependence_plot,
                        args = list(model = model, x_train = x_train, n.trees = n.trees),
                        x_list = x_list, parallel = parallel, as_list = TRUE)
    if (save_data) {
        dir_path = paste0(dir_path, "/partial_dependence_plots/")
        if (!dir.exists(dir_path)) dir.create(dir_path)
        if (dir.exists(dir_path)) { file.remove(list.files(dir_path, recursive = TRUE, full.names = TRUE)) }
        for (x in names(pd_list)) {
            ggsave(paste0(dir_path, "/pdp.", paste(x, "png", sep = '.')),
           plot = multi_grid(grobs = pd_list[x]),
           width = 4, height = 3, dpi = "retina")
        }
    }
    if (plot_show) {
     plot(multi_grid(grobs = pd_list))
    }
    return(pd_list)
	}
}


#' ks_table & plot
#'
#' \code{ks_table} is for generating a model performance table.
#' \code{ks_table_plot} is for ploting the table generated by \code{ks_table}
#' \code{ks_psi_plot} is for K-S & PSI distrbution ploting.
#' @param train_pred A data frame of training with predicted prob or score.
#' @param test_pred A data frame of validation with predict prob or score.
#' @param target The name of target variable.
#' @param score The name of prob or score variable.
#' @param g Number of breaks for prob or score.
#' @param g_width Width of graphs.
#' @param plot_show Logical, show model performance in current graphic device. Default is FALSE.
#' @param gtitle The title of the graph & The name for periodically saved graphic file. Default is "_ks_psi_table".
#' @param breaks Splitting points of prob or score.
#' @param save_data Logical, save results in locally specified folder. Default is FALSE.
#' @param dir_path The path for periodically saved graphic files.
#' @param file_name  The name for periodically saved data file. Default is NULL.
#' @param pos_flag The value of positive class of target variable, default: "1".
#' @examples
#' sub = cv_split(UCICreditCard, k = 30)[[1]]
#' dat = UCICreditCard[sub,]
#' dat = re_name(dat, "default.payment.next.month", "target")
#' dat = data_cleansing(dat, target = "target", obs_id = "ID",
#' occur_time = "apply_date", miss_values = list("", -1))
#'
#' train_test <- train_test_split(dat, split_type = "OOT", prop = 0.7,
#'                                 occur_time = "apply_date")
#' dat_train = train_test$train
#' dat_test = train_test$test
#' x_list = c("PAY_0", "LIMIT_BAL", "PAY_AMT5", "PAY_3", "PAY_2")
#' Formula = as.formula(paste("target", paste(x_list, collapse = ' + '), sep = ' ~ '))
#' set.seed(46)
#' lr_model = glm(Formula, data = dat_train[, c("target", x_list)], family = binomial(logit))
#'
#' dat_train$pred_LR = round(predict(lr_model, dat_train[, x_list], type = "response"), 5)
#' dat_test$pred_LR = round(predict(lr_model, dat_test[, x_list], type = "response"), 5)
#' # model evaluation
#' ks_psi_plot(train_pred = dat_train, test_pred = dat_test,
#'                             score = "pred_LR", target = "target",
#'                             plot_show = TRUE)
#' tb_pred <- ks_table_plot(train_pred = dat_train, test_pred = dat_test,
#'                                         score = "pred_LR", target = "target",
#'                                      g = 10, g_width = 13, plot_show = FALSE)
#' key_index = model_key_index(tb_pred)
#' @import ggplot2
#' @importFrom dplyr group_by mutate summarize  summarise n  count %>% filter
#' @importFrom data.table melt dcast
#' @export



ks_table <- function(train_pred, test_pred = NULL, target = NULL, score = NULL,
                     g = 10, breaks = NULL, pos_flag = list("1", "1", "Bad", 1)) {
     opt = options('warn' = -1,scipen = 200, stringsAsFactors = FALSE, digits = 6) #
    `train_GB_index` = `test_GB_index` = `G` = `bins` = `B` = `%train` = `%test` = `#train` = `#test` = NULL
    if (is.null(target)) {
        stop("target is missing!\n")
    }
    if (is.null(score)) {
        stop("score is missing!\n")
    }
    if (is.null(breaks)) {
        breaks = get_breaks(dat = train_pred, x = score,
                            target = target, equal_bins = TRUE,
                            best = FALSE, g = g, note = FALSE)
    }
    train_pred$bins = split_bins(dat = train_pred, x = score, breaks = breaks, bins_no = TRUE)

    if (!is.null(target)) {
        if (length(unique(train_pred[, target])) > 1) {
            if (is.null(pos_flag)) {
                train_pred$target = ifelse(train_pred[, target] %in% list("1", "1", "Bad", 1), "B", "G")
            } else {
                train_pred$target = ifelse(train_pred[, target] %in% pos_flag, "B", "G")
                if (length(unique(train_pred$target)) == 1) {
                    stop(paste("The value in pos_flag is not one of the value of train_pred's target.\n"))
                }
            }
        } else {
            stop(paste("The value of train_pred's target is unique.\n"))
        }
    } else {
        stop(paste("The target variable is missing.\n"))
    }

    train_sum <- train_pred %>%
      dplyr::filter(train_pred$target %in% c("B", "G")) %>%
      dplyr::group_by(bins) %>%
      dplyr::count(bins, target) %>%
      dplyr::mutate(percent = n / sum(n)) %>%
      as.data.table()
    train_sum <- data.table::dcast(train_sum[,c("bins", "target", "n"),with = FALSE],
                                     bins ~ target, value.var = "n")
	train_sum = quick_as_df(train_sum)
    train_sum[is.na(train_sum)] <- 0
    train_ks <- transform(train_sum,
                     train_total = G + B,
                     `%train_total` = round((G + B) / sum(G + B), 2),
                     `%train_B` = round(B / (G + B), 3),
                     `%train_cumG` = round(cumsum(G) / sum(G), 2),
                     `%train_cumB` = round(cumsum(B) / sum(B), 2),
                     `train_K-S` = abs(round((cumsum(B) / sum(B)) - (cumsum(G) / sum(G)), 2)))
    if (!is.null(test_pred) || length(test_pred) > 1) {
        test_pred$bins = split_bins(dat = test_pred, x = score, breaks = breaks, bins_no = TRUE)
        if (!is.null(target)) {
            if (length(unique(test_pred[, target])) > 1) {
                if (is.null(pos_flag)) {
                    test_pred$target = ifelse(test_pred[, target] %in% list("1", "1", "Bad", 1), "B", "G")
                } else {
                    test_pred$target = ifelse(test_pred[, target] %in% pos_flag, "B", "G")
                    if (length(unique(test_pred$target)) == 1) {
                        stop(paste("The value in pos_flag is not one of the value of test_pred's target.\n"))
                    }
                }
            } else {
                stop(paste("The value of test_pred's target is unique.\n"))
            }
        } else {
            stop(paste("The target variable is missing.\n"))
        }

        test_sum <- test_pred %>%
        dplyr::filter(test_pred$target %in% c("B", "G")) %>%
        dplyr::group_by(bins) %>%
        dplyr::count(bins, target) %>%
        dplyr::mutate(percent = n / sum(n)) %>%
        as.data.table()

        test_sum <- data.table::dcast(test_sum[,c("bins", "target", "n"),with = FALSE],
                                    bins ~ target, value.var = "n")
		test_sum = quick_as_df(test_sum)
        test_sum[is.na(test_sum)] <- 0

        test_ks <- transform(test_sum,
                     test_total = G + B,
                     `%test_total` = round((G + B) / (sum(G + B)), 2),
                     `%test_B` = round(B / (G + B), 3),
                     `%test_cumG` = round(cumsum(G) / sum(G), 2),
                     `%test_cumB` = round(cumsum(B) / sum(B), 2),
                     `test_K-S` = abs(round((cumsum(B) / sum(B)) - (cumsum(G) / sum(G)), 2))
                     )

        dt_ks = merge(train_ks[c(1, 4:9)], test_ks[c(1, 4:9)], all.x = TRUE)
        dt_ks[is.na(dt_ks)] <- 0
        names(dt_ks) = c("bins", "#train", "%train", "%train_B",
                     "%train_cumG", "%train_cumB", "train_K-S",
                     "#test", "%test", "%test_B", "%test_cumG",
                     "%test_cumB", "test_K-S")
        dt_ks = dt_ks %>% dplyr::mutate(PSI = ifelse(`%train` == 0 | `%test` == 0, 1, round((`%train` - `%test`) * log(`%train` / `%test`), 3)), `#total` = `#train` + `#test`)
        dt_ks = dt_ks[c("bins", "#total", "#train", "#test", "%train", "%test", "%train_B",
                     "%test_B", "%train_cumG", "%train_cumB", "%test_cumG", "%test_cumB",
                     "train_K-S", "test_K-S", "PSI")]
    } else {
        dt_ks = train_ks[c(1, 4:9)]
        dt_ks[is.na(dt_ks)] <- 0
        names(dt_ks) = c("bins", "#train", "%train", "%train_B",
                     "%train_cumG", "%train_cumB", "train_K-S")

        dt_ks = dt_ks[c("bins",  "#train", "%train", "%train_B",
                     "%train_cumG", "%train_cumB",
                     "train_K-S")]
    }

    return(dt_ks)
    options(opt) # reset
}

#' ks_table_plot
#'
#' @rdname ks_table
#' @export



ks_table_plot <- function(train_pred, test_pred, target = "target", score = "score",
                          g = 10,plot_show = TRUE, g_width = 12,file_name = NULL, save_data = FALSE,
                          dir_path = tempdir(), gtitle = NULL) {

    opt = options(scipen = 200, stringsAsFactors = FALSE, digits = 6) #

    ` %train_cumG` = `%train_cumB` = `%train_B` = `%train` = `%test_cumG` = `%test_cumB` = `%test_B` =
    `%test` =  `%train_cumG` = NULL

    tb_pred = ks_table(train_pred=train_pred,test_pred = test_pred, target = target, score = score, g = g)
    total <- c("Total",
               sum(tb_pred$`#total`,na.rm = TRUE),
               sum(tb_pred$`#train`, na.rm = TRUE),
               sum(tb_pred$`#test`, na.rm = TRUE),
               as_percent(sum(tb_pred$`#train`, na.rm = TRUE) / sum(tb_pred$`#total`, na.rm = TRUE), 2),
               as_percent(sum(tb_pred$`#test`, na.rm = TRUE) / sum(tb_pred$`#total` , na.rm = TRUE), 2),
               as_percent(sum(tb_pred$`%train_B` * tb_pred$`#train`, na.rm = TRUE) / sum(tb_pred$`#train`,  na.rm = TRUE), 2),
               as_percent(sum(tb_pred$`%test_B` * tb_pred$`#test`,na.rm = TRUE) / sum(tb_pred$`#test`, na.rm = TRUE), 2), "100%", "100%",
             "100%", "100%",
             max(tb_pred$`train_K-S`,na.rm = TRUE),
             max(tb_pred$`test_K-S`, na.rm = TRUE),
            sum(tb_pred$PSI,na.rm = TRUE))

    dt_pred <- tb_pred[c("bins", "#total", "#train", "#test", "%train", "%test", "%train_B",
                         "%test_B", "%train_cumG", "%train_cumB", "%test_cumG",
                         "%test_cumB", "train_K-S", "test_K-S", "PSI")]
    dt_pred <- transform(dt_pred,
                         `%train` = as_percent(`%train`, digits = 2),
                         `%test` = as_percent(`%test`, digits = 2),
                         `%train_B` = as_percent(`%train_B`, digits = 3),
                         `%test_B` = as_percent(`%test_B`, digits = 3),
                         `%train_cumG` = as_percent(`%train_cumG`, digits = 2),
                         `%train_cumB` = as_percent(`%train_cumB`, digits = 2),
                         `%test_cumG` = as_percent(`%test_cumG`, digits = 2),
                         `%test_cumB` = as_percent(`%test_cumB`, digits = 2)
                         )

   dt_pred <- rbind(dt_pred, total)
    names(dt_pred) = c("bins", "#total", "#train", "#test", "%train", "%test", "%train_B",
                       "%test_B", "%train_cumG", "%train_cumB", "%test_cumG",
                       "%test_cumB", "train_K-S", "test_K-S", "PSI")
    if (save_data) {
        dir_path = ifelse( !is.character(dir_path),
                      tempdir(), dir_path)
        if (!dir.exists(dir_path)) dir.create(dir_path)
        if (!is.character(gtitle)) { gtitle = paste0("mymodel") }
        tb_ks = plot_table(dt_pred)
        ggsave(paste0(dir_path, "/", paste(paste(gtitle, "_ks_psi_table"), "png", sep = '.')),
            plot = tb_ks, dpi = "retina", width = g_width)
        save_data(dt_pred, file_name = paste(gtitle, "_ks_psi_table"), dir_path = dir_path)
        }
    return(dt_pred)
    options(opt) # reset
}


#' ks_table_plot
#'
#' @rdname ks_table
#' @export

ks_psi_plot <- function(train_pred, test_pred, target = "target", score = "score",
                        gtitle = NULL, plot_show = TRUE, g_width = 12, save_data = FALSE,
                        breaks = NULL, g = 10, dir_path = tempdir()) {
    opt = options('warn' = -1,scipen = 200, stringsAsFactors = FALSE, digits = 6) #
   `value` = `train_test` = `bins` =   `%train_cumG` =   `%train_cumB` =  `%test_cumG` =  `%test_cumB` = NULL
    if (!dir.exists(dir_path)) dir.create(dir_path)
    if (is.null(gtitle)) { gtitle = paste0("Model") }
    tb_ks = ks_table(train_pred = train_pred, test_pred = test_pred,
                     target = target, score = score, g = g, breaks = breaks)

    ks = rbind(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0), tb_ks)
    ks_plot = ggplot(ks, aes(x = reorder(as.factor(as_percent(round(cumsum(ks$`%train`), 1))), cumsum(ks$`%train`)))) +
      geom_line(aes(y = `%train_cumG`, group = 1, color = "%train_cumG"), size = 1) +
      geom_point(aes(x = which.max(ks$`train_K-S`),
                     y = as.numeric(ks$`%train_cumG`[which.max(ks$`train_K-S`)])),
                 size = 2, shape = 21, fill = 'white', color = "#085A9C") +
      geom_segment(aes(x = which.max(ks$`train_K-S`),
                       y = as.numeric(ks$`%train_cumG`[which.max(ks$`train_K-S`)]) + 0.01,
                       xend = which.max(ks$`train_K-S`),
                       yend = as.numeric(ks$`%train_cumB`[which.max(ks$`train_K-S`)]) - 0.01),
                   colour = love_color("deep_grey"), linetype = "dashed",
                   arrow = arrow(ends = "both", length = unit(0.2, "cm"))) +
      geom_line(aes(y = `%train_cumB`, color = '%train_cumB', group = 1), size = 1) +
      geom_point(aes(x = which.max(ks$`train_K-S`),
                     y = as.numeric(ks$`%train_cumG`[which.max(ks$`train_K-S`)])),
                 size = 2, shape = 21, fill = 'white', color = '#ca3e1c') +
      geom_point(aes(x = which.max(ks$`train_K-S`),
                     y = as.numeric(ks$`%train_cumB`[which.max(ks$`train_K-S`)])),
                 size = 2, shape = 21, fill = 'white', color = '#ca3e1c') +
      annotate(geom = 'text', x = 7, y = 0.1,
               label = paste('train K-S : ', max(round(ks$`train_K-S`, 2))),
                vjust = 1.5) +
      geom_line(aes(y = `%test_cumG`, group = 1, color = "%test_cumG"),
                linetype = "dashed", size = 1) +
      geom_point(aes(x = which.max(ks$`test_K-S`),
                     y = as.numeric(ks$`%test_cumG`[which.max(ks$`test_K-S`)])),
                 size = 2, shape = 21, fill = 'white', color = "#085A9C") +
      geom_segment(aes(x = which.max(ks$`test_K-S`),
                       y = as.numeric(ks$`%test_cumG`[which.max(ks$`test_K-S`)]) + 0.01,
                       xend = which.max(ks$`test_K-S`),
                       yend = as.numeric(ks$`%test_cumB`[which.max(ks$`test_K-S`)]) - 0.01),
                   colour = love_color("deep_grey"), linetype = "dashed",
                   arrow = arrow(ends = "both", length = unit(0.2, "cm"))) +
      geom_line(aes(y = `%test_cumB`, color = '%test_cumB', group = 1),
                linetype = "dashed", size = 1) +
      geom_point(aes(x = which.max(ks$`test_K-S`),
                     y = as.numeric(ks$`%test_cumG`[which.max(ks$`test_K-S`)])),
                 size = 2, shape = 21, fill = 'white', color = '#ca3e1c') +
      geom_point(aes(x = which.max(ks$`test_K-S`),
                     y = as.numeric(ks$`%test_cumB`[which.max(ks$`test_K-S`)])),
                 size = 2, shape = 21, fill = 'white', color = '#ca3e1c') +
      annotate(geom = 'text', x = 7, y = 0.15,vjust = 1.5,
               label = paste('test K-S : ', max(round(ks$`test_K-S`, 2)))) +
      annotate("text", vjust = -1, label = "1", size = 4, colour = "black",
               x = which.max(ks$`train_K-S`) - 1,
               y = as.numeric(ks$`%train_cumB`[which.max(ks$`train_K-S`)])) +
      annotate("text", vjust = 1, label = "0", size = 4, colour = "black",
               x = which.max(ks$`test_K-S`) + 1,
               y = as.numeric(ks$`%test_cumG`[which.max(ks$`test_K-S`)])) +
      scale_colour_manual(values = c("%train_cumG" = love_color("dark_blue"),
                                     "%test_cumG" = love_color("dark_green"),
                                     "%train_cumB" = love_color("dark_red"),
                                     "%test_cumB" = love_color("dark_purple"))) +
      labs(x = "% of Total", y = "% of CumSum G/B",
           title = paste(gtitle,"K-S : Train vs. Test" ))+
      plot_theme(legend.position = "top", angle = 0)

    ts_total <- data.table::melt(as.data.table(tb_ks[c("bins", "%train", "%test")]), id.vars = c("bins"),
                                 variable.name = "train_test", value.name = "value")
    ts_1 <- data.table::melt(as.data.table(tb_ks[c("bins", "%train_B", "%test_B")]), id.vars = c("bins"),
                               variable.name = "train_test", value.name = "value")

    psi_plot <- ggplot(ts_total, aes(x = bins, y = value, fill = train_test)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
      geom_text(aes(y = value, label = paste(as_percent(value, digits = 3))),
                position = position_dodge(width = 0.7),
                size = 3, vjust = 1, hjust = 0.3, colour = "white") +
      geom_line(aes(x = factor(ts_1[[1]]),
                    y = as.numeric(ts_1$value) * max(ts_total$value) * 4,
                    color = ts_1$train_test,
                    linetype = ts_1$train_test,
                    group = ts_1$train_test),
                position = position_dodge(width = 0.5),size = 1) +
      geom_point(aes(y = as.numeric(ts_1$value) * max(ts_total$value) * 4,
                     color = ts_1$train_test,
                     group = ts_1$train_test),
                 position = position_dodge(width = 0.5),
                 fill = 'white', size = 2, shape = 21) +
      geom_text(aes(y = as.numeric(ts_1$value) * max(ts_total$value) * 4,
                    label = paste(as_percent(ts_1$value, digits = 3))),
                position = position_dodge(width = 0.5),
                colour = 'black', size = 3, vjust = -0.1) +
      annotate(geom = 'text',
             x = dim(ts_total)[1] / 3,
             y = max(c(ts_total$value, as.numeric(ts_1$value) * max(ts_total$value) * 4)) + 0.09,
             label = paste('PSI', round(sum(tb_ks$PSI,na.rm = TRUE),4), sep = " : ")) +
      scale_fill_manual(values = c('%train' = love_color("deep_grey"),
                                   '%test' = love_color("light_yellow"))) +
      scale_color_manual(values = c('%train_B' = love_color("shallow_red"),
                                    '%test_B' = love_color("sky_blue"))) +
      ylim(c(-0.01, max(c(ts_total$value, as.numeric(ts_1$value) * max(ts_total$value) * 4)) + 0.1)) +
      guides(fill = guide_legend(reverse = TRUE)) +
      xlab(score) +
      ylab("Total Percent") +
      ggtitle(paste(gtitle,"Train and Test Distribution"))+
      plot_theme(legend.position = "top",
                 angle = ifelse(nrow(ts_total) > 10, 50,
                                ifelse(nrow(ts_total) > 5, 30, 0)))
    if (save_data) {
        dir_path = ifelse(is.null(dir_path),
                      tempdir(), dir_path)
        if (!dir.exists(dir_path)) dir.create(dir_path)
        if ( !is.character(gtitle)) { gtitle = paste0("mymodel") }
        ggsave(filename = paste0(dir_path, "/", paste(paste0(gtitle, "_ks_psi_plot"), "png", sep = '.')),
           device = "png",
           plot = multi_grid(grobs = list(ks_plot, psi_plot), ncol = 2, nrow = 1),
           dpi = "retina", width = g_width, height = g_width / 2)
    }
    if (plot_show) {
        ks_psi_plot = multi_grid(grobs = list(ks_plot, psi_plot), ncol = 2, nrow = 1)
        return(plot(ks_psi_plot))
    }
    options(opt) # reset
}

#' Correlation Plot
#'
#' \code{cor_plot} is for ploting correlation matrix
#' @param dat A data.frame with independent variables and target variable.
#' @param x_list Names of independent variables.
#' @param plot_show  Logical, show graph in current graphic device.
#' @param save_data Logical, save results in locally specified folder. Default is TRUE
#' @param gtitle The title of the graph & The name for periodically saved graphic file. Default is "_correlation_of_variables".
#' @param dir_path The path for periodically saved graphic files. Default is "./model/LR"
#' @examples
#' train_test <- train_test_split(UCICreditCard,
#' split_type = "Random", prop = 0.8,save_data = FALSE)
#' dat_train = train_test$train
#' dat_test = train_test$test
#' cor_plot(dat_train[,8:12],plot_show = TRUE)
#' @import ggplot2
#' @export

cor_plot <- function(dat, dir_path = tempdir(), x_list = NULL,
                    gtitle = NULL, save_data = FALSE, plot_show = FALSE) {
    if (!is.null(x_list)) {
        dat = dat[, c(x_list)]
    }
    num_x_list = get_names(dat = dat,
                           types = c('numeric', 'integer', 'double'),
                           ex_cols = "",
                           get_ex = FALSE)
    cor_mat = cor(dat[, num_x_list],use = "complete.obs")
    if (save_data) {
        save_data(cor_mat, file_name = paste(gtitle, "correlation_matrix"),
                dir_path = dir_path, note = FALSE, row_names = TRUE)
    }
	if ( !is.character(gtitle)) { gtitle = paste0("Correlation Matrix") }else{gtitle = paste(gtitle, 'Correlation Matrix')}
    cor_p = cor_heat_plot(cor_mat,title = gtitle)
    if (save_data) {
        dir_path = ifelse(!is.character(dir_path) ,
                      tempdir(), dir_path)
        if (!dir.exists(dir_path)) dir.create(dir_path)

        ggsave(paste0(dir_path, "/", paste(paste0(gtitle, "_correlation_of_variables"), "png", sep = '.')), cor_p, dpi = "retina", width = 8)
    }
    if (plot_show) {
        plot(cor_p)
    }
}

#' Correlation Heat Plot
#'
#' \code{cor_heat_plot} is for ploting correlation matrix
#' @param cor_mat A correlation matrix.
#' @param low_color color of the lowest correlation between variables.
#' @param high_color  color of the highest correlation between variables.
#' @param title title of plot.
#' @examples
#' train_test <- train_test_split(UCICreditCard,
#' split_type = "Random", prop = 0.8,save_data = FALSE)
#' dat_train = train_test$train
#' dat_test = train_test$test
#' cor_mat = cor(dat_train[,8:12],use = "complete.obs")
#' cor_heat_plot(cor_mat)
#' @import ggplot2
#' @importFrom dplyr %>%
#' @export

cor_heat_plot <- function(cor_mat, low_color = love_color('deep_red'),
                      high_color = love_color('light_cyan'), title = 'Correlation Matrix') {

    if (!isTRUE(all.equal(cor_mat, t(cor_mat)))) stop("correlation matrix is not symmetric")

    cor1 = data.frame(which(abs(cor_mat) > 0, arr.ind = TRUE))
    cor2 = sapply(1:nrow(cor1),
               function(x) { cbind(
                        colnames(cor_mat)[cor1[x,][[1]]],
                       colnames(cor_mat)[cor1[x,][[2]]],
                       cor_mat[cor1[x,][[1]], cor1[x,][[2]]])
               }) %>% t() %>% data.frame(stringsAsFactors = FALSE)

    cor_list = cbind(cor1, cor2)
    names(cor_list) = c("Vars1", "Vars2", "V1", "V2", "cor")
    cor_list$cor = as.numeric(cor_list$cor)

    ggplot(cor_list, aes(reorder(cor_list$Vars2, cor_list$Vars2),
                    reorder(cor_list$Vars1, cor_list$Vars1),
                    fill = cor_list$cor)) +
    geom_tile(colour = 'white') +
    geom_text(aes(label = round(cor_list$cor, 2)),
            size = ifelse(length(unique(cor_list$Vars2)) < 10, 3,
                          ifelse(length(unique(cor_list$Vars2)) > 20, 2, 2.5))) +
    scale_fill_gradient2(limits = c(-1, 1),
                       low = low_color, mid = 'white',
                       high = high_color,
                       midpoint = 0,
                       na.value = love_color('pale_grey')) +
    scale_y_discrete(limits = rev(unique(cor_list$Vars1)), labels = unique(cor_list$V1)) +
    scale_x_discrete(position = "top", labels = unique(cor_list$V2)) +
    labs(x = "", y = "", title = title) +
    theme(text = element_text(size = 15), rect = element_blank()) +
    plot_theme(legend.position = 'right', angle = 90)
}



#' model_key_index
#' \code{model_key_index} is for get plots for a  variable.
#' @param tb_pred  A table generated by code{\link{ks_table}}
#' @rdname ks_table
#' @export
model_key_index <- function(tb_pred) {
    key_index = NULL
    if (any(is.element(c("train_K-S", "test_K-S", "PSI"), names(tb_pred)))) {
        b_psi = as.numeric(tb_pred[nrow(tb_pred), "PSI"])
        train_KS = as.numeric(tb_pred[nrow(tb_pred), "train_K-S"])
        test_KS = as.numeric(tb_pred[nrow(tb_pred), "test_K-S"])
        key_index = data.frame(train_KS = train_KS, test_KS = test_KS, PSI = b_psi)
    } else {
        key_index = data.frame(train_KS = NA, test_KS = NA, PSI = NA)
    }
    return(key_index)
}


#' love_color
#'
#' \code{love_color} is for get plots for a  variable.
#' @param color The name of colors.
#' @param type The type of colors, "deep".
#' @param all all of colors.
#' @examples
#' love_color(color="dark_cyan")
#' @import ggplot2
#' @export

love_color <- function(color =NULL, type = NULL,all = FALSE) {
  color_board <- c(
    dark_cyan = rgb(20, 78, 100, maxColorValue = 255),
    deep_cyan = rgb(0, 93, 125, maxColorValue = 255),
    light_cyan = rgb(34, 116, 135, maxColorValue = 255),
    green_cyan = rgb(34, 116, 135, maxColorValue = 255),
    shallow_cyan = rgb(60, 140, 190, maxColorValue = 255),
    pale_cyan = rgb(0, 147, 198, maxColorValue = 255),
    dark_red= rgb(181, 0, 0, maxColorValue = 255),
    deep_red  = rgb(154, 42, 42, maxColorValue = 255),
    light_red = rgb(174, 61, 63, maxColorValue = 255),
    shallow_red = rgb(252, 74, 42, maxColorValue = 255),
    pale_red = rgb(220, 63, 56, maxColorValue = 255),
    shallow_red2 = rgb(220, 63, 56, maxColorValue = 255),
    dark_green=  rgb(0, 100, 50, maxColorValue = 255),
    deep_green = rgb(10, 149, 136, maxColorValue = 255),
    light_green =  rgb(84, 169, 84, maxColorValue = 255),
    shallow_green =rgb(66, 192, 46, maxColorValue = 255),
    pale_green = rgb(180, 202, 198, maxColorValue = 255),
	deep_orange = rgb(255, 140, 0, maxColorValue = 255),
    light_orange = rgb(241, 156, 0, maxColorValue = 255),
    shallow_orange = rgb(241, 121, 0, maxColorValue = 255),
    dark_grey = rgb(102, 102, 102, maxColorValue = 255),
    deep_grey = rgb(128, 129, 128, maxColorValue = 255),
    light_grey = rgb(169, 169, 169, maxColorValue = 255),
    shallow_grey = rgb(191, 192, 191, maxColorValue = 255),
    pale_grey = rgb(240, 240, 240, maxColorValue = 255),
    dark_blue = rgb(33, 71, 117, maxColorValue = 255),
    deep_blue = rgb(32, 81, 139, maxColorValue = 255),
    deep_blue = rgb(80, 99, 139, maxColorValue = 255),
    light_blue = rgb(0, 91, 181, maxColorValue = 255),
    shallow_blue = rgb(0, 142, 213, maxColorValue = 255),
    sky_blue = rgb(0, 142, 213, maxColorValue = 255),
    pale_blue = rgb(144, 190, 216, maxColorValue = 255),
    water_blue = rgb(144, 190, 216, maxColorValue = 255),
   dark_purple = rgb(120, 78, 100, maxColorValue = 255),
    deep_purple = rgb(170, 44, 105, maxColorValue = 255),
    light_purple = rgb(212, 137, 168, maxColorValue = 255),
    dark_purple2 = rgb(71, 0, 123, maxColorValue = 255),
    gold_yellow = rgb(255, 215, 0, maxColorValue = 255),
    light_yellow = rgb(207, 177, 81, maxColorValue = 255),
	 red_line = rgb(232, 49 ,50 , maxColorValue = 255 ),
	 orange_line = rgb(236, 146, 23  , maxColorValue = 255 ),
	green_line = rgb(16, 174, 181 , maxColorValue = 255 ),
	grey_line = rgb(77 ,80, 84  , maxColorValue = 255 )
  )
  if(!is.null(color)&& all(is.element(color, names(color_board)))){
    colors = color_board[[color]]
  }else{
    if(!is.null(type)){
      color_index = grep(type,names(color_board))
      colors = as.vector(color_board[color_index])
    }else{
      if(all){
        colors = as.vector(color_board)
      }else{
        colors = color_board[["dark_cyan"]]
      }
    }
  }
  return(colors)
}



#' plot_theme
#'
#' \code{plot_theme} is a simper wrapper of theme for ggplot2.
#' @param legend.position see details at: code{legend.position}
#' @param angle see details at:  code{axis.text.x}
#' @param legend_size  see details at:  code{legend.text}
#' @param axis_size_x see details at:  code{axis.text.x}
#' @param axis_size_y see details at:  code{axis.text.y}
#' @param axis_title_size see details at:  code{axis.title.x}
#' @param title_size see details at:  code{plot.title}
#' @param title_vjust see details at:  code{plot.title}
#' @param title_hjust see details at:  code{plot.title}
#' @param linetype see details at:  code{panel.grid.major}
#' @param face see details at:  code{axis.title.x}
#' @details see details at: code{theme}
#' @import ggplot2
#' @export

plot_theme <- function(legend.position = "top", angle = 30,
                       legend_size = 7, axis_size_y = 8,
                       axis_size_x = 8, axis_title_size = 10,
                       title_size = 11, title_vjust = 0, title_hjust = 0,
                       linetype = "dotted", face = "bold") {
    plot_theme <- theme_bw() + theme(legend.position = legend.position,
                              panel.border = element_blank(),
                              panel.grid.major = element_line(linetype = linetype),
                              panel.grid.minor = element_blank(),
                              plot.title = element_text(face = "bold", size = title_size,
                                                        vjust = title_vjust, hjust = title_hjust),
                              legend.title = element_blank(),
                              legend.text = element_text(size = legend_size),
                              legend.key = element_blank(),
                              axis.text.x = element_text(size = axis_size_x,
                                                         vjust = 0.5, angle = angle),
                              axis.text.y = element_text(size = axis_size_y),
                              axis.title.x = element_text(size = axis_title_size, face = face),
                              axis.title.y = element_text(size = axis_title_size),
                              strip.text = element_text(size = 10, face = face),
                              strip.background = element_blank())
    return(plot_theme)
}


#' model result plots
#' \code{model_result_plot} is a wrapper of following:
#' \code{perf_table} is for generating a model performance table.
#' \code{ks_plot} is for K-S.
#' \code{roc_plot} is for ROC.
#' \code{lift_plot} is for Lift Chart.
#' \code{score_distribution_plot} is for ploting the score distribution.
#' @param train_pred A data frame of training with predicted prob or score.
#' @param test_pred A data frame of validation with predict prob or score.
#' @param target The name of target variable.
#' @param score The name of prob or score variable.
#' @param g Number of breaks for prob or score.
#' @param cut_bin A string, if equal_bins is TRUE, 'equal_depth' or 'equal_width', default is 'equal_depth'.
#' @param perf_tb Performance table.
#' @param gtitle The title of the graph & The name for periodically saved graphic file.
#' @param breaks Splitting points of prob or score.
#' @param pos_flag The value of positive class of target variable, default: "1".
#' @param total Whether to summarize the table. default: TRUE.
#' @param plot_show Logical, show model performance in current graphic device. Default is TRUE.
#' @param save_data Logical, save results in locally specified folder. Default is FALSE.
#' @param perf_dir_path The path for periodically saved graphic files.
#' @examples
#' sub = cv_split(UCICreditCard, k = 30)[[1]]
#' dat = UCICreditCard[sub,]
#' dat = re_name(dat, "default.payment.next.month", "target")
#' x_list = c("PAY_0", "LIMIT_BAL", "PAY_AMT5", "PAY_3", "PAY_2")
#' dat = data_cleansing(dat, target = "target", obs_id = "ID",x_list = x_list,
#' occur_time = "apply_date", miss_values = list("", -1))
#' dat = process_nas(dat,default_miss = TRUE)
#' train_test <- train_test_split(dat, split_type = "OOT", prop = 0.7,
#'                                 occur_time = "apply_date")
#' dat_train = train_test$train
#' dat_test = train_test$test

#' Formula = as.formula(paste("target", paste(x_list, collapse = ' + '), sep = ' ~ '))
#' set.seed(46)
#' lr_model = glm(Formula, data = dat_train[, c("target", x_list)], family = binomial(logit))
#'
#' dat_train$pred_LR = round(predict(lr_model, dat_train[, x_list], type = "response"), 5)
#' dat_test$pred_LR = round(predict(lr_model, dat_test[, x_list], type = "response"), 5)
#' # model evaluation
#' perf_table(train_pred = dat_train, test_pred = dat_test, target = "target", score = "pred_LR")
#' ks_plot(train_pred = dat_train, test_pred = dat_test, target = "target", score = "pred_LR")
#' roc_plot(train_pred = dat_train, test_pred = dat_test, target = "target", score = "pred_LR")
#' #lift_plot(train_pred = dat_train, test_pred = dat_test, target = "target", score = "pred_LR")
#' #score_distribution_plot(train_pred = dat_train, test_pred = dat_test,
#' #target = "target", score = "pred_LR")
#' #model_result_plot(train_pred = dat_train, test_pred = dat_test,
#' #target = "target", score = "pred_LR")
#' @import ggplot2
#' @importFrom dplyr group_by mutate summarize  summarise n  count %>% filter
#' @importFrom data.table melt dcast
#' @export


model_result_plot <- function(train_pred , score, target ,test_pred= NULL, gtitle = NULL,perf_dir_path = NULL,
                             save_data = FALSE,plot_show = TRUE,total = TRUE,g = 10,cut_bin = 'equal_depth' ){

  opt = options('warn' = -1,scipen = 200, stringsAsFactors = FALSE, digits = 6) #
  train_pred = checking_data(dat = train_pred, target = target)
  train_pred = train_pred[which(complete.cases(train_pred[, score])),]
  if(!is.null(test_pred)){
    test_pred = checking_data(dat = test_pred, target = target)
	test_pred = test_pred[which(complete.cases(test_pred[, score])),]
  }

  perf_tb = perf_table(train_pred = train_pred, test_pred = test_pred, target = target,
                       score = score, total = total,
                       g = g,cut_bin = cut_bin)

  tb_ks =  plot_table(grid_table = perf_tb, theme = c("cyan"),
                     title= paste0(gtitle, ".performance_table"),title.size = 12,title.color = 'black',
                     title.face = "bold", title.position='middle',
                     subtitle= NULL,subtitle.size = 8,subtitle.color = 'black',subtitle.face = "plain",
                     subtitle.position='middle',
                     tile.color = 'grey80',tile.size = 1,
                     colname.size = 3,colname.color = 'white',colname.face = 'bold',
                     colname.fill.color =love_color("dark_cyan"),
                     text.size = 3,text.color =love_color("dark_grey"),
                     text.face = 'plain', text.fill.color = c('white',love_color("pale_grey")))
  perf_tb = perf_table(train_pred = train_pred, test_pred = test_pred, target = target,
                       score = score, total = FALSE,
                       g = g,cut_bin = cut_bin)
  dis_pl = score_distribution_plot(train_pred = train_pred, test_pred = test_pred,
           target = target, score =score, gtitle = gtitle,g = g,perf_tb = perf_tb)

  ks_pl = ks_plot(train_pred = train_pred, test_pred = test_pred, target = target, score = score, gtitle = gtitle, g = g, perf_tb = perf_tb)
  roc_pl = roc_plot(train_pred = train_pred, test_pred = test_pred, target = target, score = score, gtitle = gtitle)
  lift_pl = lift_plot(train_pred = train_pred, test_pred = test_pred, target = target, score = score, gtitle = gtitle,g = g,perf_tb = perf_tb)

  if(save_data){
    perf_dir_path = ifelse(is.null(perf_dir_path), tempdir(), perf_dir_path)
    if (!dir.exists(perf_dir_path)) dir.create(perf_dir_path)

    ggsave(filename = paste0(perf_dir_path, "/", paste(paste0(gtitle, "_ks"), "png", sep = '.')), device = "png",
           plot = ks_pl, dpi = "retina", width = 5, height = 4.5)
    ggsave(filename = paste0(perf_dir_path, "/", paste(paste0(gtitle, "_roc"), "png", sep = '.')), device = "png",
           plot = roc_pl, dpi = "retina", width = 5, height = 4.5)
    ggsave(filename = paste0(perf_dir_path, "/", paste(paste0(gtitle, "_psi"), "png", sep = '.')), device = "png",
           plot = dis_pl, dpi = "retina", width = 5, height = 4.5)
    ggsave(paste0(perf_dir_path, "/", paste(paste0(gtitle, ".performance_table"), "png", sep = '.')),
           plot = tb_ks, dpi = "retina", width = 12)
    save_data(perf_tb, file_name = paste0(gtitle, ".performance_table"), dir_path = perf_dir_path,note = TRUE)
    ggsave(filename = paste0(perf_dir_path, "/", paste(paste0(gtitle, ".lift"), "png", sep = '.')), device = "png",
           plot = lift_pl, dpi = "retina", width = 5, height = 4.5)

  }
  if (plot_show) {
    plot(multi_grid(grobs = list(ks_pl,lift_pl,roc_pl, dis_pl), ncol = 2, nrow = 2))
  }
  return(perf_tb)
  options(opt)
}

#' performance table
#'
#' @rdname model_result_plot
#' @export

perf_table <- function(train_pred, test_pred = NULL, target = NULL, score = NULL,
                       g = 10, cut_bin = 'equal_depth', breaks = NULL,
                       pos_flag = list("1", "1", "Bad", 1),total = FALSE) {
  opt = options('warn' = -1,scipen = 200, stringsAsFactors = FALSE, digits = 6) #
  `train_GB_index` = `test_GB_index` = `G` = `bins` = `B` = `%train` = `%test` = `#train` = `#test` = NULL
  `%test_B` = `%test_cumB` = `%test_cumG` = `%train_B` = `%train_cumB` = `%train_cumG` = NULL

  train_pred <- train_pred[which(complete.cases(train_pred[, score])),]
  if (is.null(target)) { stop("target is missing!\n") }
  if (is.null(score)) { stop("score is missing!\n") }
  if (is.null(breaks)) {
    breaks = get_breaks(dat = train_pred, x = score,
                        target = target, equal_bins = TRUE,
                        cut_bin = cut_bin,
                        best = FALSE, g = g, note = FALSE)
  }
  train_pred$bins = split_bins(dat = train_pred, x = score, breaks = breaks, bins_no = TRUE)

  if (!is.null(target)) {
    if (length(unique(train_pred[, target])) > 1) {
      if (is.null(pos_flag)) {
        train_pred$target = ifelse(train_pred[, target] %in% list("1", "1", "Bad", 1), "B", "G")
      } else {
        train_pred$target = ifelse(train_pred[, target] %in% pos_flag, "B", "G")
        if (length(unique(train_pred$target)) == 1) {
          stop(paste("The value in pos_flag is not one of the value of train_pred's target.\n"))
        }
      }
    } else {
      stop(paste("The value of train_pred's target is unique.\n"))
    }
  } else {
    stop(paste("The target variable is missing.\n"))
  }

  train_sum = train_pred %>%
    dplyr::filter(train_pred$target %in% c("B", "G")) %>%
    dplyr::group_by(bins) %>%
    dplyr::count(bins, target) %>%
    dplyr::mutate(percent = n / sum(n)) %>%
    as.data.table()
  train_sum = data.table::dcast(train_sum[,c("bins", "target", "n"),with = FALSE],
                                 bins ~ target, value.var = "n")
   train_sum = quick_as_df(train_sum)
  train_sum[is.na(train_sum)] <- 0
  f_bad = sum(train_sum[1:floor(nrow(train_sum)/2), "B"],na.rm = TRUE)
  f_good = sum(train_sum[1:floor(nrow(train_sum)/2), "G"],na.rm = TRUE)
  l_bad = sum(train_sum[(floor(nrow(train_sum)/2)+1):nrow(train_sum), "B"],na.rm = TRUE)
  l_good = sum(train_sum[(floor(nrow(train_sum)/2)+1):nrow(train_sum), "G"],na.rm = TRUE)
  f_bad_rate = f_bad /(f_bad + f_good)
  l_bad_rate = l_bad /(l_bad + l_good)
  if (f_bad_rate < l_bad_rate) {
    train_sum_lift = train_sum[order(train_sum$bins, decreasing = TRUE),]
    train_Lift = round((cumsum(train_sum_lift$B) / cumsum(train_sum_lift$G + train_sum_lift$B)) / (sum(train_sum_lift$B) / sum(train_sum_lift$G + train_sum_lift$B)), 2)
    train_Lift = sort(train_Lift)
  } else {
    train_Lift = round((cumsum(train_sum$B) / cumsum(train_sum$G + train_sum$B)) / (sum(train_sum$B) / sum(train_sum$G + train_sum$B)), 2)
    train_Lift = sort(train_Lift, decreasing = TRUE)
  }

  train_ks <- transform(train_sum,
                        train_total = G + B,
                        `%train_total` = round((G + B) / sum(G + B), 2),
                        `%train_B` = round(B / (G + B), 3),
                        `%train_cumG` = round(cumsum(G) / sum(G), 2),
                        `%train_cumB` = round(cumsum(B) / sum(B), 2),
                        `train_K-S` = abs(round((cumsum(B) / sum(B)) - (cumsum(G) / sum(G)), 2)),
                        train_Lift = train_Lift
  )
  if (!is.null(test_pred) || length(test_pred) > 1) {
    test_pred <- test_pred[which(complete.cases(test_pred[, score])),]
    test_pred$bins = split_bins(dat = test_pred, x = score, breaks = breaks, bins_no = TRUE)
    if (!is.null(target)) {
      if (length(unique(test_pred[, target])) > 1) {
        if (is.null(pos_flag)) {
          test_pred$target = ifelse(test_pred[, target] %in% list("1", "1", "Bad", 1), "B", "G")
        } else {
          test_pred$target = ifelse(test_pred[, target] %in% pos_flag, "B", "G")
          if (length(unique(test_pred$target)) == 1) {
            stop(paste("The value in pos_flag is not one of the value of test_pred's target.\n"))
          }
        }
      } else {
        stop(paste("The value of test_pred's target is unique.\n"))
      }
    } else {
      stop(paste("The target variable is missing.\n"))
    }

    test_sum <- test_pred %>%
      dplyr::filter(test_pred$target %in% c("B", "G")) %>%
      dplyr::group_by(bins) %>%
      dplyr::count(bins, target) %>%
      dplyr::mutate(percent = n / sum(n)) %>%
      as.data.table()

    test_sum <- data.table::dcast(test_sum[,c("bins", "target", "n"),with = FALSE],
                                  bins ~ target, value.var = "n")
	test_sum = quick_as_df(test_sum)
    test_sum[is.na(test_sum)] <- 0
    f_t_bad = sum(test_sum[1:floor(nrow(test_sum)/2), "B"],na.rm = TRUE)
    f_t_good = sum(test_sum[1:floor(nrow(test_sum)/2), "G"],na.rm = TRUE)
    l_t_bad = sum(test_sum[(floor(nrow(test_sum)/2)+1):nrow(test_sum), "B"],na.rm = TRUE)
    l_t_good = sum(test_sum[(floor(nrow(test_sum)/2)+1):nrow(test_sum), "G"],na.rm = TRUE)
    f_t_bad_rate = f_t_bad /(f_t_bad + f_t_good)
    l_t_bad_rate = l_t_bad /(l_t_bad + l_t_good)
    if (f_t_bad_rate < l_t_bad_rate) {
      test_sum_lift = test_sum[order(test_sum$bins, decreasing = TRUE),]
      test_Lift = round((cumsum(test_sum_lift$B) / cumsum(test_sum_lift$G + test_sum_lift$B)) / (sum(test_sum_lift$B) / sum(test_sum_lift$G + test_sum_lift$B)), 2)
      test_Lift = sort(test_Lift)
    } else {
      test_Lift = round((cumsum(test_sum$B) / cumsum(test_sum$G + test_sum$B)) / (sum(test_sum$B) / sum(test_sum$G + test_sum$B)), 2)
      test_Lift = sort(test_Lift, decreasing = TRUE)
    }


    test_ks <- transform(test_sum,
                         test_total = G + B,
                         `%test_total` = round((G + B) / (sum(G + B)), 2),
                         `%test_B` = round(B / (G + B), 3),
                         `%test_cumG` = round(cumsum(G) / sum(G), 2),
                         `%test_cumB` = round(cumsum(B) / sum(B), 2),
                         `test_K-S` = abs(round((cumsum(B) / sum(B)) - (cumsum(G) / sum(G)), 2)),
                         test_Lift = test_Lift
    )

    dt_ks = merge(train_ks[c(1, 4:10)], test_ks[c(1, 4:10)], all.x = TRUE)
    dt_ks[is.na(dt_ks)] <- 0
    names(dt_ks) = c("bins", "#train", "%train", "%train_B",
                     "%train_cumG", "%train_cumB", "train_K-S", "train_Lift",
                     "#test", "%test", "%test_B", "%test_cumG",
                     "%test_cumB", "test_K-S", "test_Lift")
    dt_ks = dt_ks %>% dplyr::mutate(PSI = ifelse(`%train` == 0 | `%test` == 0, 1, round((`%train` - `%test`) * log(`%train` / `%test`), 3)), `#total` = `#train` + `#test`)
    dt_ks = dt_ks[c("bins", "#total", "#train", "#test", "%train", "%test", "%train_B",
                    "%test_B", "%train_cumG", "%train_cumB", "%test_cumG", "%test_cumB",
                    "train_K-S", "test_K-S", "train_Lift", "test_Lift", "PSI")]



    if(total){
      total <- c("Total",
                 sum(dt_ks$`#total`,na.rm = TRUE),
                 sum(dt_ks$`#train`, na.rm = TRUE),
                 sum(dt_ks$`#test`, na.rm = TRUE),
                 as_percent(sum(dt_ks$`#train`, na.rm = TRUE) / sum(dt_ks$`#total`, na.rm = TRUE), 2),
                 as_percent(sum(dt_ks$`#test`, na.rm = TRUE) / sum(dt_ks$`#total` , na.rm = TRUE), 2),
                 as_percent(sum(dt_ks$`%train_B` * dt_ks$`#train`, na.rm = TRUE) / sum(dt_ks$`#train`,  na.rm = TRUE), 2),
                 as_percent(sum(dt_ks$`%test_B` * dt_ks$`#test`,na.rm = TRUE) / sum(dt_ks$`#test`, na.rm = TRUE), 2), "100%", "100%",
                 "100%", "100%",
                 max(dt_ks$`train_K-S`,na.rm = TRUE),
                 max(dt_ks$`test_K-S`, na.rm = TRUE),
                 max(dt_ks$`train_Lift`,na.rm = TRUE),
                 max(dt_ks$`test_Lift`, na.rm = TRUE),
                 sum(dt_ks$PSI,na.rm = TRUE))

      dt_ks <- dt_ks[c("bins", "#total", "#train", "#test", "%train", "%test", "%train_B",
                       "%test_B", "%train_cumG", "%train_cumB", "%test_cumG",
                       "%test_cumB", "train_K-S", "test_K-S","train_Lift", "test_Lift", "PSI")]
      dt_ks <- transform(dt_ks,
                         `%train` = as_percent(`%train`, digits = 2),
                         `%test` = as_percent(`%test`, digits = 2),
                         `%train_B` = as_percent(`%train_B`, digits = 3),
                         `%test_B` = as_percent(`%test_B`, digits = 3),
                         `%train_cumG` = as_percent(`%train_cumG`, digits = 2),
                         `%train_cumB` = as_percent(`%train_cumB`, digits = 2),
                         `%test_cumG` = as_percent(`%test_cumG`, digits = 2),
                         `%test_cumB` = as_percent(`%test_cumB`, digits = 2)
      )

      dt_ks <- rbind(dt_ks, total)
      names(dt_ks) = c("bins", "#total", "#train", "#test", "%train", "%test", "%train_B",
                       "%test_B", "%train_cumG", "%train_cumB", "%test_cumG",
                       "%test_cumB", "train_K-S", "test_K-S","train_Lift", "test_Lift", "PSI")
    }

  } else {
    dt_ks = train_ks[c(1, 4:10)]
    dt_ks[is.na(dt_ks)] <- 0
    names(dt_ks) = c("bins", "#Pop", "%Pct", "%Pct_1",
                     "%Cumsum_0", "%Cumsum_1", "K-S", "Lift")

    dt_ks = dt_ks[c("bins", "#Pop", "%Pct", "%Pct_1",
                    "%Cumsum_0", "%Cumsum_1", "K-S", "Lift")]
  }
  return(dt_ks)
  options(opt) # reset
}


#' ks_plot
#'
#' @rdname model_result_plot
#' @export

ks_plot <- function(train_pred, test_pred = NULL, target = NULL, score =  NULL,
                        gtitle = NULL, breaks = NULL, g = 10,cut_bin = 'equal_depth',perf_tb = NULL) {
    opt = options('warn' = -1,scipen = 200, stringsAsFactors = FALSE, digits = 6) #
    `value` = `train_test` = `bins` = `%train_cumG` = `%train_cumB` = `%test_cumG` = `%test_cumB`= `%Cumsum_0`= `%Cumsum_1` = NULL
	train_pred <- train_pred[which(complete.cases(train_pred[, score])),]
    if (is.null(gtitle)) { gtitle = paste0("Model") }
    if (is.null(target)) { stop("target is missing!\n")}
    if (is.null(score)) { stop("score is missing!\n")}
    if(is.null(perf_tb)){
	    tb_ks = perf_table(train_pred = train_pred, test_pred = test_pred, target = target, score = score, g = g, breaks = breaks,cut_bin = cut_bin)
	}else{
	tb_ks = perf_tb
	}


    if (!is.null(test_pred) || length(test_pred) > 1) {
	   	test_pred <- test_pred[which(complete.cases(test_pred[, score])),]
        ks = rbind(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0), tb_ks)
        ks_pl = ggplot(ks, aes(x = reorder(as.factor(as_percent(round(cumsum(ks$`%train`), 1))), cumsum(ks$`%train`)))) +
		geom_line(aes(y = `%train_cumG`, group = 1, color = "%train_cumG"), size = 1) +
        geom_point(aes(x = which.max(ks$`train_K-S`),
                     y = as.numeric(ks$`%train_cumG`[which.max(ks$`train_K-S`)])),
                 size = 2, shape = 21, fill = 'white', color = "#085A9C") +
		geom_hline(yintercept = 1) + geom_hline(yintercept = 0) +
        geom_segment(aes(x = which.max(ks$`train_K-S`),
                       y = as.numeric(ks$`%train_cumG`[which.max(ks$`train_K-S`)]) + 0.01,
                       xend = which.max(ks$`train_K-S`),
                       yend = as.numeric(ks$`%train_cumB`[which.max(ks$`train_K-S`)]) - 0.01),
                   colour = love_color("deep_grey"), linetype = "dashed",
                   arrow = arrow(ends = "both", length = unit(0.2, "cm"))) +
        geom_line(aes(y = `%train_cumB`, color = '%train_cumB', group = 1), size = 1) +
        geom_point(aes(x = which.max(ks$`train_K-S`),
                     y = as.numeric(ks$`%train_cumG`[which.max(ks$`train_K-S`)])),
                 size = 2, shape = 21, fill = 'white', color = '#ca3e1c') +
        geom_point(aes(x = which.max(ks$`train_K-S`),
                     y = as.numeric(ks$`%train_cumB`[which.max(ks$`train_K-S`)])),
                 size = 2, shape = 21, fill = 'white', color = '#ca3e1c') +
        annotate(geom = 'text', x = 5, y = 0.2,
               label = paste('train K-S : ', max(round(ks$`train_K-S`, 2))),
                vjust = 1.5) +
        geom_line(aes(y = `%test_cumG`, group = 1, color = "%test_cumG"),
                linetype = "dashed", size = 1) +
        geom_point(aes(x = which.max(ks$`test_K-S`),
                     y = as.numeric(ks$`%test_cumG`[which.max(ks$`test_K-S`)])),
                 size = 2, shape = 21, fill = 'white', color = "#085A9C") +
        geom_segment(aes(x = which.max(ks$`test_K-S`),
                       y = as.numeric(ks$`%test_cumG`[which.max(ks$`test_K-S`)]) + 0.01,
                       xend = which.max(ks$`test_K-S`),
                       yend = as.numeric(ks$`%test_cumB`[which.max(ks$`test_K-S`)]) - 0.01),
                   colour = love_color("deep_grey"), linetype = "dashed",
                   arrow = arrow(ends = "both", length = unit(0.2, "cm"))) +
        geom_line(aes(y = `%test_cumB`, color = '%test_cumB', group = 1),
                linetype = "dashed", size = 1) +
        geom_point(aes(x = which.max(ks$`test_K-S`),
                     y = as.numeric(ks$`%test_cumG`[which.max(ks$`test_K-S`)])),
                 size = 2, shape = 21, fill = 'white', color = '#ca3e1c') +
        geom_point(aes(x = which.max(ks$`test_K-S`),
                     y = as.numeric(ks$`%test_cumB`[which.max(ks$`test_K-S`)])),
                 size = 2, shape = 21, fill = 'white', color = '#ca3e1c') +
        annotate(geom = 'text', x = 5, y = 0.3, vjust = 1.5,
               label = paste('test K-S : ', max(round(ks$`test_K-S`, 2)))) +
        scale_colour_manual(values = c("%train_cumG" = love_color("light_blue"),
                                     "%test_cumG" = love_color("light_green"),
                                     "%train_cumB" = love_color("light_red"),
                                     "%test_cumB" = love_color("deep_orange")),
        labels = c("%test_0", "%test_1","%train_0", "%train_1")) +
        labs(x = "% of Total", y = "% of CumSum",
           title = paste(gtitle, "K-S Curve")) + theme_light() +
		   theme(legend.title = element_blank(), legend.position = "top",
		   plot.title = element_text(face = "bold", size = 11,vjust = 0, hjust = 0))
    } else {
        ks = rbind(c(0, 0, 0, 0, 0, 0, 0,0), tb_ks)
        ks_pl =  ggplot(ks, aes(x = reorder(as.factor(as_percent(round(cumsum(ks$`%Pct`), 1))), cumsum(ks$`%Pct`)))) +
        geom_line(aes(y = `%Cumsum_0`, group = 1, color = "%Cumsum_0"), size = 1) +
        geom_point(aes(x = which.max(ks$`K-S`),
                     y = as.numeric(ks$`%Cumsum_0`[which.max(ks$`K-S`)])),
                 size = 2, shape = 21, fill = 'white', color = "#085A9C") +
		geom_hline(yintercept = 1) + geom_hline(yintercept = 0) +
        geom_segment(aes(x = which.max(ks$`K-S`),
                       y = as.numeric(ks$`%Cumsum_0`[which.max(ks$`K-S`)]) + 0.01,
                       xend = which.max(ks$`K-S`),
                       yend = as.numeric(ks$`%Cumsum_1`[which.max(ks$`K-S`)]) - 0.01),
                   colour = love_color("deep_grey"), linetype = "dashed",
                   arrow = arrow(ends = "both", length = unit(0.2, "cm"))) +
        geom_line(aes(y = `%Cumsum_1`, color = '%Cumsum_1', group = 1), size = 1) +
        geom_point(aes(x = which.max(ks$`K-S`),
                     y = as.numeric(ks$`%Cumsum_0`[which.max(ks$`K-S`)])),
                 size = 2, shape = 21, fill = 'white', color = '#ca3e1c') +
        geom_point(aes(x = which.max(ks$`K-S`),
                     y = as.numeric(ks$`%Cumsum_1`[which.max(ks$`K-S`)])),
                 size = 2, shape = 21, fill = 'white', color = '#ca3e1c') +
        annotate(geom = 'text', x = 5, y = 0.2,
               label = paste('K-S : ', max(round(ks$`K-S`, 2))),
                vjust = 1.5) +
        scale_colour_manual(values = c("%Cumsum_0" = love_color("water_blue"),
                                     "%Cumsum_1" = love_color("gold_yellow")),
        labels = c("%Cumsum_0", "%Cumsum_1")) +
        labs(x = "% of Total", y = "% of CumSum",
           title = paste(gtitle, "K-S Curve")) + theme_light() +
		   theme(legend.title = element_blank(), legend.position = "top",
		   plot.title = element_text(face = "bold", size = 11, vjust = 0, hjust = 0))
    }
    return(ks_pl)
    options(opt) # reset
}



#' lift_plot
#'
#' @rdname model_result_plot
#' @export

lift_plot <- function(train_pred, test_pred = NULL, target = NULL, score = NULL,
                      gtitle = NULL,  breaks = NULL, g = 10,cut_bin = 'equal_depth',perf_tb = NULL) {
  opt = options('warn' = -1,scipen = 200, stringsAsFactors = FALSE, digits = 6) #
  `value` = `train_test` = `bins` = `%train_cumG` = `%train_cumB` = `%test_cumG` = `%test_cumB` = Lift = test_Lift = train_Lift= NULL
  if (is.null(gtitle)) { gtitle = paste0("Model") }
  if (is.null(target)) { stop("target is missing!\n") }
  if (is.null(score)) { stop("score is missing!\n") }
  if(is.null(perf_tb)){
    tb_ks = perf_table(train_pred = train_pred, test_pred = test_pred, target = target, score = score, g = g, breaks = breaks,cut_bin = cut_bin)
  }else{
    tb_ks = perf_tb
  }

  if (!is.null(test_pred) || length(test_pred) > 1) {
    if (mean(tb_ks[1:floor(nrow(tb_ks)/2), "train_Lift"]) < mean(tb_ks[(floor(nrow(tb_ks)/2)+1):nrow(tb_ks), "train_Lift"])) {
      tb_ks = tb_ks[order(tb_ks$bins, decreasing = TRUE),]
    }
    lift_pl = ggplot(tb_ks, aes(x = reorder(as.factor(as_percent(round(cumsum(tb_ks$`%train`), 1))), cumsum(tb_ks$`%train`)))) +
      geom_line(aes(y = train_Lift, color = 'train_Lift (Model)', group = 1), size = 1) +
      geom_point(aes(y = train_Lift), size = 1.5, shape = 22, fill = 'white', color = '#ca3e1c') +
      geom_line(aes(y = test_Lift, color = 'test_Lift (Model)', group = 1), size = 1) +
      geom_point(aes(y = test_Lift), size = 1.5, shape = 22, fill = 'white', color = love_color("shallow_cyan")) +
      geom_line(aes(y = rep(1, nrow(tb_ks)), group = 1, color = "Lift (Random)"), size = 1) +
      scale_colour_manual(values = c("Lift (Random)" = "#085A9C", 'test_Lift (Model)' = love_color("green_cyan"), 'train_Lift (Model)' = '#ca3e1c')) +
      geom_point(aes(y = rep(1, nrow(tb_ks))), size = 1.5, shape = 21, fill = 'white', color = "#085A9C") +
      scale_y_continuous(limits = c(0, max(tb_ks$train_Lift) + 0.5)) +
      geom_text(aes(y = train_Lift, label = round(train_Lift, 1)), vjust = -1, hjust = 0.5, size = 3, colour = "darkgrey") +
      geom_text(aes(y = test_Lift, label = round(test_Lift, 1)), vjust = 1.3, hjust = 0.5, size = 3, colour = "darkgrey") +
      labs(x = '%Total', y = 'Lift', title = paste(gtitle,"Lift Chart")) + theme_light() +
      theme(legend.title = element_blank(), legend.position = "top",
            plot.title = element_text(face = "bold", size = 11, vjust = 0, hjust = 0))
  } else {
    if (mean(tb_ks[1:floor(nrow(tb_ks)/2), "Lift"]) < mean(tb_ks[(floor(nrow(tb_ks)/2)+1):nrow(tb_ks), "Lift"])) {
      tb_ks = tb_ks[order(tb_ks$bins, decreasing = TRUE),]
    }
    lift_pl =  ggplot(tb_ks, aes(x = reorder(as.factor(as_percent(round(cumsum(tb_ks$`%Pct`), 1))), cumsum(tb_ks$`%Pct`)))) +
      geom_line(aes(y = Lift, linetype = 'Lift (Model)', group = 1), color = '#ca3e1c', size = 1) +
      geom_point(aes(y = Lift), size = 1.5, shape = 22, fill = 'white', color = '#ca3e1c') +
      geom_line(aes(y = rep(1, nrow(tb_ks)), group = 1, color = "Lift (Random)"), size = 1) +
      scale_colour_manual(values = c("Lift (Random)" = "#085A9C")) +
      geom_point(aes(y = rep(1, nrow(tb_ks))), size = 1.5, shape = 21, fill = 'white', color = "#085A9C") +
      scale_y_continuous(limits = c(0, max(tb_ks$Lift) + 0.5)) +
      geom_text(aes(y = Lift, label = round(Lift, 1)), vjust = -1, hjust = 0.5, size = 3, colour = "darkgrey") +
      labs(x = '%Total', y = 'Lift', title = paste(gtitle,"Lift Chart")) + theme_light() +
      theme(legend.title = element_blank(), legend.position = "top",
            plot.title = element_text(face = "bold", size = 11, vjust = 0, hjust = 0))
  }
  return(lift_pl)
  options(opt) # reset
}

#' roc_plot
#'
#' @rdname model_result_plot
#' @export
roc_plot <- function(train_pred, test_pred = NULL, target = NULL, score = NULL, gtitle = NULL) {
    opt = options('warn' = -1,scipen = 200, stringsAsFactors = FALSE, digits = 6) #
    if (is.null(target)) {stop("target is missing!\n") }
    if (is.null(score)) { stop("score is missing!\n") }
    if (is.null(gtitle)) { gtitle = paste0("Model") }
	train_pred <- train_pred[which(complete.cases(train_pred[, score])),]
	if(length(train_pred[,score]) > 1 && max(train_pred[,score]) > 1)train_pred[,score] = 1/train_pred[,score]
    roc_d = data.frame(prob = train_pred[ ,score], target = train_pred[ ,target])
    auc = auc_value(target = roc_d$target, prob = roc_d$prob)
    roc_d = roc_d[order(roc_d$prob),]
    tpr = fpr = rep(0, nrow(roc_d))
    for (i in 1:nrow(roc_d)) {
        threshold = roc_d$prob[i]
        tp = sum(roc_d$prob > threshold & roc_d$target == 1)
        fp = sum(roc_d$prob > threshold & roc_d$target == 0)
        tn = sum(roc_d$prob < threshold & roc_d$target == 0)
        fn = sum(roc_d$prob < threshold & roc_d$target == 1)
        tpr[i] <- tp / (tp + fn)
        fpr[i] <- fp / (tn + fp)
    }
    tpr_fpr = data.frame(tpr, fpr)

    if (!is.null(test_pred) || length(test_pred) > 1) {
	    test_pred <- test_pred[which(complete.cases(test_pred[, score])),]
	    if(length(test_pred[,score]) > 1 && max(test_pred[,score]) > 1)test_pred[,score] = 1/test_pred[,score]
        roc_dts = data.frame(prob = test_pred[, score], target = test_pred[, target])

        auc_ts = auc_value(target = roc_dts$target, prob = roc_dts$prob)
        roc_dts = roc_dts[order(roc_dts$prob),]
        tpr_ts = fpr_ts = rep(0, nrow(roc_dts))
        for (i in 1:nrow(roc_dts)) {
            threshold = roc_dts$prob[i]
            tp_ts = sum(roc_dts$prob > threshold & roc_dts$target == 1)
            fp_ts = sum(roc_dts$prob > threshold & roc_dts$target == 0)
            tn_ts = sum(roc_dts$prob < threshold & roc_dts$target == 0)
            fn_ts = sum(roc_dts$prob < threshold & roc_dts$target == 1)
            tpr_ts[i] <- tp_ts / (tp_ts + fn_ts)
            fpr_ts[i] <- fp_ts / (tn_ts + fp_ts)
        }
        tpr_fpr_ts = data.frame(tpr_ts, fpr_ts)
        roc_pl = ggplot() +
        geom_line(aes(x = tpr_fpr$fpr, y = tpr_fpr$tpr,group = 1, color = "train ROC"), size = 1) +
        annotate(geom = 'text', x = 0.5, y = 0.4, vjust = 1.5,
               label = paste('train AUC : ', round(auc, 4))) +
        geom_line( aes(x = tpr_fpr_ts$fpr_ts, y = tpr_fpr_ts$tpr_ts, group = 1, color = "test ROC"), size = 1) +
		scale_colour_manual(values = c("train ROC" = love_color("deep_orange"), 'test ROC' = love_color("deep_blue"))) +
        annotate(geom = 'text', x = 0.5, y = 0.3, vjust = 1.5,
               label = paste('test AUC : ', round(auc_ts, 4))) +
        geom_abline() + geom_hline(yintercept = 1) + geom_hline(yintercept = 0) +
        ylim(c(0, 1)) + labs(x = 'FPR', y = 'TPR',title = paste(gtitle, "ROC Curve"))+ theme_light()+
		theme(legend.title = element_blank(), legend.position = "top",
		   plot.title = element_text(face = "bold", size = 11, vjust = 0, hjust = 0))
    } else {
        roc_pl = ggplot() +
        geom_line(aes(x = tpr_fpr$fpr, y = tpr_fpr$tpr), size = 1, color = love_color("deep_orange")) +
        geom_abline() + geom_hline(yintercept = 1) + geom_hline(yintercept = 0) +
        annotate(geom = 'text', x = 0.5, y = 0.35, vjust = 1.5,
               label = paste('AUC : ', round(auc, 4))) +
        ylim(c(0, 1)) + labs(x = 'Fpr', y = 'Tpr',title = paste(gtitle, "ROC Curve")) + theme_light() +
		theme(legend.title = element_blank(), legend.position = "top",
		   plot.title = element_text(face = "bold", size = 11, vjust = 0, hjust = 0))
    }
    return(roc_pl)
	options(opt) # reset
}


#' score_distribution_plot
#'
#' @rdname model_result_plot
#' @export


score_distribution_plot <- function(train_pred, test_pred, target, score,
                             gtitle = NULL, breaks = NULL, g = 10,cut_bin = 'equal_depth',perf_tb = NULL ) {
   opt = options('warn' = -1,scipen = 200, stringsAsFactors = FALSE, digits = 6) #
  `value` = `train_test` = `bins` = `%train_cumG` = `%train_cumB` = `%test_cumG` = `%test_cumB` =
    `%Pct` = `%Pct_1` =  NULL
  if (is.null(gtitle)) { gtitle = paste0("Model") }

     if(is.null(perf_tb)){
	  tb_ks = perf_table(train_pred = train_pred, test_pred = test_pred, target = target, score = score, g = g, breaks = breaks,cut_bin = cut_bin)
	}else{
	  tb_ks = perf_tb
	}
  if (!is.null(test_pred) || length(test_pred) > 1){

    names(tb_ks)[7:8] =  c("%train_1", "%test_1")
    ts_total = data.table::melt(as.data.table(tb_ks[c("bins", "%train", "%test")]), id.vars = c("bins"),
                                variable.name = "train_test", value.name = "value")
    ts_1 = data.table::melt(as.data.table(tb_ks[c("bins", "%train_1", "%test_1")]), id.vars = c("bins"),
                            variable.name = "train_test", value.name = "value")

    dis_pl =  ggplot(ts_total, aes(x = bins, y = value,fill = train_test)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
      geom_text(aes(y = value, label = paste(as_percent(value, digits = 3))),
                position = position_dodge(width = 0.7),
                size = 3, vjust = 1, hjust = 0.3, colour = "white") +
      geom_line(aes(x = factor(ts_1[[1]]),
                    y = as.numeric(ts_1$value) * max(ts_total$value) * 4,
                    color = ts_1$train_test,
                    linetype = ts_1$train_test,
                    group = ts_1$train_test),
                position = position_dodge(width = 0.5), size = 1) +
      geom_point(aes(y = as.numeric(ts_1$value) * max(ts_total$value) * 4,
                     color = ts_1$train_test,
                     group = ts_1$train_test),
                 position = position_dodge(width = 0.5),
                 fill = 'white', size = 2, shape = 21) +
      scale_color_manual(values = c(love_color("deep_green"),
                                    love_color("deep_orange")))+
      geom_text(aes(y = as.numeric(ts_1$value) * max(ts_total$value) * 4,
                    label = paste(as_percent(ts_1$value, digits = 3))),
                position = position_dodge(width = 0.5),
                colour = 'black', size = 3, vjust = -0.1) +
      annotate(geom = 'text',
               x = dim(ts_total)[1] / 3,
               y = max(c(ts_total$value, as.numeric(ts_1$value) * max(ts_total$value) * 4)) + 0.09,
               label = paste('PSI', round(sum(tb_ks$PSI, na.rm = TRUE), 4), sep = " : ")) +
      scale_fill_manual(values = c('%train' = love_color("shallow_red2"),
                                   '%test' = love_color("green_cyan"))) +
      ylim(c(-0.01, max(c(ts_total$value, as.numeric(ts_1$value) * max(ts_total$value) * 4)) + 0.1)) +
      xlab(score) +
      ylab("% Population") +
      ggtitle(paste(gtitle, "Population Distribution")) +
      plot_theme(legend.position = "top",
                 angle = ifelse(nrow(ts_1) > 20, 50,
                                ifelse(nrow(ts_1) > 10, 30, 0)))
  }else{


    dis_pl =  ggplot(tb_ks, aes(x = bins, y = `%Pct`)) +
      geom_bar(aes(fill = "%total") ,stat = "identity", position = position_dodge(width = 0.7)) +
      geom_text(aes(y = `%Pct`, label = paste(as_percent(`%Pct`, digits = 3))),
                position = position_dodge(width = 0.7),
                size = 3, vjust = 1, hjust = 0.3, colour = "white") +
      geom_line(aes(x = factor(tb_ks[[1]]),
                    y = as.numeric(tb_ks$`%Pct_1`) * max(tb_ks$`%Pct`) * 4,
                    color = "%flag_1"
      ),
      linetype = 1,
      group = 1,
      position = position_dodge(width = 0.5), size = 1) +
      geom_point(aes(y = as.numeric(tb_ks$`%Pct_1`) * max(tb_ks$`%Pct`) * 4),

                 color = love_color("deep_orange"),
                 group = 1,
                 position = position_dodge(width = 0.5),
                 fill = 'white', size = 2, shape = 21) +
      geom_text(aes(y = as.numeric(tb_ks$`%Pct_1`) * max(tb_ks$`%Pct`) * 4,
                    label = paste(as_percent(tb_ks$`%Pct_1`, digits = 3))),
                position = position_dodge(width = 0.5),
                colour = 'black', size = 3, vjust = -0.1) +
      scale_fill_manual(values = c("%total" = love_color("deep_green"))) +
      scale_color_manual(values = c("%flag_1" = love_color("deep_orange"))) +
      ylim(c(-0.01, max(c(tb_ks$`%Pct`,  as.numeric(tb_ks$`%Pct_1`) * max(tb_ks$`%Pct`) * 4)) + 0.05)) +
      xlab(score) +
      ylab("% Population") +
      ggtitle(paste(gtitle, "Population Distribution")) +
      plot_theme(legend.position = "top",
                 angle = ifelse(nrow(tb_ks) > 10, 50,
                                ifelse(nrow(tb_ks) > 5, 30, 0)))
  }
  return(dis_pl)
  options(opt) # reset

}


#' plot_oot_perf
#' \code{plot_oot_perf} is for ploting performance of cross time samples in the future
#' @param dat_test A data frame of testing dataset with predicted prob or score.
#' @param target The name of target variable.
#' @param x The name of prob or score variable.
#' @param occur_time The name of the variable that represents the time at which each observation takes place.
#' @param best  Logical, merge initial breaks to get optimal breaks for binning.
#' @param equal_bins  Logical, generates initial breaks for equal frequency or width binning.
#' @param period OOT period, 'weekly' and 'month' are available.if NULL, use k equal frequency samples.
#' @param k  If period is NULL, number of equal frequency samples.
#' @param g Number of breaks for prob or score.
#' @param gtitle The title of the graph & The name for periodically saved graphic file.
#' @param breaks Splitting points of prob or score.
#' @param cut_bin A string, if equal_bins is TRUE, 'equal_depth' or 'equal_width', default is 'equal_depth'.
#' @param pl 'lift' is for lift chart plot,'rate' is for positive rate plot.
#' @param plot_show Logical, show model performance in current graphic device. Default is TRUE.
#' @param save_data Logical, save results in locally specified folder. Default is FALSE.
#' @param perf_dir_path The path for periodically saved graphic files.
#' @examples
#' sub = cv_split(UCICreditCard, k = 30)[[1]]
#' dat = UCICreditCard[sub,]
#' dat = re_name(dat, "default.payment.next.month", "target")
#' x_list = c("PAY_0", "LIMIT_BAL", "PAY_AMT5", "PAY_3", "PAY_2")
#' dat = data_cleansing(dat, target = "target", obs_id = "ID",x_list = x_list,
#' occur_time = "apply_date", miss_values = list("", -1))
#' dat = process_nas(dat)
#' train_test <- train_test_split(dat, split_type = "OOT", prop = 0.7,
#'                                 occur_time = "apply_date")
#' dat_train = train_test$train
#' dat_test = train_test$test

#' Formula = as.formula(paste("target", paste(x_list, collapse = ' + '), sep = ' ~ '))
#' set.seed(46)
#' lr_model = glm(Formula, data = dat_train[, c("target", x_list)], family = binomial(logit))
#'
#' dat_train$pred_LR = round(predict(lr_model, dat_train[, x_list], type = "response"), 5)
#' dat_test$pred_LR = round(predict(lr_model, dat_test[, x_list], type = "response"), 5)
#' plot_oot_perf(dat_test = dat_test, occur_time = "apply_date", target = "target", x = "pred_LR")
#' @import ggplot2
#' @importFrom data.table rbindlist
#' @export

plot_oot_perf = function(dat_test, x, occur_time, target, k = 3, g = 10, period = 'month',
						 best = FALSE, equal_bins = TRUE, pl = 'rate', breaks = NULL,
						 cut_bin = 'equal_depth', gtitle = NULL, perf_dir_path = NULL,
						 save_data = FALSE, plot_show = TRUE) {
	if (!(class(dat_test[, x]) %in% c("numeric", "double", "integer")) ||
		 length(unique(dat_test[, x])) <= 10) {
		dat_test[, x] = as.character(dat_test[, x])
	}

	if (!is.null(period) && period == "weekly") {
		dat_test = time_transfer(dat_test, date_cols = "occur_time")
		dat_test$cohort_group = cut(as.Date(dat_test[, occur_time]), breaks = "week")
		unique_wk = sort(unique(dat_test$cohort_group))
		test_cv = list()
		for (w in 1:length(unique_wk)) {
			test_cv[[w]] = which(dat_test$cohort_group == unique_wk[w])
			if (length(unique(dat_test[test_cv[[w]], target])) == 1 |
				length(unique(dat_test[test_cv[[w]], occur_time])) < 3) {
				test_cv[[w]] = NA
			}
		}
	} else {
		if (!is.null(period) && period == "month") {
			dat_test = time_transfer(dat_test, date_cols = "occur_time")
			dat_test$cohort_group = as.character(cut(as.Date(dat_test[, occur_time]), breaks = "month"))
			unique_wk = sort(unique(dat_test$cohort_group))
			test_cv = list()
			for (w in 1:length(unique_wk)) {
				test_cv[[w]] = which(dat_test$cohort_group == unique_wk[w])
				if (length(unique(dat_test[test_cv[[w]], target])) == 1 |
  				  length(unique(dat_test[test_cv[[w]], occur_time])) < 3) {
					test_cv[[w]] = NA
				}
			}
		} else {
			test_cv = cv_split(dat_test, k = k, occur_time = occur_time)
		}

	}

	test_cv = test_cv[!is.na(test_cv)]
	if (is.null(breaks)) {
		ts_breaks = get_breaks(dat = dat_test, target = target, x = x,
						   best = best, equal_bins = equal_bins, g = g, cut_bin = cut_bin)
	}


	cohort_group = c()
	perf_list = list()
	for (i in 1:length(test_cv)) {
		max_time = gsub("-", "/", as.character(max(dat_test[test_cv[[i]],][, occur_time])))
		min_time = gsub("-", "/", as.character(min(dat_test[test_cv[[i]],][, occur_time])))
		cohort_group[i] = paste(min_time,
							substring(max_time, nchar(max_time) - 4, nchar(max_time)), sep = "_")
		perf_list[[i]] = cbind(cohort_group = cohort_group[i],
						   perf_table(train_pred = dat_test[test_cv[[i]],],
									  target = target, score = x,
									  breaks = ts_breaks))

	}

	cohort_dat = rbindlist(perf_list)
	bins_n = length(unique(as.character(cohort_group)))
	if (bins_n <= 4) {
		values = love_color(all = TRUE, type = "line")
	} else {
		if (bins_n <= 7) {
			values = love_color(all = TRUE, type = "deep")
		} else {
			if (bins_n > 7 & bins_n <= 15) {
				values = love_color(all = TRUE, type = "deep|light")
			} else {
				if (bins_n > 15 & bins_n <= 21) {
					values = love_color(all = TRUE, type = "deep|light|pale")
				} else {
					if (bins_n > 21 & bins_n <= 27) {
						values = love_color(all = TRUE, type = "deep|light|shallow|pale")
					} else {
						values = love_color(all = TRUE)
					}
				}
			}
		}
	}

	Lift = bins = `%Pct_1` = NULL

	if (pl == 'rate') {
		p1 = ggplot(cohort_dat, aes(as.character(bins), `%Pct_1`)) +
  	  geom_line(aes(group = as.character(cohort_group), colour = as.character(cohort_group)),
				size = 1.2) +
  	  geom_point(aes(x = as.character(bins),
					 y = `%Pct_1`, color = as.character(cohort_group)),
				 size = 1.3, shape = 21, fill = 'white') +
  	  scale_x_discrete(breaks = as.character(cohort_dat$bins)) +
  	  scale_colour_manual(values = values) +
  	  plot_theme() + xlab(x) +
  	  ylab("% Bad") + ggtitle(paste(gtitle, "% Bad of OOT samples"))
	} else {
		p1 = ggplot(cohort_dat, aes(as.character(bins), Lift)) +
  	  geom_line(aes(group = as.character(cohort_group), colour = as.character(cohort_group)),
				size = 1.2) +
  	  geom_point(aes(x = as.character(bins),
					 y = Lift, color = as.character(cohort_group)),
				 size = 1.3, shape = 21, fill = 'white') +
  	  scale_x_discrete(breaks = as.character(cohort_dat$bins)) +
  	  scale_colour_manual(values = values) +
  	  plot_theme() + xlab(x) +
  	  ylab("Lift") + ggtitle(paste(gtitle, "Lift of OOT samples"))

	}
	if (save_data) {
		perf_dir_path = ifelse(is.null(perf_dir_path), tempdir(), perf_dir_path)
		if (!dir.exists(perf_dir_path)) dir.create(perf_dir_path)

		ggsave(filename = paste0(perf_dir_path, "/", paste(paste0(gtitle, "_oot", "_", pl), "png", sep = '.')), device = "png",
		   plot = p1, dpi = "retina", width = 5, height = 4.5)
	}
	if (plot_show) {
		plot(p1)
	}
	return(p1)
}


#' cohort_table_plot
#' \code{cohort_table_plot} is for ploting cohort(vintage) analysis table.
#'
#' This function is not intended to be used by end user.
#'
#' @param cohort_dat  A data.frame generated by \code{cohort_analysis}.
#' @import ggplot2
#' @export


cohort_table_plot = function(cohort_dat) {
  opt = options('warn' = -1, scipen = 200, stringsAsFactors = FALSE, digits = 6) #
  cohort_dat[is.na(cohort_dat)] = 0
  Cohort_Group = Cohort_Period = Events = Events_rate = Opening_Total = Retention_Total = cohor_dat = final_Events = m_a = max_age = NULL
  cohort_plot = ggplot(cohort_dat, aes(reorder(paste0(Cohort_Period), Cohort_Period),
                                       Cohort_Group, fill = Events_rate)) +
    geom_tile(colour = 'white') +
    geom_text(aes(label = as_percent(Events_rate, 4)), size = 3) +
    scale_fill_gradient2(limits = c(0, max(cohort_dat$Events_rate)),
                         low = love_color('deep_red'), mid = 'white',
                         high = love_color(),
                         midpoint = median(cohort_dat$Events_rate,
                                           na.rm = TRUE), 
                         na.value = love_color('pale_grey')) +
    scale_y_discrete(limits = rev(unique(cohort_dat$Cohort_Group))) +
    scale_x_discrete(position = "top") +
    labs(x = "Cohort_Period", title = "Cohort Analysis") +
    theme(text = element_text(size = 15), rect = element_blank()) +
    plot_theme(legend.position = 'right', angle = 0)
  return(cohort_plot)
  options(opt)
}


#' cohort_plot
#'
#' @rdname cohort_table_plot
#' @export

cohort_plot <- function(cohort_dat){
    cohort_dat[is.na(cohort_dat)] = 0
	   Cohort_Group= Cohort_Period =Events= Events_rate= Opening_Total= Retention_Total =cohor_dat= final_Events =m_a= max_age=NULL
    ggplot(cohort_dat,aes(Cohort_Period,Events_rate )) +
      geom_line(aes(group = Cohort_Group,colour = Cohort_Group),size =1.3 ) +
      geom_point(aes(x = Cohort_Period,
                     y = Events_rate, color =Cohort_Group),
                 size = 1.3, shape = 21, fill = 'white') +
      scale_x_continuous(breaks = 1:max(cohort_dat$Cohort_Period)) +
      scale_colour_manual(values = love_color(all= TRUE,type = "dark|light|shallow"))+
      plot_theme()
}

#' plot_table
#'
#' \code{plot_table} is for table visualizaiton.
#' @param grid_table A data.frame or table
#' @param theme The theme of color, "cyan","grey","green","red","blue","purple" are available.
#' @param title The title of table
#' @param title.size The title size of plot.
#' @param title.color The title color.
#' @param title.face The title face, such as "plain", "bold".
#' @param title.position The title position,such as "left","middle","right".
#' @param subtitle The subtitle of table
#' @param subtitle.size The subtitle size.
#' @param subtitle.color The subtitle color.
#' @param subtitle.face The subtitle face, such as "plain", "bold",default is "bold".
#' @param subtitle.position The subtitle position,such as "left","middle","right", default is "middle".
#' @param tile.color The color of table lines, default is 'white'.
#' @param tile.size The size of table lines , default is 1.
#' @param colname.size The size of colnames, default is 3.
#' @param colname.color  The color of colnames, default is 'white'.
#' @param colname.face The face of colnames,default is 'bold'.
#' @param colname.fill.color  The fill color of colnames, default is love_color("dark_cyan").
#' @param text.size The size of text, default is 3.
#' @param text.color The color of text, default is love_color("dark_grey").
#' @param text.face The face of text, default is 'plain'.
#' @param text.fill.color The fill color of text, default is c('white',love_color("pale_grey").
#' @import ggplot2
#' @examples
#' iv_list = get_psi_iv_all(dat = UCICreditCard[1:1000, ],
#'                          x_list = names(UCICreditCard)[3:5], equal_bins = TRUE,
#'                          target = "default.payment.next.month", ex_cols = "ID|apply_date")
#' iv_dt =get_psi_iv(UCICreditCard, x = "PAY_3",
#'                   target = "default.payment.next.month", bins_total = TRUE)
#'
#' plot_table(iv_dt)
#' @export

plot_table <- function(grid_table, theme = c("cyan", "grey", "green", "red", "blue", "purple"),
					  title = NULL, title.size = 12, title.color = 'black', title.face = "bold", title.position = 'middle',
					  subtitle = NULL, subtitle.size = 8, subtitle.color = 'black', subtitle.face = "plain",
					  subtitle.position = 'middle',
					  tile.color = 'white', tile.size = 1,
					  colname.size = 3, colname.color = 'white', colname.face = 'bold',
					  colname.fill.color = love_color("dark_cyan"),
					  text.size = 3, text.color = love_color("dark_grey"),
					  text.face = 'plain', text.fill.color = c('white', love_color("pale_grey"))
) {
	opt = options('warn' = -1, scipen = 200, stringsAsFactors = FALSE, digits = 6) #
	grid_table = rbind(colnames(grid_table), as.data.frame(grid_table))
	grid_table = cbind(rows = rownames(grid_table), as.data.frame(grid_table))
	grid_table = as.data.table(grid_table)
	table_1 = data.table::melt(grid_table, id = 1, measure = 2:ncol(grid_table), value.factor = TRUE)
	grid_table = quick_as_df(grid_table)
	table_1 = data.frame(table_1, ind = paste0("x", 1:nrow(table_1)))

	table_theme = check_table_theme(theme = theme)
	if (!is.null(table_theme$colname.fill.color) && !is.null(table_theme$text.fill.color)) {
		colname.fill.color = table_theme$colname.fill.color
		text.fill.color = table_theme$text.fill.color
	}
	plot_ele = get_plot_elements(table_1 = table_1, grid_table = grid_table, colname.fill.color = colname.fill.color,
  text.fill.color = text.fill.color, colname.color = colname.color, text.color = text.color,
  colname.face = colname.face, text.face = text.face, colname.size = colname.size, text.size = text.size)
	fill_colors = plot_ele$fill_colors
	text_colors = plot_ele$text_colors
	fill_size = plot_ele$fill_size
	fill_bold = plot_ele$fill_bold
	value_width = plot_ele$value_width
	nudge_y = plot_ele$nudge_y
	table_2 = rearrange_table(table_1 = table_1, grid_table = grid_table, colname.face = colname.face,
  colname.size = colname.size, text.size = text.size)
	pl_tb = ggplot(table_2, aes(x = table_2$n_cumsum, y = table_2$rows)) +
	geom_tile(aes(fill = table_2$ind, width = table_2$n_width), height = value_width,
			  show.legend = FALSE, colour = tile.color, size = tile.size) +
	geom_text(nudge_x = c(-(table_2$n_width[-c((nrow(table_2) - length(unique(table_2$rows)) + 1):nrow(table_2))] / 4
	), table_2$n_width[c((nrow(table_2) - length(unique(table_2$rows)) + 1):nrow(table_2))] * 0)
	, nudge_y = nudge_y, aes(label = table_2$value), size = fill_size,
	colour = text_colors, fontface = fill_bold) +
	labs(title = title, subtitle = subtitle, x = "", y = "") +
	scale_fill_manual(limits = table_2$ind, values = fill_colors) +
	scale_x_discrete(limits = c(1:nrow(table_2) - 1)) +
	scale_y_discrete(limits = rev(unique(table_2$rows))) +
	theme(
	  legend.position = 'none', axis.ticks = element_blank(), axis.text = element_blank(),
	  plot.title = element_text(face = title.face, size = title.size, color = title.color,
								hjust = ifelse(title.position == 'middle', 0.5,
											   ifelse(title.position == 'right', 1, 0)), vjust = 0),
	  plot.subtitle = element_text(face = subtitle.face, size = subtitle.size, color = subtitle.color,
								   hjust = ifelse(subtitle.position == 'middle', 0.5,
												  ifelse(subtitle.position == 'right', 1, 0)), vjust = 0),
	  rect = element_blank())
	return(pl_tb)
	options(opt)

}


rearrange_table <- function(table_1, grid_table, colname.face, colname.size, text.size) {
	variable = n_char = n_width = rows = NULL

	table_1$value = sapply(table_1$value, function(x) {
		if (!is.na(x) && x %in% colnames(grid_table[1, -1])) {
			if (nrow(grid_table) > 10) {
				if (nchar(x) > 20 & nchar(x) <= 40) {
					x = paste(substr(x, 1, 20), substr(x, 21, nchar(x)), sep = "\n")
				} else {
					if (nchar(x) > 40 & nchar(x) <= 60) {
						x = paste(substr(x, 1, 20), substr(x, 21, 40), substr(x, 41, nchar(x)), sep = "\n")
					} else {
						if (nchar(x) > 60 & nchar(x) <= 80) {
							x = paste(substr(x, 1, 20), substr(x, 21, 40), substr(x, 41, 60),
						substr(x, 61, nchar(x)), sep = "\n")
						} else {
							if (nchar(x) > 80 & nchar(x) <= 100) {
								x = paste(substr(x, 1, 20), substr(x, 21, 40), substr(x, 41, 60),
						  substr(x, 61, 80), substr(x, 81, nchar(x)), sep = "\n")
							} else {
								if (nchar(x) > 100 & nchar(x) <= 120) {
									x = paste(substr(x, 1, 20), substr(x, 21, 40), substr(x, 41, 60),
							substr(x, 61, 80), substr(x, 81, 100), substr(x, 101, nchar(x)), sep = "\n")
								} else {
									if (nchar(x) > 120 & nchar(x) <= 140) {
										x = paste(substr(x, 1, 20), substr(x, 21, 40), substr(x, 41, 60),
							  substr(x, 61, 80), substr(x, 81, 100), substr(x, 101, 120),
							  substr(x, 121, nchar(x)), sep = "\n")
									} else {
										if (nchar(x) > 140) {
											x = paste(substr(x, 1, 20), substr(x, 21, 40), substr(x, 41, 60),
								substr(x, 61, 80), substr(x, 81, 100), substr(x, 101, 120),
								substr(x, 121, 140), substr(x, 141, nchar(x)), sep = "\n")
										} else {
											x
										}
									}
								}
							}
						}
					}
				}
			} else {
				if (nchar(x) > 25 & nchar(x) <= 50) {
					x = paste(substr(x, 1, 25), substr(x, 26, nchar(x)), sep = "\n")
				} else {
					if (nchar(x) > 50 & nchar(x) <= 75) {
						x = paste(substr(x, 1, 25), substr(x, 26, 50), substr(x, 51, nchar(x)), sep = "\n")
					} else {
						if (nchar(x) > 75 & nchar(x) <= 100) {
							x = paste(substr(x, 1, 25), substr(x, 26, 50), substr(x, 51, 75),
						substr(x, 76, nchar(x)), sep = "\n")
						} else {
							if (nchar(x) > 100 & nchar(x) <= 125) {
								x = paste(substr(x, 1, 25), substr(x, 26, 50), substr(x, 51, 75),
						  substr(x, 76, 100), substr(x, 101, nchar(x)), sep = "\n")
							} else {
								if (nchar(x) > 125) {
									x = paste(substr(x, 1, 25), substr(x, 26, 50), substr(x, 51, 75),
							substr(x, 76, 100), substr(x, 101, 125), substr(x, 126, nchar(x)), sep = "\n")
								} else {
									x
								}
							}
						}
					}
				}
			}
		} else {
			if (!is.na(x) && nchar(x) > 10 & nchar(x) <= 15) {
				x = paste(substr(x, 1, 8), substr(x, 9, nchar(x)), sep = "\n")
			} else {
				if (!is.na(x) && nchar(x) > 15) {
					x = paste(substr(x, 1, 12), substr(x, 13, nchar(x)), sep = "\n")
				} else {
					x

				}
			}
		}

	})
	table_2 = table_1 %>%
	dplyr::mutate(n_char = unlist(
	  sapply(table_1$value,
	  function(x) ifelse(colname.face == 'bold' &
	  x %in% colnames(grid_table[1, -1]) & colname.size - text.size == 0,
	  nchar(strsplit(x, "\n")[[1]][1]) + 0,
	  ifelse(colname.face == 'bold' &
	  colname.size - text.size != 0 & x %in% colnames(grid_table[1, -1]),
	  nchar(strsplit(x, "\n")[[1]][1]) + 0,
	  ifelse(colname.face != 'bold' & colname.size - text.size != 0 &
	  x %in% colnames(grid_table[1, -1])
	  , nchar(strsplit(x, "\n")[[1]][1]) + 0,
	  ifelse(colname.face == 'bold' &
	  x %in% colnames(grid_table[1, -1]) & colname.size - text.size == 0,
	  nchar(strsplit(x, "\n")[[1]][1]) + 0,
	  ifelse(colname.face == 'bold' | colname.size - text.size != 0,
	  nchar(strsplit(x, "\n")[[1]][1]) +0,
	  nchar(strsplit(x, "\n")[[1]][1]))))))))) %>%
  	dplyr::group_by(variable) %>%
	dplyr::mutate(n_width = max(n_char, na.rm = TRUE)) %>%
	dplyr::ungroup() %>%
	dplyr::group_by(rows) %>%
	dplyr::mutate(n_cumsum = base::cumsum(n_width)/2) %>%
	dplyr::ungroup()
	return(table_2)
}

check_table_theme <- function(theme = NULL) {
	colname.fill.color = text.fill.color = NULL
	if (length(theme) > 0 &&
   	 any(sapply(theme, function(x) is.element(x, c("cyan", "grey", "green", "red", "blue", "purple"))))) {
		if (theme[1] == 'cyan') {
			dark_cyan = rgb(40, 70, 100, maxColorValue = 255)
			shallow_cyan = rgb(200, 230, 245, maxColorValue = 255)
			pale_cyan = rgb(220, 240, 255, maxColorValue = 255)
			colname.fill.color = dark_cyan
			text.fill.color = c(pale_cyan, shallow_cyan)
		} else {
			if (theme[1] == 'grey') {
				dark_grey = rgb(102, 102, 102, maxColorValue = 255)
				shallow_grey = rgb(225, 226, 225, maxColorValue = 255)
				pale_grey = rgb(240, 240, 240, maxColorValue = 255)
				colname.fill.color = dark_grey
				text.fill.color = c(pale_grey, shallow_grey)

			} else {
				if (theme[1] == 'red') {
					dark_red = rgb(180, 40, 40, maxColorValue = 255)
					shallow_red = rgb(255, 230, 230, maxColorValue = 255)
					pale_red = rgb(255, 242, 242, maxColorValue = 255)
					colname.fill.color = dark_red
					text.fill.color = c(pale_red, shallow_red)
				} else {
					if (theme[1] == 'blue') {
						deep_blue = rgb(80, 99, 139, maxColorValue = 255)
						shallow_blue = rgb(210, 232, 255, maxColorValue = 255)
						pale_blue = rgb(227, 240, 255, maxColorValue = 255)
						colname.fill.color = deep_blue
						text.fill.color = c(pale_blue, shallow_blue)
					} else {
						if (theme[1] == 'green') {
							deep_green = rgb(50, 150, 150, maxColorValue = 255)
							shallow_green = rgb(200, 240, 240, maxColorValue = 255)
							pale_green = rgb(225, 255, 255, maxColorValue = 255)
							colname.fill.color = deep_green
							text.fill.color = c(pale_green, shallow_green)
						} else {
							if (theme[1] == 'purple') {
								dark_purple = rgb(180, 50, 105, maxColorValue = 255)
								shallow_purple = rgb(245, 225, 245, maxColorValue = 255)
								pale_purple = rgb(255, 240, 255, maxColorValue = 255)
								colname.fill.color = dark_purple
								text.fill.color = c(pale_purple, shallow_purple)
							}

						}
					}
				}
			}
		}
	}
	return(list(colname.fill.color = colname.fill.color, text.fill.color = text.fill.color))
}

get_plot_elements <- function(table_1, grid_table, colname.fill.color, text.fill.color,
	   colname.color, text.color, colname.face, text.face, colname.size, text.size) {
	fill_colors = c()
	m = 0
	for (i in 1:length(table_1$value)) {
		if (nrow(grid_table) %% 2 == 0) {
			m = m + 1
		}
		if (table_1$value[i] %in% colnames(grid_table[1, -1])) {

			fill_colors = c(fill_colors, colname.fill.color[1])
		} else {
			if (nrow(grid_table) %% 2 != 0) {
				m = m + 1
			}
			if (length(text.fill.color) > 1) {
				if (m %% 2 == 0) {
					fill_colors = c(fill_colors, text.fill.color[1])
				} else {

					fill_colors = c(fill_colors, text.fill.color[2])
				}

			} else {
				fill_colors = c(fill_colors, text.fill.color[1])

			}
		}
	}

	text_colors = c()
	for (i in 1:length(table_1$value)) {
		if (table_1$value[i] %in% colnames(grid_table[1, -1])) {

			text_colors = c(text_colors, colname.color)
		} else {
			text_colors = c(text_colors, text.color)
		}
	}

	fill_size = c()
	for (i in 1:length(table_1$value)) {
		if (table_1$value[i] %in% colnames(grid_table[1, -1])) {

			fill_size = c(fill_size, colname.size)
		} else {
			fill_size = c(fill_size, text.size)
		}
	}

	fill_bold = c()
	for (i in 1:length(table_1$value)) {
		if (table_1$value[i] %in% colnames(grid_table[1, -1])) {
			fill_bold = c(fill_bold, colname.face)
		} else {
			fill_bold = c(fill_bold, text.face)
		}
	}
	value_width = c()
	for (i in 1:length(table_1$value)) {
		if (table_1$value[i] %in% colnames(grid_table[1, -1]) && any(nchar(colnames(grid_table[1, -1])) > 10)) {
			value_width = c(value_width, 2)
		} else {

			value_width = c(value_width, 1)
		}
	}
	nudge_y = c()
	for (i in 1:length(table_1$value)) {
		if (table_1$value[i] %in% colnames(grid_table[1, -1]) && any(nchar(colnames(grid_table[1, -1])) > 10)) {
			nudge_y = c(nudge_y, 0.05)
		} else {

			nudge_y = c(nudge_y, 0)
		}
	}

	return(list(fill_colors = fill_colors, text_colors = text_colors, fill_size = fill_size,
		   fill_bold = fill_bold, value_width = value_width, nudge_y = nudge_y))
}

#' @title Arrange list of plots into a grid
#' @name multi_grid
#'
#' @description Plot multiple ggplot-objects as a grid-arranged single plot.
#'
#' @param grobs A list of ggplot-objects to be arranged into the grid.
#' @param nrow  Number of rows in the plot grid.
#' @param ncol Number of columns in the plot grid.
#' @param ... Other parameters.
#'
#' @return An object of class \code{gtable}.
#'
#' @details This function takes a \code{list} of ggplot-objects as argument.
#'          Plotting functions of this package that produce multiple plot
#'          objects (e.g., when there is an argument \code{facet.grid}) usually
#'          return multiple plots as list.
#'
#' @examples
#' library(ggplot2)
#' sub = cv_split(UCICreditCard, k = 30)[[1]]
#' dat = UCICreditCard[sub,]
#' dat = re_name(dat, "default.payment.next.month", "target")
#' dat = data_cleansing(dat, target = "target", obs_id = "ID",
#' occur_time = "apply_date", miss_values = list("", -1))
#' dat = process_nas(dat)
#' train_test <- train_test_split(dat, split_type = "OOT", prop = 0.7,
#'                                 occur_time = "apply_date")
#' dat_train = train_test$train
#' dat_test = train_test$test
#' x_list = c("PAY_0", "LIMIT_BAL", "PAY_AMT5", "PAY_3", "PAY_2")
#' Formula = as.formula(paste("target", paste(x_list, collapse = ' + '), sep = ' ~ '))
#' set.seed(46)
#' lr_model = glm(Formula, data = dat_train[, c("target", x_list)], family = binomial(logit))
#'
#' dat_train$pred_LR = round(predict(lr_model, dat_train[, x_list], type = "response"), 5)
#' dat_test$pred_LR = round(predict(lr_model, dat_test[, x_list], type = "response"), 5)
#' # model evaluation
#' p1 =  ks_plot(train_pred = dat_train, test_pred = dat_test, target = "target", score = "pred_LR")
#' p2 =  roc_plot(train_pred = dat_train, test_pred = dat_test, target = "target", score = "pred_LR")
#' p3 =  lift_plot(train_pred = dat_train, test_pred = dat_test, target = "target", score = "pred_LR")
#' p4 = score_distribution_plot(train_pred = dat_train, test_pred = dat_test,
#' target = "target", score = "pred_LR")
#' p_plots= multi_grid(p1,p2,p3,p4)
#' plot(p_plots)
#' @export

multi_grid <- function(..., grobs = list(...),
						 nrow = NULL, ncol = NULL) {
	n = length(grobs)
	if (is.null(nrow) && !is.null(ncol)) {
		nrow = ceiling(n / ncol)
	}
	if (is.null(ncol) && !is.null(nrow)) {
		ncol = ceiling(n / nrow)
	}
	stopifnot(nrow * ncol >= n)
	if (is.null(nrow) && is.null(ncol)) {
		if (n <= 3) {
			nrow = n
			ncol = 1
		} else {
			if (n <= 6) {
				nrow = (n + 1) %/% 2
				ncol = 2
			} else {
				if (n <= 12) {
					nrow = (n + 2) %/% 3
					ncol = 3
				} else {
					nrow = ceiling(sqrt(n))
					ncol = ceiling(n / nrow)
				}
			}
		}
	}
	inherit.ggplot = unlist(lapply(grobs, inherits, what = "ggplot"))

	if (any(inherit.ggplot)) {
		stopifnot(requireNamespace("ggplot2", quietly = TRUE))
		toconv = which(inherit.ggplot)
		grobs[toconv] = lapply(grobs[toconv], ggplot2::ggplotGrob)
	}
	positions = expand.grid(t = seq_len(nrow),
						   l = seq_len(ncol))
	positions$b = positions$t
	positions$r = positions$l
	positions = positions[order(positions$t),]
	positions = positions[seq_along(grobs),]
	## sizes
	widths = unit(rep(1, ncol), "null")
	heights = unit(rep(1, nrow), "null")

	grob_table = to_gtable(name = "arrange",

			   heights = heights,
			   widths = widths)

	grob_table = add_grobs(x = grob_table, grobs,
						t = positions$t,
						b = positions$b,
						l = positions$l,
						r = positions$r,
						z = seq_along(grobs),
						clip = "off")
	grob_table
}

to_gtable = function(widths = list(), heights = list(),
				   name = "layout") {
	layout = new_data_frame(list(t = numeric(), l = numeric(),
								b = numeric(), r = numeric(), z = numeric(), clip = character(),
								name = character()), n = 0)
	grid::gTree(grobs = list(), layout = layout, widths = widths, heights = heights,
			  respect = FALSE, name = name, rownames = NULL,
			  colnames = NULL, vp = NULL, cl = "gtable")
}


add_grobs <- function(x, grobs, t, l, b = t, r = l, z = Inf, clip = "off",
							name = x$name) {

	n_grobs = length(grobs)
	layout = unclass(x$layout)
	z = rep(z, length.out = n_grobs)
	zval = c(layout$z, z[!is.infinite(z)])
	if (length(zval) == 0) {
		zmin = 1
		zmax = 0
	} else {
		zmin = min(zval)
		zmax = max(zval)
	}
	z[z == -Inf] = zmin - rev(seq_len(sum(z == -Inf)))
	z[z == Inf] = zmax + seq_len(sum(z == Inf))
	x_row = length(x$heights)
	x_col = length(x$widths)
	t = rep(neg_to_pos(t, x_row), length.out = n_grobs)
	b = rep(neg_to_pos(b, x_row), length.out = n_grobs)
	l = rep(neg_to_pos(l, x_col), length.out = n_grobs)
	r = rep(neg_to_pos(r, x_col), length.out = n_grobs)
	clip = rep(clip, length.out = n_grobs)
	name = rep(name, length.out = n_grobs)
	x$grobs = c(x$grobs, grobs)
	x$layout = new_data_frame(list(t = c(layout$t, t), l = c(layout$l,
															l), b = c(layout$b, b),
								  r = c(layout$r, r), z = c(layout$z, z),
								  clip = c(layout$clip, clip), name = c(layout$name,
																		name)))
	x
}

neg_to_pos <- function(x, max) {
	ifelse(x >= 0, x, max + 1 + x)
}

new_data_frame <- function(x = list(), n = NULL) {
	if (length(x) != 0 && is.null(names(x))) stop("Elements must be named", call. = FALSE)
	lengths = vapply(x, length, integer(1))
	if (is.null(n)) {
		n <- if (length(x) == 0 || min(lengths) == 0) 0 else max(lengths)
	}
	for (i in seq_along(x)) {
		if (lengths[i] == n) next
		if (lengths[i] != 1) stop("Elements must equal the number of rows or 1", call. = FALSE)
		x[[i]] <- rep(x[[i]], n)
	}

	class(x) <- "data.frame"

	attr(x, "row.names") <- .set_row_names(n)
	x
}

#' Plot Density
#'
#' You can use the \code{plot_density} to produce plots that characterize the density.
#' @param dat A data.frame with independent variables and target variable.
#' @param x The name of an independent variable.
#' @param y The name of target variable.
#' @param m  The outlier cutoff.
#' @param g  Number of initial breakpoints for equal frequency binning.
#' @param y_breaks Breaks points of y.
#' @param binwidth Windth of bins for histogram.
#' @param hist If plot the histogram.
#' @param colors_y colors of y.
#' @examples
#' plot_density(dat = lendingclub, x = "annual_inc",y = "emp_length", m =0, hist = FALSE)
#' plot_density(dat = lendingclub, x = "annual_inc", m = 2,
#' colors_y = love_color(type = "line")[c(1,3)])
#' @import ggplot2
#' @importFrom dplyr group_by mutate summarize  summarise n  count %>% filter mutate_if distinct ungroup
#' @export



plot_density <- function(dat, x, y = NULL, m = 3, g = 5, y_breaks = NULL, binwidth = NULL, hist = TRUE, colors_y = c(love_color(type = "deep"), love_color(type = "light"), love_color(type = "shallow"))) {
	dat[[x]] = outlier_fuc(dat[[x]], m = m)
	..density.. = NULL
	max_min = abs(max(dat[[x]], na.rm = TRUE) - min(dat[[x]], na.rm = TRUE))
	if (is.null(binwidth)) {
		binwidth = 1
		if (!is.na(max_min) && max_min != 0) {
			if (max_min <= 100 & digits_num(dat[[x]]) == 0) {
				binwidth = 1
			} else {
				n1 = floor(log(max_min / 100, 10))
				binwidth = round(max_min / 100, - n1)
			}
		}
	}
	if (is.null(y)) {
		if (hist) {
			densitychart = ggplot(dat, aes(dat[[x]])) +
	  	  geom_histogram(position = 'identity', aes(y = ..density..),
					 fill = colors_y[2], alpha = 0.5, binwidth = binwidth) +
				  stat_density(geom = 'line', position = 'identity', color = colors_y[1], size = 1) +
				  ggtitle(paste('Density Distribution of', x)) + xlab(x) + plot_theme()
		} else {
			densitychart = ggplot(dat, aes(dat[[x]])) +
				  stat_density(geom = 'line', position = 'identity', color = colors_y[1], size = 1) +
				  ggtitle(paste('Density Distribution of', x)) + xlab(x) + plot_theme()

		}

	} else {
		if (class(dat[, y]) %in% c("numeric", "double", "integer") && length(unique(dat[, y])) > 10) {
			if (is.null(y_breaks)) {
				y_breaks = get_breaks(dat, x = y, g = g, equal_bins = TRUE, cut_bin = "equal_depth")
			}
		} else {
			dat[, y] = as.character(dat[, y])
			dat = merge_category(dat = dat, char_list  = y, m = g, note = FALSE)
			y_breaks = unique(dat[, y])
		}
		dat[["xs"]] = split_bins(dat = dat, x = y, breaks = y_breaks, bins_no = FALSE)
		dat[["xs"]] = factor(dat[["xs"]], levels = sort(unique(dat[["xs"]])))

		if (is.null(colors_y) && length(colors_y) < length(unique(dat[["xs"]]))) {
			colors_y = c(love_color(type = "deep"), love_color(type = "light"), love_color(type = "shallow"), love_color(type = "dark"))
		}

		if (hist) {
			densitychart = ggplot(dat, aes(dat[[x]], fill = dat[["xs"]])) +
  		  geom_histogram(position = 'identity', aes(y = ..density..),
		alpha = 0.5, binwidth = binwidth) +
			stat_density(geom = 'line', position = 'identity', aes(color = dat[["xs"]]), size = 1) +
			scale_fill_manual(values = colors_y) +
				  scale_color_manual(values = colors_y) +
  		  ggtitle(paste('Density Distribution of', y, "-", x)) + xlab(x) + plot_theme()

		} else {
			densitychart = ggplot(dat, aes(dat[[x]], fill = dat[["xs"]])) +
			stat_density(geom = 'line', position = 'identity', aes(color = dat[["xs"]]), size = 1) +
			scale_fill_manual(values = colors_y) +
				  scale_color_manual(values = colors_y) +
			ggtitle(paste('Density Distribution of', y, "-", x)) + xlab(x) + plot_theme()
		}
	}

	return(densitychart)
}

#' Plot Bar
#'
#' You can use the \code{plot_bar} to produce the barplot.
#' @param dat A data.frame with independent variables and target variable.
#' @param x The name of an independent variable.
#' @param g  Number of initial breakpoints for equal frequency binning.
#' @param breaks Breaks points of x.
#' @param cut_bin  'equal_width' or 'equal_depth'
#' @examples
#' plot_bar(dat = lendingclub, x = "grade")
#' @import ggplot2
#' @importFrom dplyr group_by mutate summarize  summarise n  count %>% filter mutate_if distinct ungroup
#' @export


plot_bar <- function(dat, x, breaks = NULL, g = 10, cut_bin = 'equal_width') {
    xs = percent = NULL
	if (class(dat[, x]) %in% c("numeric", "double", "integer") && length(unique(dat[, x])) > g) {
		if (is.null(breaks)) {
			breaks = get_breaks(dat, x = x, g = g, equal_bins = TRUE, cut_bin = cut_bin)
		}
	} else {
		dat[, x] = as.character(dat[, x])
		dat = merge_category(dat = dat, char_list  = x, m = g, note = FALSE)
		breaks = unique(dat[, x])
	}

	dat[["xs"]] = split_bins(dat = dat, x = x, breaks = breaks)
	data1 = dat %>% dplyr::count(xs) %>% mutate(percent = n / sum(n))
	barchart = ggplot(data1, aes(x = xs, y = percent)) +
	  geom_bar(stat = 'identity', fill = love_color(type = "line")[2]) +
	  geom_text(aes(label = paste(round(percent, 3) * 100, "%")),
				size = 2.5, vjust = 1, hjust = 0.5,
				color = love_color(type = "line")[4], position = position_stack()) +
			  guides(fill = guide_legend(reverse = TRUE)) +
				 ggtitle(paste("Frequency of", x)) +
			  xlab(x) + ylab("percent") + plot_theme(axis_size_x = 6)
	return(barchart)
}


#' Plot Relative Frequency Histogram
#'
#' You can use the \code{plot_relative_freq_histogram} to produce the relative frequency histogram plots.
#' @param dat A data.frame with independent variables and target variable.
#' @param x The name of an independent variable.
#' @param y The name of target variable. Default is NULL.
#' @param g  Number of initial breakpoints for equal frequency binning.
#' @param x_breaks Breaks points of x.
#' @param y_breaks Breaks points of y.
#' @param cut_bin  'equal_width' or 'equal_depth' to produce the breaks points.
#' @examples
#' plot_relative_freq_histogram(dat = lendingclub, x = "grade", y = "dti_joint", g = 7,
#'		cut_bin = 'equal_width')
#' @import ggplot2
#' @importFrom dplyr group_by mutate summarize  summarise n  count %>% filter mutate_if distinct ungroup
#' @export



plot_relative_freq_histogram <- function(dat, x, y = NULL, x_breaks = NULL, y_breaks = NULL, g = 10, cut_bin = 'equal_width') {
	#relative frequency histogram
	dat = char_to_num(dat[c(x,y)], note = FALSE)
	if (class(dat[, x]) %in% c("numeric", "double", "integer") && length(unique(dat[, x])) > g) {
		if (is.null(x_breaks)) {
			x_breaks = get_breaks(dat, x = x, g = g, equal_bins = TRUE, cut_bin = cut_bin)
		}
	} else {
		dat[, x] = as.character(dat[, x])
		dat = merge_category(dat = dat, char_list  = x, m = g, note = FALSE)
		x_breaks = unique(dat[, x])
	}
	dat$x = split_bins(dat = dat, x = x, breaks = x_breaks, bins_no = TRUE)

	if (!is.null(y)) {
		if (class(dat[, y]) %in% c("numeric", "double", "integer") && length(unique(dat[, y])) > g) {
			if (is.null(y_breaks)) {
				y_breaks = get_breaks(dat, x = y, g = g, equal_bins = TRUE, cut_bin = cut_bin)
			}
		} else {
			dat[, y] = as.character(dat[, y])
			dat = merge_category(dat = dat, char_list = y, m = g, note = FALSE)
			y_breaks = unique(dat[, y])
		}
		dat$x = factor(dat[["x"]], levels = rev(sort(unique(dat[["x"]]))))
		dat$y = split_bins(dat = dat, x = y, breaks = y_breaks, bins_no = TRUE)
		dat$y = factor(dat[["y"]], levels = rev(sort(unique(dat[["y"]]))))
		data1 = dat %>%
  	  dplyr::group_by(y) %>% dplyr::count(y, x) %>%
  	  dplyr::mutate(percent = n / sum(n))
		data1$xn = factor(data1$x, levels = rev(unique(data1$x)))
		data1$target = factor(data1$y, levels = rev(unique(data1$y)))
		relative_freq_hist <- ggplot(data1, aes(x = data1$y, y = data1$percent, fill = data1$x)) +
  	  geom_bar(stat = "identity", position = position_stack()) +
  	  geom_text(aes(label = paste(as_percent(data1$percent, digits = 3))),
				size = ifelse(length(data1$x) > 10, 2.1,
							  ifelse(length(data1$x) > 5, 2.5,
									 ifelse(length(data1$x) > 3, 3, 3.3))), vjust = 0, hjust = 1, colour = 'white', position = position_stack()) +
									 coord_flip(xlim = NULL, ylim = NULL, expand = FALSE, clip = "off") +
									 guides(fill = guide_legend(reverse = TRUE)) +
									 ggtitle(paste("Relative Frequency of", x, "-",y)) +
			 ylab(x) + xlab(y) +
								  scale_fill_manual(values = c(love_color(type = "deep")[1:6], love_color(type = "light")[1:8])) +
								  plot_theme(legend.position = "right", title_size = 9, legend_size = 7,
							  axis_title_size = 8)
	} else {
		dat$y = "1"
		dat$x = factor(dat[["x"]], levels = sort(unique(dat[["x"]])))
		data1 = dat %>%
  	  dplyr::group_by(y) %>% dplyr::count(y, x) %>%
  	  dplyr::mutate(percent = n / sum(n))
		data1$xn = factor(data1$x, levels = rev(unique(data1$x)))
		data1$target = factor(data1$y, levels = rev(unique(data1$y)))
		relative_freq_hist <- ggplot(data1, aes(x = data1$y, y = data1$percent, fill = data1$x)) +
  	  geom_bar(stat = "identity", position = position_stack()) +
  	  geom_text(aes(label = paste(as_percent(data1$percent, digits = 3))),
				size = ifelse(length(data1$x) > 10, 2.1,
							  ifelse(length(data1$x) > 5, 2.5,
									 ifelse(length(data1$x) > 3, 3, 3.3))), vjust = 1, hjust = 0, colour = 'white', position = position_stack()) +
									 guides(fill = guide_legend(reverse = FALSE)) +
									 ggtitle(paste("Relative Frequency of", x,"-",y)) +
			 ylab("percent") + xlab(x) +
								  scale_fill_manual(values = c(love_color(type = "deep")[1:6], love_color(type = "light")[1:8])) +
								  plot_theme(legend.position = "right", title_size = 9, legend_size = 7,
							  axis_title_size = 8)
	}
	return(relative_freq_hist)
}



#' Plot Distribution
#'
#' You can use the \code{plot_distribution_x} to produce the distrbution plot of a variable.
#' You can use the \code{plot_distribution} to produce the plots of distrbutions of all variables.
#' @param dat A data.frame with independent variables and target variable.
#' @param x  The name of an independent variable.
#' @param x_list Names of independent variables.
#' @param breaks Splitting points for an independent variable. Default is NULL.
#' @param breaks_list A table containing a list of splitting points for each independent variable. Default is NULL.
#' @param g  Number of initial breakpoints for equal frequency binning.
#' @param m  The outlier cutoff.
#' @param cut_bin A string, if equal_bins is TRUE, 'equal_depth' or 'equal_width', default is 'equal_depth'.
#' @param binwidth Windth of bins for histogram.
#' @param dir_path The path for periodically saved graphic files.
#' @examples
#' plot_distribution_x(dat = lendingclub, x = "max_bal_bc", g = 10,
#'		cut_bin = 'equal_width')
#' plot_distribution(dat = lendingclub, x_list = c("max_bal_bc", "installment"), 
#'      g = 10,dir_path = tempdir(),
#'		cut_bin = 'equal_width')
#' @import ggplot2
#' @importFrom dplyr group_by mutate summarize  summarise n  count %>% filter mutate_if distinct ungroup
#' @export


plot_distribution <- function(dat, x_list = NULL, dir_path = tempdir(), breaks_list = NULL, g = 10, m = 3, cut_bin = 'equal_width') {
	if (is.null(x_list)) {
		x_list = get_names(dat)
	}
	dat = checking_data(dat)
	for (x in x_list) {
		if (!is.null(breaks_list)) {
			if (any(as.character(breaks_list[, "Feature"]) == names(dat[x]))) {
				breaks = breaks_list[which(as.character(breaks_list[, "Feature"]) == names(dat[x])), "cuts"]

			} else {
				breaks = get_breaks(dat = dat, x = x, g = g, equal_bins = TRUE, cut_bin = cut_bin)
			}
		} else {
			breaks = get_breaks(dat = dat, x = x, g = g, equal_bins = TRUE, cut_bin = cut_bin)
		}
		if (class(dat[, x]) %in% c("numeric", "double", "integer") && length(unique(dat[, x])) > 10) {
			densitychart = plot_density(dat = dat, x = x, m = m)
			barchart = plot_bar(dat = dat, x = x, g = g, breaks = breaks, cut_bin = cut_bin)
			distri_plot = multi_grid(grobs = list(densitychart, barchart), ncol = 2, nrow = 1)
		} else {
			relative_freqchart = plot_relative_freq_histogram(dat = dat, x = x)
			barchart = plot_bar(dat = dat, x = x, g = g, breaks = breaks, cut_bin = cut_bin)
			distri_plot = multi_grid(grobs = list(relative_freqchart, barchart), ncol = 2, nrow = 1)
		}
		ggsave(paste0(dir_path, "/", x, ".png"),
		  plot = distri_plot,
		  width = 9, height = 9 / 2, dpi = "retina")
	}
}


#' @rdname plot_distribution
#' @export


plot_distribution_x <- function(dat, x, breaks = NULL, g = 10, m = 3, cut_bin = 'equal_width', binwidth = NULL) {

	dat = checking_data(dat)
	if (class(dat[, x]) %in% c("numeric", "double", "integer") && length(unique(dat[, x])) > 10) {

		densitychart = plot_density(dat = dat, x = x, m = m, binwidth = binwidth)
		barchart = plot_bar(dat = dat, x = x, g = g, breaks = breaks, cut_bin = cut_bin)
		distri_plot = multi_grid(grobs = list(densitychart, barchart), ncol = 2, nrow = 1)
	} else {
		relative_freqchart = plot_relative_freq_histogram(dat = dat, x = x)
		barchart = plot_bar(dat = dat, x = x, g = g, breaks = breaks, cut_bin = cut_bin)
		distri_plot = multi_grid(grobs = list(relative_freqchart, barchart), ncol = 2, nrow = 1)
	}
	return(plot(distri_plot))
}

#' Plot Box
#'
#' You can use the \code{plot_box} to produce boxplot.
#' @param dat A data.frame with independent variables and target variable.
#' @param x The name of an independent variable.
#' @param y The name of target variable.
#' @param g  Number of initial breakpoints for equal frequency binning.
#' @param colors_x colors of x.
#' @examples
#' plot_box(lendingclub, x = "grade", y = "installment", g = 7)
#' @import ggplot2
#' @importFrom dplyr group_by mutate summarize  summarise n  count %>% filter mutate_if distinct ungroup
#' @export

plot_box <- function(dat, y, x = NULL, g = 5, colors_x = c(love_color(type = "deep"), love_color(type = "light"), love_color(type = "shallow"), love_color(type = "dark"))) {
	if (!is.null(x)) {

		if (class(dat[, x]) %in% c("numeric", "double", "integer") && length(unique(dat[, x])) > 10) {
			if (is.null(x_breaks)) {
				x_breaks = get_breaks(dat, x = x, g = g, equal_bins = TRUE, cut_bin = "equal_depth")
			}
		} else {
			dat[, x] = as.character(dat[, x])
			dat = merge_category(dat = dat, char_list = x, m = g, note = FALSE)
			x_breaks = unique(dat[, x])
		}
		dat[["x"]] = split_bins(dat = dat, x = x, breaks = x_breaks, bins_no = TRUE)
		dat[["x"]] = factor(dat[["x"]], levels = sort(unique(dat[["x"]])))
		dat = char_to_num(dat, char_list = y, note = FALSE)
		dat[["y"]] = dat[, y]

		if (is.null(colors_x) && length(colors_x) < length(unique(dat[["x"]]))) {
			colors_y = c(love_color(type = "deep"), love_color(type = "light"), love_color(type = "shallow"), love_color(type = "dark"))
		}

		box_plot <- ggplot(dat, aes(y = y, x = x)) +
		geom_boxplot(position = "identity", aes(fill = x)) + xlab(x) +
		scale_fill_manual(values = colors_x) +
		plot_theme()
	} else {
		dat = char_to_num(dat, char_list = y, note = FALSE)
		dat[["y"]] = dat[, y]
		box_plot <- ggplot(dat, aes(y = y)) +
		geom_boxplot(position = "identity", fill = colors_x[1]) + xlab(x) +
		plot_theme()
	}
	return(box_plot)
}




outlier_fuc = function(x, m = 1) {
	if (class(x) == 'numeric' | class(x) == 'integer') {
		QL = quantile(x, 0.01, na.rm = TRUE)
		QU = quantile(x, 0.99, na.rm = TRUE)
		QU_QL = QU - QL
		if (QU_QL == 0) {
			out_imp01 = quantile(x, 0.9999, na.rm = TRUE)
		} else {
			out_imp01 = round(QU + m * QU_QL, 0)
		}
		x[x > out_imp01] = out_imp01
	}
	return(x)
}
