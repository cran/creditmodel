#' Table of Binning
#'
#' \code{get_bins_table}  is used to generates summary information of varaibles.
#' \code{get_bins_table_all} can generates bins table for all specified independent variables.
#' @param dat A data.frame with independent variables and target variable.
#' @param target The name of target variable.
#' @param x_list Names of independent variables.
#' @param ex_cols A list of excluded variables. Regular expressions can also be used to match variable names. Default is NULL.
#' @param pos_flag Value of positive class, Default is "1".
#' @param breaks_list A table containing a list of splitting points for each independent variable. Default is NULL.
#' @param breaks Splitting points for an independent variable. Default is NULL.
#' @param occur_time The name of the variable that represents the time at which each observation takes place.
#' @param oot_pct  Percentage of observations retained for overtime test (especially to calculate PSI). Defualt is 0.7
#' @param parallel Logical, parallel computing. Default is FALSE.
#' @param note   Logical, outputs info. Default is TRUE.
#' @param save_data Logical, save results in locally specified folder. Default is TRUE
#' @param file_name  The name for periodically saved bins table file. Default is "bins_table".
#' @param dir_path The path for periodically saved bins table file. Default is "./variable".
#' @seealso
#' \code{\link{get_iv}},
#' \code{\link{get_iv_all}},
#' \code{\link{get_psi}},
#' \code{\link{get_psi_all}}
#' @examples
#' breaks_list = get_breaks_all(dat = UCICreditCard, x_list = names(UCICreditCard)[3:4],
#' target = "default.payment.next.month", equal_bins =TRUE,best = FALSE,g=5,
#' ex_cols = "ID|apply_date", save_data = FALSE)
#' get_bins_table_all(dat = UCICreditCard, breaks_list = breaks_list,
#' target = "default.payment.next.month", occur_time = "apply_date")
#' @importFrom data.table fwrite melt fread dcast
#' @export



get_bins_table_all <- function(dat,  x_list = NULL, target = NULL, pos_flag = NULL,
                               ex_cols = NULL,breaks_list = NULL,  parallel = FALSE,
                               note = FALSE, occur_time = NULL, oot_pct = 0.7,
                               save_data = FALSE, file_name = NULL, dir_path = tempdir()) {
    cat(paste("[NOTE] start processing Bins Table ...."), "\n")
    opt = options(stringsAsFactors = FALSE) #
    if (is.null(x_list)) {
        if (!is.null(breaks_list)) {
            x_list =  unique(as.character(breaks_list[,"Feature"]))
        } else {
            x_list = get_x_list(x_list = x_list, dat_train = dat,
                                dat_test = NULL, ex_cols = c(target, occur_time, ex_cols))
        }
    }
    bins_list = loop_function(func = get_bins_table, x_list = x_list,
                              args = list(dat = dat,  target = target,
                                          breaks = NULL, breaks_list = breaks_list,
                                          pos_flag = pos_flag, occur_time = occur_time,
                                          oot_pct = oot_pct, note = note),
                              bind = "rbind", parallel = parallel, as_list = FALSE)
    if (save_data) {
        dir_path = ifelse(is.null(dir_path) || !is.character(dir_path) || !grepl('.|/|:', dir_path),
                      tempdir(), dir_path)
        if (!dir.exists(dir_path)) dir.create(dir_path)
        if (!is.character(file_name)) file_name = NULL
        save_dt(bins_list, file_name = ifelse(is.null(file_name), "bins.table", paste(file_name, "bins.table", sep = ".")), dir_path = dir_path, note = FALSE)
    }
    options(opt) # reset
    return(bins_list)
}

#' @param x  The name of an independent variable.
#' @rdname get_bins_table_all
#' @export

get_bins_table <- function(dat, x, target = NULL, pos_flag = NULL,
                           breaks = NULL, breaks_list = NULL,
                           occur_time = NULL, oot_pct = 0.7, note = FALSE) {

    opt = options(stringsAsFactors = FALSE) #
    good = bad = NULL
    if (is.null(breaks)) {
        if (!is.null(breaks_list)) {
            breaks = breaks_list[which(as.character(breaks_list[, "Feature"]) == names(dat[x])), "cuts"]
        }
        if (is.null(breaks)) {
            stop("breaks or breaks_list is missing.\n")
        }
    }
    if (!is.null(target)) {
        if (length(unique(dat[, target])) > 1) {
            if (is.null(pos_flag)) {
                dat$target = ifelse(dat[, target] %in% list("1", "bad", 1), "bad", "good")
            } else {
                dat$target = ifelse(dat[, target] %in% pos_flag, "bad", "good")
                if (length(unique(dat$target)) == 1) {
                    stop(paste("The value in pos_flag is not one of the value of  target.\n"))
                }
            }
        } else {
            stop(paste("The value of  target is unique.\n"))
        }
    } else {
        stop(paste("The target variable is missing.\n"))
    }


    best_bins = split_bins(dat = dat, x = x, breaks = breaks, bins_no = TRUE)
    dt_bins <- table(best_bins, dat$target)
    rm(best_bins)
    dt_bins[which(dt_bins == 0)] <- 1
    dt_bins = data.frame(unclass(dt_bins))

    #bins table
    df_bins <- within(dt_bins, {
        bins = row.names(dt_bins)
        Feature = names(dat[x])
        cuts = breaks[1:nrow(dt_bins)]
        total = good + bad
        `%total` = as_percent((good + bad) / (sum(good) + sum(bad)), digits = 3)
        `%good` = as_percent(good / sum(good), 2)
        `%bad` = as_percent(bad / sum(bad), 2)
        bad_rate = as_percent(bad / (good + bad), digits = 3)
        `GB_index` = round(((good / bad) / (sum(good) / sum(bad))) * 100, 0)
        woe = round(log((good / sum(good)) / (bad / sum(bad))), 4)
        iv = round(((good / sum(good)) - (bad / sum(bad))) * woe, 4)
    })
    rm(dt_bins)

    if (!is.null(occur_time)) {
        dt_psi = get_psi(dat = dat, x = x, breaks = breaks,
                              pos_flag = pos_flag,
                              occur_time = occur_time,
                              oot_pct = oot_pct,
                              as_table = TRUE, note = FALSE)
        df_bins = merge(df_bins,dt_psi[,c("Bins","PSI_i")],by.x = "bins", by.y = "Bins",all.x = TRUE)
        df_bins[is.na(df_bins)] = 0
        df_bins = re_name(df_bins,"PSI_i","psi")
    }else{
      df_bins$psi = rep(-1, nrow(df_bins))
    }
    df_bins = df_bins[c("Feature", "bins", "cuts", "total", "good", "bad",
                        "%total", "%good", "%bad", "bad_rate", "woe", "GB_index", "iv", "psi")]

    total_sum = c("Total", "--", "--", sum(df_bins$total),
                  sum(df_bins$good), sum(df_bins$bad),
                  as_percent(1, digits = 2), as_percent(1, digits = 2),
                  as_percent(1, digits = 2),
                  as_percent(sum(df_bins$bad) / sum(df_bins$total), 2), 0, 100,
                  round(sum(df_bins$iv), 3), round(sum(df_bins$psi), 3))
    df_bins = rbind(df_bins, total_sum)
    rownames(df_bins) = NULL
    if (note) {
        cat(paste(x, "  IV:", df_bins$iv[nrow(df_bins)], "PSI:",
                  df_bins$psi[nrow(df_bins)], "\n", collapse = "\t"))
    }
    options(opt) # reset
    return(df_bins)
}
