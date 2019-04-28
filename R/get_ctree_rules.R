#' Parse party ctree rules
#'
#'
#' \code{get_ctree_rules} This function is used to parse party ctree rules and percentage of bad under each rule.
#' @param tree_fit  A tree model object.
#' @param train_dat  A data.frame of train.
#' @param target The name of target variable.
#' @param tree_control the list of parameters to control cutting initial breaks by decision tree.
#' @param test_dat  A data.frame of test.
#' @param seed  Random number seed. Default is 46.

#' @return A data frame with tree rules and bad percent under each rule.
#'
#' @examples
#' train_test <- train_test_split(UCICreditCard, split_type = "Random", prop = 0.8, save_data = FALSE)
#' dat_train = train_test$train
#' dat_test = train_test$test
#' dat_train$default.payment.next.month = as.numeric(dat_train$default.payment.next.month)
#' get_ctree_rules(tree_fit = NULL, train_dat = dat_train[, 8:26],
#' target ="default.payment.next.month", test_dat = dat_test)
#'
#' @importFrom data.table fwrite melt fread dcast
#' @importFrom rpart rpart rpart.control
#' @importFrom sqldf sqldf
#' @export




get_ctree_rules = function(tree_fit = NULL,train_dat = NULL, target = NULL, test_dat = NULL,
                           tree_control =  list(p = 0.05, cp = 0.0001,
                                                         xval = 1, maxdepth = 10),seed = 46) {
    rules_list = split_rule = terminal_nodes = nodes_no = depth = node_rules = NULL

     if(is.null(tree_fit)){
       cp = ifelse(!is.null(tree_control[["cp"]]), tree_control[["cp"]], 0.00001)
       xval = ifelse(!is.null(tree_control[["xval"]]), tree_control[["xval"]], 5)
       maxdepth = ifelse(!is.null(tree_control[["maxdepth"]]), tree_control[["maxdepth"]], 10)
       p = ifelse(!is.null(tree_control[["p"]]), tree_control[["p"]], 0.02)
       set.seed(seed)
       trcontrol <- rpart.control(minbucket = round(nrow(train_dat) * p),
                                  cp = cp,
                                  xval = xval,
                                  maxdepth = maxdepth)

       Formula <- as.formula(paste(target, ".", sep = ' ~ '))
      tree_fit =  rpart(data = train_dat, Formula
            , control = trcontrol
            , parms = list(split = "information"))
    }

    if (!is.null(tree_fit) && class(tree_fit)[1] == "rpart") {
        rules_list = capture.output(tree_fit)[-c(1:5)]
        split_rule = strsplit(rules_list, ")|;")
        terminal_nodes = sapply(split_rule, function(x) x[[1]][1])
        node_rules$tree_nodes = 1:length(terminal_nodes)
        nodes_no = as.numeric(row.names(tree_fit$frame))
        node_rules$depth = floor(log(nodes_no, base = 2) + 1e-07) + 1
        rownam = 1:length(terminal_nodes)
        tree_rules = sapply(1:length(terminal_nodes), function(x) as.character(split_rule[x][[1]][2]))
       #split node rules
        tree_rules_split = strsplit(tree_rules, "\\s{1,10}")
        node_rules$tree_rules = sapply(tree_rules_split, function(x) {
            x = x[which(x != "")]
            y = NULL
            if (any(grepl("<", x))) {
                y = paste(x[1], x[2])
            } else {
                y = x[1]
            }
            y
        })
        node_rules = as.data.frame(node_rules)
        terminal_tree_nodes = which(grepl("\\*", rules_list))
        tree_where = data.frame(tree_nodes = as.integer(tree_fit$where), target = as.character(unlist(tree_fit$y)))
        target = as.character(tree_fit$terms[[2]])

    } else {
        if (class(tree_fit)[1] == "BinaryTree") {
            rules_list = capture.output(tree_fit@tree)
            split_rule = strsplit(rules_list, ")|;")
            tree_nodes = sapply(split_rule, function(x) x[[1]][1])
            depth = (nchar(gsub("\\d{1,4}", "", tree_nodes))) / 2 + 1
            tree_rules = sapply(1:length(split_rule), function(x) as.character(split_rule[x][[1]][2]))
            node_rules = data.frame(tree_nodes = as.integer(tree_nodes),
            depth = depth, tree_rules = as.character(tree_rules),
            row.names = 1:length(tree_nodes), stringsAsFactors = FALSE)
            terminal_tree_nodes = which(!grepl("*>[:digit:]*|*<=[:digit:]*", rules_list))
            rownam = as.integer(rownames(node_rules))
            node_rules[terminal_tree_nodes, "tree_rules"] = ""
            tree_where = data.frame(tree_nodes = as.integer(tree_fit@where),
            target = as.character(unlist(tree_fit@responses@variables)))
            target = names(tree_fit@responses@variables)
        } else {
            stop("Support only rpart tree & ctree (party package).\n")
        }
    }
    terminal_rules = train_sum = rule_no = c_rules = final_rules = train_ks = dt_rules = dt_rules_k = NULL

    for (i in terminal_tree_nodes) {
        inds = ifelse(any(node_rules$tree_rules == "root"), 2, 1)
        c_rules = sapply(inds : node_rules$depth[i], function(j) {
            rule_no = which(rownam == max(which(node_rules$depth == j & rownam <= rownam[i])))
            node_rules$tree_rules[rule_no]
            })
        c_rules = c_rules[c_rules != ""]
        terminal_rules[i] = paste(terminal_rules[i], c_rules, collapse = "  AND ", sep = "")
        terminal_rules[i] = gsub("NA", " ", terminal_rules[i])
    }
    #final_rules
    final_rules = cbind(node_rules, terminal_rules)[terminal_tree_nodes, c("tree_nodes", "terminal_rules")]
    #bad_percent
    X.train_total = X.train_B = X.test_total = X.test_B = tree = train_total = test_total = G = B = `%train` = `#train` = NULL
    #train
    tree_where$target = ifelse(tree_where[, "target"] %in% list("0", "good", 0), "G", "B")
    tree_where_pct = tree_where %>%
    dplyr::group_by(tree_nodes) %>%
    dplyr::count(tree_nodes, target) %>%
    dplyr::mutate(train_bad_pct = as_percent(n / sum(n), digits = 4))
    train_sum <- dcast(tree_where_pct[c("tree_nodes", "target", "n")],
                       tree_nodes ~ target, value.var = "n")
    train_sum[is.na(train_sum)] <- 0
    train_ks <- transform(train_sum,
                     train_total = G + B,
                     `%train_total` = round((G + B) / sum(G + B), 3),
                     `%train_B` = round(B / (G + B), 3)
                     )
    dt_rules = merge(final_rules, train_ks)
    dt_rules = dt_rules[order(dt_rules$X.train_B),]
    dt_rules_k = transform(dt_rules,
                           X.train_total = as_percent(X.train_total, digits = 3),
                           X.train_B = as_percent(X.train_B, digits = 3),
                           `%train_cumsum` = as_percent(cumsum(train_total) / sum(train_total), digits = 4),
                           `%train_cumB_rate` = as_percent(cumsum(B) / cumsum(train_total), digits = 4),
                           `%train_cumB` = as_percent(cumsum(B) / sum(B), digits = 4),
                           `%train_cumG` = as_percent(cumsum(G) / sum(G), digits = 4)
                           )
    names(dt_rules_k) = c("tree_nodes", "tree_rules", "train_B", "train_G",
                          "#train", "%train", "%train_B", "%train_cumsum","%train_cumB_rate",
                          "%train_cumB","%train_cumG")

    if (!is.null(test_dat)) {
        test_dat = checking_data(dat = test_dat, target = target)
        test_dat$target = test_dat[, target]
        test_rules = list()
        sql_s = sub_dat = test_rules_dt = tree_test_pct = test_sum = test_ks = dt_rules_ts = dt_rules_ts1 = NULL
        for (i in 1:nrow(final_rules)) {
            sql_s = paste("select * from test_dat where", as.character(final_rules[i, 2]))
            sub_dat = sqldf(sql_s)
            colnames(sub_dat) <- iconv(colnames(sub_dat), from = "UTF-8", to = "GBK")
            if (nrow(sub_dat) > 0) {
                sub_dat$tree_nodes = final_rules[i, 1]
                test_rules[[i]] = sub_dat[c("tree_nodes", "target")]
            }
        }
        test_rules_dt = Reduce("rbind", test_rules)
        test_rules_dt$target = ifelse(test_rules_dt[, "target"] %in% list("0", "good", 0), "G", "B")
        tree_test_pct = test_rules_dt %>%
          dplyr::group_by(tree_nodes) %>%
          dplyr::count(tree_nodes, target) %>%
          dplyr::mutate(test_bad_pct = as_percent(n / sum(n), digits = 4))

        test_sum <- dcast(tree_test_pct[c("tree_nodes", "target", "n")],
                          tree_nodes ~ target, value.var = "n")
        test_sum[is.na(test_sum)] <- 0
        test_ks <- transform(test_sum,
                     test_total = G + B,
                     `%test_total` = round((G + B) / (sum(G + B)), 3),
                     `%test_B` = round(B / (G + B), 3))
        #  test_ks = test_ks[c(1, 4:6)]
        #  names(test_ks) = c("tree_nodes", "#test", "%test", "%test_B")
        dt_rules_ts = merge(dt_rules_k, test_ks)
        dt_rules_ts = dt_rules_ts[order(de_percent(dt_rules_ts$`%train_B`)),]
        dt_rules_ts1 = dt_rules_ts %>%
        dplyr::mutate(psi = round((de_percent(`%train`) - X.test_total) * log(de_percent(`%train`) / X.test_total), 3),
        `#total` = `#train` + test_total,
        `%total` = as_percent((`#train` + test_total) / sum(`#train` + test_total),3),
    X.test_total = as_percent(X.test_total, digits = 4),
                   X.test_B = as_percent(X.test_B, digits = 4),
        `%test_cumsum` = as_percent(cumsum(test_total) / sum(test_total), digits = 4),
       `%test_cumB_rate` = as_percent(cumsum(B) / cumsum(test_total), digits = 4),
       `%test_cumB` = as_percent(cumsum(B) / sum(B), digits = 4),
       `%test_cumG` = as_percent(cumsum(G) / sum(G), digits = 4)
        )
        names(dt_rules_ts1) = c("tree_nodes", "tree_rules", "train_B", "train_G",
                                "#train", "%train", "%train_B", "%train_cumsum",
                                "%train_cumB_rate", "%train_cumB", "%train_cumG", "test_B", "test_G",
                                "#test", "%test", "%test_B", "psi", "#total", "%total",
                                "%test_cumsum", "%test_cumB_rate","%test_cumB","%test_cumG")
        dt_rules_k = dt_rules_ts1[c("tree_nodes", "tree_rules", "#total", "%total", "train_B", "train_G",
                                    "#train", "%train", "%train_B", "%train_cumsum", "%train_cumB_rate",
                                    "%train_cumB", "%train_cumG", "test_B", "test_G",
                                    "#test", "%test", "%test_B", "%test_cumsum", "%test_cumB_rate",
                                    "%test_cumB", "%test_cumG", "psi")]
    }
    return(dt_rules_k)
}
