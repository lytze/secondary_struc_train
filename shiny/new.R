#!/usr/local/bin/Rscript
library(lattice)
library(knitr)
cmd_arg <- commandArgs(trailingOnly = T)
# cmd_arg <- c('000001', '14', '1', '3', '3', '0.5', '0.25', '20', '1')
wid <- cmd_arg[1]
addLog <- function(text) {
    cat(paste0(Sys.time(), ': ', text, '\n'),
        file = paste0('.data/', wid, '_log'), append = T)
}
changeState <- function(text) {
    cat(paste0(text, '\n'), file = paste0('.data/', wid, '_sta'))
}
cat(Sys.getpid(), sep = '\n', file = paste0('.data/', wid, '_pid'))
addLog('Parsing Command Line Arguments')
changeState('running')
wsz <- as.numeric(cmd_arg[2])
nhl <- as.numeric(cmd_arg[3])
nhu <- as.numeric(cmd_arg[3 + 1:nhl])
oty <- as.numeric(cmd_arg[4 + nhl])
dco <- as.numeric(cmd_arg[5 + nhl])
cer <- as.numeric(cmd_arg[6 + nhl])
mfl <- as.numeric(cmd_arg[7 + nhl])
set.seed(as.numeric(cmd_arg[8 + nhl]))
addLog('Preparing ANN Class and Methods')
newANN <- function() {
    new <- TRUE
    window_size <- wsz
    num_units <- c(
        INPUT = 21 * (2 * wsz + 1),
        HIDDEN = nhu,
        OUTPUT = oty
    )
    num_layer <- length(num_units)
    displacement_coef <- dco
    critical_error <- cer
    maxium_loop <- mfl
    local_str <- NULL
    local_truth <- NULL
    amino_acids <- 1:21
    names(amino_acids) <- c(
        'A', 'R', 'N', 'D', 'C', 'Q', 'E', 'G', 'H', 'I',
        'L', 'K', 'M', 'F', 'P', 'S', 'T', 'W', 'Y', 'V',
        '-'
    )
    bias <- list()
    weight <- list()
    delta <- list()
    output <- list()
    for (i in 2:num_layer) {
        bias[[i]] <- rep(1, times = num_units[i])
        weight[[i]] <- matrix(runif(num_units[i - 1] * num_units[i], min = -0.3, max = 0.3),
                              nrow = num_units[i], ncol = num_units[i - 1])
        delta[[i]] <- rep(0, times = num_units[i])
    }
    sigma <- function(x) {
        1 / (1 + exp(-x))
    }
    forwardTransmit <- function() {
        output[[1]] <<- rep(0, times = num_units[1])
        for (i in 1:(2 * window_size + 1)) {
            output[[1]][(i - 1) * 21 + amino_acids[local_str[i]]] <<- 1
        }
        for (i in 2:num_layer) {
            output[[i]] <<- sigma(
                apply(weight[[i]], 1, function(units_from) sum(units_from * output[[i - 1]])) +
                    bias[[i]]
            )
        }
    }
    backPropagate <- function() {
        delta[[num_layer]] <<- output[[num_layer]] * (1 - output[[num_layer]]) *
            (output[[num_layer]] - local_truth)
        if (num_layer > 2) {
            for (i in (num_layer - 1):2) {
                delta[[i]] <<- output[[i]] * (1 - output[[i]]) *
                    apply(weight[[i + 1]], 2, function(units_to) sum(units_to * delta[[i + 1]]))
            }
        }
        for (i in 2:num_layer) {
            weight[[i]] <<- weight[[i]] - displacement_coef * delta[[i]] %o% output[[i - 1]]
            bias[[i]] <<- bias[[i]] - displacement_coef * delta[[i]]
        }
    }
    getPrediction <- function() {
        finale <- output[[num_layer]]
        if (num_units[num_layer] == 2) {
            if (sum(finale > 0.5) %in% c(0, 2)) {
                return('-')
            } else {
                if (finale[1] > 0.5) return('H') else return('E')
            }
        } else {
            wh <- which(finale == max(finale))
            if (length(wh) != 1) wh <- sample(wh, 1)
            switch(wh, `1` = 'H', `2`= 'E', `3` = '-')
        }
    }
    getLocalTruth <- function(target) {
        if (num_units[num_layer] == 2) {
            switch(target, H = c(1, 0), E = c(0, 1), `-` = c(0, 0))
        } else {
            switch(target, H = c(1, 0, 0), E = c(0, 1, 0), `-` = c(0, 0, 1))
        }
    }
    list(
        wid = wid,
        train = function(input, target, log = T) {
            n <- length(input)
            cushion <- paste0(rep('-', window_size), collapse = '')
            input <- strsplit(paste0(cushion, input, cushion), '')
            target <- strsplit(target, '')
            predicted <- list()
            loop <- 1
            lml <- 0
            while (loop <= maxium_loop) {
                for (i in sample(1:n, n)) {
                    res <- character()
                    for (j in 1:length(target[[i]])) {
                        local_str <<- input[[i]][j:(j + 2 * window_size)]
                        local_truth <<- getLocalTruth(target[[i]][j])
                        forwardTransmit()
                        res <- c(res, getPrediction())
                        backPropagate()
                    }
                    predicted[[i]] <- res
                }
                upd <- unlist(predicted)
                utg <- unlist(target)
                err <- sum(upd != utg) / length(utg)
                if (log == TRUE) {
                    cat(paste0('In Loop #', loop,
                               ' Error Rate = ', format(err, digit = 3), '\n'))
                } else if (log != FALSE) {
                    cat(paste0('-------------------- In Loop #', loop,
                               ' Error Rate = ', format(err, digit = 3), '\n'),
                        file = log, append = T)
                }
                if (err < critical_error) loop <- maxium_loop + 1
                loop <- loop + 1
            }
            list(Prediction = upd, Reference = utg)
        },
        crossValidate = function(input, target) {
            n <- length(input)
            cushion <- paste0(rep('-', window_size), collapse = '')
            input <- strsplit(paste0(cushion, input, cushion), '')
            target <- strsplit(target, '')
            predicted <- list()
            for (i in 1:n) {
                res <- character()
                for (j in 1:(length(input[[i]]) - 2 * window_size)) {
                    local_str <<- input[[i]][j:(j + 2 * window_size)]
                    forwardTransmit()
                    res[j] <- getPrediction()
                }
                predicted[[i]] <- res
            }
            upd <- unlist(predicted)
            utg <- unlist(target)
            list(Prediction = upd, Reference = utg)
        },
        predict = function(input) {
            n <- length(input)
            cushion <- paste0(rep('-', window_size), collapse = '')
            input <- strsplit(paste0(cushion, input, cushion), '')
            predicted <- list()
            for (i in 1:n) {
                res <- character()
                for (j in 1:(length(input[[i]]) - 2 * window_size)) {
                    local_str <<- input[[i]][j:(j + 2 * window_size)]
                    forwardTransmit()
                    res[j] <- getPrediction()
                }
                predicted[[i]] <- res
            }
            sapply(predicted, paste0, collapse = '')
        },
        getCoef = function(dirn = NULL) {
            if (is.null(dirn)) {
                dirn <- paste0(wid, '_coef')
            }
            if (!file.exists(dirn)) {
                dir.create(dirn)
            }
            for (i in 2:num_layer) {
                write.csv(weight[[i]],
                          file = paste0(dirn, '/weight_', i - 1, '_to_', i, '.csv'))
                write.csv(bias[[i]],
                          file = paste0(dirn, '/bias_', i, '.csv'))
            }
        },
        readMe = function() {
            cat(sep = '\n',
                'ann$wid to get the work ID of your ANN model.',
                'ann$train(input, target) to train the model further with additional data,',
                '    notice that the length of input and target should be the same.',
                '    this call returns a list of 2 vectors, the combined prediction and',
                '    reference/target values.',
                'ann$crossValidate(input, target) to check another input-target pair. Also',
                '    returns a list like ann$train().',
                'ann$predict(input) accept a character vecter of sequences, returns the',
                '    predicted structure.',
                'ann$getCoef(dirn) to output csv files of weight/bias tables to given directory.')
        }
    )
}
addLog('Loading Training/Testing Data')
ftrain <- readLines(paste0('.data/', wid, '_train'))
train_input <- ftrain[1:(length(ftrain) / 2) * 2 - 1]
train_target <- ftrain[1:(length(ftrain) / 2) * 2]
ftest <- readLines(paste0('.data/', wid, '_test'))
test_input <- ftest[1:(length(ftest) / 2) * 2 - 1]
test_target <- ftest[1:(length(ftest) / 2) * 2]
addLog('Initializing ANN')
ann <- newANN()
addLog('Training Model')
training <- ann$train(train_input, train_target, log = paste0('.data/', wid, '_log'))
addLog('Cross Validating')
testing <- ann$crossValidate(test_input, test_target)
addLog('Generating Model Report')
nm <- c('Coil', 'Sheet', 'Helix')
err_train <- paste(signif(sum(training[[1]] != training[[2]]) / length(training[[1]]) * 100, digits = 4), '%')
table_train <- table(training[[1]], training[[2]])
table_train <- signif(sweep(table_train, 1, rowSums(table_train) / 100, '/'), digits = 4)
rownames(table_train) <- nm
colnames(table_train) <- nm
plt <- tempfile(wid, '.tmp')
png(plt)
print(
levelplot(table_train, col.regions = gray(seq(1, 0, -0.01)),
          xlab = 'Reference', ylab = 'Prediction', at = seq(0, 100, by = 5), panel = function(...) {
    panel.levelplot(...)
    panel.text(x = rep(1:3, each = 3), y = rep(1:3, 3),
               labels = paste(table_train, '%'), col = ifelse(table_train > 60, 'white', 'black'))
}))
dev.off()
uri <- image_uri(plt)
unlink(plt)
cat(err_train, uri, sep = '\n',file = paste0('.data/', wid, '_rec'))
err_test <- paste(signif(sum(testing[[1]] != testing[[2]]) / length(testing[[1]]) * 100, digits = 4), '%')
table_test <- table(testing[[1]], testing[[2]])
table_test <- signif(sweep(table_test, 1, rowSums(table_test) / 100, '/'), digits = 4)
rownames(table_test) <- nm
colnames(table_test) <- nm
plt <- tempfile(wid, '.tmp')
png(plt)
print(
    levelplot(table_test, col.regions = gray(seq(1, 0, -0.01)),
              xlab = 'Reference', ylab = 'Prediction', at = seq(0, 100, by = 5), panel = function(...) {
                  panel.levelplot(...)
                  panel.text(x = rep(1:3, each = 3), y = rep(1:3, 3),
                             labels = paste(table_test, '%'), col = ifelse(table_test > 60, 'white', 'black'))
              }))
dev.off()
uri <- image_uri(plt)
unlink(plt)
cat(err_test, uri, sep = '\n', file = paste0('.data/', wid, '_rec'), append = T)
addLog('Saving Model Object')
save(ann, file = paste0('.data/', wid, '_model'))
ann$getCoef(paste0('.data/', wid, '_coef'))
setwd('.data')
zip(paste0(wid, '_coef'), paste0(wid, '_coef'))
unlink(paste0(wid, '_coef'), recursive = T)
setwd('..')
addLog('Clearing Data Files')
file.remove(paste0('.data/', wid, c('_test', '_train')))
addLog('Job is Done')
Sys.sleep(1)
changeState('idle')
