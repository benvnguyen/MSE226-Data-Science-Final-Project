# =============================================================================
# nn.R: feed-forward neural network (torch).
#
# Architecture (default hidden = c(4, 3)):
#   inputs (p standardized columns) -> Linear(p, 4) -> ReLU
#                                   -> Linear(4, 3) -> ReLU
#                                   -> Linear(3, 1)  = predicted earnings / $10,000
#
# Training techniques (CS230-style):
#   - Inputs standardized to mean 0, SD 1 (recipe, estimated on the training fold)
#   - He (Kaiming) initialization for ReLU layers; Xavier for the linear output layer
#   - Output bias starts at the training median, so the net starts at the baseline
#   - Adam optimizer on shuffled mini-batches
#   - L2 regularization through Adam's weight_decay (adds l2 * w to every gradient)
#   - Learning-rate decay: halve the rate when validation loss stalls for 5 epochs
#   - Early stopping on a validation split of the TRAINING fold (by household);
#     the weights from the best epoch are restored
#   - Loss = absolute error (L1) by default, the same quantity MAE measures
# =============================================================================

suppressPackageStartupMessages(library(torch))

nn_net <- nn_module(
  "EarningsNet",
  initialize = function(p_in, hidden, y0) {
    dims <- c(p_in, hidden)
    self$n_hidden <- length(hidden)
    self$hidden <- nn_module_list(lapply(seq_along(hidden), function(i) nn_linear(dims[i], dims[i + 1])))
    self$out <- nn_linear(dims[length(dims)], 1)
    for (i in seq_len(self$n_hidden)) {
      nn_init_kaiming_normal_(self$hidden[[i]]$weight, nonlinearity = "relu")
      nn_init_zeros_(self$hidden[[i]]$bias)
    }
    nn_init_xavier_normal_(self$out$weight)
    with_no_grad(self$out$bias$fill_(y0))
  },
  forward = function(x) {
    for (i in seq_len(self$n_hidden)) x <- nnf_relu(self$hidden[[i]](x))
    self$out(x)$squeeze(2)
  })

nn_n_weights <- function(p_in, hidden) {
  dims <- c(p_in, hidden, 1)
  sum((head(dims, -1) + 1) * dims[-1])
}

# Train on (X, y); early-stop on (Xv, yv). Returns the net (best epoch) and its curve.
# net$train(TRUE) = training mode; net$train(FALSE) = inference mode.
nn_train <- function(X, y, Xv, yv, params, seed = 32) {
  torch_manual_seed(seed); set.seed(seed)
  Xt <- torch_tensor(X, dtype = torch_float()); yt <- torch_tensor(y, dtype = torch_float())
  Xvt <- torch_tensor(Xv, dtype = torch_float()); yvt <- torch_tensor(yv, dtype = torch_float())
  net <- nn_net(ncol(X), params$hidden, y0 = median(y))
  opt <- optim_adam(net$parameters, lr = params$lr, weight_decay = params$l2)
  sched <- lr_reduce_on_plateau(opt, mode = "min", factor = 0.5, patience = 5)
  lossf <- if (params$loss == "mse") nnf_mse_loss else nnf_l1_loss
  n <- nrow(X); bs <- params$batch_size
  best <- Inf; best_state <- NULL; best_ep <- 0; wait <- 0
  curve <- vector("list", params$max_epochs)
  for (ep in seq_len(params$max_epochs)) {
    net$train(TRUE); perm <- sample.int(n); tl <- 0
    for (b in seq(1, n, by = bs)) {
      idx <- perm[b:min(b + bs - 1, n)]
      opt$zero_grad()
      loss <- lossf(net(Xt[idx, , drop = FALSE]), yt[idx, drop = FALSE])   # drop = FALSE: a 1-row batch stays 2-D
      loss$backward(); opt$step()
      tl <- tl + loss$item() * length(idx)
    }
    net$train(FALSE)
    vl <- with_no_grad(lossf(net(Xvt), yvt)$item())
    sched$step(vl)
    curve[[ep]] <- c(epoch = ep, train_loss = tl / n, val_loss = vl, lr = opt$param_groups[[1]]$lr)
    if (vl < best - 1e-5) {
      best <- vl; best_ep <- ep; wait <- 0
      best_state <- lapply(net$state_dict(), function(t) t$clone())
    } else if ((wait <- wait + 1) >= params$patience) break
  }
  net$load_state_dict(best_state)
  curve <- as.data.frame(do.call(rbind, curve[seq_len(ep)]))
  list(net = net, curve = curve, best_epoch = best_ep, epochs_run = ep)
}

nn_fit <- function(train, vars, params, seed = 32) {
  rec <- prep(make_recipe(vars, "nn", train))
  x <- bake(rec, NULL)
  X <- as.matrix(x[setdiff(names(x), "y")])
  # validation split for early stopping, grouped by household
  set.seed(seed)
  hh <- unique(train$SERIALNO)
  val_hh <- sample(hh, round(params$val_frac * length(hh)))
  v <- train$SERIALNO %in% val_hh
  tr <- nn_train(X[!v, , drop = FALSE], x$y[!v], X[v, , drop = FALSE], x$y[v], params, seed)
  tr$rec <- rec; tr$inputs <- colnames(X); tr$hidden <- params$hidden
  tr
}

nn_predict <- function(m, newdata) {
  x <- bake(m$rec, newdata)
  X <- as.matrix(x[m$inputs])
  m$net$train(FALSE)
  with_no_grad(as.numeric(as_array(m$net(torch_tensor(X, dtype = torch_float())))))
}

nn_sample_configs <- function(cfg, n, seed = 32) {
  set.seed(seed)
  lu <- function(r) exp(runif(n, log(r[1]), log(r[2])))     # log-uniform
  data.frame(lr = signif(lu(cfg$search_lr), 3), l2 = signif(lu(cfg$search_l2), 3),
             batch_size = sample(cfg$search_batch_size, n, replace = TRUE))
}
