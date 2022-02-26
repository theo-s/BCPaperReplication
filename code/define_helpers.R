# Helper function to do bias corrected CATE predictions for RF -----------------
EstimateCorrectedCATE <- function(theObject,
                                  feature_new,
                                  correction="none")
{
  feature_new <- as.data.frame(feature_new)

  # Check if we want to do bias correction predictions
  if (correction == "bc1") {
    return(
      Rforestry::correctedPredict(theObject@forest, cbind(feature_new, tr = 1),
                                  params.forestry = list("mtry" = 2),
                                  feats = c((ncol(feature_new)+1))) -
        Rforestry::correctedPredict(theObject@forest, cbind(feature_new, tr = 0),
                                    params.forestry = list("mtry" = 2),
                                    feats = c((ncol(feature_new)+1)))
    )
  } else if (correction == "bc2") {
    return(
      Rforestry::correctedPredict(theObject@forest, cbind(feature_new, tr = 1),
                                  params.forestry = list("mtry" = 2),
                                  feats = c((ncol(feature_new)+1)),
                                  nrounds = 1) -
        Rforestry::correctedPredict(theObject@forest, cbind(feature_new, tr = 0),
                                    params.forestry = list("mtry" = 2),
                                    feats = c((ncol(feature_new)+1)),
                                    nrounds = 1)
    )
  } else if (correction == "bc3") {
    return(
      Rforestry::correctedPredict(theObject@forest, cbind(feature_new, tr = 1), nrounds = 1,
                                  params.forestry = list("mtry" = 2),
                                  feats = c((ncol(feature_new)+1)),
                                  monotone = TRUE) -
        Rforestry::correctedPredict(theObject@forest, cbind(feature_new, tr = 0), nrounds = 1,
                                    params.forestry = list("mtry" = 2),
                                    feats = c((ncol(feature_new)+1)),
                                    monotone = TRUE)
    )
  } else if (correction == "bc4") {
    return(
      Rforestry::correctedPredict(theObject@forest, cbind(feature_new, tr = 1),
                                  params.forestry = list("mtry" = 2),
                                  feats = c((ncol(feature_new)+1)),
                                  simple=FALSE, num_quants = 3) -
        Rforestry::correctedPredict(theObject@forest, cbind(feature_new, tr = 0),
                                    params.forestry = list("mtry" = 2),
                                    feats = c((ncol(feature_new)+1)),
                                    simple=FALSE, num_quants = 3)
    )
  } else if (correction == "bc5") {
    return(
      Rforestry::correctedPredict(theObject@forest, cbind(feature_new, tr = 1), nrounds = 1,
                                  params.forestry = list("mtry" = 2),
                                  feats = c((ncol(feature_new)+1)),
                                  simple=FALSE, num_quants = 3) -
        Rforestry::correctedPredict(theObject@forest, cbind(feature_new, tr = 0), nrounds = 1,
                                    params.forestry = list("mtry" = 2),
                                    feats = c((ncol(feature_new)+1)),
                                    simple=TRUE, num_quants = 3)
    )
  } else if (correction == "bc6") {
    return(
      Rforestry::correctedPredict(theObject@forest, cbind(feature_new, tr = 1),
                                  params.forestry = list("mtry" = 2),
                                  feats = c((ncol(feature_new)+1)),
                                  nrounds = 1) -
        Rforestry::correctedPredict(theObject@forest, cbind(feature_new, tr = 0),
                                    params.forestry = list("mtry" = 2),
                                    feats = c((ncol(feature_new)+1)),
                                    nrounds = 1)
    )
  } else {
    return(
      predict(theObject@forest, cbind(feature_new, tr = 1)) -
        predict(theObject@forest, cbind(feature_new, tr = 0))
    )
  }
}

# RF confidence interval function ----------------------------------------------
slearner_CI <- function(theObject,
                        feature_new,
                        method = "maintain_group_ratios",
                        bootstrapVersion = "normalApprox",
                        B = 2000,
                        B_Second = B,
                        nthread = 0,
                        verbose = TRUE,
                        correction = "none") {
  ## shortcuts:
  feat <- theObject@feature_train
  tr <- theObject@tr_train
  yobs <- theObject@yobs_train
  creator <- theObject@creator
  ntrain <- length(tr)

  # Helper function to let us resample the data
  if (method == "maintain_group_ratios") {
    createbootstrappedData <- function() {

      smpl_0 <- sample((1:ntrain)[tr == 0],
                       replace = TRUE,
                       size = sum(1 - tr))
      smpl_1 <- sample((1:ntrain)[tr == 1],
                       replace = TRUE,
                       size = sum(tr))
      smpl <- sample(c(smpl_0, smpl_1))

      return(list(
        feat_b = feat[smpl, ],
        tr_b = tr[smpl],
        yobs_b = yobs[smpl],
        smpl = smpl
      ))
    }
  }

  # Run the bootstrap CI estimation #####################################

  # pred_B will contain for each simulation the prediction of each of the B
  # simulaions:
  pred_B <-
    as.data.frame(matrix(NA, nrow = nrow(feature_new), ncol = B))

  # S is needed for Efron's smooth bootstrap each column corresponse to one
  # bootstrap sample and each row corresponse to one of the smple indexes
  S <- as.data.frame(matrix(0, nrow = length(yobs), ncol = B))
  row.names(S) <- 1:length(yobs)
  colnames(S) <- 1:B


  known_warnings <- c()
  # this is needed such that bootstrapped warnings are only printed once
  for (b in 1:B) { # b= 1
    if (verbose)
      print(b)
    went_wrong <- 0
    # if that is 100 we really cannot fit it and bootstrap
    # seems to be infeasible.

    while (is.na(pred_B[1, b])) {
      if (went_wrong == 100)
        stop("one of the groups might be too small to
               do valid inference.")
      S[, b] <- rep(0, nrow(S))

      pred_B[, b] <-
        tryCatch({
          bs <- createbootstrappedData()

          counts <- table(bs$smpl)
          S[names(counts), b] <- counts


          withCallingHandlers(
            # this is needed such that bootstrapped warnings are only
            # printed once
            EstimateCorrectedCATE(
              creator(
                feat = bs$feat_b,
                tr = bs$tr_b,
                yobs = bs$yobs_b
              ),
              feature_new = feature_new,
              correction = correction
            ),
            warning = function(w) {
              if (w$message %in% known_warnings) {
                # message was already printed and can be ignored
                invokeRestart("muffleWarning")
              } else{
                # message is added to the known_warning list:
                known_warnings <<- c(known_warnings, w$message)
              }
            }
          )
        },
        error = function(e) {
          return(NA)
        })
      went_wrong <- went_wrong + 1
    }
  }

  if (bootstrapVersion == "normalApprox") {

    # Normal Approximated Bootstrap -----------------------------------------

    pred <- EstimateCate(theObject, feature_new = feature_new)
    # the the 5% and 95% CI from the bootstrapped procedure
    CI_b <- data.frame(
      X5. =  apply(pred_B, 1, function(x)
        quantile(x, c(.025))),
      X95. = apply(pred_B, 1, function(x)
        quantile(x, c(.975))),
      sd = apply(pred_B, 1, function(x) sd(x))
    )

    return(data.frame(
      pred = pred,
      X5. =  CI_b$X5.,
      X95. = CI_b$X95.
      # X5. =  pred - (CI_b$X95. - CI_b$X5.) / 2,
      # X95. = pred + (CI_b$X95. - CI_b$X5.) / 2
      # X5. =  2 * pred - CI_b$X95.,
      # X95. = 2 * pred - CI_b$X5.
    ))
  } else {
    stop("bootstrapVersion must be specified.")
  }
}
