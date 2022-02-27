library(pacman)
pacman::p_load("tibble")

make_feature_matrix <- function(config, to_tibble = TRUE) {
  
  n <- config$sample_size
  p <- config$n_vars
  
  out <- matrix(
    data = get_dist(config)(n * p),
    nrow = n,
    ncol = p
  )
  
  if (to_tibble) {
    out <- tibble::as_tibble(out, .name_repair = make_names)
  }
  
  out
}

get_dist <- function(config) {
  
  if (config$feature_spec$dist == "beta") {
    function(n) {
      rbeta(n = n,
            shape1 = config$feature_spec$shape1,
            shape2 = config$feature_spec$shape2,
            ncp = config$feature_spec$ncp)
    }
  } else if (config$feature_spec$dist == "binom") {
    function(n) {
      rbinom(n = n,
             size = config$feature_spec$size,
             prob = config$feature_spec$prob)
    }
  } else if (config$feature_spec$dist == "cauchy") {
    function(n) {
      rcauchy(n = n,
              location = config$feature_spec$location,
              scale = config$feature_spec$scale)
    }
  } else if (config$feature_spec$dist == "chisq") {
    function(n) {
      rchisq(n = n,
             df = config$feature_spec$df,
             ncp = config$feature_spec$ncp)
    }
  } else if (config$feature_spec$dist == "exp") {
    function(n) {
      rexp(n = n,
           rate = config$feature_spec$rate)
    }
  } else if (config$feature_spec$dist == "f") {
    function(n) {
      rf(n = n,
         df1 = config$feature_spec$df1,
         df2 = config$feature_spec$df2,
         ncp = config$feature_spec$ncp)
    }
  } else if (config$feature_spec$dist == "gamma") {
    function(n) {
      rgamma(n = n,
             shape = config$feature_spec$shape,
             rate = config$feature_spec$rate)
    }
  } else if (config$feature_spec$dist == "geom") {
    function(n) {
      rgeom(n = n,
            prob = config$feature_spec$prob)
    }
  } else if (config$feature_spec$dist == "hyper") {
    function(nn) {
      rhyper(nn = nn,
             m = config$feature_spec$m,
             n = config$feature_spec$n,
             k = config$feature_spec$k)
    }
  } else if (config$feature_spec$dist == "logis") {
    function(n) {
      rlogis(n = n,
             location = config$feature_spec$location,
             scale = config$feature_spec$scale)
    }
  } else if (config$feature_spec$dist == "lnorm") {
    function(n) {
      rlnorm(n = n,
             meanlog = config$feature_spec$meanlog,
             sdlog = config$feature_spec$sdlog)
    }
  } else if (config$feature_spec$dist == "nbinom") {
    function(n) {
      rnbinom(n = n,
              size = config$feature_spec$size,
              prob = config$feature_spec$prob,
              mu = config$feature_spec$mu)
    }
  } else if (config$feature_spec$dist == "norm") {
    function(n) {
      rnorm(n = n,
            mean = config$feature_spec$mean,
            sd = config$feature_spec$sd)
    }
  } else if (config$feature_spec$dist == "pois") {
    function(n) {
      rpois(n = n,
            lambda = config$feature_spec$lambda)
    }
  } else if (config$feature_spec$dist == "t") {
    function(n) {
      rt(n = n,
         df = config$feature_spec$df,
         ncp = config$feature_spec$ncp)
    }
  } else if (config$feature_spec$dist == "unif") {
    function(n) {
      runif(n = n,
            min = config$feature_spec$min,
            max = config$feature_spec$max)
    }
  } else if (config$feature_spec$dist == "weibull") {
    function(n) {
      rweibull(n = n,
               shape = config$feature_spec$shape,
               scale = config$feature_spec$scale)
    }
  } else if (config$feature_spec$dist == "wilcox") {
    function(nn) {
      rwilcox(nn = nn,
              m = config$feature_spec$m,
              n = config$feature_spec$n)
    }
  } else if (config$feature_spec$dist == "signrank") {
    function(nn) {
      rsignrank(nn = nn,
                n = config$feature_spec$n)
    }
  } else {
    stop("config$feature_spec$dist must be set to one of the following:\n",
         "beta, binom, cauchy, chisq, exp, f, gamma, geom, hyper, logis, ",
         "lnorm, nbinom, norm, pois, t, unif, weibull, wilcox, signrank")
  }
  
}

make_names <- function(vec) {
  paste("X", seq_along(vec), sep = "_")
}
