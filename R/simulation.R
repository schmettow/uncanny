
# library(dplyr)
# library(tidyr)
# library(stringr)
#
# library(ggplot2)
# # library(GGally)
# library(mascutils)

#' category confusion process generator
#'
#' @param Part participant
#' @param Stim stimulus
#' @param stim_humLik human likeness
#' @param cond_prtime presentation time
#' @param part_info_uptake info uptake
#' @param part_reluctance reluctance
#' @param part_shock shock
#' @param part_pace pace
#' @param part_cert_face certainty
#' @param full_table full table
#' @param resp_only only response
#'
#'
#' @return process table
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import mascutils

uv_process_catconf = function(Part = 1,
                              Stim = 1,
                              stim_humLik = .5, # -1,1
                              cond_prtime = 1000,
                              part_info_uptake = 1,
                              part_reluctance = 5,
                              part_shock = 1,
                              part_pace = 25,
                              part_cert_face = 2,
                              full_table = T,
                              resp_only = F
){
  ## Subprocess: not

  n_steps    = cond_prtime %/% part_pace
  if(n_steps < 1) stop("cond_prtime must be >= part_pace")
  init_cert_face  = stim_humLik + part_cert_face
  init_p_face = plogis(init_cert_face)
  init_cat_face = rbinom(1, 1, init_p_face)

  process =
    data_frame(step = 1:n_steps,
               proc_time = step * part_pace,
               Part = 1,
               Stim = 1,
               stim_humLik,
               cond_prtime,
               part_info_uptake,
               part_reluctance,
               part_pace,
               part_cert_face,
               part_shock,
               init_cert_face,
               init_p_face,
               init_cat_face) %>%
    mutate(confl_info       = part_info_uptake/stim_humLik, # constant
           total_confl_info = cumsum(confl_info),
           p_cat_switch     = init_cat_face * plogis(total_confl_info - part_reluctance),
           # only happens when initial cat is face
           cat_switch = as.logical(rbinom(n_steps, 1, p_cat_switch)),
           emot_humLik = stim_humLik,
           emot_shock  = cat_switch * step * part_shock,
           emotion = emot_humLik - emot_shock
    )

  ## cleaning unhappened events
  if(!init_cat_face){ # not fooled
    process =
      process[1,] %>%
      mutate(process_type = "not fooled")
  }else{    # fooled
    if(any(process$cat_switch)){  # unfool
      first_switch =
        process %>%
        filter(cat_switch) %>%
        select(step) %>%
        slice(1) %>% # the first
        as.numeric()
      process =
        process[1:first_switch,] %>%
        mutate(process_type = "cat switch")
    } else {## if no cat switch, the process runs through
      process$process_type = "run-through"
    }
  }


  if(!full_table)
    process = process %>%
    select(step, init_p_face, init_cat_face,
           p_cat_switch, cat_switch, emot_shock, emotion)
  if(resp_only) process = process %>% slice(n())
  process
}


#' @rdname uv_process_catconf
#' @export

uv_process <- function (...) {
  UseMethod("uv_process")
}



#' @rdname uv_process_catconf
#' @export

uv_process.numeric <-  function(par, ...)
  {  uv_process_catconf(stim_humLik = par[1],
                cond_prtime = par[2],
                ...)}



#' @rdname uv_process_catconf
#' @export

uv_process.matrix <-  function(mat, ...)
{plyr::adply(mat, 1, uv_process.numeric, ...) %>%
    as_data_frame() %>%
    rename(ProcID = X1)
  }



#' @rdname uv_process_catconf
#' @export
#'
uv_response <-  function(mat, ...)
     {uv_process.matrix(mat, resp_only = T, ...)}


#' diagnostic plots
#'
#' @param D uncanny sim data
#'
#' @return gg
#' @export
#' @import ggplot2
#'

plot_response <-  function(D)
{    D %>%
  ggplot(aes(x = stim_humLik, y = emotion)) +
  geom_point(aes(col = as.factor(process_type))) +
  geom_smooth(se = F, span = .5) +
  facet_grid(~cond_prtime)
}

#' @rdname plot_response
#' @export
#'


plot_process_mix <-
  function(D)
{    D %>%
  ggplot(aes(x = as.factor(cond_prtime))) +
  geom_bar() +
  facet_grid(~process_type)
}

#' @rdname plot_response
#' @export
#'

plot_dashboard <-
{  function(D)
    gridExtra::grid.arrange(
      D %>% plot_response(),
      D %>% plot_process_mix())
}
