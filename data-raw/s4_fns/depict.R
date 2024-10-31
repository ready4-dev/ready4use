depict_Ready4useDyad <- function(x,
                                 x_vars_chr = character(0),
                                 y_vars_chr = character(0),
                                 z_vars_chr = character(0),
                                 arrange_1L_lgl = FALSE,
                                 arrange_args_ls = list(),
                                 as_percent_1L_lgl = FALSE,
                                 colours_chr = c("#de2d26","#fc9272"),
                                 drop_legend_1L_lgl = FALSE,
                                 drop_missing_1L_lgl = FALSE,
                                 drop_ticks_1L_lgl = FALSE,
                                 fill_single_1L_lgl = FALSE,
                                 line_1L_chr = "black",
                                 position_xx = NULL,
                                 recode_lup_r3 = ready4show::ready4show_correspondences(),
                                 style_1L_chr = get_styles(),
                                 titles_chr = character(0),
                                 type_1L_chr = c("ggsci", "manual", "viridis"),
                                 x_labels_chr = character(0),
                                 y_labels_chr = character(0),
                                 z_labels_chr = character(0),
                                 what_1L_chr =  get_journal_plot_fn("names"),
                                 ...){
  style_1L_chr <- match.arg(style_1L_chr)
  type_1L_chr <- match.arg(type_1L_chr)
  what_1L_chr <- match.arg(what_1L_chr)
  custom_args_ls <- list(...)
  call_ls <- sys.call()
  if("fill" %in% names(call_ls) ){
    if(!"fill_single_1L_lgl" %in% names(call_ls)){
      fill_single_1L_lgl <- FALSE
    }else{
      fill_single_1L_lgl <- call_ls$fill_single_1L_lgl %>% as.character() %>% as.logical()
    }
    custom_args_ls$fill <- call_ls$fill %>% as.character()
    custom_args_ls$fill_single_1L_lgl <- NULL
  }
  if("title" %in% names(call_ls) ){
    if(!"titles_chr" %in% names(call_ls)){
      titles_chr <- character(0)
    }else{
      titles_chr <- call_ls$titles_chr %>% as.character()
    }
    custom_args_ls$title <- call_ls$title %>% as.character()
    custom_args_ls$titles_chr <- NULL
  }
  if(!identical(x_vars_chr, character(0))){
    if(identical(x_labels_chr, character(0))){
      if(what_1L_chr == "qqplot"){
        x_labels_chr <- rep("Theoretical",length(x_vars_chr))
      }else{
        x_labels_chr <- x_vars_chr
      }

    }else{
      if(is.na(x_labels_chr[1])){
        x_labels_chr <- x_vars_chr %>%
          purrr::map_chr(~ready4::get_from_lup_obj(x@dictionary_r3,
                                                   match_var_nm_1L_chr = "var_nm_chr",
                                                   match_value_xx = .x,
                                                   target_var_nm_1L_chr = "var_desc_chr"))
      }
    }
  }
  if(!identical(y_vars_chr, character(0)) | what_1L_chr %in% c("qqplot")){
    if(length(y_vars_chr)==1 & length(x_vars_chr)>1){
      y_vars_chr <- rep(y_vars_chr, length(x_vars_chr))
    }
    if(identical(y_labels_chr, character(0))){
      y_labels_chr <- y_vars_chr
    }else{
      if(is.na(y_labels_chr[1])){
        if(what_1L_chr %in% c("qqplot")){
          y_labels_chr <- x_vars_chr %>%
            purrr::map_chr(~ready4::get_from_lup_obj(x@dictionary_r3,
                                                     match_var_nm_1L_chr = "var_nm_chr",
                                                     match_value_xx = .x,
                                                     target_var_nm_1L_chr = "var_desc_chr"))
        }else{
          y_labels_chr <- y_vars_chr %>%
            purrr::map_chr(~ready4::get_from_lup_obj(x@dictionary_r3,
                                                     match_var_nm_1L_chr = "var_nm_chr",
                                                     match_value_xx = .x,
                                                     target_var_nm_1L_chr = "var_desc_chr"))
        }

      }
    }
  }
  if(length(y_labels_chr)==1){
    y_labels_chr <- rep(y_labels_chr, length(x_vars_chr))
  }
  if(what_1L_chr %in% c("donutchart","pie") & identical(z_vars_chr, character(0))){
    z_labels_chr <- x_labels_chr
  }else{
    if(!identical(z_vars_chr, character(0))){
      if(length(z_vars_chr)==1 & length(x_vars_chr)>1){
        z_vars_chr <- rep(z_vars_chr, length(x_vars_chr))
      }
      if(identical(z_labels_chr, character(0))){
        z_labels_chr <- z_vars_chr
      }
    }
    if(is.na(z_labels_chr[1])){
      if(identical(z_vars_chr, character(0))){
        z_labels_chr <- x_vars_chr %>%
          purrr::map_chr(~ready4::get_from_lup_obj(x@dictionary_r3,
                                                   match_var_nm_1L_chr = "var_nm_chr",
                                                   match_value_xx = .x,
                                                   target_var_nm_1L_chr = "var_desc_chr"))
      }else{
        z_labels_chr <- z_vars_chr %>%
          purrr::map_chr(~ready4::get_from_lup_obj(x@dictionary_r3,
                                                   match_var_nm_1L_chr = "var_nm_chr",
                                                   match_value_xx = .x,
                                                   target_var_nm_1L_chr = "var_desc_chr"))
      }

    }
    if(length(z_labels_chr)==1){
      z_labels_chr <- rep(z_labels_chr, length(x_vars_chr))
    }
  }

  if(!identical(titles_chr, character(0))){
    if(is.na(titles_chr[1])){
      titles_chr <- 1:length(x_vars_chr) %>%
        purrr::map_chr(~{
          text_1L_chr <- ""
          if(!identical(y_vars_chr, character(0))){
            text_1L_chr <- paste0(ready4::get_from_lup_obj(x@dictionary_r3,
                                                           match_var_nm_1L_chr = "var_nm_chr",
                                                           match_value_xx = y_vars_chr[.x],
                                                           target_var_nm_1L_chr = "var_desc_chr"),
                                  " by ")
          }
          text_1L_chr <- paste0(text_1L_chr, ready4::get_from_lup_obj(x@dictionary_r3,
                                                                      match_var_nm_1L_chr = "var_nm_chr",
                                                                      match_value_xx = x_vars_chr[.x],
                                                                      target_var_nm_1L_chr = "var_desc_chr"))
          if(!identical(z_vars_chr, character(0))){
            text_1L_chr <- paste0(paste0(text_1L_chr, ifelse(!identical(y_vars_chr, character(0)), " and ", " by ")),
                                  ready4::get_from_lup_obj(x@dictionary_r3,
                                                           match_var_nm_1L_chr = "var_nm_chr",
                                                           match_value_xx = z_vars_chr[.x],
                                                           target_var_nm_1L_chr = "var_desc_chr"))
          }
          text_1L_chr
        })
    }
    if(length(titles_chr)==1){
      titles_chr <- rep(titles_chr, length(x_vars_chr))
    }
  }

  plot_ls <- purrr::map(1:length(x_vars_chr),
                        ~ {
                          if(identical(x_vars_chr, character(0))){
                            x_1L_chr <- character(0)
                          }else{
                            x_1L_chr <- x_vars_chr[.x]
                          }
                          if(identical(x_labels_chr, character(0))){
                            x_label_1L_chr  <- character(0)
                          }else{
                            x_label_1L_chr  <- x_labels_chr[.x]
                          }
                          if(identical(y_vars_chr, character(0))){
                            y_1L_chr <- character(0)
                          }else{
                            y_1L_chr <- y_vars_chr[.x]
                          }
                          if(identical(y_labels_chr, character(0))){
                            y_label_1L_chr  <- character(0)
                          }else{
                            y_label_1L_chr  <- y_labels_chr[.x]
                          }
                          if(identical(z_vars_chr, character(0))){
                            by_1L_chr <- character(0)
                          }else{
                            by_1L_chr <- z_vars_chr[.x]
                          }
                          if(identical(z_labels_chr, character(0))){
                            label_fill_1L_chr <- character(0)
                          }else{
                            label_fill_1L_chr <- ifelse(what_1L_chr %in% "scatterhist", NA_character_, z_labels_chr[.x])
                            if(is.na(label_fill_1L_chr)){
                              label_fill_1L_chr <- character(0)
                            }
                          }
                          if(identical(titles_chr, character(0))){
                            title_1L_chr <- character(0)
                          }else{
                            title_1L_chr <- titles_chr[.x]
                          }
                          args_ls <- append(custom_args_ls,
                                            list(as_percent_1L_lgl = as_percent_1L_lgl,
                                                 by_1L_chr = by_1L_chr,
                                                 colours_chr = colours_chr,
                                                 drop_legend_1L_lgl = drop_legend_1L_lgl,
                                                 drop_missing_1L_lgl = drop_missing_1L_lgl,
                                                 drop_ticks_1L_lgl = drop_ticks_1L_lgl,
                                                 fill_single_1L_lgl = fill_single_1L_lgl,
                                                 label_fill_1L_chr = label_fill_1L_chr,
                                                 line_1L_chr = line_1L_chr,
                                                 position_xx = position_xx,
                                                 style_1L_chr = style_1L_chr,
                                                 title_1L_chr = title_1L_chr,
                                                 type_1L_chr = type_1L_chr,
                                                 x_1L_chr = x_1L_chr,
                                                 x_label_1L_chr = x_label_1L_chr,
                                                 recode_lup_r3 = recode_lup_r3,
                                                 y_1L_chr = y_1L_chr,
                                                 y_label_1L_chr = y_label_1L_chr,
                                                 what_1L_chr = what_1L_chr))
                          rlang::exec(make_journal_plot, x@ds_tb, !!!args_ls)
                        })
  if(length(x_vars_chr) == length(unique(x_vars_chr))){
    plot_ls <- plot_ls %>% stats::setNames(x_vars_chr)
  }
  if(arrange_1L_lgl){
    if("plotlist" %in% names(arrange_args_ls)){
      plot_ls <- append(arrange_args_ls$plotlist, plot_ls)
    }
    arrange_args_ls$plotlist <- plot_ls
    plot_xx <- rlang::exec(ggpubr::ggarrange, !!!arrange_args_ls)
  }else{
    if(length(x_vars_chr) == 1){
      plot_xx <- plot_ls %>% purrr::pluck(1)
    }else{
      plot_xx <- plot_ls
    }
  }
  return(plot_xx)
}
