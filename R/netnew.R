#' Generate NET_NEW variable
#' 
#' @description Incorporated from [ICPI Partner Progress Report](https://github.com/achafetz/PartnerProgress), `genPPR::netnew()`
#' 
#' @param df dataset to use/bind net new to
#' @param fy current fiscal year, eg 2018
#' @param qtr current quarter, eg 1
#' 
#' @export
#' 
#' @importFrom dplyr %>%
#' 
#' @examples
#' \dontrun{
#' df_mer <- netnew(df_mer, 2018, 1) }
#' 
netnew <- function(df){
  
  #filter df, keeping just TX_CURR, replacing NA with 0 (for calculation), and renameing TX_NET_NEW
    df_netnew <- df %>%
      dplyr::mutate_at(vars(dplyr::starts_with("fy2")), ~ ifelse(is.na(.),0,.)) %>% 
      dplyr::filter(indicator=="TX_CURR") %>%
      dplyr::mutate(indicator = "TX_NET_NEW") 
    
  #create a net new variable, pd - L.pd (prior to FY17 pd - 2L.pd); target = target - L.fyq4
    df_netnew <- df_netnew %>%
      dplyr::mutate(fy2015q4_nn =  fy2015q4 - fy2015q2,
                    fy2016q2_nn =  fy2016q2 - fy2015q4,
                    fy2016q4_nn =  fy2016q4 - fy2016q2,
                    fy2017q1_nn =  fy2017q1 - fy2016q4,
                    fy2017q2_nn =  fy2017q2 - fy2017q1,
                    fy2017q3_nn =  fy2017q3 - fy2017q2,
                    fy2017q4_nn =  fy2017q4 - fy2017q3,
                    fy2017_targets_nn = fy2017_targets - fy2016q4,
                    fy2018q1_nn =  fy2018q1 - fy2017q4)
    
  #replace current values (TX_CURR) with the NET_NEW values
    df_netnew <- df_netnew %>%
      dplyr::mutate(fy2015q2 = 0, 
                    fy2015q3 = 0,
                    fy2015q4 =  fy2015q4_nn,
                    fy2015apr = 0,
                    fy2016_targets = 0,
                    fy2016q1 =  0,
                    fy2016q2 =  fy2016q2_nn,
                    fy2016q3 =  0,
                    fy2016q4 =  fy2016q4_nn,
                    fy2016apr = fy2016q2_nn + fy2016q4_nn, 
                    fy2017q1 =  fy2017q1_nn,
                    fy2017q2 =  fy2017q2_nn,
                    fy2017q3 =  fy2017q3_nn,
                    fy2017q4 =  fy2017q4_nn,
                    fy2017apr = fy2017q1_nn + fy2017q2_nn + fy2017q3_nn + fy2017q4_nn,
                    fy2017_targets = fy2017_targets_nn,
                    fy2018q1 =  fy2018q1_nn) %>%
  #remove original calculation
      dplyr::select(-dplyr::ends_with("_nn"))
    
  #append TX_NET_NEW onto main dataframe
    df <- dplyr::bind_rows(df, df_netnew)
}