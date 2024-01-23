
#' @export
get_pole <- function(dimension, pole){
    if (dimension %in% names(main_dimensions)){
        main_dimensions[[dimension]][[pole]]
    }else if (dimension %in% names(comm_dimensions)){
        comm_dimensions[[dimension]][[pole]]
    }else{
        stop('Dimension not found')
    }
}

main_dimensions <- list("rile" = list("L" = c("per103", "per105", "per106", "per107", "per202", "per403", "per404", "per406", "per412", "per413", "per504", "per506", "per701"),
                                      "R" = c("per104", "per201", "per203", "per305", "per401", "per402", "per407", "per414", "per505", "per601", "per603", "per605", "per606")),
                        "rile_econ" = list("L" = c("per107", "per403", "per404", "per405", "per406", "per412", "per413", "per504", "per506", "per701"), # per405 not part of riginal rile
                                           "R" = c("per401", "per402", "per407", "per414", "per505", "per601", "per603", "per605", "per702")), #per702 not part of original rile
                        "eu" = list("L" = "per108", # more EU
                                    "R" = "per110"), # less EU
                        "intmarket" = list("L" = c("per406", "per409", "per403"),
                                           "R" = c("per401", "per407")),
                        "freemarket" = list("L" = c("per403", "per405", "per416", "per406", "per409", "per412"),
                                            "R" = c("per401", "per402", "per407"))
)



comm_dimensions <- list(
    "AFCO" = main_dimensions[['eu']],
    "AFET" = list("L" = c("per107"),
                  "R" = c("per109")),
    "AGRI_v5" = list("L" = c("per703_1"),
                     "R" = c("per703_2")),
    "AGRI" = list("L" = c("per703"),
                  "R" = c("per407")),
    "BUDG" = main_dimensions[['eu']],
    "CONT" = main_dimensions[['eu']],
    "CULT" = list("L" = c("per502", "per506"),
                  "R" = c("per507")),
    "DEVE" = list("L" = c("per107"),
                  "R" = c("per109")),
    "ECON" = main_dimensions[['freemarket']], # maybe change to freemarket
    "EMPL" = list("L" = c("per403", "per405", "per406", "per412", "per504", "per701"), # @TODO: MAYBE SWITCH FOR RILE_ECON
                  "R"= c("per401", "per407", "per702", "per505", "per402", "per414")),
    "ENVI" = list("L" = c("per501", "per416"),
                  "R" = c("per410")),
    "IMCO" = main_dimensions[['eu']],
    "INTA" = main_dimensions[['rile_econ']], # rather than intmarket?
    "ITRE" = list("L" = c(main_dimensions[['eu']][['L']], "per411"),
                  "R" = c(main_dimensions[['eu']][['R']])),
    "INST" = main_dimensions[['eu']],
    "JURI" = main_dimensions[['rile_econ']], #list("L" = c(main_dimensions[['eu']][['L']], "per107"),
    # "R" = c(main_dimensions[['eu']][['R']], "per109")),
    "LIBE" = list("L" = c("per201", "per503", "per705", "per602", "per607"),
                  "R" = c("per601", "per605", "per608")),
    "PECH" = list("L" = c("per703"),
                  "R" = c("per407")),
    "REGI" = list("L" = c("per301"),
                  "R" = c("per302")),
    "TRAN" = list("L" = c(main_dimensions[['eu']][['L']], "per411"), # rather than intmarket?
                  "R" = c(main_dimensions[['eu']][['R']])),
    "y_agri1" = list("L" = c("per703"),
                     "R" = c("per407")),
    "y_envi1" = list("L" = c("per501", "per416"),
                     "R" = c("per410")),
    "y_envi2" = list("L" = c("per416"),
                     "R" = c("per410")),
    "y_intm1" = list("L" = c("per108"),
                     "R" = c("per110")),
    "y_intm2" = list("L" = c("per407", "per401", "per403"),
                     "R" = c("per406")),
    "y_trans1" = list("L" = c("per108", "per411"),
                      "R" = c("per110"))
)

comm_dimensions[['ENER']] = comm_dimensions[['ITRE']]
comm_dimensions[['RELA']] = comm_dimensions[['INTA']]


#' @export
get_eurovoc_dimensions <- function(...){
    eurovoc_issue_dimensions[...]

}

eurovoc_issue_dimensions = list('eurovoc_04_politics' = list('L' = c(main_dimensions[['eu']][['L']]),
                                        'R' = c(main_dimensions[['eu']][['R']])),
                                'eurovoc_08_international' = list('L' = c('per107'),
                                                             'R' = c('per109')),
                                'eurovoc_10_european' = list('L' = c(main_dimensions[['eu']][['L']]),
                                                             'R' = c(main_dimensions[['eu']][['R']])),
                                'eurovoc_12_law' = list('L' = c('per503', 'per602'),
                                                             'R' = c('per605', 'per601')),
                                'eurovoc_16_economics' = list('L' = c('per409'),
                                                             'R' = c('per414')),
                                'eurovoc_20_trade' = list('L' = c('per403'),
                                                             'R' = c('per402')),
                                'eurovoc_24_finance' = list('L' = c('per403'),
                                                             'R' = c('per402')),
                                'eurovoc_28_social' = list('L' = c('per503', 'per504'),
                                                             'R' = c('per505')),
                                'eurovoc_32_education' = list('L' = c('per506'),
                                                             'R' = c('per507')),
                                'eurovoc_36_science' = list('L' = c('per411', 'per108'),
                                                             'R' = c('per110')),
                                'eurovoc_40_business' = list('L' = c('per403'),
                                                             'R' = c('per402')),
                                'eurovoc_44_employment' = list('L' = c("per403", "per405", "per406", "per412", "per701"),
                                                             'R' = c("per401", "per407", "per702", "per402", "per414")),
                                'eurovoc_48_transport' = list('L' = c('per108', 'per411'),
                                                             'R' = c('per110')),
                                'eurovoc_52_environment' = list('L' = c('per501', 'per416'), # Lowe 2012
                                                             'R' = c('per410')),
                                'eurovoc_56_agriculture' = list('L' = c('per501'),
                                                             'R' = c('per407')),
                                'eurovoc_60_agrifoodstuffs' = list('L' = c('per501'),
                                                             'R' = c('per407')),
                                'eurovoc_64_production' = list('L' = c('per108', 'per411'),
                                                             'R' = c('per110')),
                                'eurovoc_66_energy' = list('L' = c('per501', 'per416', '403'),
                                                             'R' = c('per410', 'per401')),
                                'eurovoc_68_industry' = list('L' = c(c("per403", "per405", "per406", "per412", "per701")),
                                                             'R' = c(c("per401", "per407", "per702", "per402", "per414"))),
                                'eurovoc_72_geography' = list('L' = c('per107'),
                                                             'R' = c('per109')),
                                'eurovoc_76_international' = list('L' = c('per107'),
                                                              'R' = c('per109'))
                                )
