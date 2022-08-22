library(rebus)
library(magrittr)



options(echo = TRUE)

tryCatch(expr = {

  #setwd("C:/Marius Tita/OLD/marius/backup 27.11.2007/my doc/Directia administr. riscuri/Analize Risc/Web Plafon R/Risk applications/LitiigiiDatabase")
  #setwd("/build_zone/")

  logs <- readRDS("R/logs/logs.rds")

  dosare_noi_start_date <- as.Date("2022-08-19")#Sys.Date()-2


  dosare_noi_stop_date <- Sys.Date()

  db <- config::get("database", file = "inst/golem-config.yml")

  today = Sys.Date()


  my_connection <- DBI::dbConnect(RMySQL::MySQL(), password = db$password, username = db$username,
                                  dbname = db$dbname, host = db$host)

  # Create regex for numar dosar correct identification
  regex_dosar <- number_range(lo = 1,hi = 500000,allow_leading_zeroes = F) %R% "/" %R%
    number_range(lo = 1,hi = 10000,allow_leading_zeroes = F)  %R%
    "/" %R% number_range(lo = 2005, hi = 2030,allow_leading_zeroes = F) %R% zero_or_more("*")

  litigii_sold  <-  my_connection %>% dplyr::tbl("stare_litigiu") %>% dplyr::filter(today, stare_litigiu == "activ") %>%
    dplyr::select(id_litigiu) %>%
    dplyr::inner_join( by = "id_litigiu",
                       my_connection %>% dplyr::tbl("actualizare_litigiu") %>%
                         dplyr::group_by(id_litigiu) %>%
                         dplyr::summarise(ultima_data_modificare_litigiu =
                                            max(data_modificare, na.rm = T)) %>%
                         dplyr::left_join(y = my_connection %>% dplyr::tbl("actualizare_litigiu") %>%
                                            dplyr::select(id_litigiu, data_modificare, "numar_litigiu"),
                                          by = c('id_litigiu' = 'id_litigiu',
                                                 'ultima_data_modificare_litigiu' = 'data_modificare') ) ) %>%
    dplyr::collect()

  # I filter out below litigii cu numar incorect, acestea pot furniza foarte multe rezultate la interogarea portal just

  litigii_neactualizate <- litigii_sold %>% dplyr::slice(stringr::str_which( string = litigii_sold$numar_litigiu,
                                                                             pattern = regex_dosar,negate = T))

  litigii_sold <- litigii_sold %>% dplyr::slice(stringr::str_which( string = litigii_sold$numar_litigiu, pattern = regex_dosar))



  saveRDS(object = litigii_neactualizate,file = "R/reactivedata/litigii_neactualizate.rds")

  # I need below dataframe in order to filter sentinte queried on portal just

  DBI::dbDisconnect(my_connection)


  # API_interogare_portal_just ----------------------------------------------


  # Functia de mai jos genereaza un xml curat,prelucrabil care contine toate info unui dosar.
  # Below function does not have parameters
  create_body <- function(numar_dosar) {
    headerfields = c(
      Accept = "text/xml",
      Accept = "multipart/*",
      'Content-Type' = "text/xml; charset=utf-8",
      'SOAPAction' = "portalquery.just.ro/CautareDosare")

    body_dosar <- paste0('<?xml version="1.0" encoding="utf-8"?>
    <soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
      <soap:Body>
        <CautareDosare xmlns="portalquery.just.ro">
          <numarDosar>', numar_dosar,'</numarDosar>
        </CautareDosare>
      </soap:Body>
    </soap:Envelope>')

    reader = RCurl::basicTextGatherer()
    reader$reset()
    RCurl::curlPerform(
      url = "http://portalquery.just.ro/Query.asmx",
      httpheader = headerfields,
      postfields = body_dosar,
      writefunction = reader$update)
    reader$value() %>% stringr::str_replace(pattern = "<soap:Envelope xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"><soap:Body>",
                                            replacement = "") %>% stringr::str_replace(pattern = "</soap:Body></soap:Envelope>", replacement ="") %>%
      stringr::str_remove_all(pattern = 'xsi:nil=\"true\"') %>%
      stringr::str_replace(pattern = '<CautareDosareResponse xmlns=\"portalquery.just.ro\">',replacement = "") %>%
      stringr::str_replace(pattern = '</CautareDosareResponse>',replacement = "") %>% xml2::read_xml()
  }

  # Create the function that will interogate lista numar dosar with safely.
  #  Note, it does not work to combine the below 2 commands
  safe_extract <- purrr::safely(create_body)
  # Main command to interogate portal just
  lista_xml_total_dosare_juridic <- purrr::map(litigii_sold$numar_litigiu, ~safe_extract(.x))

  # Extract the result part of the above nested list
  lista_xml_total_dosare_juridic_results <- purrr::map(lista_xml_total_dosare_juridic, "result")

  dosare_lista <- lista_xml_total_dosare_juridic_results %>% lapply(xml2::xml_children) %>%
    lapply(X = .,FUN = function(x) {
      data.frame(stringsAsFactors = F,
                 numar_litigiu = xml2::xml_child(x = x,search = '/numar') %>% xml2::xml_text(),

                 data_modificare_dosar = xml2::xml_child(x = x,search = '/dataModificare') %>%
                   xml2::xml_text() %>% lubridate::as_datetime(),

                 categorie_caz = xml2::xml_child(x =x, search = "/categorieCazNume") %>% xml2::xml_text(),

                 stadiu_procesual = xml2::xml_child(x=x, search =  "/stadiuProcesualNume") %>% xml2::xml_text(),


                 obiect = xml2::xml_child(x=x,search = "/obiect") %>% xml2::xml_text(),

                 instante = xml2::xml_child(x=x,search = "/institutie") %>% xml2::xml_text()

                 ,parti = xml2::xml_child(x=x,search = "/parti") %>% xml2::xml_text()
      )   })

  # I produce a data frame form above list dosare_lista
  lista_dosare <- dosare_lista %>% purrr::map_dfr(.x = .,~data.frame(.x, stringsAsFactors = FALSE))

  # I extract index elements from the list dosare_lista which have data in them (nrow>0)
  index_empty_elements <- which(dosare_lista %>% purrr::map_dbl(.f = ~nrow(.x)==0)==1)

  # Final processing after counting for empty observations
  if (length(index_empty_elements) > 0) {
    lista_xml_total_dosare_juridic_results <- lista_xml_total_dosare_juridic_results[-index_empty_elements]
    dosare_lista <- dosare_lista[-index_empty_elements]
    litigii_sold <- litigii_sold[-index_empty_elements,]
    lista_dosare <- lista_dosare[-index_empty_elements,]

  }

  # Extract calitatea partilor
  parti_calitate_dosare_modificate <- data.frame( stringsAsFactors = FALSE,
                    calitate_parti = lista_xml_total_dosare_juridic_results %>%
                      purrr::map(~ xml2::xml_find_all(x = ., xpath = "//DosarParte/calitateParte") %>%
                                                       xml2::xml_text() ) %>% unlist(),
                            toate_partile = lista_xml_total_dosare_juridic_results %>%
                                                    purrr::map(
                                                      ~ xml2::xml_find_all(x = ., xpath = "//DosarParte/nume") %>%
                                                        xml2::xml_text()
                                                    ) %>% unlist()
  ) %>%
    dplyr::mutate(
      concat_parti_calitate = paste0(toate_partile, calitate_parti) %>%
        stringr::str_remove_all(string = ., pattern = "\\(") %>%
        stringr::str_remove_all(string = ., pattern = "\\)")
    )


  # Produce dosare odificate care contine acum partile si calitatea lor
  dosare_modificate <- lista_dosare %>% dplyr::mutate(
    parti_adverse = purrr::map(.x = lista_dosare$parti,
                               .f = ~ parti_calitate_dosare_modificate %>% dplyr::slice(
                                 stringr::str_which(string = .x, pattern = concat_parti_calitate)) %>%
                                 dplyr::filter( stringr::str_detect( string = toate_partile,
                                            pattern = "FONDUL NA|F\\.N\\.",  negate = TRUE)  ) %>%
                                 dplyr::pull(toate_partile) %>% unique() %>% paste0(collapse = " ; ") ) %>% unlist() ) %>%
    dplyr::mutate(calitate_fngcimm = purrr::map(.x = lista_dosare$parti,
                                                .f = ~ parti_calitate_dosare_modificate %>% dplyr::slice(
                                                  stringr::str_which(string = .x, pattern = concat_parti_calitate)  ) %>%
                                                  dplyr::filter(stringr::str_detect(string = toate_partile,
                                                                                    pattern = "FONDUL NA|F\\.N\\.", negate = FALSE)) %>%
                                                  dplyr::pull(calitate_parti) %>% unique() ) )

  # Un dosar nou poate avea mai multe calitati(nu stiu bine de ce) asa o preiau doar pe ultima .x[1]
  dosare_modificate <- dosare_modificate %>%
    dplyr::mutate(calitate_fngcimm = purrr::map(.x = calitate_fngcimm,~.x[1]) %>% unlist())

  # Eliminate duplicates from dosare_modificate
  dosare_modificate <- dosare_modificate[!duplicated(dosare_modificate[, c("numar_litigiu","data_modificare_dosar")]),]

  # Extract si denumirile FNGCIMM, am nevoie de ele pentru identificarea dosarelor noi
  lista_parti_fngcimm <- purrr::map(.x = lista_xml_total_dosare_juridic_results,
                                    ~ xml2::xml_find_all(xpath = "//DosarParte/nume", x = .x) %>% xml2::xml_text()) %>%
    purrr::map( ~ .x[stringr::str_which(string = .,pattern = "FONDUL NA|F\\.N\\.", negate = FALSE)])

  # Extract of denumiri FNGCIMM from lista dosare noi
  denumiri_fngcimm <- lista_parti_fngcimm %>% unlist() %>% unique()

  # Update R/dosare_noi/lista_nume_fngcimm.rds with new data from denumiri_fngcimm if exists
  lista_nume_fngcimm <-   readRDS("R/reactivedata/dosare_noi/lista_nume_fngcimm.rds")

  if (length( which( !denumiri_fngcimm %in% lista_nume_fngcimm ) ) >0) {
    lista_nume_fngcimm <- c(denumiri_fngcimm[which(!denumiri_fngcimm %in% lista_nume_fngcimm)], lista_nume_fngcimm)
    saveRDS(object = lista_nume_fngcimm,file = "R/reactivedata/dosare_noi/lista_nume_fngcimm.rds")

  }

  # Final assembly of dosare modificate with info from litigii sold and other clenings

  dosare_modificate <- dosare_modificate %>%  dplyr::left_join(litigii_sold %>% dplyr::select(numar_litigiu, id_litigiu),
                                                               by = "numar_litigiu") %>%
    # Cleaning step. It may introduce new numar dosar if fed with the wrong numar_dosar within litigii_sold$Nr_dosar_instanta
    dplyr::filter(!is.na(id_litigiu)) %>%
    # below removes diacritics
    dplyr::mutate_at(.vars = c("instante","categorie_caz","obiect","stadiu_procesual","calitate_fngcimm","parti_adverse"),
                     .funs = ~stringi::stri_trans_general(str = .,id = "latin-ASCII"))

  query_insert_actualizare_litigii <- paste0("INSERT INTO actualizare_litigiu(id_litigiu, numar_litigiu,data_modificare, instanta,Categorie_caz,Obiect,Stadiu_procesual,Calitate_FNG,Parti) VALUES (",
                                             paste0(dosare_modificate$id_litigiu,",","'",
                                                    dosare_modificate$numar_litigiu, "'",",","'",
                                                    dosare_modificate$data_modificare_dosar,"'", ",", "'",
                                                    dosare_modificate$instante,"'",",", "'",
                                                    dosare_modificate$categorie_caz, "'", ",","'",
                                                    dosare_modificate$obiect,"'", ",", "'",
                                                    dosare_modificate$stadiu_procesual,"'",",","'",
                                                    dosare_modificate$calitate_fngcimm,"'", ",","'",
                                                    dosare_modificate$parti_adverse,"'",
                                                    collapse = "),("), ")",
                                             " ON DUPLICATE KEY UPDATE numar_litigiu=VALUES(numar_litigiu), instanta=VALUES(instanta),Categorie_caz=VALUES(Categorie_caz),Obiect=VALUES(Obiect), Stadiu_procesual = VALUES(Stadiu_procesual),Calitate_FNG=VALUES(Calitate_FNG), Parti=VALUES(Parti)")

  # Final insertion of dosare_modificate in database
  if (nrow(dosare_modificate) > 0 ) {
    my_connection <- DBI::dbConnect(RMySQL::MySQL(), password = db$password, username = db$username,
                                    dbname = db$dbname, host = db$host)
    DBI::dbSendQuery(conn = my_connection,statement = query_insert_actualizare_litigii)

    DBI::dbDisconnect(conn = my_connection)  }


  logs <- dplyr::bind_rows(data.frame(timestamp = Sys.time(), Message = "Successfully updated dosare modificate",
                                      Category="Dosare_modificate",stringsAsFactors = FALSE, row.names = NULL),logs)

  # Error handling of the above code
}, error = function(cnd) {

  logs <- dplyr::bind_rows(data.frame(timestamp = Sys.time(), Message = conditionMessage(cnd),
                                      Category="Dosare_modificate",stringsAsFactors = FALSE, row.names = NULL),logs)
})

# Cai_atac ----------------------------------------------------------------

tryCatch(expr = {

  cai_atac <-     lista_xml_total_dosare_juridic_results %>%
    purrr::set_names(x = .,nm = litigii_sold$numar_litigiu) %>%
    lapply(X = .,FUN = function(x) {
      data.frame(stringsAsFactors = FALSE,
                 data_declarare_atac = xml2::xml_find_all(x = x, xpath = "//Dosar/caiAtac/DosarCaleAtac/dataDeclarare") %>%
                   xml2::xml_text() %>% as.Date(),
                 parte_atac = xml2::xml_find_all(x = x, xpath = "//Dosar/caiAtac/DosarCaleAtac/parteDeclaratoare") %>%
                   xml2::xml_text() %>% stringi::stri_trans_general(id = "latin-ASCII"),
                 tip_cale_atac =  xml2::xml_find_all(x=x, xpath = "//Dosar/caiAtac/DosarCaleAtac/tipCaleAtac") %>%
                   xml2::xml_text()
      ) %>% unique()  } ) %>%

    purrr::map_dfr(.x = .,.f = ~data.frame(.x, stringsAsFactors = FALSE),.id = "numar_litigiu") %>%

    dplyr::left_join(litigii_sold %>% dplyr::select(numar_litigiu, id_litigiu),
                     by = "numar_litigiu")


  cai_atac <- cai_atac[!duplicated(cai_atac[, c("id_litigiu", "data_declarare_atac")]),]

  query_insert_cai_atac <- paste0("INSERT INTO cai_atac(id_litigiu, data_declarare_atac, tip_cale_atac,parte_atac) VALUES (",
                                  paste0(cai_atac$id_litigiu,",", "'",
                                         as.character(cai_atac$data_declarare_atac),"'",",","'",
                                         cai_atac$tip_cale_atac,"'",",","'",
                                         cai_atac$parte_atac,"'",
                                         collapse    ="),("),")",
                                  " ON DUPLICATE KEY UPDATE tip_cale_atac = VALUES(tip_cale_atac), parte_atac = VALUES(parte_atac)")
  # Final insert of cai atac inside database
  if (nrow(cai_atac) > 0 ){
    my_connection <- DBI::dbConnect(RMySQL::MySQL(), password = db$password, username = db$username,
                                    dbname = db$dbname, host = db$host)
    DBI::dbSendQuery(conn = my_connection,statement = query_insert_cai_atac)

    DBI::dbDisconnect(conn = my_connection)  }

  logs <- dplyr::bind_rows(data.frame(timestamp = Sys.time(), Message = "Successfully updated cai de atac",
                                      Category="Cai atac",stringsAsFactors = FALSE, row.names = NULL),logs)
  # Error handling of the cai atac code
}, error = function(cnd) {

  logs <- dplyr::bind_rows(data.frame(timestamp = Sys.time(), Message = conditionMessage(cnd),
                                      Category="Cai_atac", stringsAsFactors = FALSE, row.names = NULL),logs)
} )

# SENTINTE ----------------------------------------------------------------

tryCatch(expr = {

  sentinte_duplicate_excluse <- readRDS("R/reactivedata/sentinte_dublate_eliminate.rds")

  sentinte_actualizate <- lista_xml_total_dosare_juridic_results %>%
    purrr::set_names(x = .,nm = litigii_sold$numar_litigiu) %>%
    lapply(X = .,FUN = function(x) {
      data.frame(stringsAsFactors = FALSE,
                 date_sedinte = xml2::xml_find_all(x=x, xpath = "//Dosar/sedinte/DosarSedinta/data") %>%
                   xml2::xml_text() %>% as.Date(),
                 tip_solutie = xml2::xml_find_all(x=x, xpath = "//Dosar/sedinte/DosarSedinta/solutie") %>%
                   xml2::xml_text() %>% stringi::stri_trans_general(str = .,id = "latin-ASCII"),
                 solutie = xml2::xml_find_all(x=x, xpath = "//Dosar/sedinte/DosarSedinta/solutieSumar") %>%
                   xml2::xml_text() %>% stringi::stri_trans_general(str = .,id = "latin-ASCII")
      ) } ) %>%
    purrr::map_dfr(.x = .,.f = ~data.frame(.x, stringsAsFactors = FALSE),.id = "numar_litigiu") %>%
    dplyr::left_join(litigii_sold %>% dplyr::select(numar_litigiu, id_litigiu),
                     by = "numar_litigiu" ) %>%
    # Filter out sentinte duplicate procesate
    dplyr::mutate( identifier = paste0(id_litigiu, date_sedinte,tip_solutie,solutie) ) %>%
    dplyr::filter(! identifier %in% sentinte_duplicate_excluse$identifier)
  # dplyr::left_join(sentinte_max_date, by = "id_litigiu") %>%
  #             dplyr::mutate(dplyr::across(.cols = max_sentinte_date,.fns = ~as.Date(.x))) %>%
  # dplyr::filter( is.na(max_sentinte_date) | max_sentinte_date < date_sedinte) %>%
  #   dplyr::mutate(solutie_cumulata = ifelse(solutie_cumulata=="", solutie, ifelse(solutie=="",
  #        solutie_cumulata,   paste(solutie, solutie_cumulata,sep = " ; ")))) %>% dplyr::select(-max_sentinte_date)

  litigii_duplicate <- sentinte_actualizate %>% dplyr::slice( anyDuplicated(sentinte_actualizate[,
                                                                                                 c("id_litigiu", "date_sedinte")]) ) %>% dplyr::select(id_litigiu, date_sedinte) %>%
    dplyr::mutate(identifier = paste0(id_litigiu, date_sedinte))

  sentinte_duplicate <- sentinte_actualizate %>% dplyr::mutate(identifier = paste0(id_litigiu, date_sedinte)) %>%
    dplyr::filter(identifier %in% litigii_duplicate$identifier)

  saveRDS(object = sentinte_duplicate %>% dplyr::select(-identifier), file = "R/reactivedata/sentinte_duplicate.rds")

  sentinte_final <- sentinte_actualizate %>% dplyr::mutate(identifier = paste0(id_litigiu, date_sedinte)) %>%
    dplyr::filter( !identifier %in% litigii_duplicate$identifier) %>% dplyr::select(-identifier)


  query_sentinte <- paste0("INSERT INTO sentinte(id_litigiu, data_sentinta,tip_solutie,solutie,solutie_cumulata) VALUES (",
                           paste0(sentinte_final$id_litigiu,",","'",
                                  as.character(sentinte_final$date_sedinte),"'", "," ,"'",
                                  sentinte_final$tip_solutie, "'",",","'",
                                  sentinte_final$solutie, "'",",", "'",
                                  sentinte_final$solutie_cumulata,"'",
                                  collapse = "),("), ")",
                           " ON DUPLICATE KEY UPDATE tip_solutie = VALUES(tip_solutie), solutie = VALUES(tip_solutie),solutie = VALUES(solutie)")

  # Insert in database of sentinte actualizate
  if (nrow(sentinte_final) > 0 ){
    my_connection <- DBI::dbConnect(RMySQL::MySQL(), password = db$password, username = db$username,
                                    dbname = db$dbname, host = db$host)
    DBI::dbSendQuery(conn = my_connection,statement = query_sentinte)

    DBI::dbDisconnect(conn = my_connection)  }
  logs <- dplyr::bind_rows(data.frame(timestamp = Sys.time(), Message = "Successfully updated sentinte",
                                      Category="Sentinte", stringsAsFactors = FALSE, row.names = NULL),logs)
  # Error handling of sentinte_actualizate code
}, error = function(e) {

  logs <- dplyr::bind_rows(data.frame(timestamp = Sys.time(), Message = conditionMessage(e),
                                      Category="Sentinte", stringsAsFactors = FALSE, row.names = NULL),logs)
} )

#### DOSARE NOI

tryCatch(expr = {
  # Curl function to interogate dosare noi. Attention: it contains parameters, startdate and stopdate.

  interogare_parte <- function(numeparte, startdate = paste0(dosare_noi_start_date,"T00:00:00"),
                               stopdate = paste0(dosare_noi_stop_date,"T00:00:00")) {
    headerfields = c(
      Accept = "text/xml",
      Accept = "multipart/*",
      'Content-Type' = "text/xml; charset=utf-8",
      'SOAPAction' = "portalquery.just.ro/CautareDosare")

    body_interogare <- paste0('<?xml version="1.0" encoding="utf-8"?>
                       <soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
                       <soap:Body>
                       <CautareDosare xmlns="portalquery.just.ro">
                       <numeParte>',numeparte,'</numeParte>
                       <dataStart>',startdate,'</dataStart>
                       <dataStop>',stopdate,'</dataStop>
                       </CautareDosare>
                       </soap:Body>
                       </soap:Envelope>')
    reader = RCurl::basicTextGatherer()
    reader$reset()
    RCurl::curlPerform(
      url = "http://portalquery.just.ro/Query.asmx",
      httpheader = headerfields,
      postfields = body_interogare,
      writefunction = reader$update)
    reader$value() %>% stringr::str_replace(pattern = "<soap:Envelope xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"><soap:Body>",
                                            replacement = "") %>% stringr::str_replace(pattern = "</soap:Body></soap:Envelope>", replacement ="") %>%
      stringr::str_remove_all(pattern = 'xsi:nil=\"true\"') %>%
      stringr::str_replace(pattern = '<CautareDosareResponse xmlns=\"portalquery.just.ro\">',replacement = "") %>%
      stringr::str_replace(pattern = '</CautareDosareResponse>',replacement = "") %>% xml2::read_xml()
  }

  #ATENTIE: linia principala de interogare
  lista_interogare_dosar_nou <- purrr::map(lista_nume_fngcimm, ~interogare_parte(.x))


  lista_interogare_dosar_nou <- lista_interogare_dosar_nou %>% lapply(function(x) {if (length(xml2::xml_children(x))>0) {x}}) %>%
    purrr::discard(.x = .,.p = ~is.null(.x))

  # I need below function to be inserted inside cut function used for dosare noi which does not accept a vector of 1:1
  # and needs either a minimum of 2 or a vector of 1:minimum(2)
  breaks_function <- function(x) {
    if (length(x)==1) { return(2) }
    else {return (1:length(x)) }
  }

  # Main extract of dosare noi

  dosare_noi <- lista_interogare_dosar_nou %>% lapply(xml2::xml_children) %>%
    purrr::map(.x = ., function(x) split(x = x, f = cut(seq_along(x), breaks = breaks_function(x=x),labels = F))) %>%
    purrr::map(~unlist(x = .,recursive = F)) %>% purrr::flatten(.x = .) %>%
    lapply(X = .,FUN = function(x) {
      data.frame(stringsAsFactors = F,
                 numar_litigiu = xml2::xml_child(x = x,search = "numar") %>% xml2::xml_text(),

                 data_litigiu_nou = xml2::xml_child(x = x,search = "data") %>%
                   xml2::xml_text() %>% lubridate::as_datetime(),

                 categorie_caz_dosar_nou = xml2::xml_child(x = x,search = "categorieCazNume") %>%
                   xml2::xml_text() %>% stringi::stri_trans_general(str = .,id = "latin-ASCII"),

                 stadiu_procesual_dosar_nou = xml2::xml_child(x = x,search =  "stadiuProcesualNume") %>%
                   xml2::xml_text() %>% stringi::stri_trans_general(str = .,id = "latin-ASCII"),

                 obiect_dosar_nou = xml2::xml_child(x = x,search = "obiect") %>%
                   xml2::xml_text() %>% stringi::stri_trans_general(str = .,id = "latin-ASCII"),

                 instante_dosar_nou = xml2::xml_child(x = x,search = "institutie") %>%
                   xml2::xml_text() %>% stringi::stri_trans_general(str = .,id = "latin-ASCII"),

                 parti = xml2::xml_child(x = x,search = "parti") %>% xml2::xml_text() ) }) %>%
    purrr::map_dfr(.x = .,~data.frame(.x, stringsAsFactors = FALSE)) %>%
    dplyr::mutate(dplyr::across(.cols = data_litigiu_nou,~as.POSIXlt(.x,tz = "UTC"))) %>%
    dplyr::filter(!is.na(numar_litigiu))


  # Eliminate duplicated form dosare noi
  dosare_noi <- dosare_noi[!duplicated(dosare_noi[, c("numar_litigiu", "data_litigiu_nou")]),]


  # I generate a dataframe with parti data
  parti_calitate_dosare_noi <- data.frame(stringsAsFactors = FALSE,   calitate_parti = lista_interogare_dosar_nou %>%
                                            purrr::map(~xml2::xml_find_all(x = .,xpath="//DosarParte/calitateParte") %>%
                                                         xml2::xml_text()) %>% unlist(),
                                          toate_partile = lista_interogare_dosar_nou %>%
                                            purrr::map(~xml2::xml_find_all(x = .,xpath="//DosarParte/nume") %>%
                                                         xml2::xml_text()) %>% unlist() ) %>%
    dplyr::mutate(concat_parti_calitate = paste0(toate_partile,calitate_parti) %>%
                    stringr::str_remove_all(string = .,pattern = "\\(") %>%
                    stringr::str_remove_all(string = .,pattern = "\\)"))

  # Insert parti adverse to dosare noi
  dosare_noi_parti_adverse <- dosare_noi %>% dplyr::mutate(   parti_adverse = purrr::map(
    .x = parti,  .f = ~ parti_calitate_dosare_noi %>% dplyr::slice(
      stringr::str_which(string = .x, pattern = concat_parti_calitate) ) %>%
      dplyr::filter(stringr::str_detect(  string = toate_partile,
                                          pattern = "FONDUL NA|F\\.N\\.",        negate = T    )) %>%
      dplyr::pull(toate_partile) %>% unique() %>% paste0(collapse = " ; ") ) %>% unlist ) %>%
    dplyr::mutate(
      calitate_fngcimm = purrr::map(
        .x = parti,
        .f = ~ parti_calitate_dosare_noi %>% dplyr::slice(
          stringr::str_which(string = .x, pattern = concat_parti_calitate)
        ) %>%
          dplyr::filter(
            stringr::str_detect(
              string = toate_partile,
              pattern = "FONDUL NA|F\\.N\\.",
              negate = F
            )
          ) %>% dplyr::pull(calitate_parti) %>% unique()
      )
    )


  # Un dosar nou poate avea mai multe calitati(nu stiu bine de ce) asa o preiau doar pe ultima .x[1]
  dosare_noi_parti_adverse <- dosare_noi_parti_adverse %>%
    dplyr::mutate(calitate_fngcimm = purrr::map(.x = calitate_fngcimm,~.x[1]) %>% unlist())

  # Below I extract dosare which contain * - these are old dosare with numar nou and do not contain /a
  dosare_numar_nou <-  dosare_noi_parti_adverse %>%
    dplyr::slice( stringr::str_which(string = numar_litigiu,pattern="\\*| \\/a") )

  # Below I get the indexes in litigii_sold$numar_litigiu for which dosare_numar_nou refers to

  index_dosare_numar_nou <- purrr::map(.x = # I first have to remove* in order for the formula to work
                                         litigii_sold$numar_litigiu %>% stringr::str_remove_all(pattern = "\\*"),
                                       .f = ~stringr::str_which(string = dosare_numar_nou$numar_litigiu,pattern = .x)) %>%
    purrr::keep(.x = .,.p = ~length(.)>0) %>% unlist()

  if (is.null(index_dosare_numar_nou)) { index_dosare_numar_nou <- 0 }

  dosare_numar_nou <- dosare_numar_nou %>% dplyr::slice( index_dosare_numar_nou )

  dosare_numar_nou_existent <- readRDS("R/reactivedata/dosare_noi/dosare_numar_nou.rds")

  dosare_numar_nou_final <- dosare_numar_nou %>% dplyr::filter(!numar_litigiu %in% dosare_numar_nou_existent$numar_litigiu)

  saveRDS(object = dplyr::bind_rows(dosare_numar_nou_final,dosare_numar_nou_existent),
          file = "R/reactivedata/dosare_noi/dosare_numar_nou.rds")

  dosare_noi_existente <- readRDS('R/reactivedata/dosare_noi/dosare_noi.rds')

  dosare_excluse <- readRDS('R/reactivedata/dosare_noi/dosare_excluse.rds')

  dosare_noi_final <- dplyr::bind_rows( dosare_noi_existente %>% dplyr::filter(!numar_litigiu %in% dosare_excluse$numar_litigiu),
                                        dosare_noi_parti_adverse %>% dplyr::filter(!numar_litigiu %in% dosare_noi_existente$numar_litigiu) %>%
                                          dplyr::filter(!numar_litigiu %in% dosare_excluse$numar_litigiu)) %>%
    dplyr::filter(!numar_litigiu %in% litigii_sold$numar_litigiu) %>%
    dplyr::filter(!numar_litigiu %in% dosare_numar_nou$numar_litigiu)


  dosare_noi_final <- dosare_noi_final[!duplicated(dosare_noi_final$numar_litigiu),]

  saveRDS(object = dosare_noi_final %>% dplyr::select(-parti), "R/reactivedata/dosare_noi/dosare_noi.rds")

  logs <- dplyr::bind_rows(data.frame(timestamp = Sys.time(), Message = paste0("Successfully retrieved dosare noi from ",
                                                                               dosare_noi_start_date, " to ", dosare_noi_stop_date),
                                      Category="Dosare noi", stringsAsFactors = FALSE, row.names = NULL),logs)
}, error = function(cnd) {
  logs <- dplyr::bind_rows(data.frame(timestamp = Sys.time(), Message = conditionMessage(cnd),
                                      Category="Dosare_noi", stringsAsFactors = FALSE, row.names = NULL),logs)

} )

saveRDS(object = logs,file = "R/logs/logs.rds")



