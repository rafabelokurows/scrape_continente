library(rvest)
library(jsonlite)
library(dplyr)

codigos = read.csv("data/codigos_scrape_continente.csv")
codigos = codigos %>% slice(1:100)

#ean = "3600541575790"
# Specify the URL of the webpage
#url = "https://www.continente.pt/pesquisa/?q=3600523951949&start=0&srule=Continente&pmin=0.01"
scrape_continente = function(ean){
  url <- paste0("https://www.continente.pt/pesquisa/?q=",ean,"&start=0&srule=Continente&pmin=0.01")

  # Send a GET request to the webpage and read the HTML content
  page <- read_html(url)
  result = page %>%
    html_nodes(".search-keyword-title") %>% html_children() %>%
    html_text()
  if (as.numeric(result[2])>0){


    # Extract the product name
    nome_produto = page %>%
      html_nodes("div.row.product-grid.no-gutters.gtm-list")  %>%
      html_nodes(".product")%>%
      html_element(".col-pdp-link") %>%
      html_children() %>% html_text() %>% gsub("\\n","",.)

    link_produto= page %>%
      html_nodes("div.row.product-grid.no-gutters.gtm-list")  %>%
      html_nodes(".product")%>%
      html_element(".col-pdp-link") %>%
      html_children() %>%
      html_attr("href")

    embalagem = page %>%
      html_nodes("div.row.product-grid.no-gutters.gtm-list")  %>%
      html_nodes(".product")%>%
      html_element(".ct-pdp-details") %>% html_node(".pwc-tile--quantity") %>%
      html_text()
    if(length(embalagem)>1){embalagem = embalagem[2]}
    desconto_pct = page %>%
      html_nodes("div.row.product-grid.no-gutters.gtm-list")  %>%
      html_nodes(".product")%>%
      #html_children()%>%
      #html_children()%>%
      #html_children()%>%
      #html_children() %>%
      html_nodes(".ct-product-tile-badge-value--pvpr") %>% html_text() %>%
      gsub("\\n","",.)

    if(identical(desconto_pct,character(0))){desconto_pct = NA_character_}
    if(length(desconto_pct)>1){desconto_pct = desconto_pct[2]}

    pvp_recomendado = page %>%
      html_nodes("div.row.product-grid.no-gutters.gtm-list")  %>%
      html_nodes(".product") %>%
      html_nodes(".pwc-discount-amount") %>% html_text() %>%
      gsub("PVP Recomendado: ","",.)
    if(identical(pvp_recomendado,character(0))){pvp_recomendado = NA_character_}

    preco_por_volume = page %>%
      html_nodes("div.row.product-grid.no-gutters.gtm-list")  %>%
      html_nodes(".product") %>%
      html_nodes(".ct-price-value") %>% html_text()%>%
      gsub("\\n","",.)
    if(identical(preco_por_volume,character(0))){preco_por_volume = NA_character_}
    if(length(preco_por_volume)>1){preco_por_volume = preco_por_volume[2]}

    promocao = page %>%
      html_nodes("div.row.product-grid.no-gutters.gtm-list")  %>%
      html_nodes(".product") %>%
      html_nodes(".dual-badge-message-text")%>% html_text()
    if(identical(promocao,character(0))){promocao = NA_character_}

    ivazero = page %>%
      html_nodes("div.row.product-grid.no-gutters.gtm-list")  %>%
      html_nodes(".product") %>%
      html_nodes(".ct-product-tile-badge--iva-zero")%>% html_text()

    if(identical(ivazero,character(0))){
      ivazero = F}  else {ivazero = T}

    link_imagem = page %>%
      html_nodes("div.row.product-grid.no-gutters.gtm-list")  %>%
      html_nodes(".product") %>%
      html_element(".ct-tile-image") %>% html_attr("data-src")
    if(identical(link_imagem,character(0))){
      link_imagem = NA_character_}

    produtos = page %>%
      html_nodes("div.row.product-grid.no-gutters.gtm-list")  %>%
      html_nodes(".product")%>%
      html_children() %>% html_attr("data-product-tile-impression")
    if(length(produtos)>1){produtos = produtos[2]}
    if(length(embalagem)>1){ embalagem = embalagem[!is.na(embalagem)]}
    df_produto = produtos%>%
      fromJSON() %>% as.data.frame() %>%
      mutate(nome = nome_produto[1],
             link_produto=link_produto[1],
             embalagem = embalagem,
             ean = ean,
             desconto=desconto_pct,
             pvp_recomendado = pvp_recomendado[1],
             pvp_recomendado = pvp_recomendado[2],
             preco_por_volume =  preco_por_volume,
             promocao = promocao,
             ivazero = ivazero,
             link_imagem = link_imagem[1]) %>%
      select(ean,brand,nome,ivazero,price,desconto,pvp_recomendado,promocao,preco_por_volume,category,embalagem,link_produto,link_imagem,id,variant,channel)
    return(df_produto)
  } else {
    df_null = data.frame(ean=ean,nome=NA,price=NA,
                         desconto=NA,pvp_recomendado=NA,promocao=NA,
                         preco_por_volume=NA,category=NA,embalagem=NA,link_produto=NA,link_imagem=NA,id=NA,variant=NA,channel=NA)
    print("Não foi encontrado produto")
    return(df_null)
  }
}
#scrape_continente("5601312881308")
#TODO:
#ADICIONAR ATRIBUTOS DE PROMOÇÃO: % DE PROMOÇÃO E PVP Recomendado
#ean = "5099873003220"
#codigos2=sample_n(codigos,20)5099873003220
df_produtos=data.frame()
for (i in 1:nrow(codigos)){
  print(i)
  if (i %% 10 == 0) {
    print(i)
    print(paste0(round((i/nrow(codigos))*100,3),"%"))
    notfound=nrow(df_produtos[is.na(df_produtos$price),])
    print(paste0("Não encontrados: ",notfound, " - ",round((notfound/i)*100,3),"%"))
  }
  #print(codigos$BARCODE[i])
  aux = scrape_continente(codigos$BARCODE[i])
  df_produtos = bind_rows(df_produtos,aux)
}
#teste = scrape_continente(codigos$BARCODE)
write.csv(df_produtos,paste0("data/",format(Sys.Date(), "%Y%m%d"),"_scrape_continente.csv"),row.names=F)
save(df_produtos,file="data/produtos.rdata")
