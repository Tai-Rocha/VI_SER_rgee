######################################################################################################
## VI SER
## rgee: um pacote em R para acessar o Google Earth Engine(GEE)
## Autor do códgigo: Tainá Rocha & Adapções de exemplos disponíveis na documentação oficial do pacote  
## Data: 26 de Maio de 2022
## Versão do R: 4.2.0
## Versão do rgee: 1.1.3
## Roteiro do script:
## 1 Instalações  -------------------------------------------------------------------------------
## 2 Funções por seção e sintaxe(ee_ x ee$) -----------------------------------------------------
## 3 Estudos de caso (3) ------------------------------------------------------------------------

######################################################################################################


#1 Instalações e sintaxe de rgee (ee_ x ee$)  -------------------------------------------------------------

# Requsitos:
# Conta no Google com Earth Engine ativado
# Python >= v3.5

install.packages("rgee") # intalação parte 1- uma vez geralmente 

library(rgee) 
# ee_instal() # instalção parte 2- uma vez geralmente 

ee_install()
# Numpy- trabalha com objetos do tipo array, ex.:matrizes multi-dimensioais);
# ee - pacote para intergagir com a API Python do GEE)
# Detalhes sobre a instalaçao disponíveis no conteúdo da apresentação

#2 Comandos por seção e e sintaxe(ee_ x ee$) ----------------------------------------------------

library(rgee) 

## ee_check()

rgee::ee_check() # de bom tom sempre inciar uma nova seção do R ou R studio e for usar o rgee

# ee_Initialize()
rgee::ee_Initialize() # Obrigatório ao inciar uma nova seção do R ou R studio e for usar o rgee

# Sintaxe(ee_ x ee$)

## ee_

# Outra maneira

#rgee::ee

#rgee::ee$Classifier$amnhMaxent(....)

#3 Estudos de caso ---------------------------------------------------------

# Análise exploratória de dados climáticos (Precipitação). Com objetivo de verificar a tendencia dos valores de precipitação ao longo do ano.


# Vizualizar essa tendencia em um graffito

library(dplyr) # manipulação dos dados (dataframe)
library(geojsonio) # Converte dados para 'GeoJSON'  
library(ggplot2) # gráficos 
library(rgee) # obtenção dos dados / estíticas 
library(raster) # manipualção de dados vetoriais e matriciais 
library(sf)   # manipulação de dados vetoriais 
library(tidyr) # manipulação dos dados (dataframe)


# Lendo o dado vetorial - área de estudo
nc_shape <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE) 

# Plotando o dado
plot(sf::st_geometry(nc_shape)) #plotando 

# Pipiline para acessar o dado de precipatção, fazer um recorte temporal, selecionar a variável de interesse (prec) e renomear.

terraclimate <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE") |> 
  ee$ImageCollection$filterDate("2001-01-01", "2002-01-01") |> 
  ee$ImageCollection$map(function(x) x$select("pr")) |>  # Selecionar bandas de precipitação
  ee$ImageCollection$toBands() |>  # converter de um objeto imagecollection para objeto image
  ee$Image$rename(sprintf("PP_%02d",1:12)) # renomeando as bandas 

# Extraindo 
ee_nc_rain <- ee_extract(x = terraclimate, y = nc_shape["NAME"], sf = FALSE)

## 
ee_nc_rain |> 
  tidyr::pivot_longer(-NAME, names_to = "month", values_to = "pr")  |> 
  dplyr::mutate(month, month=gsub("PP_", "", month)) |> 
  ggplot2::ggplot(aes(x = month, y = pr, group = NAME, color = pr)) +
  ggplot2::geom_line(alpha = 0.4) +
  ggplot2::xlab("Month") +
  ggplot2::ylab("Precipitation (mm)") +
  ggplot2::theme_minimal()

#Estudo de caso 2: obtendo estatícas por grids 

library(purrr)
library(raster)

dat <- structure(list(ID = 758432:758443, 
                      lat = c(-14.875, -14.875, -14.625, -14.625, -14.875, -14.875, -14.625, -14.625, -14.375, -14.375, -14.125, -14.125), 
                      lon = c(-42.875, -42.625, -42.625, -42.875, -42.375, -42.125, -42.125, -42.375, -42.375, -42.125, -42.125, -42.375)), 
                 class = "data.frame", row.names = c(NA, -12L))


dat_rast <- raster::rasterFromXYZ(dat[, c('lon', 'lat', 'ID')], crs = '+proj=longlat +datum=WGS84 +no_defs')
dat_poly <- raster::rasterToPolygons(dat_rast, fun=NULL, na.rm=TRUE, dissolve=FALSE)

plot(dat_poly)

# Trasformando o dado vetorial em um objeto ee$FeatureCollection  

coords <- as.data.frame(raster::geom(dat_poly))

polygonsFeatures <- coords %>% 
  split(.$object) %>% 
  purrr::map(~{  
    rgee::ee$Feature(ee$Geometry$Polygon(mapply( function(x,y){list(x,y)} ,.x$x,.x$y,SIMPLIFY=F)))
  })

polygonsCollection <- rgee::ee$FeatureCollection(unname(polygonsFeatures))

# Plotando 
rgee::Map$addLayer(polygonsCollection)


# Acessando o dado de clima (temperatura mínima e máxima)

# Selecionando alguns dias 

startDate <- rgee::ee$Date('2020-01-01');
endDate <- rgee::ee$Date('2020-01-10');

ImageCollection <- rgee::ee$ImageCollection('NASA/NEX-GDDP')$filter(ee$Filter$date(startDate, endDate))

# Pegando lista de imagens ( um por dia)

ListOfImages <- ImageCollection$toList(ImageCollection$size());


# Primeira imagem
image <- rgee::ee$Image(ListOfImages$get(8))
image$getInfo()

# Média

Means <- image$reduceRegions(collection = polygonsCollection,reducer= ee$Reducer$mean())
Means$getInfo()


#

output_mean <- rgee::ee_table_to_drive(
  collection = Means,
  fileFormat = "CSV",
  fileNamePrefix = "test"
)
output_mean$start()

ee_monitoring(output_mean)
