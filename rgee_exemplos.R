######################################################################################################
## VI SER
## rgee: um pacote em R para acessar o Google Earth Engine(GEE)
## Autor do códgigo: Tainá Rocha & Adapções de exemplos disponíveis na documentação oficial do pacote  
## Data: 26 de Maio de 2022
## Versão do R
## Versão do rgee:
## Roteiro do script:
## 1 Instalações --------------------------------------------------------------------------------
## 2 Comandos por seção -------------------------------------------------------------------------
## 3 Estudos de caso ----------------------------------------------------------------------------

######################################################################################################


#1 Instalações e sintaxe de rgee  -------------------------------------------------------------

# Requsitos:
#1- Conta no Google com Earth Engine ativado
#2- Python >= v3.5

install.packages("rgee") # uma vez

library(rgee) #sempre que incicar a seção ou

# ee_ 
# ee_instal()
# Ou rgee:: 

## Aproveitando a deixa dos rgee::  ee$

rgee::ee$Classifier$amnhMaxent(....)

# Ou seja: rgee vai funcionar de acordo com duas sintaxes ee_ ou ee$


# Logo após a instalação, é preciso de fazer algumas instalções

library(rgee)

ee_install()

# ou pacote::

rgee::ee_install() # apenas uma  vez. Fazer logo após a instalação do rgee


# rgee é depende de Phyton, por isso precisamos instalar os seguinte os pacotes phyton:
# `Numpy`- trabalha com objetos do tipo array, ex.:matrizes multi-dimensioais);
# `ee` - pacote para intergagir com a API e GEE em Python )
## Esta intalação pode ser feita de três maneiras após instalar o rgee (ver detalhes refências da apesentação). Geralmente é preciso ser feito uma vez só).
## Detalhes sobre a instalaçao ver na seção de referências da apresentação

#2 Comandos por seção ----------------------------------------------------

library(rgee) 

rgee::ee_check()

rgee::ee_Initialize()

#3 Estudos de caso ---------------------------------------------------------

# Análise exploratória de dados climáticos (Precipitação). Com objetivo de verificar a tendencia dos valores de precipitação ao longo do ano.


# Vizualizar essa tendencia em um graffito

library(dplyr) # manipulação dos dados (dataframe)
library(geojsonio) # Converte dados para 'GeoJSON'  
library(ggplot2) # gráficos 
library(rgee) # obtenção dos dados / esatíticas 
library(raster) # manipualção de dados vetoriais e matriciais 
library(sf)   # manipulação de dados vetoriais 
library(tidyr) # manipulação dos dados (dataframe)


# Lendo o dado vetorial 
nc_shape <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE) 

# Plotando o dado
plot(sf::st_geometry(nc_shape)) #plotando 

# Pipiline para acessar o dado, fazer um recorte temporal, selecionar a variável de interesse (prec) e renomear.

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

#Estudo de caso 2: médias 

dat <- structure(list(ID = 758432:758443, 
                      lat = c(-14.875, -14.875, -14.625, -14.625, -14.875, -14.875, -14.625, -14.625, -14.375, -14.375, -14.125, -14.125), 
                      lon = c(-42.875, -42.625, -42.625, -42.875, -42.375, -42.125, -42.125, -42.375, -42.375, -42.125, -42.125, -42.375)), 
                 class = "data.frame", row.names = c(NA, -12L))


dat_rast <- raster::rasterFromXYZ(dat[, c('lon', 'lat', 'ID')], crs = '+proj=longlat +datum=WGS84 +no_defs')
dat_poly <- raster::rasterToPolygons(dat_rast, fun=NULL, na.rm=TRUE, dissolve=FALSE)

# Trasformando o dado vetorial em um objeto ee$FeatureCollection  

coords <- as.data.frame(raster::geom(dat_poly))

polygonsFeatures <- coords %>% 
  split(.$object) %>% 
  purrr::map(~{  
    ee$Feature(ee$Geometry$Polygon(mapply( function(x,y){list(x,y)} ,.x$x,.x$y,SIMPLIFY=F)))
  })

polygonsCollection = ee$FeatureCollection(unname(polygonsFeatures))
Map$addLayer(polygonsCollection)


# Selecionando algns dias 
startDate = ee$Date('2020-01-01');
endDate = ee$Date('2020-01-10');


# Acessando o dado de clima (temperatura mínima e máxima)

ImageCollection = ee$ImageCollection('NASA/NEX-GDDP')$filter(ee$Filter$date(startDate, endDate))#$filterBounds(polygonsCollection)

# Pegando lista de imagens ( um por dia)

ListOfImages = ImageCollection$toList(ImageCollection$size());

# Primeira imagem
image <- ee$Image(ListOfImages$get(0))

# Add the mean of each band as new properties of each polygon

Means = image$reduceRegions(collection = polygonsCollection,reducer= ee$Reducer$mean())
Means$getInfo()


## The polygons data can be downloaded on Google Drive:

task_vector <- ee_table_to_drive(
  collection = Means,
  fileFormat = "CSV",
  fileNamePrefix = "test"
)
task_vector$start()
ee_monitoring(task_vector)