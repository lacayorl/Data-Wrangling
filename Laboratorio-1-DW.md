Laboratorio 1 DW
================

## Problema 1

Ha sido contratado para trabajar en una consultoría a una embotelladora
nacional. La embotelladora se encarga de distribuir su producto a
distintos clientes, utilizando diferentes equipos de transporte y
pilotos.

### Paquetes a utilizar

``` r
library(readxl)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(xlsx)
library(readr)
```

### Cargar datasets

``` r
X01_2018 <- read_excel("01-2018.xlsx")
X02_2018 <- read_excel("02-2018.xlsx")
X03_2018 <- read_excel("03-2018.xlsx")
X04_2018 <- read_excel("04-2018.xlsx")
X05_2018 <- read_excel("05-2018.xlsx")
X06_2018 <- read_excel("06-2018.xlsx")
X07_2018 <- read_excel("07-2018.xlsx")
X08_2018 <- read_excel("08-2018.xlsx")
```

    ## New names:
    ## * `` -> ...10

``` r
X09_2018 <- read_excel("09-2018.xlsx")
X10_2018 <- read_excel("10-2018.xlsx")
X11_2018 <- read_excel("11-2018.xlsx")
```

### Agregar columna de fecha para cada mes del año 2018

``` r
X01_2018$Fecha <- as.Date("01-01-2018",format="%d-%m-%Y")
X02_2018$Fecha <- as.Date("01-02-2018",format="%d-%m-%Y")
X03_2018$Fecha <- as.Date("01-03-2018",format="%d-%m-%Y")
X04_2018$Fecha <- as.Date("01-04-2018",format="%d-%m-%Y")
X05_2018$Fecha <- as.Date("01-05-2018",format="%d-%m-%Y")
X06_2018$Fecha <- as.Date("01-06-2018",format="%d-%m-%Y")
X07_2018$Fecha <- as.Date("01-07-2018",format="%d-%m-%Y")
X08_2018$Fecha <- as.Date("01-08-2018",format="%d-%m-%Y")
X09_2018$Fecha <- as.Date("01-09-2018",format="%d-%m-%Y")
X10_2018$Fecha <- as.Date("01-10-2018",format="%d-%m-%Y")
X11_2018$Fecha <- as.Date("01-11-2018",format="%d-%m-%Y")
```

### Unir los datos mensulaes en un solo dataframe

``` r
DS_Anual <- bind_rows(X01_2018,X02_2018,X03_2018,X04_2018,X05_2018,X06_2018,
                      X07_2018,X08_2018,X09_2018,X10_2018,X11_2018)
DS_Anual <- DS_Anual[,-(10:11)]
head(DS_Anual)
```

    ## # A tibble: 6 x 9
    ##   COD_VIAJE CLIENTE   UBICACION CANTIDAD PILOTO      Q CREDITO UNIDAD Fecha     
    ##       <dbl> <chr>         <dbl>    <dbl> <chr>   <dbl>   <dbl> <chr>  <date>    
    ## 1  10000001 EL PINCH~     76002     1200 Fernan~ 300        30 Camio~ 2018-01-01
    ## 2  10000002 TAQUERIA~     76002     1433 Hector~ 358.       90 Camio~ 2018-01-01
    ## 3  10000003 TIENDA L~     76002     1857 Pedro ~ 464.       60 Camio~ 2018-01-01
    ## 4  10000004 TAQUERIA~     76002      339 Angel ~  84.8      30 Panel  2018-01-01
    ## 5  10000005 CHICHARR~     76001     1644 Juan F~ 411        30 Camio~ 2018-01-01
    ## 6  10000006 UBIQUO L~     76001     1827 Luis J~ 457.       30 Camio~ 2018-01-01

### Grabar el dataframe como un archivo csv

``` r
write.csv(DS_Anual,"DS_ANUAL.csv",row.names = FALSE)
```

## Problema 2

Utilizando la función lapply, encuentre la moda de cada vector de una
lista de por lo menos 3 vectores.

### Crear una lisa con vectores

``` r
vector1 <- c(1,3,5,7,9,3,5,3,9)
vector2 <- c(2,4,6,8,6,4,8,6,6)
vector3 <- c(1,2,3,6,9,3,7,6,3)

lista <- list(vector1,vector2,vector3)
lista
```

    ## [[1]]
    ## [1] 1 3 5 7 9 3 5 3 9
    ## 
    ## [[2]]
    ## [1] 2 4 6 8 6 4 8 6 6
    ## 
    ## [[3]]
    ## [1] 1 2 3 6 9 3 7 6 3

### Utilizar lapply para obtener la moda

``` r
moda <- function(x) {
   return(as.numeric(names(which.max(table(x)))))
}

lapply(lista,moda)
```

    ## [[1]]
    ## [1] 3
    ## 
    ## [[2]]
    ## [1] 6
    ## 
    ## [[3]]
    ## [1] 3

## Problema 3

Cargar el dataset de parque vehicular de enero 2019

``` r
parque_vehicular <- read_delim(file = "INE_PARQUE_VEHICULAR_080219.txt")
```

    ## New names:
    ## * `` -> ...11

    ## Warning: One or more parsing issues, see `problems()` for details

    ## Rows: 2435294 Columns: 11

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: "|"
    ## chr (8): MES, NOMBRE_DEPARTAMENTO, NOMBRE_MUNICIPIO, MODELO_VEHICULO, LINEA_...
    ## dbl (2): ANIO_ALZA, CANTIDAD
    ## lgl (1): ...11

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
head(parque_vehicular)
```

    ## # A tibble: 6 x 11
    ##   ANIO_ALZA MES   NOMBRE_DEPARTAMENTO NOMBRE_MUNICIPIO MODELO_VEHICULO
    ##       <dbl> <chr> <chr>               <chr>            <chr>          
    ## 1      2007 05    HUEHUETENANGO       "HUEHUETENANGO"  2007           
    ## 2      2007 05    EL PROGRESO         "EL JICARO"      2007           
    ## 3      2007 05    SAN MARCOS          "OCOS"           2007           
    ## 4      2007 05    ESCUINTLA           "SAN JOS\xc9"    2006           
    ## 5      2007 05    JUTIAPA             "MOYUTA"         2007           
    ## 6      2007 05    GUATEMALA           "FRAIJANES"      1997           
    ## # ... with 6 more variables: LINEA_VEHICULO <chr>, TIPO_VEHICULO <chr>,
    ## #   USO_VEHICULO <chr>, MARCA_VEHICULO <chr>, CANTIDAD <dbl>, ...11 <lgl>
