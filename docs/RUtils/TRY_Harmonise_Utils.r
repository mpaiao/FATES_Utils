#==========================================================================================
#   Additional TRY utilities for harmonising data
#
#   Author: Marcos Longo 
#   Email:  m l o n g o a@t l b l d.o.t g o v
#
#   Date: 09-Apr-2022
#
#------------------------------------------------------------------------------------------





#---~---
#     This function converts longitude/latitude into country names.
#
# Input variables:
# lon           - vector with longitudes (degrees, negative means west)
# lat           - vector with latitudes (degrees, negative means south)
# geo_adm1_path - path where the GeoBoundaries administrative 1 GeoJSON files are located.
# simplified    - which large countries (i.e., Australia, Brazil, Canada, China, Russia
#                 and United States) should use the simplified boundaries for sub-national
#                 administrative regions? This can be either a logical variable (TRUE 
#                 meaning all, FALSE meaning none) or the list of countries using the 
#                 ISO-3 country codes (i.e., AUS, BRA, CAN, CHN, RUS and/or USA, case
#                 insensitive).
#---~---
TRY_LonLatToGeoInfo <<- function(lon,lat,geo_adm1_path,simplified=TRUE){


   #---~---
   #   Standardise the simplified names.
   #---~---
   simple_lookup = c( AUS = "Australia"
                    , BRA = "Brazil"
                    , CAN = "Canada"
                    , CHN = "China"
                    , RUS = "Russia"
                    , USA = "United States"
                    )#end simple_lookup
   if (is.logical(simplified)){
      #---~---
      #   Set countries
      #---~---
      if (simplified){
         simplified = simple_lookup
      }else{
         simplified = character(0)
      }#end if (simplified)
      #---~---
   }else{
      #---~---
      #   Make sure the names are valid.
      #---~---
      simplified = stringr::str_to_upper(simplified)
      simplified = simple_lookup[simplified %in% names(simple_lookup)]
      #---~---
   }#end if (is.logical(simplified))
   #---~---



   #---~---
   #   Make sure the input path and the required files exist.
   #---~---
   if (! dir.exists(geo_adm1_path)){
      stop(paste0(" Path geo_adm1_path=\"",geo_adm1_path,"\" is not valid."))
   }else{
      #---~---
      #   Set file names
      #---~---
      Adm1GeoJSON = 
         c( Australia    = file.path(geo_adm1_path,"geoBoundaries-AUS-ADM1.geojson")
          , Brazil       = file.path(geo_adm1_path,"geoBoundaries-BRA-ADM1.geojson")
          , Canada       = file.path(geo_adm1_path,"geoBoundaries-CAN-ADM1.geojson")
          , China        = file.path(geo_adm1_path,"geoBoundaries-CHN-ADM1.geojson")
          , Russia       = file.path(geo_adm1_path,"geoBoundaries-RUS-ADM1.geojson")
          , UnitedStates = file.path(geo_adm1_path,"geoBoundaries-USA-ADM1.geojson")
          )#end c
      #---~---


      #---~---
      #   Replace files with simplified counterparts when needed.
      #---~---
      is_simple              = names(Adm1GeoJSON) %in% simplified
      Adm1GeoJSON[is_simple] = gsub ( pattern     = "ADM1\\.geojson"
                                    , replacement = "ADM1_simplified.geojson"
                                    , x           = Adm1GeoJSON[is_simple]
                                    )#end gsub
      #---~---



      #---~---
      #   Check file names.
      #---~---
      is_fine = file.exists(Adm1GeoJSON)
      if (any (! is_fine)){
         cat0("---~---"                                        )
         cat0("   FATAL ERROR!"                                )
         cat0("---~---"                                        )
         cat0(" In path geo_adm1_path = \"",geo_adm1_path,"\"" )
         cat0(" The following adm1 files are missing:"         )
         for (n in which(! is_fine)){
            cat0(" ",names(Adm1GeoJSON)[n],"  -  ",basename(Adm1GeoJSON)[n])
         }#end for (n in which(! is_fine))
         cat0("---~---"                                        )
         stop(" Download missing files from https://www.geoboundaries.org")
      }#end if
      #---~---
   }#end if (! dir.exists(geo_adm1_path))
   #---~---



   #---~---
   #   Retrieve spatial points for all countries and the sub-national boundaries for the 
   # countries larger than or equal to Australia.
   #---~---
   CountriesSP    = rworldmap::getMap(resolution="low")
   AustraliaSP    = geojsonsf::geojson_sf(Adm1GeoJSON["Australia"   ])
   BrazilSP       = geojsonsf::geojson_sf(Adm1GeoJSON["Brazil"      ])
   CanadaSP       = geojsonsf::geojson_sf(Adm1GeoJSON["Canada"      ])
   ChinaSP        = geojsonsf::geojson_sf(Adm1GeoJSON["China"       ])
   RussiaSP       = geojsonsf::geojson_sf(Adm1GeoJSON["Russia"      ])
   UnitedStatesSP = geojsonsf::geojson_sf(Adm1GeoJSON["UnitedStates"])
   AustraliaSP    = sf::st_cast(AustraliaSP   ,"MULTIPOLYGON")
   BrazilSP       = sf::st_cast(BrazilSP      ,"MULTIPOLYGON")
   CanadaSP       = sf::st_cast(CanadaSP      ,"MULTIPOLYGON")
   ChinaSP        = sf::st_cast(ChinaSP       ,"MULTIPOLYGON")
   RussiaSP       = sf::st_cast(RussiaSP      ,"MULTIPOLYGON")
   UnitedStatesSP = sf::st_cast(UnitedStatesSP,"MULTIPOLYGON")
   #---~---


   #---~---
   #   For larger countries, remove the country prefixes.
   #---~---
   AustraliaSP$shapeISO    = gsub(pattern="^AU-",replacement="",x=AustraliaSP$shapeISO   )
   BrazilSP$shapeISO       = gsub(pattern="^BR-",replacement="",x=BrazilSP$shapeISO      )
   RussiaSP$shapeISO       = gsub(pattern="^RU-",replacement="",x=RussiaSP$shapeISO      )
   UnitedStatesSP$shapeISO = gsub(pattern="^US-",replacement="",x=UnitedStatesSP$shapeISO)
   #---~---



   #---~---
   #   Canada and China: standardise the provinces as they are not in the shapeISO
   #---~---
   CanadaSP$shapeISO[CanadaSP$shapeName %in%"Alberta"                  ] = "AB"
   CanadaSP$shapeISO[CanadaSP$shapeName %in%"British Columbia"         ] = "BC"
   CanadaSP$shapeISO[CanadaSP$shapeName %in%"Manitoba"                 ] = "MB"
   CanadaSP$shapeISO[CanadaSP$shapeName %in%"New Brunswick"            ] = "NB"
   CanadaSP$shapeISO[CanadaSP$shapeName %in%"Newfoundland and Labrador"] = "NL"
   CanadaSP$shapeISO[CanadaSP$shapeName %in%"Northwest Territories"    ] = "NT"
   CanadaSP$shapeISO[CanadaSP$shapeName %in%"Nova Scotia"              ] = "NS"
   CanadaSP$shapeISO[CanadaSP$shapeName %in%"Nunavut"                  ] = "NU"
   CanadaSP$shapeISO[CanadaSP$shapeName %in%"Ontario"                  ] = "ON"
   CanadaSP$shapeISO[CanadaSP$shapeName %in%"Prince Edward Island"     ] = "PE"
   CanadaSP$shapeISO[CanadaSP$shapeName %in%"Quebec"                   ] = "QC"
   CanadaSP$shapeISO[CanadaSP$shapeName %in%"Saskatchewan"             ] = "SK"
   CanadaSP$shapeISO[CanadaSP$shapeName %in%"Yukon"                    ] = "YT"
   ChinaSP$shapeISO [ChinaSP$shapeName  %in% "Anhui"                   ] = "AH"
   ChinaSP$shapeISO [ChinaSP$shapeName  %in% "Beijing"                 ] = "BJ"
   ChinaSP$shapeISO [ChinaSP$shapeName  %in% "Chongqing"               ] = "CQ"
   ChinaSP$shapeISO [ChinaSP$shapeName  %in% "Fujian"                  ] = "FJ"
   ChinaSP$shapeISO [ChinaSP$shapeName  %in% "Gansu"                   ] = "GS"
   ChinaSP$shapeISO [ChinaSP$shapeName  %in% "Guangdong"               ] = "GD"
   ChinaSP$shapeISO [ChinaSP$shapeName  %in% "Guangxi"                 ] = "GX"
   ChinaSP$shapeISO [ChinaSP$shapeName  %in% "Guizhou"                 ] = "GZ"
   ChinaSP$shapeISO [ChinaSP$shapeName  %in% "Hainan"                  ] = "HI"
   ChinaSP$shapeISO [ChinaSP$shapeName  %in% "Hebei"                   ] = "HE"
   ChinaSP$shapeISO [ChinaSP$shapeName  %in% "Heilongjiang"            ] = "HL"
   ChinaSP$shapeISO [ChinaSP$shapeName  %in% "Henan"                   ] = "HA"
   ChinaSP$shapeISO [ChinaSP$shapeName  %in% "Hubei"                   ] = "HB"
   ChinaSP$shapeISO [ChinaSP$shapeName  %in% "Hunan"                   ] = "HN"
   ChinaSP$shapeISO [ChinaSP$shapeName  %in% "Inner Mongolia"          ] = "NM"
   ChinaSP$shapeISO [ChinaSP$shapeName  %in% "Jiangsu"                 ] = "JS"
   ChinaSP$shapeISO [ChinaSP$shapeName  %in% "Jiangxi"                 ] = "JX"
   ChinaSP$shapeISO [ChinaSP$shapeName  %in% "Jilin"                   ] = "JL"
   ChinaSP$shapeISO [ChinaSP$shapeName  %in% "Liaoning"                ] = "LN"
   ChinaSP$shapeISO [ChinaSP$shapeName  %in% "Ningxia Hui"             ] = "NX"
   ChinaSP$shapeISO [ChinaSP$shapeName  %in% "Qinghai"                 ] = "QH"
   ChinaSP$shapeISO [ChinaSP$shapeName  %in% "Shaanxi"                 ] = "SN"
   ChinaSP$shapeISO [ChinaSP$shapeName  %in% "Shandong"                ] = "SD"
   ChinaSP$shapeISO [ChinaSP$shapeName  %in% "Shanghai"                ] = "SH"
   ChinaSP$shapeISO [ChinaSP$shapeName  %in% "Shanxi"                  ] = "SX"
   ChinaSP$shapeISO [ChinaSP$shapeName  %in% "Sichuan"                 ] = "SC"
   ChinaSP$shapeISO [ChinaSP$shapeName  %in% "Tianjin"                 ] = "TJ"
   ChinaSP$shapeISO [ChinaSP$shapeName  %in% "Tibet Autonomous Region" ] = "XZ"
   ChinaSP$shapeISO [ChinaSP$shapeName  %in% "Xinjiang"                ] = "XJ"
   ChinaSP$shapeISO [ChinaSP$shapeName  %in% "Yunnan"                  ] = "YN"
   ChinaSP$shapeISO [ChinaSP$shapeName  %in% "Zhejiang"                ] = "ZJ"
   #---~---



   #---~---
   #   Retrieve spatial points.
   #---~---
   PointsSP    = vect(x=data.frame(x=lon,y=lat),geom=c("x","y"),crs="EPSG:4326")
   PointsSF    = st_as_sf(PointsSP)
   CountriesSF = st_as_sf(CountriesSP)
   dummy       = sf_use_s2(FALSE)
   GeoInfo     = as_tibble(st_join(PointsSF,CountriesSF))
   dummy       = sf_use_s2(TRUE)
   #---~---


   #---~---
   #   For some reason French Guiana doesn't belong to any continent.
   #---~---
   GeoInfo$Stern[GeoInfo$NAME %in% "French Guiana"] = "South America"
   #---~---


   #---~---
   #   Get indices of the polygons object containing each point.
   #---~---
   Country   = str_to_title(as.character(GeoInfo$NAME))
   Continent = str_to_title(as.character(GeoInfo$Stern))
   #---~---


   #---~---
   #   Standardise countries
   #---~---
   Country[Country %in% "Bosnia and Herz."         ] = "Bosnia and Herzegovina"
   Country[Country %in% "Central African Rep."     ] = "Central African Republic"
   Country[Country %in% "Czech Rep."               ] = "Czechia"
   Country[Country %in% "Dominican Rep."           ] = "Dominican Republic"
   Country[Country %in% "Eq. Guinea"               ] = "Equatorial Guinea"
   Country[Country %in% "Fr. Polynesia"            ] = "French Polynesia"
   Country[Country %in% "Guinea Bissau"            ] = "Guinea-Bissau"
   Country[Country %in% "Macedonia"                ] = "North Macedonia"
   Country[Country %in% "N. Cyprus"                ] = "Cyprus (Northern Cyprus)"
   Country[Country %in% "N. Korea"                 ] = "North Korea"
   Country[Country %in% "Nz"                       ] = "New Zealand"
   Country[Country %in% "S. Geo. and S. Sandw. Is."] = "South Georgia and the South Sandwich Islands"
   Country[Country %in% "S. Korea"                 ] = "South Korea"
   Country[Country %in% "Solomon Is."              ] = "Solomon Islands"
   Country[Country %in% "Somaliland"               ] = "Somalia"
   Country[Country %in% "Swaziland"                ] = "Eswatini"
   Country[Country %in% "Zaire"                    ] = "Congo (Kinshasa)"
   #---~---



   #---~---
   #   For every large country, identify the sub-national region if any data 
   # are from the country.
   #---~---
   LargeCountries = c("Australia","Brazil","Canada","China","Russia","United States")
   LargeCountries = LargeCountries[LargeCountries %in% Country]
   for (Large in LargeCountries){
      cat0("      > Assign sub-national region for data from ",Large,".")
      #---~---
      #   Select data from the large country
      #---~---
      IsLarge  = Country %in% Large
      PointsSP = vect( x    = data.frame(x=lon[IsLarge],y=lat[IsLarge])
                     , geom = c("x","y")
                     , crs  = "EPSG:4326"
                     )#end vect
      PointsSF = st_as_sf(PointsSP)
      #---~---


      #---~---
      #   Convert country data into a spatial polygon
      #---~---
      LargeSF = sf::st_as_sf(get(paste0(gsub(pattern=" ",replacement="",x=Large),"SP")))
      LargeSF = st_transform(LargeSF,crs(CountriesSF))
      #---~---


      #---~---
      #   Append sub-national label to data.
      #---~---
      dummy            = sf_use_s2(FALSE)
      LargeInfo        = as_tibble(st_join(PointsSF,LargeSF))
      dummy            = sf_use_s2(TRUE)
      SubNational      = as.character(LargeInfo$shapeISO)
      Append           = ! is.na(SubNational)
      Country[IsLarge] = ifelse( test = Append
                               , yes  = paste(Country[IsLarge],SubNational)
                               , no   = Country[IsLarge]
                               )#end ifelse
      #---~---
   }#end for (Large in LargeCountries)
   #---~---


   #---~---
   #   Simplify continents.
   #---~---
   Continent[Continent %in% "Australia"      ] = "Oceania"
   Continent[Continent %in% "Australasia"    ] = "Oceania"
   Continent[Continent %in% "Caribbean"      ] = "North America"
   Continent[Continent %in% "Central America"] = "North America"
   Continent[Continent %in% "Central Asia"   ] = "Asia"
   Continent[Continent %in% "East Asia"      ] = "Asia"
   Continent[Continent %in% "North Africa"   ] = "Africa"
   Continent[Continent %in% "South+E Africa" ] = "Africa"
   Continent[Continent %in% "South Asia"     ] = "Asia"
   Continent[Continent %in% "West Africa"    ] = "Africa"
   Continent[Continent %in% "West Asia"      ] = "Asia"
   #---~---


   #---~---
   #   Return a data table
   #---~---
   ans = tidyr::tibble( Country = Country, Continent = Continent)
   #---~---

   return(ans)
}#end TRY_LonLatToGeoInfo
#---~---



#---~---
#     This function selects African observations.
#---~---
SelectAfricaTropical <<- function(x){

   #---~---
   #   First step: List countries with considerable amount of land in the African tropics.
   #---~---
   AfricanCountries = c( "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi"
                       , "Cameroon","Cape Verde", "Central African Republic", "Chad"
                       , "Congo (Brazzaville)", "Congo (Kinshasa)", "Djibouti"
                       , "Equatorial Guinea", "Ethiopia", "Gabon", "Gambia", "Ghana"
                       , "Guinea-Bissau", "Ivory Coast", "Kenya", "Liberia", "Madagascar"
                       , "Malawi", "Mali", "Mauritania", "Mozambique", "Namibia", "Niger"
                       , "Nigeria", "Rwanda", "Senegal", "Seychelles", "Sierra Leone"
                       , "Somalia", "Sudan", "Tanzania", "Togo", "Uganda", "Zambia"
                       , "Zimbabwe"
                       )#end c
   AfricanContinents = c( "Africa")
   #---~--- 


   #---~---
   #   Second step: List climates considered tropical.  For tropical deserts, we apply
   # a somewhat more strict criterion to exclude non-tropical deserts to the extent 
   # possible.
   #---~---
   TropicalClimates   = c("Af","Am","As","Aw")
   TropDesertClimates = c("BSh","BWh")
   #---~---


   #---~---
   #   Third step: List tropical biomes.
   #---~---
   TropicalBiomes   = c( "02 - Tropical Grassland", "03 - Tropical Scrubland"
                       , "04 - Tropical Savannah", "05 - Tropical Dry Forest"
                       , "06 - Tropical Moist Forest", "07 - Mangroves"
                       )#end c
   DesertBiomes     = c("01 - Desert/Semi-arid")
   #---~---


   #---~---
   #   We first select the observations based on longitude and latitude. If not, then we
   # rely on countries, climates and biomes. We use 23.5 as a proxy for the intertropical
   # zone.
   #---~---
   IsAfricanCoord     = ( x$lon %wr% c(-20, 57) ) &  ( x$lat %wr% c(-23.5,23.5) )
   MissCoord          = is.na(x$lat) | is.na(x$lon)
   IsAfricanCountry   = MissCoord & ( x$country   %in% AfricanCountries  )
   IsAfricanContinent = MissCoord & ( x$continent %in% AfricanContinents )
   IsTropicalClimate  = MissCoord & ( x$climate   %in% TropicalClimates  )
   IsTropicalBiome    = MissCoord & ( x$biome     %in% TropicalBiomes    )
   #---~---


   #---~---
   #   For deserts, they must have hot summers and some indication that they are indeed 
   # tropical. This may miss montane tropical regions, unfortunately.
   #---~---
   IsTropicalDesert  = ( MissCoord & (x$climate_pft %in% "tropical" )
                         & ( ( x$climate %in% TropDesertClimates )
                             | ( x$biome   %in% DesertBiomes       )
                         )#end IsTropicalDesert
                       )#end IsTropicalDesert
   #---~---


   #---~---
   #   Select likely African observations. We will assess the inclusion/exclusion
   # based on how many data we end up with. We will seek to have as many as possible 
   # without risking adding sites that shouldn't be included.
   #---~---
   IsAfricantropical = ( IsAfricanCoord |  IsAfricanCountry 
                       | ( IsAfricanContinent
                         & ( IsTropicalClimate | IsTropicalBiome | IsTropicalDesert )
                         )#end IsAfricanContinent
                       )#end IsAfricantropical
   #---~---

   return(IsAfricantropical)
}#end function SelectAfricaTropical
#---~---






#---~---
#     This function selects Neotropical (sensu strictu) observations.
#---~---
SelectNeoTropical <<- function(x){

   #---~---
   #   First step: List countries (and Brazilian states) with considerable amount of land
   # in the tropics.
   #---~---
   NeotropCountries = c( "Anguilla", "Antigua and Barbuda", "Aruba", "Bahamas", "Barbados"
                       , "Belize", "Bolivia", "Brazil", "Brazil AC", "Brazil AL"
                       , "Brazil AM", "Brazil AP", "Brazil BA", "Brazil CE", "Brazil DF"
                       , "Brazil ES", "Brazil GO", "Brazil MA", "Brazil MG", "Brazil MS"
                       , "Brazil MT", "Brazil PA", "Brazil PB", "Brazil PE", "Brazil PI"
                       , "Brazil RJ", "Brazil RN", "Brazil RO", "Brazil RR", "Brazil SE"
                       , "Brazil SP", "Brazil TO", "British Virgin Islands"
                       , "Caribbean Netherlands", "Cayman Islands", "Colombia", "Costa Rica"
                       , "Cuba", "Curacao", "Dominica", "Dominican Republic", "Ecuador"
                       , "El Salvador", "French Guiana", "Grenada", "Guadeloupe"
                       , "Guatemala", "Guyana", "Haiti", "Honduras", "Jamaica", "Martinique"
                       , "Mexico", "Montserrat", "Nicaragua", "Panama", "Paraguay", "Peru"
                       , "Puerto Rico", "Saint Barthelemy", "Saint Kitts and Nevis"
                       , "Saint Lucia", "Saint Martin", "Saint Vincent and the Grenadines"
                       , "Sint Maarten", "Suriname", "Trinidad and Tobago"
                       , "Turks and Caicos Islands", "United States Virgin Islands"
                       , "Venezuela"
                       )#end c
   NeotropContinents = c( "North America", "South America")
   #---~---


   #---~---
   #   Add list of sites known to be located in Neotropical forests in the tropics
   #---~---
   NeotropSites = c( "10?26'N_83?59'W", "10?28'N_84?02'W"
                   , "35km S. of Brasilia; Brazil", "429_La Selva; Costa Rica"
                   , "429_Rio Loro; Zulia; Vanexuela", "429_Turrialba; Costa Rica"
                   , "658_Chile", "681_Panama", "726_French Guiana", "732_Costa Rica"
                   , "759_Panama", "776_tropical moist forest of La Chonta"
                   , "8?39'N__71?24'W", "9?10'N__79?51'W", "AC; Nova California;"
                   , "Acarouany", "AGP-01", "AGP-02", "Agua Salud", "Aguadita"
                   , "Aiuaba; Brazil", "ALF-01", "ALP-01", "ALP-11", "ALP-12", "ALP-21"
                   , "ALP-22", "ALP-30", "ALP-40", "Amazonas"
                   , "Area de Conservacion Guanacaste and Parque Nacional Palo Verde"
                   , "Arecibo", "BAFOG", "Bahia Hachal; P. N. Santa Rosa"
                   , "Barro Colorado Island", "Barro Colorado Island (BCI Panama)"
                   , "Barro Colorado Island (BCI)", "Barro Colorado Island;"
                   , "Barro Colorado Island; Panama"
                   , "Barro Colorado Island; Parque Natural Metropolitano and San Lorenzo Protected Area"
                   , "BCI", "BhaskarCAMex_SB.Bhaskar", "BhaskarCAMex_Teh.Bhaskar"
                   , "Blue Mountains National Park", "BNT-04", "BOG-01", "BOG-02", "Bolivar"
                   , "Bolivia", "Bongers_et_al_Los_Tuxtlas", "Bongers_LosTuxtlas"
                   , "Borinquen", "Bosque de Paz reserve", "Bosque de Paz reserve potrero"
                   , "Botanical Garden of the Universidad Central de Venezuela in Caracas"
                   , "Brasileiro de Geografia e Estatistica ecological reserve"
                   , "Brazil", "Brotas", "Buena Vista", "Cabo Rojo", "Cachi finca"
                   , "cafetal near Tapanti", "Cain56_Brazil", "Camerik_Itatiaia"
                   , "CaracasOpereas", "CaracasSuccessiolForest"
                   , "Caribbean National Forest", "Cartago", "CAX-01", "CAX-02"
                   , "Caxiuana Forest-Almeirim_Brazil", "Central Amazon; near Tefe; Brazil"
                   , "central Veracruz", "CernusakPanama_Gamboa"
                   , "Cerrado reservoir located in the north area of Federal University of Sao Carlos"
                   , "Cerro Caraigres", "Cerro de la Muerte pasture", "Cerro de Uyuca"
                   , "Cerro Jefe", "Chamela", "Charallave", "Chiapas", "Chimborazo"
                   , "CHO-01", "Coley_BCI", "Colombia", "Colombian Amazon"
                   , "Colombian Amazonia; near Araracuara; Caqueta", "Colonizer"
                   , "Combu Isl; near Belem; Brazil", "Control - T1", "Control - T2"
                   , "Control - T3", "Cordillera Central", "Coro", "Costa del Pacifico"
                   , "Costa Rica", "CPP-01", "Curua-Una Forest Reserve"
                   , "Curvelo; Minas Gerais state; Brazil", "CUZ-03"
                   , "DF; Brasilia; Ecological Reserve of IBGE", "Diemer-Ecuador_high"
                   , "Diemer-Ecuador_highest", "Diemer-Ecuador_low", "Diemer-Ecuador_lowest"
                   , "DominguesSantarem_FLONAKm67", "Dominican Republic"
                   , "DROUGHT_AGP01_columbia", "DROUGHT_AGP02_columbia"
                   , "DROUGHT_ALP11_peru", "DROUGHT_ALP12_peru", "DROUGHT_ALP21_peru"
                   , "DROUGHT_ALP22_peru", "DROUGHT_ALP30_peru", "DROUGHT_BNT04_brazil"
                   , "DROUGHT_JEN11_Peru", "DROUGHT_JEN12_Peru", "DROUGHT_LOR01_columbia"
                   , "DROUGHT_LOR02_columbia", "DROUGHT_RES04_Brazil"
                   , "DROUGHT_RES05_Brazil", "DROUGHT_RES06_Brazil"
                   , "DROUGHT_SCR01_venezuela", "DROUGHT_SCR04_venezuela"
                   , "DROUGHT_SCR05_venezuela", "DROUGHT_SUC01_peru", "DROUGHT_SUC02_peru"
                   , "DROUGHT_SUC03_peru", "Dzibilchaltun"
                   , "East Coast;  Estacion de Biologia Los Tuxtlas"
                   , "Ecological Reserve of IBGE"
                   , "Ecological Reserve of the Brazilian Institute of Geography and Statistics"
                   , "Edwards_Bolivia1", "Edwards_Bolivia2", "Edwards_Bolivia3"
                   , "Edwards_DominicanRep1", "Edwards_DominicanRep2"
                   , "Edwards_DominicanRep3", "Edwards_Venezuela", "El Cayo"
                   , "El Verde Field Station", "Emas (Pirassununga); Sao Paulo; Brazil"
                   , "Estacion de Biologia Chamela", "Estacion de Biologia Los Tuxtlas"
                   , "Estacion de Biologia Tropical; Los Tuxtlas", "Estado Aragua"
                   , "Estado Miranda", "FLOKm67", "forest near Navarro", "forest near Orosi"
                   , "Fort Sherman", "Fort Sherman canopy crane; Panama (Atlantic Coast)"
                   , "Fortuna watershed; Cordillera Central"
                   , "Franco_Brasilia_Instituto Brasileiro de Geografia e Estatistica EcolRes"
                   , "Franco&Luttge_Brasilia", "Franco&Luttge_Brasilia_Brazil"
                   , "Freddy's finca", "French Guiana", "French Guyana", "Fringe"
                   , "Ft.Sherman", "Gallagher_Bolivia _Las Trancas study area"
                   , "Gallagher_Brazil_Araras", "Gallagher_Brazil_Belo Horizontle"
                   , "Gallagher_Brazil_Bragantina region of East-Amazonia"
                   , "Gallagher_Brazil_Ecological Station of Paulo de Faria"
                   , "Gallagher_Brazil_Fazenda Barreiro Rico"
                   , "Gallagher_Brazil_Flona-Tapajo forest"
                   , "Gallagher_Brazil_Itatiaia National Park"
                   , "Gallagher_Brazil_Paragominas", "Gallagher_Brazil_Paragominas 2"
                   , "Gallagher_Brazil_Ribeirao Cachoeira", "Gallagher_Brazil_Santa Genebra"
                   , "Gallagher_Brazil_Santa Genebra Reserve", "Gallagher_Brazil_Sao Carlos"
                   , "Gallagher_Brazil_Sao Jose do Rio Preto"
                   , "Gallagher_Brazil_Sao Vicente", "Gallagher_Brazil_Vassununga"
                   , "Gallagher_Colombia_Pena Roja"
                   , "Gallagher_Colombia_Tinigua National Park"
                   , "Gallagher_Costa Rica_La Selva"
                   , "Gallagher_Costa Rica_Sector Santa Rosa of the"
                   , "Gallagher_French Guiana_Nouragues Biological Station"
                   , "Gallagher_Guyana_Mabura Hill"
                   , "Gallagher_Mexico_Calakmul Biosphere Reserve"
                   , "Gallagher_Mexico_Estacion de Biologia"
                   , "Gallagher_Panama_Parque Metropoltiano"
                   , "Gallagher_Peru_Allpahuayo Reserve"
                   , "Gallagher_Puerto Rico_Luquillo Experimental Forest (LEF)"
                   , "Gallagher_Venezuela _Arboretum de la Escuela"
                   , "Gallagher_Venezuela _Rio Negro Basin", "Gamboa", "Garden in Bri Bri"
                   , "Gorgona", "Grubb_Jamaica_DryLimeScrub", "Grubb_Jamaica_DrySlope"
                   , "Grubb_Jamaica_GullyForest", "Grubb_Jamaica_HighAltitude"
                   , "Grubb_Jamaica_MorRidge", "Grubb_Jamaica_MullRidge"
                   , "Grubb_Jamaica_VeryWetRidge", "Grubb_Jamaica_WetLimeLOW"
                   , "Grubb_Jamaica_WetLimeUP", "Grubb_Jamaica_WetSlope"
                   , "Guagua Pichincha; Cayambe", "Guajataca", "Guanacaste", "Guanica"
                   , "Gx-7", "Gx1-9", "HCC-21", "HCC-22", "Heredia", "Heredia Province"
                   , "Hidalgo", "Hogan_etal_PNM_crane", "Hogan_etal_PNM_crane_Panama"
                   , "Horizontes", "Horquetas", "Huc et al 1994_CIRADForets.FrenchGuiana"
                   , "IBGE Ecological Reserve", "Ihla de Marchantaria (Manaus)", "Inpa"
                   , "INRA; Kourou; French Guiana"
                   , "Instituto Brasileiro de Geografia e Estatistica EcolRes"
                   , "Itatinga E102", "Itatinga; Sao Paulo; SE Brazil", "JAC-01", "JAC-03"
                   , "JAC-04", "Jalisco;  Estacion de Biologia Chamela"
                   , "Jaru Biological Reserve; Rondonia State; south-west Brazil"
                   , "JAS-02", "JAS-03", "JAS-04", "JAS-05", "JEN-11", "JEN-12", "JRI-01"
                   , "JUR-01", "Kappelle_CostaRica", "Kelly_Jamaica_BroomHall"
                   , "Kelly_Jamaica_HogHouseHill", "Kelly_Jamaica_RoundHill"
                   , "Kitajima_Pama", "Kitajima_Panama", "Kitajima_Panama_Panama"
                   , "km 43 marker on Interamerican Highway", "km 67"
                   , "km 67 Seca-Floresta Site", "km 77", "km 83", "Kosnipata Valley"
                   , "Kourou", "La Casona; P. N. Santa Rosa", "La Chonta"
                   , "La Chonta Forest; Bolivia", "La Esperanza"
                   , "La Mancha Lagoon;Veracruz", "La Paz"
                   , "La Selva Biological Station; Costa Rica"
                   , "La Selva BiologicalResearch Station; Costa Rica"
                   , "La Selva Research Station", "La_Chonta__Bolivia"
                   , "Las Joyas Scientific Station;Sierra de Manantlan Biosphere Reserve; Jalisco State; western Mexico"
                   , "LaSelva", "Lat -2.58; Long -60.1", "Lat 2.854; Long -54.958"
                   , "les Nouragues Biological Field Station; French Guiana", "LFB-01"
                   , "LFB-02", "LOR-01", "LOR-02", "Loreto", "Los Esesmiles", "Los Tuxtlas"
                   , "Los Tuxtlas Tropical RainforestReserve; Mexico"
                   , "Lowland tropical dry forest of Area de Conservacion Guanacaste (ACG)"
                   , "Luquillo Experimental Forest (LEF)"
                   , "Luquillo Experimental Forest;  El Verde Field Station"
                   , "Madre de dios; Peru", "ManuelAntonio", "Marin_Medina_Piritu_Venezuela"
                   , "Martin_etal_Guanacaste", "Martin_etal_Guanacaste_Costa Rica", "Maui"
                   , "MBO-01", "Medina78_SanCarlos_RioNegro", "MET", "Mexico"
                   , "Minas Gerais state", "Missao do Cururu", "Mogi Guacu", "Mojui"
                   , "Moles_Chajul", "Moles_Chamela", "Moles_LosAmigosBamboo"
                   , "Moles_LosAmigosfloodplain", "Moles_LosAmigosFloodplain"
                   , "Moles_LosAmigosSuccessional", "Moles_LosAmigosTerrace"
                   , "Montagne Tortue", "Monte Guilarte", "Monteverde"
                   , "Mulkey9193_BCI_Panama", "Mulkey9193_BCI_Panama_Panama"
                   , "N of Manaus; Brazil", "N Venezuela", "Nara", "NE Para; Brazil"
                   , "near Queimadas; Bahia state; NE Brazil", "neoEcuador", "neoLaSelva"
                   , "neoPeru", "Neotropics_AtCrane", "Neotropics_Bolivia"
                   , "Nhecolandia Pantanal; MG; Brazil", "northeastern Brazil"
                   , "northeastern Costa Rica", "Northern Yucatan; Mexico", "Nouragues"
                   , "Oaxaca", "Olivares_Caracas_Venezuela", "outside Panama; Panama"
                   , "P. N. Cahuita", "P. N. Manuel Antonio"
                   , "P. N. Rincon de la Vieja; trail to main crater"
                   , "PA;  Curua-Una Forest Reserve", "Palo Verde National Park", "Pama"
                   , "Pampa del Tamarugal; Atacama Desert; C. Chile", "Panama", "Paracou"
                   , "Paracou_French Guiana", "Paragominas; Para; Brazil"
                   , "Paramo de la Virgen", "Parolin_Manaus", "Parque Nacional San Lorenzo"
                   , "Parque Natural Metropolitana", "Parque Natural Metropolitano"
                   , "Parque Natural Metropolitano canopy crane; Panama (Pacific Coast)"
                   , "Parque Natural Metropolitano; Panama City", "ParqueMetropolitano"
                   , "Passo di Cumbre", "Passo di Jama", "PBR_unpbl_San Carlos bana"
                   , "PBR_unpbl_San Carlos caat", "PBR_unpbl_San Carlos TFsoil"
                   , "Perez Zeledon finca", "Peru", "Petit Piton", "Playa Dominicalito"
                   , "Playa Hermosa", "Playa Herradura", "plot Sassaan", "PNM"
                   , "PNMCrane_neoHogan", "Podocarpus Nat Park; S Ecuador"
                   , "Podocarpus National Park and the Reserva Biologica San Francisco in the Provinces of Loja and Z ..."
                   , "Prado&DeMoraes1997_SAO_CARLOS", "Prado&DeMoraes1997_SAO_CARLOS_Brazil"
                   , "Primary forest", "Puente d.Inqua;Mendoza"
                   , "Puerto Rico; Luquillo Experimental Forest Biosphere Reserve"
                   , "Puntarenas Province", "Pyykko_Upata", "RAINFOR_BOG01", "RAINFOR_BOG02"
                   , "RAINFOR_BRA01", "RAINFOR_CAX01", "RAINFOR_CAX02", "RAINFOR_CAX03"
                   , "RAINFOR_CAX04", "RAINFOR_CAX05", "RAINFOR_CHO01", "RAINFOR_CPP01"
                   , "RAINFOR_CUZ03", "RAINFOR_ELD12", "RAINFOR_ELD34", "RAINFOR_HCC21"
                   , "RAINFOR_HCC22", "RAINFOR_JAS02", "RAINFOR_JAS03", "RAINFOR_JAS04"
                   , "RAINFOR_JAS05", "RAINFOR_JRI01", "RAINFOR_LFB01", "RAINFOR_LFB02"
                   , "RAINFOR_LSL01", "RAINFOR_LSL02", "RAINFOR_MAN01", "RAINFOR_MAN02"
                   , "RAINFOR_MAN03", "RAINFOR_MAN04", "RAINFOR_MBO01", "RAINFOR_RES03"
                   , "RAINFOR_RES04", "RAINFOR_RES05", "RAINFOR_RES06", "RAINFOR_RIO12"
                   , "RAINFOR_SUC04", "RAINFOR_SUM01", "RAINFOR_TAM01", "RAINFOR_TAM02"
                   , "RAINFOR_TAM03", "RAINFOR_TAM04", "RAINFOR_TAM05", "RAINFOR_TAM06"
                   , "RAINFOR_TAM07", "RAINFOR_TAP01", "RAINFOR_TAP02", "RAINFOR_TAP03"
                   , "RAINFOR_TAP04", "RAINFOR_TIP03", "RAINFOR_TIP05", "RAINFOR_YAN01"
                   , "RAINFOR_YAN02", "Rara Avis Reserve", "Reichetal_Venezuela"
                   , "Reichetal_Venezuela_Venezuela", "RES-02", "RES-04", "RES-05", "RES-06"
                   , "Reserva Florestal Adolpho Ducke"
                   , "Restinga de Jurubatiba National Park", "Riberalta; Bolivia"
                   , "Riberalta; Bolivian Amazon", "Rio Grande", "Rio Grande du Sol"
                   , "Rio Kananari", "Rio Negro", "road near CATIE station"
                   , "road to P. N. Tapanti", "road to P. N. Tenorio"
                   , "Road to Playa Naranjo; P. N. Santa Rosa", "road to Taus"
                   , "RolletRoth1990_Immataca", "Roth_1992_PlayaGrande"
                   , "RR; Maraca Island;", "S~ao Paulo State; SE Brazil"
                   , "San Carlos de Rio Negro", "San Carlos de Rio NegroColonizer"
                   , "San Carlos de Rio NegroCultivated"
                   , "San Carlos de Rio NegroEarly succession"
                   , "San Carlos de Rio NegroLate succession", "San Carlos TFsoil"
                   , "San Carlos; Amazonas", "San Cristobal", "San Martin", "San Pedro"
                   , "San Pedro di Atacama", "San Ramon", "SANLO", "Santa Rosa; Costa Rica"
                   , "SantiagoMexico_Dzibilchaltun", "SantiagoPanama_Ft.Sherman"
                   , "SantiagoPanama_ParqueMetropolitano"
                   , "Sardinilla; Bue Vista region; Pama", "Saut Lavilette", "SB.Bhaskar"
                   , "Sector Santa Rosa of Area de Conservaci on Guanacaste"
                   , "Shadehouse in Perez Zeledon"
                   , "Sierra de Huatla Reserve;  Early successional (10 years)"
                   , "Sierra de Huatla Reserve;  Late successional (60 years)"
                   , "Sierra de Huatla Reserve;  Primary forest"
                   , "Sierra de los Cuchumatanes", "Sierra de Luquillo", "SIP-01"
                   , "slopes of Turrialba Volcano", "Sobrado_Charallave"
                   , "Sobrado_Charallave_Venezuela", "Sobrado&Medina_SanCarlos_bana"
                   , "SobradoMedina_SanCarlosBana", "SobradoVenezuela_CaracasOpenAreas"
                   , "SobradoVenezuela_CaracasSuccessionalForest"
                   , "South America (tropical)", "SP; Campinas;", "SPD-01", "SPD-02"
                   , "Sterk_frenchguiana", "SUC-01", "SUC-02", "SUC-03", "SUC-05"
                   , "Sugden_Venezuela_High", "Sugden_Venezuela_Low"
                   , "Sugden_Venezuela_Med", "SUM-01", "Susques", "TAM-01", "TAM-02"
                   , "TAM-04", "TAM-05", "TAM-06", "TAM-07", "TAM-09", "Tambopata_Peru"
                   , "TAP-04", "Tapajos NF; Brazil", "Taus", "Tehuacan Valey"
                   , "Tezara_etal98_Coro", "Tezara_etal98_Coro_Venezuela"
                   , "Teziutlan-Nautla roads", "Tinamu", "TIP-03", "TIP-05", "Tlalchinol"
                   , "Tonantins", "Toro Negro", "Trachypogon-savanna area in Cojedes state"
                   , "Tres cruses", "Tresor", "Trocha plot 8"
                   , "tropical forest in the Parque Nacional San Lorenzo on the Caribbean coast of Central Panama"
                   , "tropical moist forest of La Chonta", "Troy", "TRU-01", "TRU-03"
                   , "TRU-04", "TRU-08", "VelazquezRosas_LaChinantla_1"
                   , "VelazquezRosas_LaChinantla_2", "VelazquezRosas_LaChinantla_3"
                   , "VelazquezRosas_LaChinantla_4", "VelazquezRosas_LaChinantla_5"
                   , "Venezuela", "Venezuela's Pataguana peninsula", "Vera Cruz"
                   , "Volcan Barva", "Volcan Tajumulco", "WAY-01", "Wayquecha"
                   , "Wilf_Chinea_Bisley", "Williams et al_LosTuxtlas2"
                   , "Williams et al_LosTuxtlas2_Mexico", "Williams_Linera_Mexico"
                   , "YAN-01", "YAN-02", "Yasuni Forest Dynamics plot; Ecuador"
                   , "Yasuni National Park; Ecuador; Manu National Park; Peru"
                   , "Yasuni-ridgetop", "Yasuni-upper slope", "Yasuni-valley bottom"
                   , "Zacapa", "Zotz_Fortuna_Panama"
                   )#end c
   #---~---



   #---~---
   #   Second step: List climates considered tropical.  For tropical deserts, we apply
   # a somewhat more strict criterion to exclude non-tropical deserts to the extent 
   # possible.
   #---~---
   TropicalClimates   = c("Af","Am","As","Aw")
   TropDesertClimates = c("BSh","BWh")
   #---~---



   #---~---
   #   Second step: List tropical biomes.
   #---~---
   TropicalBiomes   = c( "02 - Tropical Grassland", "03 - Tropical Scrubland"
                       , "04 - Tropical Savannah", "05 - Tropical Dry Forest"
                       , "06 - Tropical Moist Forest", "07 - Mangroves"
                       )#end c
   DesertBiomes     = c("01 - Desert/Semi-arid")
   #---~---


   #---~---
   #   We first select the observations based on longitude and latitude. If not, then we
   # rely on countries, climates and biomes. We use 23.5 as a proxy for the intertropical
   # zone.
   #---~---
   IsNeotropCoord     = ( x$lon %wr% c(-110.25,-34.75) ) &  ( x$lat %wr% c(-23.5,23.5) )
   MissCoord          = is.na(x$lat) | is.na(x$lon)
   IsNeotropSite      = MissCoord & ( x$site      %in% NeotropSites )
   IsNeotropCountry   = MissCoord & ( x$country   %in% NeotropCountries )
   IsNeotropContinent = MissCoord & ( x$continent %in% NeotropContinents )
   IsTropicalClimate  = MissCoord & ( x$climate   %in% TropicalClimates )
   IsTropicalBiome    = MissCoord & ( x$biome     %in% TropicalBiomes )
   #---~---


   #---~---
   #   For deserts, they must have hot summers and some indication that they are indeed 
   # tropical. This may miss montane tropical regions, unfortunately.
   #---~---
   IsTropicalDesert  = ( MissCoord & (x$climate_pft %in% "tropical" )
                       & ( ( x$climate %in% TropDesertClimates )
                         | ( x$biome   %in% DesertBiomes       )
                         )#end IsTropicalDesert
                       )#end IsTropicalDesert
   #---~---


   #---~---
   #   Select likely Neotropical observations. We will assess the inclusion/exclusion
   # based on how many data we end up with. We will seek to have as many as possible 
   # without risking adding sites that shouldn't be included.
   #---~---
   IsNeotropical = ( IsNeotropCoord | IsNeotropSite | IsNeotropCountry 
                   | ( IsNeotropContinent
                     & ( IsTropicalClimate | IsTropicalBiome | IsTropicalDesert )
                     )#end IsNeotropContinent
                   )#end IsNeotropical
   #---~---

   return(IsNeotropical)
}#end function SelectNeoTropical
#---~---




#---~---
#     This function selects West US (sensu strictu) observations.
#---~---
SelectWestUS <<- function(x){
   #---~---
   #   First step: List country/state of interest.
   #---~---
   WestUSStates = c("United States CA", "United States OR", "United States WA")
   WestUSCountry = c("United States")
   WestUSContinent = c("North America")
   #---~---


   #---~---
   #   Second step: List climates represented in West US.
   #---~---
   WestUSClimates   = c("Csa", "Csb", "Dsb", "Dsc", "Dfa", "Dfc")
   #---~---


   #---~---
   #   We first select the observations based on longitude and latitude. If not, then we
   # rely on state, country, and climate.
   #---~---
   IsWestUSCoord      = ( x$lon %wr% c(-126,-114) ) &  ( x$lat %wr% c(32,49) )
   MissCoord          = is.na(x$lat) | is.na(x$lon)
   IsWestUSState      = MissCoord & (x$country   %in% WestUSStates)
   IsWestUSCountry    = MissCoord & (x$country   %in% WestUSCountry)
   IsWestUSContinent  = MissCoord & (x$continent %in% WestUSContinent)
   IsWestUSClimate    = MissCoord & (x$climate   %in% WestUSClimates )
   #---~---


   #---~---
   #   Select likely WestUS observations. We will assess the inclusion/exclusion
   # based on how many data we end up with. We will seek to have as many as possible 
   # without risking adding sites that shouldn't be included.
   #---~---
   IsWestUS = ( IsWestUSCoord | IsWestUSState
              | ( IsWestUSContinent & IsWestUSCountry & IsWestUSClimate)
              )#end IsWestUS
   #---~---

   return(IsWestUS)
}#end function SelectWestUS
#---~---




#---~---
#   This function assigns growth forms to individuals using harmonised species and 
# additional inference from trait values.
#---~---
TRY_Harmonise_GrowthForm <<- function(x,MinDBH=10,MinHeight=5){

   #--- Some growth form entries are empty but not with NA. Turn them into NA
   x = x %>%
      mutate( growth_form     = ifelse( test = growth_form %in% ""
                                      , yes  = NA_character_
                                      , no   = growth_form
                                      )#end ifelse
            , plant_woodiness = ifelse( test = plant_woodiness %in% ""
                                      , yes  = NA_character_
                                      , no   = plant_woodiness
                                      )#end ifelse
            )#end mutate
   #---~---

   #--- Initialise tibble for unique growth forms, and append counter for likely trees
   UniqGrowth = x %>%
      mutate( TreeScore      = 0L
            , ShrubScore     = 0L
            , WoodyScore     = 0L
            , SemiWoodyScore = 0L
            , NonWoodyScore  = 0L
            )#end mutate
   #---~---

   #---~---
   #   Try multiple wood traits.
   #---~---

   TreeRaunkiaer      = c( "Megaphanerophyte" , "Mesophanerophyte")
   ShrubRaunkiaer     = c( "Microphanerophyte", "Nanophanerophyte")
   WoodyRaunkiaer     = c( TreeRaunkiaer, ShrubRaunkiaer )
   SemiWoodyRaunkiaer = c( "Chamaephyte" )
   NonWoodyRaunkiaer  = c( "Aerophyte", "Epiphyte"  ,"Hemicryptophyte", "Geophyte"
                         , "Helophyte", "Hydrophyte", "Therophyte"    )
   UniqGrowth    = UniqGrowth %>%
       mutate( TreeScore      = ( TreeScore      + as.integer(dbh       %ge% MinDBH              )
                                                 + as.integer(height    %ge% MinHeight           )
                                                 + as.integer(raunkiaer %in% TreeRaunkiaer       ) )
             , ShrubScore     = ( ShrubScore     + as.integer(raunkiaer %in% ShrubRaunkiaer      ) )
             , WoodyScore     = ( WoodyScore     + as.integer(is.finite(wood_dens)               )
                                                 + as.integer(is.finite(bark_dens)               )
                                                 + as.integer(dbh       %ge% MinDBH              )
                                                 + as.integer(height    %ge% MinHeight           )
                                                 + as.integer(raunkiaer %in% WoodyRaunkiaer      ) )
             , SemiWoodyScore = ( SemiWoodyScore + as.integer(raunkiaer %in% SemiWoodyRaunkiaer  ) )
             , NonWoodyScore  = ( NonWoodyScore  + as.integer(raunkiaer %in% NonWoodyRaunkiaer   ) )
             )#end mutate
   #---~---


   #---~---
   #   Summarise information for each species.
   #---~---
   UniqGrowth = UniqGrowth %>%
      group_by(ScientificName) %>%
      summarise( GrowthForm     = commonest(x=growth_form    ,na.rm=TRUE)
               , Woodiness      = commonest(x=plant_woodiness,na.rm=TRUE)
               , TreeScore      = sum      (x=TreeScore      ,na.rm=TRUE)
               , ShrubScore     = sum      (x=ShrubScore     ,na.rm=TRUE)
               , WoodyScore     = sum      (x=WoodyScore     ,na.rm=TRUE)
               , SemiWoodyScore = sum      (x=SemiWoodyScore ,na.rm=TRUE)
               , NonWoodyScore  = sum      (x=NonWoodyScore  ,na.rm=TRUE)
               ) %>% #end summarise
      ungroup() %>%
      mutate( GrowthForm = ifelse( test = is.na(GrowthForm) & (TreeScore     %gt% 0 )
                                 , yes  = "Tree"
                                 , no   = GrowthForm
                                 )#end ifelse
            , GrowthForm = ifelse( test = is.na(GrowthForm) & (ShrubScore    %gt% 0 )
                                 , yes  = "Shrub"
                                 , no   = GrowthForm
                                 )#end ifelse
            , Woodiness  = ifelse( test = is.na(Woodiness) & (WoodyScore     %gt% 0 )
                                 , yes  = "Woody"
                                 , no   = Woodiness
                                 )#end ifelse
            , Woodiness  = ifelse( test = is.na(Woodiness) & (SemiWoodyScore %gt% 0 )
                                 , yes  = "Semi-woody"
                                 , no   = Woodiness
                                 )#end ifelse
            , Woodiness  = ifelse( test = is.na(Woodiness) & (NonWoodyScore  %gt% 0 )
                                 , yes  = "Non-woody"
                                 , no   = Woodiness
                                 )#end ifelse
            )#end mutate
   #---~---


   #---~---
   #   Assign missing values to the commonest life form identified
   #---~---
   idx    = match(x$ScientificName,UniqGrowth$ScientificName)
   Answer = x %>%
      mutate( growth_form     = ifelse( test = is.na(growth_form)
                                      , yes  = UniqGrowth$GrowthForm[idx]
                                      , no   = growth_form
                                      )#end ifelse
            , plant_woodiness = ifelse( test = is.na(plant_woodiness)
                                      , yes  = UniqGrowth$Woodiness [idx]
                                      , no   = plant_woodiness
                                      )#end ifelse
            )#end mutate
   #---~---


   #--- Return answer
   return(Answer)
   #---~---
}#end TRY_Harmonise_GrowthForm
#---~---



#---~---
#   Function to help harmonising biomes using habitat information.
#   NOTE: This is done for sub-regions because there are way too many habitat classes.
#---~---
TRY_Harmonise_NeoTropical_Biome <<- function(x){

   #--- Return input in case variables are missing.
   if (! all(c("biome","habitat") %in% names(x))) return(x)
   #---~---

   #--- Copy data to scalars, but only for potential replacement
   Habitat = ifelse(test=is.na(x$biome),yes=tolower(x$habitat),no=NA_character_)
   #---~---



   #---~---
   #   We only use habitat when the information makes it unambiguous.
   #---~---
   IsDesert       = Habitat %in% c("desert")
   IsTropScrub    = Habitat %in% c("bush steppe","shrub steppe")
   IsTropSav      = Habitat %in% c("cerrado","sav")
   IsTropDrFor    = Habitat %in% c("dry forest","dry forest near coast")
   IsTropMoFor    =
      Habitat %in% c( "amazonian terra firme forest"
                    , "evergreen lowland tropical forest on a terra firme plateau (grand plateau) on granite substrate ..."
                    , "evergreen lowland tropical forest on a terra firme plateau (petit plateau) densely populated wi ..."
                    , "evergreen lowland tropical forest on a terra firme plateau bluff above acarouany creek."
                    , "evergreen lowland tropical forest on a terra firme plateau bluff above mataroni creek near the  ..."
                    , "evergreen lowland tropical forest on a terra firme plateau near st. laurent du maroni that was  ..."
                    , "evergreen lowland tropical forest on a terra firme plateau with lateral drainage."
                    , "evergreen lowland tropical forest on a terra firme plateau with some podzolization and disturba ..."
                    , "evergreen lowland tropical forest on a terra firme volcanic plateau within the tresor nature re ..."
                    , "evergreen lowland tropical forest on a terra firme volcanic plateau."
                    , "forest (woodland; forest habitats and other wooded land)"
                    , "fragment of humid forest by roadside", "humid forest"
                    , "humid forest at edge of pasture", "logged forest", "mature forest"
                    , "montane rain forest", "primary forest", "rain forest"
                    , "roadside in humid forest", "tropical wet forest", "troprf"
                    )#end c
   IsMangrove     = Habitat %in% c("mangroves bordering beach")
   IsPasture      =
      Habitat %in% c( "pasture", "pasture among fragments of rain forest"
                    , "pasture bordering rain forest", "pasture in matrix of cloud forest"
                    , "pasture in matrix of cloud forest remnants"
                    , "pastures in matrix of rain forest", "roadside among pastures"
                    )#end c
   IsPlantation   =
      Habitat %in% c( "3-year old experimental coffee agroforestry site"
                    , "coffee plantation in matrix of humid forest"
                    )#end c
   IsWetlands     = Habitat %in% c("rheophyte")


   #---~---
   #   Update biome
   #---~---
   Answer = x
   Answer$biome[IsDesert    ] = "01 - Desert/Semi-arid"
   Answer$biome[IsTropScrub ] = "03 - Tropical Scrubland"
   Answer$biome[IsTropSav   ] = "04 - Tropical Savannah"
   Answer$biome[IsTropDrFor ] = "05 - Tropical Dry Forest"
   Answer$biome[IsTropMoFor ] = "06 - Tropical Moist Forest"
   Answer$biome[IsMangrove  ] = "07 - Mangroves"
   Answer$biome[IsPasture   ] = "24 - Pastures"
   Answer$biome[IsPlantation] = "26 - Planted Forests"
   #---~---


   #--- Return answer
   return(Answer)
   #---~---
}#end TRY_Harmonise_NeoTropical_Biome
#---~---





#---~---
#     This function fills in mass- and area-based photosynthesis traits when SLA and
# one of the bases are provided. This will respect the input data, even if they are
# not consistent.
#
# Input variables:
# x      - tibble object with traits with standard names.
# am_fac - additional area-mass conversion factor (in case units are inconsistent)
#---~---
TRY_Harmonise_Photosynthesis <<- function(x,am_fac=1.){
   Answer = x %>%
      mutate( a_amax  = ifelse(test=is.finite(a_amax ),yes=a_amax ,no=m_amax /SLA/am_fac)
            , a_jmax  = ifelse(test=is.finite(a_jmax ),yes=a_jmax ,no=m_jmax /SLA/am_fac)
            , a_rdmax = ifelse(test=is.finite(a_rdmax),yes=a_rdmax,no=m_rdmax/SLA/am_fac)
            , a_vcmax = ifelse(test=is.finite(a_vcmax),yes=a_vcmax,no=m_vcmax/SLA/am_fac)
            , m_amax  = ifelse(test=is.finite(m_amax ),yes=m_amax ,no=a_amax *SLA*am_fac)
            , m_jmax  = ifelse(test=is.finite(m_jmax ),yes=m_jmax ,no=a_jmax *SLA*am_fac)
            , m_rdmax = ifelse(test=is.finite(m_rdmax),yes=m_rdmax,no=a_rdmax*SLA*am_fac)
            , m_vcmax = ifelse(test=is.finite(m_vcmax),yes=m_vcmax,no=a_vcmax*SLA*am_fac)
            )#end mutate

   return(Answer)
}#end function TRY_Harmonise_Photosynthesis
#---~---





#---~---
#     This function fills in leaf density and leaf thickness when SLA and
# one of the bases are provided. This will respect the original input data, even if they 
# are not self-consistent.
#
# Input variables:
# x      - tibble object with traits with standard names.
# td_fac - additional conversion factor (in case units are inconsistent)
#---~---
TRY_Harmonise_LeafArchitecture <<- function(x,td_fac=1.){
   Answer = x %>%
      mutate( leaf_dens  = ifelse(test=is.finite(leaf_dens ),yes=leaf_dens ,no=td_fac/SLA/leaf_thick)
            , leaf_thick = ifelse(test=is.finite(leaf_thick),yes=leaf_thick,no=td_fac/SLA/leaf_dens )
            )#end mutate
   return(Answer)
}#end function TRY_Harmonise_LeafArchitecture
#---~---





#---~---
#     This function fills in mass- and area-based component concentration traits when SLA
# and one of the bases are provided. This will respect the original input data, even if they 
# are not self-consistent.
#
# Input variables:
# x      - tibble object with traits with standard names.
# am_fac - additional area-mass conversion factor (in case units are inconsistent)
#---~---
TRY_Harmonise_AreaMassConc <<- function(x,a2m_fac=1.){
   Answer = x %>%
      mutate( leaf_m_carbon = ifelse(test=leaf_m_carbon %gt% 0.,yes=leaf_m_carbon,no=NA_real_)
            , leaf_a_carbon = ifelse(test=leaf_a_carbon %gt% 0.,yes=leaf_a_carbon,no=NA_real_)
            , leaf_m_nitro  = ifelse(test=leaf_m_nitro  %gt% 0.,yes=leaf_m_nitro ,no=NA_real_)
            , leaf_a_nitro  = ifelse(test=leaf_a_nitro  %gt% 0.,yes=leaf_a_nitro ,no=NA_real_)
            , leaf_m_phosph = ifelse(test=leaf_m_phosph %gt% 0.,yes=leaf_m_phosph,no=NA_real_)
            , leaf_a_phosph = ifelse(test=leaf_a_phosph %gt% 0.,yes=leaf_a_phosph,no=NA_real_)
            , leaf_m_chloro = ifelse(test=leaf_m_chloro %gt% 0.,yes=leaf_m_chloro,no=NA_real_)
            , leaf_a_chloro = ifelse(test=leaf_a_chloro %gt% 0.,yes=leaf_a_chloro,no=NA_real_)
            , leaf_m_carot  = ifelse(test=leaf_m_carot  %gt% 0.,yes=leaf_m_carot ,no=NA_real_)
            , leaf_a_carot  = ifelse(test=leaf_a_carot  %gt% 0.,yes=leaf_a_carot ,no=NA_real_)
            , leaf_m_carbon = ifelse( test = is.finite(leaf_m_carbon)
                                    , yes  = leaf_m_carbon
                                    , no   = leaf_a_carbon * SLA * a2m_fac
                                    )#end ifelse
            , leaf_a_carbon = ifelse( test = is.finite(leaf_a_carbon)
                                    , yes  = leaf_a_carbon
                                    , no   = leaf_m_carbon / SLA / a2m_fac
                                    )#end ifelse
            , leaf_m_nitro  = ifelse( test = is.finite(leaf_m_nitro )
                                    , yes  = leaf_m_nitro 
                                    , no   = leaf_a_nitro  * SLA * a2m_fac
                                    )#end ifelse
            , leaf_a_nitro  = ifelse( test = is.finite(leaf_a_nitro )
                                    , yes  = leaf_a_nitro 
                                    , no   = leaf_m_nitro  / SLA / a2m_fac
                                    )#end ifelse
            , leaf_m_phosph = ifelse( test = is.finite(leaf_m_phosph)
                                    , yes  = leaf_m_phosph
                                    , no   = leaf_a_phosph * SLA * a2m_fac
                                    )#end ifelse
            , leaf_a_phosph = ifelse( test = is.finite(leaf_a_phosph)
                                    , yes  = leaf_a_phosph
                                    , no   = leaf_m_phosph / SLA / a2m_fac
                                    )#end ifelse
            , leaf_m_chloro = ifelse( test = is.finite(leaf_m_chloro)
                                    , yes  = leaf_m_chloro
                                    , no   = leaf_a_chloro * SLA * a2m_fac
                                    )#end ifelse
            , leaf_a_chloro = ifelse( test = is.finite(leaf_a_chloro)
                                    , yes  = leaf_a_chloro
                                    , no   = leaf_m_chloro / SLA / a2m_fac
                                    )#end ifelse
            , leaf_m_carot  = ifelse( test = is.finite(leaf_m_carot )
                                    , yes  = leaf_m_carot 
                                    , no   = leaf_a_carot  * SLA * a2m_fac
                                    )#end ifelse
            , leaf_a_carot  = ifelse( test = is.finite(leaf_a_carot )
                                    , yes  = leaf_a_carot 
                                    , no   = leaf_m_carot  / SLA / a2m_fac
                                    )#end ifelse
            )#end mutate



   return(Answer)
}#end function TRY_Harmonise_AreaMassConc
#---~---





#---~---
#     This function fills in stoichiometry relationships based on contents and ratios.
# This will respect the input data, even if they are not consistent.
#
# Input variables:
# x      - tibble object with traits with standard names.
# cn_fac - additional conversion factor between C and N (in case units are inconsistent)
# cp_fac - additional conversion factor between C and P (in case units are inconsistent)
#---~---
TRY_Harmonise_LeafStoichiometry <<- function(x,cn_fac=1.,cp_fac=1.){
   Answer = x %>%
      mutate( leaf_c   = ifelse( test = leaf_c   %gt% 0., yes = leaf_c  , no = NA_real_)
            , leaf_n   = ifelse( test = leaf_n   %gt% 0., yes = leaf_n  , no = NA_real_)
            , leaf_p   = ifelse( test = leaf_p   %gt% 0., yes = leaf_p  , no = NA_real_)
            , leaf_c2n = ifelse( test = leaf_c2n %gt% 0., yes = leaf_c2n, no = NA_real_)
            , leaf_c2p = ifelse( test = leaf_c2p %gt% 0., yes = leaf_c2p, no = NA_real_)
            , leaf_n2p = ifelse( test = leaf_c2p %gt% 0., yes = leaf_c2p, no = NA_real_)
            , leaf_c   = ifelse( test = is.finite(leaf_c)
                               , yes  = leaf_c
                               , no   = 1./cn_fac*leaf_c2n*leaf_n
                               )#end ifelse
            , leaf_c   = ifelse( test = is.finite(leaf_c)
                               , yes  = leaf_c
                               , no   = 1./cp_fac*leaf_c2p*leaf_p
                               )#end ifelse
            , leaf_n   = ifelse( test = is.finite(leaf_n)
                               , yes  = leaf_n
                               , no   = cn_fac*leaf_c  /leaf_c2n
                               )#end ifelse
            , leaf_n   = ifelse( test = is.finite(leaf_n)
                               , yes  = leaf_n
                               , no   = cn_fac/cp_fac*leaf_n2p*leaf_p
                               )#end ifelse
            , leaf_p   = ifelse( test = is.finite(leaf_p)
                               , yes  = leaf_p
                               , no   = cp_fac*leaf_c  /leaf_c2p
                               )#end ifelse
            , leaf_p   = ifelse( test = is.finite(leaf_p)
                               , yes  = leaf_p
                               , no   = cp_fac/cn_fac*leaf_n  /leaf_n2p
                               )#end ifelse
            , leaf_c2n = ifelse( test = is.finite(leaf_c2n)
                               , yes  = leaf_c2n
                               , no   = cn_fac*leaf_c  /leaf_n
                               )#end ifelse
            , leaf_c2p = ifelse( test = is.finite(leaf_c2p)
                               , yes  = leaf_c2p
                               , no   = cp_fac*leaf_c  /leaf_p
                               )#end ifelse
            , leaf_n2p = ifelse( test = is.finite(leaf_n2p)
                               , yes  = leaf_n2p
                               , no   = cp_fac/cn_fac*leaf_n  /leaf_p
                               )#end ifelse
            , leaf_c2n = ifelse( test = is.finite(leaf_c2n)
                               , yes  = leaf_c2n
                               , no   = leaf_c2p/leaf_n2p
                               )#end ifelse
            , leaf_c2p = ifelse( test = is.finite(leaf_c2p)
                               , yes  = leaf_c2p
                               , no   = leaf_c2n*leaf_n2p
                               )#end ifelse
            , leaf_n2p = ifelse( test = is.finite(leaf_n2p)
                               , yes  = leaf_n2p
                               , no   = leaf_c2p/leaf_c2n
                               )#end ifelse
            )#end mutate
   return(Answer)
}#end function TRY_Harmonise_LeafStoichiometry
#---~---





#---~---
#     This function fills in leaf area and leaf biomass when SLA and
# one of the bases are provided. This will respect the input data, even if they are
# not consistent.
#
# Input variables:
# x      - tibble object with traits with standard names.
# td_fac - additional conversion factor (in case units are inconsistent)
#---~---
TRY_Harmonise_LeafBioArea <<- function(x,am_fac=1.){
   Answer = x %>%
      mutate( bleaf     = ifelse(test=is.finite(bleaf    ),yes=bleaf    ,no=am_fac*leaf_area/SLA)
            , leaf_area = ifelse(test=is.finite(leaf_area),yes=leaf_area,no=bleaf*SLA/am_fac    )
            )#end mutate
   return(Answer)
}#end function TRY_Harmonise_LeafBioArea
#---~---





#---~---
#     This function fills in crown area and crown diameter when both are included in the
# trait search but only one of them is provided for an observation. This will respect the 
# input data, even if they are not consistent.
#
# Input variables:
# x         - tibble object with traits with standard names.
# cd2ca_fac - additional conversion factor (in case units are inconsistent)
#---~---
TRY_Harmonise_CrownSize <<- function(x,cd2ca_fac=1.){
   Answer = x %>%
      mutate( crown_area = ifelse( test = is.finite(crown_area)
                                 , yes  = crown_area
                                 , no   = 0.25 * pi * crown_diam^2 * cd2ca_fac
                                 )#end ifelse
            , crown_diam = ifelse( test = is.finite(crown_diam)
                                 , yes  = crown_diam
                                 , no   = 2. * sqrt( crown_area / pi / cd2ca_fac )
                                 )#end ifelse
            )#end mutate
   return(Answer)
}#end function TRY_Harmonise_CrownSize
#---~---





#---~---
#     This function fills in leaf texture (toughness) measurements when leaf thickness
# is provided (or finds leaf thickness when both are provided).
#
# Input variables:
# x        - tibble object with traits with standard names.
# thtx_fac - additional conversion factor between leaf thickness and texture
#            (in case units are inconsistent)
#---~---
TRY_Harmonise_LeafTexture <<- function(x,thtx_fac=1.){
   Answer = x %>%
      mutate( leaf_thick   = ifelse(test=leaf_thick   %gt% 0.,yes=leaf_thick  ,no=NA_real_)
            , leaf_f_tear  = ifelse(test=leaf_f_tear  %gt% 0.,yes=leaf_f_tear ,no=NA_real_)
            , leaf_f_punct = ifelse(test=leaf_f_punct %gt% 0.,yes=leaf_f_punct,no=NA_real_)
            , leaf_t_tear  = ifelse(test=leaf_t_tear  %gt% 0.,yes=leaf_t_tear ,no=NA_real_)
            , leaf_t_punct = ifelse(test=leaf_t_punct %gt% 0.,yes=leaf_t_punct,no=NA_real_)
            , leaf_f_tear  = ifelse( test = is.finite(leaf_f_tear)
                                   , yes  = leaf_f_tear
                                   , no   = leaf_t_tear   * leaf_thick   * thtx_fac
                                   )#end ifelse
            , leaf_f_punct = ifelse( test = is.finite(leaf_f_punct)
                                   , yes  = leaf_f_punct
                                   , no   = leaf_t_punct  * leaf_thick   * thtx_fac
                                   )#end ifelse
            , leaf_t_tear  = ifelse( test = is.finite(leaf_t_tear)
                                   , yes  = leaf_t_tear
                                   , no   = leaf_f_tear   / leaf_thick   / thtx_fac
                                   )#end ifelse
            , leaf_t_punct = ifelse( test = is.finite(leaf_t_punct)
                                   , yes  = leaf_t_punct
                                   , no   = leaf_f_punct  / leaf_thick   / thtx_fac
                                   )#end ifelse
            , leaf_thick   = ifelse( test = is.finite(leaf_thick)
                                   , yes  = leaf_thick
                                   , no   = leaf_f_tear  / leaf_t_tear   / thtx_fac
                                   )#end ifelse
            , leaf_thick   = ifelse( test = is.finite(leaf_thick)
                                   , yes  = leaf_thick
                                   , no   = leaf_f_punct / leaf_t_punct / thtx_fac
                                   )#end ifelse
            )#end mutate
   return(Answer)
}#end function TRY_Harmonise_LeafTexture
#---~---
