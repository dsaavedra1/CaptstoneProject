library(dplyr)
library(xts)
library(stringr)

data<-read.csv('data3.csv',header = T)
class(data)
dim(data)

summary(data)
data$Peticion<-NULL


##### Cleaning Sector

data$Sector<-str_replace_all(data$Sector,'[:punct:]|[:digit:]','')
data$Sector<-str_to_title(data$Sector)
data$Sector<-str_trim(data$Sector,side ='both')

SectorEd<-str_detect(data$Sector,'Gestion Juridica')
data$Sector[SectorEd]<-'Gestion Juridica'

SeguridadConvi<-str_detect(data$Sector,'Seguridad Convivencia')
data$Sector[SeguridadConvi]<-'Seguridad Convivencia Y Justicia'

data$Sector<-str_replace_all(data$Sector,
                             'Seguridad  Convivencia Y  Justicia',
                             'Seguridad Convivencia Y Justicia')




######## Cleaning Entidad

data$Entidad<-str_replace_all(data$Entidad,'[:punct:]|[:digit:]','')
data$Entidad<-str_to_title(data$Entidad)
data$Entidad<-str_trim(data$Entidad,side ='both')


AlcaLocal<-str_detect(data$Entidad,'Alcaldia Local')
data$Entidad[AlcaLocal]<-'Alcaldia Local'

HopiLo<-str_detect(data$Entidad,'Hospital')
data$Entidad[HopiLo]<-'Hospital Localidad'

Eru<-str_detect(data$Entidad,'Eru  Empresa')
data$Entidad[Eru]<-'Empresa De Renovacion y Desarrollo Urbano'

FVIGI<-str_detect(data$Entidad,'Fondo Vigilancia')
data$Entidad[FVIGI]<-'Fondo Vigilancia'

data$Entidad<-str_replace(data$Entidad,
                          "Unidad Administrativa Especial Cuerpo Oficial Bomberos Bogota",
                          "Cuerpo Official de Bomberos")

AccUmna<-str_detect(data$Entidad,'Accion Comunal')
data$Entidad[AccUmna]<-'Accion Comunal'

data$Entidad<-str_replace(data$Entidad,
                          "Idpac",
                          "Accion Comunal")
data$Entidad<-str_replace(data$Entidad,
                          "Dadep  Defensoria Del Espacio Publico",
                          "Defensoria Del Espacio Publico")

Idrd<-str_detect(data$Entidad,'Idrd')
data$Entidad[Idrd]<-'Recreacion y Deporte'

IdpcEntidad<-str_detect(data$Entidad,'Idpc')
data$Entidad[IdpcEntidad]<-'Patrimonio Cultural'

IdpybaEntidad<-str_detect(data$Entidad,'Idpyba')
data$Entidad[IdpybaEntidad]<-'Proteccion y Bienestar Animal'


IdtEnti<-str_detect(data$Entidad,'Idt')
data$Entidad[IdtEnti]<-'Instituto Distrital de Turismo'

IduEntidad<-str_detect(data$Entidad,'Idu')
data$Entidad[IduEntidad]<-'Instituto de Desarrollo Urbano'

data$Entidad<-str_replace(data$Entidad,
                          "Secretaria De Seguridad Nueva",
                          "Secretaria De Seguridad")

data$Entidad<-str_replace(data$Entidad,
                          "Secretaria Juridica Nueva",
                          "Secretaria Juridica")

SimEnti<-str_detect(data$Entidad,'Sim')
data$Entidad[SimEnti]<-'Servicios Para La Movilidad'

data$Entidad<-str_replace(data$Entidad,
                          "Subred Centro Oriente Nueva",
                          "Subred Centro Oriente")

data$Entidad<-str_replace(data$Entidad,
                          "Subred Norte Nueva",
                          "Subred Norte")

data$Entidad<-str_replace(data$Entidad,
                          "Subred Sur Nueva",
                          "Subred Sur")

data$Entidad<-str_replace(data$Entidad,
                          "Subred Sur Occidente Nueva",
                          "Subred Sur Occidente")

UaespEnti<-str_detect(data$Entidad,'Uaesp')
data$Entidad[UaespEnti]<-'Unidad de Servicios Publicos'



data$Entidad<-str_replace(data$Entidad,
                          "Ofb  Orquesta Filarmonica",
                          "Orquesta Filarmonica")



########### Cleaning Dependencia

data$Dependencia<-str_to_lower(data$Dependencia)
data$Dependencia<-str_to_title(data$Dependencia)

data$Dependencia<-str_replace_all(data$Dependencia,'[:punct:]','')
data$Dependencia<-str_replace_all(data$Dependencia,'[:digit:]','')



########### TEMA

data$Tema<-str_to_lower(data$Tema)
data$Tema<-str_to_title(data$Tema)

data$Tema<-str_replace_all(data$Tema,'[:punct:]','')
data$Tema<-str_replace_all(data$Tema,'[:digit:]','')
data$Tema<-str_trim(data$Tema)

colegio<-str_detect(data$Tema,'Colegio')
data$Tema[colegio]<-'Colegio'

AlcaldiaTema<-str_detect(data$Tema,'Alcaldia Local')
data$Tema[AlcaldiaTema]<-'Alcaldia Local'

AreaAtencion<-str_detect(data$Tema,'Area De Atencion')
data$Tema[AreaAtencion]<-'Area de Atencion'

data$Tema<-str_replace(data$Tema,'\\â',"")

data$Tema<-str_replace(data$Tema,
                       'Atencion A La Ciudadania',
                       "Atencion al Ciudadano")
data$Tema<-str_replace(data$Tema,
                       'Atencion Al Ciudadano',
                       "Atencion al Ciudadano")
data$Tema<-str_replace(data$Tema,
                       'Atencion al Ciudadano Tramite',
                       "Atencion al Ciudadano")
data$Tema<-str_replace(data$Tema,
                       'Atencion Al Cliente',
                       "Atencion al Ciudadano")
data$Tema<-str_replace(data$Tema,
                       'Atencion Al Usuario',
                       "Atencion al Ciudadano")

Colegio<-str_detect(data$Tema,'Colegio')
data$Tema[Colegio]<-'Colegio'

Comisaria<-str_detect(data$Tema,'Comisaria De Familia')
data$Tema[Comisaria]<-'Comisaria de Familia'

Control<-str_detect(data$Tema,'Control Politico')
data$Tema[Control]<-'Control Politico Social Fiscal Preventivo'

DireccionLo<-str_detect(data$Tema,'Direccion Local De')
data$Tema[DireccionLo]<-'Direccion Local de Educacion'

GestionJuri<-str_detect(data$Tema,'Gestion Juridica')
data$Tema[GestionJuri]<-'Gestion Juridica Legal de Regulacion'

Impues<-str_detect(data$Tema,'Impuestos')
data$Tema[Impues]<-'Impuestos Tasas y Contribuciones'

Usuar<-str_detect(data$Tema,'Usuario Atencion')
data$Tema[Usuar]<-'Usuario Atencion al Ciudadano'

data$Tema<-str_replace_all(data$Tema,'\\Â','')
data$Tema<-str_trim(data$Tema)

UnidadSer<-str_detect(data$Tema,'Unidad De Servicios De Salud ')
data$Tema[UnidadSer]<-'Unidad De Servicios De Salud'

Subdire<-str_detect(data$Tema,'Subdireccion Local')
data$Tema[Subdire]<-'Subdireccion Local'

SubdirAdm<-str_detect(data$Tema,'Subdireccion Administrativa')
data$Tema[SubdirAdm]<-'Subdireccion Administrativa'

ServicoA<-str_detect(data$Tema,'Servicio Al Ciudadano  Tramite')
data$Tema[ServicoA]<-'Servicio Al Ciudadano'

data$Tema<-str_replace(data$Tema,
                       'Servicio A La Ciudadania',
                       "Servicio Al Ciudadano")
data$Tema<-str_replace(data$Tema,
                       'Servicio Al Cliente',
                       "Servicio Al Ciudadano")



##########  Subtemas

data$Subtema<-str_replace_all(data$Subtema,'[:punct:]|[:digit:]','')
data$Subtema<-str_to_title(data$Subtema)
data$Subtema<-str_trim(data$Subtema,side ='both')

data$Subtema<-str_replace_all(data$Subtema,'\\Â','')
data$Subtema<-str_replace_all(data$Subtema,'\\â','')
data$Subtema<-str_replace_all(data$Subtema,'\\ã','')
data$Subtema<-str_replace_all(data$Subtema,'\\€','')


#########  Localidad 

data$Localidad<-str_replace_all(data$Localidad,'[:digit:]|[:punct:]','')
data$Localidad<-str_to_title(data$Localidad)


########### Cleaning Canal 

data$Canal<-str_replace(data$Canal,
                        "MOVIL",
                        "TELEFONO")

data$Canal<-str_replace(data$Canal,
                        "APP-APLICACION TELEFONO",
                        "WEB")

data$Canal<-str_replace(data$Canal,
                        "REDES SOCIALES",
                        "E-MAIL")

data$Canal<-str_replace_all(data$Canal,'[:digit:]','')
data$Canal<-str_to_title(data$Canal)



##########  Tipo de Peticion

data$TipoPeticion<-str_replace_all(data$TipoPeticion,
                                   '[:punct:]|[:digit:]','')
data$TipoPeticion<-str_to_title(data$TipoPeticion)
data$TipoPeticion<-str_trim(data$TipoPeticion,side ='both')


##########  Estado Inicial  

data$EstadoInicial<-str_replace_all(data$EstadoInicial,
                                    '[:punct:]|[:digit:]','')
data$EstadoInicial<-str_to_title(data$EstadoInicial)
data$EstadoInicial<-str_trim(data$EstadoInicial,side ='both')


########## Estado Final 

data$EstadoFinal<-str_replace_all(data$EstadoFinal,
                                    '[:punct:]|[:digit:]','')
data$EstadoFinal<-str_to_title(data$EstadoFinal)
data$EstadoFinal<-str_trim(data$EstadoFinal,side ='both')














####################               #######################
                  # VISUALIZATION #
####################               #######################
library(ggplot2)
library(ggthemes)

ggplot(data, aes(Sector))+geom_bar(fill="#00526D")+coord_flip()+
  labs(title="Sector Frequency Distribution ") + labs(x="Sector")+
  labs(y="Count")+ theme_economist() + scale_colour_economist()

ggplot(data, aes(Entidad))+geom_bar(fill="#00526D")+
  labs(title="Entidad Frequency Distribution ") + labs(x="Entidad")+
  labs(y="Count")+theme_economist() + scale_colour_economist()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data, aes(Dependencia))+geom_bar(fill="#00526D")+
  labs(title="Dependencia Frequency Distribution ") + labs(x="Dependencia")+
  labs(y="Count")+theme_economist() + scale_colour_economist()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data, aes(Tema))+geom_bar(fill="#00526D")+
  labs(title="Tema Frequency Distribution ") + labs(x="Tema")+
  labs(y="Count")+theme_economist() + scale_colour_economist()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data, aes(Canal)) +geom_bar(fill="#00526D")+coord_flip()+
  labs(title="Comunication Channels Frecuency Distribution ") + labs(x="Canal")+
  labs(y="Count")+theme_economist() + scale_colour_economist()



ggplot(data, aes(TipoPeticion))+geom_bar(fill="#00526D")+
  labs(title="Tipo de Peticion  Frecuency Distribution ") + labs(x="Tipo de Petition")+
  labs(y="Count")+theme_economist() + scale_colour_economist()+
  coord_flip()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data, aes(EstadoInicial))+geom_bar(fill="#00526D")+
  labs(title="Estado Inicial  Frecuency Distribution ") + labs(x="Estado Inicial")+
  labs(y="Count")+theme_economist() + scale_colour_economist()+
  coord_flip()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(data, aes(EstadoFinal))+geom_bar(fill="#00526D")+
  labs(title="Estado Final Frecuency Distribution") + labs(x="Estado Final")+
  labs(y="Count")+theme_economist() + scale_colour_economist()+
  coord_flip()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#library(gridExtra)
#grid.arrange(p2, p5, ncol= 2)

#grid.arrange(p6, p7, ncol= 2)

str(data)
data$Sector<-factor(data$Sector)
data$Entidad<-factor(data$Entidad)
data$Dependencia<-factor(data$Dependencia)
data$Tema<-factor(data$Tema)
data$Subtema<-factor(data$Subtema)
data$Localidad<-factor(data$Localidad)
data$Canal<-factor(data$Canal)
data$TipoPeticion<-factor(data$TipoPeticion)
data$EstadoInicial<-factor(data$EstadoInicial)
data$EstadoFinal<-factor(data$EstadoFinal)


#################                        ################
                  # Data by attribute  #

# Filter the most important Sector 
nlevels(data$Sector)
data %>% 
  group_by(Sector) %>%
  summarise(no_rows = length(Sector))%>%
  arrange(desc(no_rows))


# Filter the most important institution 
nlevels(data$Entidad)
data %>% 
  group_by(Entidad) %>%
  summarise(no_rows = length(Entidad))%>%
  arrange(desc(no_rows))


# Filter the most important institution 
nlevels(data$Dependencia)
data %>% 
  group_by(Dependencia) %>%
  summarise(no_rows = length(Dependencia))%>%
  arrange(desc(no_rows))

# Filter the most important institution 
nlevels(data$Tema)
data %>% 
  group_by(Tema) %>%
  summarise(no_rows = length(Tema))%>%
  arrange(desc(no_rows))

# Filter the most important institution 
nlevels(data$Localidad)
Localidad<-data %>% 
  group_by(Localidad) %>%
  summarise(no_rows = length(Localidad))%>%
  arrange(desc(no_rows))

# Filter the most important institution 
nlevels(data$Canal)
data %>% 
  group_by(Canal) %>%
  summarise(no_rows = length(Canal))%>%
  arrange(desc(no_rows))

# Filter the most important Tipo de Peticion
nlevels(data$TipoPeticion)
data %>% 
  group_by(TipoPeticion) %>%
  summarise(no_rows = length(TipoPeticion))%>%
  arrange(desc(no_rows))


# Filter the most important Tipo de Peticion
nlevels(data$EstadoInicial)
data %>% 
  group_by(EstadoInicial) %>%
  summarise(no_rows = length(EstadoInicial))%>%
  arrange(desc(no_rows))


# Filter the most important Tipo de Peticion
nlevels(data$EstadoFinal)
data %>% 
  group_by(EstadoFinal) %>%
  summarise(no_rows = length(EstadoFinal))%>%
  arrange(desc(no_rows))





################                 ##################
               # FIX TIME SERIES #
################                 #################

library(zoo)
library('lubridate')
library(DT)

data$FechaIngreso<-as.Date(data$FechaIngreso)
data$FechaVencimiento<-as.Date(data$FechaVencimiento)
data$FechaFinalizacion<-as.Date(data$FechaFinalizacion)
data$FechaCierre<-as.Date(data$FechaCierre)



######### FECHA DE INGRESO FOR DAYS TO YEAR 

data$FiDay<- factor(day(data$FechaIngreso))
data$FiMonth <- factor(month(data$FechaIngreso, label = TRUE))
data$FiDayWeek <- factor(wday(data$FechaIngreso, label = TRUE))
data$FiYear <- factor(year(data$FechaIngreso))

# Visualize days
day_data <- data %>%
  group_by(FiDay) %>%
  summarize(Total = n()) 

datatable(day_data)

ggplot(day_data,aes(FiDay,Total))+geom_bar(stat = 'identity',fill="#00526D")+
  labs(title="Fecha de Ingreso by Month Date") + 
  labs(x="Localidad")+labs(y="Mean Days")+theme_economist() +
  scale_colour_economist()


# Visualize months
month_data <- data %>%
  group_by(FiMonth) %>%
  summarize(Total = n()) 
# To build datatable()

datatable(month_data)

ggplot(month_data, aes(FiMonth,Total))+geom_bar(stat = 'identity',fill="#00526D")+
  labs(title="Fecha de Ingreso by Month") + labs(x="Month")+
  labs(y="Total")+theme_economist() + scale_colour_economist()

# Visualize Day of the Week
dayofweek_data <- data %>%
  group_by(FiDayWeek) %>%
  summarize(Total = n()) 

datatable(dayofweek_data)

ggplot(dayofweek_data, aes(FiDayWeek,Total))+geom_bar(stat = 'identity',fill="#00526D")+
  labs(title="Fecha de Ingreso by Month Week Day ") + labs(x="Day of the Week")+
  labs(y="Total")+theme_economist() + scale_colour_economist()


# Visualize Year
year_data <- data %>%
  group_by(FiYear) %>%
  summarize(Total = n()) 

datatable(year_data)

ggplot(year_data, aes(FiYear,Total))+geom_bar(stat = 'identity',fill="#00526D")+
  labs(title="Fecha de Ingreso by Year ") + labs(x="Year")+
  labs(y="Total")+theme_economist() + scale_colour_economist()

# Year and month Heat MAP
month_year_group<-data %>%
  group_by(FiYear, FiMonth) %>%
  summarize(Total = n())

ggplot(month_year_group, aes(FiMonth, FiYear, fill = Total)) +
  geom_tile(color = "white") +ggtitle("Heat Map by Month and Year")+
  theme(plot.title = element_text(hjust = 0.5))








########## FECHA DE VENCIMIENTO 

library('lubridate')
data$dayFv<- factor(day(data$FechaVencimiento))
data$monthFv <- factor(month(data$FechaVencimiento, label = TRUE))
data$yearFv <- factor(year(data$FechaVencimiento))
data$dayofweekFv <- factor(wday(data$FechaVencimiento, label = TRUE))

# Visualize days
day_dataFv <- data %>%
  group_by(dayFv) %>%
  summarize(Total = n()) 

datatable(day_dataFv)

ggplot(day_dataFv, aes(dayFv,Total))+geom_bar(stat = 'identity',fill="#00526D")+
  labs(title="Fecha de Vencimiento by Day") + labs(x="Day ")+
  labs(y="Total")+theme_economist() + scale_colour_economist()

# Visualize months
month_dataFv <- data %>%
  group_by(monthFv) %>%
  summarize(Total = n()) 

datatable(month_dataFv)

ggplot(month_dataFv, aes(monthFv,Total))+geom_bar(stat = 'identity',fill="#00526D")+
  labs(title="Fecha de Vencimiento by Month") + labs(x="Month ")+
  labs(y="Total")+theme_economist() + scale_colour_economist()


# Visualize Day of Week
dayofweek_dataFv <- data %>%
  group_by(dayofweekFv) %>%
  summarize(Total = n()) 

datatable(dayofweek_dataFv)

ggplot(dayofweek_dataFv, aes(dayofweekFv,Total))+geom_bar(stat = 'identity',fill="#00526D")+
  labs(title="Fecha de Vencimiento by Week Day ") + labs(x=" Week Day ")+
  labs(y="Total")+theme_economist() + scale_colour_economist()

# Visualize Year
year_dataFv <- data %>%
  group_by(yearFv) %>%
  summarize(Total = n()) 

datatable(year_dataFv)

ggplot(year_dataFv, aes(yearFv,Total))+geom_bar(stat = 'identity',fill="#00526D")+
  labs(title="Fecha de Vencimiento by Year ") + labs(x=" Year ")+
  labs(y="Total")+theme_economist() + scale_colour_economist()


# Year and month Heat MAP
month_year_groupFv<-data %>%
  group_by(yearFv, monthFv) %>%
  summarize(Total = n())

ggplot(month_year_groupFv, aes(monthFv, yearFv, fill = Total)) +
  geom_tile(color = "white") +ggtitle("Heat Map by Month and Year")+
  theme(plot.title = element_text(hjust = 0.5))










########## FECHA DE FINALIZACION 

library('lubridate')
data$dayFF<- factor(day(data$FechaFinalizacion))
data$monthFF <- factor(month(data$FechaFinalizacion, label = TRUE))
data$yearFF <- factor(year(data$FechaFinalizacion))
data$dayofweekFF <- factor(wday(data$FechaFinalizacion, label = TRUE))

# Visualize days
day_dataFF <- data %>%
  group_by(dayFF) %>%
  summarize(Total = n()) 

datatable(day_dataFF)

ggplot(day_dataFF, aes(dayFF,Total))+geom_bar(stat = 'identity',fill="#00526D")+
  labs(title="Fecha de Finalizacion by Day ") + labs(x=" Day ")+
  labs(y="Total")+theme_economist() + scale_colour_economist()

# Visualize months
month_dataFF <- data %>%
  group_by(monthFF) %>%
  summarize(Total = n()) 

datatable(month_dataFF)

ggplot(month_dataFF, aes(monthFF,Total))+geom_bar(stat = 'identity',fill="#00526D")+
  labs(title="Fecha de Finalizacion by Month ") + labs(x=" Month ")+
  labs(y="Total")+theme_economist() + scale_colour_economist()

# Visualize months
dayofweek_dataFF <- data %>%
  group_by(dayofweekFF) %>%
  summarize(Total = n()) 

datatable(dayofweek_dataFF)

ggplot(dayofweek_dataFF, aes(dayofweekFF,Total))+geom_bar(stat = 'identity',fill="#00526D")+
  labs(title="Fecha de Finalizacion by Day Week ") + labs(x=" Day Week ")+
  labs(y="Total")+theme_economist() + scale_colour_economist()

# Visualize Year
year_dataFF <- data %>%
  group_by(yearFF) %>%
  summarize(Total = n()) 

datatable(year_dataFF)

ggplot(year_dataFF, aes(yearFF,Total))+geom_bar(stat = 'identity',fill="#00526D")+
  labs(title="Fecha de Finalizacion by Year") + labs(x=" Year ")+
  labs(y="Total")+theme_economist() + scale_colour_economist()

# Year and month Heat MAP
month_year_groupFF<-data %>%
  group_by(yearFF, monthFF) %>%
  summarize(Total = n())

# The Heat map shows that months previous to the change
# of a new administration speed up the finalizacion of claims
ggplot(month_year_groupFF, aes(monthFF, yearFF, fill = Total)) +
  geom_tile(color = "white") +ggtitle("Heat Map by Month and Year")+
  theme(plot.title = element_text(hjust = 0.5))






###############                      ####################
              # Days to Sove a Claim #
###############                      ####################


# Difference in days between Fecha de Vemiento  Fecha de Finalizacion
data$dayFFFV<-difftime(data$FechaVencimiento,
                       data$FechaFinalizacion, units = "day")


dayFFFVPlot<-data %>% 
  group_by(dayFFFV) %>%
  summarise(no_rows = length(dayFFFV))%>%
  arrange(desc(no_rows))


ggplot(dayFFFVPlot, aes(dayFFFV,no_rows))+geom_bar(stat = 'identity',fill="#00526D")+
  labs(title="Day Frequency Distribution ") + labs(x=" Days ")+
  labs(y="Total")+theme_economist() + scale_colour_economist()

# Filter the mean of days by Localidad
dayFVFFlocaMean<-data %>% 
  group_by(Localidad) %>%
  summarise(no_rows = mean(dayFFFV))%>%
  arrange(desc(no_rows))

dayFVFFlocaMean<- dayFVFFlocaMean[-c(8),] 
dayFVFFlocaMean<- dayFVFFlocaMean[-c(20),] 

names(dayFVFFlocaMean)<-c('Localidad','AverDaySolve')


dayFVFFlocaMean$CodLoc<-c('6','8','5','10','18',
                           '4','9','14','11','19',
                           '20','2','3','1','16',
                           '12','7','15','17','13')

dayFVFFlocaMean$CodLoc<-as.integer(dayFVFFlocaMean$CodLoc)
dayFVFFlocaMean$AverDaySolve<-as.integer(dayFVFFlocaMean$AverDaySolve)

ggplot(dayFVFFlocaMean,aes(Localidad,AverDaySolve))+
  geom_bar(stat = 'identity',fill="#00526D")+coord_flip()+
  labs(title="Average Days To Finalize Claim ") + labs(x="Localidad")+
  labs(y="Mean Days")+ 
  theme_economist() + scale_colour_economist()



#################                #####################
                #  MAPS CREATION #
#################                #####################

#install.packages('pacman')
#pacman::p_load(leaflet, glue, dplyr, sf, tmap, tmaptools, tidycensus, ggmap, htmltools,
#               htmlwidgets)

#install.packages('leaflet')
#install.packages('glue')
#install.packages('dplyr')
#install.packages('sf')
#install.packages('tmap')
#install.packages('tmaptools')
#install.packages('tidycensus')
#install.packages('ggmap')
#install.packages('htmltools')

library(tmap)
library(sf)
Bogolocal<- st_read("localidades/localidades.shp")
Bogolocal$CODIGO_LOC<-as.integer(Bogolocal$CODIGO_LOC)

common_local <- union(Bogolocal$CODIGO_LOC, dayFVFFlocaMean$CodLoc)
length(common_local) == length(Bogolocal$CODIGO_LOC)

BogdayFFFV<- merge(x=Bogolocal, y=dayFVFFlocaMean, by.x=c("CODIGO_LOC"), 
                   by.y=c("CodLoc"), all.y=TRUE)
ttm()
tmap_last()

tm_shape(BogdayFFFV) +
  tm_polygons(col = "AverDaySolve", id = "NOMBRE",palette='Blues',
              border.alpha = 0.6,border.col = 'black', title='Days to Sove')+
  tm_layout(title = 'Days to Solve Inquiries by Localidad')



## Libraries to choose the color of the maps
library(tmaptools)
library(shinyjs)
palette_explorer()

################################################################








data2<-data%>%
  filter(yearFv=='2019')
data2<-data2[,c(1,2,3,4,5,6,7,8,9,10,29)]


#data2<-sample_n(data2, size =300000 , replace = T)
str(data2$dayFFFV)
data2$dayFFFV<-as.numeric(data2$dayFFFV)

# create two variables one with positive values 
# other with negative
data2$dayFFFVN<-data2$dayFFFV
data2$dayFFFVN[data2$dayFFFVN >0]<- 0
data2$dayFFFV[data2$dayFFFV<0] <-0
data2$dayFFFVN<-factor(data2$dayFFFVN)
data2$dayFFFV<-factor(data2$dayFFFV)
plot(data2$dayFFFV)



# create one binary variable 
data2$dayFFFV[data2$dayFFFV >=0]<- 1
data2$dayFFFV[data2$dayFFFV<0] <-0
data2$dayFFFV<-factor(data2$dayFFFV)

write.csv(data2,"dataBinary.csv", row.names = FALSE)




