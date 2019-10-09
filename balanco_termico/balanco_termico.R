library(ggplot2)

setwd("/home/marcelo/Downloads/")

## FUNCOES ----

sumcols = function(df, col_list, output_type){
  first = TRUE
  for(c in col_list){
    if(first == TRUE){
      main_col = df[,grepl(paste0(c,output_type),colnames(df))]
      first = FALSE
    }else{
      main_col = main_col + df[,grepl(paste0(c,output_type),colnames(df))]
    }
  }
  return(main_col)
}

df.balanco <- function(df, zt, internal_walls, external_walls, floor, roof, windows, doors, energy_unit='kJ'){
  
  if(energy_unit == 'J'){
    divisor = 1
  }else{
    if(energy_unit == 'kJ'){
      divisor = 1000
    }else{
      if(energy_unit == 'Wh'){
        divisor = 3600
      }else{
        if(energy_unit == 'kWh'){
          divisor = 3600000
        }
      }
    }
  }
  
  df_balanco = data.frame(
    mes = as.numeric(substr(df$Date.Time, 2,3)),
    dia = as.numeric(substr(df$Date.Time, 5,6)),
    hora = as.POSIXct(strptime(df$Date.Time," %m/%d  %H:%M:%S")),
    internal_gains = df[,grepl(paste0(zt,'.Zone.Total.Internal.Convective.Heating.Energy..J..'),colnames(df))]/divisor, 
    windows = -sumcols(df, windows, '.Surface.Inside.Face.Convection.Heat.Gain.Energy..J..')/divisor,
    internal_walls = -sumcols(df, internal_walls, '.Surface.Inside.Face.Convection.Heat.Gain.Energy..J..')/divisor,
    external_walls = -sumcols(df, external_walls, '.Surface.Inside.Face.Convection.Heat.Gain.Energy..J..')/divisor,
    doors = -sumcols(df, doors, '.Surface.Inside.Face.Convection.Heat.Gain.Energy..J..')/divisor, 
    floor = -sumcols(df, floor, '.Surface.Inside.Face.Convection.Heat.Gain.Energy..J..')/divisor,
    roof = -sumcols(df, roof, '.Surface.Inside.Face.Convection.Heat.Gain.Energy..J..')/divisor
  )
  
  if(any(grepl('Zone.Air.System.Sensible',colnames(df)))){ 
    if((sum(df[,grepl(paste0(zt,'.Zone.Air.System.Sensible.Cooling.Energy..J.'),colnames(df))]) != 0)){
      df_balanco$cooling = -df[,grepl(paste0(zt,'.Zone.Air.System.Sensible.Cooling.Energy..J.'),colnames(df))]/divisor
   }
    if((sum(df[,grepl(paste0(zt,'.Zone.Air.System.Sensible.Heating.Energy..J.'),colnames(df))])) != 0){
      df_balanco$heating = df[,grepl(paste0(zt,'.Zone.Air.System.Sensible.Heating.Energy..J..'),colnames(df))]/divisor
   }
  
  if(any(grepl('Infiltration',colnames(df)))){
    df_balanco$vn_loss = -df[,grepl(paste0(zt,'.AFN.Zone.Infiltration.Sensible.Heat.Loss.Energy..J..'),colnames(df))]/divisor
    df_balanco$vn_gain = df[,grepl(paste0(zt,'.AFN.Zone.Infiltration.Sensible.Heat.Gain.Energy..J..'),colnames(df))]/divisor
  }
  
  return(df_balanco)
}

day.to.plot = function(df, day){  # insert c([month], [day])
  
  df$mes = as.numeric(substr(df$Date.Time, 2,3))
  df$dia = as.numeric(substr(df$Date.Time, 5,6))
  # df$hora = as.POSIXct(strptime(df$Date.Time," %m/%d  %H:%M:%S"))
  timesteps_x_day = nrow(df)/365
  
  if(day == 'max'){
    plot_day <- which.max(df[,grepl('Environment.Site.Outdoor.Air.Drybulb.Temperature',colnames(df))])
  }
  else{
    if(day == 'min'){
      plot_day <- which.min(df[,grepl('Environment.Site.Outdoor.Air.Drybulb.Temperature',colnames(df))])
    }
    else{
      plot_day = which(df$dia == day[2] & df$mes == day[1])[1]+timesteps_x_day/2
    }
  }
  
  df_to_plot = df[(plot_day-timesteps_x_day):(plot_day+timesteps_x_day), ]
  return (df_to_plot)
}

plot.day = function(df, file_name, width = 32, height = 18, units = "cm", dpi = 500, energy_unit='kJ', title=TRUE){
  
  if(any(grepl('vn',colnames(df)))){
    df$vn = df$vn_loss + df$vn_gain
  }
  
  plot = ggplot(df) + 
    geom_line(aes(x = hora, y=doors, colour = "Portas")) + 
    geom_line(aes(x = hora, y=windows, colour = "Janelas"))+
    geom_line(aes(x = hora, y=internal_walls, colour = "Paredes Internas"))+
    geom_line(aes(x = hora, y=external_walls, colour = "Paredes Externas"))+
    geom_line(aes(x = hora, y=roof, colour = "Cobertura"))+
    geom_line(aes(x = hora, y=floor, colour = "Piso"))+
    geom_line(aes(x = hora, y=internal_gains, colour = "Cargas Internas"))+
    labs(x="Hora", y=paste0("Calor (",energy_unit,')'))+
    theme(axis.text.x=element_text(angle=90, vjust = 0.5, size=9),
          legend.position = "bottom",
          axis.title.x = element_text(size=12, vjust = 0.75),
          axis.title.y = element_text(size=12, vjust = 2),
          axis.text.y = element_text(size=10),
          legend.title = element_blank(),
          legend.text = element_text(size=12, hjust = 0.5),
          strip.text = element_text(size=13)) +
    scale_x_datetime(date_breaks='6 hours',date_labels='%H:%M',expand = c(0, 0))
  
  if(any(grepl('cooling',colnames(df)))){
    plot = plot +
      geom_line(aes(x = hora, y=cooling, colour = "Resfriamento"))+
      geom_line(aes(x = hora, y=heating, colour = "Aquecimento"))
  }
  if(any(grepl('vn',colnames(df)))){
    plot = plot +
      geom_line(aes(x = hora, y=vn, colour = "Ventilação Natural"))
  }
  if(title){
    plot = plot +
      ggtitle('Balanço Térmico')
  }
  show(plot)
  ggsave(paste0(file_name,'.png'),plot=plot, width=width, height=height, units=units, dpi=dpi)
}

plot.period = function(df, file_name, width = 32, height = 18, units = "cm", dpi = 500, energy_unit='kJ', title=TRUE, period=c(c(1,1),c(12,31))){
  dfplot = data.frame('ganho_perda'=colnames(df)[4:length(colnames(df))], 'valor_kwh' = apply(df_balanco[,4:length(colnames(df_balanco))], 2, sum))
  plot = ggplot(dfplot, aes(x=ganho_perda, y=valor_kwh, fill=ganho_perda)) +
    geom_bar(stat="identity")+
    labs(x="Ganhos e Perdas", y=paste0('Ganhos e Perdas no Ar da Zona (',energy_unit,')'))+
    theme(legend.title = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_text(size=12, vjust = 2),
          axis.text.y = element_text(size=10),
          legend.text = element_text(size=10)
          #strip.text.x = element_text(size=13)
    )#+
    # scale_fill_manual(values=c("#FFFF99","#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#FF7F00","#CAB2D6","#6A3D9A"))#+  # esse eh o Paired
    # ylim(-1500,1500)
  if(title){
    plot = plot +
      ggtitle('Balanço Térmico Anual')
  }
  show(plot)
  ggsave(paste0(file_name,'.png'),plot=plot, width=width, height=height, units=units, dpi=dpi)
}

## LISTANDO O ARQUIVOS  ----

file_list = list.files(pattern="*.csv")
file_list = file_list[!grepl('Table|table', file_list)]  # todos os csv menos o table
print(file_list)

## TESTANDO AS FUNCOES  ----

df = read.csv("construction_01_comp_02_ocup_01.csv")
# df = read.csv("construction_01_comp_02_ocup_01 (1).csv")
# df = read.csv("HVAC_setpoint23.csv")
# df = read.csv("HVACeVN_setpoint23.csv")
df2 = read.csv("eplusout.csv")
df$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..Hourly. = df2$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..Hourly.

colnames(df)
zt = 'DORM1'
internal_walls = c('PARINT1_DORM1SALA','PARINT2_DORM1DORM2')
external_walls = c('PAREXT_LESTE_DORM1','PAREXT_SUL_DORM1')
floor = c('PISO_QUARTO1')
roof = c('FORRO_DORM1ATICO')
windows = c('JANQUARTO1_SUL')
doors = c('PORTAINT_DORM1SALA')

df_balanco = df.balanco(df, zt, internal_walls, external_walls, floor, roof, windows, doors)

dfplot = day.to.plot(df, c(4,20))  # 'min')  # 
dfplot = df.balanco(dfplot, zt, internal_walls, external_walls, floor, roof, windows, doors)
plot.day(dfplot, '~/Desktop/plot_teste_min')

dfplot = day.to.plot(df, c(4,20))  # 'min')  # 
dfplot = df.balanco(dfplot, zt, internal_walls, external_walls, floor, roof, windows, doors)
plot.day(dfplot, '~/Desktop/plot_teste_min')

dfplot = day.to.plot(df, c(4,20))  # 'min')  # 
dfplot = df.balanco(dfplot, zt, internal_walls, external_walls, floor, roof, windows, doors)
plot.day(dfplot, '~/Desktop/plot_teste_min')

dfplot = day.to.plot(df, c(4,20))  # 'min')  # 
dfplot = df.balanco(dfplot, zt, internal_walls, external_walls, floor, roof, windows, doors)
plot.day(dfplot, '~/Desktop/plot_teste_min', title = FALSE)

plot.period(df = df_balanco, file_name = '~/Desktop/teste2')
