library(ggplot2)

setwd("/home/marcelo/Downloads/")
# setwd("/home/marcelo/Dropbox/equipe-r/nbr_15575/rodolfo/outputs/")


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
    geom_line(aes(x = hora, y=doors, colour = "Portas",linetype="Portas")) + 
    geom_line(aes(x = hora, y=windows, colour = "Janelas",linetype="Janelas"))+
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

# df = read.csv("~/Downloads/construction_01_comp_02_ocup_01.csv")
# df = read.csv("construction_01_comp_02_ocup_01 (1).csv")
# df = read.csv("HVAC_setpoint23.csv")
# df = read.csv("HVACeVN_setpoint23.csv")
# df = read.csv("~/Dropbox/equipe-r/nbr_15575/rodolfo/outputs/sao_paulo_apm_inter_ac_leve.csv")
# df = read.csv("~/Dropbox/equipe-r/nbr_15575/rodolfo/outputs/sao_paulo_apm_inter_ac_ref.csv")
# df = read.csv("~/Dropbox/equipe-r/nbr_15575/rodolfo/outputs/sao_paulo_apm_inter_vn_leve.csv")
# df = read.csv("~/Dropbox/equipe-r/nbr_15575/rodolfo/outputs/sao_paulo_apm_inter_vn_ref.csv")
df = read.csv("curitiba_pav_cob_leve_vn.csv")  # colunas_df)
df = df[which(is.na(df$CORE.Zone.Total.Internal.Convective.Heating.Energy..J..Hourly.)==FALSE),]


# df2 = read.csv("~/Downloads/construction_01_comp_02_ocup_01.csv")
# df$Q1_M.Zone.Total.Internal.Convective.Heating.Energy..J.. = df2$DORM1.Zone.Total.Internal.Convective.Heating.Energy..J..Hourly.
# df2 = read.csv("~/Downloads/eplusout.csv")
# df$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..Hourly. = df2$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..Hourly.

colnames(df)

colunas_df = c("Date.Time","Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.",
  colnames(df)[grepl('CANTO_SO_SALA',colnames(df))])
# 
# zt = 'DORM1'
# internal_walls = c('PARINT1_DORM1SALA','PARINT2_DORM1DORM2')
# external_walls = c('PAREXT_LESTE_DORM1','PAREXT_SUL_DORM1')
# floor = c('PISO_QUARTO1')
# roof = c('FORRO_DORM1ATICO')
# windows = c('JANQUARTO1_SUL')
# doors = c('PORTAINT_DORM1SALA')

zt = 'Q1_M'
# internal_walls = c('PARL_Q1_M','PARN_Q1_M','PARO1_Q1_M','PARO2_Q1_M')
# external_walls = c('PARS_Q1_M')
# floor = c('PISO_Q1_M')
# roof = c('TETO_Q1_M')
# windows = c('JAN_Q1_M')
# doors = c('POR_Q1_COR_M')

# zt = 'SALACOZ_M'
# internal_walls = c('PARL3_SALACOZ_M','PARN_SALACOZ_M','PARL2_SALACOZ_M','PARL1_SALACOZ_M','PARO1_SALACOZ_M',
#                    'PARO2_SALACOZ_M')
# external_walls = c('PARS_SALACOZ_M')
# floor = c('PISO_SALACOZ_M')
# roof = c('TETO_SALACOZ_M')
# windows = c('JAN_COZ_M','JAN_SALA_M')
# doors = c('POR_SALA_COR_M')

### MOSTREI PARA O RODOLFO -----
zt = 'CANTO_SO_SALA'
internal_walls = c('CANTO_SO_SALA_PAR_L','CANTO_SO_SALA_PAR_N1','CANTO_SO_SALA_PAR_N2','CANTO_SO_SALA_PAR_O1','CANTO_SO_SALA_PAR_O2')
external_walls = c('CANTO_SO_SALA_PAR_S')
floor = c('CANTO_SO_SALA_PISO')
roof = c('CANTO_SO_SALA_COB')
windows = c('CANTO_SO_SALA_JAN_S1','CANTO_SO_SALA_JAN_S2')
doors = c('CANTO_SO_SALA_PORTA_N1','CANTO_SO_SALA_PORTA_N2','CANTO_SO_SALA_PORTA_O1','CANTO_SO_SALA_PORTA_O2')

colnames(df)[grepl('CANTO_SO_SALA',colnames(df))]

df_balanco = df.balanco(df, zt, internal_walls, external_walls, floor, roof, windows, doors,
                        energy_unit='kWh')
plot.period(df_balanco,
            energy_unit='kWh')

dfplot = day.to.plot(df,c(10,18))
dfplot = df.balanco(dfplot, zt, internal_walls, external_walls, floor, roof, windows, doors,
                    energy_unit='kWh')
plot.day(dfplot,energy_unit='kWh')

### MOSTREI PARA O RODOLFO ATEH AQUI -----

dfplot = day.to.plot(df, c(7,2))  # 'min')  # 
dfplot = df.balanco(dfplot, zt, internal_walls, external_walls, floor, roof, windows, doors)
plot.day(dfplot, '~/Desktop/plot_teste_min')

dfplot = day.to.plot(df, 'min')  # 
dfplot = df.balanco(dfplot, zt, internal_walls, external_walls, floor, roof, windows, doors)
plot.day(dfplot, '~/Desktop/plot_teste_min')

dfplot = day.to.plot(df, 'max')  # 
dfplot = df.balanco(dfplot, zt, internal_walls, external_walls, floor, roof, windows, doors)
plot.day(dfplot, '~/Desktop/plot_teste_min')

dfplot = day.to.plot(df, c(4,20))  # 'min')  # 
dfplot = df.balanco(dfplot, zt, internal_walls, external_walls, floor, roof, windows, doors)
plot.day(dfplot, '~/Desktop/plot_teste_min', title = FALSE)

plot.period(df = df_balanco, file_name = '~/Desktop/teste2')

########

plot.day = function(df, file_name, width = 32, height = 18, units = "cm", dpi = 500, energy_unit='kJ', title=TRUE){
  
  if(any(grepl('vn',colnames(df)))){
    df$vn = df$vn_loss + df$vn_gain
  }
  
  # df2 = dfplot
  # df = df2
  dfplot = data.frame(
    'Hora' = rep(df$hora,7),
    'Energia' = c(df$doors, df$windows, df$internal_walls, df$external_walls, df$roof, df$floor, df$internal_gains),
    'Fonte' = c(rep('Portas',nrow(df)),rep('Janelas',nrow(df)),rep('Paredes Internas',nrow(df)),rep('Paredes Externas',nrow(df)),
                rep('Cobertura',nrow(df)),rep('Piso',nrow(df)),rep('Cargas Internas',nrow(df)))
  )
  
  cores = c('Portas' = 'brown',
            'Janelas' = 'lightblue4',
            'Paredes Internas' = 'navajowhite',
            'Paredes Externas' = 'navajowhite4',
            'Cobertura' = 'hotpink',
            'Piso' = 'green4',
            'Cargas Internas' = 'gold'
            )
  
  if(any(grepl('cooling',colnames(df)))){
    if(abs(sum(df$cooling)) > 0){
      dfadd = data.frame(
        'Hora'=df$hora,
        'Energia'=df$cooling,
        'Fonte'=rep('Resfriamento',nrow(df))
      )
      dfplot = rbind(dfplot, dfadd)
      cores = c(cores, 'Resfriamento'='blue4')
    }
  }
  
  if(any(grepl('heating',colnames(df)))){
    if(abs(sum(df$heating)) > 0){
      dfadd = data.frame(
        'Hora'=df$hora,
        'Energia'=df$heating,
        'Fonte'=rep('Aquecimento',nrow(df))
      )
      dfplot = rbind(dfplot, dfadd)
      cores = c(cores, 'Aquecimento'='red2')
    }
  }
  
  if(any(grepl('vn',colnames(df)))){
    if(abs(sum(df$vn)) > 0){
      dfadd = data.frame(
        'Hora'=df$hora,
        'Energia'=df$vn,
        'Fonte'=rep('Ventilação Natural',nrow(df))
      )
      dfplot = rbind(dfplot, dfadd)
      cores = c(cores, 'Ventilação Natural'='skyblue2')
    }
  }
  
  plot = ggplot(dfplot,aes(x=Hora,y=Energia,colour=Fonte))+
    geom_line() +
    scale_color_manual(values = cores) +
    labs(x="Hora", y=paste0("Calor (",energy_unit,')'))+
    theme(axis.text.x=element_text(angle=90, vjust = 0.5, size=9),
          legend.position = "bottom",
          axis.title.x = element_text(size=12, vjust = 0.75),
          axis.title.y = element_text(size=12, vjust = 2),
          axis.text.y = element_text(size=10),
          legend.title = element_blank(),
          legend.text = element_text(size=12, hjust = 0.5),
          strip.text = element_text(size=13)) +
    scale_x_datetime(date_breaks='2 hours',date_labels='%H:%M',expand = c(0, 0))
  
  if(title){
    plot = plot +
      ggtitle('Balanço Térmico')
  }
  show(plot)
  ggsave(paste0(file_name,'.png'),plot=plot, width=width, height=height, units=units, dpi=dpi)
}
dfplot = day.to.plot(df, 'min')  # c(4,20))  # 
dfplot = df.balanco(dfplot, zt, internal_walls, external_walls, floor, roof, windows, doors)
plot.day(dfplot, '~/Desktop/plot_teste_min')

superficies = c('Portas','Janelas','Paredes Internas','Paredes Externas','Cobertura','Piso','Cargas Internas')

dftest = data.frame('hora'=rep(dfplot$hora, 3),'carga'=c(dfplot$doors,dfplot$windows,dfplot$internal_walls),'surface'=c(rep('door',nrow(dfplot)),rep('window',nrow(dfplot)),rep('internal_wall',nrow(dfplot))))

ggplot(dftest, aes(x=hora,y=carga, colour=surface, linetype=surface))+
  geom_line()+
  scale_linetype_manual(values = c(rep("solid", 2), rep("dashed", 1))) +
  scale_color_manual(values = c('window'='red','door'='green','internal_wall'='yellow'))

ggplot(dfplot) + 
  geom_line(aes(x = hora, y=doors, colour = "Portas",linetype="Portas"),lwd=1) + 
  geom_line(aes(x = hora, y=windows, colour = "Janelas",linetype="Janelas"),lwd=1)+
  geom_line(aes(x = hora, y=internal_walls, colour = "Paredes Internas", linetype = "Paredes Internas"),lwd=1)+
  labs(x="Hora", y=paste0("Calor (",'kJ',')'))+
  theme(axis.text.x=element_text(angle=90, vjust = 0.5, size=9),
        legend.position = "bottom",
        axis.title.x = element_text(size=12, vjust = 0.75),
        axis.title.y = element_text(size=12, vjust = 2),
        axis.text.y = element_text(size=10),
        legend.title = element_blank(),
        legend.text = element_text(size=12, hjust = 0.5),
        strip.text = element_text(size=13)) +
  scale_x_datetime(date_breaks='6 hours',date_labels='%H:%M',expand = c(0, 0))

#####
mort3 <- structure(
  list(
    State = structure(
      c(
        8L, 9L, 11L, 12L, 4L, 2L,6L, 13L, 3L, 5L, 7L, 10L, 14L, 15L, 1L, 16L, 8L, 9L, 11L, 12L, 4L, 2L, 6L, 13L, 3L, 5L, 7L, 10L, 14L, 15L, 1L, 16L, 8L, 9L, 
        11L, 12L, 4L, 2L, 6L, 13L, 3L, 5L, 7L, 10L, 14L, 15L, 1L, 16L, 8L, 9L, 11L, 12L, 4L, 2L, 6L, 13L, 3L, 5L, 7L, 10L, 14L, 15L, 
        1L, 16L, 8L, 9L, 11L, 12L, 4L, 2L, 6L, 13L, 3L, 5L, 7L, 10L, 14L, 15L, 1L, 16L, 8L, 9L, 11L, 12L, 4L, 2L, 6L, 13L, 3L, 5L, 
        7L, 10L, 14L, 15L, 1L, 16L, 8L, 9L, 11L, 12L, 4L, 2L, 6L, 13L, 3L, 5L, 7L, 10L, 14L, 15L, 1L, 16L, 8L, 9L, 11L, 12L, 4L, 2L, 
        6L, 13L, 3L, 5L, 7L, 10L, 14L, 15L, 1L, 16L, 8L, 9L, 11L, 12L, 4L, 2L, 6L, 13L, 3L, 5L, 7L, 10L, 14L, 15L, 1L, 16L, 8L, 9L, 
        11L, 12L, 4L, 2L, 6L, 13L, 3L, 5L, 7L, 10L, 14L, 15L, 1L, 16L, 8L, 9L, 11L, 12L, 4L, 2L, 6L, 13L, 3L, 5L, 7L, 10L, 14L, 15L, 
        1L, 16L, 8L, 9L, 11L, 12L, 4L, 2L, 6L, 13L, 3L, 5L, 7L, 10L, 14L, 15L, 1L, 16L, 8L, 9L, 11L, 12L, 4L, 2L, 6L, 13L, 3L, 5L, 
        7L, 10L, 14L, 15L, 1L, 16L
        ), 
      class = "factor", 
      .Label = c(
        "SH","HH", "NI", "HB", "NW", "HE", "RP", "BW", "BY", "SL", "BE", "BB", "MV", "SN", "ST", "TH"
        )
      ), 
    BCmort = c(
      16.5, 16.6, 15, 14.4, 13.5, 17.1, 15.8, 16.3, 18.3, 16.8, 17, 18.1, 13.1, 15.1, 18.8, 13.1, 16.4, 16.1, 15.8, 12.8, 16.3, 19.2, 16.8, 13, 17.9, 17, 19.4, 
      19.4, 13.1, 13.8, 18.1, 13.8, 15.9, 17.3, 17.5, 13.7, 17.4, 17.5, 16.7, 15.5, 18.1, 18, 20.1, 19.1, 11.8, 14.6, 18.2, 13.4, 16.8, 
      17.5, 15.6, 14.1, 13.9, 18.2, 17.1, 15.2, 18.1, 16.6, 19.3, 18.6, 13.1, 14.6, 19.6, 12.4, 16.6, 17.8, 17.5, 14.3, 20.5, 19.2, 19, 
      12.6, 19.5, 17.8, 19.2, 21, 14.4, 13.4, 19.8, 14, 17.5, 18.9, 16.4, 14.7, 17.7, 20.1, 18.5, 14.5, 19.1, 19.2, 20.1, 19.7, 14.2, 
      16.2, 17.9, 12.6, 18, 18.7, 17.7, 16.5, 16.6, 20.3, 18.1, 15.2, 19, 20, 19.8, 21.3, 13.8, 14.8, 20.4, 14.8, 18.2, 18.7, 16.9, 
      16.2, 20.2, 20.4, 18.5, 14, 20.2, 18.7, 20.3, 17.7, 14.4, 14.5, 21.7, 13.7, 18.3, 19.7, 17.8, 16.5, 20.2, 21.7, 18.8, 16.7, 20.4, 
      20, 19.6, 22.9, 15.2, 14.9, 21.7, 14.6, 18.3, 19.7, 17, 16.7, 22.9, 16.2, 19.6, 15.9, 20.3, 19.9, 18.9, 21.8, 14.9, 18, 21.4, 
      16.1, 19.6, 19.2, 19.1, 16.7, 20, 18.2, 20.5, 15.5, 20.5, 21.1, 21.3, 23.8, 15.8, 15.3, 21.3, 15.7, 19.6, 20.3, 19.2, 17.4, 18.1, 
      23.1, 20.6, 16.2, 21.5, 20.3, 21.4, 20.8, 16.1, 15.8, 22.1, 14.5, 20, 20.2, 19, 18.7, 23.1, 21.8, 19.4, 17.4, 20.9, 20.5, 20.4, 
      23.2, 16.3, 17.6, 23.1, 16.5
      ), 
    year = c(
      2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010, 2009, 2009, 2009, 2009, 2009, 2009, 2009, 2009, 2009, 2009, 
      2009, 2009, 2009, 2009, 2009, 2009, 2008, 2008, 2008, 2008, 2008, 2008, 2008, 2008, 2008, 2008, 2008, 2008, 2008, 2008, 2008, 2008, 
      2007, 2007, 2007, 2007, 2007, 2007, 2007, 2007, 2007, 2007, 2007, 2007, 2007, 2007, 2007, 2007, 2006, 2006, 2006, 2006, 2006, 2006, 
      2006, 2006, 2006, 2006, 2006, 2006, 2006, 2006, 2006, 2006, 2005, 2005, 2005, 2005, 2005, 2005, 2005, 2005, 2005, 2005, 2005, 2005, 
      2005, 2005, 2005, 2005, 2004, 2004, 2004, 2004, 2004, 2004, 2004, 2004, 2004, 2004, 2004, 2004, 2004, 2004, 2004, 2004, 2003, 2003, 
      2003, 2003, 2003, 2003, 2003, 2003, 2003, 2003, 2003, 2003, 2003, 2003, 2003, 2003, 2002, 2002, 2002, 2002, 2002, 2002, 2002, 2002, 
      2002, 2002, 2002, 2002, 2002, 2002, 2002, 2002, 2001, 2001, 2001, 2001, 2001, 2001, 2001, 2001, 2001, 2001, 2001, 2001, 2001, 2001, 
      2001, 2001, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 1999, 1999, 1999, 1999, 
      1999, 1999, 1999, 1999, 1999, 1999, 1999, 1999, 1999, 1999, 1999, 1999, 1998, 1998, 1998, 1998, 1998, 1998, 1998, 1998, 1998, 1998, 
      1998, 1998, 1998, 1998, 1998, 1998
      ), 
    eastWest = structure(
      c(
        1L,1L, 2L, 2L, 1L, 1L, 1L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 1L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 1L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 1L, 2L, 1L, 
        1L, 2L, 2L, 1L, 1L, 1L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 1L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 1L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 1L, 2L, 1L, 
        1L, 2L, 2L, 1L, 1L, 1L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 1L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 1L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 1L, 2L, 1L, 
        1L, 2L, 2L, 1L, 1L, 1L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 1L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 1L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 1L, 2L, 1L, 
        1L, 2L, 2L, 1L, 1L, 1L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 1L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 1L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 1L, 2L, 1L, 
        1L, 2L, 2L, 1L, 1L, 1L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 1L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 1L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 1L, 2L, 1L, 
        1L, 2L, 2L, 1L, 1L, 1L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 1L, 2L
        ), 
      .Label = c(
        "west","east"
        ), 
      class = "factor"
      )
    ), 
  .Names = c(
    "State", "BCmort", "year", "eastWest"), 
  class = "data.frame", 
  row.names = c(NA, -208L)
  )

ggplot(mort3, aes(x = year, y = BCmort, col = State, linetype = State)) +
  geom_line(lwd = 1) +
  scale_linetype_manual(values = c(rep("solid", 10), rep("dashed", 6))) +
  scale_color_manual(values = c(rep("blue", 6), rep("red", 10))) +  # c(brewer.pal(10, "Set3"), brewer.pal(6, "Set3"))) +
  # opts(title = "BC mortality") +
  theme_bw()

colVec<-c(brewer.pal(10,"Set3"),brewer.pal(6,"Set3"))
ltyVec<-rep(c("solid","dashed"),c(10,6))

ggplot(mort3, aes(x = year, y = BCmort, col = State, lty = eastWest)) +
  geom_line(lwd = 1) +
  scale_linetype_manual(values = c(west = "solid", east = "dashed")) +
  scale_color_manual(values = c(brewer.pal(10, "Set3"), brewer.pal(6, "Set3"))) +
  opts(title = "BC mortality")
