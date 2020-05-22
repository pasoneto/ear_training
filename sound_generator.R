library(phonTools)

###############################################################################
## FUNCTION RECEIVES A FREQUENCY AND AN INTERVAL (IN CENTS), AND RETURNS THE ##
## DISTANCE, IN HERTZ, BETWEEN F1 AND THE DESIRED CENT                       ##
###############################################################################

cent_to_hertz = function(f1, cent){
  #freq to cent original equation: 1200*( log(x = (f2/f1), base = 2))
  dif = f1*(2^ (cent / 1200) ) 
  return(dif)
}


################################################################
## FUNCTION RECEIVES A BASE FREQUENCY, AN INTERVAL (IN CENTS) ##
## AND RETURNS A WAVE FILE WITH THE INTERVAL                  ##
################################################################

trial_making =
function(frequencia_base, intervalo){
  
    int2 = cent_to_hertz(frequencia_base, intervalo)#funçao que converte cent para Hz
  
    som1 = seewave::synth(f = 22050, 
                   d = 0.8, 
                   cf = frequencia_base, 
                   harmonics = c(1, 2, 3),
                   #shape = 'sine',
                   output = 'ts')

    pausa = replicate(5000, 0)
    
    som2 = seewave::synth(f = 22050, 
                          d = 1, 
                          cf = int2, 
                          harmonics = c(1, 2, 3),
                          #shape = 'sine', 
                          output = 'ts')
    

    trial = c(som1, pausa, som2)
    return(trial)
}

seewave::listen(trial_making(500, 400), f = 22050)
######################################################################
## LOOP CREATES A VECTOR WITH WAV FILES WHERE EACH INDEX IS A TRIAL ##
######################################################################
setwd('C:/Users/Lenovo/Desktop/world/Jogos/Ear Training/sound')

intervals = seq(50, 2400, 50) 
nomes = c('unissono_+1','segunda_menor','segunda_menor+1','segunda_maior','segunda_maior+1','terca_menor','terca_menor+1','terca_maior','terca_maior+1','quarta_justa','quarta_justa+1','quarta_aumentada',
          'quarta_aumentada+1','quinta_justa','quinta_justa+1','quinta_aumentada',
          'quinta_aumentada+1','sexta_maior','sexta_maior+1','sexta_aumentada',
          'sexta_aumentada+1','setima_maior','setima_maior+1','oitava','unissono_+1_oitava','segunda_menor_oitava','segunda_menor+1_oitava','segunda_maior_oitava',
          'segunda_maior+1_oitava','terca_menor_oitava','terca_menor+1_oitava','terca_maior_oitava','terca_maior+1_oitava','quarta_justa_oitava','quarta_justa+1_oitava','quarta_aumentada_oitava',
          'quarta_aumentada+1_oitava','quinta_justa_oitava','quinta_justa+1_oitava','quinta_aumentada_oitava',
          'quinta_aumentada+1_oitava','sexta_maior_oitava','sexta_maior+1_oitava','sexta_aumentada_oitava',
          'sexta_aumentada+1_oitava','setima_maior_oitava','setima_maior+1_oitava','oitava_oitava')

#Making C files
for(i in 1:length(intervals)){
  name = paste('do_', nomes[i], '.wav', sep = '')
  sound = as.numeric(trial_making(130.813, intervals[i]))
  assign(name, sound)
  seewave::savewav(wave = assign(name, sound), filename = name, f = 22050)
}

#Making Lá files
for(i in 1:length(intervals)){
  name = paste('la_', nomes[i], '.wav', sep = '')
  sound = as.numeric(trial_making(220, intervals[i]))
  assign(name, sound)
  seewave::savewav(wave = assign(name, sound), filename = name, f = 22050)
}

#Makinh G files
for(i in 1:length(intervals)){
  name = paste('sol_', nomes[i], '.wav', sep = '')
  sound = as.numeric(trial_making(392, intervals[i]))
  assign(name, sound)
  seewave::savewav(wave = assign(name, sound), filename = name, f = 22050)
}



#Visualizing and listening
seewave::listen(oi, f = 22050)
plot(oi[1:100], type = 'ls')
plot(oi[35000:35100], type = 'ls')