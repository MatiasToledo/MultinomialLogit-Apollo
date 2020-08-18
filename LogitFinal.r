### Clear memory
rm(list = ls())

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="LogitFinal_MT",
  modelDescr ="Multinomial Logit intento 2",
  indivID    ="ID"
)


database = read.csv("Datos_Edit.csv",header=TRUE, sep = ";")


### Vector of parameters

apollo_beta=c(con_Caminata = 0, 
              con_Bici = 0,
              con_Micro = 0,
              con_4 = 0, 
              con_5 = 0,
              b_tiempo = 0,
              #b_tiempo_1=0,
              #b_tiempo_2=0,
              #b_tiempo_3=0,
              #b_tiempo_5=0,
              b_edad_17_20_2 = 0,
              b_edad_24_30_3 = 0,
              b_edad_30_mas_3 = 0,
              b_Desempleado_4 = 0,
              b_N_Auto_0_5 = 0,
              b_Dinero_50_3 = 0,
              b_Auto_Exito_5 = 0,
              b_Compra_Auto_5 = 0,
              b_Nota_TranPub = 0,
              b_Movilidad_Elect_CCP = 0,
              B_Frase3_C_2 = 0,
              B_Frase4_C_5 = 0,
              B_Frase6_C_2 = 0,
              B_Frase6_C_5 = 0
              )

apollo_fixed = c("con_Caminata")
apollo_inputs = apollo_validateInputs()


apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){

  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ###list of probabilities P
  P = list()
  ### Utilities
  V = list()

  V[['Caminata']]  = con_Caminata + b_tiempo * Tiempo_1
  V[['Bici']]  = con_Bici + b_tiempo * Tiempo_2 + b_edad_17_20_2 * Edad_17_20 + B_Frase3_C_2 * Frase3_C + B_Frase6_C_2 * Frase6_C
  V[['Micro']]  = con_Micro + b_tiempo * Tiempo_3 + b_edad_24_30_3 * Edad_24_30 + b_edad_30_mas_3 * Edad_30_mas + b_Dinero_50_3 * Dinero_50 + b_Movilidad_Elect_CCP * Movilidad_Elect_CCP + b_Nota_TranPub * Nota_TranPublico
  V[['G1']] = con_4 + b_Desempleado_4 * Desempleado
  V[['G2']] = con_5 + b_tiempo * Tiempo_5 + b_N_Auto_0_5 * Auto_si + b_Auto_Exito_5 * Auto_Exito + b_Compra_Auto_5 * Compra_Auto + B_Frase4_C_5 * Frase4_C + B_Frase6_C_5 * Frase6_C
  
  ### MNL Settings
  mnl_settings = list(
    alternatives  = c(Caminata=1, Bici=2, Micro=3, G1=4,G2=5), 
    avail         = list(Caminata=av_Caminata, Bici=av_Bici, Micro=av_Micro, G1=av_G1,G2=av_G2), 
    choiceVar     = Opc_Principal_Modelo,
    V             = V
  )

  P[['model']] = apollo_mnl(mnl_settings, functionality)

  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

#Model Estimation
model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(model)
apollo_saveOutput(model)


