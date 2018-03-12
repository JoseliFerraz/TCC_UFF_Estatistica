# --------------------------
# Programacao TCC
# --------------------------
# Aluna : Joseli Moreira Ferraz
# A seguir o desenvilvimento do codigo necessario
# para estudar se existe relacao entre a prematuridade e poluicao
# ------------------------------------
# Determinacao das variaveis de pesquisa
# Y_i prematuridade1: i-esimo recem nascido prematudos <= 37 semanas de gestacao
# Y_i prematuridade2: i-esimo recem nascido prematudos <= 31 semanas de gestacao
# X_i2 : peso bebe 1 (<=1500g); 2 (>1500g e <2500g); 3 >=2500g
# X_i3 : escolaridade
# X_i4 : idade da mae
# X_i5 :consultas pre natais
# X_i6 : metodo de parto
# X_i7 : sexo do bebe
# X_i8: estado civil da mae
# X_i9:raca/cor
# X_i10 : primeiro filho
# X_11 poluicao1: 1 (pm_2.5> 25_um ) ; 0 cc
# X_12 poluicao2: 1 (pm_2.5> 25_um ) ; 2 (25_um<pm_2.5<75_um ); 3 (pm_2.5>75_um )
#-------------------------------------------------------

poluicaod=read.csv2()
head(poluicaod)
names(poluicaod)
str(poluicaod)
attach(poluicaod)

######################################
# An?lise Descritiva das variaveis selecionadas para este estudo
######################################

# ------------------------
#prematuro < 37 semanas
# ------------------------

# Agrupando as seguintes categorias:
# 9: Ignorado e 99 missing
#1: Menos de 22 semanas ; 2: 22 a 27 semanas ; 3: 28 a 31 semanas; 4: 32 a 36 semanas
#5: 37 a 41 semanas e 6: 42 semanas e mais
gestacao_37s=NULL
#head(poluicaod$GESTACAO,n=50)
for (i in 1:length(poluicaod$GESTACAO)){
  if((poluicaod$GESTACAO[i]==1) | (poluicaod$GESTACAO[i]==2) | (poluicaod$GESTACAO[i]==3) | (poluicaod$GESTACAO[i]==4)){
    gestacao_37s[i]=1  # prematuro < 37 semanas
  }
  else{
    gestacao_37s[i]=99 # NA'S e missing
  }
}
table(gestacao_37s);summary(gestacao_37s)
apply(gestacao_37s,1,table)

tapply(PESO,gest,mean)
tapply(PESO,gest,sd)
tapply(PESO,gest,quantile)
sapply(gestacao_37s,quantile,c(0.10,0.90))
table(gest)
prop.table(table(gestacao_37s))*100
prop.table(table(BPN,gest),2)*100




# --------------------
# Y_i prematuridade2 i-esimo recem nascido prematudos <= 31 (7 meses aprox)
# --------------------
# Agrupando as seguintes categorias:
# 9: Ignorado e 99 missing
#1: Menos de 22 semanas ; 2: 22 a 27 semanas ; 3: 28 a 31 semanas; 
#4: 32 a 36 semanas
#5: 37 a 41 semanas e 6: 42 semanas e mais
table(poluicaod$GESTACAO);summary(poluicaod$GESTACAO)
gestacao_31=NULL
for (i in 1:length(poluicaod$GESTACAO)){
  if (poluicaod$GESTACAO[i]==99 | poluicaod$GESTACAO[i]==9){
    gestacao_31[i]=99 # missing
    # na's=>missing
  }
  else
    if(poluicaod$GESTACAO[i]==1|poluicaod$GESTACAO[i]==2|poluicaod$GESTACAO[i]==3){
      gestacao_31[i]=1 
    }
  else{
    gestacao_31[i]=2
  }
}
table(gestacao_31)
prop.table(table(gestacao_31))*100

# --------------------
# Peso bebe
# --------------------
# X_i2 : peso bebe: 1 (<=2500g); 0 CC
pb=NULL
for (i in 1:length(poluicaod$PESO)){
  if(poluicaod$PESO[i]==99|poluicaod$PESO[i]>2500){
    pb[i]=0
  }else{
    pb[i]=1
  }
}
table(pb)
prop.table(table(pb))*100 
prop.table(table(gestacao_37s,pb),2)*100
prop.table(table(gestacao_31,pb),2)*100
# --------------------
# X_i3 : escolaridade
# --------------------
#Analfabeta , se possui 0 anos de estudos
#Ensino Fundamental, 1 a 7 anos de estudos
#Ensino Medio, 8 a 11 anos de estudos
#Superior, maior que 12 anos de estudos
table(poluicaod$ESCMAE)
summary(poluicaod$ESCMAE)
escol_mae=NULL
for(i in 1:length(poluicaod$ESCMAE)){
  if(poluicaod$ESCMAE[i]==99){
    escol_mae[i]=99 #missing
  }
  else
    if(poluicaod$ESCMAE[i]==0|| poluicaod$ESCMAE[i]==1){
      escol_mae[i]=1 #analfabeta
    }
  else
    if(poluicaod$ESCMAE[i]==2 || poluicaod$ESCMAE[i]==3){
      escol_mae[i]=2 #ensino fundamental
    }
  else
    if(poluicaod$ESCMAE[i]== 4){
      escol_mae[i]=3 #ensino m?dio
    }
  else{
    escol_mae[i]=4 #ensino superior
  }  
}
table(escol_mae)
prop.table(table(escol_mae))*100 
prop.table(table(gestacao_37s,escol_mae),2)*100
prop.table(table(gestacao_31,escol_mae),2)*100 
# --------------------
# X_i4 : idade da mae
# --------------------
#1, <19 anos ; 2, 20 a 29 anos ; 3: 30 a 34 anos;  4: >35 anos
summary(poluicaod$IDADEMAE)

idade_mae=NULL#n?o tem missing
for(i in 1:length(poluicaod$IDADEMAE)){
  if(poluicaod$IDADEMAE[i] <= 19){
    idade_mae[i]=1 #at? 19 anos
  }
  else
    if(poluicaod$IDADEMAE[i]>=20 & poluicaod$IDADEMAE[i]<=29){
      idade_mae[i]=2 # de 20 a 29 anos
    }
  else
    if(poluicaod$IDADEMAE[i]>=30 & poluicaod$IDADEMAE[i]<=34){
      idade_mae[i]=3 # de 30 a 34 anos
    }
  else{
    idade_mae[i]=4 # de 35anos ou mais
  }  
}
table(idade_mae)
prop.table(table(idade_mae))*100 
prop.table(table(gestacao_37s,idade_mae),2)*100
prop.table(table(gestacao_31,idade_mae),2)*100

# --------------------
# X_i8: estado civil da mae
# --------------------
#1: Solteira ; 2: Casada; 3: Viuva ; 4: Separado judicialmente/Divorciado
# 5: Uni?o consensual (vers?es anteriores); 9: Ignorado
table(poluicaod$ESTCIVMAE)
estado_civil=NULL
for(i in 1:length(poluicaod$ESTCIVMAE)){
  if(poluicaod$ESTCIVMAE[i] == 99 | poluicaod$ESTCIVMAE[i] == 9){
    estado_civil[i]=99 # missing
  }
  else
    if(poluicaod$ESTCIVMAE[i] == 1 |poluicaod$ESTCIVMAE[i] == 3 | poluicaod$ESTCIVMAE[i] == 4){
      estado_civil[i]=1 # solteira
    }
  else{
    estado_civil[i]=2 # casada ou consensual
  }
}
table(estado_civil)
prop.table(table(estado_civil))*100 
prop.table(table(gestacao_37s,estado_civil),2)*100
prop.table(table(gestacao_31,estado_civil),2)*100


# --------------------
# X_i9:raca/cor
# --------------------
# 1:Branca; 2:Preta; 3:Amarela; 4: Parda; 5: Ind?gena
table(poluicaod$RACACOR);summary(poluicaod$RACACOR)
raca=NULL
for (i in 1:length(poluicaod$RACACOR)){
  if(poluicaod$RACACOR[i]==99){
    raca[i]=99 #missing
  }
  else
    if(poluicaod$RACACOR[i]==1){ # branca
      raca[i]=1
    }
  else{
    raca[i]=2 #parda preta amarela e ind?gena
  }
}
table(raca)
prop.table(table(raca))*100 
prop.table(table(gestacao_37s,raca),2)*100
prop.table(table(gestacao_31,raca),2)*100
# --------------------
# X_i5 :consultas pre natais
# --------------------
# Trocando categoria ignorado por missing
summary(poluicaod$CONSULTAS);table(poluicaod$CONSULTAS)
consultas=NULL
for (i in 1:length(poluicaod$CONSULTAS)){
  if(poluicaod$CONSULTAS[i] == 99 | poluicaod$CONSULTAS[i]==9){
    consultas[i] = 99 #missing
  }
  else
    if(poluicaod$CONSULTAS[i]==1){
      consultas[i] = 1 # nenhuma
    }
  else
    if(poluicaod$CONSULTAS[i]==2){
      consultas[i] = 2 # 1 a 3
    }
  else
    if(poluicaod$CONSULTAS[i]==3){
      consultas[i] = 3 # 4 a 6
    }
  else
    consultas[i] = 4 # 7 ou +
  
}
#table(poluicaod$CONSULTAS)
table(consultas)

prop.table(table(consultas))*100 
prop.table(table(gestacao_37s,consultas),2)*100
prop.table(table(gestacao_31,consultas),2)*100

# --------------------
# X_i7 : sexo do bebe
# --------------------
# 0: Ignorado ; 1: Masculino ; 2: Feminino
summary(poluicaod$SEXO) # n?o tem na'a
sexo=NULL
for (i in 1:length(poluicaod$SEXO)){
  if(poluicaod$SEXO[i]==9){
    sexo[i]=99 # ignorado => missing
  }
  else
    if(poluicaod$SEXO[i]==1){
      sexo[i]=1 #masculino
    }
  else{
    sexo[i]=2 #feminino
  }
}
table(sexo)
prop.table(table(sexo))*100 
prop.table(table(gestacao_37s,sexo),2)*100
prop.table(table(gestacao_31,sexo),2)*100

# --------------------
# X_i6 : metodo de parto
# --------------------
table(poluicaod$PARTO);summary(poluicaod$PARTO)
# havia NA'S
parto=NULL
for (i in 1:length(poluicaod$PARTO)){
  if(poluicaod$PARTO[i]==99){
    parto[i]=99
  }
  else
    if(poluicaod$PARTO[i]==1){
      parto[i]=1 #vaginal
    }
  else{
    parto[i]=2 #cesareo
  }  
}
table(parto)
prop.table(table(parto))*100 
prop.table(table(gestacao_37s,parto),2)*100
prop.table(table(gestacao_31,parto),2)*100

# -------------------------
# X_i10 : primeiro filho
# -------------------------
table(poluicaod$QTDFILVIVO);summary(poluicaod$QTDFILVIVO)
table(poluicaod$QTDFILMORT);summary(poluicaod$QTDFILMORT)
prim_fil=NULL
for(i in 1:length(poluicaod$QTDFILVIVO)){
  if(poluicaod$QTDFILVIVO[i]==0 & poluicaod$QTDFILMORT[i]==0){
    prim_fil[i]=1 #sim
  }else
    if(poluicaod$QTDFILVIVO[i]==99 & poluicaod$QTDFILMORT[i]==0){
      prim_fil[i]=99
    }
  else
    if(poluicaod$QTDFILVIVO[i]==0 & poluicaod$QTDFILMORT[i]==99){
      prim_fil[i]=99
    }
  else
    if(poluicaod$QTDFILVIVO[i]!=0 & poluicaod$QTDFILMORT[i]==99){
      prim_fil[i]=99
    }
  else
    if(poluicaod$QTDFILVIVO[i]==99 & poluicaod$QTDFILMORT[i]!=0){
      prim_fil[i]=99
    }
  else
    if(poluicaod$QTDFILVIVO[i]==99 & poluicaod$QTDFILMORT[i]==99){
      prim_fil[i]=99
    }  
  else
    prim_fil[i]=2 #n?o possui filho morto
}
table(prim_fil)
prop.table(table(prim_fil))*100 
prop.table(table(gestacao_37s,prim_fil),2)*100
prop.table(table(gestacao_31,prim_fil),2)*100
