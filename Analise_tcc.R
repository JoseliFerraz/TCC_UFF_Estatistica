# --------------------------
# Programacao TCC
# --------------------------
# Aluna : Joseli Moreira Ferraz
# A seguir o desenvilvimento do codigo necessario
# para estudar se existe relacao entre a prematuridade e poluicao
# -----------------------------
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

poluicaod=read.csv2("SINAS_Poluicao1_missing.csv")
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

# -----------------------------
#  Transformando em fator
# -----------------------------
# menos 37 semanas

gestacao_37s1=NULL
for (i in 1:length(poluicaod$GESTACAO)){
  if((poluicaod$GESTACAO[i]==1) | (poluicaod$GESTACAO[i]==2) | (poluicaod$GESTACAO[i]==3) | (poluicaod$GESTACAO[i]==4)){
    gestacao_37s1[i]=1  # prematuro < 37 semanas
  }
  else
    if((poluicaod$GESTACAO[i]==5) | (poluicaod$GESTACAO[i]==6) ){
      gestacao_37s1[i]=0 # > 37 semanas
    }
  else{
    gestacao_37s1[i]=99# NA'S e missing
  }
}
gest_37=as.factor(ifelse(gestacao_37s1==1,1,ifelse(gestacao_37s1==0,0,ifelse(gestacao_37s1==99,NA,NA))))
pb=as.factor(ifelse(pb==1,1,ifelse(pb==0,0,ifelse(pb==99,NA,NA))))
escol_mae=as.factor(ifelse(escol_mae==1,1,ifelse(escol_mae==2,2,ifelse(escol_mae==3,3,ifelse(escol_mae==4,4,ifelse(escol_mae==99,NA,NA))))))
idade_mae=as.factor(ifelse(idade_mae==1,1,ifelse(idade_mae==2,2,ifelse(idade_mae==3,3,ifelse(idade_mae==4,4,ifelse(idade_mae==99,NA,NA))))))
estado_civil=as.factor(ifelse(estado_civil==1,1,ifelse(estado_civil==2,2,ifelse(estado_civil==99,NA,NA))))
raca=as.factor(ifelse(raca==1,1,ifelse(raca==2,2,ifelse(raca==99,NA,NA))))
consultas=as.factor(ifelse(consultas==1,1,ifelse(consultas==2,2,ifelse(consultas==3,3,ifelse(consultas==4,4,ifelse(consultas==99,NA,NA))))))
sexo=as.factor(ifelse(sexo==1,1,ifelse(sexo==2,2,ifelse(sexo==99,NA,NA))))
parto=as.factor(ifelse(parto==1,1,ifelse(parto==2,2,ifelse(parto==99,NA,NA))))
prim_fil=as.factor(ifelse(prim_fil==1,1,ifelse(prim_fil==2,2,ifelse(prim_fil==99,NA,NA))))
modelo1=glm(gest_37~pb+escol_mae+idade_mae+estado_civil+raca+consultas+sexo+parto+prim_fil,family = binomial)
summary(modelo1)
# Criacao banco csv armazenando dados
#x=data.frame(gest_37,pb,escol_mae,idade_mae,estado_civil,raca,consultas,sexo,parto,prim_fil)
#write.csv2(x,"1_gest37_var.csv") antigo gest37_var.csv"
# Leitura do excel 2 folha
dados_37=read.csv2("SINAS_Poluicao1_missing.csv", skip=5, sheet = 2);attach(dados_37)
# -----------------------------------------------------
# ----------------------------------------------------
# Modelo 
# -----------------------------------------------------
# ----------------------------------------------------
# 
rls=glm(gest_37 ~pb,family=binomial);rls;summary(rls) # pvalor <2e-16

beta=coefficients(rls);beta #3.185591
OR=exp(beta);OR # 24.18157953

# IC (pc)  IC = [ Beta + - 1.96*z=Std. Error]
linf=exp(3.185-1.96*0.07150);linf
lsup=exp(3.185+1.96*0.07150);lsup

# --------------------------------
# Escolaridade da mae
# --------------------------------
rls1=glm(gest_37 ~escol_mae,family=binomial);rls1;summary(rls1) #0.000118
beta1=coefficients(rls1);beta1
OR=exp(beta1);OR
# escol_mae2
linf2=exp(-0.149-1.96*0.352);linf2
lsup2=exp(-0.149+1.96*0.352);lsup2
#escol_mae3
linf3=exp(-0.232-1.96*0.351);linf3
lsup3=exp(-0.232+1.96*0.351);lsup3
#escol_mae4
linf4=exp(-0.453-1.96*0.358);linf4
lsup4=exp(-0.453+1.96*0.358);lsup4

# --------------------------------
# Idade da mae
# --------------------------------
rls2=glm(gest_37 ~idade_mae,family=binomial);rls2
summary(rls2) #0.00177 
beta2=coefficients(rls2);beta2
OR=exp(beta2);OR

# IC (pc)  IC = [ Beta + - 1.96*z=Std. Error]
# Idade 2
linf2=exp(-0.254-1.96*0.0610);linf2
lsup2=exp(-0.254+1.96*0.0610);lsup2

# Idade 3
linf3=exp(-0.359-1.96*0.0858);linf3
lsup3=exp(-0.359+1.96*0.0858);lsup3

# Idade 4
linf4=exp(-0.152-1.96*0.1051);linf4
lsup4=exp(-0.152+1.96*0.1051);lsup4
# --------------------------------
# Estado civil da mae
# --------------------------------
rls3=glm(gest_37 ~estado_civil,family=binomial);rls3;summary(rls3) #1.7e-05 
beta3=coefficients(rls3);beta3
OR=exp(beta3);OR
linf=exp(-0.1996-1.96*0.0526);linf
lsup=exp(-0.1996+1.96*0.0526);lsup
# --------------------------------
# Raca cor da mae
# --------------------------------
rls4=glm(gest_37 ~raca,family=binomial);rls4;summary(rls4) #0.0203  
beta4=coefficients(rls4);beta4
OR=exp(beta4);OR
linf=exp(0.2185-1.96*0.0701);linf
lsup=exp(0.2185+1.96*0.0701);lsup
# --------------------
# X_i5 :consultas pre natais
# --------------------
rls5=glm(gest_37 ~consultas,family=binomial);rls5;summary(rls5) #0.12  
beta5=coefficients(rls5);beta5
OR=exp(beta5);OR
# 2 consultas
linf2=exp(0.229-1.96*0.091);linf2
lsup2=exp(0.229+1.96*0.091);lsup2
# 3 consultas
linf3=exp(-0.101-1.96*0.111);linf3
lsup3=exp(-0.101+1.96*0.111);lsup3
# 4 consultas
linf4=exp(-0.851-1.96*0.102);linf4
lsup4=exp(-0.851+1.96*0.102);lsup4

# --------------------
# X_i7 : sexo do bebe
# --------------------
rls6=glm(gest_37 ~sexo,family=binomial);rls6;summary(rls6) #0.462  
beta6=coefficients(rls6);beta6
OR=exp(beta6);OR
linf=exp(-0.018-1.96*0.051);linf
lsup=exp(-0.018+1.96*0.051);lsup
# --------------------
# X_i6 : metodo de parto
# --------------------
rls7=glm(gest_37 ~parto,family=binomial);rls7;summary(rls7) #0.0468  
beta7=coefficients(rls7);beta7
OR=exp(beta7);OR
linf=exp(0.349-1.96*0.052);linf
lsup=exp(0.349+1.96*0.052);lsup
# -------------------------
# X_i10 : primeiro filho
# -------------------------
rls8=glm(gest_37 ~prim_fil,family=binomial);rls8;summary(rls8) #0.0468  
beta8=coefficients(rls8);beta8
OR=exp(beta8);OR
linf=exp(-0.141-1.96*0.054);linf
lsup=exp(-0.141+1.96*0.054);lsup

#------------------
# Modelo completo
#------------------
modelo_comp_37=glm(gest_37~pb+escol_mae+idade_mae+estado_civil+raca+consultas+sexo+parto+prim_fil,family=binomial)
summary(modelo_comp_37)

# As variaveis selecionadas foram
# Teste de Wald 
# H_0 : B_K =0 : nao existe relacao, nao explica o modelo
# H_1 : B_K =/0 : existe relacao, explica o modelo
# Deseja-se rejeitar H_0

# Modelo selecionado
mod1_comp_37=glm(gest_37~pb+idade_mae+estado_civil+consultas+parto,family=binomial)
summary(mod1_comp_37)


#########################################################################
#               Mudando categorias de regerencia
#########################################################################
escol_mae2=as.factor(ifelse(escol_mae==1,0,ifelse(escol_mae==2,2,ifelse(escol_mae==3,3,ifelse(escol_mae==4,4,ifelse(escol_mae==99,NA,NA))))));table(escol_mae2)

consultas2=as.factor(ifelse(consultas==1,0,ifelse(consultas==2,2,ifelse(consultas==3,3,ifelse(consultas==4,4,ifelse(consultas==99,NA,NA))))));table(consultas2)

prim_fil2=as.factor(ifelse(prim_fil==1,1,ifelse(prim_fil==2,0,ifelse(prim_fil==99,NA,NA))));table(prim_fil2)

idade_mae2=as.factor(ifelse(idade_mae==1,1,ifelse(idade_mae==2,0,ifelse(idade_mae==3,0,ifelse(idade_mae==4,4,ifelse(idade_mae==99,NA,NA))))));table(idade_mae2)


#poluicaod$idade3=as.factor(ifelse(poluicaod$idade==1,1,ifelse(poluicaod$idade==2,1,ifelse(poluicaod$idade==3,0,ifelse(poluicaod$idade==4,0,ifelse(poluicaod$idade==5,2,ifelse(poluicaod$idade==99,NA,NA)))))))
#table(poluicaod$idade3)

#poluicaod$idade4=as.factor(ifelse(poluicaod$idade==1,1,ifelse(poluicaod$idade==2,2,ifelse(poluicaod$idade==3,3,ifelse(poluicaod$idade==4,4,ifelse(poluicaod$idade==5,4,ifelse(poluicaod$idade==99,NA,NA)))))))
#table(poluicaod$idade4)


estado_civil2=as.factor(ifelse(estado_civil==1,1,ifelse(estado_civil==2,0,ifelse(estado_civil==99,NA,NA))));table(estado_civil2)

raca2=as.factor(ifelse(raca==1,0,ifelse(raca==2,1,ifelse(raca==99,NA,NA))));table(raca2)

#y=data.frame(gest_37,pb,escol_mae2,consultas2,prim_fil2,idade_mae2,estado_civil2,raca2,sexo,parto)
#write.csv2(y,"2_gest37_reg.csv") # 3 folha do excel
dados_37_nv=read.csv2("SINAS_Poluicao1_missing.csv", skip=5, sheet = 3);attach(dados_37_nv)

# Modelo completo com variaveis ajustadas
modelo2_comp_37=glm(gest_37~pb+escol_mae2+idade_mae2+estado_civil2+raca2+consultas2+sexo+parto+prim_fil2,family=binomial)

modelo2_comp_378=glm(gest_37~pb+escol_mae2+idade_mae2+estado_civil2+raca2+consultas2+sexo+parto+prim_fil2,family=binomial(link = "logit"))

summary(modelo2_comp_37)
anova(modelo2_comp_37,test="Chisq")
lm(modelo2_comp_37)

# Modelo 1
# Modelo reduzido : variaveis significativas
mod2_comp_37=glm(gest_37~pb+idade_mae+estado_civil2+consultas2+parto,family=binomial)
summary(mod2_comp_37)
h=predict(mod2_comp_37)

length(gest_37)
length(h)
f=fitted.values(mod2_comp_37);length(f)
length(table(f))
# Inserindo var raca
mod3_comp_37=glm(gest_37~pb+idade_mae+estado_civil2+consultas2+parto+raca2,family=binomial)
summary(mod3_comp_37)

# Inserindo var sexo
mod4_comp_37=glm(gest_37~pb+idade_mae+estado_civil2+consultas2+parto+sexo,family=binomial)
summary(mod4_comp_37)

# Inserindo var raca+sexo
mod5_comp_37=glm(gest_37~pb+idade_mae+estado_civil2+consultas2+parto+raca2+sexo,family=binomial)
summary(mod5_comp_37)

# -------------------------------------------------
# Teste de Modelos Encaixado : Teste qui-quadrado
# -------------------------------------------------

# ------------
# 1 teste
# ------------
mod1=glm(gest_37~pb)
mod2=glm(gest_37~pb+consultas2);anova(mod1,mod2,test="Chisq") # 0.249 N rj
mod3=glm(gest_37~pb+parto);anova(mod1,mod3,test="Chisq") # 0.03315 rj
mod4=glm(gest_37~pb+raca2);anova(mod1,mod4,test="Chisq") # 1.037e-08 rj
mod5=glm(gest_37~pb+escol_mae2);anova(mod1,mod5,test="Chisq") # 0.3516 N rj
mod6=glm(gest_37~pb+estado_civil2);anova(mod1,mod6,test="Chisq") # 0.04105 rj


# ------------
# 2 teste
# ------------
mod7=glm(gest_37~consultas2)
mod8=glm(gest_37~consultas2+parto);anova(mod7,mod8,test="Chisq") # 9.391e-08 rj
mod9=glm(gest_37~consultas2+raca2);anova(mod7,mod9,test="Chisq") # 1.346e-08 rj
mod10=glm(gest_37~consultas2+escol_mae2);anova(mod7,mod10,test="Chisq") # 0.009731 rj
mod11=glm(gest_37~consultas2+estado_civil2);anova(mod7,mod11,test="Chisq") # 1.171e-07 rj

# ------------
# 3 teste
# ------------
mod12=glm(gest_37~parto)
mod13=glm(gest_37~parto+raca2);anova(mod12,mod13,test="Chisq") # 2.238e-11 rj
mod14=glm(gest_37~parto+escol_mae2);anova(mod12,mod14,test="Chisq") # 0.16421 N rj
mod15=glm(gest_37~parto+estado_civil2);anova(mod12,mod15,test="Chisq") # 2.141e-09 rj

# 1 pb ;2  consultas ; 3 parto; 4 raca;5 escol_mae2; 6 estado_civil2, 7 sexo
mod1_comp37=glm(gest_37~pb+consultas2+parto+raca2+escol_mae2,family=binomial)
mod2_comp37=glm(gest_37~pb+consultas2+parto+raca2+escol_mae2+estado_civil2,family=binomial)
mod3_comp37=glm(gest_37~pb+consultas2+parto+raca2+escol_mae2+sexo,family=binomial)
mod4_comp37=glm(gest_37~pb+consultas2+parto+raca2+escol_mae2+estado_civil2+sexo,family=binomial)

#pb+consultas2+parto+raca2+escol_mae2+estado_civil2+sex0

#-----------
# Analise Deviance
#-----------
mod1_comp37
mod2_comp37
mod3_comp37
mod4_comp37
deviance(mod1_comp37) 
#-----------
# Criterio AIC BIC
#-----------
summary(mod1_comp37)
AIC(mod1_comp37)# 
BIC(mod1_comp37)#

AIC(mod2_comp37)# 
BIC(mod2_comp37)#

AIC(mod3_comp37)# 
BIC(mod3_comp37)#

AIC(mod4_comp37)# 
BIC(mod4_comp37)#

#-----------
# Teste de hos e lemshow
#-----------
library(ResourceSelection)
hl=hoslem.test(gest_37, fitted(mod1_comp37), g=10);hl
h2=hoslem.test(gest_37, fitted(mod2_comp37), g=10);h2
h3=hoslem.test(gest_37, fitted(mod3_comp37), g=10);h3
h4=hoslem.test(gest_37, fitted(mod4_comp37), g=10);h4


# --------------------------------------------
#-------------------------------------
# Distancia de Cook :Medida mais completa para influencia
#-------------------------------------
Di=cooks.distance(mod2_comp_37);sort(Di)# ordenado a distancia de cook ,observamos
# Verificando se ha extrapolacao
subset(Di,subset = Di>=1) # Não ha extrapolacao de ponto 
plot(Di)
Di=cooks.distance(mod3_comp_37);sort(Di)# ordenado a distancia de cook ,observamos
# Verificando se ha extrapolacao
subset(Di,subset = Di>=1) # Não ha extrapolacao de ponto 

Di=cooks.distance(mod4_comp_37);sort(Di)# ordenado a distancia de cook ,observamos
# Verificando se ha extrapolacao
subset(Di,subset = Di>=1) # Não ha extrapolacao de ponto 

Di=cooks.distance(mod5_comp_37);sort(Di)# ordenado a distancia de cook ,observamos
# Verificando se ha extrapolacao
subset(Di,subset = Di>=1) # Não ha extrapolacao de ponto 

#-------------------------------------
# Analise Grafica 
#-------------------------------------
# Ychapeu : estimada :Modelo Y~pb+idade_mae+estado_civil2+consultas2+parto
#-------------------------------------
Ychapeu=fitted.values(mod2_comp_37);Ychapeu
# -----------------------------------------------
# residuos estudentizado : Do modelo selecionado 
# -----------------------------------------------
res_estudent=rstandard(mod2_comp_37);res_estudent
#-------------------------------------
# Ychapeu x Residuos 
#-------------------------------------
plot(Ychapeu,res_estudent,pch=19,xlab = "Valor esperado", ylab = "Resíduos Estudentizados")
abline(h=c(-2,0,2))
# Logo ha dois pts que ultrapassam a margem (-2,2)

# ---------
# Modelo 2
# ---------
Ychapeu2=fitted.values(mod3_comp_37)
# -----------------------------------------------
# residuos estudentizado : Do modelo selecionado 
# -----------------------------------------------
res_estudent=rstandard(mod3_comp_37)
#-------------------------------------
# Ychapeu x Residuos
#-------------------------------------
plot(Ychapeu2,res_estudent,pch=19,xlab = "Valor esperado", ylab = "Resíduos Estudentizados")
abline(h=c(-2,0,2))
# Logo ha dois pts que ultrapassam a margem (-2,2)

# ---------
# Modelo 3
# ---------
Ychapeu=fitted.values(mod4_comp_37)
# -----------------------------------------------
# residuos estudentizado : Do modelo selecionado 
# -----------------------------------------------
res_estudent=rstandard(mod4_comp_37)
#-------------------------------------
# Ychapeu x Residuos
#-------------------------------------
plot(Ychapeu,res_estudent,pch=19,xlab = "Valor esperado", ylab = "Resíduos Estudentizados")
abline(h=c(-2,0,2))
# Logo nao ha pts que ultrapassam a margem (-2,2)

# ---------
# Modelo 4
# ---------
Ychapeu=fitted.values(mod5_comp_37)
# -----------------------------------------------
# residuos estudentizado : Do modelo selecionado 
# -----------------------------------------------
res_estudent=rstandard(mod5_comp_37)
#-------------------------------------
# Ychapeu x Residuos
#-------------------------------------
plot(Ychapeu,res_estudent,pch=19,xlab = "Valor esperado", ylab = "Resíduos Estudentizados")
abline(h=c(-2,0,2))
# Logo nao ha  pts que ultrapassam a margem (-2,2)

#-------------------------------------
##Curva ROC
# https://www.r-bloggers.com/roc-curves-and-classification/
# http://blog.revolutionanalytics.com/2016/08/roc-curves-in-two-lines-of-code.html
library(ROCR)
#----------
# Modelo 1
#----------
# Para o primeiro modelo ,newdata = na.omit(gest_37) / na.rm=TRUE
S=predict(mod1_comp37,type="response");length(S)

# Calculo da sensibilidade e especificidade
roc.curve=function(s,print=FALSE){
  Ps=(S>s)*1
  FP=sum((Ps==1)*(gest_37==0))/sum(gest_37==0)
  TP=sum((Ps==1)*(gest_37==1))/sum(gest_37==1)
  if(print==TRUE){
    print(table(Observed=gest_37,Predicted=Ps))
  }
  vect=c(FP,TP)
  names(vect)=c("FPR","TPR")
  return(vect)
}
threshold = 0.5
Tab_clas=roc.curve(threshold,print=TRUE);Tab_clas

# Sensibilidade Especificidade

# S(P(YCHAPEU=1|Y=1)) :D/C+D
s=(814/(814+953))*100;s # 0.460

# E(P(YCHAPEU=0|Y=0)): A/A+B
e=(11309/(11309+506));e #0.9571

R_c=1-e;R_c # 0.04282691


#Preditores
pred <- prediction(as.numeric(S),as.numeric(gest_37))

# Calcula verdadeiros positivos e falsos positivos
perf <- performance(pred,"tpr", "fpr")

#Grafico curva ROC Modelo1
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
plot(roc.perf,ylab="Sensibilidade",xlab="1-Especificidade");abline(a=0, b= 1);abline(a=0.43, b= 1)

# Area sob a curva : area parcial
auc.perf = performance(pred, measure = "auc")
auc.perf@y.values #0.751345

# Legendas
names(m_1)=c("Sensibilidae 46,06%","Especificidade 95%","Área sob a curva 0.75","Critério ROC 0.042%")
legend(locator(1),names(m_1),cex=0.5,box.lty=0)


#
# Curva Roc do Modelos

S2=predict(mod2_comp37,type="response");length(S2)
#S2[is.na(S2)]=0
pred2 <- prediction(as.numeric(S2),as.numeric(gest_37))

S3=predict(mod3_comp37,type="response");length(S3)
pred3 <- prediction(as.numeric(S3),as.numeric(gest_37))
#S3[is.na(S3)]=0

S4=predict(mod4_comp37,type="response");length(S4)
pred4 <- prediction(as.numeric(S4),as.numeric(gest_37))
#summary(S4)
#S4[is.na(S4)]=0
summary(gest_37)

#Preditores
pred2 <- prediction(as.numeric(S2),as.numeric(gest_37))
pred3 <- prediction(as.numeric(S3),as.numeric(gest_37))
pred4 <- prediction(as.numeric(S4),as.numeric(gest_37))

# Calcula verdadeiros positivos e falsos positivos
perf2 <- performance(pred2,"tpr", "fpr")
perf3 <- performance(pred3,"tpr", "fpr")
perf4<- performance(pred4,"tpr", "fpr")

# Calculo da sensibilidade e especificidade dos Modelos 2,3,4
roc.curve=function(s,print=FALSE){
  Ps=(S2>s)*1 
  FP=sum((Ps==1)*(gest_37==0))/sum(gest_37==0)
  TP=sum((Ps==1)*(gest_37==1))/sum(gest_37==1)
  if(print==TRUE){
    print(table(Observed=gest_37,Predicted=Ps))
  }
  vect=c(FP,TP)
  names(vect)=c("FPR","TPR")
  return(vect)
}
threshold = 0.5
Tab_clas=roc.curve(threshold,print=TRUE);Tab_clas
# Sensi 0.457
# Especificidade 96
# Val predi 0.043
1-0.043
# Area sob a curva : area parcial
auc.perf = performance(pred2, measure = "auc")
auc.perf@y.values #0.7488415

#Grafico curva ROC Modelo2
roc.perf2 = performance(pred2, measure = "tpr", x.measure = "fpr")
plot(roc.perf2,ylab="Sensibilidade",xlab="1-Especificidade");abline(a=0, b= 1);abline(a=0.43, b= 1)

# Legendas
names(m_1)=c("Sensibilidae 45,78%","Especificidade 96%","Área sob a curva 0.74","Critério ROC 0.043%")
legend(locator(1),names(m_1),cex=0.5,box.lty=0)


# Modelo 3
roc.curve=function(s,print=FALSE){
  Ps=(S3>s)*1
  FP=sum((Ps==1)*(gest_37==0))/sum(gest_37==0)
  TP=sum((Ps==1)*(gest_37==1))/sum(gest_37==1)
  if(print==TRUE){
    print(table(Observed=gest_37,Predicted=Ps))
  }
  vect=c(FP,TP)
  names(vect)=c("FPR","TPR")
  return(vect)
}
threshold = 0.5
Tab_clas=roc.curve(threshold,print=TRUE);Tab_clas
# Sensi 0.45953594 
# Especificidade  96
# Val predi 0.04223445
1-0.04223445
# Area sob a curva : area parcial
auc.perf = performance(pred3, measure = "auc")
auc.perf@y.values #0.750645

#Grafico curva ROC Modelo3
roc.perf3 = performance(pred3, measure = "tpr", x.measure = "fpr")
plot(roc.perf3,ylab="Sensibilidade",xlab="1-Especificidade");abline(a=0, b= 1);abline(a=0.43, b= 1)

# Legendas
names(m_1)=c("Sensibilidae 45,95%","Especificidade 96%","Área sob a curva 0.75","Critério ROC 0.042%")
legend(locator(1),names(m_1),cex=0.5,box.lty=0)

# -----------
# Modelo 4
#----------
roc.curve=function(s,print=FALSE){
  Ps=(S4>s)*1
  FP=sum((Ps==1)*(gest_37==0))/sum(gest_37==0)
  TP=sum((Ps==1)*(gest_37==1))/sum(gest_37==1)
  if(print==TRUE){
    print(table(Observed=gest_37,Predicted=Ps))
  }
  vect=c(FP,TP)
  names(vect)=c("FPR","TPR")
  return(vect)
}
threshold = 0.5
Tab_clas=roc.curve(threshold,print=TRUE);Tab_clas

# Sensib 0.45840407
# Especi 96
# 1- espe 0.04206517 
1-0.04206517
# Area sob a curva : area parcial
auc.perf = performance(pred4, measure = "auc")
auc.perf@y.values #0.7492378

#Grafico curva ROC Modelo4
roc.perf4 = performance(pred4, measure = "tpr", x.measure = "fpr")
plot(roc.perf4,ylab="Sensibilidade",xlab="1-Especificidade");abline(a=0, b= 1);abline(a=0.43, b= 1)

# Legendas
names(m_1)=c("Sensibilidae 45,84%","Especificidade 96%","Área sob a curva 0.74","Critério ROC 0.042%")
legend(locator(1),names(m_1),cex=0.5,box.lty=0)