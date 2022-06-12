#Titular X, Conjuge Y, Dependente Z
##colunas impares das tabuas F, pares M. 

#Instrucoes
#1-alterar o caminho na variavel tabuainput na funcao selecttable para o local da
#planilha com as tabuas de mortalidade
#2-ler todas funcoes do script
#3-rodar funcao main no final do script 

library(svDialogs)
library(readxl)

#funcao com os inputs
input=function(){ 
  nvidas<<-as.numeric(dlgInput("Numero de vidas(2 ou 3): ", Sys.info()["user"])$res)
  IdadeTitular<<-as.numeric(dlgInput("Idade do titular ", Sys.info()["user"])$res)
  IdadeConjuge<<-as.numeric(dlgInput("Idade do conjuge ", Sys.info()["user"])$res)
  SexoTitular<<-as.character(dlgInput("Sexo do titular (M ou F) ", Sys.info()["user"])$res)
  SexoConjuge<<-as.character(dlgInput("Sexo do conjuge (M ou F) ", Sys.info()["user"])$res)
  if(nvidas==3){
    SexoDependente<<-as.character(dlgInput("Sexo do dependente (M ou F) ", Sys.info()["user"])$res)
    IdadeDependente<<-as.numeric(dlgInput("Idade do dependente (0 a 24) ", Sys.info()["user"])$res)}
  taxajuros<<-as.numeric(dlgInput("Taxa de juros (decimal) ", Sys.info()["user"])$res)
  TipoTabua<<-as.numeric(dlgInput("Tabua(1=AT2000,2=BR-EMSmt-2015,3=BR-EMSmt-2021): ", Sys.info()["user"])$res)
  Diferimento<<-as.numeric(dlgInput("Diferimento em anos: ", Sys.info()["user"])$res)
  valorfacebene<<-as.numeric(dlgInput("Valor de face do beneficio: ", Sys.info()["user"])$res)
  duracaocontrato<<-as.numeric(dlgInput("Duracao da vigencia contratual(anos): ", Sys.info()["user"])$res)
  pagamento<<-as.numeric(dlgInput("Duracao do(s) pagamentos(anos): ", Sys.info()["user"])$res)
  pensaomorte<<-as.numeric(dlgInput("Pensao por morte(sim=1, nao=0): ", Sys.info()["user"])$res)
  if(pensaomorte==1){
    coefreversao<<-as.numeric(dlgInput("Coeficiente de reversao(em decimal): ", Sys.info()["user"])$res)}
  lastsurvivor<<-as.numeric(dlgInput("Situacao de ultimo sobrevivente(sim=1, nao=0): ", Sys.info()["user"])$res)
  jointlives<<-as.numeric(dlgInput("Situacao de vidas conjuntas(sim=1, nao=0): ", Sys.info()["user"])$res)
  
}


#deletar na versao final
#agemax=tail(max(which(!is.na(tabuainput[,1]))),n=1) #ultima idade da tabua utilizada

#inicializacao dos vetores de comutacao, podendo-se alterar l0 e tamanho max de cada vetor(agemax)
variaveis=function(){
  agemax<<-119
  l0<<-10000000
  idade<<-c(0:agemax)
  l<<-vector(length=agemax+1) #estoque de vidas
  d<<-vector(length=agemax+1) #numero de mortes no periodo
  vx<<-vector(length=agemax+1) #desconto a valor presente
  Dx<<-vector(length=agemax+1) #valores a serem pagos por vida
  N<<-vector(length=agemax+1)
  Cx<<-vector(length=agemax+1)
  M<<-vector(length=agemax+1)
  p<<-vector(length=agemax+1)
  qx<<-vector(length=agemax+1)
  #alocar tabuas para os individuos escolhidos
  tabuaX<<-vector(length=agemax)
  tabuaY<<-vector(length=agemax)
  tabuaZ<<-vector(length=agemax)
  #criando dataframes
  duasvidasX<<-data.frame(idade,qx,l,d,vx,Dx,N,Cx,M)
  duasvidasY<<-data.frame(idade,qx,l,d,vx,Dx,N,Cx,M)
  duasvidasXY<<-data.frame(idade,p,l,d,vx,Dx,N,Cx,M)
  #renomear colunas
  colnames(duasvidasX)<<-c("Idade","qx","lx","dx","v^x","Dx","Nx","Cx","Mx")
  colnames(duasvidasY)<<-c("Idade","qy","ly","dy","v^y","Dy","Ny","Cy","My")
  colnames(duasvidasXY)<<-c("Idade","pxy","lxy","dxy","v^t","Dxy","Nxy","Cxy","Mxy")
  
  #criando dataframes
  tresvidasX<<-data.frame(idade,qx,l,d,vx,Dx,N,Cx,M)
  tresvidasY<<-data.frame(idade,qx,l,d,vx,Dx,N,Cx,M)
  tresvidasZ<<-data.frame(idade,qx,l,d,vx,Dx,N,Cx,M)
  tresvidasXY<<-data.frame(idade,p,l,d,vx,Dx,N,Cx,M)
  tresvidasYZ<<-data.frame(idade,p,l,d,vx,Dx,N,Cx,M)
  tresvidasXZ<<-data.frame(idade,p,l,d,vx,Dx,N,Cx,M)
  tresvidasXYZ<<-data.frame(idade,p,l,d,vx,Dx,N,Cx,M)
  #renomear colunas
  colnames(tresvidasX)<<-c("Idade","qx","lx","dx","v^x","Dx","Nx","Cx","Mx")
  colnames(tresvidasY)<<-c("Idade","qy","ly","dy","v^y","Dy","Ny","Cy","My")
  colnames(tresvidasZ)<<-c("Idade","qz","lz","dz","v^z","Dz","Nz","Cz","Mz")
  colnames(tresvidasXY)<<-c("Idade","pxy","lxy","dxy","v^t","Dxy","Nxy","Cxy","Mxy")
  colnames(tresvidasYZ)<<-c("Idade","pyz","lyz","dyz","v^t","Dyz","Nyz","Cyz","Myz")
  colnames(tresvidasXZ)<<-c("Idade","pxz","lxz","dxz","v^t","Dxz","Nxz","Cxz","Mxz")
  colnames(tresvidasXYZ)<<-c("Idade","pxyz","lxyz","dxyz","v^t","Dxyz","Nxyz","Cxyz","Mxyz")
}

#funcao para selecionar tabua
selecttable=function(Sexo){
  #local do arquivo com a planilha com as tabuas
  tabuainput=read_excel("D:/BrowserDownloads/EAC0424_T_GRUPOXX_TABUAS.xlsx")
  if(Sexo=="M" && TipoTabua==1){tabua=tabuainput[,2]}
  else if(Sexo=="F" && TipoTabua==1){tabua=tabuainput[,1]}
  else if(Sexo=="M" && TipoTabua==2){tabua=tabuainput[,4]}
  else if(Sexo=="F" && TipoTabua==2){tabua=tabuainput[,3]}
  else if(Sexo=="M" && TipoTabua==3){tabua=tabuainput[,6]}
  else if(Sexo=="F" && TipoTabua==3){tabua=tabuainput[,5]}
}

tabuaXYZ=function(SexoTitular,SexoConjuge,SexoDependente){
  tabuaX<<-selecttable(SexoTitular)
  tabuaY<<-selecttable(SexoConjuge)
    if(nvidas==3){tabuaZ<<-selecttable(SexoDependente)}
}

#funcoes para completar as tabuas
#funcao para 1 vida
UmaVida=function(umavida,taxajuros,tabua,l0){
  umavida[2]=tabua #tabua escolhida
  umavida[1,3]=l0 #l0
  umavida[1,4]=umavida[1,2]*umavida[1,3] #d0
  for(i in 2:116){umavida[i,3]=umavida[i-1,3]-umavida[i-1,4] #lx
  umavida[i,4]=umavida[i,2]*umavida[i,3]} #dx
  umavida[5]=(1/(1+taxajuros)^umavida[1])#v^t
  umavida[6]=umavida[3]*umavida[5] #Dx
  for(i in 1:116){umavida[i,7]=sum(umavida[i:116,6])}#NX
  for(i in 1:116){umavida[i,8]=umavida[i,4]*umavida[i+1,5]} #Cx
  for(i in 1:116){umavida[i,9]=sum(umavida[i:116,8])} #Mx
  umavida 
}

#tabuas de 1 vida 
duasvidasX=UmaVida(duasvidasX,taxajuros=taxajuros,tabua=tabuaX,l0=l0)
duasvidasY=UmaVida(duasvidasY,taxajuros,tabua=tabuaY,l0=l0)
tresvidasX=UmaVida(tresvidasX,taxajuros,tabuaX,l0)
tresvidasY=UmaVida(tresvidasY,taxajuros,tabuaY,l0)
tresvidasZ=UmaVida(tresvidasZ,taxajuros,tabuaZ,l0)

#deletar na versao final
#max(which(!is.na(x))) index do ultimo valor nao nulo em x

#funcao para 2 vidas
DuasVidas=function(duasvidas,duasvidas1,duasvidas2,taxajuros,l0,Idade1,Idade2){
  #fim=tail(tabua[,1],n=1)
  for(i in 1:116){
    duasvidas[i-1,2]=(1-duasvidas1[Idade1+i-1,2])*(1-duasvidas2[Idade2+i-1,2])}
  duasvidas[1,3]=l0
  duasvidas[1,4]=(1-duasvidas[1,2])*duasvidas[1,3] #d0
  for(i in 2:116){duasvidas[i,3]=duasvidas[i-1,3]-duasvidas[i-1,4] #lx
  duasvidas[i,4]=(1-duasvidas[i,2])*duasvidas[i,3]} #dx
  duasvidas[5]=(1/(1+taxajuros)^duasvidas[1])#v^t
  duasvidas[6]=duasvidas[3]*duasvidas[5] #Dx
  for(i in 1:116){duasvidas[i,7]=sum(duasvidas[i:116,6],na.rm=TRUE)}#NX
  for(i in 1:116){duasvidas[i,8]=duasvidas[i,4]*duasvidas[i+1,5]} #Cx
  for(i in 1:116){duasvidas[i,9]=sum(duasvidas[i:116,8],na.rm=TRUE)} #Mx
  duasvidas 
}


#tabuas 2 vidas
duasvidasXY=DuasVidas(duasvidasXY,duasvidas1=duasvidasX,duasvidas2=duasvidasY,taxajuros,l0,Idade1=IdadeTitular,Idade2=IdadeConjuge)
tresvidasXY=DuasVidas(tresvidasXY,duasvidas1=tresvidasX,duasvidas2=tresvidasY,taxajuros,l0,Idade1=IdadeTitular,Idade2=IdadeConjuge)
tresvidasYZ=DuasVidas(tresvidasYZ,duasvidas1=tresvidasY,duasvidas2=tresvidasZ,taxajuros,l0,Idade1=IdadeConjuge,Idade2=IdadeDependente)
tresvidasXZ=DuasVidas(tresvidasXZ,duasvidas1=tresvidasX,duasvidas2=tresvidasZ,taxajuros,l0,Idade1=IdadeTitular,Idade2=IdadeDependente)

#funcao para 3 vidas
TresVidas=function(tresvidas,tresvidasX,tresvidasY,tresvidasZ,taxajuros,l0){
  for(i in 1:116){
    tresvidas[i-1,2]=(1-tresvidasX[IdadeTitular+i-1,2])*(1-tresvidasY[IdadeConjuge+i-1,2])*(1-tresvidasZ[IdadeDependente+i-1,2])}
  tresvidas[1,3]=l0
  tresvidas[1,4]=(1-tresvidas[1,2])*tresvidas[1,3] #d0
  for(i in 2:116){tresvidas[i,3]=tresvidas[i-1,3]-tresvidas[i-1,4] #lx
  tresvidas[i,4]=(1-tresvidas[i,2])*tresvidas[i,3]} #dx
  tresvidas[5]=(1/(1+taxajuros)^tresvidas[1])#v^t
  tresvidas[6]=tresvidas[3]*tresvidas[5] #Dx
  for(i in 1:116){tresvidas[i,7]=sum(tresvidas[i:116,6],na.rm=TRUE)}#NX
  for(i in 1:116){tresvidas[i,8]=tresvidas[i,4]*tresvidas[i+1,5]} #Cx
  for(i in 1:116){tresvidas[i,9]=sum(tresvidas[i:116,8],na.rm=TRUE)} #Mx
  tresvidas
}

tresvidasXYZ=TresVidas(tresvidasXYZ,tresvidasX,tresvidasY,tresvidasZ,taxajuros,l0)

#calculo 1 vida (falta definir n) 

#antecipada
anuidadeVitAntec=valorfacebene*umavida[Idade,7]/umavida[Idade,6]
anuidadeTempAntec=valorfacebene*(umavida[Idade,7]-umavida[Idade+n,7])/umavida[Idade,6]
anuidadeVitDifAntec=valorfacebene*(umavida[Idade+n,7])/umavida[Idade,6]
#diferida em n e temp em m
anuidadeTempDifAntec=valorfacebene*(umavida[Idade+n,7]-umavida[Idade+n+m,7])/umavida[Idade,6]
#postecipada
anuidadeVitPos=valorfacebene*umavida[Idade+1,7]/umavida[Idade,6]
anuidadeTempPos=valorfacebene*(umavida[Idade+1,7]-umavida[Idade+n+1,7])/umavida[Idade,6]
anuidadeVitDifPos=valorfacebene*(umavida[Idade+n+1,7])/umavida[Idade,6]
#diferida em n e temp em m
anuidadeTempDifPos=valorfacebene*(umavida[Idade+n+1,7]-umavida[Idade+n+m+1,7])/umavida[Idade,6]

#Seguros Vida PUP
SeguroVida=valorfacebene*umavida[Idade,9]/umavida[Idade,6]
SeguroVidaDif=valorfacebene*umavida[Idade+n,9]/umavida[Idade,6]
SeguroVidaTemp=valorfacebene*(umavida[Idade,9]-umavida[Idade+n,9])/umavida[Idade,6]
#seguros vida anuais puros nivelados
SeguroVidaAnual=valorfacebene*umavida[Idade,9]/umavida[Idade,7]
SeguroVidaTemp=valorfacebene*(umavida[Idade,9]-umavida[Idade+n,9])/umavida[Idade,6]/(umavida[Idade,7]-umavida[Idade+n,7])/umavida[Idade,6]

#calculo 2 vidas X = Idade1 = maior e Y = Idade 2 = menor  n = duracao bene
Vidasconjuntas=valorfacebene*duasvidas[1,7]/duasvidas[1,6]
anuidadevitX=valorfacebene*duasvidas[Idade1,7]/duasvidas[Idade1,6]
anuidadevitY=valorfacebene*duasvidas[Idade2,7]/duasvidas[Idade2,6]
UltimoSobrevivente=anuidadeX+anuidadeY-Vidasconjuntas
AnuidadeReversaoXY=(anuidadeY-Vidasconjuntas)*coefreversao #de X para Y
AnuidadeReversaoYX=(anuidadeX-Vidasconjuntas)*coefreversao #de Y para X

#calculo 3 vidas
anuidadeVit=valorfacebene*tresvidas[1,7]/tresvidas[1,6]

#funcao para o calculo das precificacoes
calculo=function(){
  resultado=matrix(nrow=7,ncol=14)
  colnames(resultado)=c("AnuidadeVitAntec","AnuidadeTempAntec","AnuidadeVitDifAntec","AnuidadeTempDifAntec","AnuidadeVitPos","AnuidadeTempPos","AnuidadeVitDifPos","AnuidadeTempDifPos","SeguroVida","SeguroVidaDif","SeguroVidaTemp","VidasConjuntas","UltimoSobrevivente","Reversao")
  rownames(resultado)=c("X","Y","Z","XY","XZ","YZ","XYZ")
  #anuidade vitalicia antecipada
  resultado[1,1]=valorfacebene*duasvidasX[IdadeTitular+1,7]/duasvidasX[IdadeTitular+1,6]
  resultado[2,1]=valorfacebene*duasvidasY[IdadeConjuge+1,7]/duasvidasY[IdadeConjuge+1,6]
  resultado[3,1]=valorfacebene*tresvidasZ[IdadeDependente+1,7]/tresvidasZ[IdadeDependente+1,6]
  resultado[4,1]=valorfacebene*duasvidasXY[1,7]/duasvidasXY[1,6]
  resultado[5,1]=valorfacebene*tresvidasXZ[1,7]/tresvidasXZ[1,6]
  resultado[6,1]=valorfacebene*tresvidasYZ[1,7]/tresvidasYZ[1,6]
  resultado[7,1]=valorfacebene*tresvidasXYZ[1,7]/tresvidasXYZ[1,6]
  #anuidade temporaria de n anos antecipada
  resultado[1,2]=valorfacebene*(duasvidasX[IdadeTitular+1,7]-duasvidasX[IdadeTitular+Diferimento+1,7])/duasvidasX[IdadeTitular+1,6]
  resultado[2,2]=valorfacebene*(duasvidasX[IdadeConjuge+1,7]-duasvidasY[IdadeConjuge+Diferimento+1,7])/duasvidasY[IdadeConjuge+1,6]
  resultado[3,2]=valorfacebene*(tresvidasZ[IdadeDependente+1,7]-tresvidasZ[IdadeDependente+Diferimento+1,7])/tresvidasZ[IdadeDependente+1,6]
  resultado[4,2]=valorfacebene*(duasvidasXY[IdadeTitular+1,7]-duasvidasXY[IdadeTitular+Diferimento+IdadeConjuge+1,7])/duasvidasX[IdadeTitular+1,6]
  resultado[5,2]=valorfacebene*(tresvidasXZ[IdadeTitular+1,7]-tresvidasXZ[IdadeTitular+Diferimento+IdadeDependente+1,7])/duasvidasX[IdadeTitular+1,6]
  resultado[6,2]=valorfacebene*(tresvidasYZ[IdadeConjuge+1,7]-tresvidasYZ[IdadeConjuge+Diferimento+IdadeDependente+1,7])/duasvidasX[IdadeTitular+1,6]
  resultado[7,2]=valorfacebene*(tresvidasXYZ[IdadeDependente+1,7]-duasvidasX[IdadeDependente+1,7])/duasvidasX[IdadeDependente+1,6]
  
  #anuidade vitalicia diferida em n antecipada
  resultado[1,3]=valorfacebene*(duasvidasX[IdadeTitular+Diferimento+1,7])/duasvidasX[IdadeTitular+1,6]
  resultado[2,3]=valorfacebene*(duasvidasY[IdadeConjuge+Diferimento+1,7])/duasvidasY[IdadeConjuge+1,6]
  resultado[3,3]=valorfacebene*(tresvidasZ[IdadeDependente+Diferimento+1,7])/tresvidasZ[IdadeDependente+1,6]
  
  #anuidade vitalicia diferida em n e temporaria em m antecipada
  resultado[1,4]
  resultado[2,4]
  resultado[3,4]
  
  #anuidade vitalicia postecipada 
  resultado[1,5]=valorfacebene*duasvidasX[IdadeTitular+2,7]/duasvidasX[IdadeTitular+1,6]
  resultado[2,5]=valorfacebene*duasvidasY[IdadeConjuge+2,7]/duasvidasY[IdadeConjuge+1,6]
  resultado[3,5]=valorfacebene*tresvidasZ[IdadeDependente+2,7]/tresvidasZ[IdadeDependente+1,6]
  
  #anuidade temporaria de n anos postecipada
  resultado[1,6]=valorfacebene*(duasvidasX[IdadeTitular+2,7]-duasvidasX[IdadeTitular+Diferimento+2,7])/duasvidasX[IdadeTitular+1,6]
  resultado[2,6]=valorfacebene*(duasvidasX[IdadeConjuge+2,7]-duasvidasY[IdadeConjuge+Diferimento+2,7])/duasvidasY[IdadeConjuge+1,6]
  resultado[3,6]=valorfacebene*(tresvidasZ[IdadeDependente+2,7]-tresvidasZ[IdadeDependente+Diferimento+2,7])/tresvidasZ[IdadeDependente+1,6]
  
  #anuidade vitalicia diferida em n postecipada
  resultado[1,7]
  resultado[2,7]
  resultado[3,7]
  
  #Seguro Vida Inteira PUP
  resultado[1,9]=valorfacebene*(duasvidasX[IdadeTitular+1,9])/(duasvidasX[IdadeTitular+1,6])
  resultado[2,9]=valorfacebene*(duasvidasY[IdadeConjuge+1,9])/(duasvidasY[IdadeConjuge+1,6])
  resultado[3,9]=valorfacebene*(tresvidasZ[IdadeDependente+1,9])/(tresvidasZ[IdadeDependente+1,6])
  resultado[4,9]=valorfacebene*(duasvidasXY[IdadeTitular+1,9])/(duasvidasXY[IdadeTitular+1,6])
  resultado[5,9]=valorfacebene*(tresvidasXZ[IdadeTitular+1,9])/(tresvidasXZ[IdadeTitular+1,6])
  resultado[6,9]=valorfacebene*(tresvidasYZ[IdadeConjuge+1,9])/(tresvidasYZ[IdadeConjuge+1,6])
  resultado[7,9]=valorfacebene*(tresvidasXYZ[IdadeTitular+1,9])/(tresvidasXYZ[IdadeTitular+1,6])
  
  #Seguro Vida Diferido em n anos PUP
  resultado[1,10]=valorfacebene*(duasvidasX[IdadeTitular+Diferimento+1,9])/duasvidasX[IdadeTitular+1,6]
  resultado[2,10]=valorfacebene*(duasvidasY[IdadeConjuge+Diferimento+1,9])/duasvidasY[IdadeConjuge+1,6]
  resultado[3,10]=valorfacebene*(tresvidasZ[IdadeDependente+Diferimento+1,9])/tresvidasZ[IdadeDependente+1,6]
  
  #Seguro Vida Temporario n anos
  resultado[1,11]=valorfacebene*(duasvidasX[IdadeTitular+1,9]-duasvidasX[IdadeTitular+Diferimento+1,9])/duasvidasX[IdadeTitular+1,6]
  resultado[2,11]=valorfacebene*(duasvidasY[IdadeConjuge+1,9]-duasvidasY[IdadeConjuge+Diferimento+1,9])/duasvidasY[IdadeConjuge+1,6]
  resultado[3,11]=valorfacebene*(tresvidasZ[IdadeDependente+1,9]-tresvidasZ[IdadeDependente+Diferimento+1,9])/tresvidasZ[IdadeDependente+1,6]
  
  if(jointlives==1){
    resultado[4,12]=valorfacebene*duasvidasXY[1,7]/duasvidasXY[1,6]
    resultado[5,12]=valorfacebene*tresvidasXZ[1,7]/tresvidasXZ[1,6]
    resultado[6,12]=valorfacebene*tresvidasYZ[1,7]/tresvidasYZ[1,6]
    resultado[7,12]=valorfacebene*tresvidasXYZ[1,7]/tresvidasXYZ[1,6]
  }
  if(lastsurvivor==1){
    resultado[4,13]=valorfacebene*(resultado[1,1]+resultado[2,1]-resultado[4,12])
    resultado[5,13]=valorfacebene*(resultado[1,1]+resultado[3,1]-resultado[5,12])
    resultado[6,13]=valorfacebene*(resultado[2,1]+resultado[3,1]-resultado[6,12])
    #resultado[7,13]=valorfacebene*
  }
  #reversao X para Y
  resultado[4,14]=(resultado[1,2]-resultado[4,12])*coefreversao
  #reversao Y para X
  resultado[5,14]=(resultado[1,1]-resultado[4,12])*coefreversao
  
  resultado<<-resultado  
}

#funcao "main" (unica a ser rodada de fato)

main=function(){
  input()
  variaveis()
  tabuaXYZ()
  #Uma vida
  duasvidasX=UmaVida(duasvidasX,taxajuros=taxajuros,tabua=tabuaX,l0=l0)
  duasvidasY=UmaVida(duasvidasY,taxajuros,tabua=tabuaY,l0=l0)
  tresvidasX=UmaVida(tresvidasX,taxajuros,tabuaX,l0)
  tresvidasY=UmaVida(tresvidasY,taxajuros,tabuaY,l0)
  tresvidasZ=UmaVida(tresvidasZ,taxajuros,tabuaZ,l0)
  #Duas vidas
  duasvidasXY=DuasVidas(duasvidasXY,duasvidas1=duasvidasX,duasvidas2=duasvidasY,taxajuros,l0,Idade1=IdadeTitular,Idade2=IdadeConjuge)
  tresvidasXY=DuasVidas(tresvidasXY,duasvidas1=tresvidasX,duasvidas2=tresvidasY,taxajuros,l0,Idade1=IdadeTitular,Idade2=IdadeConjuge)
  tresvidasYZ=DuasVidas(tresvidasYZ,duasvidas1=tresvidasY,duasvidas2=tresvidasZ,taxajuros,l0,Idade1=IdadeConjuge,Idade2=IdadeDependente)
  tresvidasXZ=DuasVidas(tresvidasXZ,duasvidas1=tresvidasX,duasvidas2=tresvidasZ,taxajuros,l0,Idade1=IdadeTitular,Idade2=IdadeDependente)
  #Tres vidas
  tresvidasXYZ=TresVidas(tresvidasXYZ,tresvidasX,tresvidasY,tresvidasZ,taxajuros,l0)
  calculo()
}

#funcao a ser rodada
main()
