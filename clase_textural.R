#TRINAGULO TEXTURA

n_textura<-c("Arcilloso", "Arcillo arenoso","Arcillo limoso",
               "Franco arcillo arenoso","Franco arcilloso","Franco arcillo Limoso",
               "Franco arenoso","Franco","Franco limoso",
               "Arena Franca","Limo", "Arena");n_textura

comment(n_textura)<-'n_textura : nombre de las clases texturales'

arc1<-c(0:15)
arc2<-c(16:20)
arc3<-c(21:55)
arc4<-c(56:100)


r_arc<-30

if((r_arc+r_aren+r_lim)!=100){
  print("Error de ingreso")
}else{
  print("Correcto")
}

c_arc<-arc1/r_arc
ea1<-c_arc[c_arc==1]

c_arc<-arc2/r_arc
ea2<-c_arc[c_arc==1]

c_arc<-arc3/r_arc
ea3<-c_arc[c_arc==1]

c_arc<-arc4/r_arc
ea4<-c_arc[c_arc==1]

ea1==1
ea2==1
ea3==1
ea4==1

if(ea1==1|ea2==1){
  if(ea1==1){
    ea1
  }else{
    ea2
  }
  
}else{
  if(ea3==1){
    ea3
  }else{
    ea4
  }
}






r_aren<-50
r_lim<-20

lim1<-c(0:10)
lim2<-c(11:40)
lim3<-c(41:60)
lim4<-c(61:73)
lim5<-c(74:88)
lim6<-c(89:100)

aren1<-c(0:20)
aren2<-c(21:50)
aren3<-c(51:70)
aren4<-c(71:85)
aren5<-c(86:100)

arcilla<-c(arc1,arc2,arc3,arc4);arcilla
limo<-c(lim1,lim2,lim3,lim4,lim5,lim6);limo
arena<-c(aren1,aren2,aren3,aren4,aren5);arena



