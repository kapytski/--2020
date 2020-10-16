require(Rcpp)
require(lattice)
# require(animation)
sourceCpp("Functions_Cpp.cpp")

# инициализация данных

Ampl<-1
Lx<-20
Ly<-20
freq<-1
lambda<-0.25
Ns<-40
Ls<-2.5
deltaS<-Ls/Ns
Npoints<-250
Nt<-20
Tscan<-10
delay.factor=1

omega<-2*pi*freq
v<-lambda*freq

# получаем координаты X и Y
X<-GetGrid(Npoints,Lx)
Y<-GetGrid(Npoints,Ly)

# получаем координаты источников
Ys<-rep(0,Ns)
Xs<-(Lx-Ls)/2+(1:Ns)*deltaS
XsVisible<-Xs*Npoints/Lx

# для корректного отображения амплитуд строим набор возможных амплитуд
at<-seq(from=-Ampl*Ns,to=Ampl*Ns,length.out = 20)

at<-seq(from=0,to=Ampl*Ns^2,length.out = 20)
m<-max(at)
k<-0
# функция для получения моментов времени для источников

get.time.moments<-function(k){
  ti=40
  temp<-mapply(function(x,y) {x-y},x=rep(ti,round(Ns)),y=0:(round(Ns)-1)*delay.factor)
  # делаем "кольцевой вектор"
  koef<-(sin(pi*k/Nt)*0.125)
  temp*koef+2*ti
}


ani.options(interval=0.25)
saveGIF({

  for(k in 1:(2*Nt)){
    # print(paste("frame",ti,"is done"))
    time.moments<- get.time.moments(k)
    # print(round(time.moments))
    res<-TotalShift(X,Y,Xs,Ys,
                    time.moments<-time.moments, 
                    Ampl,omega,v)
    res<-res^2
    m<-max(res)
    
    if(m==0){
      at<-seq(from=0,to=1,length.out = 20)
    } else {
      at<-seq(from=0,to=m,length.out = 20)
      # print(at)
    }
    
   print(levelplot(res,
                    panel=function(...){
                      panel.levelplot(...)
                      panel.points(x=XsVisible,y=Ys,pch=16,col="black",cex=1.4)
                    },
                    at=at,col.regions = colorRampPalette(c("white","darkgreen"))(20),
                    # main=bquote( t[i]==~.(round(ti,2))~","
                    #              ~N[s]==.(Ns)~","
                    #              ~lambda==.(lambda) ) 
                    main=bquote( t[i]==~.(40+k)~","
                                 ~N[s]==.(Ns)~","
                                 ~lambda==.(lambda) ) 
    )
    )
    
  }
  
  
  
},paste0("scan motion.gif."))
