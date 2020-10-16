require(Rcpp); require(lattice); require(animation)
sourceCpp("Functions_Cpp.cpp ")

# инициализация данных
Ampl<-1
Lx<-10; Ly<-10
freq<-1; lambda<-0.9
Ns<-2
Ls<-2.5
deltaS<-Ls/Ns
Npoints<-250
delay.factor=0

omega<-2*pi*freq; v<-lambda*freq

# получаем координаты X и Y
X<-GetGrid(Npoints,Lx); Y<-GetGrid(Npoints,Ly)

# получаем координаты источников
Ys<-rep(0,Ns)
Xs<-(Lx-Ls)/2+(1:Ns)*deltaS
XsVisible<-Xs*Npoints/Lx

# для корректного отображения цветов амплитуд строим набор возможных интенсивностей
at<-seq(from=0,to=Ampl*Ns^2,length.out = 20)

# функция для получения моментов времени для источников
get.time.moments<-function(ti){
  mapply(function(x,y) {x-y},x=rep(ti,Ns),y=0:(Ns-1)*delay.factor)} 

# получение кадров и их соединение в анимацию
ani.options(interval=0.5)
saveGIF({
  for(ti in seq(from=0,to=10,length.out = 10)){
    print(paste("frame",ti,"is done"))
    time.moments<- get.time.moments(ti)
    res<-TotalShift(X,Y,Xs,Ys, time.moments<-get.time.moments(ti), Ampl,omega,v)
    res<-res^2
    print(levelplot(res,
                    panel=function(...){
                      panel.levelplot(...)
                      panel.points(x=XsVisible,y=Ys,pch=16,col="black",cex=1.4)
                    },
                    at=at,col.regions = colorRampPalette(c("white","blue"))(20),
                    main=paste("t=",round(ti,1)))
    )
  }
},paste0("out_intensities.gif"))
