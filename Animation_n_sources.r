require(Rcpp); require(lattice); require(animation)
sourceCpp("Functions_Cpp.cpp")

# инициализация данных
Ampl<-1
Lx<-20; Ly<-20
freq<-1; lambda<-0.25
Ns<-40; Ls<-2.5
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

# для корректного отображения амплитуд строим набор возможных амплитуд
at<-seq(from=0,to=Ampl*Ns^2,length.out = 20)

# функция для получения моментов времени для источников
get.time.moments<-function(ti){
  mapply(function(x,y) {x-y},x=rep(ti,round(Ns)),y=0:(round(Ns)-1)*delay.factor)
} 

ani.options(interval=0.5)
saveGIF({
  for(ti in seq(from=0,to=80,length.out = 20)){
    print(paste("frame",ti,"is done"))
    time.moments<- get.time.moments(ti)
    res<-TotalShift(X,Y,Xs,Ys, time.moments<-get.time.moments(ti), Ampl,omega,v)
    res<-res^2
    m<-max(res)
    at<-seq(from=0,to=ifelse(m==0,1,m),length.out = 20)
    print(levelplot(res,
                    panel=function(...){
                      panel.levelplot(...)
                      panel.points(x=XsVisible,y=Ys,pch=16,col="black",cex=1.4)
                    },
                    at=at,col.regions = colorRampPalette(c("white","darkgreen"))(20),
                    main=bquote( t[i]==~.(round(ti,2))~"," ~N[s]==.(Ns)~","~lambda==.(lambda) ) ) )
    }
},paste0("out_n_sources.gif"))
