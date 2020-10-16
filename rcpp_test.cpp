#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector GetGrid(unsigned short int N, float L){
  float Delta=L/N;
  NumericVector result(N);
  
  for(int i=0;i<N;++i){
    result[i]=Delta*(i);
  }
  return result;
  
}

// [[Rcpp::export]]
float Distance(float X1,float Y1, float X2, float Y2) {
  return sqrt( pow(X1-X2,2)+pow(Y1-Y2,2) );
}

// [[Rcpp::export]]
float GetShift0(float X, float Y, float XSource, float YSource, float Time,
               float Ampl, float Omega, float Vel){
  float d=Distance(X,Y,XSource,YSource);
  float arg=0;
  if(d==0){
    return 0;
  } else {
    arg=Time-d/Vel;
    if(arg<0){
      return 0;
    } else{
      return Ampl*sin(Omega*arg);
    }
      
  }  
}

// [[Rcpp::export]]
float GetShift(float X, float Y, float XSource, float YSource, float Time,
               float Ampl, float Omega, float Vel){
  float d=Distance(X,Y,XSource,YSource);
  if(d==0){
    return 0;
  } else {
    float arg=Time-d/Vel;
    return Ampl*sin(Omega*arg);
  }  
}

// [[Rcpp::export]]
NumericMatrix TotalShift(NumericVector X, NumericVector Y,NumericVector XSource, NumericVector YSource,
                         NumericVector TimeSource, float Ampl, float Omega, float Vel){
  short int Nx=X.size();
  short int Ny=Y.size();
  short int NSources=XSource.size();
  float s=0;
  
  NumericMatrix result(Nx,Ny);
  
  for(short int i=0;i<Nx;i++){
    for(short int j=0;j<Ny;j++){
      s=0;
      for(short int k=0;k<NSources;k++){
        s=s+GetShift0(X[i],Y[j],XSource[k],YSource[k],TimeSource[k],Ampl,Omega,Vel);
      }
      result(i,j)=s;
    }
  }
  
  return result;
  
  
}





