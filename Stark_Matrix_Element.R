#Stark Map Matrix Element

StarkMatrixElem <- function(n1, n2, l1, l2, j1, j2, mj1, mj2, Field){
  if(mj1 != mj2){
    StarkElem <- 0
  } else{
    if((l2==(l1+1))|(l2==(l1-1))){
      
      if(SphereMatElement(l1,l2,mj1+1/2)==0){
        
      SumPlus <- 0
      
      } else{
        
        SumPlus <- Clebsch_Gordan(l1,1/2,mj1+1/2,-1/2,j1,mj1)*Clebsch_Gordan(l2,1/2,mj1+1/2,-1/2,j2,mj1)*SphereMatElement(l1,l2,mj+1/2)
        
      }
      if(SphereMatElement(l1,l2,mj1-1/2)==0){
        
        SumMinus <- 0
        
      } else{
        
        SumMinus <- Clebsch_Gordan(l1,1/2,mj1-1/2,1/2,j1,mj1)*Clebsch_Gordan(l2,1/2,mj1-1/2,1/2,j2,mj1)*SphereMatElement(l1,l2,mj1-1/2)
        
      }
            
      StarkElem <- RadialMatrixElement(1,n1,n2,l1,l2,j1,j2)*Field*(SumPlus + SumMinus)
      
    } else{
      StarkElem <- 0
    }
  }
  
  StarkElem
  
}