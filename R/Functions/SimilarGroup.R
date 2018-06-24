
#fun_dist <- function(a, M) sqrt(rowSums( (M - matrix(1,nrow(M),1) %*% as.numeric(a))**2 ))
fun_dist <- function(a, M) sqrt(rowSums( (M - matrix(1,nrow(M),1) %*% as.numeric(a))^2 ))


findSimilarGroup <- function(Non_Promo_Group, Promo_Group,GroupNbr, variablesToInclude,scale=TRUE) {
  Non_Promo_Group<-as.data.frame(Non_Promo_Group)
  Promo_Group<-as.data.frame(Promo_Group)
  fulldataset<-rbind(Non_Promo_Group,Promo_Group)
  GroupNbrCol <- which(colnames(fulldataset) %in% GroupNbr)
  columnsToScale <- which(colnames(fulldataset) %in% variablesToInclude)
  if(scale){
    for(k in 1:length(columnsToScale)){
      fulldataset[,columnsToScale[k]] <- scale(fulldataset[,columnsToScale[k]])
    }
  }
  Non_Promo_Group_Scale<-fulldataset[which(fulldataset[,GroupNbrCol] %in% Non_Promo_Group[,GroupNbrCol]),]
  Promo_Group_Scale<-fulldataset[which(fulldataset[,GroupNbrCol]%in%Promo_Group[,GroupNbrCol]),]
  Non_Promo_Group_Scale$SimGroup<-0
  for(i in 1:nrow(Non_Promo_Group_Scale)){
    Non_Promo_Group_Scale[i,]$SimGroup<-Promo_Group_Scale[which.min(fun_dist(Non_Promo_Group_Scale[i,columnsToScale], 
                                                                             Promo_Group_Scale[,columnsToScale])),GroupNbrCol]
    Promo_Group_Scale<-Promo_Group_Scale[which(!(Promo_Group_Scale[,GroupNbrCol]%in%Non_Promo_Group_Scale$SimGroup)),]
  }
  return(Non_Promo_Group_Scale)
} 

findSimilarGroup_slow <- function(group1, group2) {
  group2$distance<-0
  group1$SimGroup<-0
  for(i in 1:nrow(group1)){
    for(j in 1:nrow(group2)){
      group2[j,"distance"]<-as.numeric(fun_dist(group1[i,3:4], group2[j,3:4]))
    }
    group1[i,]$SimGroup<-group2[which.min(group2$distance),"Group_Nbr"]
    group2<-group2[which(!(group2$Group_Nbr%in%group1$SimGroup)),]
  }
  return(group1)
}
