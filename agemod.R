d<-read.csv("framingham1.csv",header=TRUE)
dim(d)
d$age<-as.numeric(d$age)
#min age=35 and max age=70
count1<-c(0,0,0,0,0,0,0,0);
count2<-c(0,0,0,0,0,0,0,0);
for(i in 1:4240)
{  
   if(d[i,2]>=35&&d[i,2]<40){
     count1[1]<-count1[1]+1;
     if(d[i,16]==1){
       count2[1]<-count2[1]+1;
     }
   }
   else if(d[i,2]>=40&&d[i,2]<45){
     count1[2]<-count1[2]+1;
     if(d[i,16]==1){
       count2[2]<-count2[2]+1;
     }
   }
   else if(d[i,2]>=45&&d[i,2]<50){
     count1[3]<-count1[3]+1;
     if(d[i,16]==1){
       count2[3]<-count2[3]+1;
     }
   }
   else if(d[i,2]>=50&&d[i,2]<55){
     count1[4]<-count1[4]+1;
     if(d[i,16]==1){
       count2[4]<-count2[4]+1;
     }
   }
   else if(d[i,2]>=55&&d[i,2]<60){
     count1[5]<-count1[5]+1;
     if(d[i,16]==1){
       count2[5]<-count2[5]+1;
     }
   }
   else if(d[i,2]>=60&&d[i,2]<65){
     count1[6]<-count1[6]+1;
     if(d[i,16]==1){
       count2[6]<-count2[6]+1;
     }
   }
   else if(d[i,2]>=65&&d[i,2]<70){
     count1[7]<-count1[7]+1;
     if(d[i,16]==1){
       count2[7]<-count2[7]+1;
     }
   }
   else if(d[i,2]>=70&&d[i,2]<80){
     count1[8]<-count1[8]+1;
     if(d[i,16]==1){
       count2[8]<-count2[8]+1;
     }
   }
   i
}
count1
count2
per<-c(0,0,0,0,0,0,0,0)
for(i in 1:8)
  per[i]<-(count2[i]/count1[i])
per<-per*100
a<-1:8
plot(a,per)
plot(per,a)
plot(per,a,type="b")
plot(a,per,type="l")
plot(per,a,type="l")
plot(a,per,type="b")
