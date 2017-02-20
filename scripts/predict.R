
predictDemo<-function(){


  #####################################################################
  ##################################################################

  multistep<-TRUE

  ser<-c("A","D","leuven","S","henon","chao","rossler","rossler2","lorenz","mkg17","mkg30")
  delays<-c(16,20,50,16,16,16,16,16,16,16,16)  ## embedding order

  dur<-c(100,25,200,22,50,100,100,100,100,100,100) ## length of the prediction window
  st<-rbind(c(0,1179,2869,2999),
            c(0, 99, 199, 299),
            c(0, 200, 400,600),
            c(0,22,23,24),
            c(0,20,30,40),
            c(0,20,30,40),
            c(0,20,30,40),
            c(0,20,30,40),
            c(0,20,30,40),
            c(0,20,30,40),
            c(0,20,30,40))

  if (length(ser)!=length(dur) | NROW(st)!= length(ser) | length(ser)!=length(delays))
    stop("Error")

  ## starting point of the prediction window in *_cont.txt
  algos<-c("comb","prop","lio","loo")

  cnt<-1 ## series counter
  E.all<-NULL
  Y.all<-NULL
  l.all<-NULL
  mean.E.all<-NULL
  for (s in ser[(cnt):length(ser)]){

    DELAY<-delays[cnt]
    name1<-paste("/u/gbonte/datamining/data/tseries/",s,".txt",sep="")
    Ta<-read.table(name1)
    A<-Ta[,1]
    name2<-paste("/u/gbonte/datamining/data/tseries/",s,"_cont.txt",sep="")
    tt<-try(file(name2,open="r"),silent=T)
    if (class(tt)!="try-error"){
      Ta.cont<-read.table(name2)
      A.cont<-Ta.cont[,1]
    } else {
      L<-min(15000,round(length(A)/2))
      A.cont<-A[(L+1):length(A)]
      A<-A[1:L]
    }

    cat("training=",length(A),"test=",length(A.cont),"delay=",DELAY,"\n")
    scal<-T
    adapt<-0

    L<-length(A)

    if (scal){
      scaled.A<-scaling(A)
      ts.train<-scaled.A$scale.data
      mean.data<-scaled.A$mean.data
      std.data<-scaled.A$std.data
    } else
      ts.train<-A

    ts.train<-array(ts.train,c(L,1))
    l.train<-length(ts.train)
    ts.test<-A.cont

    graphical<-F
    mm<-MakeInput(ts.train,DELAY,0)

    data.input1<-mm$inp
    data.output1<-mm$out




    for (iter in 1:NCOL(st)){


      start<-st[cnt,iter]


      if (start<=0){
        TS<-ts.train[1:(l.train+start)]
        q1=rev(ts.train[(l.train-DELAY+1+start):(l.train+start)])
      }else{

        if (scal){
          TS<-c(ts.train,scaling2(ts.test[1:start],mean.data,std.data))
          l.TS<-length(TS)
          q1<-TS[(l.TS-DELAY+1):l.TS]
          q1<-rev(q1)
        }else{
          TS<-c(ts.train,ts.test[1:start])
          l.TS<-length(TS)
          q1<-TS[(l.TS-DELAY+1):l.TS]
          q1<-rev(q1)
        }
      }

      mm<-MakeInput(array(TS,c(length(TS),1)),DELAY,0)

      data.input1<-mm$inp
      data.output1<-mm$out


      Q1<-q1
      for (nit in 1:5){
        E<-NULL
        for (a in algos){
          pred<-NULL
          ind.h<-NULL
          e<-NULL
          it.i<-0
          q1<-Q1
          stop.hor<-FALSE
          for (i in (start+1):(start+dur[cnt])) {
            it.i<-it.i+1

            if (multistep){
              if (a=="lio")
                pred1<-lazylio(data.input1,data.output1,q1,6,20,10,N.iter=nit)
              if (a=="prop")
                pred1<-lazyprop(data.input1,data.output1,q1,6,20,10,N.iter=nit)
              if (a=="loo")
                pred1<-lazyloo(data.input1,data.output1,q1,6,20,10,N.iter=nit)
              if (a=="comb")
                pred1<-mean(c(lazyloo(data.input1,data.output1,q1,6,20,10,N.iter=nit),
                              lazylio(data.input1,data.output1,q1,6,20,10,N.iter=nit),
                              lazyprop(data.input1,data.output1,q1,6,20,10,N.iter=nit)))
              if (graphical){
                TT=50
                ts<-scaling2(ts.test[(i):(i+TT)],mean.data,std.data  )
                lazysee(data.input1,data.output1,q1,ts,pred1,4,TT,most.freq(ind.h,3))

              }


              if (! is.null(ind.h)){

                ind.h<-c(L$ind[1],ind.h+1)
                print(most.freq(ind.h,2))
              } else {
                ind.h<-L$ind.h[1]
              }
              (start+1):i


            }
            else
              pred1<-predict(mod, q1)$h

            if (scal){
              pred<-c(pred,unscalin(pred1,mean.data,std.data))


            } else {
              if (cnt==1)
                pred<-c(pred,max(0,pred1))
              else
                pred<-c(pred,pred1)
            }

            e<-c(e,ts.test[i]-pred[it.i])


            if (multistep){
              q1<-c(pred1, q1)
            }  else {
              if (scal)
                q1<-c(scaling2(ts.test[i],mean.data,std.data), q1)
              else
                q1<-c(ts.test[i], q1)
            }

            NMSE<-sum(e[1:it.i]^2)/((i-start)*sd(ts.test[(start+1):i])^2 )

            if (!is.na(NMSE)){
              if (!stop.hor & NMSE<1){
                hor.pred<-i-start
              }
              if (NMSE>=1)
                stop.hor<-TRUE
            }

            if (i>=start+dur[cnt]){

              RMSE<-sqrt(sum(e[1:it.i]^2)/(i-start) )

              cat("series",s,"part=",iter,
                  "algo=",a,"nit=",nit," it.i=",it.i,"NMSE=",NMSE,
                  "\n RMSE=",RMSE,"MSE=",RMSE^2,"MAE=", mean(abs(e[1:it.i])),
                  "hor=",hor.pred,"\n")
              if (T){
                plot(ts.test[(start+1):i],type="l")
                lines(pred,col="red")
              }


            }

            q1<-q1[1:DELAY]


          }

          E<-cbind(E,e)
        } ## for (a in algos
        ##E<-cbind(E,apply(E,1,mean))


        Y.all<-c(Y.all,ts.test[(start+1):i])
        l.all<-c(l.all,i-start)

        E.all<-rbind(E.all,abs(E))
        colnames(E.all)<-algos
        print(dim(E.all))
        print(apply(E.all,2,mean))
        print(friedmao(E.all,post=TRUE)$C)

        mean.E.all<-rbind(mean.E.all,apply(abs(E),2,mean))
        rownames(mean.E.all)[NROW(mean.E.all)]<-paste(s,iter,nit,sep=".")
        ## colnames(mean.E.all)<-c(algos,"cmb")
        colnames(mean.E.all)<-algos
        print(dim(mean.E.all))
        print(apply(mean.E.all,2,mean))
        print(friedmao(mean.E.all,post=TRUE)$C)

        save(file="out.Rdata",list=c("E.all","mean.E.all","Y.all","l.all"))
      } # for nit
    } ## for iter
    cnt<-cnt+1
  } ## for s


}
