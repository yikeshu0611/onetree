#' To do normal distrbution test
#'
#' @param x must be numeric or integer
#'
#' @return test results
#' @export
#'
#' @examples Normaldistr_test(rnorm(100))
Normaldistr_test<-function(x){
  x=na.omit(x)
  if (is.numeric(x) | is.integer(x)){
    #if min(x)>0
    if (min(x)>0){
      x1<-x
      x2<-x^2
      x3<-x^3
      x4<-sqrt(x)
      x5<-1/x
      x6<-1/(x^2)
      x7<-1/(x^3)
      x8<-1/sqrt(x)
      x9<-log(x)

      transformation<-c("itself","square","cubic","square root","inverse","1/square","1/cubic","1/square root","log")
      formula<-c("x","x^2","x^3","x^1/2","1/x","1/(x^2)","1/(x^3)","1/(x^1/2)","log(x)")

      skew=c()
      kurt=c()
      m <- mean(x1)
      n <- length(x1)
      s <- sd(x1)
      skew1 <- sum((x1-m)^3/s^3)/n
      kurt1 <- sum((x1-m)^4/s^4)/n - 3
      skew=rbind(skew,skew1)
      kurt=rbind(kurt,kurt1)

      m <- mean(x2)
      n <- length(x2)
      s <- sd(x2)
      skew2 <- sum((x2-m)^3/s^3)/n
      kurt2 <- sum((x2-m)^4/s^4)/n - 3
      skew=rbind(skew,skew2)
      kurt=rbind(kurt,kurt2)

      m <- mean(x3)
      n <- length(x3)
      s <- sd(x3)
      skew3 <- sum((x3-m)^3/s^3)/n
      kurt3 <- sum((x3-m)^4/s^4)/n - 3
      skew=rbind(skew,skew3)
      kurt=rbind(kurt,kurt3)

      m <- mean(x4)
      n <- length(x4)
      s <- sd(x4)
      skew4 <- sum((x4-m)^3/s^3)/n
      kurt4 <- sum((x4-m)^4/s^4)/n - 3
      skew=rbind(skew,skew4)
      kurt=rbind(kurt,kurt4)

      m <- mean(x5)
      n <- length(x5)
      s <- sd(x5)
      skew5 <- sum((x5-m)^3/s^3)/n
      kurt5 <- sum((x5-m)^4/s^4)/n - 3
      skew=rbind(skew,skew5)
      kurt=rbind(kurt,kurt5)

      m <- mean(x6)
      n <- length(x6)
      s <- sd(x6)
      skew6 <- sum((x6-m)^3/s^3)/n
      kurt6 <- sum((x6-m)^4/s^4)/n - 3
      skew=rbind(skew,skew6)
      kurt=rbind(kurt,kurt6)

      m <- mean(x7)
      n <- length(x7)
      s <- sd(x7)
      skew7 <- sum((x7-m)^3/s^3)/n
      kurt7 <- sum((x7-m)^4/s^4)/n - 3
      skew=rbind(skew,skew7)
      kurt=rbind(kurt,kurt7)

      m <- mean(x8)
      n <- length(x8)
      s <- sd(x8)
      skew8 <- sum((x8-m)^3/s^3)/n
      kurt8 <- sum((x8-m)^4/s^4)/n - 3
      skew=rbind(skew,skew8)
      kurt=rbind(kurt,kurt8)

      m <- mean(x9)
      n <- length(x9)
      s <- sd(x9)
      skew9 <- sum((x9-m)^3/s^3)/n
      kurt9 <- sum((x9-m)^4/s^4)/n - 3
      skew=rbind(skew,skew9)
      kurt=rbind(kurt,kurt9)


      skew<-round(skew,3)
      names(skew)[1]<-c("skew")
      skew<-data.frame(skew)

      kurt<-round(kurt,3)
      names(kurt)[1]<-c("kurt")
      kurt<-data.frame(kurt)

      if (length(x)<=5000){
        #Shapiro-Wilk
        p_value1=c()


        result_shapiro<-shapiro.test(x1)
        p_value1<-rbind(p_value1,result_shapiro$p.value)



        result_shapiro<-shapiro.test(x2)
        p_value1<-rbind(p_value1,result_shapiro$p.value)



        result_shapiro<-shapiro.test(x3)
        p_value1<-rbind(p_value1,result_shapiro$p.value)


        result_shapiro<-shapiro.test(x4)
        p_value1<-rbind(p_value1,result_shapiro$p.value)


        result_shapiro<-shapiro.test(x5)
        p_value1<-rbind(p_value1,result_shapiro$p.value)


        result_shapiro<-shapiro.test(x6)
        p_value1<-rbind(p_value1,result_shapiro$p.value)


        result_shapiro<-shapiro.test(x7)
        p_value1<-rbind(p_value1,result_shapiro$p.value)


        result_shapiro<-shapiro.test(x8)
        p_value1<-rbind(p_value1,result_shapiro$p.value)


        result_shapiro<-shapiro.test(x9)
        p_value1<-rbind(p_value1,result_shapiro$p.value)


        p_value1<-round(p_value1,5)
        names(p_value1)[1]<-c("p_value1")
        p_value1<-data.frame(p_value1)
        # Kolmogorov-Smirnov
        p_value2=c()
        result_ks<-suppressWarnings(ks.test(x1,"pnorm",mean(x1),sqrt(var(x1))))
        p_value2<-rbind(p_value2,result_ks$p.value)

        result_ks<-suppressWarnings(ks.test(x2,"pnorm",mean(x2),sqrt(var(x2))))
        p_value2<-rbind(p_value2,result_ks$p.value)

        result_ks<-suppressWarnings(ks.test(x3,"pnorm",mean(x3),sqrt(var(x3))))
        p_value2<-rbind(p_value2,result_ks$p.value)

        result_ks<-suppressWarnings(ks.test(x4,"pnorm",mean(x4),sqrt(var(x4))))
        p_value2<-rbind(p_value2,result_ks$p.value)

        result_ks<-suppressWarnings(ks.test(x5,"pnorm",mean(x5),sqrt(var(x5))))
        p_value2<-rbind(p_value2,result_ks$p.value)

        result_ks<-suppressWarnings(ks.test(x6,"pnorm",mean(x6),sqrt(var(x6))))
        p_value2<-rbind(p_value2,result_ks$p.value)

        result_ks<-suppressWarnings(ks.test(x7,"pnorm",mean(x7),sqrt(var(x7))))
        p_value2<-rbind(p_value2,result_ks$p.value)

        result_ks<-suppressWarnings(ks.test(x8,"pnorm",mean(x8),sqrt(var(x8))))
        p_value2<-rbind(p_value2,result_ks$p.value)

        result_ks<-suppressWarnings(ks.test(x9,"pnorm",mean(x9),sqrt(var(x9))))
        p_value2<-rbind(p_value2,result_ks$p.value)

        p_value2<-round(p_value2,5)
        names(p_value2)[1]<-c("p_value2")
        p_value2<-data.frame(p_value2)

        results_norm<-cbind(transformation,formula,kurt,skew,p_value1,p_value2)

        cat("      Normal transformation and test")
        cat("\n")
        cat("\n")
        rownames(results_norm)=NULL
        print(results_norm)
        cat("\n")
        cat("sample size is",length(x),"\n")
        cat("\n")
        cat(" p_value1:test by Shapiro-Wilk(sample size:3~5000)","\n","p_value2:test by Kolmogorov-Smirnov(sample size:>5000)","\n")


      }else{

        # Kolmogorov-Smirnov
        p_value2=c()

        result_ks<-suppressWarnings(ks.test(x1,"pnorm",mean(x1),sqrt(var(x1))))
        p_value2<-rbind(p_value2,result_ks$p.value)

        result_ks<-suppressWarnings(ks.test(x2,"pnorm",mean(x2),sqrt(var(x2))))
        p_value2<-rbind(p_value2,result_ks$p.value)

        result_ks<-suppressWarnings(ks.test(x3,"pnorm",mean(x3),sqrt(var(x3))))
        p_value2<-rbind(p_value2,result_ks$p.value)

        result_ks<-suppressWarnings(ks.test(x4,"pnorm",mean(x4),sqrt(var(x4))))
        p_value2<-rbind(p_value2,result_ks$p.value)

        result_ks<-suppressWarnings(ks.test(x5,"pnorm",mean(x5),sqrt(var(x5))))
        p_value2<-rbind(p_value2,result_ks$p.value)

        result_ks<-suppressWarnings(ks.test(x6,"pnorm",mean(x6),sqrt(var(x6))))
        p_value2<-rbind(p_value2,result_ks$p.value)

        result_ks<-suppressWarnings(ks.test(x7,"pnorm",mean(x7),sqrt(var(x7))))
        p_value2<-rbind(p_value2,result_ks$p.value)

        result_ks<-suppressWarnings(ks.test(x8,"pnorm",mean(x8),sqrt(var(x8))))
        p_value2<-rbind(p_value2,result_ks$p.value)

        result_ks<-suppressWarnings(ks.test(x9,"pnorm",mean(x9),sqrt(var(x9))))
        p_value2<-rbind(p_value2,result_ks$p.value)

        p_value2<-round(p_value2,5)
        names(p_value2)[1]<-c("p_value2")
        p_value2<-data.frame(p_value2)

        results_norm<-cbind(transformation,formula,kurt,skew,p_value2)

        cat("      Normal transformation and test")
        cat("\n")
        rownames(results_norm)=NULL
        print(results_norm)
        cat("\n")
        cat("sample size is",length(x),"\n")
        cat("","\n","p_value2:test by Kolmogorov-Smirnov(sample size:>5000)","\n")

      }



      ####picture
      #x1
      dev.new()
      par(mfrow=c(2,2),oma = c(2, 0, 3, 0))
      #p1
      hist(x,  freq=FALSE, main = "Histogram of x", xlab = "x")
      lines(density(x1),col="blue")

      #p2
      x_sequence=1:length(x)
      plot(x_sequence,x, main = "Singal x scatter plot", xlab = "sequence of x", ylab = "x")

      #p3
      qqnorm(x,main = "qqplot of x")
      qqline(x)

      #p4
      boxplot(x,main="boxplot of x")

      mtext("Statistical Plots of NTT:x", font = 2, outer=TRUE)


      #x2
      dev.new()

      par(mfrow=c(2,2),oma = c(2, 0, 3, 0))
      ##p1
      hist(x2,  freq=FALSE, main = "Histogram of x^2", xlab = "x^2")
      lines(density(x2),col="blue")

      #p2
      x2_sequence=1:length(x2)
      plot(x2_sequence,x2, main = "Singal x^2 scatter plot", xlab = "sequence of x^2", ylab = "x^2")

      #p3
      qqnorm(x2,main = "qqplot of x^2")
      qqline(x2)

      #p4
      boxplot(x2,main="boxplot of x^2")

      mtext("Statistical Plots of NTT:x^2", font = 2, outer=TRUE)

      #x3
      dev.new()

      par(mfrow=c(2,2),oma = c(2, 0, 3, 0))
      ##p1
      hist(x3,  freq=FALSE, main = "Histogram of x^3", xlab = "x^3")
      lines(density(x3),col="blue")

      #p2
      x3_sequence=1:length(x3)
      plot(x3_sequence,x3, main = "Singal x^3 scatter plot", xlab = "sequence of x^3", ylab = "x^3")

      #p3
      qqnorm(x3,main = "qqplot of x^3")
      qqline(x3)

      #p4
      boxplot(x3,main="boxplot of x^3")

      mtext("Statistical Plots of NTT:x^3", font = 2, outer=TRUE)

      #x4
      dev.new()

      par(mfrow=c(2,2),oma = c(2, 0, 3, 0))
      ##p1
      hist(x4,  freq=FALSE, main = "Histogram of sqrt(x)", xlab = "sqrt(x)")
      lines(density(x4),col="blue")


      #p2
      x4_sequence=1:length(x4)
      plot(x4_sequence,x4, main = "Singal sqrt(x) scatter plot", xlab = "sequence of sqrt(x)", ylab = "sqrt(x)")

      #p3
      qqnorm(x4,main = "qqplot of sqrt(x)")
      qqline(x4)

      #p4
      boxplot(x4,main="boxplot of sqrt(x)")

      mtext("Statistical Plots of NTT:sqrt(x)", font = 2, outer=TRUE)

      #x5
      dev.new()

      par(mfrow=c(2,2),oma = c(2, 0, 3, 0))
      ##p1
      hist(x5,  freq=FALSE, main = "Histogram of 1/x", xlab = "1/x")
      lines(density(x5),col="blue")


      #p2
      x5_sequence=1:length(x5)
      plot(x5_sequence,x5, main = "Singal 1/x scatter plot", xlab = "sequence of 1/x", ylab = "1/x")

      #p3
      qqnorm(x5,main = "qqplot of 1/x")
      qqline(x5)

      #p4
      boxplot(x5,main="boxplot of 1/x")

      mtext("Statistical Plots of NTT:1/x", font = 2, outer=TRUE)

      #x6
      dev.new()

      par(mfrow=c(2,2),oma = c(2, 0, 3, 0))
      ##p1
      hist(x6,freq=FALSE, main = "Histogram of 1/(x^2)", xlab = "1/(x^2)")
      lines(density(x6),col="blue")


      #p2
      x6_sequence=1:length(x6)
      plot(x6_sequence,x6, main = "Singal 1/(x^2) scatter plot", xlab = "sequence of 1/(x^2)", ylab = "1/(x^2)")

      #p3
      qqnorm(x6,main = "qqplot of 1/(x^2)")
      qqline(x6)

      #p4
      boxplot(x6,main="boxplot of 1/(x^2)")

      mtext("Statistical Plots of NTT:1/(x^2)", font = 2, outer=TRUE)

      #x7
      dev.new()

      par(mfrow=c(2,2),oma = c(2, 0, 3, 0))
      ##p1
      hist(x7,  freq=FALSE, main = "Histogram of 1/(x^3)", xlab = "1/(x^3)")
      lines(density(x7),col="blue")


      #p2
      x7_sequence=1:length(x7)
      plot(x7_sequence,x7, main = "Singal 1/(x^3) scatter plot", xlab = "sequence of 1/(x^3)", ylab = "1/(x^3)")

      #p3
      qqnorm(x7,main = "qqplot of 1/(x^3)")
      qqline(x7)

      #p4
      boxplot(x7,main="boxplot of 1/(x^3)")

      mtext("Statistical Plots of NTT:1/(x^3)", font = 2, outer=TRUE)

      #x8
      dev.new()

      par(mfrow=c(2,2),oma = c(2, 0, 3, 0))
      ##p1
      hist(x8,  freq=FALSE, main = "Histogram of 1/sqrt(x)", xlab = "1/sqrt(x)")
      lines(density(x8),col="blue")


      #p2
      x8_sequence=1:length(x8)
      plot(x8_sequence,x8, main = "Singal 1/sqrt(x) scatter plot", xlab = "sequence of 1/sqrt(x)", ylab = "1/sqrt(x)")

      #p3
      qqnorm(x8,main = "qqplot of 1/sqrt(x)")
      qqline(x8)

      #p4
      boxplot(x8,main="boxplot of 1/sqrt(x)")

      mtext("Statistical Plots of NTT:1/sqrt(x)", font = 2, outer=TRUE)

      #x9
      dev.new()

      par(mfrow=c(2,2),oma = c(2, 0, 3, 0))
      ##p1
      hist(x9,  freq=FALSE, main = "Histogram of log(x)", xlab = "log(x)")
      lines(density(x9),col="blue")


      #p2
      x9_sequence=1:length(x9)
      plot(x9_sequence,x9, main = "Singal log(x) scatter plot", xlab = "sequence of log(x)", ylab = "log(x)")

      #p3
      qqnorm(x9,main = "qqplot of log(x)")
      qqline(x9)

      #p4
      boxplot(x9,main="boxplot of log(x)")

      mtext("Statistical Plots of NTT:log(x)", font = 2, outer=TRUE)

    }
    #if min(x)>>>>>>>>>>>>>>>>>>>>>>>>>>>000000000000000000000000000000000000000000000000

    #if min(x)=======================================000000000000000000000000000000000000000000000000,cut x5 x6 x7 x8 x9
    if (min(x)==0){
      x1<-x
      x2<-x^2
      x3<-x^3
      x4<-sqrt(x)
      transformation<-c("itself","square","cubic","square root")
      formula<-c("x","x^2","x^3","x^1/2")

      #acquir skew and kurt
      skew=c()
      kurt=c()

      m <- mean(x1)
      n <- length(x1)
      s <- sd(x1)
      skew1 <- sum((x1-m)^3/s^3)/n
      kurt1 <- sum((x1-m)^4/s^4)/n - 3
      skew=rbind(skew,skew1)
      kurt=rbind(kurt,kurt1)

      m <- mean(x2)
      n <- length(x2)
      s <- sd(x2)
      skew2 <- sum((x2-m)^3/s^3)/n
      kurt2 <- sum((x2-m)^4/s^4)/n - 3
      skew=rbind(skew,skew2)
      kurt=rbind(kurt,kurt2)

      m <- mean(x3)
      n <- length(x3)
      s <- sd(x3)
      skew3 <- sum((x3-m)^3/s^3)/n
      kurt3 <- sum((x3-m)^4/s^4)/n - 3
      skew=rbind(skew,skew3)
      kurt=rbind(kurt,kurt3)

      m <- mean(x4)
      n <- length(x4)
      s <- sd(x4)
      skew4 <- sum((x4-m)^3/s^3)/n
      kurt4 <- sum((x4-m)^4/s^4)/n - 3
      skew=rbind(skew,skew4)
      kurt=rbind(kurt,kurt4)


      skew<-round(skew,3)
      names(skew)[1]<-c("skew")
      skew<-data.frame(skew)

      kurt<-round(kurt,3)
      names(kurt)[1]<-c("kurt")
      kurt<-data.frame(kurt)

      #do test
      if (length(x)<=5000){
        #Shapiro-Wilk
        p_value1=c()


        result_shapiro<-shapiro.test(x1)
        p_value1<-rbind(p_value1,result_shapiro$p.value)



        result_shapiro<-shapiro.test(x2)
        p_value1<-rbind(p_value1,result_shapiro$p.value)



        result_shapiro<-shapiro.test(x3)
        p_value1<-rbind(p_value1,result_shapiro$p.value)


        result_shapiro<-shapiro.test(x4)
        p_value1<-rbind(p_value1,result_shapiro$p.value)


        p_value1<-round(p_value1,5)
        names(p_value1)[1]<-c("p_value1")
        p_value1<-data.frame(p_value1)



        # Kolmogorov-Smirnov

        p_value2=c()

        result_ks<-suppressWarnings(ks.test(x1,"pnorm",mean(x1),sqrt(var(x1))))
        p_value2<-rbind(p_value2,result_ks$p.value)

        result_ks<-suppressWarnings(ks.test(x2,"pnorm",mean(x2),sqrt(var(x2))))
        p_value2<-rbind(p_value2,result_ks$p.value)

        result_ks<-suppressWarnings(ks.test(x3,"pnorm",mean(x3),sqrt(var(x3))))
        p_value2<-rbind(p_value2,result_ks$p.value)

        result_ks<-suppressWarnings(ks.test(x4,"pnorm",mean(x4),sqrt(var(x4))))
        p_value2<-rbind(p_value2,result_ks$p.value)


        p_value2<-round(p_value2,5)
        names(p_value2)[1]<-c("p_value2")
        p_value2<-data.frame(p_value2)

        results_norm<-cbind(transformation,formula,kurt,skew,p_value1,p_value2)

        cat("      Normal transformation and test")
        cat("\n")
        cat("\n")
        rownames(results_norm)=NULL
        print(results_norm)
        cat("\n")
        cat("sample size is",length(x),"\n")
        cat("\n")
        cat(" p_value1:test by Shapiro-Wilk(sample size:3~5000)","\n","p_value2:test by Kolmogorov-Smirnov(sample size:>5000)","\n")


      }else{

        # Kolmogorov-Smirnov
        p_value2=c()

        result_ks<-suppressWarnings(ks.test(x1,"pnorm",mean(x1),sqrt(var(x1))))
        p_value2<-rbind(p_value2,result_ks$p.value)

        result_ks<-suppressWarnings(ks.test(x2,"pnorm",mean(x2),sqrt(var(x2))))
        p_value2<-rbind(p_value2,result_ks$p.value)

        result_ks<-suppressWarnings(ks.test(x3,"pnorm",mean(x3),sqrt(var(x3))))
        p_value2<-rbind(p_value2,result_ks$p.value)

        result_ks<-suppressWarnings(ks.test(x4,"pnorm",mean(x4),sqrt(var(x4))))
        p_value2<-rbind(p_value2,result_ks$p.value)


        p_value2<-round(p_value2,5)
        names(p_value2)[1]<-c("p_value2")
        p_value2<-data.frame(p_value2)

        results_norm<-cbind(transformation,formula,kurt,skew,p_value2)

        cat("      Normal transformation and test")
        cat("\n")
        rownames(results_norm)=NULL
        print(results_norm)
        cat("\n")
        cat("sample size is",length(x),"\n")
        cat("","\n","p_value2:test by Kolmogorov-Smirnov(sample size:>5000)","\n")

      }



      ####picture
      #x1
      dev.new()
      par(mfrow=c(2,2),oma = c(2, 0, 3, 0))
      #p1
      hist(x,  freq=FALSE, main = "Histogram of x", xlab = "x")
      lines(density(x1),col="blue")

      #p2
      x_sequence=1:length(x)
      plot(x_sequence,x, main = "Singal x scatter plot", xlab = "sequence of x", ylab = "x")

      #p3
      qqnorm(x,main = "qqplot of x")
      qqline(x)

      #p4
      boxplot(x,main="boxplot of x")

      mtext("Statistical Plots of NTT:x", font = 2, outer=TRUE)


      #x2
      dev.new()

      par(mfrow=c(2,2),oma = c(2, 0, 3, 0))
      ##p1
      hist(x2,  freq=FALSE, main = "Histogram of x^2", xlab = "x^2")
      lines(density(x2),col="blue")

      #p2
      x2_sequence=1:length(x2)
      plot(x2_sequence,x2, main = "Singal x^2 scatter plot", xlab = "sequence of x^2", ylab = "x^2")

      #p3
      qqnorm(x2,main = "qqplot of x^2")
      qqline(x2)

      #p4
      boxplot(x2,main="boxplot of x^2")

      mtext("Statistical Plots of NTT:x^2", font = 2, outer=TRUE)

      #x3
      dev.new()

      par(mfrow=c(2,2),oma = c(2, 0, 3, 0))
      ##p1
      hist(x3,  freq=FALSE, main = "Histogram of x^3", xlab = "x^3")
      lines(density(x3),col="blue")

      #p2
      x3_sequence=1:length(x3)
      plot(x3_sequence,x3, main = "Singal x^3 scatter plot", xlab = "sequence of x^3", ylab = "x^3")

      #p3
      qqnorm(x3,main = "qqplot of x^3")
      qqline(x3)

      #p4
      boxplot(x3,main="boxplot of x^3")

      mtext("Statistical Plots of NTT:x^3", font = 2, outer=TRUE)

      #x4
      dev.new()

      par(mfrow=c(2,2),oma = c(2, 0, 3, 0))
      ##p1
      hist(x4,  freq=FALSE, main = "Histogram of sqrt(x)", xlab = "sqrt(x)")
      lines(density(x4),col="blue")


      #p2
      x4_sequence=1:length(x4)
      plot(x4_sequence,x4, main = "Singal sqrt(x) scatter plot", xlab = "sequence of sqrt(x)", ylab = "sqrt(x)")

      #p3
      qqnorm(x4,main = "qqplot of sqrt(x)")
      qqline(x4)

      #p4
      boxplot(x4,main="boxplot of sqrt(x)")

      mtext("Statistical Plots of NTT:sqrt(x)", font = 2, outer=TRUE)

    }
    #if min(x)=======================================000000000000000000000000000000000000000000000000,cut x5 x6 x7 x8 x9

    ##min(x)<0 & max(x)>0 & 0 %in% x  cut x4 x5 x6 x7 x8 x9
    if (min(x)<0 & max(x)>0 & 0 %in% x){
      x1<-x
      x2<-x^2
      x3<-x^3
      transformation<-c("itself","square","cubic")
      formula<-c("x","x^2","x^3")

      #acquir skew and kurt
      skew=c()
      kurt=c()

      m <- mean(x1)
      n <- length(x1)
      s <- sd(x1)
      skew1 <- sum((x1-m)^3/s^3)/n
      kurt1 <- sum((x1-m)^4/s^4)/n - 3
      skew=rbind(skew,skew1)
      kurt=rbind(kurt,kurt1)

      m <- mean(x2)
      n <- length(x2)
      s <- sd(x2)
      skew2 <- sum((x2-m)^3/s^3)/n
      kurt2 <- sum((x2-m)^4/s^4)/n - 3
      skew=rbind(skew,skew2)
      kurt=rbind(kurt,kurt2)

      m <- mean(x3)
      n <- length(x3)
      s <- sd(x3)
      skew3 <- sum((x3-m)^3/s^3)/n
      kurt3 <- sum((x3-m)^4/s^4)/n - 3
      skew=rbind(skew,skew3)
      kurt=rbind(kurt,kurt3)


      skew<-round(skew,3)
      names(skew)[1]<-c("skew")
      skew<-data.frame(skew)

      kurt<-round(kurt,3)
      names(kurt)[1]<-c("kurt")
      kurt<-data.frame(kurt)

      #do test
      if (length(x)<=5000){
        #Shapiro-Wilk
        p_value1=c()


        result_shapiro<-shapiro.test(x1)
        p_value1<-rbind(p_value1,result_shapiro$p.value)



        result_shapiro<-shapiro.test(x2)
        p_value1<-rbind(p_value1,result_shapiro$p.value)



        result_shapiro<-shapiro.test(x3)
        p_value1<-rbind(p_value1,result_shapiro$p.value)



        p_value1<-round(p_value1,5)
        names(p_value1)[1]<-c("p_value1")
        p_value1<-data.frame(p_value1)



        # Kolmogorov-Smirnov

        p_value2=c()

        result_ks<-suppressWarnings(ks.test(x1,"pnorm",mean(x1),sqrt(var(x1))))
        p_value2<-rbind(p_value2,result_ks$p.value)

        result_ks<-suppressWarnings(ks.test(x2,"pnorm",mean(x2),sqrt(var(x2))))
        p_value2<-rbind(p_value2,result_ks$p.value)

        result_ks<-suppressWarnings(ks.test(x3,"pnorm",mean(x3),sqrt(var(x3))))
        p_value2<-rbind(p_value2,result_ks$p.value)


        p_value2<-round(p_value2,5)
        names(p_value2)[1]<-c("p_value2")
        p_value2<-data.frame(p_value2)

        results_norm<-cbind(transformation,formula,kurt,skew,p_value1,p_value2)

        cat("      Normal transformation and test")
        cat("\n")
        cat("\n")
        rownames(results_norm)=NULL
        print(results_norm)
        cat("\n")
        cat("sample size is",length(x),"\n")
        cat("\n")
        cat(" p_value1:test by Shapiro-Wilk(sample size:3~5000)","\n","p_value2:test by Kolmogorov-Smirnov(sample size:>5000)","\n")


      }else{

        # Kolmogorov-Smirnov
        p_value2=c()

        result_ks<-suppressWarnings(ks.test(x1,"pnorm",mean(x1),sqrt(var(x1))))
        p_value2<-rbind(p_value2,result_ks$p.value)

        result_ks<-suppressWarnings(ks.test(x2,"pnorm",mean(x2),sqrt(var(x2))))
        p_value2<-rbind(p_value2,result_ks$p.value)

        result_ks<-suppressWarnings(ks.test(x3,"pnorm",mean(x3),sqrt(var(x3))))
        p_value2<-rbind(p_value2,result_ks$p.value)


        p_value2<-round(p_value2,5)
        names(p_value2)[1]<-c("p_value2")
        p_value2<-data.frame(p_value2)

        results_norm<-cbind(transformation,formula,kurt,skew,p_value2)

        cat("      Normal transformation and test")
        cat("\n")
        rownames(results_norm)=NULL
        print(results_norm)
        cat("\n")
        cat("sample size is",length(x),"\n")
        cat("","\n","p_value2:test by Kolmogorov-Smirnov(sample size:>5000)","\n")

      }



      ####picture
      #x1
      dev.new()
      par(mfrow=c(2,2),oma = c(2, 0, 3, 0))
      #p1
      hist(x,  freq=FALSE, main = "Histogram of x", xlab = "x")
      lines(density(x1),col="blue")

      #p2
      x_sequence=1:length(x)
      plot(x_sequence,x, main = "Singal x scatter plot", xlab = "sequence of x", ylab = "x")

      #p3
      qqnorm(x,main = "qqplot of x")
      qqline(x)

      #p4
      boxplot(x,main="boxplot of x")

      mtext("Statistical Plots of NTT:x", font = 2, outer=TRUE)


      #x2
      dev.new()

      par(mfrow=c(2,2),oma = c(2, 0, 3, 0))
      ##p1
      hist(x2,  freq=FALSE, main = "Histogram of x^2", xlab = "x^2")
      lines(density(x2),col="blue")

      #p2
      x2_sequence=1:length(x2)
      plot(x2_sequence,x2, main = "Singal x^2 scatter plot", xlab = "sequence of x^2", ylab = "x^2")

      #p3
      qqnorm(x2,main = "qqplot of x^2")
      qqline(x2)

      #p4
      boxplot(x2,main="boxplot of x^2")

      mtext("Statistical Plots of NTT:x^2", font = 2, outer=TRUE)

      #x3
      dev.new()

      par(mfrow=c(2,2),oma = c(2, 0, 3, 0))
      ##p1
      hist(x3,  freq=FALSE, main = "Histogram of x^3", xlab = "x^3")
      lines(density(x3),col="blue")

      #p2
      x3_sequence=1:length(x3)
      plot(x3_sequence,x3, main = "Singal x^3 scatter plot", xlab = "sequence of x^3", ylab = "x^3")

      #p3
      qqnorm(x3,main = "qqplot of x^3")
      qqline(x3)

      #p4
      boxplot(x3,main="boxplot of x^3")

      mtext("Statistical Plots of NTT:x^3", font = 2, outer=TRUE)

    }
    ##min(x)<0 & max(x)>0 & 0 %in% x


    ##min(x)<0 & max(x)>0 & 0 %in% x cut x4 x8 x9
    if (min(x)<0 & max(x)>0 & !is.element(0,x)){
      x1<-x
      x2<-x^2
      x3<-x^3
      x5<-1/x
      x6<-1/(x^2)
      x7<-1/(x^3)

      transformation<-c("itself","square","cubic","inverse","1/square","1/cubic")
      formula<-c("x","x^2","x^3","1/x","1/(x^2)","1/(x^3)")

      skew=c()
      kurt=c()
      m <- mean(x1)
      n <- length(x1)
      s <- sd(x1)
      skew1 <- sum((x1-m)^3/s^3)/n
      kurt1 <- sum((x1-m)^4/s^4)/n - 3
      skew=rbind(skew,skew1)
      kurt=rbind(kurt,kurt1)

      m <- mean(x2)
      n <- length(x2)
      s <- sd(x2)
      skew2 <- sum((x2-m)^3/s^3)/n
      kurt2 <- sum((x2-m)^4/s^4)/n - 3
      skew=rbind(skew,skew2)
      kurt=rbind(kurt,kurt2)

      m <- mean(x3)
      n <- length(x3)
      s <- sd(x3)
      skew3 <- sum((x3-m)^3/s^3)/n
      kurt3 <- sum((x3-m)^4/s^4)/n - 3
      skew=rbind(skew,skew3)
      kurt=rbind(kurt,kurt3)

      m <- mean(x5)
      n <- length(x5)
      s <- sd(x5)
      skew5 <- sum((x5-m)^3/s^3)/n
      kurt5 <- sum((x5-m)^4/s^4)/n - 3
      skew=rbind(skew,skew5)
      kurt=rbind(kurt,kurt5)

      m <- mean(x6)
      n <- length(x6)
      s <- sd(x6)
      skew6 <- sum((x6-m)^3/s^3)/n
      kurt6 <- sum((x6-m)^4/s^4)/n - 3
      skew=rbind(skew,skew6)
      kurt=rbind(kurt,kurt6)

      m <- mean(x7)
      n <- length(x7)
      s <- sd(x7)
      skew7 <- sum((x7-m)^3/s^3)/n
      kurt7 <- sum((x7-m)^4/s^4)/n - 3
      skew=rbind(skew,skew7)
      kurt=rbind(kurt,kurt7)


      skew<-round(skew,3)
      names(skew)[1]<-c("skew")
      skew<-data.frame(skew)

      kurt<-round(kurt,3)
      names(kurt)[1]<-c("kurt")
      kurt<-data.frame(kurt)

      if (length(x)<=5000){
        #Shapiro-Wilk
        p_value1=c()


        result_shapiro<-shapiro.test(x1)
        p_value1<-rbind(p_value1,result_shapiro$p.value)



        result_shapiro<-shapiro.test(x2)
        p_value1<-rbind(p_value1,result_shapiro$p.value)



        result_shapiro<-shapiro.test(x3)
        p_value1<-rbind(p_value1,result_shapiro$p.value)



        result_shapiro<-shapiro.test(x5)
        p_value1<-rbind(p_value1,result_shapiro$p.value)


        result_shapiro<-shapiro.test(x6)
        p_value1<-rbind(p_value1,result_shapiro$p.value)


        result_shapiro<-shapiro.test(x7)
        p_value1<-rbind(p_value1,result_shapiro$p.value)




        p_value1<-round(p_value1,5)
        names(p_value1)[1]<-c("p_value1")
        p_value1<-data.frame(p_value1)



        # Kolmogorov-Smirnov

        p_value2=c()

        result_ks<-suppressWarnings(ks.test(x1,"pnorm",mean(x1),sqrt(var(x1))))
        p_value2<-rbind(p_value2,result_ks$p.value)

        result_ks<-suppressWarnings(ks.test(x2,"pnorm",mean(x2),sqrt(var(x2))))
        p_value2<-rbind(p_value2,result_ks$p.value)

        result_ks<-suppressWarnings(ks.test(x3,"pnorm",mean(x3),sqrt(var(x3))))
        p_value2<-rbind(p_value2,result_ks$p.value)


        result_ks<-suppressWarnings(ks.test(x5,"pnorm",mean(x5),sqrt(var(x5))))
        p_value2<-rbind(p_value2,result_ks$p.value)

        result_ks<-suppressWarnings(ks.test(x6,"pnorm",mean(x6),sqrt(var(x6))))
        p_value2<-rbind(p_value2,result_ks$p.value)

        result_ks<-suppressWarnings(ks.test(x7,"pnorm",mean(x7),sqrt(var(x7))))
        p_value2<-rbind(p_value2,result_ks$p.value)


        p_value2<-round(p_value2,5)
        names(p_value2)[1]<-c("p_value2")
        p_value2<-data.frame(p_value2)

        results_norm<-cbind(transformation,formula,kurt,skew,p_value1,p_value2)

        cat("      Normal transformation and test")
        cat("\n")
        cat("\n")
        rownames(results_norm)=NULL
        print(results_norm)
        cat("\n")
        cat("sample size is",length(x),"\n")
        cat("\n")
        cat(" p_value1:test by Shapiro-Wilk(sample size:3~5000)","\n","p_value2:test by Kolmogorov-Smirnov(sample size:>5000)","\n")


      }else{

        # Kolmogorov-Smirnov
        p_value2=c()

        result_ks<-suppressWarnings(ks.test(x1,"pnorm",mean(x1),sqrt(var(x1))))
        p_value2<-rbind(p_value2,result_ks$p.value)

        result_ks<-suppressWarnings(ks.test(x2,"pnorm",mean(x2),sqrt(var(x2))))
        p_value2<-rbind(p_value2,result_ks$p.value)

        result_ks<-suppressWarnings(ks.test(x3,"pnorm",mean(x3),sqrt(var(x3))))
        p_value2<-rbind(p_value2,result_ks$p.value)


        result_ks<-suppressWarnings(ks.test(x5,"pnorm",mean(x5),sqrt(var(x5))))
        p_value2<-rbind(p_value2,result_ks$p.value)

        result_ks<-suppressWarnings(ks.test(x6,"pnorm",mean(x6),sqrt(var(x6))))
        p_value2<-rbind(p_value2,result_ks$p.value)

        result_ks<-suppressWarnings(ks.test(x7,"pnorm",mean(x7),sqrt(var(x7))))
        p_value2<-rbind(p_value2,result_ks$p.value)


        p_value2<-round(p_value2,5)
        names(p_value2)[1]<-c("p_value2")
        p_value2<-data.frame(p_value2)

        results_norm<-cbind(transformation,formula,kurt,skew,p_value2)

        cat("      Normal transformation and test")
        cat("\n")
        rownames(results_norm)=NULL
        print(results_norm)
        cat("\n")
        cat("sample size is",length(x),"\n")
        cat("","\n","p_value2:test by Kolmogorov-Smirnov(sample size:>5000)","\n")

      }



      ####picture
      #x1
      dev.new()
      par(mfrow=c(2,2),oma = c(2, 0, 3, 0))
      #p1
      hist(x,  freq=FALSE, main = "Histogram of x", xlab = "x")
      lines(density(x1),col="blue")

      #p2
      x_sequence=1:length(x)
      plot(x_sequence,x, main = "Singal x scatter plot", xlab = "sequence of x", ylab = "x")

      #p3
      qqnorm(x,main = "qqplot of x")
      qqline(x)

      #p4
      boxplot(x,main="boxplot of x")

      mtext("Statistical Plots of NTT:x", font = 2, outer=TRUE)


      #x2
      dev.new()

      par(mfrow=c(2,2),oma = c(2, 0, 3, 0))
      ##p1
      hist(x2,  freq=FALSE, main = "Histogram of x^2", xlab = "x^2")
      lines(density(x2),col="blue")

      #p2
      x2_sequence=1:length(x2)
      plot(x2_sequence,x2, main = "Singal x^2 scatter plot", xlab = "sequence of x^2", ylab = "x^2")

      #p3
      qqnorm(x2,main = "qqplot of x^2")
      qqline(x2)

      #p4
      boxplot(x2,main="boxplot of x^2")

      mtext("Statistical Plots of NTT:x^2", font = 2, outer=TRUE)

      #x3
      dev.new()

      par(mfrow=c(2,2),oma = c(2, 0, 3, 0))
      ##p1
      hist(x3,  freq=FALSE, main = "Histogram of x^3", xlab = "x^3")
      lines(density(x3),col="blue")

      #p2
      x3_sequence=1:length(x3)
      plot(x3_sequence,x3, main = "Singal x^3 scatter plot", xlab = "sequence of x^3", ylab = "x^3")

      #p3
      qqnorm(x3,main = "qqplot of x^3")
      qqline(x3)

      #p4
      boxplot(x3,main="boxplot of x^3")

      mtext("Statistical Plots of NTT:x^3", font = 2, outer=TRUE)

      #x5
      dev.new()

      par(mfrow=c(2,2),oma = c(2, 0, 3, 0))
      ##p1
      hist(x5,  freq=FALSE, main = "Histogram of 1/x", xlab = "1/x")
      lines(density(x5),col="blue")


      #p2
      x5_sequence=1:length(x5)
      plot(x5_sequence,x5, main = "Singal 1/x scatter plot", xlab = "sequence of 1/x", ylab = "1/x")

      #p3
      qqnorm(x5,main = "qqplot of 1/x")
      qqline(x5)

      #p4
      boxplot(x5,main="boxplot of 1/x")

      mtext("Statistical Plots of NTT:1/x", font = 2, outer=TRUE)

      #x6
      dev.new()

      par(mfrow=c(2,2),oma = c(2, 0, 3, 0))
      ##p1
      hist(x6,freq=FALSE, main = "Histogram of 1/(x^2)", xlab = "1/(x^2)")
      lines(density(x6),col="blue")


      #p2
      x6_sequence=1:length(x6)
      plot(x6_sequence,x6, main = "Singal 1/(x^2) scatter plot", xlab = "sequence of 1/(x^2)", ylab = "1/(x^2)")

      #p3
      qqnorm(x6,main = "qqplot of 1/(x^2)")
      qqline(x6)

      #p4
      boxplot(x6,main="boxplot of 1/(x^2)")

      mtext("Statistical Plots of NTT:1/(x^2)", font = 2, outer=TRUE)

      #x7
      dev.new()

      par(mfrow=c(2,2),oma = c(2, 0, 3, 0))
      ##p1
      hist(x7,  freq=FALSE, main = "Histogram of 1/(x^3)", xlab = "1/(x^3)")
      lines(density(x7),col="blue")


      #p2
      x7_sequence=1:length(x7)
      plot(x7_sequence,x7, main = "Singal 1/(x^3) scatter plot", xlab = "sequence of 1/(x^3)", ylab = "1/(x^3)")

      #p3
      qqnorm(x7,main = "qqplot of 1/(x^3)")
      qqline(x7)

      #p4
      boxplot(x7,main="boxplot of 1/(x^3)")

      mtext("Statistical Plots of NTT:1/(x^3)", font = 2, outer=TRUE)

    }
    ##min(x)<0 & max(x)>0 & 0 %in%  cut x4 x8 x9
    ####max===0
    if (max(x)==0){
      x1<-x
      x2<-x^2
      x3<-x^3
      transformation<-c("itself","square","cubic")
      formula<-c("x","x^2","x^3")

      #acquir skew and kurt
      skew=c()
      kurt=c()

      m <- mean(x1)
      n <- length(x1)
      s <- sd(x1)
      skew1 <- sum((x1-m)^3/s^3)/n
      kurt1 <- sum((x1-m)^4/s^4)/n - 3
      skew=rbind(skew,skew1)
      kurt=rbind(kurt,kurt1)

      m <- mean(x2)
      n <- length(x2)
      s <- sd(x2)
      skew2 <- sum((x2-m)^3/s^3)/n
      kurt2 <- sum((x2-m)^4/s^4)/n - 3
      skew=rbind(skew,skew2)
      kurt=rbind(kurt,kurt2)

      m <- mean(x3)
      n <- length(x3)
      s <- sd(x3)
      skew3 <- sum((x3-m)^3/s^3)/n
      kurt3 <- sum((x3-m)^4/s^4)/n - 3
      skew=rbind(skew,skew3)
      kurt=rbind(kurt,kurt3)


      skew<-round(skew,3)
      names(skew)[1]<-c("skew")
      skew<-data.frame(skew)

      kurt<-round(kurt,3)
      names(kurt)[1]<-c("kurt")
      kurt<-data.frame(kurt)

      #do test
      if (length(x)<=5000){
        #Shapiro-Wilk
        p_value1=c()


        result_shapiro<-shapiro.test(x1)
        p_value1<-rbind(p_value1,result_shapiro$p.value)



        result_shapiro<-shapiro.test(x2)
        p_value1<-rbind(p_value1,result_shapiro$p.value)



        result_shapiro<-shapiro.test(x3)
        p_value1<-rbind(p_value1,result_shapiro$p.value)



        p_value1<-round(p_value1,5)
        names(p_value1)[1]<-c("p_value1")
        p_value1<-data.frame(p_value1)



        # Kolmogorov-Smirnov

        p_value2=c()

        result_ks<-suppressWarnings(ks.test(x1,"pnorm",mean(x1),sqrt(var(x1))))
        p_value2<-rbind(p_value2,result_ks$p.value)

        result_ks<-suppressWarnings(ks.test(x2,"pnorm",mean(x2),sqrt(var(x2))))
        p_value2<-rbind(p_value2,result_ks$p.value)

        result_ks<-suppressWarnings(ks.test(x3,"pnorm",mean(x3),sqrt(var(x3))))
        p_value2<-rbind(p_value2,result_ks$p.value)


        p_value2<-round(p_value2,5)
        names(p_value2)[1]<-c("p_value2")
        p_value2<-data.frame(p_value2)

        results_norm<-cbind(transformation,formula,kurt,skew,p_value1,p_value2)

        cat("      Normal transformation and test")
        cat("\n")
        cat("\n")
        rownames(results_norm)=NULL
        print(results_norm)
        cat("\n")
        cat("sample size is",length(x),"\n")
        cat("\n")
        cat(" p_value1:test by Shapiro-Wilk(sample size:3~5000)","\n","p_value2:test by Kolmogorov-Smirnov(sample size:>5000)","\n")


      }else{

        # Kolmogorov-Smirnov
        p_value2=c()

        result_ks<-suppressWarnings(ks.test(x1,"pnorm",mean(x1),sqrt(var(x1))))
        p_value2<-rbind(p_value2,result_ks$p.value)

        result_ks<-suppressWarnings(ks.test(x2,"pnorm",mean(x2),sqrt(var(x2))))
        p_value2<-rbind(p_value2,result_ks$p.value)

        result_ks<-suppressWarnings(ks.test(x3,"pnorm",mean(x3),sqrt(var(x3))))
        p_value2<-rbind(p_value2,result_ks$p.value)


        p_value2<-round(p_value2,5)
        names(p_value2)[1]<-c("p_value2")
        p_value2<-data.frame(p_value2)

        results_norm<-cbind(transformation,formula,kurt,skew,p_value2)

        cat("      Normal transformation and test")
        cat("\n")
        rownames(results_norm)=NULL
        print(results_norm)
        cat("\n")
        cat("sample size is",length(x),"\n")
        cat("","\n","p_value2:test by Kolmogorov-Smirnov(sample size:>5000)","\n")

      }



      ####picture
      #x1
      dev.new()
      par(mfrow=c(2,2),oma = c(2, 0, 3, 0))
      #p1
      hist(x,  freq=FALSE, main = "Histogram of x", xlab = "x")
      lines(density(x1),col="blue")

      #p2
      x_sequence=1:length(x)
      plot(x_sequence,x, main = "Singal x scatter plot", xlab = "sequence of x", ylab = "x")

      #p3
      qqnorm(x,main = "qqplot of x")
      qqline(x)

      #p4
      boxplot(x,main="boxplot of x")

      mtext("Statistical Plots of NTT:x", font = 2, outer=TRUE)


      #x2
      dev.new()

      par(mfrow=c(2,2),oma = c(2, 0, 3, 0))
      ##p1
      hist(x2,  freq=FALSE, main = "Histogram of x^2", xlab = "x^2")
      lines(density(x2),col="blue")

      #p2
      x2_sequence=1:length(x2)
      plot(x2_sequence,x2, main = "Singal x^2 scatter plot", xlab = "sequence of x^2", ylab = "x^2")

      #p3
      qqnorm(x2,main = "qqplot of x^2")
      qqline(x2)

      #p4
      boxplot(x2,main="boxplot of x^2")

      mtext("Statistical Plots of NTT:x^2", font = 2, outer=TRUE)

      #x3
      dev.new()

      par(mfrow=c(2,2),oma = c(2, 0, 3, 0))
      ##p1
      hist(x3,  freq=FALSE, main = "Histogram of x^3", xlab = "x^3")
      lines(density(x3),col="blue")

      #p2
      x3_sequence=1:length(x3)
      plot(x3_sequence,x3, main = "Singal x^3 scatter plot", xlab = "sequence of x^3", ylab = "x^3")

      #p3
      qqnorm(x3,main = "qqplot of x^3")
      qqline(x3)

      #p4
      boxplot(x3,main="boxplot of x^3")

      mtext("Statistical Plots of NTT:x^3", font = 2, outer=TRUE)
    }
    ####max===0
    ####max(x)<0,cut x4 x8  x9
    if (max(x)<0){
      x1<-x
      x2<-x^2
      x3<-x^3
      x5<-1/x
      x6<-1/(x^2)
      x7<-1/(x^3)

      transformation<-c("itself","square","cubic","inverse","1/square","1/cubic")
      formula<-c("x","x^2","x^3","1/x","1/(x^2)","1/(x^3)")

      skew=c()
      kurt=c()
      m <- mean(x1)
      n <- length(x1)
      s <- sd(x1)
      skew1 <- sum((x1-m)^3/s^3)/n
      kurt1 <- sum((x1-m)^4/s^4)/n - 3
      skew=rbind(skew,skew1)
      kurt=rbind(kurt,kurt1)

      m <- mean(x2)
      n <- length(x2)
      s <- sd(x2)
      skew2 <- sum((x2-m)^3/s^3)/n
      kurt2 <- sum((x2-m)^4/s^4)/n - 3
      skew=rbind(skew,skew2)
      kurt=rbind(kurt,kurt2)

      m <- mean(x3)
      n <- length(x3)
      s <- sd(x3)
      skew3 <- sum((x3-m)^3/s^3)/n
      kurt3 <- sum((x3-m)^4/s^4)/n - 3
      skew=rbind(skew,skew3)
      kurt=rbind(kurt,kurt3)

      m <- mean(x5)
      n <- length(x5)
      s <- sd(x5)
      skew5 <- sum((x5-m)^3/s^3)/n
      kurt5 <- sum((x5-m)^4/s^4)/n - 3
      skew=rbind(skew,skew5)
      kurt=rbind(kurt,kurt5)

      m <- mean(x6)
      n <- length(x6)
      s <- sd(x6)
      skew6 <- sum((x6-m)^3/s^3)/n
      kurt6 <- sum((x6-m)^4/s^4)/n - 3
      skew=rbind(skew,skew6)
      kurt=rbind(kurt,kurt6)

      m <- mean(x7)
      n <- length(x7)
      s <- sd(x7)
      skew7 <- sum((x7-m)^3/s^3)/n
      kurt7 <- sum((x7-m)^4/s^4)/n - 3
      skew=rbind(skew,skew7)
      kurt=rbind(kurt,kurt7)


      skew<-round(skew,3)
      names(skew)[1]<-c("skew")
      skew<-data.frame(skew)

      kurt<-round(kurt,3)
      names(kurt)[1]<-c("kurt")
      kurt<-data.frame(kurt)

      if (length(x)<=5000){
        #Shapiro-Wilk
        p_value1=c()


        result_shapiro<-shapiro.test(x1)
        p_value1<-rbind(p_value1,result_shapiro$p.value)



        result_shapiro<-shapiro.test(x2)
        p_value1<-rbind(p_value1,result_shapiro$p.value)



        result_shapiro<-shapiro.test(x3)
        p_value1<-rbind(p_value1,result_shapiro$p.value)



        result_shapiro<-shapiro.test(x5)
        p_value1<-rbind(p_value1,result_shapiro$p.value)


        result_shapiro<-shapiro.test(x6)
        p_value1<-rbind(p_value1,result_shapiro$p.value)


        result_shapiro<-shapiro.test(x7)
        p_value1<-rbind(p_value1,result_shapiro$p.value)




        p_value1<-round(p_value1,5)
        names(p_value1)[1]<-c("p_value1")
        p_value1<-data.frame(p_value1)



        # Kolmogorov-Smirnov

        p_value2=c()

        result_ks<-suppressWarnings(ks.test(x1,"pnorm",mean(x1),sqrt(var(x1))))
        p_value2<-rbind(p_value2,result_ks$p.value)

        result_ks<-suppressWarnings(ks.test(x2,"pnorm",mean(x2),sqrt(var(x2))))
        p_value2<-rbind(p_value2,result_ks$p.value)

        result_ks<-suppressWarnings(ks.test(x3,"pnorm",mean(x3),sqrt(var(x3))))
        p_value2<-rbind(p_value2,result_ks$p.value)


        result_ks<-suppressWarnings(ks.test(x5,"pnorm",mean(x5),sqrt(var(x5))))
        p_value2<-rbind(p_value2,result_ks$p.value)

        result_ks<-suppressWarnings(ks.test(x6,"pnorm",mean(x6),sqrt(var(x6))))
        p_value2<-rbind(p_value2,result_ks$p.value)

        result_ks<-suppressWarnings(ks.test(x7,"pnorm",mean(x7),sqrt(var(x7))))
        p_value2<-rbind(p_value2,result_ks$p.value)


        p_value2<-round(p_value2,5)
        names(p_value2)[1]<-c("p_value2")
        p_value2<-data.frame(p_value2)

        results_norm<-cbind(transformation,formula,kurt,skew,p_value1,p_value2)

        cat("      Normal transformation and test")
        cat("\n")
        cat("\n")
        rownames(results_norm)=NULL
        print(results_norm)
        cat("\n")
        cat("sample size is",length(x),"\n")
        cat("\n")
        cat(" p_value1:test by Shapiro-Wilk(sample size:3~5000)","\n","p_value2:test by Kolmogorov-Smirnov(sample size:>5000)","\n")


      }else{

        # Kolmogorov-Smirnov
        p_value2=c()

        result_ks<-suppressWarnings(ks.test(x1,"pnorm",mean(x1),sqrt(var(x1))))
        p_value2<-rbind(p_value2,result_ks$p.value)

        result_ks<-suppressWarnings(ks.test(x2,"pnorm",mean(x2),sqrt(var(x2))))
        p_value2<-rbind(p_value2,result_ks$p.value)

        result_ks<-suppressWarnings(ks.test(x3,"pnorm",mean(x3),sqrt(var(x3))))
        p_value2<-rbind(p_value2,result_ks$p.value)


        result_ks<-suppressWarnings(ks.test(x5,"pnorm",mean(x5),sqrt(var(x5))))
        p_value2<-rbind(p_value2,result_ks$p.value)

        result_ks<-suppressWarnings(ks.test(x6,"pnorm",mean(x6),sqrt(var(x6))))
        p_value2<-rbind(p_value2,result_ks$p.value)

        result_ks<-suppressWarnings(ks.test(x7,"pnorm",mean(x7),sqrt(var(x7))))
        p_value2<-rbind(p_value2,result_ks$p.value)


        p_value2<-round(p_value2,5)
        names(p_value2)[1]<-c("p_value2")
        p_value2<-data.frame(p_value2)

        results_norm<-cbind(transformation,formula,kurt,skew,p_value2)

        cat("      Normal transformation and test")
        cat("\n")
        rownames(results_norm)=NULL
        print(results_norm)
        cat("\n")
        cat("sample size is",length(x),"\n")
        cat("","\n","p_value2:test by Kolmogorov-Smirnov(sample size:>5000)","\n")

      }



      ####picture
      #x1
      dev.new()
      par(mfrow=c(2,2),oma = c(2, 0, 3, 0))
      #p1
      hist(x,  freq=FALSE, main = "Histogram of x", xlab = "x")
      lines(density(x1),col="blue")

      #p2
      x_sequence=1:length(x)
      plot(x_sequence,x, main = "Singal x scatter plot", xlab = "sequence of x", ylab = "x")

      #p3
      qqnorm(x,main = "qqplot of x")
      qqline(x)

      #p4
      boxplot(x,main="boxplot of x")

      mtext("Statistical Plots of NTT:x", font = 2, outer=TRUE)


      #x2
      dev.new()

      par(mfrow=c(2,2),oma = c(2, 0, 3, 0))
      ##p1
      hist(x2,  freq=FALSE, main = "Histogram of x^2", xlab = "x^2")
      lines(density(x2),col="blue")

      #p2
      x2_sequence=1:length(x2)
      plot(x2_sequence,x2, main = "Singal x^2 scatter plot", xlab = "sequence of x^2", ylab = "x^2")

      #p3
      qqnorm(x2,main = "qqplot of x^2")
      qqline(x2)

      #p4
      boxplot(x2,main="boxplot of x^2")

      mtext("Statistical Plots of NTT:x^2", font = 2, outer=TRUE)

      #x3
      dev.new()

      par(mfrow=c(2,2),oma = c(2, 0, 3, 0))
      ##p1
      hist(x3,  freq=FALSE, main = "Histogram of x^3", xlab = "x^3")
      lines(density(x3),col="blue")

      #p2
      x3_sequence=1:length(x3)
      plot(x3_sequence,x3, main = "Singal x^3 scatter plot", xlab = "sequence of x^3", ylab = "x^3")

      #p3
      qqnorm(x3,main = "qqplot of x^3")
      qqline(x3)

      #p4
      boxplot(x3,main="boxplot of x^3")

      mtext("Statistical Plots of NTT:x^3", font = 2, outer=TRUE)

      #x5
      dev.new()

      par(mfrow=c(2,2),oma = c(2, 0, 3, 0))
      ##p1
      hist(x5,  freq=FALSE, main = "Histogram of 1/x", xlab = "1/x")
      lines(density(x5),col="blue")


      #p2
      x5_sequence=1:length(x5)
      plot(x5_sequence,x5, main = "Singal 1/x scatter plot", xlab = "sequence of 1/x", ylab = "1/x")

      #p3
      qqnorm(x5,main = "qqplot of 1/x")
      qqline(x5)

      #p4
      boxplot(x5,main="boxplot of 1/x")

      mtext("Statistical Plots of NTT:1/x", font = 2, outer=TRUE)

      #x6
      dev.new()

      par(mfrow=c(2,2),oma = c(2, 0, 3, 0))
      ##p1
      hist(x6,freq=FALSE, main = "Histogram of 1/(x^2)", xlab = "1/(x^2)")
      lines(density(x6),col="blue")


      #p2
      x6_sequence=1:length(x6)
      plot(x6_sequence,x6, main = "Singal 1/(x^2) scatter plot", xlab = "sequence of 1/(x^2)", ylab = "1/(x^2)")

      #p3
      qqnorm(x6,main = "qqplot of 1/(x^2)")
      qqline(x6)

      #p4
      boxplot(x6,main="boxplot of 1/(x^2)")

      mtext("Statistical Plots of NTT:1/(x^2)", font = 2, outer=TRUE)

      #x7
      dev.new()

      par(mfrow=c(2,2),oma = c(2, 0, 3, 0))
      ##p1
      hist(x7,  freq=FALSE, main = "Histogram of 1/(x^3)", xlab = "1/(x^3)")
      lines(density(x7),col="blue")


      #p2
      x7_sequence=1:length(x7)
      plot(x7_sequence,x7, main = "Singal 1/(x^3) scatter plot", xlab = "sequence of 1/(x^3)", ylab = "1/(x^3)")

      #p3
      qqnorm(x7,main = "qqplot of 1/(x^3)")
      qqline(x7)

      #p4
      boxplot(x7,main="boxplot of 1/(x^3)")

      mtext("Statistical Plots of NTT:1/(x^3)", font = 2, outer=TRUE)

    }
    ##max(x)<0 , cut x4 x8 x9
    ####between(-1,+),Arcsin(X)
    if (max(x)<=1 & min(x)>= -1){
      x10<-acos(x)

      transformation<-c("Arcsin(X)")
      formula<-c("acos(x)")

      skew=c()
      kurt=c()
      m <- mean(x10)
      n <- length(x10)
      s <- sd(x10)
      skew10 <- sum((x10-m)^3/s^3)/n
      kurt10 <- sum((x10-m)^4/s^4)/n - 3
      skew=rbind(skew,skew10)
      kurt=rbind(kurt,kurt10)

      skew<-round(skew,3)
      names(skew)[1]<-c("skew")
      skew<-data.frame(skew)

      kurt<-round(kurt,3)
      names(kurt)[1]<-c("kurt")
      kurt<-data.frame(kurt)
      # do test
      if (length(x10)<=5000){
        #Shapiro-Wilk
        p_value1=c()

        result_shapiro<-shapiro.test(x10)
        p_value1<-rbind(p_value1,result_shapiro$p.value)

        p_value1<-round(p_value1,5)
        names(p_value1)[1]<-c("p_value1")
        p_value1<-data.frame(p_value1)


        # Kolmogorov-Smirnov

        p_value2=c()

        result_ks<-suppressWarnings(ks.test(x10,"pnorm",mean(x10),sqrt(var(x10))))
        p_value2<-rbind(p_value2,result_ks$p.value)

        p_value2<-round(p_value2,5)
        names(p_value2)[1]<-c("p_value2")
        p_value2<-data.frame(p_value2)

        results_norm<-cbind(transformation,formula,kurt,skew,p_value1,p_value2)

        cat("\n", "      Arcsin(X) transformation and test")
        cat("\n")
        cat("\n")
        rownames(results_norm)=NULL
        print(results_norm)
        cat("\n")
        cat("sample size is",length(x),"\n")
        cat("\n")
        cat(" p_value1:test by Shapiro-Wilk(sample size:3~5000)","\n","p_value2:test by Kolmogorov-Smirnov(sample size:>5000)","\n")


      }else{

        # Kolmogorov-Smirnov
        p_value2=c()

        result_ks<-suppressWarnings(ks.test(x10,"pnorm",mean(x10),sqrt(var(x10))))
        p_value2<-rbind(p_value2,result_ks$p.value)


        p_value2<-round(p_value2,5)
        names(p_value2)[1]<-c("p_value2")
        p_value2<-data.frame(p_value2)

        results_norm<-cbind(transformation,formula,kurt,skew,p_value2)

        cat("\n", "      Arcsin(X) transformation and test")
        cat("\n")
        rownames(results_norm)=NULL
        print(results_norm)
        cat("\n")
        cat("sample size is",length(x),"\n")
        cat("","\n","p_value2:test by Kolmogorov-Smirnov(sample size:>5000)","\n")

      }


      ####picture
      #x10
      dev.new()

      par(mfrow=c(2,2),oma = c(2, 0, 3, 0))
      ##p1
      hist(x10,  freq=FALSE, main = "Histogram of Arcsin(X)", xlab = "Arcsin(X)")
      lines(density(x10),col="blue")

      #p2
      x10_sequence=1:length(x10)
      plot(x10_sequence,x10, main = "Singal xArcsin(X) scatter plot", xlab = "sequence of Arcsin(X)", ylab = "Arcsin(X)")

      #p3
      qqnorm(x10,main = "qqplot of Arcsin(X)")
      qqline(x10)

      #p4
      boxplot(x10,main="boxplot of Arcsin(X)")

      mtext("Statistical Plots of NTT:Arcsin(X)", font = 2, outer=TRUE)

    }
    ####between(-1,+),Arcsin(X)

    m <- mean(x)
    n <- length(x)
    s <- sd(x)
    skewtest <- sum((x-m)^3/s^3)/n
    if (skewtest<0){cat("\n","skew(x)<0, you can use fumula","x<-'max(x)-x'","or","x<-'max(x)-x+1'","to get a new x,which skew is over 0, and do ntt again")}
  }else{
    stop("x must be numeric or integer")
  }
}
