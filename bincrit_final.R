bincrit <- function(n,p,numtail=2,typetail=c("lower","upper","L","U",""),sig_level,strictly_lower=c(T,F),table_included=c(T,F),curve_included=c(T,F)) {
  if (numtail==1) {
    if (typetail=="L") {
      c1=0
      while (pbinom(c1,n,p)<(sig_level/100)) {
        c1=c1+1
      }
      c1=c1-1
    } else if (typetail=="U") {
      c2=0
      while (pbinom(c2,n,p)<(1-(sig_level/100))) {
        c2=c2+1
      }
    }
  } else if (numtail==2) {
    c1=0
    c2=0
    sig_level=sig_level/2
    while (pbinom(c1,n,p)<(sig_level/100)) {
      c1=c1+1
    }
    if (c1!=0) {
      c1=c1-1
    }
    while (pbinom(c2,n,p)<(1-(sig_level/100))) {
      c2=c2+1
    }
  }
  if (strictly_lower==F) {
    if (!(numtail==1&typetail=="U"))
      if (((sig_level/100) - (pbinom(c1,n,p))) > ((pbinom((c1+1),n,p)) - (sig_level/100))) {
        c1=c1+1
      }
    if (!is.na(c2)) {
      if (((pbinom(c2,n,p)) - (1-(sig_level/100))) > ((1-(sig_level/100)) - (pbinom((c2-1),n,p)))) {
        c2=c2-1
      }
    }
  }

  if (numtail==2) {
    print(paste0("lower crit value: ", c1))
    print(paste0("upper crit value: ", c2))
  } else if (typetail=="L") {
    print(paste0("lower crit value: ",c1))
  } else if (typetail=="U") {
    print(paste0("upper crit value: ",c2))
  }

  x=seq(0,n)
  Density=dbinom(x,n,p)
  gd=data.frame(x,Density)
  # Here a factor, crit_region, is added to a data frame (gd) along with the x values and their density functions, so they can be used to add in colour by bar (critical region in red, the specific critical value in dark red and the acceptance region in dark green) and a label for the critical values
  if (curve_included) {
    if (numtail==1&typetail=="L") {
      crit_region = c(rep(1,(c1)),paste0("critical value - ",c1),rep(0,(n-c1)))
      gd=data.frame(x,Density,crit_region)
      graph <- ggplot(gd, aes(x=x, y=Density, group=crit_region, fill=as.factor(crit_region)),) +
      geom_col(width=0.5,show.legend = F) +
      scale_fill_manual(values=c("olivedrab4","red","darkred")) +
      geom_text(data=subset(gd,crit_region!=0),aes(label=crit_region,color=crit_region),
        angle=90,nudge_y=0.03,size=2,show.legend=F) +
      scale_color_manual(values=c(rgb(0,0,0,0),"black"))
      }

    else if (numtail==1&typetail=="U") {
        crit_region = c(rep(0,(c2)),paste0("critical value - ",c2),rep(1,(n-(c2))))
        gd=data.frame(x,Density,crit_region)
        graph <- ggplot(gd, aes(x=x, y=Density, group=crit_region, fill=as.factor(crit_region))) +
        geom_col(width=0.5,show.legend = F) +
        scale_fill_manual(values=c("olivedrab4","red","darkred")) +
        geom_text(data=subset(gd,crit_region!=0),aes(label=crit_region,
          colour=crit_region),angle=90,nudge_y=0.03,size=2,show.legend=F) +
          scale_color_manual(values=c(rgb(0,0,0,0),"black"))
    }

    else if (numtail==2) {
        crit_region = c(rep(1,c1),paste0("critical value - ",c1),rep(0,(c2-c1-1)),paste0("critical value - ",c2),rep(1,(n-(c2))))
        gd=data.frame(x,Density,crit_region)
        graph <- ggplot(gd, aes(x=x, y=Density, group=crit_region,fill=as.factor(crit_region))) +
        geom_col(width=0.5,show.legend = F) +
        scale_fill_manual(values=c("olivedrab4","red","darkred","darkred")) +
        geom_text(data=subset(gd,crit_region!=0),aes(label=crit_region,colour=crit_region),
          angle=90,nudge_y=0.03,size=2,show.legend=F) +
        scale_color_manual(values=c(rgb(0,0,0,0),"black","black"))
    }
    print(graph)
  }
  if (table_included) {
    table=matrix(data=c("x","P(X<=x)"), ncol=2)
    x=0
    while (x<=n) {
      table=rbind(table,c(x,pbinom(x,n,p)))
      x=x+1
    }
    print(table)
    print(gd)
  }
}

bincrit(n=50,p=0.6,numtail=2,typetail="U",sig_level=0.05,strictly_lower=F,table_included=F,curve_included=T)

