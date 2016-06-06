#-----------------------------------------------------------------------------------------------------
#
#  Function to create a map of the world that shows countries by categories
#
#-----------------------------------------------------------------------------------------------------



# Country mapping functions
ctry_map <- function(
   fname       # Name of output file (without .png extension)
  ,countries   # Vector of country names 
  ,c.colors    # Vector of country colors (same length as countries)
  ,nv.color='gray90'  # Color of countries not included in the countries vector
)  {


  # Save list of regions from the world database, and flag Antarctica and bodies of water
  world.db <- map('world' ,plot=F)
  regs <- world.db$names
  Nreg <- length(regs)
  Antarctica <- rep(F ,Nreg)
  Antarctica[grep('Antarctica',regs)] <- T
  water <- rep(F ,Nreg)
  water[grep('Sea',regs)] <- T
  water[grep('Lake',regs)] <- T
  regs[Antarctica]
  regs[water]


  # Strip off the text before the colon in the regs column
  lenr <- length(regs)
  rmaps <- rep(NA,lenr)
  for (i in c(1:lenr)) {
    rmaps[i] <- strsplit(regs[i],':')[[1]][1]
  }
  ctrydb <- data.frame(key=c(1:lenr) ,regs=regs ,rmaps=rmaps ,Antarctica=Antarctica ,water=water 
   ,stringsAsFactors=F
  )


  # Subset on countries specified by the user and 
  # assign the colors specified by the user
  user.ctry <- subset(ctrydb ,rmaps %in% countries ,c(key,rmaps))
  user.ctry$COL <- c.colors[match(user.ctry$rmaps,countries)]

  # Merge the user-specified countries back with the whole database
  # and assign all of the other countries with the background country color
  ctrydat <- merge(ctrydb,user.ctry,all.x=T)
  
  # Re-order to be in the same order as the map database
  ctrydat <- ctrydat[order(ctrydat$key),]
  ctrydat$COL <- with(ctrydat ,ifelse(is.na(COL) ,nv.color ,COL))


  # Subset on data to be printed
  datplot <- subset(ctrydat ,!Antarctica & !water)


  # Plotting functions
  png(paste(fname,'png',sep='.') ,width=12 ,height=6 ,units="in" ,res=360 ,bg="transparent")
  par(oma=c(0,0,0,0) ,mar=c(0,0,0,0))
  LWD <- 0.5

  map("world" 
    ,boundary=F ,interior=T ,fill=T ,col=datplot$COL ,border='white' ,lwd=LWD
    ,regions=datplot$regs
    ,exact=T
  )

  # Need to bring to front Lesotho
  if ('Lesotho' %in% datplot$regs) {
    map("world" ,add=T
      ,boundary=F ,interior=T ,fill=T ,col=datplot$COL[datplot$rmaps=='Lesotho'] ,border='white' ,lwd=LWD
      ,regions='Lesotho'
      ,exact=T
    )
  }
  
 #mtext(format(Sys.time(), "%d%b%Y") ,side=1 ,line=-2 ,cex=8/12 ,adj=0.9 ,outer=T)

}  # End of ctry_map function

