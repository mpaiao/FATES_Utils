#=======
#=======
#     This script loads all other scripts in this path, and also loads all the necessary
# packages.
#---~---
if ("srcdir" %in% ls()){
   srcdir <<- srcdir
}else{
   srcdir <<- getwd()
}#end if
#---~---



#--- Find which major version of R is calling this script.
R.major <<- as.numeric(R.version$major)
#---~---



#--- Make the screen output as wide as the screen permits.
ncstring = as.integer(Sys.getenv("COLUMNS"))
if (! is.na(ncstring)){
   if (ncstring > 80 & ncstring < 500) options(width=ncstring)
}#end if (! is.na(ncstring))
#---~---



#--- Fix the colours according to the current background.
if (! "ibackground" %in% ls()) ibackground = 0
if (ibackground == 0){
  foreground    = "black"
  background    = "white"
}else if (ibackground == 1){
   foreground    <<- "white"
   background    <<- "black"
}else if (ibackground == 2){
   foreground    <<- "white"
   background    <<- "#282828"
}else{
   stop(paste0(" Invalid ibackground value (",ibackground,")"))
}#end if
#---~---




#--- Define the size of the titles and axes.
if (! "ptsz" %in% ls()){
   ptsz <<- 16
}else{
   ptsz <<- ptsz
}#end if
cex.ptsz <<- 1.0 * min(1.0,ptsz / 15)
cex.main <<- 1.1 * min(1.0,ptsz / 14)
cex.lab  <<- 1.0 * min(1.0,ptsz / 14)
#---~---






#--- Create the default plotting settings for R.
par.user <<- list( bg       = "transparent"
                 , col      = foreground
                 , col.axis = foreground
                 , col.lab  = foreground
                 , col.main = foreground
                 , col.sub  = foreground
                 , fg       = foreground
                 , cex.main = cex.main
                 , cex.lab  = cex.lab
                 , family   = "Helvetica"
                 , mar      = c(5.1,4.4,4.1,2.1)
                 , mgp      = c(2.25,0.25,0)
                 , tcl      = +0.25
                 )#end list
#---~---


#--- Wrapper for loading packages without pop ups.
discreet.require <<- function(...){
   dummy = suppressPackageStartupMessages(suppressWarnings(require(...)))
   return(dummy)
}#end discreet.require
#---~---

#--- Load all packages needed.
loaded.package = list()
loaded.package[["agricolae"     ]] = discreet.require(agricolae     )
loaded.package[["cluster"       ]] = discreet.require(cluster       )
loaded.package[["data.table"    ]] = discreet.require(data.table    )
loaded.package[["egg "          ]] = discreet.require(egg           )
loaded.package[["extrafont "    ]] = discreet.require(extrafont     )
loaded.package[["FactoMineR"    ]] = discreet.require(FactoMineR    )
loaded.package[["fields"        ]] = discreet.require(fields        )
loaded.package[["geojsonsf"     ]] = discreet.require(geojsonsf     )
loaded.package[["geomtextpath"  ]] = discreet.require(geomtextpath  )
loaded.package[["ggtext"        ]] = discreet.require(ggtext        )
loaded.package[["hexbin"        ]] = discreet.require(hexbin        )
loaded.package[["lubridate"     ]] = discreet.require(lubridate     )
loaded.package[["MASS"          ]] = discreet.require(MASS          )
loaded.package[["missMDA"       ]] = discreet.require(missMDA       )
loaded.package[["ncdf4"         ]] = discreet.require(ncdf4         )
loaded.package[["nlme"          ]] = discreet.require(nlme          )
loaded.package[["openair"       ]] = discreet.require(openair       )
loaded.package[["patchwork"     ]] = discreet.require(patchwork     )
loaded.package[["raster"        ]] = discreet.require(raster        )
loaded.package[["RColorBrewer"  ]] = discreet.require(RColorBrewer  )
loaded.package[["reshape2"      ]] = discreet.require(reshape2      )
loaded.package[["rgbif"         ]] = discreet.require(rgbif         )
loaded.package[["rworldmap"     ]] = discreet.require(rworldmap     )
loaded.package[["scales"        ]] = discreet.require(scales        )
loaded.package[["sf"            ]] = discreet.require(sf            )
loaded.package[["smatr"         ]] = discreet.require(smatr         )
loaded.package[["stringr"       ]] = discreet.require(stringr       )
loaded.package[["tabularaster"  ]] = discreet.require(tabularaster  )
loaded.package[["terra"         ]] = discreet.require(terra         )
loaded.package[["tidyverse"     ]] = discreet.require(tidyverse     )
loaded.package[["viridis"       ]] = discreet.require(viridis       )
#--- Packages that must be loaded at the end.
loaded.package[["forecast"      ]] = discreet.require(forecast      )
loaded.package[["ggfortify"     ]] = discreet.require(ggfortify     )
#---~---


#--- Make sure all packages are loaded fine.
loaded.package = unlist(loaded.package)
if (! all(loaded.package)){
   miss = which(! loaded.package)
   cat(" You must install the following packages before using the scripts:","\n")
   for (m in miss) cat(" -> ",names(loaded.package)[m],"\n",sep="")
   risky = readline(" Are you sure you want to proceed [y|N]? ")
   risky = tolower(risky)
   if (! risky %in% c("y","yes")) stop("Missing packages!!!")
}#end if
#---~---



#--- Organise the files so we load them in the right order.
at.first      = c("rconstants.r","unitlist.r")
at.end        = c("fates_varlist.r","hlm_varlist.r")
myself        = c("load.everything.r")
all.scripts   = sort(list.files(path=srcdir,pattern="\\.[RrSsQq]$"))
back.up       = sort(list.files(path=srcdir,pattern="^[~]"))
keep          = ! ( all.scripts %in% at.first
                  | all.scripts %in% at.end
                  | all.scripts %in% myself
                  | all.scripts %in% back.up
                  )#end
middle        = all.scripts[keep]
order.scripts = c(at.first,middle,at.end)
nscripts      = length(order.scripts)
#---~---



#---~---
#     Load all files, in order.  Here we replace the warnings by errors, just to make sure
# that all the functions are clean.
#---~---
warn.orig = getOption("warn")
options(warn=2)
cat(" + Load scripts from ",srcdir,".","\n",sep="")
for (iscript in sequence(nscripts)){
   script.now  = order.scripts[iscript]
   full        = file.path(srcdir,script.now)
   isok        = try(source(full),silent=TRUE)
   if ("try-error" %in% is(isok)){
      options(warn=warn.orig)
      cat("   - Script ",script.now," has bugs!  Check the errors/warnings: ","\n",sep="")
      source(full)
      stop("Source code problem")
   }#end if
}#end for
options(warn=warn.orig)
#---~---



#--- Check for fortran code to be loaded.
all.f90  = sort( c( list.files(path=srcdir,pattern="\\.[Ff]90$")
                  , list.files(path=srcdir,pattern="\\.[Ff]$")
                  )#end c
               )#end sort
nall.f90 = length(all.f90)
for (if90 in sequence(nall.f90)){
   fnow    = file.path(srcdir,all.f90[if90])
   flib.o  = fnow
   flib.o  = gsub(pattern = "\\.[Ff]90$",replacement=".o",x=flib.o)
   flib.o  = gsub(pattern = "\\.[Ff]$"  ,replacement=".o",x=flib.o)
   flib.so = fnow
   flib.so = gsub(pattern = "\\.[Ff]90$",replacement=".so",x=flib.so)
   flib.so = gsub(pattern = "\\.[Ff]$"  ,replacement=".so",x=flib.so)
   flib.sl = fnow
   flib.sl = gsub(pattern = "\\.[Ff]90$",replacement=".sl",x=flib.sl)
   flib.sl = gsub(pattern = "\\.[Ff]$"  ,replacement=".sl",x=flib.sl)

   #--- Select library.
   if (file.exists(flib.so)){
      flib.sx = flib.so
   }else if (file.exists(flib.sl)){
      flib.sx = flib.sl
   }else{
      #--- This is guaranteed to fail, so it will force recompilation.
      flib.sx = flib.o
      #---~---
   }#end if (file.exists(flib.so))
   #---~---



   #--- Check whether dynamic library can be loaded.  In case not, recompile.
   dummy = try(dyn.load(flib.sx),silent=TRUE)
   if ("try-error" %in% is(dummy)){
      dummy = if (file.exists(flib.so)){file.remove(flib.so)}else{character(0)}
      dummy = if (file.exists(flib.sl)){file.remove(flib.sl)}else{character(0)}
      dummy = if (file.exists(flib.o )){file.remove(flib.o )}else{character(0)}
      dummy = rcmd(cmd="SHLIB",cmdargs=fnow  )
      dummy = rcmd(cmd="SHLIB",cmdargs=flib.o)
      #---~---
   }#end if ("try-error" %in% is(dummy))
   #---~---
}#end for (if90 in sequence(nall.f90))
#---~---



#---~---
#       Get rid of the extremely annoying and unnecessary bell, and the extremely annoying
# dplyr messages when using summarise.
#---~---
options(locatorBell=FALSE,dplyr.summarise.inform=FALSE)
#---~---


#--- Assume time zone to be GMT.
Sys.setenv(TZ="GMT")
#---~---


#--- Load GhostScript fonts.
extrafont::loadfonts(device="all")
#---~---


#=======
#=======
