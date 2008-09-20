### * <HEADER>
###
attach(NULL, name = "CheckExEnv")
assign("nameEx", 
       local({
	   s <- "__{must remake R-ex/*.R}__"
           function(new) {
               if(!missing(new)) s <<- new else s
           }
       }),
       pos = "CheckExEnv")
## Add some hooks to label plot pages for base and grid graphics
assign("base_plot_hook",
       function() {
           pp <- par(c("mfg","mfcol","oma","mar"))
           if(all(pp$mfg[1:2] == c(1, pp$mfcol[2]))) {
               outer <- (oma4 <- pp$oma[4]) > 0; mar4 <- pp$mar[4]
               mtext(sprintf("help(\"%s\")", nameEx()), side = 4,
                     line = if(outer)max(1, oma4 - 1) else min(1, mar4 - 1),
               outer = outer, adj = 1, cex = .8, col = "orchid", las=3)
           }
       },
       pos = "CheckExEnv")
assign("grid_plot_hook",
       function() {
           grid::pushViewport(grid::viewport(width=grid::unit(1, "npc") - 
                              grid::unit(1, "lines"), x=0, just="left"))
           grid::grid.text(sprintf("help(\"%s\")", nameEx()),
                           x=grid::unit(1, "npc") + grid::unit(0.5, "lines"),
                           y=grid::unit(0.8, "npc"), rot=90,
                           gp=grid::gpar(col="orchid"))
       },
       pos = "CheckExEnv")
setHook("plot.new",     get("base_plot_hook", pos = "CheckExEnv"))
setHook("persp",        get("base_plot_hook", pos = "CheckExEnv"))
setHook("grid.newpage", get("grid_plot_hook", pos = "CheckExEnv"))
assign("cleanEx",
       function(env = .GlobalEnv) {
	   rm(list = ls(envir = env, all.names = TRUE), envir = env)
           RNGkind("default", "default")
	   set.seed(1)
   	   options(warn = 1)
	   .CheckExEnv <- as.environment("CheckExEnv")
	   delayedAssign("T", stop("T used instead of TRUE"),
		  assign.env = .CheckExEnv)
	   delayedAssign("F", stop("F used instead of FALSE"),
		  assign.env = .CheckExEnv)
	   sch <- search()
	   newitems <- sch[! sch %in% .oldSearch]
	   for(item in rev(newitems))
               eval(substitute(detach(item), list(item=item)))
	   missitems <- .oldSearch[! .oldSearch %in% sch]
	   if(length(missitems))
	       warning("items ", paste(missitems, collapse=", "),
		       " have been removed from the search path")
       },
       pos = "CheckExEnv")
assign("ptime", proc.time(), pos = "CheckExEnv")
## at least one package changes these via ps.options(), so do this
## before loading the package.
## Use postscript as incomplete files may be viewable, unlike PDF.
## Choose a size that is close to on-screen devices, fix paper
ps.options(width = 7, height = 7, paper = "a4", reset = TRUE)
grDevices::postscript("Rlabkey-Ex.ps")
		      
assign("par.postscript", graphics::par(no.readonly = TRUE), pos = "CheckExEnv")
options(contrasts = c(unordered = "contr.treatment", ordered = "contr.poly"))
options(warn = 1)    
library('Rlabkey')

assign(".oldSearch", search(), pos = 'CheckExEnv')
assign(".oldNS", loadedNamespaces(), pos = 'CheckExEnv')
cleanEx(); nameEx("labkey.deleteRows")
### * labkey.deleteRows

flush(stderr()); flush(stdout())

### Name: labkey.deleteRows
### Title: Delete rows of data in a labkey database
### Aliases: labkey.deleteRows
### Keywords: IO

### ** Examples

# Examples to be updated when labkey.org has 8.3
#mydf <- data.frame(lsid=c('urn:lsid:labkey.org:****','urn:lsid:labkey.org:****','urn:lsid:labkey.org.****'),stringsAsFactors=FALSE))

#mydata <- labkey.deleteRows(baseUrl="https://www.labkey.org", folderPath="/home/Study/demo", schemaName="study", queryName="HIV Test Results", toDelete=mydf)




cleanEx(); nameEx("labkey.executeSql")
### * labkey.executeSql

flush(stderr()); flush(stdout())

### Name: labkey.executeSql
### Title: Retrieve data from a labkey database using SQL commands
### Aliases: labkey.executeSql
### Keywords: IO

### ** Examples


library(Rlabkey)

# Retrieve participant id, visit date and hemoglobin from Lab Results table
# from www.labkey.org
### NOTE: This won't work until 8.3 is up on www.labkey.org ####

#mydata <- labkey.executeSql(baseUrl="https://www.labkey.org", folderPath="/home/Study/demo", 
# schemaName="study", sql= 'select "Lab Results".ParticipantId, "Lab Results".Labdt, 
# "Lab Results".Labhemo from "Lab Results"')




cleanEx(); nameEx("labkey.insertRows")
### * labkey.insertRows

flush(stderr()); flush(stdout())

### Name: labkey.insertRows
### Title: Insert new rows of data into a labkey database
### Aliases: labkey.insertRows
### Keywords: IO

### ** Examples

# Example to be modified when 8.3 is on labkey.org
# Insert some data:
#mydf <- data.frame(SequenceNum=c(2,3), lsid=c("URG345","URG346"), participantId=c(5055, 5056), stringsAsFactors=FALSE)

#mydata <- labkey.insertRows(baseUrl="https://www.labkey.org", folderPath="/home/Study/demo", schemaName="study", queryName="HIV Test Results, toInsert=mydf)





cleanEx(); nameEx("labkey.selectRows")
### * labkey.selectRows

flush(stderr()); flush(stdout())

### Name: labkey.selectRows
### Title: Retrieve data from a labkey database using url specifications
### Aliases: labkey.selectRows
### Keywords: IO

### ** Examples


## Retrieving data from the Labkey.org web site:

library(Rlabkey)

# Retrieve HIV Test Results and plot Western Blot data
getdata <- labkey.selectRows(baseUrl="https://www.labkey.org", folderPath="/home/Study/demo", 
                                schemaName="study", queryName="HIV Test Results")
plot(factor(getdata$"HIV Western Blot"), main="HIV Western Blot")

# Select columns and apply filters
myfilters<- makeFilter(c("HIVLoadQuant","GREATER_THAN",500), c("HIVRapidTest","EQUALS","Positive"))

getdata <- labkey.selectRows(baseUrl="https://www.labkey.org", folderPath="/home/Study/demo", 
schemaName="study", queryName="HIV Test Results", 
colSelect=c("ParticipantId","HIVDate","HIVLoadQuant","HIVRapidTest"), colFilter=myfilters)





cleanEx(); nameEx("labkey.updateRows")
### * labkey.updateRows

flush(stderr()); flush(stdout())

### Name: labkey.updateRows
### Title: Update existing rows of data in a labkey database
### Aliases: labkey.updateRows
### Keywords: IO

### ** Examples


# Examples to be updated when labkey.org has 8.3
#mydf=data.frame(lsid=c('urn:lsid:labkey.org:****','urn:lsid:labkey.org:****') , ParticipantId=c(700010040, 700010066), MVdt=c('09-Aug-06','09-Aug-06'), MVcomm=c('y','y'), MVswitch=c(88,99), stringsAsFactors=FALSE)

#mydata <- labkey.updateRows(baseUrl="https://www.labkey.org", folderPath="/home/Study/demo", schemaName="study", queryName="HIV Test Results", toUpdate=mydf)





cleanEx(); nameEx("makeFilter")
### * makeFilter

flush(stderr()); flush(stdout())

### Name: makeFilter
### Title: Builds an array of filters
### Aliases: makeFilter
### Keywords: file

### ** Examples


# Specify two filters:
myfilters<- makeFilter(c("HIVLoadQuant","GREATER_THAN",500), c("HIVRapidTest","EQUALS","Positive"))

# Filter using "equals one of" operator:
myfilter2 <- makeFilter(filter1=c("HIVLoadIneq","EQUALS_ONE_OF","Equals ; Less than"))

# Use in labkey.selectRows function
getdata <- labkey.selectRows(baseUrl="https://www.labkey.org", folderPath="/home/Study/demo", 
schemaName="study", queryName="HIV Test Results", 
colSelect=c("ParticipantId","HIVDate","HIVLoadQuant","HIVRapidTest"), colFilter=myfilters)





### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
