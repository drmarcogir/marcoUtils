#' Produce plot from model
#' 
#' 
#' 
#' 
#'


# read in dataset
env<-read.csv("/home/marco/Dropbox/szn/data/hotspots/envdata/combinedenv_ws.csv")
spenv<-read.csv("/home/marco/Dropbox/szn/data/hotspots/spenv/spenv_w_s.csv")

# a bit of clearning
env<-subset(env,depth >= -200)

# log chlor (various outliers)
env$chlor<-log(env$chlor)

# invert sign for depth
env$depth<--env$depth

# standardize variables
env<-scalemg(indf=env,group="spid",colstand=c("slope","depth","sla","sst","chlor"))

# re-merge with species data
spenv<-merge(spenv,env[,!names(env) %in% c("x","y")])

# set up Matern correlation
env$x<-env$x/1000
env$y<-env$y/1000

spenv$x_sp<-spenv$x_sp/1000
spenv$y_sp<-spenv$y_sp/1000



mgstack<-stackcreate(insp=spenv,inenv=env,spcol="spid",predl=c("depths","slopes","slas","chlors","ssts"),
cutoff=0.5,max.edge=40)


inmod="/home/marco/inlamodels/mod_sp1"
insp=spenv
predl=c("depth","slope","sla","chlor","sst")
instack=mgstack



smoothplot<-function(species=2,instack=mgstack,inmod,insp=spenv,predl=c("depth","slope","sla","chlor","sst")){
# subset species data
spdf<-insp[insp$spid %in% species,]
# get stack 
tmpstack<-mgstack[[1]]
# load model
load(inmod)
# combined fitted values into a dataframe
I0 = inla.stack.index(tmpstack, "sp")
E0 = mod$summary.fitted.values[I0$data,]
fitsp<-data.frame(spdf,E0)
fitsp.l<-melt(fitsp[,c(predl,"mean")],id.vars=c("mean"))
# create plot
p<-ggplot(data=fitsp.l,aes(x=value,y=mean))+geom_smooth()+facet_wrap(~variable,scales="free")




