library(R.matlab)
library(oro.nifti)
library(data.table)

rm(list=ls())

# Initial setup

wdir = '/Volumes/HDD/Google Drive/Sean/Projects/Comp-Robot/MEG/data/collated_beta_5mm'
setwd(wdir)
tfile = 'template.nii'
template = readNIfTI(tfile)
doneone = 0;

# Get the number of tests to be done

temp = matrix(template, nrow = 1, byrow = TRUE)
nvox = length(temp)

# Get participant variables

stats = read.csv('/Volumes/HDD/data/BCI/work/participant_stats.csv')
row.names(stats) = stats[,1]
subs.g = c('02','04','06','08','12','14','24','30')
subs.r = c('01','03','05','07','11','19','21','25')

# Storage

output = data.frame
groups = c('robot','graphs')
sessions = c('s1','s2')
imagerys = c('l','r')

# Load all data

d.graphs.s1.l = readMat('GraphsAvg_session1_left_15-30Hz.mat'); d.graphs.s1.l = d.graphs.s1.l$dat
d.graphs.s1.r = readMat('GraphsAvg_session1_right_15-30Hz.mat'); d.graphs.s1.r = d.graphs.s1.r$dat
d.graphs.s2.l = readMat('GraphsAvg_session2_left_15-30Hz.mat'); d.graphs.s2.l = d.graphs.s2.l$dat
d.graphs.s2.r = readMat('GraphsAvg_session2_right_15-30Hz.mat'); d.graphs.s2.r = d.graphs.s2.r$dat
d.robot.s1.l = readMat('RobotAvg_session1_left_15-30Hz.mat'); d.robot.s1.l = d.robot.s1.l$dat
d.robot.s1.r = readMat('RobotAvg_session1_right_15-30Hz.mat'); d.robot.s1.r = d.robot.s1.r$dat
d.robot.s2.l = readMat('RobotAvg_session2_left_15-30Hz.mat'); d.robot.s2.l = d.robot.s2.l$dat
d.robot.s2.r = readMat('RobotAvg_session2_right_15-30Hz.mat'); d.robot.s2.r = d.robot.s2.r$dat

# Main loop

d1 = dim(d.graphs.s1.l)[1]
d2 = dim(d.graphs.s1.l)[2]
d3 = dim(d.graphs.s1.l)[3]

# Make a coordinate table

coords = cbind(seq_len(nvox),rep(0,nvox),rep(0,nvox),rep(0,nvox),rep(0,nvox))
for (x in 1:d1) {
  for (y in 1:d2) {
    for (z in 1:d3) {
      r = which(coords[,2]==0)[1]
      coords[r,2] = x
      coords[r,3] = y
      coords[r,4] = z
      if (template@.Data[x,y,z,1!=0]) {
          coords[r,5] = 1;
      }
    }
  }
}

coords = as.data.frame(coords)
colnames(coords) = c('voxel','x','y','z','brain')

backup = coords
coords = coords[coords$brain==1,]
voxseq = coords[,1]

# Main function

output = lapply(voxseq,function(i) {
  
  x = coords[coords$voxel==i,2]
  y = coords[coords$voxel==i,3]
  z = coords[coords$voxel==i,4]
                
  dc1 = list()
  dc2 = list()
  dc3 = list()
  dc4 = list()
  dc5 = list()
  dcAge = list()
  dcHan = list()
  
  for(groupno in 1:2) {
    
    group = groups[groupno]
    
    for(sessionno in 1:2) {
      
      session = sessions[sessionno]
      
      for(imageryno in 1:2) {
        
        imagery = imagerys[imageryno]
        
        for(sub in 1:dim(get(paste('d.',group,'.',session,'.',imagery,sep='')))[4]) {
          
          # Build subject ID to get stats
          if(group=='graphs') {
            ID = paste('BCI15-',subs.g[sub],sep='')
          }
          if(group=='robot') {
            ID = paste('BCI15-',subs.r[sub],sep='')
          }
          age = stats[ID,'Age']
          handedness = stats[ID,'Handedness']
          
          substr = paste(group,sub,sep='')
          val = get(paste('d.',group,'.',session,'.',imagery,sep=''))[x,y,z,sub]
          
          dc1[length(dc1)+1] = ID
          dcAge[length(dcAge)+1] = age
          dcHan[length(dcHan)+1] = handedness
          dc2[length(dc2)+1] = group
          dc3[length(dc3)+1] = session
          dc4[length(dc4)+1] = imagery
          dc5[length(dc5)+1] = val
          
        } # Sub
        
      } # Imagery
      
    } # Session
    
  } #Group
  
  # Format data
  
  print(paste(x,y,z,sep=' '))
  
  sample = cbind(dc1,dcAge,dcHan,dc2,dc3,dc4,x,y,z,dc5)
  colnames(sample) = c('sub','age','hand','group','session','imagery','x','y','z','act')
  sample<-data.frame(sample, stringsAsFactors='FALSE')

})

output = rbindlist(output)

save(output,file='/Volumes/HDD/Google Drive/Sean/Projects/Comp-Robot/MEG/data/beta_5mm_fulldat.R')