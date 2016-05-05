library(R.matlab)
library(lme4)
library(LMERConvenienceFunctions)
library(oro.nifti)

rm(list=ls())

wdir = '/Volumes/HDD/Google Drive/Sean/Projects/Comp-Robot/MEG/data/collated_5mm'
setwd(wdir)
tfile = 'template.nii'
template = readNIfTI(tfile)
nodes = readNIfTI('../../final_5mm/278nodes.nii')

mask = template;

template@.Data[,,,] = 0

group.img = template;
ses.img = template;
imagery.img = template;

graphs.ses.img = template;
robot.ses.img = template;
left.ses.img = template;
right.ses.img = template;
groups.s1.img = template;
groups.s2.img = template;
imagery.s1.img = template;
imagery.s2.img = template;

# Storage

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

for(x in 1:d1) {
  
  for(y in 1:d2) {
    
    for(z in 1:d3) {
      
      if(nodes@.Data[x,y,z]>0) {
              
        dc1 = list()
        dc2 = list()
        dc3 = list()
        dc4 = list()
        dc5 = list()
        
        for(groupno in 1:2) {
          
          group = groups[groupno]
          
          for(sessionno in 1:2) {
            
            session = sessions[sessionno]
            
            for(imageryno in 1:2) {
              
              imagery = imagerys[imageryno]
              
              for(sub in 1:dim(get(paste('d.',group,'.',session,'.',imagery,sep='')))[4]) {
                
                substr = paste(group,sub,sep='')
                val = get(paste('d.',group,'.',session,'.',imagery,sep=''))[x,y,z,sub]
                
                dc1[length(dc1)+1] = substr
                dc2[length(dc2)+1] = group
                dc3[length(dc3)+1] = session
                dc4[length(dc4)+1] = imagery
                dc5[length(dc5)+1] = val
                
              } # Sub
              
            } # Imagery
            
          } # Session
          
        } #Group
        
        # Format data
        
        sample = cbind(dc1,dc2,dc3,dc4,dc5)
        colnames(sample) = c('sub','group','session','imagery','act')
        sample<-data.frame(sample)
        
        sample$sub<-as.character(sample$sub)
        sample$sub<-as.factor(sample$sub)
        sample$group<-as.character(sample$group)
        sample$group<-as.factor(sample$group)
        sample$session<-as.character(sample$session)
        sample$session = as.factor(sample$session)
        sample$imagery<-as.character(sample$imagery)
        sample$imagery = as.factor(sample$imagery)
        sample$act<-as.character(sample$act)
        sample$act<-as.factor(sample$act)
        
        # Level sample
        
        sample$group<-relevel(sample$group,'graphs')
        sample$session<-relevel(sample$session,'s1')
        sample$imagery<-relevel(sample$imagery,'l')
        
        # Main effect model
        
        m.m = lmer(act~group+session+imagery+(1|sub),data=sample)
        m.m.aov = pamer.fnc(m.m)
        m.m.smry = summary(m.m)$coefficients
        df = m.m.aov['group','lower.den.df']
        
        # Get T and P values
        
        t.group<-m.m.smry['grouprobot','t value'] # Pos: Greater for robot
        t.ses<-m.m.smry['sessions2','t value'] # Pos: Greater for s2
        t.img<-m.m.smry['imageryr','t value'] # Pos: Greater for Right
        p.group = 2*(1-pt(abs(t.group),df))
        p.ses = 2*(1-pt(abs(t.ses),df))
        p.img = 2*(1-pt(abs(t.img),df))
        
        # Interaction model
        
        t.graphs.ses = 0.001;
        p.graphs.ses = 1;
        t.robot.ses = 0.001;
        p.robot.ses = 1;
        
        t.groups.s1 = 0.001;
        p.groups.s1 = 1;
        t.groups.s2 = 0.001;
        p.groups.s2 = 1;
        
        t.left.ses = 0.001;
        p.left.ses = 1;
        t.right.ses = 0.001;
        p.right.ses = 1;
        
        t.imagery.s1 = 0.001;
        p.imagery.s1 = 1;
        t.imagery.s2 = 0.001;
        p.imagery.s2 = 1;
        
        if(sample$act[1]!=0) {
            
          m.i = lmer(act~group+session+imagery+group:session+session:imagery+(1|sub),data=sample)
          m.i.aov = pamer.fnc(m.i)
          m.i.smry = summary(m.i)$coefficients
          
          # If significant group:session, get T and P values
    
          if(m.i.aov['group:session','lower.p.val']<=0.05) {
            
            # group:session interaction, graph[2 -1]
            sample$group<-relevel(sample$group,'graphs')
            sample$session<-relevel(sample$session,'s1')
            m.i<-update(m.i)
            m.i.smry = summary(m.i)$coefficients
            t.graphs.ses<-m.i.smry['sessions2','t value']
            p.graphs.ses = 2*(1-pt(abs(t.graphs.ses),df))
            
            # relevel to get robot[1]-graphs[1]
            t.groups.s1<-m.i.smry['grouprobot','t value']
            p.groups.s1 = 2*(1-pt(abs(t.groups.s1),df))
            
            # relevel to get robot[2]-graphs[2]
            sample$session<-relevel(sample$session,'s2')
            m.i<-update(m.i)
            m.i.smry = summary(m.i)$coefficients
            t.groups.s2<-m.i.smry['grouprobot','t value']
            p.groups.s2 = 2*(1-pt(abs(t.groups.s2),df))
            
            # relevel to get robot[2-1]
            sample$group<-relevel(sample$group,'robot')
            sample$session<-relevel(sample$session,'s1')
            m.i<-update(m.i)
            m.i.smry = summary(m.i)$coefficients
            t.robot.ses<-m.i.smry['sessions2','t value']
            p.robot.ses = 2*(1-pt(abs(t.robot.ses),df))
    
          } # If group:session is significant
          
          # If significant session:imagery, get T and P values
          
          if(m.i.aov['session:imagery','lower.p.val']<=0.05) {
            
            # session:imagery interaction, left[2 -1]
            sample$imagery<-relevel(sample$imagery,'l')
            m.i<-update(m.i)
            m.i.smry = summary(m.i)$coefficients
            t.left.ses<-m.i.smry['sessions2','t value']
            p.left.ses = 2*(1-pt(abs(t.left.ses),df))
            
            # relevel to get right[2-1]
            sample$imagery<-relevel(sample$imagery,'r')
            m.i<-update(m.i)
            m.i.smry = summary(m.i)$coefficients
            t.right.ses<-m.i.smry['sessions2','t value']
            p.right.ses = 2*(1-pt(abs(t.right.ses),df))
            
            # relevel to get right-left[1]
            sample$session<-relevel(sample$session,'s1')
            sample$imagery<-relevel(sample$imagery,'l')
            m.i<-update(m.i)
            m.i.smry = summary(m.i)$coefficients
            t.imagery.s1<-m.i.smry['imageryr','t value']
            p.imagery.s1 = 2*(1-pt(abs(t.imagery.s1),df))
            
            # relevel to get right-left[2]
            sample$session<-relevel(sample$session,'s2')
            m.i<-update(m.i)
            m.i.smry = summary(m.i)$coefficients
            t.imagery.s2<-m.i.smry['imageryr','t value']
            p.imagery.s2 = 2*(1-pt(abs(t.imagery.s2),df))
            
          } # If session:imagery is significant
          
        } # If there's data
        
        # For each, if p is sig, write to grid
        
        group.img@.Data[x,y,z,1] = (t.group/abs(t.group)) * (1-p.group);                      # Pos: Greater for robot
        ses.img@.Data[x,y,z,1] = (t.ses/abs(t.ses)) * (1-p.ses);                              # Pos: Greater for s2
        imagery.img@.Data[x,y,z,1] = (t.img/abs(t.img)) * (1-p.img);                          # Pos: Greater for right
        graphs.ses.img@.Data[x,y,z,1] = (t.graphs.ses/abs(t.graphs.ses)) * (1-p.graphs.ses);  # Pos: Greater for s2 [graphs only]
        robot.ses.img@.Data[x,y,z,1] = (t.robot.ses/abs(t.robot.ses)) * (1-p.robot.ses);      # Pos: Greater for s2 [robot only]
        left.ses.img@.Data[x,y,z,1] = (t.left.ses/abs(t.left.ses)) * (1-p.left.ses);          # Pos: Greater for s2 [left hand]
        right.ses.img@.Data[x,y,z,1] = (t.right.ses/abs(t.right.ses)) * (1-p.right.ses);      # Pos: Greater for s2 [right hand]
        groups.s1.img@.Data[x,y,z,1] = (t.groups.s1/abs(t.groups.s1)) * (1-p.groups.s1);      # Pos: Greater for robot [s1 only]
        groups.s2.img@.Data[x,y,z,1] = (t.groups.s2/abs(t.groups.s2)) * (1-p.groups.s2);      # Pos: Greater for robot [s2 only]
        imagery.s1.img@.Data[x,y,z,1] = (t.imagery.s1/abs(t.imagery.s1)) * (1-p.imagery.s1);      # Pos: Greater for right [s1 only]
        imagery.s2.img@.Data[x,y,z,1] = (t.imagery.s2/abs(t.imagery.s2)) * (1-p.imagery.s2);      # Pos: Greater for right [s2 only]
      
        print(paste(x,y,z,sep=' '))
      
      }
      
    } # Z loop
    
  } # Y loop
  
} # X loop

setwd('../../final_5mm')

# Main effect maps

writeNIfTI(group.img,'main_robot-graphs')
writeNIfTI(ses.img,'main_s2-s1')
writeNIfTI(imagery.img,'main_right-left')

# Imagery by session maps

writeNIfTI(left.ses.img,'int_s2-s1_left')
writeNIfTI(right.ses.img,'int_s2-s1_right')
writeNIfTI(imagery.s1.img,'int_right-left_s1')
writeNIfTI(imagery.s2.img,'int_right-left_s2')

# Group by session maps

writeNIfTI(graphs.ses.img,'int_s2-s1_graphs')
writeNIfTI(robot.ses.img,'int_s2-s1_robot')
writeNIfTI(groups.s1.img,'int_robot-graphs_s1')
writeNIfTI(groups.s2.img,'int_robot-graphs_s2')
