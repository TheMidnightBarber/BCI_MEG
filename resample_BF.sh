#!/bin/bash

cd '/Volumes/HDD/Google Drive/Sean/Projects/Comp-Robot/MEG/data'
for sub in bci*
do

	for ses in 1 2
	do
	
		mkdir $sub/ds_session$ses
		
		cd $sub/session$ses
		
		for file in *
		do
			
			/Volumes/HDD/programs/AFNI/3dresample -inset $file -dxyz 10 10 10 -prefix ../ds_session$ses/$file
			
		done
		
		cd ../..
		
	done
	
done