#!/bin/sh

#init git 
#git init

#add files 
git add *.R *.sh template* 

#commit files 
git commit -m " add scripts both for old and update scripts and some files !"


#set the origin, only for the first time
git remote add origin git@github.com:ychenas/apcm_v2.git

#add branch name, here is main 
git branch -M main

#push commit files to the server/origin as master or branch 
git push -u origin main


