#!/bin/bash
echo "running batch scripts to creat job files from template.job"
#cd /home/u9197633/R_scripts/R_spot/
cd /home/u3176872/LULCC/code/collabcode/
# LOAD R MODULE 
#module load r/3.1.1

# submit the job on queue 
# chmod u+x $CONFIG_FILE
# qsub -jeo -q short $CONFIG_FILE
CONT=0

# argunment 1 to 6 are parameters for the function SPOT_step1_230303.R
 
# arg1 : xmin min of XID of GRID BOX within the AOI(Area Of Interest)
# arg2 : xmax max of XID of GRID BOX within the AOI
# arg3 : ymin min of YID of GRID BOX within the AOI 
# arg4 : ymax max of YID of GRID BOX within the AOI
# arg5 : wrk_yr selected working year
# arg6 : aoi_region selected region of AOI (which was define in the function) 
 ## declare an array variable
 
#declare -a grid.x=("224" "225" "226" "227" "228" "229" "230" "231" "232" "234" "235" "236" "237" "238" "239" "240")
#declare -a grid.y=("185" "186" "187" "188" "189" "190" "191" "192" "193" "194" "195" "196" "197" "198" "199" "200" "201" "202" "203" "204" "205" "206" "207" "208" "209" "210" "211" "212")

## now loop through the above array
#for iyr  in "${wrk_yr[@]}"

#do 

#for imon in "${wrk_mn[@]}"
#do
#   echo "Doing job for year:${iyr},  month: ${imon}!"  
#   Rscript  fun_hiram_compress_ncdf.R $iyr $imon > "log_${iyr}_${imon}.txt"
#done

#done
 
#for irun in {1..1}

#do
#combine based
#case 1 
#if [ "${irun}" == "1" ]; then 

wrk_yr="2022"

#aoi_reg="GRID_wholetaiwan"
#taiwan   
#for ix in {224..242..4}
#do
#xmin=$ix; let xmax=$ix+3
       
#for iy in {185..215..5} 
#do
#ymin=$iy; let ymax=$iy+4 

#separate to first and second half taiwan to run because sometimes jobs don't all run fully for some reason, misses some
#aoi_reg="GRID_firsthalf_taiwan"
#taiwan   
#for ix in {224..228..4}
#do
#xmin=$ix; let xmax=$ix+3
       
#for iy in {185..215..5} 
#do
#ymin=$iy; let ymax=$iy+4 

#aoi_reg="GRID_secondhalf_taiwan"
#taiwan   
#for ix in {232..242..4}
#do
#xmin=$ix; let xmax=$ix+3
       
#for iy in {185..215..5} 
#do
#ymin=$iy; let ymax=$iy+4 

#aoi_reg="GRID_taipei"
#taipei
#xmin=234
#xmax=237
#ymin=209
#ymax=212

aoi_reg="GRID_taoyuan"
#taoyuan
xmin=231
xmax=234
ymin=208
ymax=210

#aoi_reg="GRID_pingtung"
#pingtung
#xmin=229
#xmax=231
#ymin=185
#ymax=186

#aoi_reg="GRID_north"
#north
#xmin=224
#xmax=240
#ymin=205
#ymax=212

#aoi_reg="GRID_central"
#central
#xmin=224
#xmax=230
#ymin=195
#ymax=204

#aoi_reg="GRID_east"
#east
#xmin=231
#xmax=240
#ymin=195
#ymax=204

#aoi_reg="GRID_south"
#south
#xmin=224
#xmax=240
#ymin=185
#ymax=194


#initiate a job

#counter for the iteration or loop 
CONT=$(($CONT+1))
#echo "The No. of for loop: $CONT ."

#copy the template from template.job (script for submitting R job with some argunments)
TEMPLATE_FILE="template.submit"
CONFIG_FILE="R_${wrk_yr}_${xmin}_${xmax}_${ymin}_${ymax}_${aoi_reg}.job"
cp $TEMPLATE_FILE $CONFIG_FILE

#dynamic text for the argunments, input and output files and directory
log_file="R_${wrk_yr}_${xmin}_${xmax}_${ymin}_${ymax}_${aoi_reg}.log"

#dynamic text for the argunments, input and output files and directory
out_file="R_${wrk_yr}_${xmin}_${xmax}_${ymin}_${ymax}_${aoi_reg}.out"

#dynamic text for the argunments, input and output files and directory
name_file="R_${wrk_yr}_${xmin}_${xmax}_${ymin}_${ymax}_${aoi_reg}.name"
#
#
TARGET_KEY="work_dir"
#REPLACEMENT_VALUE="\/home\/u9197633\/R_scripts\/R_spot\/"
REPLACEMENT_VALUE="\/home\/u3176872\/LULCC\/code\/collabcode\/"
sed -i -c "s/$TARGET_KEY/$REPLACEMENT_VALUE/" $CONFIG_FILE

TARGET_KEY="R_filename"
REPLACEMENT_VALUE="pondclassify.R"
sed -i -c "s/$TARGET_KEY/$REPLACEMENT_VALUE/" $CONFIG_FILE

# RE-SET THE PARAMETER VALUES FOR EACH JOB
TARGET_KEY="xmin"
REPLACEMENT_VALUE=${xmin}
sed -i -c "s/$TARGET_KEY/$REPLACEMENT_VALUE/" $CONFIG_FILE

TARGET_KEY="xmax"
REPLACEMENT_VALUE=${xmax}
sed -i -c "s/$TARGET_KEY/$REPLACEMENT_VALUE/" $CONFIG_FILE

TARGET_KEY="ymin"
REPLACEMENT_VALUE=${ymin}
sed -i -c "s/$TARGET_KEY/$REPLACEMENT_VALUE/" $CONFIG_FILE

TARGET_KEY="ymax"
REPLACEMENT_VALUE=${ymax}
sed -i -c "s/$TARGET_KEY/$REPLACEMENT_VALUE/" $CONFIG_FILE

TARGET_KEY="aoi_reg"
REPLACEMENT_VALUE=${aoi_reg}
sed -i -c "s/$TARGET_KEY/$REPLACEMENT_VALUE/" $CONFIG_FILE

TARGET_KEY="wrk_yr"
REPLACEMENT_VALUE=${wrk_yr}
sed -i -c "s/$TARGET_KEY/$REPLACEMENT_VALUE/" $CONFIG_FILE

TARGET_KEY="log_file"
REPLACEMENT_VALUE=${log_file}
sed -i -c "s/$TARGET_KEY/$REPLACEMENT_VALUE/" $CONFIG_FILE


TARGET_KEY="R_job.name"
REPLACEMENT_VALUE=${job_file}
sed -i -c "s/$TARGET_KEY/$REPLACEMENT_VALUE/" $CONFIG_FILE

TARGET_KEY="R_job.out"
REPLACEMENT_VALUE=${out_file}
sed -i -c "s/$TARGET_KEY/$REPLACEMENT_VALUE/" $CONFIG_FILE


echo " arg1:$xmin, arg2:$xmax, arg3:$ymin, arg4:$ymax, arg5:$wrk_yr, arg6:$aoi_reg "  
# submit the job on queuing system  
chmod u+x $CONFIG_FILE

#run on curie by tesing Queue
#ccc_msub -q standard -T 1800 -A gen6328 -Q TEST $CONFIG_FILE 
#normal run on CURIE
#ccc_msub -q standard -T 86400 -A gen6328 $CONFIG_FILE 
#run on airain
#ccc_msub -q ivybridge -T 86400 -A dsm $CONFIG_FILE 

#print the job name &  excute the job 
echo $CONFIG_FILE
sbatch $CONFIG_FILE

echo $CONT


#done   
#done 



#case 2
#if [ "${irun}" == "2" ]; then 
#xmin=224; xmax=240; ymin=205; ymax=212;wrk_yr="2022"; aoi_reg="GRIDH"   
#fi


