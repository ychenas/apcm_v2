#!/bin/bash
##################
## NCHC TWNIA3 ###
##################
#SBATCH --job-name=compress.job
#SBATCH --ntasks=1
#SBATCH --account=MST111248
##SBATCH --ntasks-per-node=56
#SBATCH -p ct56
##SBATCH -p ct560
##SBATCH -p ct2k
##SBATCH -p t3atm_600
#SBATCH -o Script_Compress.out
###SBATCH --time=8-00:00:00
##SBATCH --mem-per-cpu=1200MB
##SBATCH --mail-type=ALL

#**************************************************************
# Created by Yi-Ying Chen for NCHC TAIWANIA3
#**************************************************************

