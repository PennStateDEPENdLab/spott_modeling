#!/bin/bash

#SBATCH -p general
#SBATCH -N 1
#SBATCH --mem 64g
#SBATCH -n 8
#SBATCH -t 2:00:00

#module use /proj/mnhallqlab/sw/modules
#module load matlab/2021a

#matlab  -nodisplay -r "run matlabtest.m"
#/Applications/MATLAB_R2020b.app/bin/matlab -nodisplay -r fit_group_vba_ffx_RM

export var=cond000011

/Applications/MATLAB_R2020b.app/bin/matlab -nodisplay -r matlabtest

#module load r
#R CMD BATCH --no-save --no-restore RTest.R


