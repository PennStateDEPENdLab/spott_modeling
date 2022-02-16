#!/bin/bash

#SBATCH -p general
#SBATCH -N 1
#SBATCH --mem 64g
#SBATCH -n 8
#SBATCH -t 2:00:00


[ -z "$subj" ] && echo "No subject ID variable passed in" && exit 1

#echo $subj

module use /proj/mnhallqlab/sw/modules
module load matlab/2021a

#matlab  -nodisplay -r fit_group_vba_ffx_RM /proj/mnhallqlab/projects/spott_modeling/par_sim/vba_${subj}.out
matlab  -nodisplay -r matlabtest | tee vba_${subj}.out

#/Applications/MATLAB_R2020b.app/bin/matlab -nodisplay -r fit_group_vba_ffx_RM
#/Applications/MATLAB_R2020b.app/bin/matlab -nodisplay -r matlabtest

