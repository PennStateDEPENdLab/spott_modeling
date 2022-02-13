#!/bin/bash

while read -r ID; do
    sbatch --export=subj=$ID sbatch_one_ID.bash
    echo $ID
done < sim_IDs.txt


