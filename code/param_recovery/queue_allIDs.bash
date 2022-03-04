#!/bin/bash

while read -r ID; do
    sbatch --export=subj=$ID sbatch_one_ID.bash
done < sim_IDs.txt


