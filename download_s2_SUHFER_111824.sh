#!/bin/bash
#SBATCH --job-name=s2_download_SUHFER
#SBATCH --account=fc_mel
#SBATCH --partition=savio3_htc
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=32
#SBATCH --time=48:00:00
#SBATCH --mail-type=ALL
#SBATCH --mail-user=erin_carroll@berkeley.edu
#ssh erin_carroll

## Command to run:
python /global/home/users/erin_carroll/jobs/download_s2_SUHFER_111824.py