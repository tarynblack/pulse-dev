#!/bin/bash
#SBATCH -J F_01_995_999		# job name
#SBATCH -o F_01_995_999.out%j	# output and error file name (%j expands to jobID)
#SBATCH -n 64			# total number of mpi tasks requested
#SBATCH -N 4			# Total number of nodes requested (16 cores/node)
#SBATCH -p normal		# queue (partition) -- normal, development, etc.
#SBATCH -t 30:00:00		# run time (hh:mm:ss) - 1.0 hours

ibrun ./plume.exe		# run the MPI executable named a.out
