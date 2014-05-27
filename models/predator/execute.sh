#!/bin/sh
#PBS -N predator-41
#PBS -l ncpus=15
#PBS -q dlab
#PBS -m abe
#PBS -d /home/ifuenzalida/Predator_simulation

	time mpiexec.mpich2 -prepend-rank -n 25 PISKa -i predator25.cka -t 60 -p 1000 -sync-t 60 -d comparts/ -seed 234

