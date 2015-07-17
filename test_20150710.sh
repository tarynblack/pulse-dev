#PBS -N test_20150710
#PBS -l nodes=1:ppn=64
#PBS -l walltime=60:00:00
#PBS -q atlas-6
#PBS -k oe
#PBS -j oe
#PBS -m abe
#PBS -l mem=240gb

RSH=/usr/bin/rsh
MYMPI=/usr/local/mvapich2/1.9/intel-13.2.146
MYDIR=$HOME/data2/test_20150710
EXE=$MYDIR/plume.exe
PATH="$MYMPI/bin:$PATH"; export PATH

MYPROCS=64

cd $MYDIR

echo "Started on `/bin/hostname`"
echo
echo "PATH is [$PATH]"
echo
echo "Nodes chosen are:"
cat $PBS_NODEFILE
echo

#$EXE
mpirun -np $MYPROCS $EXE

##echo "======================================================================="
##$MYMPI/bin/mpirun_rsh -rsh -np $MYPROCS -hostfile $PBS_NODEFILE $EXE
##echo "======================================================================="
