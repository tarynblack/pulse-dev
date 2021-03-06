#PBS -N atlas_sub
#PBS -l nodes=1:ppn=64
#PBS -l walltime=110:00:00
#PBS -q atlas-6
#PBS -k oe
#PBS -j oe
#PBS -m abe
##PBS -l mem=240gb

##ulimit -s unlimited
RSH=/usr/bin/rsh
##MYMPI=/usr/local/packages/mvapich2/1.9/intel-13.2.146
MYMPI=/usr/local/mvapich2/1.9/intel-13.2.146
MYDIR=$HOME/data2/F_10P_9955_999
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

##$EXE
##mpirun -np $MYPROCS $EXE
mpirun -rmk pbs $EXE

##echo "======================================================================="
##$MYMPI/bin/mpirun_rsh -rsh -np $MYPROCS -hostfile $PBS_NODEFILE $EXE
##echo "======================================================================="
