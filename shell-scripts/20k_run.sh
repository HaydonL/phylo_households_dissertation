#!/bin/sh

INDIR="/rds/general/user/phl19/home/git/phylo_households_dissertation"
OUTDIR="/rds/general/user/phl19/home/results/phylo_households_dissertation"

mkdir $OUTDIR

cat > $OUTDIR/bash_20k_run.pbs <<EOF
  
#!/bin/sh
#PBS -l walltime=08:00:00
#PBS -l select=1:ncpus=10:ompthreads=1:mem=240gb
#PBS -j oe
module load anaconda3/personal
source activate Renv 

INDIR=$INDIR
Rscript \$INDIR/src/run_20k.R -indir \$INDIR

EOF

cd $OUTDIR
qsub bash_20k_run.pbs