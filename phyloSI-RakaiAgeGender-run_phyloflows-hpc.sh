#!/bin/sh

STAN_MODEL="gp_221201d"
JOBNAME="firstrun"
INDIR="/rds/general/user/phl19/home/git/phyloSI-RakaiAgeGender"
OUTDIR="/rds/general/user/phl19/home/results/phyloSI-RakaiAgeGender"

mkdir $OUTDIR

cat > $OUTDIR/bash_$STAN_MODEL-$JOBNAME.pbs <<EOF
  
#!/bin/sh
#PBS -l walltime=48:00:00
#PBS -l select=1:ncpus=10:ompthreads=1:mem=240gb
#PBS -j oe
module load anaconda3/personal
source activate phyloSI-RakaiAgeGender # new line added
  
JOB_TEMP=\${EPHEMERAL}/\${PBS_JOBID}
mkdir -p \$JOB_TEMP
cd \$JOB_TEMP  
PWD=\$(pwd)

INDIR=$INDIR
OUTDIR=$OUTDIR
STAN_MODEL=$STAN_MODEL
JOBNAME=$JOBNAME
  
# main directory
CWD=\$PWD/\$STAN_MODEL-\$JOBNAME

mkdir \$CWD
mkdir \$CWD/figures
  
Rscript \$INDIR/src/transmission_flows/run_stan.R -indir \$INDIR -outdir \$CWD -stan_model \$STAN_MODEL -jobname \$JOBNAME
  
cp -R --no-preserve=mode,ownership \$PWD/* \$OUTDIR
EOF


cd $OUTDIR
qsub bash_$STAN_MODEL-$JOBNAME.pbs
