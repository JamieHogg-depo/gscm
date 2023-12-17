#!/bin/bash

base_folder='gscm' # do not change this

which_m='GSCM'
all_aus=1

# Model
for grid_ix in {1..18}
do

	# get the current date
	cr_date=$(date +%Y%m%d)
	cur_date=$cr_date'2'

	# create directories
	mkdir -p $base_folder/sub_src/$cur_date
	mkdir -p $base_folder/outputs/$cur_date/lyra_errors
	mkdir -p $base_folder/outputs/$cur_date/lyra_out
	mkdir -p $base_folder/outputs/$cur_date/r
	
	# create the unique .sub script files
	specs=$which_m'_ix'$grid_ix
	file=$specs'.sub'

	# paste the commands in the .sub scripts
	cat > $base_folder/sub_src/$cur_date/$file <<EOF
#!/bin/bash -l
#PBS -N $specs
#PBS -l ncpus=4
#PBS -l mem=40GB
#PBS -l walltime=30:00:00
#PBS -e $base_folder/outputs/$cur_date/lyra_errors/$specs
#PBS -o $base_folder/outputs/$cur_date/lyra_out/$specs

module load r/4.0.3-foss-2020b
module load gdal/3.2.1-foss-2020b

R -e ".libPaths('r_lib');
base_folder='$base_folder';
grid_ix=$grid_ix;
cur_date=$cur_date;
which_m='$which_m';
all_aus=($all_aus == 1);
source(paste0(base_folder, '/ms_gscm.R'));"
EOF

	# run each script
	qsub $base_folder/sub_src/$cur_date/$file

done
