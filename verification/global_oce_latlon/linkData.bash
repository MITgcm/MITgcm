function Usage { 
  echo "Usage: $0 [ <testName> ]"
  echo "       where <testName> is for instance  shallow_openad"
  echo "       executed in build_<testName>"
}
if [ $# -eq 1 ]
then 
  testName=$1
else 
  testName=${PWD##*/build_}
fi
inputDir=../../../../MITgcm_contrib/heimbach/OpenAD/input_${testName}
if [ ! -d ${inputDir} ] 
then 
  echo "Error: input directory ${inputDir} doesn't exist"
  Usage $*
  exit -1
fi
ln -sf ${inputDir}/* .
tDir=$PWD
cd ../../global_ocean.90x40x15/input
./prepare_run > /dev/null
cd $tDir
ln -sf ../../global_ocean.90x40x15/input/*.bin .
