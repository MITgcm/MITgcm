function Usage { 
  echo "Usage: $0 <testName>"
  echo "       where <testName> is for instance  shallow_openad"
  echo "       executed in build_<testName>"
}
testName=${PWD##*/build_}
inputDir=../../../../MITgcm_contrib/heimbach/OpenAD/input_${testName}
if [ ! -d ${inputDir} ] 
then 
  echo "Error: input directory ${inputDir} doesn't exist"
  Usage $*
  exit -1
fi
ln -sf ${inputDir}/* .
cd ../../global_ocean.90x40x15/input
./prepare_run > /dev/null
cd ../../OpenAD/build_${testName}
ln -sf ../../global_ocean.90x40x15/input/*.bin .
