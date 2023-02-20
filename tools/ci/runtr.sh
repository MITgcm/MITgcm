#!/bin/bash
#
# Test report running script for Travis CI 
# - called from .travis.yml script: section with environment variables 
#   possibly set.
#   If they are not set then defaults are configured here.
# MITGCM_DECMD - command to run under Docker
# MITGCM_EXP   - MITgcm test to run
# MITGCM_TROPT - Test report options
#

if [ -z "${MITGCM_TROPT}" ]; then
 export MITGCM_TROPT='-devel -of=../tools/build_options/linux_amd64_gfortran -match 14 -pass'
fi
if [ -z "${MITGCM_DECMD}" ]; then
 export MITGCM_DECMD='docker exec -i ubuntu_18_04-testreport bash -c'
fi
if [ -z "${MITGCM_INPUT_DIR_PAT}" ]; then
 export MITGCM_INPUT_DIR_PAT='/input.*'
fi

${MITGCM_DECMD} "cd /MITgcm/verification; ./testreport -t ${MITGCM_EXP} ${MITGCM_TROPT}"
