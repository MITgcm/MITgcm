#!/bin/bash
#
# Testreport running script for CI,
# - now as GitHub action, called from build_testing.ym
#   (formerly with Travis, called from .travis.yml script )
# - section with environment variables:
#   if they are not set then defaults are configured here.
# MITGCM_DECMD - command to run under Docker
# MITGCM_EXP   - MITgcm test to run
# MITGCM_TROPT - Test report options
# MITGCM_ROOT  - MITgcm root dir as seen by MITGCM_DECMD (default: /MITgcm)
#

if [ -z "${MITGCM_TROPT}" ]; then
 export MITGCM_TROPT='-devel -of=../tools/build_options/linux_amd64_gfortran -match 14'
fi
if [ -z "${MITGCM_DECMD}" ]; then
 export MITGCM_DECMD='docker exec -i ubuntu_18_04-testreport bash -c'
fi
if [ -z "${MITGCM_ROOT}" ]; then
 export MITGCM_ROOT='/MITgcm'
fi
if [ -z "${MITGCM_INPUT_DIR_PAT}" ]; then
 export MITGCM_INPUT_DIR_PAT='/input.*'
fi

${MITGCM_DECMD} "cd ${MITGCM_ROOT}/verification; ./testreport -pass -t ${MITGCM_EXP} ${MITGCM_TROPT}"
