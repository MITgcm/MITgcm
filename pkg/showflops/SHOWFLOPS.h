
#if defined(TIME_PER_TIMESTEP) || defined(USE_PAPI_FLOPS) || defined(USE_PCL_FLOPS)
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#ifdef TIME_PER_TIMESTEP
CCE107 common block for per timestep timing
C     !TIMING VARIABLES
C     == Timing variables ==
      REAL*8 utnew, utold, stnew, stold, wtnew, wtold
      DATA utnew, utold, stnew, stold, wtnew, wtold /6*0.0D0/
#endif
#ifdef USE_PAPI_FLOPS
CCE107 common block for PAPI summary performance
#include <fpapi.h>
      INTEGER*8 flpops, instr
      DATA flpops, instr /2*0/
      INTEGER check
      REAL*4 real_time, proc_time, mflops, ipc
      DATA real_time, proc_time, mflops, ipc /4*0.0E0/
#else
#ifdef USE_PCL_FLOPS
CCE107 common block for PCL summary performance
#include <pclh.f>
      INTEGER pcl_counter_list(5), flags, nevents, res, ipcl
      INTEGER*8 i_result(5), descr
      REAL*8 fp_result(5)
      COMMON /pclvars/ i_result, descr, fp_result, pcl_counter_list,
     $     flags, nevents 
      INTEGER nmaxevents
      PARAMETER (nmaxevents = 61)
      CHARACTER*22 pcl_counter_name(0:nmaxevents-1)
      COMMON /pclnames/ pcl_counter_name
#endif
#endif
#endif
