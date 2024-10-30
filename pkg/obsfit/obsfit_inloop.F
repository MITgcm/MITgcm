#include "OBSFIT_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: OBSFIT_INLOOP

C     !INTERFACE: 
      SUBROUTINE OBSFIT_INLOOP( myTime, myThid )

C     !DESCRIPTION:
C     ==================================================================
C     | Computes and writes model counterparts for ObsFit data  
C     ==================================================================

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "PARAMS.h"
#ifdef ALLOW_CAL
# include "cal.h"
#endif
#ifdef ALLOW_CTRL
# include "OPTIMCYCLE.h"
#endif
#ifdef ALLOW_OBSFIT
# include "OBSFIT_SIZE.h"
# include "OBSFIT.h"
# include "netcdf.inc"
#endif
#ifdef ALLOW_AUTODIFF
# include "tamc.h"
#endif

C     !INPUT PARAMETERS: 
C     myTime    :: Current time in simulation
C     myThid    :: my Thread Id number
      _RL     myTime
      INTEGER myThid
CEOP

#ifdef ALLOW_OBSFIT
C     !LOCAL VARIABLES:
      INTEGER k,bi,bj,sample_num,num_file
      INTEGER sample_i(NUM_INTERP_PTS_OBS)
      INTEGER sample_j(NUM_INTERP_PTS_OBS)
      INTEGER sample_k(NUM_INTERP_PTS_OBS)
      _RL     sample_frac(NUM_INTERP_PTS_OBS)
      _RL     sample_modval, saved_modval
#ifndef ALLOW_CTRL
      INTEGER optimcycle
#endif
#ifndef ALLOW_ECCO
      INTEGER i,j
#endif
#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER act1, act2, act3, act4
      INTEGER max1, max2, max3
      INTEGER ikey, itdkey
#endif
      CHARACTER*(MAX_LEN_MBUF) msgBuf

#ifndef ALLOW_CTRL
      optimcycle = 0
#endif
#ifdef ALLOW_AUTODIFF_TAMC
      itdkey = bi + (bj - 1)*nSx + (ikey_dynamics - 1)*nSx*nSy
#endif

      _BEGIN_MASTER( myThid )

      DO bj=1,nSy
       DO bi=1,nSx

        DO num_file=1,NFILESMAX_OBS

         DO sample_num=1,NSAMPLESMAX
          IF (sample_num.LE.sampleNo(num_file,bi,bj)) THEN

           IF ((sample_timeE(num_file,sample_num,bi,bj).GE.myTime)
     &      .AND.(sample_timeS(num_file,sample_num,bi,bj).LT. 
     &      (myTime+deltaTclock))) THEN

            DO k=1,NUM_INTERP_PTS_OBS
             sample_i(k)=sample_interp_i(num_file,sample_num,k,bi,bj)
             sample_j(k)=sample_interp_j(num_file,sample_num,k,bi,bj)
             sample_k(k)=sample_interp_k(num_file,sample_num,k,bi,bj)
             sample_frac(k)=sample_interp_frac
     &                                  (num_file,sample_num,k,bi,bj)
            ENDDO

            sample_modval=0
            saved_modval=0

C If averaging or integrating in time, read previously 
C recorded value and add to it
            IF (obsfitOperation(num_file).GT.0) THEN
             CALL active_read_obs_tile(num_file,
     &          saved_modval,
     &          sample_num,.false.,optimcycle,bi,bj,myThid,
     &          obsfit_dummy(num_file,bi,bj))
            ENDIF

            CALL obsfit_interp(sample_modval,
     &         sample_i,sample_j,sample_k,sample_frac,
     &         sample_type(num_file,sample_num,bi,bj),
     &         num_file,myTime,bi,bj,myThid)

            sample_modval=sample_modval*
     &                   obsfit_facmod(num_file)

            IF (obsfitOperation(num_file).GT.0) THEN
               sample_modval = saved_modval+sample_modval*deltaTclock
            ENDIF

            sample_modmask(bi,bj)=sample_modmask(bi,bj)*
     &                   sample_weight(num_file,sample_num,bi,bj)

            CALL active_write_obs_tile(num_file,
     &         sample_modval,
     &         sample_num,optimcycle,bi,bj,myThid,
     &         obsfit_dummy(num_file,bi,bj))

           ENDIF !if ((sample_time...
          ENDIF !if (sampleNo(num_file,bi,bj).NE.0) then
         ENDDO !do sample_num...
        ENDDO !do num_file=1,NFILESMAX_OBS
      
       ENDDO
      ENDDO

      _END_MASTER( myThid )

#endif

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|