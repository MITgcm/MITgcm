#include "CPP_EEOPTIONS.h"

C-- File do_time_averages.F: routines to calculate time averages of fields
C--   Contents
C--   o do_time_averages   - Main routine to set up the averages
C--   o timeaver_ini_xyz   - Initialize array
C--   o timeaver_1fld_xyz  - Sum over single array
C--   o timeaver_2fld_xyz  - Sum over "double" arrays
C--   o timeaver_final_xyz - Get final averages
C--
C--   o 98/06/12 - elk

      SUBROUTINE DO_TIME_AVERAGES(myCurrentTime, myIter, myThid)
C     /==========================================================\
C     | SUBROUTINE DO_TIME_AVERAGES                              |
C     | o Time averaging routine for U, V, T, S, UT, VT in model |
C     |   main time-stepping                                     |
C     \==========================================================/

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "AVER.h"

      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
C     myIter - Iteration number
C     myCurrentTime - Current time of simulation ( s )
      INTEGER myThid
      INTEGER myIter
      REAL    myCurrentTime

C     == Local variables ==
C     suff - Hold suffix part of a filename
C     TimeAve - total time over average
      CHARACTER*(MAX_LEN_FNAM) suff
      REAL DDTT
 
C     Initialize fields for the first call ever
      IF ( myIter .EQ. nIter0 ) THEN
         CALL TIMEAVER_INI_XYZ(thetatave, myThid)
         CALL TIMEAVER_INI_XYZ(salttave, myThid)
         CALL TIMEAVER_INI_XYZ(uVeltave, myThid)
         CALL TIMEAVER_INI_XYZ(vVeltave, myThid)
         CALL TIMEAVER_INI_XYZ(uttave, myThid)
         CALL TIMEAVER_INI_XYZ(vttave, myThid)
         TimeAve(1,myThid)=0.
      ENDIF
      
      IF ( myIter .EQ. nIter0 .OR.
     & DIFFERENT_MULTIPLE
     &     (taveFreq,myCurrentTime,myCurrentTime-deltaTClock) )
     &   THEN
       DDTT=0.5*deltaTclock
      ELSE
       DDTT=deltaTclock
      ENDIF

C     Time Averages of single fields
      CALL TIMEAVER_1FLD_XYZ(theta, thetatave, DDTT, myThid)
      CALL TIMEAVER_1FLD_XYZ(salt,  salttave,  DDTT, myThid)
      CALL TIMEAVER_1FLD_XYZ(uVel,  uVeltave,  DDTT, myThid)
      CALL TIMEAVER_1FLD_XYZ(vVel,  vVeltave,  DDTT, myThid)

C     Time Averages of "double" fields
      CALL TIMEAVER_2FLD_XYZ(theta, uVel, uttave, 1, DDTT, myThid)
      CALL TIMEAVER_2FLD_XYZ(theta, vVel, vttave, 2, DDTT, myThid)

C     Keep record of how much time has been integrated over
      TimeAve(1,myThid)=TimeAve(1,myThid)+DDTT

C     Final Time Averages and Dump Files if needed
      IF ( myIter.NE.nIter0 
     &        .AND. 
     &     DIFFERENT_MULTIPLE
     &     (taveFreq,myCurrentTime,myCurrentTime-deltaTClock)
     &                                                        ) THEN

         WRITE(suff,'(I10.10)') myIter

C        Normalize by integrated time
         CALL TIMEAVER_FINAL_XYZ(thetatave,myThid)
         CALL TIMEAVER_FINAL_XYZ(salttave,myThid)
         CALL TIMEAVER_FINAL_XYZ(uVeltave,myThid)
         CALL TIMEAVER_FINAL_XYZ(vVeltave,myThid)
         CALL TIMEAVER_FINAL_XYZ(uttave,myThid)
         CALL TIMEAVER_FINAL_XYZ(vttave,myThid)

C        Write to files
         _BARRIER
         _BEGIN_MASTER( myThid )
         CALL WRITE_FLD_XYZ_RL('Ttave.',suff,thetatave,myIter,myThid)
         CALL WRITE_FLD_XYZ_RL('Stave.',suff,salttave,myIter,myThid)
         CALL WRITE_FLD_XYZ_RL('uVeltave.',suff,uVeltave,myIter,myThid)
         CALL WRITE_FLD_XYZ_RL('vVeltave.',suff,vVeltave,myIter,myThid)
         CALL WRITE_FLD_XYZ_RL('UTtave.',suff,uttave,myIter,myThid)
         CALL WRITE_FLD_XYZ_RL('VTtave.',suff,vttave,myIter,myThid)
         _END_MASTER( myThid )
         _BARRIER

C        Reset the averages to zero
         CALL TIMEAVER_INI_XYZ(thetatave, myThid)
         CALL TIMEAVER_INI_XYZ(salttave, myThid)
         CALL TIMEAVER_INI_XYZ(uVeltave, myThid)
         CALL TIMEAVER_INI_XYZ(vVeltave, myThid)
         CALL TIMEAVER_INI_XYZ(uttave, myThid)
         CALL TIMEAVER_INI_XYZ(vttave, myThid)
         TimeAve(1,myThid)=0.

C        Time Averages of single fields
         DDTT=0.5*deltaTclock
         CALL TIMEAVER_1FLD_XYZ(theta, thetatave, DDTT, myThid)
         CALL TIMEAVER_1FLD_XYZ(salt,  salttave,  DDTT, myThid)
         CALL TIMEAVER_1FLD_XYZ(uVel,  uVeltave,  DDTT, myThid)
         CALL TIMEAVER_1FLD_XYZ(vVel,  vVeltave,  DDTT, myThid)

C        Time Averages of "double" fields
         CALL TIMEAVER_2FLD_XYZ(theta, uVel, uttave, 1, DDTT, myThid)
         CALL TIMEAVER_2FLD_XYZ(theta, vVel, vttave, 2, DDTT, myThid)

C        Keep record of how much time has been integrated over
         TimeAve(1,myThid)=TimeAve(1,myThid)+DDTT

      ENDIF
 
      RETURN
      END

CStartofinterface
      SUBROUTINE TIMEAVER_INI_XYZ( fldtave, myThid )
C     /==========================================================\
C     | SUBROUTINE TIMEAVER_INI_XYZ                              |
C     | o Initialize 3D array                                    |
C     \==========================================================/

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "AVER.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
C     fldtave - time averaged Field
      INTEGER myThid
      _RL fldtave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nz,nSx,nSy)

CEndofinterface

C     == Local variables ==
C     i,j,k,bi,bj  - Loop counters
      INTEGER i, j, k, bi, bj
 
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k=1,Nz
         DO j=1,sNy
          DO i=1,sNx
             fldtave(i,j,k,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
 
      RETURN
      END

CStartofinterface
      SUBROUTINE TIMEAVER_1FLD_XYZ( fld, fldtave, deltaT, myThid )
C     /==========================================================\
C     | SUBROUTINE TIMEAVER_1FLD_XYZ                             |
C     | o Sum over time a single array                           |
C     \==========================================================/

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "AVER.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
C     fld  - Input Field
C     fldtave - time averaged Field
      _RL fld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nz,nSx,nSy)
      _RL fldtave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nz,nSx,nSy)
      REAL deltaT
      INTEGER myThid

CEndofinterface

C     == Local variables ==
C     i,j,k,bi,bj  - Loop counters
      INTEGER i, j, k, bi, bj
 
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k=1,Nz
         DO j=1,sNy
          DO i=1,sNx
             fldtave(i,j,k,bi,bj)=
     &       fldtave(i,j,k,bi,bj)+fld(i,j,k,bi,bj)*deltaT
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO
 
      RETURN
      END

CStartofinterface
      SUBROUTINE TIMEAVER_2FLD_XYZ( fld1, fld2, fldtave, dir, deltaT, myThid )
C     /==========================================================\
C     | SUBROUTINE TIMEAVER_2FLD_XYZ                             |
C     | o Sum over time a product of two arrays depending on the |
C     |   type of the second array (tracer_grid, u_grid, v_grid) |
C     |   The first array must be on tracer_grid                 |
C     \==========================================================/

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "AVER.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
C     dir - type of grid for 2nd array (0: tracer, 1: zonal, 2 merid)
C     fld  - Input Field
C     fldtave - time averaged Field
      _RL fld1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nz,nSx,nSy)
      _RL fld2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nz,nSx,nSy)
      _RL fldtave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nz,nSx,nSy)
      INTEGER dir
      REAL deltaT
      INTEGER myThid

CEndofinterface

C     == Local variables ==
C     i,j,k,bi,bj  - Loop counters
      INTEGER i, j, k, bi, bj
 
      IF ( dir.eq.0 ) THEN 

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k=1,Nz
         DO j=1,sNy
          DO i=1,sNx
             fldtave(i,j,k,bi,bj)= fldtave(i,j,k,bi,bj)
     &       +  fld1(i,j,k,bi,bj)*fld2(i,j,k,bi,bj)*deltaT
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO

      ELSEIF ( dir.eq.1 )  THEN
 
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k=1,Nz
         DO j=1,sNy
          DO i=1,sNx
             fldtave(i,j,k,bi,bj)= fldtave(i,j,k,bi,bj)
     &       + .5 * ( fld1(i-1,j,k,bi,bj) + fld1(i,j,k,bi,bj) )
     &                *   fld2(i,j,k,bi,bj)
     &                *   deltaT
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO

      ELSEIF ( dir.eq.2 ) THEN
 
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k=1,Nz
         DO j=1,sNy
          DO i=1,sNx
             fldtave(i,j,k,bi,bj)= fldtave(i,j,k,bi,bj)
     &       + .5 * ( fld1(i,j-1,k,bi,bj) + fld1(i,j,k,bi,bj) )
     &                *   fld2(i,j,k,bi,bj)
     &                *   deltaT
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO

      ENDIF

      RETURN
      END

CStartofinterface
      SUBROUTINE TIMEAVER_FINAL_XYZ( fldtave, myThid )
C     /==========================================================\
C     | SUBROUTINE TIMEAVER_FINAL_XY                             |
C     | o Get final average of field                             |
C     \==========================================================/

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "AVER.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
C     timeave - time averaged
C     fldtave - time averaged Field
      INTEGER myThid
      _RL fldtave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nz,nSx,nSy)

CEndofinterface

C     == Local variables ==
C     i,j,k,bi,bj  - Loop counters
      INTEGER i, j, k, bi, bj
 
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k=1,Nz
         DO j=1,sNy
          DO i=1,sNx
             fldtave(i,j,k,bi,bj) = fldtave(i,j,k,bi,bj) / timeave(1,myThid)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
 
      RETURN
      END
