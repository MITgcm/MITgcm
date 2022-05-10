#include "CTRL_OPTIONS.h"

CBOP
C     !ROUTINE: CTRL_MAP_FORCING
C     !INTERFACE:
      SUBROUTINE CTRL_MAP_FORCING( myTime, myIter, myThid )

C     !DESCRIPTION: \bv
c     *=================================================================
c     | SUBROUTINE CTRL_MAP_FORCING
c     | Add the surface flux anomalies of the control vector
c     | to the model flux fields and update the tile halos.
c     | The control vector is defined in the header file "ctrl.h".
c     *=================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "CTRL_SIZE.h"
#include "ctrl.h"
#include "CTRL_GENARR.h"
#include "ctrl_dummy.h"
#include "optim.h"
#ifdef ALLOW_STREAMICE
# include "STREAMICE.h"
#endif
#ifdef ALLOW_AUTODIFF
#include "AUTODIFF_MYFIELDS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myTime :: time counter for this thread
C     myIter :: iteration counter for this thread
C     myThid :: thread number for this instance of the routine.
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C     !FUNCTIONS:

C     !LOCAL VARIABLES:
C     == Local variables ==
#ifdef ALLOW_GENTIM2D_CONTROL
      INTEGER bi,bj
      INTEGER i,j
#endif
#ifndef ALLOW_OPENAD
#ifdef ALLOW_GENTIM2D_CONTROL
      INTEGER iarr
      _RL     tmpUE(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL     tmpVN(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL     tmpUX(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL     tmpVY(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif
#endif

c     == end of interface ==
CEOP

#ifndef ALLOW_OPENAD
#ifdef ALLOW_GENTIM2D_CONTROL

      DO bj = myByLo(myThid),myByHi(myThid)
       DO bi = myBxLo(myThid),myBxHi(myThid)
        DO j = 1-OLy,sNy+OLy
         DO i = 1-OLx,sNx+OLx
          tmpUE(i,j,bi,bj) = 0. _d 0
          tmpVN(i,j,bi,bj) = 0. _d 0
          tmpUX(i,j,bi,bj) = 0. _d 0
          tmpVY(i,j,bi,bj) = 0. _d 0
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      DO bj = myByLo(myThid),myByHi(myThid)
       DO bi = myBxLo(myThid),myBxHi(myThid)
        DO j = 1,sNy
         DO i = 1,sNx
          DO iarr = 1, maxCtrlTim2D
           IF (xx_gentim2d_file(iarr)(1:5).EQ.'xx_fe') tmpUE
     &       (i,j,bi,bj)=tmpUE(i,j,bi,bj)+xx_gentim2d(i,j,bi,bj,iarr)
           IF (xx_gentim2d_file(iarr)(1:5).EQ.'xx_fn') tmpVN
     &        (i,j,bi,bj)=tmpVN(i,j,bi,bj)+xx_gentim2d(i,j,bi,bj,iarr)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      _EXCH_XY_RL(tmpUE,myThid)
      _EXCH_XY_RL(tmpVN,myThid)
      CALL rotate_uv2en_rl(tmpUX,tmpVY,tmpUE,tmpVN,
     &     .FALSE.,.TRUE.,.TRUE.,1,myThid)

      DO bj = myByLo(myThid),myByHi(myThid)
       DO bi = myBxLo(myThid),myBxHi(myThid)
        DO j = 1,sNy
         DO i = 1,sNx
          fu(i,j,bi,bj)=fu(i,j,bi,bj)+tmpUX(i,j,bi,bj)
          fv(i,j,bi,bj)=fv(i,j,bi,bj)+tmpVY(i,j,bi,bj)
          DO iarr = 1, maxCtrlTim2D
           IF (xx_gentim2d_file(iarr)(1:7).EQ.'xx_qnet') Qnet
     &       (i,j,bi,bj)=Qnet(i,j,bi,bj)+xx_gentim2d(i,j,bi,bj,iarr)
           IF (xx_gentim2d_file(iarr)(1:8).EQ.'xx_empmr') EmPmR
     &       (i,j,bi,bj)=EmPmR(i,j,bi,bj)+xx_gentim2d(i,j,bi,bj,iarr)
           IF (xx_gentim2d_file(iarr)(1:6).EQ.'xx_qsw') Qsw
     &       (i,j,bi,bj)=Qsw(i,j,bi,bj)+xx_gentim2d(i,j,bi,bj,iarr)
           IF (xx_gentim2d_file(iarr)(1:6).EQ.'xx_sst') SST
     &       (i,j,bi,bj)=SST(i,j,bi,bj)+xx_gentim2d(i,j,bi,bj,iarr)
           IF (xx_gentim2d_file(iarr)(1:6).EQ.'xx_sss') SSS
     &       (i,j,bi,bj)=SSS(i,j,bi,bj)+xx_gentim2d(i,j,bi,bj,iarr)
           IF (xx_gentim2d_file(iarr)(1:8).EQ.'xx_pload') pLoad
     &       (i,j,bi,bj)=pLoad(i,j,bi,bj)+xx_gentim2d(i,j,bi,bj,iarr)
           IF (xx_gentim2d_file(iarr)(1:11).EQ.'xx_saltflux') saltFlux
     &       (i,j,bi,bj)=saltFlux(i,j,bi,bj)+xx_gentim2d(i,j,bi,bj,iarr)
           IF (xx_gentim2d_file(iarr)(1:5).EQ.'xx_fu') fu
     &       (i,j,bi,bj)=fu(i,j,bi,bj)+xx_gentim2d(i,j,bi,bj,iarr)
           IF (xx_gentim2d_file(iarr)(1:5).EQ.'xx_fv') fv
     &       (i,j,bi,bj)=fv(i,j,bi,bj)+xx_gentim2d(i,j,bi,bj,iarr)
          ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO

       CALL EXCH_XY_RS( Qnet , myThid )
       CALL EXCH_XY_RS( EmPmR , myThid )
       CALL EXCH_XY_RS( Qsw , myThid )
       CALL EXCH_XY_RS( SST , myThid )
       CALL EXCH_XY_RS( SSS , myThid )
       CALL EXCH_XY_RS( pLoad , myThid )
       CALL EXCH_XY_RS( saltFlux , myThid )
       CALL EXCH_UV_XY_RS( fu, fv, .TRUE., myThid )

#ifdef ALLOW_STREAMICE
       IF ( useStreamice ) THEN
        DO iarr = 1, maxCtrlTim2D
         IF (xx_gentim2d_file(iarr)(1:17).EQ.'xx_bdot_streamice') THEN
          DO bj = myByLo(myThid), myByHi(myThid)
           DO bi = myBxLo(myThid), myBxHi(myThid)
            DO j = 1-OLy,sNy+OLy
             DO i = 1-OLx,sNx+OLx
              bdot_streamice(i,j,bi,bj) = xx_gentim2d(i,j,bi,bj,iarr)
             ENDDO
            ENDDO
           ENDDO
          ENDDO
          CALL EXCH_XY_RL( bdot_streamice, myThid )
         ENDIF
        ENDDO
       ENDIF
#endif

#endif /* ALLOW_GENTIM2D_CONTROL */
#endif /* ndef ALLOW_OPENAD */

      RETURN
      END
