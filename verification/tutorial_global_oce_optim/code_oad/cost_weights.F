#include "COST_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

      SUBROUTINE COST_WEIGHTS( myThid )

C     ==================================================================
C     SUBROUTINE COST_WEIGHTS
C     ==================================================================
C
C     o Set weights used in the cost function and in the
C       normalization of the sensitivities when ALLOW_NON_DIMENSIONAL

      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"

#include "ctrl.h"
#include "ctrl_weights.h"
#include "cost.h"

C     == routine arguments ==
      INTEGER  myThid

C     == Functions ==
      INTEGER  MDS_RECLEN
      EXTERNAL MDS_RECLEN

C     == local variables ==
      INTEGER bi,bj
      INTEGER i,j,k
      INTEGER itlo,ithi,jtlo,jthi
      INTEGER jMin,jMax,iMin,iMax
      INTEGER iUnit, length_of_rec

      _RL dummy
      _RL wti(Nr)
      REAL*8 tmpwti(Nr)
      CHARACTER*(MAX_LEN_MBUF) msgBuf

C     == end of interface ==

      jtlo = myByLo(myThid)
      jthi = myByHi(myThid)
      itlo = myBxLo(myThid)
      ithi = myBxHi(myThid)
      iMin = 1-OLx
      iMax = sNx+OLx
      jMin = 1-OLy
      jMax = sNy+OLy

C--   Initialize variance (weight) fields.
      DO k = 1,Nr
         wti(k) = 0. _d 0
      ENDDO
      DO bj = jtlo,jthi
        DO bi = itlo,ithi
          DO j = jMin,jMax
            DO i = iMin,iMax
              whfluxm(i,j,bi,bj)= 0. _d 0
            ENDDO
          ENDDO
          DO k = 1,Nr
             wunit(k,bi,bj)  = 1. _d 0
             wtheta(k,bi,bj) = 0. _d 0
             wsalt(k,bi,bj)  = 0. _d 0
          ENDDO
        ENDDO
      ENDDO

C--   Read error information and set up weight matrices.

#ifdef ALLOW_COST_TEMP
C  Temperature weights for cost function
       _BEGIN_MASTER(myThid)
       CALL MDSFINDUNIT( iUnit, myThid )
       length_of_rec = MDS_RECLEN( precFloat64, Nr, myThid )
       OPEN( iUnit, FILE='Err_levitus_15layer.bin', STATUS='OLD',
     &       FORM='UNFORMATTED',ACCESS='DIRECT',RECL=length_of_rec )
       READ(iUnit,rec=1) tmpwti
       CLOSE(iUnit)
#ifdef _BYTESWAPIO
       CALL MDS_BYTESWAPR8( Nr, tmpwti )
#endif
       _END_MASTER(myThid)
       _BARRIER

       DO k=1,Nr
         wti(k) = tmpwti(k)
       ENDDO
       WRITE(msgBuf,'(3A)') 'S/R COST_WEIGHTS:',
     &  ' Temperature weights loaded from: ','Err_levitus_15layer.bin'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT , myThid )

c     print*,'Weights for temperature: wti', (wti(k),k=1,nr)

      DO bj = jtlo,jthi
        DO bi = itlo,ithi
          DO k = 1, Nr
               wtheta(k,bi,bj) = 1. _d 0/wti(k)/wti(k)
          ENDDO
        ENDDO
      ENDDO
#endif /* ALLOW_COST_TEMP */

C--   Then the hflux weights :

#ifdef ALLOW_COST_HFLUXM
      CALL READ_REC_3D_RL( 'Err_hflux.bin', precFloat64, 1,
     &                      whfluxm, 1, 0, myThid )
      _EXCH_XY_RL(whfluxm   , myThid )
      DO bj = jtlo,jthi
        DO bi = itlo,ithi
          DO j = jMin,jMax
            DO i = iMin,iMax
c            print*,'Uncertainties for Heat Flux',i,j,whfluxm(i,j,bi,bj)
             IF (whfluxm(i,j,bi,bj) .NE. 0. _d 0) THEN
                 whfluxm(i,j,bi,bj) = 1. _d 0 /whfluxm(i,j,bi,bj)
     &                                        /whfluxm(i,j,bi,bj)
             ELSE
                 whfluxm(i,j,bi,bj) = 1. _d 0
             ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDDO
#ifdef ALLOW_NONDIMENSIONAL_CONTROL_IO
      CALL ACTIVE_WRITE_XY('whfluxm',whfluxm,1,0,myThid,dummy)
#endif
#endif /* ALLOW_COST_HFLUXM */
      RETURN
      END
