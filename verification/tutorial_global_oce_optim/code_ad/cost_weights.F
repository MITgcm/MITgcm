C $Header: /u/gcmpack/MITgcm/verification/tutorial_global_oce_optim/code_ad/cost_weights.F,v 1.2 2008/06/23 18:38:34 dfer Exp $
C $Name:  $

#include "COST_CPPOPTIONS.h"

      SUBROUTINE COST_WEIGHTS( myThid )

c     ==================================================================
c     SUBROUTINE COST_WEIGHTS
c     ==================================================================
c
c     o Set weights used in the cost function and in the
c       normalization of the sensitivities when ALLOW_NON_DIMENSIONAL

      IMPLICIT NONE

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"

#include "ctrl.h"
#include "ctrl_weights.h"
#include "cost.h"

c     == routine arguments ==

      INTEGER  myThid

c     == local variables ==

      INTEGER bi,bj
      INTEGER i,j,k
      INTEGER itlo,ithi,jtlo,jthi
      INTEGER jMin,jMax,iMin,iMax
      INTEGER iUnit

      _RL dummy
      _RL wti(Nr)
      REAL*8 tmpwti(Nr)
      CHARACTER*(MAX_LEN_MBUF) msgBuf

c     == end of interface ==

      jtlo = myByLo(myThid)
      jthi = myByHi(myThid)
      itlo = myBxLo(myThid)
      ithi = myBxHi(myThid)
      iMin = 1-OLx
      iMax = sNx+OLx
      jMin = 1-OLy
      jMax = sNy+OLy

c--   Initialize variance (weight) fields.
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

c--   Read error information and set up weight matrices.

#ifdef ALLOW_COST_TEMP
C  Temperature weights for cost function   
       _BEGIN_MASTER(myThid)
       CALL MDSFINDUNIT( iUnit, myThid )
       OPEN(iUnit, FILE='Err_levitus_15layer.bin', STATUS='OLD',
     &      FORM='UNFORMATTED',ACCESS='DIRECT',RECL=2*WORDLENGTH*Nr)
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

      print*,'Weights for temperature: wti', (wti(k),k=1,nr)

      DO bj = jtlo,jthi
        DO bi = itlo,ithi
          DO k = 1, Nr
               wtheta(k,bi,bj) = 1. _d 0/wti(k)/wti(k)
          ENDDO
        ENDDO
      ENDDO
#endif
c
c--   Then the hflux weights :
c
#if (defined (ALLOW_COST_HFLUXM) || defined (ALLOW_HFLUXM_CONTROL))
      _BEGIN_MASTER(myThid)
      CALL MDSREADFIELD('Err_hflux.bin',precFloat64,
     &                  'RL',1,whfluxm,1,myThid)
      _END_MASTER(myThid)
      _EXCH_XY_RL(whfluxm   , myThid )
      DO bj = jtlo,jthi
        DO bi = itlo,ithi
          DO j = jMin,jMax
            DO i = iMin,iMax
             print*,'Uncertainties for Heat Flux',i,j,whfluxm(i,j,bi,bj)
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
      call active_write_xy('whfluxm',whfluxm,1,0,myThid,dummy)
#endif
#endif
c
      end
