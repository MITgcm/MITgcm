C $Header: /u/gcmpack/MITgcm/pkg/ebm/ebm_area_t.F,v 1.3 2004/05/21 21:45:35 heimbach Exp $
C $Name:  $

#include "EBM_OPTIONS.h"

      SUBROUTINE EBM_AREA_T( myTime, myIter, myThid )
C     |==========================================================|
C     | S/R CALCULATE ZONAL MEAN TEMPERATURE                     |
C     |==========================================================|

      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "DYNVARS.h"
#include "GRID.h"
#ifdef ALLOW_EBM
# include "EBM.h"
#endif
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
# include "tamc_keys.h"
#endif

C     === Routine arguments ===
C     myThid - Instance number for this innvocation 
      INTEGER myThid
      INTEGER myIter
      _RL myTime

CEndOfInterface
C     == Local variables ==
C     I, J, K - Loop counters

#ifdef ALLOW_EBM

      INTEGER iMin, iMax, jMin, jMax
      INTEGER i, j, k, bi, bj
      _RL TmlS_tile(nSx, nSy)
      _RL TmlN_tile(nSx, nSy)
      _RL TtS_tile(nSx, nSy)
      _RL TtN_tile(nSx, nSy)
      _RL CountmlS_tile(nSx, nSy)
      _RL CountmlN_tile(nSx, nSy)
      _RL CounttS_tile(nSx, nSy)
      _RL CounttN_tile(nSx, nSy)
      _RL CountmlS
      _RL CountmlN
      _RL CounttS
      _RL CounttN

C--   Top layer only
      k = 1

      TmlS = 0.0
      CountmlS = 0.0
      TmlN = 0.0
      CountmlN = 0.0
      TtS = 0.0
      CounttS = 0.0
      TtN = 0.0
      CounttN = 0.0

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

          TmlS_tile(bi,bj) = 0.
          TmlN_tile(bi,bj) = 0.
          TtS_tile(bi,bj) = 0.
          TtN_tile(bi,bj) = 0.
          CountmlS_tile(bi,bj) = 0.
          CountmlN_tile(bi,bj) = 0.
          CounttS_tile(bi,bj) = 0.
          CounttN_tile(bi,bj) = 0.
          
C--   Calculate sum of temperatures over each tile
        DO j = 1,sNy
         DO i = 1, sNx
          IF ( yC(i,j,bi,bj) .GE. -lat(3) .AND. 
     &         yC(i,j,bi,bj) .LE. -lat(2)) THEN
            TmlS_tile(bi,bj) = TmlS_tile(bi,bj) + 
     &           theta(i,j,k,bi,bj)
            CountmlS_tile(bi,bj) = CountmlS_tile(bi,bj) + 
     &           maskC(i,j,k,bi,bj)
          ELSEIF ( yC(i,j,bi,bj) .GT. -lat(2) .AND. 
     &       yC(i,j,bi,bj) .LT. -lat(1)) THEN 
            TtS_tile(bi,bj) = TtS_tile(bi,bj) + 
     &           theta(i,j,k,bi,bj)
            CounttS_tile(bi,bj) = CounttS_tile(bi,bj) + 
     &           maskC(i,j,k,bi,bj)
          ELSEIF ( yC(i,j,bi,bj) .GT. lat(1) .AND. 
     &       yC(i,j,bi,bj) .LT. lat(2)) THEN
            TtN_tile(bi,bj) = TtN_tile(bi,bj) + 
     &           theta(i,j,k,bi,bj)
            CounttN_tile(bi,bj) = CounttN_tile(bi,bj) + 
     &           maskC(i,j,k,bi,bj)
          ELSEIF ( yC(i,j,bi,bj) .GE. lat(2) .AND. 
     &       yC(i,j,bi,bj) .LT. lat(3)) THEN
             TmlN_tile(bi,bj) = TmlN_tile(bi,bj) + 
     &           theta(i,j,k,bi,bj)
            CountmlN_tile(bi,bj) = CountmlN_tile(bi,bj) + 
     &           maskC(i,j,k,bi,bj)
         ENDIF
            
         ENDDO
        ENDDO

C--   Sum over the tiles
        TmlS = TmlS + TmlS_tile(bi,bj)
        CountmlS = CountmlS + CountmlS_tile(bi,bj)
        TmlN = TmlN + TmlN_tile(bi,bj)
        CountmlN = CountmlN + CountmlN_tile(bi,bj)
        TtS = TtS + TtS_tile(bi,bj)
        CounttS = CounttS + CounttS_tile(bi,bj)
        TtN = TtN + TtN_tile(bi,bj)
        CounttN = CounttN + CounttN_tile(bi,bj)
        
       ENDDO
      ENDDO

      _GLOBAL_SUM_R8( TmlS, myThid )
      _GLOBAL_SUM_R8( TmlN, myThid )
      _GLOBAL_SUM_R8( TtS, myThid )
      _GLOBAL_SUM_R8( TtN, myThid )
c
      _GLOBAL_SUM_R8( CountmlS, myThid )
      _GLOBAL_SUM_R8( CountmlN, myThid )
      _GLOBAL_SUM_R8( CounttS, myThid )
      _GLOBAL_SUM_R8( CounttN, myThid )

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE CountmlS = comlev1, key = ikey_dynamics
CADJ STORE CountmlN = comlev1, key = ikey_dynamics
CADJ STORE CounttS  = comlev1, key = ikey_dynamics
CADJ STORE CounttN  = comlev1, key = ikey_dynamics
#endif
      if ( CountmlS .NE. 0.) TmlS = TmlS/CountmlS + t_mlt
      if ( CountmlN .NE. 0.) TmlN = TmlN/CountmlN + t_mlt
      if ( CounttS .NE. 0.) TtS = TtS/CounttS + t_mlt
      if ( CounttN .NE. 0.) TtN = TtN/CounttN + t_mlt

#endif /* ALLOW_EBM */

      RETURN
      END



