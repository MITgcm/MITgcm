C $Header: /u/gcmpack/MITgcm/pkg/bling/Attic/tracer_min_val.F,v 1.1 2016/05/19 20:29:27 mmazloff Exp $
C $Name:  $

#include "BLING_OPTIONS.h"

CBOP
      subroutine TRACER_MIN_VAL(
     I                          PTR, min_val )

C     =================================================================
C     | subroutine tracer_min_val
C     | 
C     =================================================================

      implicit none
      
C     === Global variables ===
#include "SIZE.h"

C     === Routine arguments ===
      _RL     PTR(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     min_val
CEOP

C     == Local variables ==
      INTEGER i,j,k

       DO k=1,Nr
         DO i=1,sNx
          DO j=1,sNy
           PTR(i,j,k) = max( min_val, PTR(i,j,k) )
          ENDDO
         ENDDO
       ENDDO

      RETURN
      END
