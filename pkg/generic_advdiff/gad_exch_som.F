C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_exch_som.F,v 1.4 2008/05/09 21:43:16 jmc Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

CBOP
C     !ROUTINE: GAD_EXCH_SOM
C     !INTERFACE:
      SUBROUTINE GAD_EXCH_SOM(
     U                         smTr,
     I                         myNz, myThid )


C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE GAD_EXCH_SOM
C     | o Apply exchanges to update overlaps of 1srt & 2nd.Order
C     |   Moments array, corresponding to 1 tracer
C     *==========================================================*
C     \ev
C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GAD.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     smTr   :: tracer 1rst & 2nd Order moments
C     myNz   ::  3rd dimension of array to exchange
C     myThid ::  my Thread Id number
      INTEGER myNz
      _RL     smTr(1-OLx:sNx+OLx,1-OLy:sNy+OLy,myNz,nSx,nSy,nSOM)
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     === Local variables ===

C--   Apply exchanges to 1rst.O.Moments:
C-    Sx,Sy :
      CALL EXCH_UV_AGRID_3D_RL(
     U                 smTr(1-OLx,1-OLy,1,1,1,1),
     U                 smTr(1-OLx,1-OLy,1,1,1,2),
     I                 .TRUE., myNz, myThid )
C-    Sz :
      CALL EXCH_3D_RL(
     U                 smTr(1-OLx,1-OLy,1,1,1,3),
     I                 myNz, myThid )

C--   Apply exchanges to 2nd.O.Moments:
C-    Sxx,Syy :
      CALL EXCH_UV_AGRID_3D_RL(
     U                 smTr(1-OLx,1-OLy,1,1,1,4),
     U                 smTr(1-OLx,1-OLy,1,1,1,5),
     I                 .FALSE., myNz, myThid )
C-    Szz :
      CALL EXCH_3D_RL(
     U                 smTr(1-OLx,1-OLy,1,1,1,6),
     I                 myNz, myThid )
C-    Sxy :
      CALL EXCH_SM_3D_RL(
     U                 smTr(1-OLx,1-OLy,1,1,1,7),
     I                 .TRUE., myNz, myThid )
C-    Sxz,Syz :
      CALL EXCH_UV_AGRID_3D_RL(
     U                 smTr(1-OLx,1-OLy,1,1,1,8),
     U                 smTr(1-OLx,1-OLy,1,1,1,9),
     I                 .TRUE., myNz, myThid )

      RETURN
      END
