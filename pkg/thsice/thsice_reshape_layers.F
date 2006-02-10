C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_reshape_layers.F,v 1.3 2006/02/10 00:30:32 jmc Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"

CBOP
C     !ROUTINE: THSICE_RESHAPE_LAYERS
C     !INTERFACE:
      SUBROUTINE THSICE_RESHAPE_LAYERS(
     U                                 qicen,
     I                                 hlyr, hnew, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R THSICE_RESHAPE_LAYERS
C     | Repartition into equal-thickness layers, conserving energy.
C     *==========================================================*
C     | This is the 2-layer version (formerly "NEW_LAYERS_WINTON") 
C     |  from M. Winton 1999, JAOT, sea-ice model.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "EEPARAMS.h"
#include "THSICE_PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     qicen  :: ice enthalpy (J/kg)
C     hnew   :: new ice layer thickness (m)
C     hlyr   :: individual ice layer thickness (m)
C     myThid :: Number of this instance
      _RL  qicen(*)
      _RL  hnew(*)
      _RL  hlyr
      INTEGER myThid
CEOP

#ifdef ALLOW_THSICE
C     == Local Variables ==
C      f1         :: Fraction of upper layer ice in new layer
C      qh1, qh2   :: qice*h for layers 1 and 2
C      qhtot      :: qh1 + qh2
C      q2tmp      :: Temporary value of qice for layer 2
      _RL  f1
      _RL  qh1, qh2
      _RL  qhtot
      _RL  q2tmp

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      if (hnew(1).gt.hnew(2)) then
C-    Layer 1 gives ice to layer 2
         f1 = (hnew(1)-hlyr)/hlyr
         q2tmp = f1*qicen(1) + (1. _d 0-f1)*qicen(2)
         if (q2tmp.gt.Lfresh) then
            qicen(2) = q2tmp
         else
C-    Keep q2 fixed to avoid q2<Lfresh and T2>0
            qh2 = hlyr*qicen(2)
            qhtot = hnew(1)*qicen(1) + hnew(2)*qicen(2)
            qh1 = qhtot - qh2
            qicen(1) = qh1/hlyr
         endif
      else
C-    Layer 2 gives ice to layer 1
         f1 = hnew(1)/hlyr
         qicen(1) = f1*qicen(1) + (1. _d 0-f1)*qicen(2)
      endif

#endif  /* ALLOW_THSICE */

      RETURN
      END
