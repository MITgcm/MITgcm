C $Header: /u/gcmpack/MITgcm/pkg/cfc/cfc_param.F,v 1.2 2006/08/02 22:50:49 jmc Exp $
C $Name:  $

#include "GCHEM_OPTIONS.h"

CStartOfInterface
      SUBROUTINE CFC_PARAM( myThid )
C     *==========================================================*
C     | SUBROUTINE CFC_PARAM
C     | o Set parameters needed for CFC runs
C     *==========================================================*
C     *==========================================================*
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
c #include "PARAMS.h"
#include "CFC.h"

C     == Routine arguments ==
C     myThid -  Number of this instance of CFC_PARAM
      INTEGER myThid
CEndOfInterface

#ifdef ALLOW_CFC

C     == Local variables ==

      _BEGIN_MASTER( myThid )
C set up coefficients for CFC11
C define Schmidt no. coefficients for CFC11
      sca_11_1 = 3501.8 _d 0
      sca_11_2 = -210.31 _d 0
      sca_11_3 =    6.1851 _d 0
      sca_11_4 =   -0.075139 _d 0
C coefficients for determining solubility of CFC12
      A1_11=  -229.9261 _d 0
      A2_11=   319.6552 _d 0
      A3_11=   119.4471 _d 0
      A4_11=   -1.39165 _d 0
      B1_11=   -0.142382 _d 0
      B2_11=    0.091459 _d 0
      B3_11=   -0.0157274 _d 0

C set up coefficients for CFC12
C define Schmidt no. coefficients for CFC12
      sca_12_1 = 3845.4 _d 0
      sca_12_2 = -228.95 _d 0
      sca_12_3 =    6.1908 _d 0
      sca_12_4 =   -0.067430 _d 0
C coefficients for determining solubility of CFC12
      A1_12=  -218.0971 _d 0
      A2_12=   298.9702 _d 0
      A3_12=   113.8049 _d 0
      A4_12=   -1.39165 _d 0
      B1_12=   -0.143566 _d 0
      B2_12=    0.091015 _d 0
      B3_12=   -0.0153924 _d 0

      _END_MASTER( myThid )

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* ALLOW_CFC */

      RETURN
      END
