
#include "CPP_OPTIONS.h"
#include "GCHEM_OPTIONS.h"

CStartOfInterface
      SUBROUTINE CFC_PARAM( myThid )
C     /==========================================================\
C     | SUBROUTINE CFC_PARAM                                     |
C     | o Set parameters needed for                              |
C     | o CFC runs
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "CFC.h"

C     == Routine arguments ==
C     myThid -  Number of this instance of INI_TR1
      INTEGER myThid
      integer i,j,bi,bj
CEndOfInterface

C     == Local variables ==

#ifdef ALLOW_CFC

      _BARRIER

C set up coefficients for CFC11
C define Schmidt no. coefficients for CFC11
      sca_11_1 = 3501.8
      sca_11_2 = -210.31
      sca_11_3 =    6.1851
      sca_11_4 =   -0.075139
C coefficients for determining solubility of CFC12
      A1_11=  -229.9261
      A2_11=   319.6552
      A3_11=   119.4471
      A4_11=   -1.39165
      B1_11=   -0.142382
      B2_11=    0.091459
      B3_11=   -0.0157274

C set up coefficients for CFC12
C define Schmidt no. coefficients for CFC12
      sca_12_1 = 3845.4
      sca_12_2 = -228.95
      sca_12_3 =    6.1908
      sca_12_4 =   -0.067430
C coefficients for determining solubility of CFC12
      A1_12=  -218.0971
      A2_12=   298.9702
      A3_12=   113.8049
      A4_12=   -1.39165
      B1_12=   -0.143566 
      B2_12=    0.091015
      B3_12=   -0.0153924

c
#endif



      RETURN
      END
