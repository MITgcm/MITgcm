
#include "DIC_OPTIONS.h"
#include "GCHEM_OPTIONS.h"

CBOP
C !ROUTINE: DIC_ABIOTIC_PARAM

C !INTERFACE: ==========================================================
      SUBROUTINE DIC_ABIOTIC_PARAM( myThid )

C !DESCRIPTION:
C Set parameters needed for calculating air-sea exchange of CO2 and O2

C !USES: =============================================================== 
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "DIC_ABIOTIC.h"

c !INPUT PARAMETERS: ===================================================
      INTEGER myThid

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
      integer i,j,bi,bj
CEOP

      _BARRIER

C set up coefficients for DIC chemistry
C define Schmidt no. coefficients for CO2
      sca1 = 2073.1
      sca2 = -125.62
      sca3 =    3.6276
      sca4 =   -0.043219
C define Schmidt no. coefficients for O2
C based on Keeling et al [GBC, 12, 141, (1998)]
      sox1 = 1638.0
      sox2 = -81.83
      sox3 =    1.483
      sox4 =   -0.008004

C coefficients for determining saturation O2
      oA0=  2.00907
      oA1=  3.22014
      oA2=  4.05010
      oA3=  4.94457
      oA4= -2.56847E-1
      oA5=  3.88767
      oB0= -6.24523E-3
      oB1= -7.37614E-3
      oB2= -1.03410E-2
      oB3= -8.17083E-3
      oC0= -4.88682E-7


C set carbon mol/m3 <---> mol/kg conversion factor
      permil = 1.0 / 1024.5

cph this is to please TAF
      gsm_s = 0.
      gsm_dic = 0.
      gsm_alk = 0.

C Conversion factor for atmospheric pressure pLoad when coupled 
C to atmospheric model: 1 Atm = 1.01325e5 Pa = 1013.25 mb
      Pa2Atm = 1.01325 _d 5

c
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          ak0(i,j,bi,bj)=0.d0
          ak1(i,j,bi,bj)=0.d0
          ak2(i,j,bi,bj)=0.d0
          akw(i,j,bi,bj)=0.d0
          akb(i,j,bi,bj)=0.d0
          akf(i,j,bi,bj)=0.d0
          ak1p(i,j,bi,bj)=0.d0
          ak2p(i,j,bi,bj)=0.d0
          ak3p(i,j,bi,bj)=0.d0
          aksi(i,j,bi,bj)=0.d0
          ff(i,j,bi,bj)=0.d0
          ft(i,j,bi,bj)=0.d0
          st(i,j,bi,bj)=0.d0
          bt(i,j,bi,bj)=0.d0
         ENDDO
        ENDDO
       ENDDO
      ENDDO


      RETURN
      END
