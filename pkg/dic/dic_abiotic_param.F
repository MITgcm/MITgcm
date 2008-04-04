C $Header: /u/gcmpack/MITgcm/pkg/dic/Attic/dic_abiotic_param.F,v 1.9 2008/04/04 21:32:37 dfer Exp $
C $Name:  $

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
#include "DIC_VARS.h"

c !INPUT PARAMETERS: ===================================================
      INTEGER myThid

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
      integer i,j,bi,bj
CEOP

      _BARRIER

C set up coefficients for DIC chemistry
C define Schmidt no. coefficients for CO2
      sca1 = 2073.1 _d 0
      sca2 = -125.62 _d 0
      sca3 =    3.6276 _d 0
      sca4 =   -0.043219 _d 0
C define Schmidt no. coefficients for O2
C based on Keeling et al [GBC, 12, 141, (1998)]
      sox1 = 1638.0 _d 0
      sox2 = -81.83 _d 0
      sox3 =    1.483 _d 0
      sox4 =   -0.008004 _d 0

C coefficients for determining saturation O2
      oA0=  2.00907 _d 0
      oA1=  3.22014 _d 0
      oA2=  4.05010 _d 0
      oA3=  4.94457 _d 0
      oA4= -2.56847 _d -1
      oA5=  3.88767 _d 0
      oB0= -6.24523 _d -3
      oB1= -7.37614 _d -3
      oB2= -1.03410 _d -2
      oB3= -8.17083 _d -3
      oC0= -4.88682 _d -7


C set carbon mol/m3 <---> mol/kg conversion factor
      permil = 1. _d 0 / 1024.5 _d 0

cph this is to please TAF
      gsm_s = 0. _d 0
      gsm_dic = 0. _d 0
      gsm_alk = 0. _d 0

C Conversion factor for atmospheric pressure pLoad when coupled
C to atmospheric model: 1 Atm = 1.01325e5 Pa = 1013.25 mb
      Pa2Atm = 1.01325 _d 5

c
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          ak0(i,j,bi,bj)=0. _d 0
          ak1(i,j,bi,bj)=0. _d 0
          ak2(i,j,bi,bj)=0. _d 0
          akw(i,j,bi,bj)=0. _d 0
          akb(i,j,bi,bj)=0. _d 0
          akf(i,j,bi,bj)=0. _d 0
          ak1p(i,j,bi,bj)=0. _d 0
          ak2p(i,j,bi,bj)=0. _d 0
          ak3p(i,j,bi,bj)=0. _d 0
          aksi(i,j,bi,bj)=0. _d 0
          ff(i,j,bi,bj)=0. _d 0
          ft(i,j,bi,bj)=0. _d 0
          st(i,j,bi,bj)=0. _d 0
          bt(i,j,bi,bj)=0. _d 0
         ENDDO
        ENDDO
       ENDDO
      ENDDO


      RETURN
      END
