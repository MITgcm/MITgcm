C $Header: /u/gcmpack/MITgcm/model/src/ini_eos.F,v 1.16 2008/01/24 17:22:06 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"
#define EXCLUDE_EOS_CHECK

CBOP
C     !ROUTINE: INI_EOS
C     !INTERFACE:
      SUBROUTINE INI_EOS( myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE INI_EOS
C     | o Initialise coefficients of equation of state.
C     *==========================================================*
C     \ev

C     !USES:

      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "EOS.h"
#include "GRID.h"
#include "DYNVARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid -  Number of this instance of INI_CORI
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     i,k    :: Loop counters
      INTEGER  i, k
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      IF ( .NOT.fluidIsWater ) RETURN

      _BARRIER
      _BEGIN_MASTER(myThid)

      equationOfState = eosType

      DO k = 1,6
         eosJMDCFw(k) = 0. _d 0
      ENDDO
      DO k = 1,9
         eosJMDCSw(k) = 0. _d 0
      ENDDO
      DO k = 1,5
         eosJMDCKFw(k) = 0. _d 0
      ENDDO
      DO k = 1,7
         eosJMDCKSw(k) = 0. _d 0
      ENDDO
      DO k = 1,14
         eosJMDCKP(k) = 0. _d 0
      ENDDO
      DO k = 0,11
         eosMDJWFnum(k) = 0. _d 0
      ENDDO
      DO k = 0,12
         eosMDJWFden(k) = 0. _d 0
      ENDDO

      IF ( equationOfState .EQ. 'LINEAR' ) THEN
         IF ( tAlpha .EQ. UNSET_RL ) tAlpha = 2.  _d -4
         IF ( sBeta  .EQ. UNSET_RL ) sBeta  = 7.4 _d -4
      ELSEIF ( equationOfState .EQ. 'POLY3' ) THEN
         OPEN(37,FILE='POLY3.COEFFS',STATUS='OLD',FORM='FORMATTED')
         READ(37,*) k
         IF (k.NE.Nr) THEN
            WRITE(msgBuf,'(A)')
     &           'ini_eos: attempt to read POLY3.COEFFS failed'
            CALL PRINT_ERROR( msgBuf , myThid )
            WRITE(msgBuf,'(A)')
     &           '           because bad # of levels in data'
            CALL PRINT_ERROR( msgBuf , myThid )
            STOP 'Bad data in POLY3.COEFFS'
         ENDIF
         READ(37,*) (eosRefT(k),eosRefS(k),eosSig0(k),k=1,Nr)
         DO k=1,Nr
            READ(37,*) (eosC(i,k),i=1,9)
         ENDDO
         CLOSE(37)

      ELSEIF ( equationOfState .EQ. 'JMD95Z'
     &    .OR. equationOfState .EQ. 'JMD95P'
     &    .OR. equationOfState .EQ. 'UNESCO' ) THEN
C
C     Jackett & McDougall (1995, JPO) equation of state
C     rho = R(salinity, potential temperature, pressure)
C     pressure needs to be available (from the previous
C     time step to linearize the problem)
C
         IF ( equationOfState .EQ. 'JMD95Z' .AND. usingPCoords ) THEN
            WRITE(msgBuf,'(A)')
     &      'ini_eos: equation of state ''JMD95Z'' should not'
            CALL PRINT_ERROR( msgBuf , myThid )
            WRITE(msgBuf,'(A)')
     &      '         be used together with pressure coordinates.'
            CALL PRINT_ERROR( msgBuf , myThid )
            WRITE(msgBuf,'(A)')
     &      '         Use only ''JMD95P'' with ''OCEANICP''.'
            CALL PRINT_ERROR( msgBuf , myThid )
            STOP 'ABNORMAL END: S/R INI_EOS'
         ENDIF

C     coefficients nonlinear equation of state in pressure coordinates for
C     1. density of fresh water at p = 0
         eosJMDCFw(1) =  999.842594 _d +00
         eosJMDCFw(2) =    6.793952 _d -02
         eosJMDCFw(3) = -  9.095290 _d -03
         eosJMDCFw(4) =    1.001685 _d -04
         eosJMDCFw(5) = -  1.120083 _d -06
         eosJMDCFw(6) =    6.536332 _d -09
C     2. density of sea water at p = 0
         eosJMDCSw(1) =    8.24493  _d -01
         eosJMDCSw(2) = -  4.0899   _d -03
         eosJMDCSw(3) =    7.6438   _d -05
         eosJMDCSw(4) = -  8.2467   _d -07
         eosJMDCSw(5) =    5.3875   _d -09
         eosJMDCSw(6) = -  5.72466  _d -03
         eosJMDCSw(7) =    1.0227   _d -04
         eosJMDCSw(8) = -  1.6546   _d -06
         eosJMDCSw(9) =    4.8314   _d -04
         IF ( equationOfState(1:5) .EQ. 'JMD95' ) THEN
C     3. secant bulk modulus K of fresh water at p = 0
            eosJMDCKFw(1) =   1.965933 _d +04
            eosJMDCKFw(2) =   1.444304 _d +02
            eosJMDCKFw(3) = - 1.706103 _d +00
            eosJMDCKFw(4) =   9.648704 _d -03
            eosJMDCKFw(5) = - 4.190253 _d -05
C     4. secant bulk modulus K of sea water at p = 0
            eosJMDCKSw(1) =   5.284855 _d +01
            eosJMDCKSw(2) = - 3.101089 _d -01
            eosJMDCKSw(3) =   6.283263 _d -03
            eosJMDCKSw(4) = - 5.084188 _d -05
            eosJMDCKSw(5) =   3.886640 _d -01
            eosJMDCKSw(6) =   9.085835 _d -03
            eosJMDCKSw(7) = - 4.619924 _d -04
C     5. secant bulk modulus K of sea water at p
            eosJMDCKP( 1) =   3.186519 _d +00
            eosJMDCKP( 2) =   2.212276 _d -02
            eosJMDCKP( 3) = - 2.984642 _d -04
            eosJMDCKP( 4) =   1.956415 _d -06
            eosJMDCKP( 5) =   6.704388 _d -03
            eosJMDCKP( 6) = - 1.847318 _d -04
            eosJMDCKP( 7) =   2.059331 _d -07
            eosJMDCKP( 8) =   1.480266 _d -04
            eosJMDCKP( 9) =   2.102898 _d -04
            eosJMDCKP(10) = - 1.202016 _d -05
            eosJMDCKP(11) =   1.394680 _d -07
            eosJMDCKP(12) = - 2.040237 _d -06
            eosJMDCKP(13) =   6.128773 _d -08
            eosJMDCKP(14) =   6.207323 _d -10

         ELSEIF ( equationOfState .EQ. 'UNESCO' ) THEN

            WRITE(msgBuf,'(a)')
     &           'WARNING WARNING WARNING WARNING WARNING WARNING '
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &           SQUEEZE_RIGHT , myThid )
            WRITE(msgBuf,'(a,a)')
     &           'WARNING: using the UNESCO formula with potential ',
     &           'temperature'
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &           SQUEEZE_RIGHT , myThid )
            WRITE(msgBuf,'(a)')
     &           'WARNING: can result in density errors of up to 5%'
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &           SQUEEZE_RIGHT , myThid )
            WRITE(msgBuf,'(a)')
     &           'WARNING: (see Jackett and McDougall 1995, JAOT)'
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &           SQUEEZE_RIGHT , myThid )
            WRITE(msgBuf,'(a)')
     &           'WARNING WARNING WARNING WARNING WARNING WARNING '
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &           SQUEEZE_RIGHT , myThid )

C     3. secant bulk modulus K of fresh water at p = 0
            eosJMDCKFw(1) =   1.965221 _d +04
            eosJMDCKFw(2) =   1.484206 _d +02
            eosJMDCKFw(3) = - 2.327105 _d +00
            eosJMDCKFw(4) =   1.360477 _d -02
            eosJMDCKFw(5) = - 5.155288 _d -05
C     4. secant bulk modulus K of sea water at p = 0
            eosJMDCKSw(1) =   5.46746  _d +01
            eosJMDCKSw(2) = - 0.603459 _d +00
            eosJMDCKSw(3) =   1.09987  _d -02
            eosJMDCKSw(4) = - 6.1670   _d -05
            eosJMDCKSw(5) =   7.944    _d -02
            eosJMDCKSw(6) =   1.6483   _d -02
            eosJMDCKSw(7) = - 5.3009   _d -04
C     5. secant bulk modulus K of sea water at p
            eosJMDCKP( 1) =   3.239908 _d +00
            eosJMDCKP( 2) =   1.43713  _d -03
            eosJMDCKP( 3) =   1.16092  _d -04
            eosJMDCKP( 4) = - 5.77905  _d -07
            eosJMDCKP( 5) =   2.2838   _d -03
            eosJMDCKP( 6) = - 1.0981   _d -05
            eosJMDCKP( 7) = - 1.6078   _d -06
            eosJMDCKP( 8) =   1.91075  _d -04
            eosJMDCKP( 9) =   8.50935  _d -05
            eosJMDCKP(10) = - 6.12293  _d -06
            eosJMDCKP(11) =   5.2787   _d -08
            eosJMDCKP(12) = - 9.9348   _d -07
            eosJMDCKP(13) =   2.0816   _d -08
            eosJMDCKP(14) =   9.1697   _d -10
         ELSE
            STOP 'INI_EOS: We should never reach this point!'
         ENDIF

      ELSEIF ( equationOfState .EQ. 'MDJWF' ) THEN

         eosMDJWFnum( 0) =  9.99843699 _d +02
         eosMDJWFnum( 1) =  7.35212840 _d +00
         eosMDJWFnum( 2) = -5.45928211 _d -02
         eosMDJWFnum( 3) =  3.98476704 _d -04
         eosMDJWFnum( 4) =  2.96938239 _d +00
         eosMDJWFnum( 5) = -7.23268813 _d -03
         eosMDJWFnum( 6) =  2.12382341 _d -03
         eosMDJWFnum( 7) =  1.04004591 _d -02
         eosMDJWFnum( 8) =  1.03970529 _d -07
         eosMDJWFnum( 9) =  5.18761880 _d -06
         eosMDJWFnum(10) = -3.24041825 _d -08
         eosMDJWFnum(11) = -1.23869360 _d -11


         eosMDJWFden( 0) =  1.00000000 _d +00
         eosMDJWFden( 1) =  7.28606739 _d -03
         eosMDJWFden( 2) = -4.60835542 _d -05
         eosMDJWFden( 3) =  3.68390573 _d -07
         eosMDJWFden( 4) =  1.80809186 _d -10
         eosMDJWFden( 5) =  2.14691708 _d -03
         eosMDJWFden( 6) = -9.27062484 _d -06
         eosMDJWFden( 7) = -1.78343643 _d -10
         eosMDJWFden( 8) =  4.76534122 _d -06
         eosMDJWFden( 9) =  1.63410736 _d -09
         eosMDJWFden(10) =  5.30848875 _d -06
         eosMDJWFden(11) = -3.03175128 _d -16
         eosMDJWFden(12) = -1.27934137 _d -17

      ELSEIF( equationOfState .EQ. 'IDEALG' ) THEN

      ELSE

         WRITE(msgbuf,'(3a)') ' INI_EOS: equationOfState = "',
     &        equationOfState,'"'
         CALL PRINT_ERROR( msgbuf, myThid )
         STOP 'ABNORMAL END: S/R INI_EOS'

      ENDIF

C--   Check EOS initialisation:

      CALL CHECK_EOS( myThid )

      _END_MASTER( myThid )
      _BARRIER

      RETURN
      END

CBOP
C     !ROUTINE: CHECK_EOS
C     !INTERFACE:
      SUBROUTINE CHECK_EOS( myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE CHECK_EOS
C     | o check the equation of state.
C     *==========================================================*
C     \ev

C     !USES:

      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "EOS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid -  Number of this instance of CHECK_EOS
      INTEGER myThid

#ifndef EXCLUDE_EOS_CHECK
C     !LOCAL VARIABLES:
C     == Local variables ==
C     bi,bj  - Loop counters
C     I,J,K
      INTEGER bi, bj
      INTEGER imin, imax, jmin, jmax
      INTEGER  i, j, k
      _RL tFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL sFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL rhoLoc (1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL bulkMod(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL psave

      INTEGER ncheck, kcheck
      PARAMETER ( ncheck = 13 )
      _RL tloc(ncheck), ptloc(ncheck), sloc(ncheck), ploc(ncheck)
      _RL rloc(ncheck), bloc(ncheck)

      CHARACTER*(MAX_LEN_MBUF) msgBuf

      DATA tloc
     &     /3.25905152915860 _d 0, 20.38687090048638 _d 0,
     &     25.44820830309568 _d 0, 20.17368557065936 _d 0,
     &     13.43397459640398 _d 0,
     &      5.               _d 0, 25.               _d 0,
     &      5.               _d 0, 25.               _d 0,
     &      5.               _d 0, 25.               _d 0,
     &      5.               _d 0, 25.               _d 0/,
     &     ptloc
     &     /3.               _d 0, 20.               _d 0,
     &     25.               _d 0, 20.               _d 0,
     &     12.               _d 0,
     &      5.               _d 0, 25.               _d 0,
     &      5.               _d 0, 25.               _d 0,
     &      4.03692566635316 _d 0, 22.84661726775120 _d 0,
     &      3.62720389416752 _d 0, 22.62420229124846 _d 0/
     &     sloc
     &     /35.5 _d 0, 35. _d 0,
     &      35.0 _d 0, 20. _d 0,
     &      40.0 _d 0,
     &       0.  _d 0,  0. _d 0,
     &      35.  _d 0, 35. _d 0,
     &       0.  _d 0,  0. _d 0,
     &      35.  _d 0, 35. _d 0/
     &     ploc
     &     /300. _d 5,  200. _d 5,
     &      200. _d 5,  100. _d 5,
     &      800. _d 5,
     &        0. _d 0,    0. _d 0,
     &        0. _d 0,    0. _d 0,
     &     1000. _d 5, 1000. _d 5,
     &     1000. _d 5, 1000. _d 5/
      DATA rloc
     &     /1041.83267  _d 0, 1033.213387 _d 0,
     &      1031.654229 _d 0, 1017.726743 _d 0,
     &      1062.928258 _d 0,
     &       999.96675  _d 0,  997.04796  _d 0,
     &      1027.67547  _d 0, 1023.34306  _d 0,
     &      1044.12802  _d 0, 1037.90204  _d 0,
     &      1069.48914  _d 0, 1062.53817  _d 0/
     &     bloc
     &     /   -1.00000 _d 0,    -1.00000 _d 0,
     &         -1.00000 _d 0,    -1.00000 _d 0,
     &         -1.00000 _d 0,
     &      20337.80375 _d 0, 22100.72106 _d 0,
     &      22185.93358 _d 0, 23726.34949 _d 0,
     &      23643.52599 _d 0, 25405.09717 _d 0,
     &      25577.49819 _d 0, 27108.94504 _d 0/


      bi   = 1
      bj   = 1
      k    = 1
      imin = 1
      imax = 1
      jmin = 1
      jmax = 1
      i    = 1
      j    = 1
      IF ( equationOfState.NE.'LINEAR'
     &     .AND. equationOfState.NE.'POLY3' ) THEN
C     check nonlinear EOS
         WRITE(msgBuf,'(a,a)')
     &        'check_eos: Check the equation of state: Type ',
     &        equationOfState
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &        SQUEEZE_RIGHT , myThid )
         psave = pressure(i,j,k,bi,bj)
         DO kcheck = 1,ncheck
            pressure(i,j,k,bi,bj) = ploc(kcheck)
            IF ( equationOfState.NE.'UNESCO' ) THEN
               tFld(i,j,k,bi,bj) = ptloc(kcheck)
            ELSE
               tFld(i,j,k,bi,bj) = tloc(kcheck)
            ENDIF
            sFld(i,j,k,bi,bj)    = sloc(kcheck)
            rholoc(i,j)          =  0. _d 0
            bulkMod(i,j)         = -1. _d 0

            CALL FIND_RHO(
     &           bi, bj, iMin, iMax, jMin, jMax,  k, k,
     &           tFld, sFld, rholoc, myThid )

            CALL FIND_BULKMOD(
     &           bi, bj, imin, imax, jmin, jmax, k, k,
     &           tFld, sFld, bulkMod, myThid )

            WRITE(msgBuf,
     &           '(a4,f4.1,a5,f4.1,a6,f5.0,a5,a3,f10.5,1x,f11.5)')
     &           'rho(', sFld(i,j,k,bi,bj), ' PSU,',
     &           tFld(i,j,k,bi,bj), ' degC,',
c    &           pressure(i,j,k,bi,bj)*SItoBar, ' bar)',' = ',
     &           rloc(kcheck), bloc(kcheck)
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &           SQUEEZE_RIGHT , myThid )
            WRITE(msgBuf,'(a14,a22,f10.5,1x,f11.5)')
     &           'rho(find_rho) ',
     &           ' = ', rholoc(i,j)+rhoConst, bulkMod(i,j)
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &           SQUEEZE_RIGHT , myThid )

            CALL FIND_RHO_SCALAR( tFld(i,j,k,bi,bj), sLoc(kcheck),
     &           pLoc(kcheck), rhoLoc(i,j), myThid )
            bulkMod(i,j) = 0. _d 0
            WRITE(msgBuf,'(a21,a15,f10.5,1x,f11.5)')
     &           'rho(find_rho_scalar) ',
     &           ' = ', rholoc(i,j)+rhoConst, bulkMod(i,j)
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &           SQUEEZE_RIGHT , myThid )

         ENDDO
C     end check nonlinear EOS
c        pressure(i,j,k,bi,bj) = psave

         WRITE(msgBuf,'(A)') 'end check the equation of state'
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &        SQUEEZE_RIGHT , myThid )

      ENDIF
#endif /* EXCLUDE_EOS_CHECK */

      RETURN
      END

