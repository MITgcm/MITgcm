#include "CPP_OPTIONS.h"
#undef INCLUDE_EOS_CHECK

C--  File ini_eos.F: Routines to initialise Equation of State
C--   Contents
C--   o INI_EOS
C--   o EOS_CHECK

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
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

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid -  Number of this instance of INI_CORI
      INTEGER myThid

C     !FUNCTIONS
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
C     == Local variables ==
C     i,k    :: Loop counters
      INTEGER  i, k, iLen
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      IF ( .NOT.fluidIsWater ) RETURN

      _BARRIER
      _BEGIN_MASTER(myThid)

      equationOfState = eosType
      iLen = ILNBLNK(equationOfState)
      iLen = MAX(iLen,1)

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
      DO k = 1,48
         teos(k)        = 0. _d 0
      ENDDO

C     For all current EOS that explicitly depend on pressure ('UNESCO',
C     'JMD95','MDJWF' & 'TEOS10' ), the input pressure is assumed to be
C     relative to reference atmospheric pressure of 1.atm = 101325 Pa
      eosRefP0 = 101325. _d 0

      IF ( equationOfState .EQ. 'LINEAR' ) THEN
         IF ( tAlpha .EQ. UNSET_RL ) tAlpha = 2.  _d -4
         IF ( sBeta  .EQ. UNSET_RL ) sBeta  = 7.4 _d -4
      ELSEIF ( equationOfState .EQ. 'POLY3' ) THEN
         OPEN(37,FILE='POLY3.COEFFS',STATUS='OLD',FORM='FORMATTED')
         READ(37,*) k
         IF (k.NE.Nr) THEN
            WRITE(msgBuf,'(A)')
     &           'ini_eos: attempt to read POLY3.COEFFS failed'
            CALL PRINT_ERROR( msgBuf, myThid )
            WRITE(msgBuf,'(A)')
     &           '           because bad # of levels in data'
            CALL PRINT_ERROR( msgBuf, myThid )
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

C     Jackett & McDougall (1995, JAOT) equation of state
C     rho = R(salinity, potential temperature, pressure)
C     pressure needs to be available (from the previous
C     time step to linearize the problem)

         IF ( equationOfState .EQ. 'JMD95Z' .AND. usingPCoords ) THEN
            WRITE(msgBuf,'(A)')
     &      'ini_eos: equation of state ''JMD95Z'' should not'
            CALL PRINT_ERROR( msgBuf, myThid )
            WRITE(msgBuf,'(A)')
     &      '         be used together with pressure coordinates.'
            CALL PRINT_ERROR( msgBuf, myThid )
            WRITE(msgBuf,'(A)')
     &      '         Use only ''JMD95P'' with ''OCEANICP''.'
            CALL PRINT_ERROR( msgBuf, myThid )
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
     &                          SQUEEZE_RIGHT, myThid )
            WRITE(msgBuf,'(a,a)')
     &           'WARNING: using the UNESCO formula with potential ',
     &           'temperature'
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                          SQUEEZE_RIGHT, myThid )
            WRITE(msgBuf,'(a)')
     &           'WARNING: can result in density errors of up to 5%'
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                          SQUEEZE_RIGHT, myThid )
            WRITE(msgBuf,'(a)')
     &           'WARNING: (see Jackett and McDougall 1995, JAOT)'
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                          SQUEEZE_RIGHT, myThid )
            WRITE(msgBuf,'(a)')
     &           'WARNING WARNING WARNING WARNING WARNING WARNING '
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                          SQUEEZE_RIGHT, myThid )

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

      ELSEIF ( equationOfState .EQ. 'TEOS10' ) THEN

       teos(01) =  9.998420897506056 _d +02
       teos(02) =  2.839940833161907 _d  00
       teos(03) = -3.147759265588511 _d -02
       teos(04) =  1.181805545074306 _d -03
       teos(05) = -6.698001071123802 _d  00
       teos(06) = -2.986498947203215 _d -02
       teos(07) =  2.327859407479162 _d -04
       teos(08) = -3.988822378968490 _d -02
       teos(09) =  5.095422573880500 _d -04
       teos(10) = -1.426984671633621 _d -05
       teos(11) =  1.645039373682922 _d -07
       teos(12) = -2.233269627352527 _d -02
       teos(13) = -3.436090079851880 _d -04
       teos(14) =  3.726050720345733 _d -06
       teos(15) = -1.806789763745328 _d -04
       teos(16) =  6.876837219536232 _d -07
       teos(17) = -3.087032500374211 _d -07
       teos(18) = -1.988366587925593 _d -08
       teos(19) = -1.061519070296458 _d -11
       teos(20) =  1.550932729220080 _d -10
       teos(21) =  1.000000000000000 _d  00
       teos(22) =  2.775927747785646 _d -03
       teos(23) = -2.349607444135925 _d -05
       teos(24) =  1.119513357486743 _d -06
       teos(25) =  6.743689325042773 _d -10
       teos(26) = -7.521448093615448 _d -03
       teos(27) = -2.764306979894411 _d -05
       teos(28) =  1.262937315098546 _d -07
       teos(29) =  9.527875081696435 _d -10
       teos(30) = -1.811147201949891 _d -11
       teos(31) = -3.303308871386421 _d -05
       teos(32) =  3.801564588876298 _d -07
       teos(33) = -7.672876869259043 _d -09
       teos(34) = -4.634182341116144 _d -11
       teos(35) =  2.681097235569143 _d -12
       teos(36) =  5.419326551148740 _d -06
       teos(37) = -2.742185394906099 _d -05
       teos(38) = -3.212746477974189 _d -07
       teos(39) =  3.191413910561627 _d -09
       teos(40) = -1.931012931541776 _d -12
       teos(41) = -1.105097577149576 _d -07
       teos(42) =  6.211426728363857 _d -10
       teos(43) = -1.119011592875110 _d -10
       teos(44) = -1.941660213148725 _d -11
       teos(45) = -1.864826425365600 _d -14
       teos(46) =  1.119522344879478 _d -14
       teos(47) = -1.200507748551599 _d -15
       teos(48) =  6.057902487546866 _d -17

       Sprac_Sref = 35.0 _d 0 / 35.16504 _d 0
       I_S0       = 0.025 _d 0 *Sprac_Sref
       I_Ts       = 0.025 _d 0
       I_cp0      = 1.0 _d 0 / 3991.86795711963 _d 0

       H00 = 61.01362420681071 _d 0   *  I_cp0
       H01 = 168776.46138048015 _d 0  * (I_cp0*I_Ts)
       H02 = -2735.2785605119625 _d 0 * (I_cp0*I_Ts**2)
       H03 = 2574.2164453821433 _d 0  * (I_cp0*I_Ts**3)
       H04 = -1536.6644434977543 _d 0 * (I_cp0*I_Ts**4)
       H05 = 545.7340497931629 _d 0   * (I_cp0*I_Ts**5)
       H06 = -50.91091728474331 _d 0  * (I_cp0*I_Ts**6)
       H07 = -18.30489878927802 _d 0  * (I_cp0*I_Ts**7)
       H20 = 268.5520265845071 _d 0   *  I_cp0
       H21 = -12019.028203559312 _d 0 * (I_cp0*I_Ts)
       H22 = 3734.858026725145 _d 0   * (I_cp0*I_Ts**2)
       H23 = -2046.7671145057618 _d 0 * (I_cp0*I_Ts**3)
       H24 = 465.28655623826234 _d 0  * (I_cp0*I_Ts**4)
       H25 = -0.6370820302376359 _d 0 * (I_cp0*I_Ts**5)
       H26 = -10.650848542359153 _d 0 * (I_cp0*I_Ts**6)
       H30 = 937.2099110620707 _d 0   *  I_cp0
       H31 = 588.1802812170108 _d 0   * (I_cp0*I_Ts)
       H32 = 248.39476522971285 _d 0  * (I_cp0*I_Ts**2)
       H33 = -3.871557904936333 _d 0  * (I_cp0*I_Ts**3)
       H34 = -2.6268019854268356 _d 0 * (I_cp0*I_Ts**4)
       H40 = -1687.914374187449 _d 0  *  I_cp0
       H41 = 936.3206544460336 _d 0   * (I_cp0*I_Ts)
       H42 = -942.7827304544439 _d 0  * (I_cp0*I_Ts**2)
       H43 = 369.4389437509002 _d 0   * (I_cp0*I_Ts**3)
       H44 = -33.83664947895248 _d 0  * (I_cp0*I_Ts**4)
       H45 = -9.987880382780322 _d 0  * (I_cp0*I_Ts**5)
       H50 = 246.9598888781377 _d 0   *  I_cp0
       H60 = 123.59576582457964 _d 0  *  I_cp0
       H70 = -48.5891069025409 _d 0   *  I_cp0

       TPN00 = -1.446013646344788 _d -2
       TPN10 = -3.305308995852924 _d -3 * Sprac_Sref
       TPN20 =  1.062415929128982 _d -4 * Sprac_Sref**2
       TPN01 =  9.477566673794488 _d -1
       TPN11 =  2.166591947736613 _d -3 * Sprac_Sref
       TPN02 =  3.828842955039902 _d -3
       TPD10 =  6.506097115635800 _d -4 * Sprac_Sref
       TPD01 =  3.830289486850898 _d -3
       TPD02 =  1.247811760368034 _d -6

      ELSEIF( equationOfState .EQ. 'IDEALG' ) THEN

      ELSE

         WRITE(msgBuf,'(3A)') 'INI_EOS: eosType= "',
     &                     equationOfState(1:iLen), '" not valid'
         CALL PRINT_ERROR( msgbuf, myThid )
         STOP 'ABNORMAL END: S/R INI_EOS'

      ENDIF

C--   Check EOS initialisation:

      CALL EOS_CHECK( myThid )

      _END_MASTER( myThid )
      _BARRIER

      RETURN
      END

CBOP
C     !ROUTINE: EOS_CHECK
C     !INTERFACE:
      SUBROUTINE EOS_CHECK( myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE EOS_CHECK
C     | o check the equation of state.
C     *==========================================================*
C     \ev

C     !USES:

      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "EOS.h"
#include "GRID.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid ::  Number of this instance of EOS_CHECK
      INTEGER myThid

#ifdef INCLUDE_EOS_CHECK
C     !LOCAL VARIABLES:
C     == Local variables ==
C     bi,bj  - Loop counters
C     i,j,k
      INTEGER bi, bj
      INTEGER iMin, iMax, jMin, jMax
      INTEGER  i, j, k
      _RL tFld   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sFld   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL pFld   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rhoLoc (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL bulkMod(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

      INTEGER ncheck, kcheck, k1c, k2c, iLen
      PARAMETER ( ncheck = 20 )
      _RL tLoc(ncheck), ptLoc(ncheck), sLoc(ncheck), pLoc(ncheck)
      _RL rLoc(ncheck), bLoc(ncheck)
      _RL surf_pRef_save, rhoDif

      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*(13) blkWrd

      DATA tLoc
     &     /3.25905152915860 _d 0, 20.38687090048638 _d 0,
     &     25.44820830309568 _d 0, 20.17368557065936 _d 0,
     &     13.43397459640398 _d 0,
     &      5.               _d 0, 25.               _d 0,
     &      5.               _d 0, 25.               _d 0,
     &      5.               _d 0, 25.               _d 0,
     &      5.               _d 0, 25.               _d 0,
     &      5.               _d 0, 25.               _d 0,
     &      5.               _d 0, 25.               _d 0,
     &      5.               _d 0, 25.               _d 0,
     &      5.               _d 0/
     &     ptLoc
     &     /3.               _d 0, 20.               _d 0,
     &     25.               _d 0, 20.               _d 0,
     &     12.               _d 0,
     &      5.               _d 0, 25.               _d 0,
     &      5.               _d 0, 25.               _d 0,
     &      4.03692566635316 _d 0, 22.84661726775120 _d 0,
     &      3.62720389416752 _d 0, 22.62420229124846 _d 0,
     &     19.5130           _d 0,  3.5588           _d 0,
     &      1.8157           _d 0,  1.3434           _d 0,
     &      1.1583           _d 0,  1.0518           _d 0,
     &      1.0073           _d 0/
     &     sLoc
     &     /35.5 _d 0, 35. _d 0,
     &      35.0 _d 0, 20. _d 0,
     &      40.0 _d 0,
     &       0.  _d 0,  0. _d 0,
     &      35.  _d 0, 35. _d 0,
     &       0.  _d 0,  0. _d 0,
     &      35.  _d 0, 35. _d 0,
     &      34.7392 _d 0, 34.4652 _d 0,
     &      34.7738 _d 0, 34.8435 _d 0,
     &      34.8637 _d 0, 34.8739 _d 0, 34.8776 _d 0/
     &     pLoc
     &     /300. _d 5,  200. _d 5,
     &      200. _d 5,  100. _d 5,
     &      800. _d 5,
     &        0. _d 0,    0. _d 0,
     &        0. _d 0,    0. _d 0,
     &     1000. _d 5, 1000. _d 5,
     &     1000. _d 5, 1000. _d 5,
     &        0. _d 0, 1010. _d 4,
     &     2025. _d 4, 3045. _d 4,
     &     4069. _d 4, 5098. _d 4,
     &     6131. _d 4/
      DATA rLoc
     &     /1041.83267  _d 0, 1033.213387 _d 0,
     &      1031.654229 _d 0, 1017.726743 _d 0,
     &      1062.928258 _d 0,
     &       999.96675  _d 0,  997.04796  _d 0,
     &      1027.67547  _d 0, 1023.34306  _d 0,
     &      1044.12802  _d 0, 1037.90204  _d 0,
     &      1069.48914  _d 0, 1062.53817  _d 0,
     &      1024.571477039354932 _d 0, 1031.937207908966911 _d 0,
     &      1037.002326523349893 _d 0, 1041.668901630106348 _d 0,
     &      1046.179673478751511 _d 0, 1050.591667083023594 _d 0,
     &      1054.901835905235885 _d 0/
     &     bLoc
     &     /   -1.00000 _d 0,    -1.00000 _d 0,
     &         -1.00000 _d 0,    -1.00000 _d 0,
     &         -1.00000 _d 0,
     &      20337.80375 _d 0, 22100.72106 _d 0,
     &      22185.93358 _d 0, 23726.34949 _d 0,
     &      23643.52599 _d 0, 25405.09717 _d 0,
     &      25577.49819 _d 0, 27108.94504 _d 0,
     &         -1.00000 _d 0,    -1.00000 _d 0,
     &         -1.00000 _d 0,    -1.00000 _d 0,
     &         -1.00000 _d 0,    -1.00000 _d 0,
     &         -1.00000 _d 0/

      blkWrd = '             '
      bi   = 1
      bj   = 1
      k    = 1
      iMin = 1
      iMax = 1
      jMin = 1
      jMax = 1
      i    = 1
      j    = 1
      IF (       equationOfState.NE.'LINEAR'
     &     .AND. equationOfState.NE.'POLY3'
     &     .AND. equationOfState.NE.'IDEALG' ) THEN
C     check nonlinear EOS
        WRITE(msgBuf,'(A,A)')
     &        'EOS_CHECK: Check the equation of state: Type ',
     &        equationOfState
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        IF ( equationOfState.EQ.'JMD95Z' .OR.
     &       equationOfState.EQ.'JMD95P' ) THEN
          WRITE(msgBuf,'(A)')
     &         'EOS_CHECK: check Rho values for eosType=JMD95:'
          k1c = 1
          k2c = 1
        ELSEIF ( equationOfState.EQ.'MDJWF' ) THEN
          WRITE(msgBuf,'(A)')
     &         'EOS_CHECK: check Rho values for eosType=MDJWF:'
          k1c = 2
          k2c = 5
        ELSEIF ( equationOfState.EQ.'UNESCO' ) THEN
          WRITE(msgBuf,'(A)')
     &         'EOS_CHECK: check Rho & K values for eosType=UNESCO:'
          k1c = 6
          k2c = 13
        ELSEIF ( equationOfState.EQ.'TEOS10' ) THEN
          WRITE(msgBuf,'(A)')
     &         'EOS_CHECK: check Rho values for eosType=TEOS10:'
          k1c = 14
          k2c = ncheck
        ELSE
          WRITE(msgBuf,'(3A)') 'EOS_CHECK: Invalid eosType= ',
     &          equationOfState
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R EOS_CHECK'
        ENDIF
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
C-    Will use "surf_pRef" to get the right pressure out of S/R PRESSURE_FOR_EOS
C     --> save current value into "surf_pRef_save"
        surf_pRef_save = surf_pRef
        DO kcheck = k1c, k2c

          surf_pRef = eosRefP0 + pLoc(kcheck)
          IF ( usingPCoords ) THEN
            surf_pRef = surf_pRef - rC(k)
          ENDIF
          IF ( equationOfState.NE.'UNESCO' ) THEN
             tFld(i,j) = ptLoc(kcheck)
          ELSE
             tFld(i,j) = tLoc(kcheck)
          ENDIF
          sFld(i,j)    = sLoc(kcheck)
          pFld(i,j)    = pLoc(kcheck)
          rhoLoc(i,j)  =  0. _d 0

          CALL FIND_RHO_2D(
     I           iMin, iMax, jMin, jMax, k,
     I           tFld, sFld,
     O           rhoLoc,
     I           k, bi, bj, myThid )
          rhoDif = rhoLoc(i,j) + rhoConst - rLoc(kcheck)

          IF ( equationOfState.EQ.'UNESCO' ) THEN
            CALL FIND_BULKMOD(
     I           iMin, iMax, jMin, jMax,
     I           pFld, tFld, sFld,
     O           bulkMod,
     I           myThid )
C-    Reference value
            WRITE(msgBuf,'(2(A,F4.1),A,F5.0,2A,F10.5,A,F11.5)')
     &           'rho(', sFld(i,j), ' g/kg,',
     &           tFld(i,j), ' degC,',
     &           pLoc(kcheck)*SItoBar, ' bar)',
     &           ' = ', rLoc(kcheck), ' ', bLoc(kcheck)
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                          SQUEEZE_RIGHT, myThid )
            WRITE(msgBuf,'(A,F10.5,A,F11.5,A,1PE10.3)')
     &           ' rho(find_rho_2d)                 = ',
     &           rhoLoc(i,j)+rhoConst, ' ', bulkMod(i,j), ' ', rhoDif
            iLen = LEN( blkWrd )
          ELSE
C-    Reference value
            WRITE(msgBuf,'(2(A,F4.1),A,F5.0,2A,F10.5,A,F11.5)')
     &           'rho(', sFld(i,j), ' g/kg,',
     &           tFld(i,j), ' degC,',
     &           pLoc(kcheck)*SItoBar, ' bar)',
     &           ' = ', rLoc(kcheck)
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                          SQUEEZE_RIGHT, myThid )
            WRITE(msgBuf,'(A,F10.5,A,1PE10.3)')
     &         ' rho(find_rho_2d)                 = ',
     &         rhoLoc(i,j)+rhoConst, ' ', rhoDif
            iLen = 1
          ENDIF
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )

          surf_pRef = eosRefP0
          CALL FIND_RHO_SCALAR( tFld(i,j), sLoc(kcheck),
     &         pLoc(kcheck), rhoLoc(i,j), myThid )
          rhoDif = rhoLoc(i,j) - rLoc(kcheck)
          WRITE(msgBuf,'(A,F10.5,A,1PE10.3)')
     &         ' rho(find_rho_scalar)             = ',
     &         rhoLoc(i,j), blkWrd(1:iLen), rhoDif
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )

        ENDDO
C     end check nonlinear EOS ; restore surf_pRef value
        surf_pRef = surf_pRef_save

        WRITE(msgBuf,'(A)') 'EOS_CHECK: Done'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )

      ENDIF
#endif /* INCLUDE_EOS_CHECK */

      RETURN
      END
