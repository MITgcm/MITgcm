#include "GCHEM_OPTIONS.h"

CBOP
C !ROUTINE: GCHEM_READPARMS

C !INTERFACE: ==========================================================
      SUBROUTINE GCHEM_READPARMS( myThid )

C !DESCRIPTION:
C     Initialize GCHEM parameters, read in data.gchem

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GCHEM.h"
#include "GCHEM_VARS.h"

C !INPUT PARAMETERS: ===================================================
C  myThid         :: thread number
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  none

#ifdef ALLOW_GCHEM

C !FUNCTIONS
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C !LOCAL VARIABLES: ====================================================
C  tIter0         :: retired parameter
C  iUnit          :: unit number for I/O
C  msgBuf         :: message buffer
      INTEGER tIter0
      INTEGER iUnit
      INTEGER iL
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

C  selectBTconst :: estimates borate concentration from salinity:
C     =1 :: use default formulation of Uppström (1974)(same as S/R CARBON_COEFFS)
C     =2 :: use new formulation from Lee et al (2010)
C  selectFTconst :: estimates fluoride concentration from salinity:
C     =1 :: use default formulation of Riley (1965) (same as S/R CARBON_COEFFS)
C     =2 :: use new formulation from Culkin (1965)
C  selectHFconst :: sets the first dissociation constant for hydrogen fluoride:
C     =1 :: use default  Dickson and Riley (1979) (same as S/R CARBON_COEFFS)
C     =2 :: use new formulation of Perez and Fraga (1987)
C  selectK1K2const :: sets the 1rst & 2nd dissociation constants of carbonic acid:
C     =1 :: use default formulation of Millero (1995) with data
C            from Mehrbach et al. (1973) (same as S/R CARBON_COEFFS)
C     =2 :: use formulation of Roy et al. (1993)
C     =3 :: use "combination" formulation of Millero (1995)
C     =4 :: use formulation of Luecker et al. (2000)
C     =5 :: use formulation of Millero (2010, Mar. Fresh Wat. Res.)
C     =6 :: use formulation of Waters, Millero, Woosley (2014, Mar. Chem.)
C  selectPHsolver :: sets the pH solver to use:
C     =0 :: use Follows et al., (2006) solver;
C     =1 :: use the GENERAL solver from Munhoven (2013);
C     =2 :: use SEC solver  from Munhoven (2013);
C     =3 :: use FAST solver from Munhoven (2013);
C- Sub-package on/off flags: not fully implemented, requires
C  to test the flag before any corresponding pkg S/R call

      NAMELIST /GCHEM_PARM01/
     &                   useCFC,
     &                   useDIC,
     &                   useBLING,
     &                   useSPOIL,
     &                   useDARWIN,
     &           fileName1, fileName2, fileName3,
     &           fileName4, fileName5,
     &           gchem_int1, gchem_int2, gchem_int3,
     &           gchem_int4, gchem_int5, nsubtime,
     &           gchem_rl1, gchem_rl2, gchem_rl3,
     &           gchem_rl4, gchem_rl5,
     &           gchem_ForcingPeriod, gchem_ForcingCycle,
     &           selectBTconst, selectFTconst, selectHFconst,
     &           selectK1K2const, selectPHsolver, selectModernCoeffs,
     &           tIter0

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( .NOT.useGCHEM ) THEN
C-    pkg GCHEM is not used
        _BEGIN_MASTER(myThid)
C-    Track pkg activation status:
C     print a (weak) warning if data.gchem is found
         CALL PACKAGES_UNUSED_MSG( 'useGCHEM', ' ', ' ' )
        _END_MASTER(myThid)
        RETURN
      ENDIF

      _BEGIN_MASTER(myThid)

C- Set defaults values for parameters in GCHEM.h
       useCFC = .FALSE.
       useDIC = .FALSE.
       useBLING  = .FALSE.
       useSPOIL  = .FALSE.
       useDARWIN = .FALSE.
       tIter0 = UNSET_I
       fileName1=' '
       fileName2=' '
       fileName3=' '
       fileName4=' '
       fileName5=' '
       nsubtime = 1
       gchem_int1=0
       gchem_int2=0
       gchem_int3=0
       gchem_int4=0
       gchem_int5=0
       gchem_rl1=0. _d 0
       gchem_rl2=0. _d 0
       gchem_rl3=0. _d 0
       gchem_rl4=0. _d 0
       gchem_rl5=0. _d 0
C  carbon parameters
       selectBTconst   = UNSET_I
       selectFTconst   = UNSET_I
       selectHFconst   = UNSET_I
       selectK1K2const = UNSET_I
       selectPHsolver  = UNSET_I
       selectModernCoeffs = UNSET_I
C  default periodic forcing to same as for physics
       gchem_ForcingPeriod=externForcingPeriod
       gchem_ForcingCycle=externForcingCycle
C- Open and read the data.gchem file
      WRITE(msgBuf,'(A)') ' GCHEM_READPARMS: opening data.gchem'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      CALL OPEN_COPY_DATA_FILE(
     I                   'data.gchem', 'GCHEM_PARM01',
     O                   iUnit,
     I                   myThid )
      READ(UNIT=iUnit,NML=GCHEM_PARM01)
      WRITE(msgBuf,'(A)')
     &  ' GCHEM_READPARMS: finished reading data.gchem'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

C- Close the open data file
#ifdef SINGLE_DISK_IO
      CLOSE(iUnit)
#else
      CLOSE(iUnit,STATUS='DELETE')
#endif /* SINGLE_DISK_IO */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C-    derive other parameters:

C-    set parameter default values
      IF ( selectBTconst  .EQ.UNSET_I ) selectBTconst   = 1
      IF ( selectFTconst  .EQ.UNSET_I ) selectFTconst   = 1
      IF ( selectHFconst  .EQ.UNSET_I ) selectHFconst   = 1
      IF ( selectK1K2const.EQ.UNSET_I ) selectK1K2const = 1
      IF ( selectPHsolver .EQ.UNSET_I ) selectPHsolver  = 0
      IF ( selectModernCoeffs.EQ.UNSET_I ) THEN
       IF ( selectPHsolver .EQ. 0 ) THEN
        selectModernCoeffs = 0
       ELSE
        selectModernCoeffs = 1
       ENDIF
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Print out parameter values:

      iUnit = standardMessageUnit

      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
      WRITE(msgBuf,'(A)') '// ==================================='
      CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
      WRITE(msgBuf,'(A)') '// GCHEM package parameters :'
      CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
      WRITE(msgBuf,'(A)') '// ==================================='
      CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)

C Record the solver to calculate pH and evaluate surface ocean pCO2
      IF ( selectPHsolver.GT.0 ) THEN
         WRITE(msgBuf,'(A)')
     &  'Using Munhoven (2013) Solvesaphe for pH/pCO2'
         CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
      ELSEIF ( selectPHsolver.EQ.0 ) THEN
         WRITE(msgBuf,'(A)')
     &  'Using Follows et al. (2006) for pH/pCO2'
         CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
      ENDIF

C Record which carbonate coefficients are used and which pH/pCO2 solver
      IF ( selectModernCoeffs.GT.0 ) THEN
       WRITE(msgBuf,'(A)')
     &  'Using Munhoven (2013) Solvesaphe carbon coefficients'
       CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
C Munhoven (2013) "Solvesaphe" coefficients have several options:
       IF ( selectK1K2const.EQ.1 ) THEN
         WRITE(msgBuf,'(A)') 'Millero (1995)/Mehrbach'
       ELSEIF ( selectK1K2const.EQ.2 ) THEN
         WRITE(msgBuf,'(A)') 'Roy et al. (1993)'
       ELSEIF ( selectK1K2const.EQ.3 ) THEN
         WRITE(msgBuf,'(A)') 'Millero (1995)'
       ELSEIF ( selectK1K2const.EQ.4 ) THEN
         WRITE(msgBuf,'(A)') 'Luecker et al. (2000)'
       ELSEIF ( selectK1K2const.EQ.5 ) THEN
         WRITE(msgBuf,'(A)') 'Millero et al. (2010)'
       ELSEIF ( selectK1K2const.EQ.6 ) THEN
         WRITE(msgBuf,'(A)') 'Waters et al. (2014)'
       ENDIF
       iL = ILNBLNK(msgBuf)
       WRITE(msgBuf,'(A)')
     &      'Using '//msgBuf(1:iL)//' K1 and K2 coefficients'
       CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)

       IF ( selectHFconst.EQ.1 ) THEN
         WRITE(msgBuf,'(A)')
     &  'Using Dickson and Riley (1979) KF coefficient'
       ELSEIF ( selectHFconst.EQ.2 ) THEN
         WRITE(msgBuf,'(A)')
     &  'Using Perez and Fraga (1987) KF coefficient'
       ENDIF
       CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)

       IF ( selectBTconst.EQ.1 ) THEN
         WRITE(msgBuf,'(A)')
     & 'Using Uppstrom (1974) BT estimation from salinity'
       ELSEIF ( selectBTconst.EQ.2 ) THEN
         WRITE(msgBuf,'(A)')
     &  'Using Lee et al (2010) BT estimation from salinity'
       ENDIF
       CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)

       IF ( selectFTconst.EQ.1 ) THEN
         WRITE(msgBuf,'(A)')
     &  'Using Riley (1965) FT estimation from salinity'
         CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
       ELSEIF ( selectBTconst.EQ.2 ) THEN
         WRITE(msgBuf,'(A)')
     &  'Using Culkin (1965) FT estimation from salinity'
         CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
       ENDIF
      ELSE
C OCMIP2 Coefficients from S/R CARBON_COEFFS in CARBON_CHEM.F
       WRITE(msgBuf,'(A)')
     &  'Using Millero (1995)/Mehrbach K1 and K2 coefficients'
       CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
       WRITE(msgBuf,'(A)')
     &  'Using Dickson and Riley (1979) KF coefficient'
       CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
       WRITE(msgBuf,'(A)')
     &  'Using Uppstrom (1974) BT estimation from salinity'
       CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
       WRITE(msgBuf,'(A)')
     &  'Using Riley (1965) FT estimation from salinity'
       CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
      ENDIF

C- Check for retired parameters:
      IF ( tIter0 .NE. UNSET_I ) THEN
c      nRetired = nRetired+1
       WRITE(msgBuf,'(A,A)')
     &  'S/R GCHEM_READPARMS: Paramater "tIter0" is',
     &  ' no longer allowed in file "data.gchem"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)')
     &  'S/R GCHEM_READPARMS: "tIter0" has been moved to',
     &  ' PTRACERS_Iter0 in file "data.ptracers".'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R GCHEM_READPARMS'
      ENDIF

      _END_MASTER(myThid)

C  Everyone else must wait for the parameters to be loaded
      _BARRIER

#ifdef ALLOW_CFC
      IF ( useCFC ) THEN
        CALL CFC_READPARMS(myThid)
      ENDIF
#endif

#ifdef ALLOW_DIC
      IF ( useDIC ) THEN
        CALL DIC_READPARMS(myThid)
      ENDIF
#endif

#ifdef ALLOW_BLING
      IF ( useBLING ) THEN
        CALL BLING_READPARMS(myThid)
      ENDIF
#endif

#ifdef ALLOW_SPOIL
      IF ( useSPOIL ) THEN
        CALL SPOIL_READPARMS(myThid)
      ENDIF
#endif

#ifdef ALLOW_DARWIN
      IF ( useDARWIN ) THEN
        CALL DARWIN_READPARMS(myThid)
      ENDIF
#endif

C- Register GCHEM tracer indices
      CALL GCHEM_TR_REGISTER( myThid )

#endif /* ALLOW_GCHEM */

      RETURN
      END
