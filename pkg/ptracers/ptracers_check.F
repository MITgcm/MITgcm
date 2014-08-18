C $Header: /u/gcmpack/MITgcm/pkg/ptracers/ptracers_check.F,v 1.4 2014/08/18 14:34:27 jmc Exp $
C $Name:  $

#include "PTRACERS_OPTIONS.h"
c#include "GAD_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: PTRACERS_CHECK

C     !INTERFACE:
      SUBROUTINE PTRACERS_CHECK( myThid )

C     !DESCRIPTION:
C     Print summary of PTRACERS setting and Check dependances
C     with main model and other packages

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#include "GAD.h"

C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

#ifdef ALLOW_PTRACERS
C     !LOCAL VARIABLES:
C     iTracer    :: tracer index
C     iUnit      :: unit number for I/O
C     msgBuf     :: Informational/error message buffer
      INTEGER iTracer
      INTEGER iUnit
      CHARACTER*(MAX_LEN_MBUF) msgBuf

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _BEGIN_MASTER(myThid)

      WRITE(msgBuf,'(A)') 'PTRACERS_CHECK: #define ALLOW_PTRACERS'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

C--   Print a summary of pTracer parameter values:
      iUnit = standardMessageUnit
      WRITE(msgBuf,'(A)') '// ==================================='
      CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT , myThid )
      WRITE(msgBuf,'(A)') '// PTRACERS parameters '
      CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT , myThid )
      WRITE(msgBuf,'(A)') '// ==================================='
      CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT , myThid )
      CALL WRITE_0D_I( PTRACERS_numInUse, INDEX_NONE,
     &   'PTRACERS_numInUse =',
     &   ' /* number of tracers */')
      CALL WRITE_0D_I( PTRACERS_Iter0, INDEX_NONE,
     &   'PTRACERS_Iter0 =',
     &   ' /* timestep number when tracers are initialized */')
      CALL WRITE_0D_L( PTRACERS_startAllTrc, INDEX_NONE,
     &   'PTRACERS_startAllTrc =','/* all tracers start @ startTime */')
      CALL WRITE_0D_L( PTRACERS_doAB_onGpTr, INDEX_NONE,
     &   'PTRACERS_doAB_onGpTr =',
     &   '/* apply AB on Tendencies (rather than on Tracers) */')
      CALL WRITE_0D_L( PTRACERS_addSrelax2EmP, INDEX_NONE,
     &   'PTRACERS_addSrelax2EmP =','/* add Salt relaxation to EmP */')
      CALL WRITE_1D_RL( PTRACERS_dTLev, Nr, INDEX_K,
     &   'PTRACERS_dTLev =',
     &'   /* Ptracer timestep ( s ) */')
      CALL WRITE_0D_RL(PTRACERS_monitorFreq, INDEX_NONE,
     &   'PTRACERS_monitorFreq =',
     &   ' /* Frequency^-1 for monitor output (s) */')
      CALL WRITE_0D_RL(PTRACERS_dumpFreq, INDEX_NONE,
     &   'PTRACERS_dumpFreq =',
     &   ' /* Frequency^-1 for snapshot output (s) */')
      CALL WRITE_0D_RL(PTRACERS_taveFreq, INDEX_NONE,
     &   'PTRACERS_taveFreq =',
     &   ' /* Frequency^-1 for time-Aver. output (s) */')
      CALL WRITE_0D_L( PTRACERS_useRecords, INDEX_NONE,
     &   'PTRACERS_useRecords =', ' /* all tracers in 1 file */')

      CALL WRITE_0D_L( PTRACERS_timeave_mnc, INDEX_NONE,
     &     'PTRACERS_timeave_mnc =',
     &     ' /* use MNC for Tave output */')
      CALL WRITE_0D_L( PTRACERS_snapshot_mnc, INDEX_NONE,
     &     'PTRACERS_snapshot_mnc =',
     &     ' /* use MNC for snapshot output */')
      CALL WRITE_0D_L( PTRACERS_pickup_write_mnc, INDEX_NONE,
     &     'PTRACERS_pickup_write_mnc =',
     &     ' /* use MNC for writing pickups */')
      CALL WRITE_0D_L( PTRACERS_pickup_read_mnc, INDEX_NONE,
     &     'PTRACERS_pickup_read_mnc =',
     &     ' /* use MNC for reading pickups */')

      DO iTracer=1,PTRACERS_numInUse
        WRITE(msgBuf,'(A)') ' -----------------------------------'
        CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(A,I4)') ' tracer number : ',iTracer
        CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT, myThid )
        CALL WRITE_0D_C( PTRACERS_names(iTracer), -1, INDEX_NONE,
     &     'PTRACERS_names =', ' /* Tracer short name */')
        CALL WRITE_0D_C( PTRACERS_long_names(iTracer), -1, INDEX_NONE,
     &     'PTRACERS_long_names =', ' /* Tracer long name */')
        CALL WRITE_0D_C( PTRACERS_ioLabel(iTracer), 0, INDEX_NONE,
     &     'PTRACERS_ioLabel =', ' /* tracer IO Label */')
        IF ( .NOT.PTRACERS_startAllTrc )
     &  CALL WRITE_0D_RL( PTRACERS_startStepFwd(iTracer), INDEX_NONE,
     &     'PTRACERS_startStepFwd =', ' /* tracer starting time */')
        CALL WRITE_0D_I( PTRACERS_advScheme(iTracer), INDEX_NONE,
     &     'PTRACERS_advScheme =', ' /* Advection Scheme */')
        CALL WRITE_0D_L( PTRACERS_SOM_Advection(iTracer), INDEX_NONE,
     &     'PTRACERS_SOM_Advection =',
     &     ' /* tracer uses SOM advection scheme */')
        CALL WRITE_0D_L( PTRACERS_ImplVertAdv(iTracer), INDEX_NONE,
     &     'PTRACERS_ImplVertAdv =',
     &     ' /* implicit vert. advection flag */')
        CALL WRITE_0D_L( PTRACERS_MultiDimAdv(iTracer), INDEX_NONE,
     &     'PTRACERS_MultiDimAdv =',
     &     ' /* tracer uses Multi-Dim advection */')
        CALL WRITE_0D_L( PTRACERS_AdamsBashGtr(iTracer), INDEX_NONE,
     &     'PTRACERS_AdamsBashGtr =',
     &     ' /* apply AB on tracer tendency */')
        CALL WRITE_0D_L( PTRACERS_AdamsBash_Tr(iTracer), INDEX_NONE,
     &     'PTRACERS_AdamsBash_Tr =',
     &     ' /* apply AB on passive tracer */')
        CALL WRITE_0D_RL( PTRACERS_diffKh(iTracer), INDEX_NONE,
     &     'PTRACERS_diffKh =', ' /* Laplacian Diffusivity */')
        CALL WRITE_0D_RL( PTRACERS_diffK4(iTracer), INDEX_NONE,
     &     'PTRACERS_diffK4 =', ' /* Biharmonic Diffusivity */')
        CALL WRITE_1D_RL( PTRACERS_diffKrNr(1,iTracer), Nr, INDEX_K,
     &     'PTRACERS_diffKrNr =', ' /* Vertical Diffusivity */')
        CALL WRITE_0D_L( PTRACERS_useGMRedi(iTracer), INDEX_NONE,
     &     'PTRACERS_useGMRedi =', ' /* apply GM-Redi */')
        CALL WRITE_0D_L( PTRACERS_useDWNSLP(iTracer), INDEX_NONE,
     &     'PTRACERS_useDWNSLP =', ' /* apply DOWN-SLOPE Flow */')
        CALL WRITE_0D_L( PTRACERS_useKPP(iTracer), INDEX_NONE,
     &     'PTRACERS_useKPP =', ' /* apply KPP scheme */')
        CALL WRITE_1D_RL( PTRACERS_ref(1,iTracer), Nr, INDEX_K,
     &     'PTRACERS_ref =', ' /* Reference vertical profile */')
        CALL WRITE_0D_RL( PTRACERS_EvPrRn(iTracer), INDEX_NONE,
     &     'PTRACERS_EvPrRn =', '/* tracer conc. in Evap. & Rain */')

      ENDDO
      WRITE(msgBuf,'(A)') ' -----------------------------------'
      CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT, myThid )

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( PTRACERS_Iter0.GE.nEndIter ) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** PTRACERS_CHECK:',
     &       ' PTRACERS_Iter0 beyond run-end (=nEndIter)'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(2A)') '** WARNING ** PTRACERS_CHECK:',
     &       ' ==> do not load initial conditions nor pickup file'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
      ELSEIF ( PTRACERS_Iter0.GT.nIter0 ) THEN
        WRITE(msgBuf,'(2A,I3,A)') 'PTRACERS_CHECK:',
     &       ' wrong setting of PTRACERS_Iter0 :'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A,I3,A)') 'PTRACERS_CHECK:',
     &       ' nIter0 < PTRACERS_Iter0 < nEndIter not supported'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R PTRACERS_CHECK'
      ENDIF

#ifndef INCLUDE_IMPLVERTADV_CODE
      DO iTracer=1,PTRACERS_numInUse
       IF ( PTRACERS_ImplVertAdv(iTracer) ) THEN
        WRITE(msgBuf,'(A)')
     &   'PTRACERS_CHECK: #undef INCLUDE_IMPLVERTADV_CODE'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A,I3,A)') 'PTRACERS_CHECK:',
     &   ' but pTracers_ImplVertAdv(',iTracer,' ) is TRUE'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R PTRACERS_CHECK'
       ENDIF
      ENDDO
      IF ( PTRACERS_dTLev(1).NE.PTRACERS_dTLev(Nr)
     &     .AND. implicitDiffusion ) THEN
        WRITE(msgBuf,'(A)')
     &   'PTRACERS_CHECK: #undef INCLUDE_IMPLVERTADV_CODE'
        CALL PRINT_ERROR( msgBuf , myThid)
        WRITE(msgBuf,'(2A)') 'PTRACERS_CHECK: ',
     &   'but implicitDiffusion=T with non-uniform PTRACERS_dTLev'
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R PTRACERS_CHECK'
      ENDIF
#endif
      DO iTracer=1,PTRACERS_numInUse
        IF ( PTRACERS_useGMRedi(iTracer) .AND. .NOT.useGMRedi ) THEN
          WRITE(msgBuf,'(2A,I3,A)') 'PTRACERS_CHECK:',
     &     ' pTracers_useGMRedi(',iTracer,' ) is TRUE'
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(A,L5,A)')
     &     'PTRACERS_CHECK: But not useGMRedi (=',useGMRedi,')'
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R PTRACERS_CHECK'
        ENDIF
        IF ( PTRACERS_useDWNSLP(iTracer) .AND. .NOT.useDOWN_SLOPE ) THEN
          WRITE(msgBuf,'(2A,I3,A)') 'PTRACERS_CHECK:',
     &     ' pTracers_useDWNSLP(',iTracer,' ) is TRUE'
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(2A,L5,A)') 'PTRACERS_CHECK:',
     &     ' But not useDOWN_SLOPE (=', useDOWN_SLOPE, ')'
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R PTRACERS_CHECK'
        ENDIF
        IF ( PTRACERS_useKPP(iTracer) .AND. .NOT.useKPP ) THEN
          WRITE(msgBuf,'(2A,I3,A)') 'PTRACERS_CHECK:',
     &     ' pTracers_useKPP(',iTracer,' ) is TRUE'
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(A,L5,A)')
     &     'PTRACERS_CHECK: But not useKPP (=',useKPP,')'
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R PTRACERS_CHECK'
        ENDIF
      ENDDO

      _END_MASTER(myThid)
C     Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* ALLOW_PTRACERS */

      RETURN
      END
