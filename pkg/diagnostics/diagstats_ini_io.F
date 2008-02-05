C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagstats_ini_io.F,v 1.4 2008/02/05 15:31:19 jmc Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

CBOP
C     !ROUTINE: DIAGSTATS_INI_IO
C     !INTERFACE:
      SUBROUTINE DIAGSTATS_INI_IO( myThid )

C     !DESCRIPTION: \bv
C     *==================================================================
C     | S/R DIAGSTATS_INI_IO
C     | o set I/O unit for ASCII output file
C     *==================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "PARAMS.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER m, n, j, iL, nUnit
      CHARACTER*(MAX_LEN_FNAM) dataFName
      CHARACTER*(MAX_LEN_MBUF) msgBuf, tmpBuf
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

      _BEGIN_MASTER( myThid)

#ifdef ALLOW_USE_MPI
      IF ( diagSt_Ascii .AND. mpiMyId.EQ.0 ) THEN
#else
      IF ( diagSt_Ascii ) THEN
#endif

        DO n=1,diagSt_nbLists

C-      get a free unit number as the I/O channel for this routine
          CALL MDSFINDUNIT( nUnit, myThid )
          diagSt_ioUnit(n) = nUnit

          iL = ILNBLNK(diagSt_Fname(n))
          WRITE(dataFName,'(2A,I10.10,A)')
     &          diagSt_Fname(n)(1:iL), '.', nIter0, '.txt'
          OPEN( nUnit, FILE=dataFName, STATUS='unknown' )

          WRITE(msgBuf,'(4A,I6)') 'DIAGSTATS_INI_IO: ',
     &         'open file: ',dataFName(1:iL+15), ' , unit=', nUnit
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT , myThid)

C-      write a kind of header:
          WRITE(nUnit,'(2A)')      '# header of file: ',
     &                               diagSt_Fname(n)(1:iL)
          WRITE(nUnit,'(A,F17.6)') '# frequency (s): ', diagSt_freq(n)
          WRITE(nUnit,'(A,F17.6)') '# phase (s)    : ', diagSt_phase(n)
          WRITE(msgBuf,'(A)')      '# Regions      : '
          iL = 17
          DO j=0,nRegions
            IF (diagSt_region(j,n).GE.1 .AND.iL+3.LE.MAX_LEN_MBUF) THEN
              tmpBuf(1:iL) = msgBuf(1:iL)
              WRITE(msgBuf,'(A,I3)') tmpBuf(1:iL),j
              iL = iL+3
            ENDIF
          ENDDO
          WRITE(nUnit,'(A)') msgBuf(1:iL)
          DO j=1,diagSt_nbFlds(n),10
            WRITE(nUnit,'(A,20A)')      '# Fields       :',
     &        (' ', diagSt_Flds(m,n), m=j,MIN(diagSt_nbFlds(n),j+9) )
          ENDDO
          DO j=1,diagSt_nbFlds(n),50
            WRITE(nUnit,'(A,50I4)')   '# Nb of levels : ',
     &         ( kdiag(jSdiag(m,n)), m=j,MIN(diagSt_nbFlds(n),j+49) )
          ENDDO
          WRITE(nUnit,'(2A)') '# end of header ----------------------',
     &                        '--------------------------------------'
          WRITE(nUnit,'(A)') ' '

        ENDDO

      ENDIF

      _END_MASTER( myThid )

      RETURN
      END
