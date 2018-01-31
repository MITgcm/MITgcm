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
#include "PARAMS.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid   :: my Thread Id number
      INTEGER myThid

C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      INTEGER m, n, j, iL, nUnit
      CHARACTER*(10) suff
      CHARACTER*(MAX_LEN_FNAM) dataFName
      CHARACTER*(MAX_LEN_MBUF) msgBuf, tmpBuf
CEOP

      _BEGIN_MASTER( myThid)

      IF ( diagSt_Ascii .AND. myProcId.EQ.0 ) THEN

        DO n=1,diagSt_nbLists

C-      get a free unit number as the I/O channel for this routine
          CALL MDSFINDUNIT( nUnit, myThid )
          diagSt_ioUnit(n) = nUnit

C-      set file name
          IF ( rwSuffixType.EQ.0 ) THEN
            WRITE(suff,'(I10.10)') nIter0
          ELSE
            CALL RW_GET_SUFFIX( suff, startTime, nIter0, myThid )
          ENDIF
          iL = ILNBLNK(diagSt_Fname(n))
          WRITE(dataFName,'(4A)')
     &          diagSt_Fname(n)(1:iL), '.', suff, '.txt'

C-      open file with corresponding file unit
          OPEN( nUnit, FILE=dataFName, STATUS='unknown' )

          WRITE(msgBuf,'(4A,I6)') 'DIAGSTATS_INI_IO: ',
     &         'open file: ',dataFName(1:iL+15), ' , unit=', nUnit
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )

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
