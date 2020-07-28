#include "CPP_EEOPTIONS.h"
#include "W2_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: W2_SET_CS6_FACETS( myThid )

C !INTERFACE:
      SUBROUTINE W2_SET_CS6_FACETS( myThid )

C     !DESCRIPTION:
C     Set-up multi facets(=sub-domains) topology : 6 facets Cube case
C     Facet Dimension taken from the 1rst 3 facet_dims (nRed, nGreen, nBlue)
C     if provided in "data.exch2"; if not, assume regular Cube (equal size)
C     and derive single dimension from "SIZE.h".

C     !USES:
      IMPLICIT NONE

C      Tile topology settings data structures
#include "SIZE.h"
#include "EEPARAMS.h"
#include "W2_EXCH2_SIZE.h"
#include "W2_EXCH2_PARAMS.h"
#include "W2_EXCH2_TOPOLOGY.h"

C     !INPUT PARAMETERS:
C     myThid  :: my Thread Id number
C               (Note: not relevant since threading has not yet started)
      INTEGER myThid

C     !LOCAL VARIABLES:
C     === Local variables ===
C     msgBuf     :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*1 edge(4)
      INTEGER i, j, ii, jj, lo, ll
      INTEGER nRd, nGr, nBl
      INTEGER setDims, addDims
      LOGICAL prtFlag
      Real*4  tmpVar
CEOP
      DATA edge / 'N' , 'S' , 'E' , 'W' /

      WRITE(msgBuf,'(2A,I3,A)') 'W2_SET_CS6_FACETS:',
     &              ' preDefTopol=', preDefTopol, ' selected'
      CALL PRINT_MESSAGE( msgBuf, W2_oUnit,SQUEEZE_RIGHT,myThid )
      prtFlag = ABS(W2_printMsg).GE.2
     &       .OR. ( W2_printMsg .NE.0 .AND. myProcId.EQ.0 )

C--   Number of facets:
      nFacets = 6
      IF ( nfacets.GT.W2_maxNbFacets ) THEN
       CALL ALL_PROC_DIE( 0 )
       STOP 'ABNORMAL END: S/R W2_SET_CS6_FACETS (nFacets>maxNbFacets)'
      ENDIF

C--   Facet Edge connections ( edges order: N,S,E,W <==> 1,2,3,4 )
      DO j=1,nFacets
       IF ( MOD(j,2).EQ.1 ) THEN
         jj = j+2
         facet_link(1,j) = 0.4 + FLOAT( 1+MOD(jj+5,6) )
         jj = j-1
         facet_link(2,j) = 0.1 + FLOAT( 1+MOD(jj+5,6) )
         jj = j+1
         facet_link(3,j) = 0.4 + FLOAT( 1+MOD(jj+5,6) )
         jj = j-2
         facet_link(4,j) = 0.1 + FLOAT( 1+MOD(jj+5,6) )
       ELSE
         jj = j+1
         facet_link(1,j) = 0.2 + FLOAT( 1+MOD(jj+5,6) )
         jj = j-2
         facet_link(2,j) = 0.3 + FLOAT( 1+MOD(jj+5,6) )
         jj = j+2
         facet_link(3,j) = 0.2 + FLOAT( 1+MOD(jj+5,6) )
         jj = j-1
         facet_link(4,j) = 0.3 + FLOAT( 1+MOD(jj+5,6) )
       ENDIF
      ENDDO

C--   facet dimension: take the 1rst 3 numbers from facet_dims
      nRd = facet_dims(1)
      nGr = facet_dims(2)
      nBl = facet_dims(3)
      DO j=4,W2_maxNbFacets*2
        IF ( facet_dims(j).NE.0 ) THEN
         WRITE(msgBuf,'(2A,I5)') 'W2_SET_CS6_FACETS:',
     &     ' no more than 3 dims (nRd,nGr,nBl) expected for CS-6 Topol'
         CALL PRINT_MESSAGE( msgBuf, W2_oUnit,SQUEEZE_RIGHT,myThid )
         CALL PRINT_ERROR( msgBuf, myThid )
         CALL ALL_PROC_DIE( 0 )
         STOP 'ABNORMAL END: S/R W2_SET_CS6_FACETS: allows 3 dims only'
        ENDIF
      ENDDO
      IF ( nRd.GT.0 .AND. nGr+nBl.EQ.0 ) THEN
C-    Only 1rst dim is set: assuming a regular Cube
        nGr = nRd
        nBl = nRd
      ELSEIF ( nRd+nGr+nBl.EQ.0 ) THEN
C-    try to get cube size from number of tiles, assuming a regular Cube
        nGr = exch2_nTiles*sNx*sNy
        tmpVar = FLOAT(nGr)/6.
        tmpVar = SQRT(tmpVar)
        nRd = NINT(tmpVar)
        IF ( nRd*nRd*6 .EQ. nGr ) THEN
          nGr = nRd
          nBl = nRd
          WRITE(msgBuf,'(2A,I5)') 'W2_SET_CS6_FACETS:',
     &     ' facet-dims Unset; assume nRd=nGr=nBl=', nRd
          CALL PRINT_MESSAGE( msgBuf, W2_oUnit,SQUEEZE_RIGHT,myThid )
        ELSE
          WRITE(msgBuf,'(A,I8,2(A,I4),A,I10,A,I6,A)')
     &     ' nTiles*sNx*sNy=', exch2_nTiles,' x',sNx,' x',sNy,' =',nGr
          CALL PRINT_MESSAGE( msgBuf, W2_oUnit,SQUEEZE_RIGHT,myThid )
          WRITE(msgBuf,'(A,I6,A,I10)')
     &     '       not equal to: 6 x',nRd,'^2 =', nRd*nRd*6
          CALL PRINT_MESSAGE( msgBuf, W2_oUnit,SQUEEZE_RIGHT,myThid )
          WRITE(msgBuf,'(2A,I5)') 'W2_SET_CS6_FACETS:',
     &     ' facet-dims Unset; attempt to fit single dim FAIL'
          CALL PRINT_MESSAGE( msgBuf, W2_oUnit,SQUEEZE_RIGHT,myThid )
          CALL PRINT_ERROR( msgBuf, myThid )
        ENDIF
      ENDIF
      IF ( nRd*nGr*nBl.EQ.0 ) THEN
       CALL ALL_PROC_DIE( 0 )
       STOP 'ABNORMAL END: S/R W2_SET_CS6_FACETS (Dims are missing)'
      ENDIF

C--   Set facet dimension : 1rst 3 are known:
      facet_dims(1) = nRd
      facet_dims(2) = nGr
      facet_dims(3) = nBl
C-    Derive the other using from connection graph (topology):
      setDims = 3
      addDims = 1
      DO WHILE ( addDims.GT.0 )
        addDims = 0
        DO j=2,nFacets
         DO i=1,4
C-    connected to:
          jj = INT(facet_link(i,j))
          ii = MOD( NINT(facet_link(i,j)*10.), 10 )
          IF ( jj.GE.1 .AND. jj.LE.nFacets
     &         .AND.  ii.GE.1 .AND. ii.LE.4 ) THEN
C-    Length of N or S Edge = x-size, E or W Edge = y-size
           lo = 2*(j-1) + (i+1)/2
C-    Corresponding Edge length
           ll = 2*(jj-1)+(ii+1)/2
           IF ( facet_dims(lo).EQ.0 .AND. facet_dims(ll).GT.0 ) THEN
             addDims = addDims + 1
             facet_dims(lo) = facet_dims(ll)
             IF ( prtFlag ) THEN
              WRITE(msgBuf,'(A,I3,3A,2(I4,A),I3,3A,I8)')
     &         ' facet',j,'.',edge(i), ' set dim', lo, ' = dim', ll,
     &         ' from',jj,'.',edge(ii),' :',facet_dims(ll)
              CALL PRINT_MESSAGE(msgBuf,W2_oUnit,SQUEEZE_RIGHT,myThid)
             ENDIF
           ENDIF
          ENDIF
         ENDDO
        ENDDO
        setDims = setDims + addDims
      ENDDO

      IF ( setDims.NE.nFacets*2 ) THEN
        WRITE(msgBuf,'(A,I3,A)') ' W2_SET_CS6_FACETS:',
     &     nFacets*2-setDims, ' facet-dims left Unset'
        CALL PRINT_MESSAGE( msgBuf, W2_oUnit, SQUEEZE_RIGHT, myThid )
        CALL PRINT_ERROR( msgBuf, myThid )
        DO j=1,nFacets
         IF ( facet_dims(2*j-1)*facet_dims(2*j).EQ.0 ) THEN
          WRITE(W2_oUnit,'(A,I3,2(A,I8))')
     &        ' facets #', j, ' , x-size=', facet_dims(2*j-1),
     &                        ' , y-size=', facet_dims(2*j)
          CALL PRINT_MESSAGE( msgBuf, W2_oUnit,SQUEEZE_RIGHT,myThid )
          CALL PRINT_ERROR( msgBuf, myThid )
         ENDIF
        ENDDO
        CALL ALL_PROC_DIE( 0 )
        STOP 'ABNORMAL END: S/R W2_SET_CS6_FACETS (unset facet dims)'
      ENDIF

      RETURN
      END
