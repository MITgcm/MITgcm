C $Header: /u/gcmpack/MITgcm/eesupp/src/eewrite_eeenv.F,v 1.6 2001/02/04 14:38:43 cnh Exp $
C $Name:  $

#include "CPP_EEOPTIONS.h"

CStartOfInterface
      SUBROUTINE EEWRITE_EEENV
C     /==========================================================\
C     | SUBROUTINE EERWITE_EEENV                                 |
C     | o Write execution environment summary                    |
C     |==========================================================|
C     | Write a summary of the execution environment as          |
C     | configured for this run. The execution environment is    |
C     | the computational mode in which the model operatoes. It  |
C     | includes the computational grid but does not include any |
C     | model specific nuerical parameters.                      |
C     \==========================================================/
      IMPLICIT NONE

C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"

CEndOfInterface

C     === Local variables ===
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      
      WRITE(msgBuf,'(A)') 
     & '// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)

      WRITE(msgBuf,'(A)') 
     & '// Computational Grid Specification ( see files "SIZE.h" )'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)

      WRITE(msgBuf,'(A)') 
     & '//                                  ( and "eedata"       )'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)

      WRITE(msgBuf,'(A)') 
     & '// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)

      WRITE(msgBuf,'(A,I5,A)') '     nPx =',nPx,
     & ' ; /* No. processes in X */'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)

      WRITE(msgBuf,'(A,I5,A)') '     nPy =',nPy,
     & ' ; /* No. processes in Y */'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)

      WRITE(msgBuf,'(A,I5,A)') '     nSx =',nSx,
     & ' ; /* No. tiles in X per process */'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)

      WRITE(msgBuf,'(A,I5,A)') '     nSy =',nSy,
     & ' ; /* No. tiles in Y per process */'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)

      WRITE(msgBuf,'(A,I5,A)') '     sNx =',sNx,
     & ' ; /* Tile size in X */'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)

      WRITE(msgBuf,'(A,I5,A)') '     sNy =',sNy,
     & ' ; /* Tile size in Y */'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)

      WRITE(msgBuf,'(A,I5,A)') '     OLx =',OLx,
     & ' ; /* Tile overlap distance in X */'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)

      WRITE(msgBuf,'(A,I5,A)') '     OLy =',OLy,
     & ' ; /* Tile overlap distance in Y */'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)

      WRITE(msgBuf,'(A,I5,A)') '     nTx =',nTx,
     & ' ; /* No. threads in X per process */'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)

      WRITE(msgBuf,'(A,I5,A)') '     nTy =',nTy,
     & ' ; /* No. threads in Y per process */'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)

      WRITE(msgBuf,'(A,I5,A)') '      Nr =', Nr,
     & ' ; /* No. levels in the vertical   */ '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)

      WRITE(msgBuf,'(A,I5,A)') '      nX =', nX,
     & ' ; /* Total domain size in X ( = nPx*nSx*sNx ) */'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)

      WRITE(msgBuf,'(A,I5,A)') '      nY =', nY,
     & ' ; /* Total domain size in Y ( = nPy*nSy*sNy ) */'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)

      WRITE(msgBuf,'(A,I5,A)') '  nTiles =', nSx*nSy,
     & ' ; /* Total no. tiles per process ( = nSx*nSy ) */'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)

      WRITE(msgBuf,'(A,I5,A)') '  nProcs =', nPx*nPy,
     & ' ; /* Total no. processes ( = nPx*nPy ) */'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)

      WRITE(msgBuf,'(A,I5,A)') 'nThreads =', nTx*nTy,
     & ' ; /* Total no. threads per process ( = nTx*nTy ) */'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)

      WRITE(msgBuf,'(A,L5,A)') 'usingMPI =', usingMPI,
     & ' ; /* Flag used to control whether MPI is in use */'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)

      WRITE(msgBuf,'(A,A,A)') '          ', '     ' ,
     & '   /*  note: To execute a program with MPI calls */'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)

      WRITE(msgBuf,'(A,A,A)') '          ', '     ' ,
     & '   /*  it must be launched appropriately e.g     */'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)

      WRITE(msgBuf,'(A,A,A)') '          ', '     ' ,
     & '   /*  "mpirun -np 64 ......"                    */'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)

      WRITE(msgBuf,'(A)') '                '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)

C
      RETURN
      END
