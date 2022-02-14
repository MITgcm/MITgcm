#include "CPP_EEOPTIONS.h"

CBOP
C     !ROUTINE: EEWRITE_EEENV

C     !INTERFACE:
      SUBROUTINE EEWRITE_EEENV

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE EERWITE\_EEENV
C     | o Write execution environment summary
C     *==========================================================*
C     | Write a summary of the execution environment as
C     | configured for this run. The execution environment is
C     | the computational mode in which the model operatoes. It
C     | includes the computational grid but does not include any
C     | model specific numerical parameters.
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"

C     !LOCAL VARIABLES:
C     == Local variables ==
C     msgBuf :: Temp. for building text messages.
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*(8) fmtStr
      INTEGER iTmp
CEOP

      iTmp = MAX(5,1 + INT(LOG10(DFLOAT(MAX(Nx,Ny)))))
      WRITE(fmtStr,'(A,I1,A)') '(A,I',iTmp,',A)'

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

      WRITE(msgBuf,fmtStr) '     nPx =',nPx,
     & ' ; /* No. processes in X */'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)

      WRITE(msgBuf,fmtStr) '     nPy =',nPy,
     & ' ; /* No. processes in Y */'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)

      WRITE(msgBuf,fmtStr) '     nSx =',nSx,
     & ' ; /* No. tiles in X per process */'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)

      WRITE(msgBuf,fmtStr) '     nSy =',nSy,
     & ' ; /* No. tiles in Y per process */'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)

      WRITE(msgBuf,fmtStr) '     sNx =',sNx,
     & ' ; /* Tile size in X */'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)

      WRITE(msgBuf,fmtStr) '     sNy =',sNy,
     & ' ; /* Tile size in Y */'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)

      WRITE(msgBuf,fmtStr) '     OLx =',OLx,
     & ' ; /* Tile overlap distance in X */'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)

      WRITE(msgBuf,fmtStr) '     OLy =',OLy,
     & ' ; /* Tile overlap distance in Y */'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)

      WRITE(msgBuf,fmtStr) '     nTx =',nTx,
     & ' ; /* No. threads in X per process */'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)

      WRITE(msgBuf,fmtStr) '     nTy =',nTy,
     & ' ; /* No. threads in Y per process */'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)

      WRITE(msgBuf,fmtStr) '      Nr =', Nr,
     & ' ; /* No. levels in the vertical   */ '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)

      WRITE(msgBuf,fmtStr) '      Nx =', Nx,
     & ' ; /* Total domain size in X ( = nPx*nSx*sNx ) */'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)

      WRITE(msgBuf,fmtStr) '      Ny =', Ny,
     & ' ; /* Total domain size in Y ( = nPy*nSy*sNy ) */'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)

      WRITE(msgBuf,fmtStr) '  nTiles =', nSx*nSy,
     & ' ; /* Total no. tiles per process ( = nSx*nSy ) */'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)

      WRITE(msgBuf,fmtStr) '  nProcs =', nPx*nPy,
     & ' ; /* Total no. processes ( = nPx*nPy ) */'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)

      WRITE(msgBuf,fmtStr) 'nThreads =', nTx*nTy,
     & ' ; /* Total no. threads per process ( = nTx*nTy ) */'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)

      WRITE(fmtStr,'(A,I1,A)') '(A,L',iTmp,',A)'
      WRITE(msgBuf,fmtStr) 'usingMPI =', usingMPI,
     & ' ; /* Flag used to control whether MPI is in use */'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)

      WRITE(fmtStr,'(A,I1,A)') '(A,A',iTmp,',A)'
      WRITE(msgBuf,fmtStr) '          ', '     ' ,
     & '   /*  note: To execute a program with MPI calls */'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)

      WRITE(msgBuf,fmtStr) '          ', '     ' ,
     & '   /*  it must be launched appropriately e.g     */'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)

      WRITE(msgBuf,fmtStr) '          ', '     ' ,
     & '   /*  "mpirun -np 64 ......"                    */'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)

      WRITE(fmtStr,'(A,I1,A)') '(A,L',iTmp-1,',A)'
      WRITE(msgBuf,fmtStr) 'useCoupler=', useCoupler,
     & ' ; /* Flag used to control communications with   */'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)
      WRITE(fmtStr,'(A,I1,A)') '(A,A',iTmp,',A)'
      WRITE(msgBuf,fmtStr)  '           ', '     ',
     & '  /*  other model components, through a coupler */'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)

      WRITE(msgBuf,'(A,L5,A)') 'useNest2W_parent =', useNest2W_parent,
     & ' ;/* Control 2-W Nesting comm */'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A,L5,A)') 'useNest2W_child  =', useNest2W_child,
     & ' ;/* Control 2-W Nesting comm */'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)

      WRITE(msgBuf,'(A,L5,A)') 'debugMode =', debugMode,
     & ' ; /* print debug msg. (sequence of S/R calls)  */'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A,L5,A)')
     &  'printMapIncludesZeros=', printMapIncludesZeros,
     &  ' ; /* print zeros in Std.Output maps */'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A,I5,A)') 'maxLengthPrt1D=', maxLengthPrt1D,
     &           ' /* maxLength of 1D array printed to StdOut */'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

      WRITE(msgBuf,'(A)') '                '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)

      RETURN
      END
