C $Id: plot_field.F,v 1.1 1998/05/25 20:21:05 cnh Exp $
#include "CPP_OPTIONS.h"
#include "CPP_MACROS.h"
      SUBROUTINE PLOT_FIELD ( A, Nx, Ny )
      IMPLICIT NONE
C
      INTEGER Nx, Ny
      REAL    A(Nx,Ny)
C
      INTEGER NC
      PARAMETER ( NC = 28 )
      CHARACTER*(NC) CVAL
      PARAMETER ( CVAL =
     &'-abcdefghijklmnopqrstuvwxyz+' )
      REAL AMN, AMX, CINT
      CHARACTER BUFFER(1024)
      INTEGER I, J, IDX
C
      AMN =  1.D32
      AMX = -1.D32
      DO J=1,Ny
       DO I=1,Nx
        IF( A(I,J).NE.0.) THEN
         IF ( A(I,J) .LT. AMN ) AMN = A(I,J)
         IF ( A(I,J) .GT. AMX ) AMX = A(I,J)
        ENDIF
       ENDDO
      ENDDO
      CINT = (AMX-AMN)/FLOAT(NC-1)
      WRITE (0,*) ' Min = ', AMN,' Max = ',AMX, ' CINT = ', CINT
      DO J = Ny, 1 , -1
       DO I = 1, Nx
        IF ( CINT .NE. 0. ) THEN
         IDX = NINT(  FLOAT(NC-1)*(A(I,J)-AMN)/(AMX-AMN) )+1
         BUFFER(I) = CVAL(IDX:IDX)
        ELSE
         BUFFER(I) = '*'
        ENDIF
        IF ( A(I,J) .EQ. 0. ) BUFFER(I) = '.'
C       IF ( A(I,J) .LT. 0. ) BUFFER(I) = '-'
C       IF ( A(I,J) .GT. 0. ) BUFFER(I) = '+'
       ENDDO
       WRITE(0,'(I3,1X,1024A)') J,(BUFFER(I),I=1,Nx)
      ENDDO
C
      END
