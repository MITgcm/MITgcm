#include "STREAMICE_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: STREAMICE_INIT_FIXED

C !INTERFACE:
      SUBROUTINE STREAMICE_INIT_PHI( myThid )

C     !DESCRIPTION:
C     Initialize STREAMICE nodal basis gradients for FEM solver

C     !USES:
      IMPLICIT NONE
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "STREAMICE.h"
#include "STREAMICE_CG.h"
#include "GRID.h"

C     myThid ::  my Thread Id number
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     === Local variables ===
      INTEGER bi, bj, i, j, xnode, ynode, xq, yq, m, n, p, kx, ky
      REAL gradx(2), grady(2)  ! gradients at quadrature points

C     here the terms used to calculate matrix terms in the 
C     velocity solve are initialized
C
C     this is a quasi-finite element method; the gradient
C     of the basis functions are approximated based on knowledge
C     of the grid
C
C     Dphi (i,j,bi,bj,m,n,p): 
C       gradient (in p-direction) of nodal basis function in 
C       cell (i,j) on thread (bi,bj) which is centered on node m, 
C       at quadrature point n
C
C    %  3 - 4
C    %  |   |
C    %  1 - 2
C
C     NOTE 2x2 quadrature is hardcoded - might make it specifiable through CPP   
C
C     this will not be updated in overlap cells - so we extend it as far as we can
      
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-Oly,sNy+Oly-1
         DO i=1-Olx,sNx+Olx-1

          DO xq = 1,2
           gradx(xq) = Xquad(3-xq) * recip_dxG (i,j,bi,bj) + 
     &                 Xquad(xq) * recip_dxG (i+1,j,bi,bj)
           grady(xq) = Xquad(3-xq) * recip_dyG (i,j,bi,bj) + 
     &                 Xquad(xq) * recip_dyG (i,j+1,bi,bj)
          ENDDO

          DO n = 1,4 
      
           xq = 2 - mod(n,2)
           yq = floor ((n+1)/2.0)
           
           DO m = 1,4

            xnode = 2 - mod(m,2)
            ynode = floor ((m+1)/2.0)

            kx = 1 ; ky = 1
            if (xq.eq.xnode) kx = 2
            if (yq.eq.ynode) ky = 2

            
            Dphi (i,j,bi,bj,m,n,1) = 
     &       (2*xnode-3) * Xquad(ky) * gradx(yq)
            Dphi (i,j,bi,bj,m,n,2) = 
     &       (2*ynode-3) * Xquad(kx) * grady(xq)
           
           ENDDO

           grid_jacq_streamice (i,j,bi,bj,n) = 
     &      (Xquad(3-xq)*dyG(i,j,bi,bj) + Xquad(xq)*dyG(i+1,j,bi,bj)) * 
     &      (Xquad(3-yq)*dxG(i,j,bi,bj) + Xquad(yq)*dxG(i,j+1,bi,bj)) 

          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      RETURN
      END
