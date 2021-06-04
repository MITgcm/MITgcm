#include "CPP_EEOPTIONS.h"

      SUBROUTINE CTRL_SMOOTH (
     U                         fld,
     I                         mask, myThid )

c     Apply horizontal smoothing to global _RL 2-D array

      IMPLICIT NONE

#include "SIZE.h"
#include "EEPARAMS.h"

c     input/output
c     fld    : 2-D array to be smoothed
      _RL fld ( 1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy )
      _RS mask( 1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy )
      INTEGER myThid

c     local
c     bi, bj : array indices
c     k      : vertical index used for masking
      integer k, bi, bj
      integer i, j, im1, ip1, jm1, jp1
      _RL tempVar
      _RL fld_tmp( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy )

      integer   imin      , imax          , jmin      , jmax
      parameter(imin=2-OLx, imax=sNx+OLx-1, jmin=2-OLy, jmax=sNy+OLy-1)
      _RL        p0    , p5    , p25     , p125      , p0625
      parameter( p0=0. _d 0 , p5=0.5 _d 0 , p25=0.25 _d 0 )
      parameter( p125=0.125 _d 0 , p0625=0.0625 _d 0 )

      k = 1
      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)

         DO j = jmin, jmax
          jm1 = j-1
          jp1 = j+1
          DO i = imin, imax
           im1 = i-1
           ip1 = i+1
           tempVar =
     &           p25   *   mask(i  ,j  ,bi,bj)   +
     &           p125  * ( mask(im1,j  ,bi,bj)   +
     &                     mask(ip1,j  ,bi,bj)   +
     &                     mask(i  ,jm1,bi,bj)   +
     &                     mask(i  ,jp1,bi,bj) ) +
     &           p0625 * ( mask(im1,jm1,bi,bj)   +
     &                     mask(im1,jp1,bi,bj)   +
     &                     mask(ip1,jm1,bi,bj)   +
     &                     mask(ip1,jp1,bi,bj) )
           IF ( tempVar .GE. p25 ) THEN
             fld_tmp(i,j) = (
     &              p25  * fld(i  ,j,bi,bj  )*mask(i  ,j  ,bi,bj) +
     &              p125 *(fld(im1,j ,bi,bj )*mask(im1,j  ,bi,bj) +
     &                     fld(ip1,j ,bi,bj )*mask(ip1,j  ,bi,bj) +
     &                     fld(i  ,jm1,bi,bj)*mask(i  ,jm1,bi,bj) +
     &                     fld(i  ,jp1,bi,bj)*mask(i  ,jp1,bi,bj))+
     &              p0625*(fld(im1,jm1,bi,bj)*mask(im1,jm1,bi,bj) +
     &                     fld(im1,jp1,bi,bj)*mask(im1,jp1,bi,bj) +
     &                     fld(ip1,jm1,bi,bj)*mask(ip1,jm1,bi,bj) +
     &                     fld(ip1,jp1,bi,bj)*mask(ip1,jp1,bi,bj)))
     &              / tempVar
           ELSE
             fld_tmp(i,j) = fld(i,j,bi,bj)
           ENDIF
          ENDDO
         ENDDO

c     transfer smoothed field to output array
         DO j = jmin, jmax
          DO i = imin, imax
            fld(i,j,bi,bj) = fld_tmp(i,j)
          ENDDO
         ENDDO

C-    end bi,bj loop.
       ENDDO
      ENDDO

      RETURN
      END
