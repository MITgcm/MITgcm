
c     ==================================================================
c     HEADER MINIMIZATION
c     ==================================================================
c
c     o Header for the minimization algorithm.
c
c     started: Christian Eckert eckert@mit.edu  07-Apr-2000
c
c     changed: Christian Eckert eckert@mit.edu
c
c
c     ==================================================================
c     HEADER MINIMIZATION
c     ==================================================================

      common /minimization_l/
     &                        fileavailable,
     &                        tileavailable
      logical tileavailable( nsx*npx, nsy*npy )
      logical fileavailable( nsx*npx, nsy*npy )

c     ==================================================================
c     END OF HEADER MINIMIZATION
c     ==================================================================
