C $Header: /u/gcmpack/MITgcm/model/inc/Attic/FFTSUPPORT.h,v 1.2 2000/03/24 16:03:01 adcroft Exp $

#include "CPP_OPTIONS.h"

C     /==========================================================\
C     | o FFTSUPPORT.h                                           |
C     | General FFT supporting data structures                   |
C     |==========================================================|
C     | FFT filtering is used to allow a timestep that           |
C     | violates the straight CFL condition.                     |
C     | This header contains data structures that are used by    |
C     | the FFT filters to damp high-frequency signals.          |
C     | Platform specific FFT library header files are included  |
C     | from separate sources.                                   |
C     \==========================================================/

C     o Latitude circle filtering support data structures.
C     epsFFTFilt - Latitude and frequency dependent fourier mode
C                  amplitude scaling factors. These factors are used
C                  to damp more strongly in polar regions on a spherical
C                  grid whilst leaving equatorial regions undamped.
C                  Two levels are defined one for U grid oriented
C                  points and one for V grid points.
C                  Filter is initialised with amplitude scaling factors
C                  for each of the sNx/2 frequencies.
C                  ** Note ** FFT filtering assumes a periodic domain
C                  with filtered data visible to a single thread.
C                  To ensure this the filter routines fail if nSx > 1
C                  - we could do sNx*nSx if myBxLo=1 and myBxHi=nSx
C                     but we do not for now.
C                  Some filter packages require sNx to be a power of
C                  two. Some only require that sNx is even.
C                  Note. We require an overlap on y so that we can filter
C                        the g terms to for calculating div(Ghat) without
C                        needing communication or synchronisation.
C                        G1 - Grid 1 e.g. U,T,S
C                        G2 - Grid 2 e.g. V
C                        epsFFTFilt.. is a frequency x latitude array
C                        not a longitude x latitude array. The valid 
C                        points in epsFFTFilt.. are in i=1:sNx/2+1
C                        We use a longitude x latitude data structure so that
C                        we can call the standard exchanges routines to 
C                        initialise the overlap regions of 
C                        epsFFTFilt.. 
      COMMON /FFT_SUPPORT/ epsFFTFiltG1, epsFFTFiltG2
      _RL epsFFTFiltG1( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy, nSx, nSy )
      _RL epsFFTFiltG2( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy, nSx, nSy )

C     Data structures for Digital/Compaqs DXML scientfic library
#ifdef ALLOW_DXML_FFT
#include "FFT_DXML_SUPPORT.h"
#endif
