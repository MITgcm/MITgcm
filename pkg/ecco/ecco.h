
c     ==================================================================
c     HEADER ECCO
c     ==================================================================
c
c     o Header for the ECCO project.
c
c     started: Christian Eckert eckert@mit.edu  23-Feb-1999
c
c     changed: Christian Eckert eckert@mit.edu
c
c
c     ==================================================================
c     HEADER ECCO
c     ==================================================================

c     Version of the release and versions of used packages:
c     =====================================================
c
c     eccoVersion                - ecco release version.
c     usesCalendarVersion        - version of the calendar that has to
c                                  be used.
c     usesExternalForcingVersion - version of the external forcing that
c                                  has to be used.
c     usesAdjointSupportVersion  - version of the adjoint support routines
c                                  that have to be used.
c     usesOptimizationVersion    - version of the oof-line optimization
c                                  that has to be used.

      character*(5) eccoVersion
      character*(5) eccoUsesCalVersion
      character*(5) eccoUsesExfVersion
      character*(5) eccoUsesAdsuppVersion
      character*(5) eccoUsesOptimVersion

      parameter(    eccoVersion           = '0.1.0' )
      parameter(    eccoUsesCalVersion    = '0.1.4' )
      parameter(    eccoUsesExfVersion    = '0.1.1' )
      parameter(    eccoUsesAdsuppVersion = '0.1.0' )
      parameter(    eccoUsesOptimVersion  = '2.1.0' )


c     Experiment name:
c     ================

      common /ecco_c/
     &                expId
      character*(10)  expId


c     Integration information:
c     ========================
c
c     nyears - number of calendar years that are affected by the
c              current integration.

      common /ecco_i/
     &                nyears, nmonths, ndays, numsteps,
     &                ecco_prevcall
      integer nyears
      integer nmonths
      integer ndays
      integer numsteps

      integer ecco_prevcall

c     ==================================================================
c     END OF HEADER ECCO
c     ==================================================================


