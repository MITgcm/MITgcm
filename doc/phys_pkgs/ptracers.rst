.. _sub_phys_pkg_ptracers:

PTRACERS Package
----------------


Introduction
++++++++++++

This is a ''passive'' tracer package. Passive here means that the tracers
don’t affect the density of the water (as opposed to temperature and
salinity) so no not actively affect the physics of the ocean. Tracers
are initialized, advected, diffused and various outputs are taken care
of in this package. For methods to add additional sources and sinks of
tracers use the :ref:`gchem Package <sub_phys_pkg_gchem>`.

Can use up tp 3843 tracers. But can not use the :ref:`diagnostics package <sub_outp_pkg_diagnostics>` with more
than about 90 tracers. Use :filelink:`utils/matlab/ioLb2num.m` and :filelink:`num2ioLb.m <utils/matlab/num2ioLB.m>` to
find correspondence between tracer number and tracer designation in the
code for more than 99 tracers (since tracers only have two digit
designations).

Equations
+++++++++

Key subroutines and parameters
++++++++++++++++++++++++++++++

The only code you should have to modify is: :filelink:`PTRACERS_SIZE.h <pkg/ptracers/PTRACERS_SIZE.h>` where
you need to set in the number of tracers to be used in the experiment:
:varlink:`PTRACERS_num`.

Run time parameters set in :code:`data.ptracers`:

- :varlink:`PTRACERS_Iter0` which is the integer timestep when the tracer experiment is initialized. If ``nIter0`` = ``PTRACERS_Iter0`` then the tracers are initialized to zero or from initial files. If ``nIter0`` :math:`>` ``PTRACERS_Iter0`` then tracers (and previous timestep tendency terms) are read in from a the ptracers pickup file. Note that tracers of zeros will be carried around if ``nIter0`` :math:`<` ``PTRACERS_Iter0``.
- :varlink:`PTRACERS_numInUse`: number of tracers to be used in the run (needs to be :math:`<=` ``PTRACERS_num`` set in :filelink:`PTRACERS_SIZE.h <pkg/ptracers/PTRACERS_SIZE.h>`)
- :varlink:`PTRACERS_dumpFreq`: defaults to :varlink:`dumpFreq` (set in data)
- :varlink:`PTRACERS_taveFreq`: defaults to :varlink:`taveFreq` (set in data)
- :varlink:`PTRACERS_monitorFreq`: defaults to :varlink:`monitorFreq` (set in data)
- :varlink:`PTRACERS_timeave_mnc`: needs :varlink:`useMNC`, :varlink:`timeave_mnc`, default to false
- :varlink:`PTRACERS_snapshot_mnc`: needs :varlink:`useMNC` , :varlink:`snapshot_mnc`, default to false
- :varlink:`PTRACERS_monitor_mnc`: needs :varlink:`useMNC`, :varlink:`monitor_mnc`, default to false
- :varlink:`PTRACERS_pickup_write_mnc`: needs :varlink:`useMNC`, :varlink:`pickup_write_mnc`, default to false
- :varlink:`PTRACERS_pickup_read_mnc`: needs :varlink:`useMNC`, :varlink:`pickup_read_mnc`, default to false
- :varlink:`PTRACERS_useRecords`: defaults to false. If true, will write all tracers in a single file, otherwise each tracer in a seperate file.

The following can be set for each tracer (tracer number iTrc):

- :varlink:`PTRACERS_advScheme` (iTrc) will default to :varlink:`saltAdvScheme` (set in data). For other options see Table :ref:`adv_scheme_summary`.
- :varlink:`PTRACERS_ImplVertAdv` (iTrc): implicit vertical advection flag, defaults to false.
- :varlink:`PTRACERS_diffKh` (iTrc): horizontal Laplacian Diffusivity, defaults to :varlink:`diffKhS` (set in data).
- :varlink:`PTRACERS_diffK4` (iTrc): Biharmonic Diffusivity, defaults to :varlink:`diffK4S` (set in data).
- :varlink:`PTRACERS_diffKr` (iTrc): vertical diffusion, defaults to un-set.
- :varlink:`PTRACERS_diffKrNr` (k,iTrc): level specific vertical diffusion, defaults to :varlink:`diffKrNrS`. Will be set to :varlink:`PTRACERS_diffKr` if this is set.
- :varlink:`PTRACERS_ref` (k,iTrc): reference tracer value for each level k, defaults to 0. Currently only used for dilution/concentration of tracers at surface if :varlink:`PTRACERS_EvPrRn` (iTrc) is set and :varlink:`convertFW2Salt` (set in data) is set to something other than -1 (note default is :varlink:`convertFW2Salt` = 35).
- :varlink:`PTRACERS_EvPrRn` (iTrc): tracer concentration in freshwater. Needed for calculation of dilution/concentration in surface layer due to freshwater addition/evaporation. Defaults to un-set in which case no dilution/concentration occurs.
- :varlink:`PTRACERS_useGMRedi` (iTrc): apply GM or not. Defaults to :varlink:`useGMREdi`.
- :varlink:`PTRACERS_useKPP` (iTrc): apply KPP or not. Defaults to :varlink:`useKPP`.
- :varlink:`PTRACERS_initialFile` (iTrc): file with initial tracer concentration. Will be used if ``PTRACERS_Iter0`` :math:`=` ``nIter0``. Default is no name, in which case tracer is initialised as zero. If ``PTRACERS_Iter0`` :math:`<` ``nIter0``, then tracer concentration will come from ``pickup_ptracer``.
- :varlink:`PTRACERS_names` (iTrc): tracer name. Needed for netcdf. Defaults to nothing.
- :varlink:`PTRACERS_long_names` (iTrc): optional name in long form of tracer.
- :varlink:`PTRACERS_units` (iTrc): optional units of tracer.

.. _ptracers_diagnostics:

PTRACERS Diagnostics
++++++++++++++++++++

Note that beyond 99 ptracers, diagnostics will be labeled with letters in
addition to numbers, e.g., the diagnostic for the 100th ptracer is TRAC0a, etc.

::

   ---------------------------------------------------------------
   <-Name->|<- code ->|<--  Units   -->|<- Tile (max=80c)
   ---------------------------------------------------------------
   TRAC01  |SMR     MR|mol/m^3         |Dissolved Inorganic Carbon (DIC) [mol C/m^3] concentration
   UTRAC01 |UUr     MR|mol/m^3.m/s     |Zonal Mass-Weighted Transp of DIC
   VTRAC01 |VVr     MR|mol/m^3.m/s     |Merid Mass-Weighted Transp of DIC
   WTRAC01 |WM      MR|mol/m^3.m/s     |Vert  Mass-Weighted Transp of DIC
   ForcTr01|SMR     MR|mol/m^3/s       |DIC forcing tendency
   AB_gTr01|SMR     MR|mol/m^3/s       |DIC tendency from Adams-Bashforth
   Tp_gTr01|SMR     MR|mol/m^3/s       |DIC total transport tendency (before gchem_forcing_sep)
   ADVrTr01|WM      LR|mol/m^3.m^3/s   |Vertical   Advective Flux of DIC
   ADVxTr01|UU      MR|mol/m^3.m^3/s   |Zonal      Advective Flux of DIC
   ADVyTr01|VV      MR|mol/m^3.m^3/s   |Meridional Advective Flux of DIC
   DFrETr01|WM      LR|mol/m^3.m^3/s   |Vertical Diffusive Flux of DIC (Explicit part)
   DFxETr01|UU      MR|mol/m^3.m^3/s   |Zonal      Diffusive Flux of DIC
   DFyETr01|VV      MR|mol/m^3.m^3/s   |Meridional Diffusive Flux of DIC
   DFrITr01|WM      LR|mol/m^3.m^3/s   |Vertical Diffusive Flux of DIC (Implicit part)
   TRAC02  |SMR     MR|mol/m^3         |Alkalinity (Alk) [mol eq/m^3] concentration
   UTRAC02 |UUr     MR|mol/m^3.m/s     |Zonal Mass-Weighted Transp of Alk
   VTRAC02 |VVr     MR|mol/m^3.m/s     |Merid Mass-Weighted Transp of Alk
   WTRAC02 |WM      MR|mol/m^3.m/s     |Vert  Mass-Weighted Transp of Alk
   ForcTr02|SMR     MR|mol/m^3/s       |Alk forcing tendency
   AB_gTr02|SMR     MR|mol/m^3/s       |Alk tendency from Adams-Bashforth
   Tp_gTr02|SMR     MR|mol/m^3/s       |Alk total transport tendency (before gchem_forcing_sep)
   ADVrTr02|WM      LR|mol/m^3.m^3/s   |Vertical   Advective Flux of Alk
   ADVxTr02|UU      MR|mol/m^3.m^3/s   |Zonal      Advective Flux of Alk
   ADVyTr02|VV      MR|mol/m^3.m^3/s   |Meridional Advective Flux of Alk
   DFrETr02|WM      LR|mol/m^3.m^3/s   |Vertical Diffusive Flux of Alk (Explicit part)
   DFxETr02|UU      MR|mol/m^3.m^3/s   |Zonal      Diffusive Flux of Alk
   DFyETr02|VV      MR|mol/m^3.m^3/s   |Meridional Diffusive Flux of Alk
   DFrITr02|WM      LR|mol/m^3.m^3/s   |Vertical Diffusive Flux of Alk (Implicit part)
   TRAC03  |SMR     MR|mol/m^3         |Phosphate (PO4) [mol P/m^3] concentration
   UTRAC03 |UUr     MR|mol/m^3.m/s     |Zonal Mass-Weighted Transp of PO4
   VTRAC03 |VVr     MR|mol/m^3.m/s     |Merid Mass-Weighted Transp of PO4
   WTRAC03 |WM      MR|mol/m^3.m/s     |Vert  Mass-Weighted Transp of PO4
   ForcTr03|SMR     MR|mol/m^3/s       |PO4 forcing tendency
   AB_gTr03|SMR     MR|mol/m^3/s       |PO4 tendency from Adams-Bashforth
   Tp_gTr03|SMR     MR|mol/m^3/s       |PO4 total transport tendency (before gchem_forcing_sep)
   ADVrTr03|WM      LR|mol/m^3.m^3/s   |Vertical   Advective Flux of PO4
   ADVxTr03|UU      MR|mol/m^3.m^3/s   |Zonal      Advective Flux of PO4
   ADVyTr03|VV      MR|mol/m^3.m^3/s   |Meridional Advective Flux of PO4
   DFrETr03|WM      LR|mol/m^3.m^3/s   |Vertical Diffusive Flux of PO4 (Explicit part)
   DFxETr03|UU      MR|mol/m^3.m^3/s   |Zonal      Diffusive Flux of PO4
   DFyETr03|VV      MR|mol/m^3.m^3/s   |Meridional Diffusive Flux of PO4
   DFrITr03|WM      LR|mol/m^3.m^3/s   |Vertical Diffusive Flux of PO4 (Implicit part)
   TRAC04  |SMR     MR|mol/m^3         |Dissolved Organic Phosphorus (DOP) [mol P/m^3] concentration
   UTRAC04 |UUr     MR|mol/m^3.m/s     |Zonal Mass-Weighted Transp of DOP
   VTRAC04 |VVr     MR|mol/m^3.m/s     |Merid Mass-Weighted Transp of DOP
   WTRAC04 |WM      MR|mol/m^3.m/s     |Vert  Mass-Weighted Transp of DOP
   ForcTr04|SMR     MR|mol/m^3/s       |DOP forcing tendency
   AB_gTr04|SMR     MR|mol/m^3/s       |DOP tendency from Adams-Bashforth
   Tp_gTr04|SMR     MR|mol/m^3/s       |DOP total transport tendency (before gchem_forcing_sep)
   ADVrTr04|WM      LR|mol/m^3.m^3/s   |Vertical   Advective Flux of DOP
   ADVxTr04|UU      MR|mol/m^3.m^3/s   |Zonal      Advective Flux of DOP
   ADVyTr04|VV      MR|mol/m^3.m^3/s   |Meridional Advective Flux of DOP
   DFrETr04|WM      LR|mol/m^3.m^3/s   |Vertical Diffusive Flux of DOP (Explicit part)
   DFxETr04|UU      MR|mol/m^3.m^3/s   |Zonal      Diffusive Flux of DOP
   DFyETr04|VV      MR|mol/m^3.m^3/s   |Meridional Diffusive Flux of DOP
   DFrITr04|WM      LR|mol/m^3.m^3/s   |Vertical Diffusive Flux of DOP (Implicit part)
   TRAC05  |SMR     MR|mol/m^3         |Dissolved Oxygen (O2) [mol O/m^3] concentration
   UTRAC05 |UUr     MR|mol/m^3.m/s     |Zonal Mass-Weighted Transp of O2
   VTRAC05 |VVr     MR|mol/m^3.m/s     |Merid Mass-Weighted Transp of O2
   WTRAC05 |WM      MR|mol/m^3.m/s     |Vert  Mass-Weighted Transp of O2
   ForcTr05|SMR     MR|mol/m^3/s       |O2 forcing tendency
   AB_gTr05|SMR     MR|mol/m^3/s       |O2 tendency from Adams-Bashforth
   Tp_gTr05|SMR     MR|mol/m^3/s       |O2 total transport tendency (before gchem_forcing_sep)
   ADVrTr05|WM      LR|mol/m^3.m^3/s   |Vertical   Advective Flux of O2
   ADVxTr05|UU      MR|mol/m^3.m^3/s   |Zonal      Advective Flux of O2
   ADVyTr05|VV      MR|mol/m^3.m^3/s   |Meridional Advective Flux of O2
   DFrETr05|WM      LR|mol/m^3.m^3/s   |Vertical Diffusive Flux of O2 (Explicit part)
   DFxETr05|UU      MR|mol/m^3.m^3/s   |Zonal      Diffusive Flux of O2
   DFyETr05|VV      MR|mol/m^3.m^3/s   |Meridional Diffusive Flux of O2
   DFrITr05|WM      LR|mol/m^3.m^3/s   |Vertical Diffusive Flux of O2 (Implicit part)

Do’s and Don’ts
+++++++++++++++

Reference Material
++++++++++++++++++

