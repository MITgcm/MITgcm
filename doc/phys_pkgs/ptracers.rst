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

   ---------------------------------------------------------------------------------
   Num |<-Name->|Levs|  mate |<- code ->|<--  Units   -->|<- Tile (max=80c)
   ---------------------------------------------------------------------------------
   215 |TRAC01  | 15 |       |SMR     MR|mol/m^3         |Dissolved Inorganic Carbon (DIC) [mol C/m^3] concentration
   216 |UTRAC01 | 15 |   217 |UUr     MR|mol/m^3.m/s     |Zonal Mass-Weighted Transp of DIC
   217 |VTRAC01 | 15 |   216 |VVr     MR|mol/m^3.m/s     |Merid Mass-Weighted Transp of DIC
   218 |WTRAC01 | 15 |       |WM      MR|mol/m^3.m/s     |Vert  Mass-Weighted Transp of DIC
   219 |ForcTr01| 15 |       |SMR     MR|mol/m^3/s       |DIC forcing tendency
   220 |AB_gTr01| 15 |       |SMR     MR|mol/m^3/s       |DIC tendency from Adams-Bashforth
   221 |Tp_gTr01| 15 |       |SMR     MR|mol/m^3/s       |DIC tendency before gchem_forcing_sep
   222 |ADVrTr01| 15 |       |WM      LR|mol/m^3.m^3/s   |Vertical   Advective Flux of DIC
   223 |ADVxTr01| 15 |   224 |UU      MR|mol/m^3.m^3/s   |Zonal      Advective Flux of DIC
   224 |ADVyTr01| 15 |   223 |VV      MR|mol/m^3.m^3/s   |Meridional Advective Flux of DIC
   225 |DFrETr01| 15 |       |WM      LR|mol/m^3.m^3/s   |Vertical Diffusive Flux of DIC (Explicit part)
   226 |DFxETr01| 15 |   227 |UU      MR|mol/m^3.m^3/s   |Zonal      Diffusive Flux of DIC
   227 |DFyETr01| 15 |   226 |VV      MR|mol/m^3.m^3/s   |Meridional Diffusive Flux of DIC
   228 |DFrITr01| 15 |       |WM      LR|mol/m^3.m^3/s   |Vertical Diffusive Flux of DIC (Implicit part)
   229 |TRAC02  | 15 |       |SMR     MR|mol/m^3         |Alkalinity (Alk) [mol eq/m^3] concentration
   230 |UTRAC02 | 15 |   231 |UUr     MR|mol/m^3.m/s     |Zonal Mass-Weighted Transp of Alk
   231 |VTRAC02 | 15 |   230 |VVr     MR|mol/m^3.m/s     |Merid Mass-Weighted Transp of Alk
   232 |WTRAC02 | 15 |       |WM      MR|mol/m^3.m/s     |Vert  Mass-Weighted Transp of Alk
   233 |ForcTr02| 15 |       |SMR     MR|mol/m^3/s       |Alk forcing tendency
   234 |AB_gTr02| 15 |       |SMR     MR|mol/m^3/s       |Alk tendency from Adams-Bashforth
   235 |Tp_gTr02| 15 |       |SMR     MR|mol/m^3/s       |Alk tendency before gchem_forcing_sep
   236 |ADVrTr02| 15 |       |WM      LR|mol/m^3.m^3/s   |Vertical   Advective Flux of Alk
   237 |ADVxTr02| 15 |   238 |UU      MR|mol/m^3.m^3/s   |Zonal      Advective Flux of Alk
   238 |ADVyTr02| 15 |   237 |VV      MR|mol/m^3.m^3/s   |Meridional Advective Flux of Alk
   239 |DFrETr02| 15 |       |WM      LR|mol/m^3.m^3/s   |Vertical Diffusive Flux of Alk (Explicit part)
   240 |DFxETr02| 15 |   241 |UU      MR|mol/m^3.m^3/s   |Zonal      Diffusive Flux of Alk
   241 |DFyETr02| 15 |   240 |VV      MR|mol/m^3.m^3/s   |Meridional Diffusive Flux of Alk
   242 |DFrITr02| 15 |       |WM      LR|mol/m^3.m^3/s   |Vertical Diffusive Flux of Alk (Implicit part)
   243 |TRAC03  | 15 |       |SMR     MR|mol/m^3         |Phosphate (PO4) [mol P/m^3] concentration
   244 |UTRAC03 | 15 |   245 |UUr     MR|mol/m^3.m/s     |Zonal Mass-Weighted Transp of PO4
   245 |VTRAC03 | 15 |   244 |VVr     MR|mol/m^3.m/s     |Merid Mass-Weighted Transp of PO4
   246 |WTRAC03 | 15 |       |WM      MR|mol/m^3.m/s     |Vert  Mass-Weighted Transp of PO4
   247 |ForcTr03| 15 |       |SMR     MR|mol/m^3/s       |PO4 forcing tendency
   248 |AB_gTr03| 15 |       |SMR     MR|mol/m^3/s       |PO4 tendency from Adams-Bashforth
   249 |Tp_gTr03| 15 |       |SMR     MR|mol/m^3/s       |PO4 tendency before gchem_forcing_sep
   250 |ADVrTr03| 15 |       |WM      LR|mol/m^3.m^3/s   |Vertical   Advective Flux of PO4
   251 |ADVxTr03| 15 |   252 |UU      MR|mol/m^3.m^3/s   |Zonal      Advective Flux of PO4
   252 |ADVyTr03| 15 |   251 |VV      MR|mol/m^3.m^3/s   |Meridional Advective Flux of PO4
   253 |DFrETr03| 15 |       |WM      LR|mol/m^3.m^3/s   |Vertical Diffusive Flux of PO4 (Explicit part)
   254 |DFxETr03| 15 |   255 |UU      MR|mol/m^3.m^3/s   |Zonal      Diffusive Flux of PO4
   255 |DFyETr03| 15 |   254 |VV      MR|mol/m^3.m^3/s   |Meridional Diffusive Flux of PO4
   256 |DFrITr03| 15 |       |WM      LR|mol/m^3.m^3/s   |Vertical Diffusive Flux of PO4 (Implicit part)
   257 |TRAC04  | 15 |       |SMR     MR|mol/m^3         |Dissolved Organic Phosphorus (DOP) [mol P/m^3] concentration
   258 |UTRAC04 | 15 |   259 |UUr     MR|mol/m^3.m/s     |Zonal Mass-Weighted Transp of DOP
   259 |VTRAC04 | 15 |   258 |VVr     MR|mol/m^3.m/s     |Merid Mass-Weighted Transp of DOP
   260 |WTRAC04 | 15 |       |WM      MR|mol/m^3.m/s     |Vert  Mass-Weighted Transp of DOP
   261 |ForcTr04| 15 |       |SMR     MR|mol/m^3/s       |DOP forcing tendency
   262 |AB_gTr04| 15 |       |SMR     MR|mol/m^3/s       |DOP tendency from Adams-Bashforth
   263 |Tp_gTr04| 15 |       |SMR     MR|mol/m^3/s       |DOP tendency before gchem_forcing_sep
   264 |ADVrTr04| 15 |       |WM      LR|mol/m^3.m^3/s   |Vertical   Advective Flux of DOP
   265 |ADVxTr04| 15 |   266 |UU      MR|mol/m^3.m^3/s   |Zonal      Advective Flux of DOP
   266 |ADVyTr04| 15 |   265 |VV      MR|mol/m^3.m^3/s   |Meridional Advective Flux of DOP
   267 |DFrETr04| 15 |       |WM      LR|mol/m^3.m^3/s   |Vertical Diffusive Flux of DOP (Explicit part)
   268 |DFxETr04| 15 |   269 |UU      MR|mol/m^3.m^3/s   |Zonal      Diffusive Flux of DOP
   269 |DFyETr04| 15 |   268 |VV      MR|mol/m^3.m^3/s   |Meridional Diffusive Flux of DOP
   270 |DFrITr04| 15 |       |WM      LR|mol/m^3.m^3/s   |Vertical Diffusive Flux of DOP (Implicit part)
   271 |TRAC05  | 15 |       |SMR     MR|mol/m^3         |Dissolved Oxygen (O2) [mol O/m^3] concentration
   272 |UTRAC05 | 15 |   273 |UUr     MR|mol/m^3.m/s     |Zonal Mass-Weighted Transp of O2
   273 |VTRAC05 | 15 |   272 |VVr     MR|mol/m^3.m/s     |Merid Mass-Weighted Transp of O2
   274 |WTRAC05 | 15 |       |WM      MR|mol/m^3.m/s     |Vert  Mass-Weighted Transp of O2
   275 |ForcTr05| 15 |       |SMR     MR|mol/m^3/s       |O2 forcing tendency
   276 |AB_gTr05| 15 |       |SMR     MR|mol/m^3/s       |O2 tendency from Adams-Bashforth
   277 |Tp_gTr05| 15 |       |SMR     MR|mol/m^3/s       |O2 tendency before gchem_forcing_sep
   278 |ADVrTr05| 15 |       |WM      LR|mol/m^3.m^3/s   |Vertical   Advective Flux of O2
   279 |ADVxTr05| 15 |   280 |UU      MR|mol/m^3.m^3/s   |Zonal      Advective Flux of O2
   280 |ADVyTr05| 15 |   279 |VV      MR|mol/m^3.m^3/s   |Meridional Advective Flux of O2
   281 |DFrETr05| 15 |       |WM      LR|mol/m^3.m^3/s   |Vertical Diffusive Flux of O2 (Explicit part)
   282 |DFxETr05| 15 |   283 |UU      MR|mol/m^3.m^3/s   |Zonal      Diffusive Flux of O2
   283 |DFyETr05| 15 |   282 |VV      MR|mol/m^3.m^3/s   |Meridional Diffusive Flux of O2
   284 |DFrITr05| 15 |       |WM      LR|mol/m^3.m^3/s   |Vertical Diffusive Flux of O2 (Implicit part)

Do’s and Don’ts
+++++++++++++++

Reference Material
++++++++++++++++++

