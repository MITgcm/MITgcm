
.. _pkg_monitor:

Monitor: Simulation State Monitoring Toolkit
============================================

Introduction
------------

:filelink:`pkg/monitor` is primarily intended as a convenient method for
calculating and writing the following statistics:

- minimum
- maximum
- mean
- standard deviation

for spatially distributed fields. By default, :filelink:`pkg/monitor` output is sent
to the “standard output” channel where it appears as ASCII text
containing a ``%MON`` string such as this example:

::

         (PID.TID 0000.0001) %MON time_tsnumber      =                     3
         (PID.TID 0000.0001) %MON time_secondsf      =   3.6000000000000E+03
         (PID.TID 0000.0001) %MON dynstat_eta_max    =   1.0025466645951E-03
         (PID.TID 0000.0001) %MON dynstat_eta_min    =  -1.0008899950901E-03
         (PID.TID 0000.0001) %MON dynstat_eta_mean   =   2.1037438449350E-14
         (PID.TID 0000.0001) %MON dynstat_eta_sd     =   5.0985228723396E-04
         (PID.TID 0000.0001) %MON dynstat_eta_del2   =   3.5216706549525E-07
         (PID.TID 0000.0001) %MON dynstat_uvel_max   =   3.7594045977254E-05
         (PID.TID 0000.0001) %MON dynstat_uvel_min   =  -2.8264287531564E-05
         (PID.TID 0000.0001) %MON dynstat_uvel_mean  =   9.1369201945671E-06
         (PID.TID 0000.0001) %MON dynstat_uvel_sd    =   1.6868439193567E-05
         (PID.TID 0000.0001) %MON dynstat_uvel_del2  =   8.4315445301916E-08

:filelink:`pkg/monitor` text can be readily parsed by the ``testreport`` script
to determine, somewhat crudely but quickly, how similar the output from
two experiments are when run on different platforms or before/after code
changes.

:filelink:`pkg/monitor` output can also be useful for quickly diagnosing
practical problems such as CFL limitations, model progress (through
iteration counts), and behavior within some packages that use it.

Using pkg/monitor
-----------------

As with most packages, :filelink:`pkg/monitor` can be turned on or off at compile
and/or run times using the ``packages.conf`` and ``data.pkg`` files.

The monitor output can be sent to the standard output channel, to an
:filelink:`pkg/mnc`–generated file, or to both simultaneously. For :filelink:`pkg/mnc` output,
the flag  ``monitor_mnc=.TRUE.`` should be set within the ``data.mnc`` file. For output to both ASCII and
:filelink:`pkg/mnc`, the flag ``outputTypesInclusive=.TRUE.`` should be set
within the ``PARM03`` section of the main ``data`` file.
It should be noted that the ``outputTypesInclusive`` flag will make
**ALL** kinds of output (that is, everything written by :filelink:`pkg/mdsio`,
:filelink:`pkg/mnc`, and :filelink:`pkg/monitor`) simultaneously active so it should be used
only with caution -– and perhaps only for debugging purposes.


.. tabularcolumns:: |\Y{.4}|L|L|


+-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
| CPP Flag Name                                 | Default | Description                                                                                                          |
+===============================================+=========+======================================================================================================================+
| :varlink:`MONITOR_TEST_HFACZ`                 | #undef  | disable use of hFacZ                                                                                                 |
+-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
