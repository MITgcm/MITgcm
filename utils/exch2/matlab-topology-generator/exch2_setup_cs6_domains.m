% Make list of subdomains. Assume MITgcm standard cube layout, three color 
% path labeling and global indexing convention.
function [ndomains,domain,domain_nx,domain_ny] = ...
         exch2_setup_cs6_domains(nr,nb,ng);

% $Header: /u/gcmpack/MITgcm/utils/exch2/matlab-topology-generator/Attic/exch2_setup_cs6_domains.m,v 1.3 2008/07/23 13:22:29 cnh Exp $
% $Name:  $

ndomains=6;
domain(1).dnx=nb;
domain(1).dny=nr;
domain(1).basex=1;
domain(1).basey=1;
domain(1).wFace='n';
domain(1).nFace='w';
domain(1).eFace='w';
domain(1).sFace='n';
domain(1).wDomain=5;
domain(1).nDomain=3;
domain(1).eDomain=2;
domain(1).sDomain=6;

domain(2).dnx=ng;
domain(2).dny=nr;
domain(2).basex=domain(1).basex+domain(1).dnx;
domain(2).basey=1;
domain(2).wFace='e';
domain(2).nFace='s';
domain(2).eFace='s';
domain(2).sFace='e';
domain(2).wDomain=1;
domain(2).nDomain=3;
domain(2).eDomain=4;
domain(2).sDomain=6;

domain(3).dnx=ng;
domain(3).dny=nb;
domain(3).basex=domain(2).basex+domain(2).dnx;
domain(3).basey=1;
domain(3).wFace='n';
domain(3).nFace='w';
domain(3).eFace='w';
domain(3).sFace='n';
domain(3).wDomain=1;
domain(3).nDomain=5;
domain(3).eDomain=4;
domain(3).sDomain=2;

domain(4).dnx=nr;
domain(4).dny=nb;
domain(4).basex=domain(3).basex+domain(3).dnx;
domain(4).basey=1;
domain(4).wFace='e';
domain(4).nFace='s';
domain(4).eFace='s';
domain(4).sFace='e';
domain(4).wDomain=3;
domain(4).nDomain=5;
domain(4).eDomain=6;
domain(4).sDomain=2;

domain(5).dnx=nr;
domain(5).dny=ng;
domain(5).basex=domain(4).basex+domain(4).dnx;
domain(5).basey=1;
domain(5).wFace='n';
domain(5).nFace='w';
domain(5).eFace='w';
domain(5).sFace='n';
domain(5).wDomain=3;
domain(5).nDomain=1;
domain(5).eDomain=6;
domain(5).sDomain=4;

domain(6).dnx=nb;
domain(6).dny=ng;
domain(6).basex=domain(5).basex+domain(5).dnx;
domain(6).basey=1;
domain(6).wFace='e';
domain(6).nFace='s';
domain(6).eFace='s';
domain(6).sFace='e';
domain(6).wDomain=5;
domain(6).nDomain=1;
domain(6).eDomain=2;
domain(6).sDomain=4;

domain_nx=domain(6).basex+domain(6).dnx-1;
domain_ny=max([domain(:).dny]);
 
return
