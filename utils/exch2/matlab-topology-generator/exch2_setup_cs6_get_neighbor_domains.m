function [tile] = exch2_setup_cs6_get_neighbor_domains(tile, domain,ntiles);
% Set neighboring domain fiel for each tile

% $Header: /u/gcmpack/MITgcm/utils/exch2/matlab-topology-generator/Attic/exch2_setup_cs6_get_neighbor_domains.m,v 1.2 2007/03/19 20:34:26 jmc Exp $
% $Name:  $

for it=1:ntiles
 dlox=tile(it).tbasex+1;
 dhix=tile(it).tbasex+tile(it).tnx;
 dloy=tile(it).tbasey+1;
 dhiy=tile(it).tbasey+tile(it).tny;
 tile(it).wDomain=tile(it).mydomain;
 tile(it).nDomain=tile(it).mydomain;
 tile(it).eDomain=tile(it).mydomain;
 tile(it).sDomain=tile(it).mydomain;
 if dlox == 1
  tile(it).wDomain=domain(tile(it).mydomain).wDomain;
 end
 if dloy == 1
  tile(it).sDomain=domain(tile(it).mydomain).sDomain;
 end
 if dhix == domain(tile(it).mydomain).dnx
  tile(it).eDomain=domain(tile(it).mydomain).eDomain;
 end
 if dhiy == domain(tile(it).mydomain).dny
  tile(it).nDomain=domain(tile(it).mydomain).nDomain;
 end
end

return
