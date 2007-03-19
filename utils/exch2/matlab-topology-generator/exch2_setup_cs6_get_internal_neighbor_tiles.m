function [tile] = ...
         exch2_setup_cs6_get_internal_neighbor_tiles(tile, domain, ntiles)
% Figure out what points I send my edges to. We do this by a search procedure 
% rather than a functional relationship. The search procedure visits each edge 
% of each tile in turn.  For internal edges (edges that don't cross a domain 
% boundary) the index range at +/-1 in the normal direction to the edge is 
% searched for. This identifies all the tiles that % border this tile.

% $Header: /u/gcmpack/MITgcm/utils/exch2/matlab-topology-generator/Attic/exch2_setup_cs6_get_internal_neighbor_tiles.m,v 1.2 2007/03/19 20:34:26 jmc Exp $
% $Name:  $

for it=1:ntiles
  tile(it).nW=0;
  tile(it).wTile(1)=0;
  tile(it).nE=0;
  tile(it).eTile(1)=0;
  tile(it).nS=0;
  tile(it).sTile(1)=0;
  tile(it).nN=0;
  tile(it).nTile(1)=0;
% First do tiles that are internal to the domain
  if tile(it).wDomain==tile(it).mydomain
%  Now need to find remote tile(s) that have target points on the line one to the
%  west and that have the north-south extents of this tile.
   rxHi= tile(it).tbasex;
   if rxHi == 0 
    rxHi = domain(tile(it).mydomain).dnx;
   end
   ryLo=            1+tile(it).tbasey;
   ryHi= tile(it).tny+tile(it).tbasey;
   nW=0;
   dom=tile(it).mydomain;
   for j=domain(dom).tileidlo:domain(dom).tileidhi
    if   tile(j).tnx+tile(j).tbasex == rxHi  ...
       & 1+tile(j).tbasey           >= ryLo  ...
       & tile(j).tny+tile(j).tbasey <= ryHi
     nW=nW+1;
     tile(it).nW=nW;
     tile(it).wTile(nW)=tile(j).tileid;
    end
   end
   if nW > 0
    for j=1:nW
     k=tile(it).wTile(j);
    end
   end
  end
  if tile(it).eDomain==tile(it).mydomain
%  Now need to find remote tile(s) that have target points on the line one to the
%  east and that have north-south extents of this tile.
   rxLo= tile(it).tnx+tile(it).tbasex+1;
   ryLo=            1+tile(it).tbasey;
   ryHi= tile(it).tny+tile(it).tbasey;
   nE=0;
   dom=tile(it).mydomain;
   for j=domain(dom).tileidlo:domain(dom).tileidhi
    if   1          +tile(j).tbasex == rxLo  ...
       & 1+tile(j).tbasey           >= ryLo  ...
       & tile(j).tny+tile(j).tbasey <= ryHi
     nE=nE+1;
     tile(it).nE=nE;
     tile(it).eTile(nE)=tile(j).tileid;
    end
   end
  end
  if tile(it).nDomain==tile(it).mydomain
%  Now need to find remote tile(s) that have target points on the line one to the
%  north and that have east-west extents of this tile.
   rxLo=            1+tile(it).tbasex;
   rxHi= tile(it).tnx+tile(it).tbasex;
   ryLo= tile(it).tny+tile(it).tbasey+1;
   nN=0;
   dom=tile(it).mydomain;
   for j=domain(dom).tileidlo:domain(dom).tileidhi
    if   1          +tile(j).tbasey == ryLo  ...
       & 1+tile(j).tbasex           >= rxLo  ...
       & tile(j).tnx+tile(j).tbasex <= rxHi
     nN=nN+1;
     tile(it).nN=nN;
     tile(it).nTile(nN)=tile(j).tileid;
    end
   end
  end
  if tile(it).sDomain==tile(it).mydomain
%  Now need to find remote tile(s) that have target points on the line one to the
%  south and that have east-west extents of this tile.
   rxLo=            1+tile(it).tbasex;
   rxHi= tile(it).tnx+tile(it).tbasex;
   ryHi= tile(it).tbasey;
   if ryHi == 0 
    ryHi = domain(tile(it).mydomain).dny;
   end
   nS=0;
   dom=tile(it).mydomain;
   for j=domain(dom).tileidlo:domain(dom).tileidhi
    if   tile(j).tbasey+tile(j).tny == ryHi  ...
       & 1+tile(j).tbasex           >= rxLo  ...
       & tile(j).tnx+tile(j).tbasex <= rxHi
     nS=nS+1;
     tile(it).nS=nS;
     tile(it).sTile(nS)=tile(j).tileid;
    end
   end
  end
end
% and what transformations to apply to the canonical index
% Should include figuring out the neighbor tiles here? There could be more than
% one for a face. How do figure this out?
return
