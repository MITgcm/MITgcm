function [tile, domain] = exch2_setup_squeeze_blanks( domain, tile, tnx, tny, ntile)
%
% Remove tiles that are listed as blank so that the resultant topology ignores
% tiles listed as blank. This is used to permit simulation in which continent 
% points are omitted from an ocean simulation.

% $Header: /u/gcmpack/MITgcm/utils/exch2/matlab-topology-generator/Attic/exch2_setup_squeeze_blanks.m,v 1.3 2009/04/11 05:21:47 jmc Exp $
% $Name:  $

fstate=exist('blanklist.txt','file');
if fstate ~= 0
 load blanklist.txt
 blanklist=sort(blanklist);
else
 blanklist=zeros(0);
end

% Flag tiles to ignore
for i=1:length(blanklist)
 bt=find([tile.tileid]==blanklist(i));
 tile(bt).tileid=0;
 tile(bt).mydomain=0;
 tile(bt).nW=0;
 tile(bt).nE=0;
 tile(bt).nS=0;
 tile(bt).nN=0;
%- remove reference to this tile from the W,N,E,S neighbour list:
 for k=1:length(tile)
  it=find([tile(k).wTile]==bt);
  for j=1:length(it)
   tile(k).wTile(it(j))=0;
  end
  it=find([tile(k).nTile]==bt);
  for j=1:length(it)
   tile(k).nTile(it(j))=0;
  end
  it=find([tile(k).eTile]==bt);
  for j=1:length(it)
   tile(k).eTile(it(j))=0;
  end
  it=find([tile(k).sTile]==bt);
  for j=1:length(it)
   tile(k).sTile(it(j))=0;
  end
 end
end

% Update the domain sizes
for i=1:6
 id=find([tile.mydomain]==i);
 if length(id) > 0,
  idmax=max([tile(id).tileid]);
  idmin=min([tile(id).tileid]);
  domain(i).tileidlo=idmin;
  domain(i).tileidhi=idmax;
 else
  domain(i).tileidlo=0;
  domain(i).tileidhi=0;
 end
end

% Remove zero references from neighbour tables
for i=1:length(tile), if tile(i).tileid > 0,
%West
 nList=tile(i).wTile;
 iKeep=find(nList~=0);
 tile(i).wTile=nList(iKeep);
 tile(i).nW=length(iKeep);
%North
 nList=tile(i).nTile;
 iKeep=find(nList~=0);
 tile(i).nTile=nList(iKeep);
 tile(i).nN=length(iKeep);
%South
 nList=tile(i).sTile;
 iKeep=find(nList~=0);
 tile(i).sTile=nList(iKeep);
 tile(i).nS=length(iKeep);
%East
 nList=tile(i).eTile;
 iKeep=find(nList~=0);
 tile(i).eTile=nList(iKeep);
 tile(i).nE=length(iKeep);
end ; end

return
