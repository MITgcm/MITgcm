function [tile, domain] = exch2_setup_squeeze_blanks( domain, tile, tnx, tny, ntile)
%
% Remove tiles that are listed as blank so that the resultant topology has tile
% ranks that only include the tiles bot listed as blank. This is used to permit
% simulation in which continent points are omitted from an ocean simulation.

% $Header: /u/gcmpack/MITgcm/utils/exch2/matlab-topology-generator/Attic/exch2_setup_squeeze_blanks.m,v 1.2 2007/03/19 20:34:26 jmc Exp $
% $Name:  $

fstate=exist('blanklist.txt','file');
if fstate ~= 0
 load blanklist.txt
 blanklist=sort(blanklist);
else
 blanklist=zeros(0);
end

% Create the renumber list. After this loop
% tnum(2,:) contains the original numbering
% tnum(1,:) contains the new numbering
tnum=zeros(2,length(tile));
for i=1:length(tile)
 tnum(1,i)=i;
 tnum(2,i)=i;
end
for i=1:length(blanklist)
 k=find([tile.tileid]==blanklist(i));
 tnum(1,k+1:end)=tnum(1,k+1:end)-1;
 tnum(1,k)=0;
end

% Now assign new numbers
% Need to renumber::
%                   tileid
%                   wTile
%                   eTile
%                   sTile
%                   nTile
for i=1:length(tnum)
 it=find([tile.tileid]==tnum(2,i));
 for j=1:length(it)
  tile(it(j)).tileid=tnum(1,i);
 end
 for k=1:length(tile)
  it=find([tile(k).wTile]==tnum(2,i));
  for j=1:length(it)
   tile(k).wTile(it(j))=tnum(1,i);
  end
  it=find([tile(k).nTile]==tnum(2,i));
  for j=1:length(it)
   tile(k).nTile(it(j))=tnum(1,i);
  end
  it=find([tile(k).eTile]==tnum(2,i));
  for j=1:length(it)
   tile(k).eTile(it(j))=tnum(1,i);
  end
  it=find([tile(k).sTile]==tnum(2,i));
  for j=1:length(it)
   tile(k).sTile(it(j))=tnum(1,i);
  end
 end
end

inb=find([tile.tileid]~=0);
tilenew=tile(inb);
tile=tilenew;

% Update the domain sizes too
for i=1:6
 id=find([tile.mydomain]==i);
 idmax=max([tile(id).tileid]);
 idmin=min([tile(id).tileid]);
 domain(i).tileidlo=idmin;
 domain(i).tileidhi=idmax;
end

% Remove zero references from neighbor tables
for i=1:length(tile)
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
end

return
