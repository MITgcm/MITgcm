function [tile,ntiles,ierr,domain]= ...
         exch2_setup_cs6_tiles(tnx,tny,domain,ndomains,mapIO);

% $Header: /u/gcmpack/MITgcm/utils/exch2/matlab-topology-generator/Attic/exch2_setup_cs6_tiles.m,v 1.3 2007/03/21 02:02:12 jmc Exp $
% $Name:  $

ierr=0;
tileid=0;
nbPts=0;
for i=1:ndomains
 dnx=domain(i).dnx;
 dny=domain(i).dny;
 dbasex=domain(i).basex;
 dbasey=domain(i).basey;
 fprintf('Processing domain %d - size is %d x %d\n',i, ...
          domain(i).dnx,domain(i).dny);
 ntilex=dnx/tnx;
 ntiley=dny/tny;
 checkx=fix(dnx/tnx)*tnx-dnx;
 checky=fix(dny/tny)*tny-dny;
 if checkx ~= 0
  fprintf('Error in domain %d. Domain size in x (%d) is not an exact multiple of tile size in x (%d).\n',i,dnx,tnx);
  ierr=ierr+1;
 end
 if checky ~= 0
  fprintf('Error in domain %d. Domain size in x (%d) is not an exact multiple of tile size in x (%d).\n',i,dny,tny);
  ierr=ierr+1;
 end
 tileidbase=tileid;
 for jt=1:ntiley
  for it=1:ntilex
   tileid=tileid+1;
   tbasex=(it-1)*tnx;
   tbasey=(jt-1)*tny;
%- do global IO mapping according to mapIO
   if mapIO == -1,
   %- old format
     txgloballo=dbasex+tbasex;
     tygloballo=dbasey+tbasey;
   elseif mapIO == 0,
   %- compact format = 1 long line
     txgloballo=1+nbPts+tbasex+tbasey*dnx;
     tygloballo=1;
   else
   %- compact format: piled in the Y direction
     txgloballo=1+rem(nbPts+tbasex+tbasey*dnx,mapIO);
     tygloballo=1+floor((nbPts+tbasex+tbasey*dnx)/mapIO);
   end
   if ierr == 0 
    fprintf(' Tile %d - offset within domain %d, %d\n',tileid,tbasex,tbasey);
    fprintf('          base global coordinate %d, %d\n',txgloballo,tygloballo);
   end 
   tile(tileid).tnx=tnx;
   tile(tileid).tbasex=tbasex;
   tile(tileid).txgloballo=txgloballo;
   tile(tileid).tny=tny;
   tile(tileid).tbasey=tbasey;
   tile(tileid).tygloballo=tygloballo;
   tile(tileid).mydomain=i;
   tile(tileid).tileid=tileid;
   tile(tileid).tx=it;
   tile(tileid).ty=jt;
   tile(tileid).isWedge=0;
   tile(tileid).isNedge=0;
   tile(tileid).isEedge=0;
   tile(tileid).isSedge=0;
  end
 end
% Set the index range of tiles in each domain.
 domain(i).tileidlo=tileidbase+1;
 domain(i).tileidhi=tileid;
 nbPts=nbPts+dnx*dny;
end
ntiles=tileid;

return
