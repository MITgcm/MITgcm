% purpose: find location of grdchk points from "data.grdchk" settings

%- from a model run:
hFacC=rdmds('hFacC');
dims=size(hFacC); nx=dims(1); ny=dims(2); nr=dims(3);

%- from SIZE.h :
sNx=nx/2 ; sNy=ny/2 ;

%- from data.grdchk :
 iGloPos = 31;
 jGloPos = 7;
 kGloPos = 1;
 iGloTile = 2;
 jGloTile = 2;

%- from data.grdchk :
 nstep = 450; nend = 1700;

%- count number of wet-points in this tile(iGloTile,jGloTile):
nSx=nx/sNx; nSy=ny/sNy;

msk=reshape(ceil(hFacC),[sNx nSx sNy nSy nr]);
mskTile=squeeze(msk(:,iGloTile,:,jGloTile,:));

cumMsk=cumsum(reshape(mskTile,[sNx*sNy*nr 1]));

%- find location:
 ic1=iGloPos; jc1=jGloPos; kc1=kGloPos;
 ijk1=ic1+(jc1-1)*sNx+(kc1-1)*sNx*sNy;
 yy1=cumMsk(ijk1);

 for nstep=450,
%for nstep=[450 560 553],
    %-- find the location of all grdchk points:
   fprintf('In tile (%i,%i), nstep= %4i , (ic,jc,kc) = ( %2i , %2i , %2i ) :\n', ...
                iGloTile,jGloTile, nstep,ic1,jc1,kc1);
   for n=0:nstep:nend
     [I]=find(cumMsk==(yy1+n));
     is=I(1)-1;          kg=1+floor(is/(sNx*sNy));
     is=rem(is,sNx*sNy); jg=1+floor(is/sNx);
     is=rem(is,sNx);     ig=1+is;
     fprintf(' #%6i : i,j,k = %2i , %2i , %2i\n',n,ig,jg,kg);
   end
   fprintf(' ------------\n');
 end

return
