% Generate blanklist.example.txt for baseline cs32 setup
% Requires:
% MITgcm/verification/global_ocean.cs32x15/input/bathy_Hmin50.bin

addpath ../../matlab/cs_grid/read_cs    % readbin.m location
nr=32;                                  % face dimension
nf=6;                                   % number of faces
tnx=8;tny=4;                            % tile subgrid sizes

fname='../../../verification/global_ocean.cs32x15/input/bathy_Hmin50.bin';
bathy=readbin(fname,[nr*6 nr],1,'real*8');
bathy(find(bathy))=1;                   % surface coastline mask

% map the surface coastline and plot tiles
clf
image(bathy','CDataMapping','scaled');
set(gca,'ydir','normal')
hold on
for i=(tnx+.5):tnx:(nr*nf)
  plot([i i],[.5 (nr+.5)],'k')
end
for i=(tny+.5):tny:nr
  plot([.5 (nr*nf+.5)],[i i],'k')
end

% generate blanklist and number the tiles in the figure
blanklist=[];
for f=1:nf
  for j=1:(nr/tny)
    for i=1:(nr/tnx)
      i1=(f-1)*nr+(i-1)*tnx+1;
      i2=(f-1)*nr+i*tnx;
      j1=(j-1)*tny+1;
      j2=j*tny;
      n=(f-1)*nr+(j-1)*tny+i;
      if sum(sum(bathy(i1:i2,j1:j2)))==0
        blanklist=[blanklist n];
      end
      text(i1+2,j1+2,int2str(n))
    end
  end
end
disp(blanklist)
