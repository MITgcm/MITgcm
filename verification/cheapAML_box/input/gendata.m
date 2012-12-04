clear;

ieee='b';
accuracy='real*8';

H=1000;
nx=100;
ny=100;
nz=1;

% =======================  TOPOGRAPHY ========================================= 

% Flat bottom at z=-Ho

h=-H*ones(nx,ny);



% Walls
h(1,:)   = 0.0;
h(end,:) = 0.0;
h(:,1)   = 0.0;
h(:,end) = 0.0;

% land
for n1=1:nx
for n2=1:ny
  if (n2>60 && n1<n2)
    h(n1,n2) = 0.0;
  end
end
end

fid=fopen('topog.box','w',ieee); fwrite(fid,h,accuracy); fclose(fid);

