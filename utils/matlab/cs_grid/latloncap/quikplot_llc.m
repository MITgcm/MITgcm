function quikplot_llc(fld);

% Function quikplot_llc(fld)
% plot lat-lon-cap field
%
% INPUTS
% fld  input array of dimension nx*nx*13

if nargin < 1, error('please specify field to plot'); end

[nx ny]=size(fld);
ny=nx*3;

% read face 1, nx*nx*3
f1=fld(:,1:(nx*3));

% read face 2, nx*nx*3
f2=fld(:,(nx*3+1):(nx*6));

% read face 3, nx*nx
f3=fld(:,(nx*6+1):(nx*7));

% read face 4, nx*3*nx
f4=f1';
for f=8:10
    i1=(1:nx)+(f-8)*nx;
    i2=(1:3:(nx*3))+7*nx+f-8;
    f4(i1,:)=fld(:,i2);
end

% read face 5, nx*3*nx
f5=f1';
for f=11:13
    i1=(1:nx)+(f-11)*nx;
    i2=(1:3:(nx*3))+10*nx+f-11;
    f5(i1,:)=fld(:,i2);
end

% plot field
f=nan*ones(4*nx,ny+nx/2);
f(1:nx,1:ny)=f1;
f((nx+1):(2*nx),1:ny)=f2;
f(1:nx,(ny+1):(ny+nx/2))=rot90(f3(1:(nx/2),:),1);
f((2*nx+1):(3*nx),(ny+1):(ny+nx/2))=rot90(f3((nx/2+1):nx,:),3);
f((2*nx+1):(3*nx),1:ny)=rot90(f4,3);
f((3*nx+1):(4*nx),1:ny)=rot90(f5,3);
quikpcolor(f');
set(gca,'xtick',[],'ytick',[])
