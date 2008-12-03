function [flt,data,header] = read_flt_traj(fName)
% Reads the float_trajectories files.
%
% returns a structured array with fields 'time','x','y','k','u','v','t','s','p'
%
% eg.
% >> flts=read_flt_traj('float_trajectories');
% >> plot( flts(3).time, flts(3).x/1e3 )
% >> for k=1:126;plot(flts(k).x/1e3,flts(k).y/1e3);hold on;end;hold off

imax=10;		% record size
ieee='b';		% IEEE big-endian format
bytesPerRec=imax*8;	% 8 bytes per real*8

fls=dir([fName '.*data']);

data=zeros(imax,0);
header=zeros(imax,0);

% Read everything
for k=1:size(fls,1)
 fid=fopen(fls(k).name,'r',ieee);
 nrecs=fls(k).bytes/bytesPerRec;
 ldata=fread(fid,[imax nrecs],'real*8');
 fclose(fid);
 header=[header ldata(:,1)];
 data=[data ldata(:,2:end)];
end

flt=struct('numsteps',[],'time',[],'x',[],'y',[],'k',[]);

% Sort it all out
for k=1:max(max(data(1,:)));
 j=find( data(1,:)==k );
 [t,jj]=sort( data(2,j) ); j=j(jj);
 flt(k).time=data(2,j);
 flt(k).x=data(3,j);
 flt(k).y=data(4,j);
 flt(k).k=data(5,j);
 flt(k).u=data(6,j);
 flt(k).v=data(7,j);
 flt(k).t=data(8,j);
 flt(k).s=data(9,j);
 flt(k).p=data(10,j);
end
