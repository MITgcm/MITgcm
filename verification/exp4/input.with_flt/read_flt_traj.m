function [flt,data,header] = read_flt_traj(varargin)
% Reads the float_trajectories files.
%
% flts=read_flt_traj(File_Names,[Worldlength]);
% input Worldlength (= 4 or 8) is optional
% returns a structured array with fields 'time','x','y','k','u','v','t','s','p'
%
% eg.
% >> flts=read_flt_traj('float_trajectories',4);
% >> plot( flts(3).time, flts(3).x/1e3 )
% >> for k=1:126;plot(flts(k).x/1e3,flts(k).y/1e3);hold on;end;hold off

fName = varargin{1};
imax=13;                  % record size
ieee='b';                 % IEEE big-endian format
WORDLENGTH = 8;           % 8 bytes per real*8
if length(varargin)==2
   WORDLENGTH = varargin{2};
end
bytesPerRec=imax*WORDLENGTH;
rtype =['real*',num2str(WORDLENGTH)];

[I]=strfind(fName,'/');
if length(I) == 0,
 bDr='';
else
 fprintf(' found Dir Sep in file name (');
 fprintf(' %i',I);
 bDr=fName(1:I(end));
 fprintf(' ) ; load files from Dir "%s"\n',bDr);
end

fls=dir([fName,'.*data']);

data=zeros(imax,0);
header=zeros(imax,0);

% Read everything
for k=1:size(fls,1)
 fid=fopen([bDr,fls(k).name],'r',ieee);
%fprintf('fid= %i\n',fid);
 nrecs=fls(k).bytes/bytesPerRec;
 ldata=fread(fid,[imax nrecs],rtype);
 fclose(fid);
 header=[header ldata(:,1)];
 data=[data ldata(:,2:end)];
 clear ldata;
end

flt=struct('numsteps',[],'time',[],'x',[],'y',[],'z',[]);

% Sort it all out
for k=1:max(max(data(1,:)));
 j=find( data(1,:)==k );
 [t,jj]=sort( data(2,j) ); j=j(jj);
 flt(k).time=data(2,j);
 flt(k).x=data( 3,j);
 flt(k).y=data( 4,j);
 flt(k).z=data( 5,j);
 flt(k).i=data( 6,j);
 flt(k).j=data( 7,j);
 flt(k).k=data( 8,j);
 flt(k).p=data( 9,j);
 flt(k).u=data(10,j);
 flt(k).v=data(11,j);
 flt(k).t=data(12,j);
 flt(k).s=data(13,j);
end

return
