function grid = mit_loadgrid(varargin);
% function grid = mit_loadgrid(dname);
% load the geometry of an arbitrary run with the mitgcm

% Aug 15, 2002: fixed a bug (?): yc' -> yc 

  if nargin == 0
    dname = '.';
  else
    dname = varargin{1};
  end
  % tracer time step
  deltattracer = mit_getparm(fullfile(dname,'data'),'deltaTtracer');
  if isempty(deltattracer)
    error('deltaTtracer is empty')
  end
  gravity = mit_getparm(fullfile(dname,'data'),'gravity');
  if isempty(gravity);
    disp('assuming gravity = 9.81')
    gravity = 9.81;
  end
  rhoNil  = mit_getparm(fullfile(dname,'data'),'rhonil');
  if isempty(rhoNil);
    rhoNil  = mit_getparm(fullfile(dname,'data'),'rhoNil');
  end
  if isempty(rhoNil);
    rhoNil  = mit_getparm(fullfile(dname,'data'),'rhoConst');
  end
  if isempty(rhoNil);
    rhoNil  = mit_getparm(fullfile(dname,'data'),'rhoconst');
  end
  if isempty(rhoNil);
    disp('assuming rhoNil = 1035')
    rhoNil = 1035;
  end
  % determine vertical coordinates
  br = mit_getparm(fullfile(dname,'data'),'buoyancyRelation');
  if isempty(br)
    disp('assuming buoyancyRelation = OCEANIC')
    br = 'OCEANIC';
  end
  if ~strcmpi(br,'OCEANIC');
    pfac = 1/rhoNil/gravity;
  else
    pfac = 1;
  end
  % create masks for plotting
  hfac=rdmds(fullfile(dname,'hFacC')); hfacc=change(hfac,'==',0,NaN); 
  hfac=rdmds(fullfile(dname,'hFacW')); hfacw=change(hfac,'==',0,NaN);
  hfac=rdmds(fullfile(dname,'hFacS')); hfacs=change(hfac,'==',0,NaN); 
  clear hfac;
  cmask=change(hfacc,'>',0,1);
  umask=change(hfacw,'>',0,1);
  vmask=change(hfacs,'>',0,1);
  
  [nx ny nz] = size(cmask);
  
  % find the index of the deepest wet tracer-cell
  klowc = sum(change(cmask,'==',NaN,0),3);
  
  % create grid parameters
  dz = mit_getparm(fullfile(dname,'data'),'delR');
  if isempty(dz);
    disp('trying delZ')
    dz = mit_getparm(fullfile(dname,'data'),'delZ');
    if isempty(dz)
      error('vertical grid cannot be established')
    end
  end
  if size(dz,1) == 1;
    dz = dz';
  end
  if length(dz) ~= nz
    error('dz could not be retrieved correctly from the data file')
  end
  dz = dz*pfac;

  if ~strcmp(br,'OCEANIC');
    dz = dz(end:-1:1);
  end
  zgpsi = [0;cumsum(dz)];
  zc = .5*(zgpsi(1:nz)+zgpsi(2:nz+1));
  zg=zgpsi(1:nz);
  
  xc = rdmds(fullfile(dname,'XC'));
  yc = rdmds(fullfile(dname,'YC'));
  xg = rdmds(fullfile(dname,'XG'));
  yg = rdmds(fullfile(dname,'YG'));
  dxc = rdmds(fullfile(dname,'DXC'));
  dyc = rdmds(fullfile(dname,'DYC'));
  dxg = rdmds(fullfile(dname,'DXG'));
  dyg = rdmds(fullfile(dname,'DYG'));

  lonc = xc(:,1);
  latc = yc(1,:)';
  long = xg(:,1);
  latg = yg(1,:)';
    
  % depth
  depth = rdmds(fullfile(dname,'Depth'))*pfac;
  
  % current directory
  if strcmp(dname,'.') | strcmp(dname,'./')
    [pathstr,dirname,ext]=fileparts(pwd);
    dirname = [dirname ext];
  else
    [pathstr,dirname,ext]=fileparts(fullfile(pwd,dname));
    dirname = [dirname ext];
  end

  ius = findstr(dirname,'_');
  if ~isempty(ius)
    for k=1:length(ius)
      dirname = [dirname(1:ius(k)-1) '\' dirname(ius(k):end)];
      ius = ius+1;
    end
  end
  % 
  if ~strcmp(br,'OCEANIC');
    hfacc = hfacc(:,:,end:-1:1);
    hfacs = hfacs(:,:,end:-1:1);
    hfacw = hfacw(:,:,end:-1:1);
    cmask = cmask(:,:,end:-1:1);
    umask = umask(:,:,end:-1:1);
    vmask = vmask(:,:,end:-1:1);
    klowc = nz - klowc + 1;
    klowc(find(klowc==nz+1)) = 0;
  end

  % area and volume of grid cells has to be done after flipping the masks 
  % vertically
  rac = rdmds(fullfile(dname,'RAC')).*cmask(:,:,1);
  ras = rdmds(fullfile(dname,'RAS')).*vmask(:,:,1);
  raw = rdmds(fullfile(dname,'RAW')).*umask(:,:,1);
  rac3d = repmat(rac,[1 1 nz]).*hfacc;
  volc = rac3d.*permute(repmat(dz,[1 nx ny]),[2 3 1]);
  % create the structure
  % Aug 15, 2002: fixed a bug (?): yc' -> yc 
  grid = struct('dname',dirname, ...
		'deltattracer',deltattracer, ...
		'gravity',gravity','rhonil',rhoNil, ...
                'buoyancy',br,'pfac',pfac, ...
		'nx',nx,'ny',ny,'nz',nz, ...
		'xc',xc,'yc',yc,'xg',xg,'yg',yg, ...
		'lonc',lonc,'latc',latc,'long',long,'latg',latg, ...
		'dxc',dxc,'dyc',dyc,'dxg',dxg,'dyg',dyg, ...
		'rac',rac,'ras',ras,'raw',raw, ...
		'dz',dz,'zc',zc,'zg',zg,'zgpsi',zgpsi, ...
		'depth',depth,'hfacc',hfacc,'hfacs',hfacs,'hfacw',hfacw, ...
		'cmask',cmask,'umask',umask,'vmask',vmask,'volc',volc, ...
		'klowc',klowc);
  
  return
