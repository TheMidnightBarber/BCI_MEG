clear all
cd('~/Downloads/nodes');
dat = load_nii('template.nii');

sx = dat.hdr.dime.pixdim(2);
sy = dat.hdr.dime.pixdim(3);
sz = dat.hdr.dime.pixdim(4);
dx = size(dat.img,1);
dy = size(dat.img,2);
dz = size(dat.img,3);

% Get node locations in mm (Talairach)

% fid = fopen('area_coords_RAS_plusCerebellum.txt');
fid = fopen('278nodes.txt');
locs = textscan(fid,'%.0f %.0f %.0f');
fclose(fid);
locs = horzcat(locs{1},locs{2},locs{3});

% Convert to voxel locations

locs(:,1) = round(locs(:,1)/sx+(dx/2));
locs(:,2) = round(locs(:,2)/sy+(dy/2)) + 4;
locs(:,3) = round(locs(:,3)/sz+(dz/2)) - 1;
locs(locs==0)=1;

% Create output grid

output = dat;
output.img(:,:,:) = 0;

for i = 1:size(locs,1)
    lx = locs(i,1);
    ly = locs(i,2);
    lz = locs(i,3);
    output.img(lx,ly,lz) = i;
end

save_nii(output,'278nodes.nii');