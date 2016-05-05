function FDR_adjust_nii(input,mask)

    % Read 2-tailed map of 1-p in NII format, FDR adjust, and output
    
%     clear all
%     wdir = '/Volumes/HDD/Google Drive/Sean/Projects/Comp-Robot/MEG/final/';
%     cd(wdir);
%     input = 'main_s2-s1.nii.gz';
%     mask = 'res_10mm_gm.nii';

    dat = load_nii(input);
    mask = load_nii(mask);
    
    dat.img = dat.img .* mask.img;

    pvals = reshape(dat.img,[],1);
    pvals = pvals(pvals>0 | pvals<0);

    signmask = pvals ./ abs(pvals);
    pvals = 1 - abs(pvals);

    [~, ~, ~, adj_p]=fdr_bh(pvals);
    adj_p(adj_p>1)=1;

    newp = (1-adj_p) .* signmask;

    temp = reshape(dat.img,[],1);

    temp(temp>0 | temp<0) = newp;
    d1 = size(dat.img,1);
    d2 = size(dat.img,2);
    d3 = size(dat.img,3);
    dat.img = reshape(temp,d1,d2,d3);

    output = ['FDR_' input];
    save_nii(dat,output);

end