%% START_IME_TOOLBOX: examples to run the IME detection

% Notes:
% - these functions require Matlab's proprietary toolbox Image Processing.
% - when running the IME detection on a subset of islands, do not subset Chl spatially 
%	(the IME detection needs to be run on the full Chl map to ensure the algorithm does not stop prematurely when exiting the domain)
% - here are some crude reproductions of the IME figures in the paper without using additional toolboxes. Figures in the paper were produced using m_map 
%	(https://www.eoas.ubc.ca/~rich/map.html)



%% -------------- Reproduce Fig. 1: mean map -------------- %%

load('inputs/Chl_climatology.mat','Chl')
load('inputs/island_database.mat','islands')

% Chl time average
Chl.chl=mean(Chl.chl,3,'omitnan'); 
Chl=rmfield(Chl,'month');

% IME detection
[islands,Chl]=ime_detect_IMEs(Chl,islands); 

% Generate masks for different levels of Chl increase nearby islandss
levels_strength=[0 0.1 1];		% levels above 0 (all), above 10% and above 100%
Chl_increase_nearby=(islands.Chl_max-islands.Chl_REF)./islands.Chl_REF;	
mask_IME_levels=false(length(Chl.lat),length(Chl.lon),length(levels_strength)+1);
for ilevel=1:length(levels_strength)
	IME_abovelevel=double(islands.keep_IME==1 & Chl_increase_nearby>=levels_strength(ilevel));	% find IME where Chl_increase_nearby is above current level
	IME_abovelevel(isnan(islands.keep_IME))=NaN;												% remove shared IMEs
	mask_IME_level=Chl.mask_IME_all;					% start from all IMEs
	for iisland=find(IME_abovelevel==0)'				% then remove the ones that are not above each threshold
		dist_lon=abs(Chl.lon-islands.lon(iisland));
		dist_lat=abs(Chl.lat-islands.lat(iisland));
		mask_IME_level(bwselect(Chl.mask_IME_all,find(dist_lon==min(dist_lon),1),find(dist_lat==min(dist_lat),1),4))=0;
	end
	mask_IME_levels(:,:,ilevel)=mask_IME_level;			% each level contains a mask of all IMEs above a given threshold
end
	
% figure
figure, gf=gcf; gf.Position(3)=gf.Position(4)/60*170; hold on
pcolor(Chl.lon,Chl.lat,Chl.chl), shading flat
hbar=colorbar; caxis([0 0.25])
set(get(hbar,'title'),'string','mg m^{-3}');
scatter(islands.lon,islands.lat,10,'k','p')
h=struct(); colors_level=[[1 0.7 0.7];[1 0.17 0.17];[0.7 0 0]];
for ilevel=1:length(levels_strength)
	[~,h.(['level',num2str(ilevel)])]=...
		contour(Chl.lon,Chl.lat,double(mask_IME_levels(:,:,ilevel) & ~mask_IME_levels(:,:,ilevel+1)),...
		[0.5 0.5],'Color',colors_level(ilevel,:),'LineWidth',1);
end
legend([h.level1,h.level2,h.level3],{'IME regions (Chl increase nearby islands < 10%)','IME region (10% < Chl increase nearby islands < 100%)','IME region (Chl increase nearby islands > 100%)'})
xlabel('Longitude'), ylabel('Latitude'), title('IME detection (mean conditions)')
print('-djpeg','-r300','outputs/mean_IME.jpg')



%% -------------- Reproduce Fig. 1: F- Marquesas insert -------------- %%

region_lon=[-155 -137]+360; region_lat=[-13 -5]; region_month=5;		% update to reproduce other inserts
load('inputs/Chl_climatology.mat','Chl')
load('inputs/island_database.mat','islands')

% Chl time extraction
Chl.chl=Chl.chl(:,:,Chl.month==region_month); 
Chl=rmfield(Chl,'month');

% Island space extraction
iok = islands.lon>=region_lon(1) & islands.lon<=region_lon(2) & islands.lat>=region_lat(1) & islands.lat<=region_lat(2);
for varname=fieldnames(islands)', varname=varname{:}; islands.(varname)=islands.(varname)(iok); end

% IME detection
[islands,Chl]=ime_detect_IMEs(Chl,islands); 

% Generate masks for different levels of Chl increase nearby islandss (same as above)
levels_strength=[0 0.1 1];	
Chl_increase_nearby=(islands.Chl_max-islands.Chl_REF)./islands.Chl_REF;	
mask_IME_levels=false(length(Chl.lat),length(Chl.lon),length(levels_strength)+1);
for ilevel=1:length(levels_strength)
	IME_abovelevel=double(islands.keep_IME==1 & Chl_increase_nearby>=levels_strength(ilevel));
	IME_abovelevel(isnan(islands.keep_IME))=NaN;		
	mask_IME_level=Chl.mask_IME_all;	
	for iisland=find(IME_abovelevel==0)'
		dist_lon=abs(Chl.lon-islands.lon(iisland));
		dist_lat=abs(Chl.lat-islands.lat(iisland));
		mask_IME_level(bwselect(Chl.mask_IME_all,find(dist_lon==min(dist_lon),1),find(dist_lat==min(dist_lat),1),4))=0;
	end
	mask_IME_levels(:,:,ilevel)=mask_IME_level;	
end

% figure
ilon=Chl.lon>=region_lon(1) & Chl.lon<=region_lon(2);
ilat=Chl.lat>=region_lat(1) & Chl.lat<=region_lat(2);
figure, gf=gcf; gf.Position(3)=gf.Position(4)/(region_lat(2)-region_lat(1))*(region_lon(2)-region_lon(1)); hold on
pcolor(Chl.lon(ilon),Chl.lat(ilat),Chl.chl(ilat,ilon)), shading flat
hbar=colorbar; caxis([0.05 0.25])
set(get(hbar,'title'),'string','mg m^{-3}');
scatter(islands.lon,islands.lat,'k','p')
h=struct(); colors_level=[[1 0.7 0.7];[1 0.17 0.17];[0.7 0 0]];
for ilevel=1:length(levels_strength)
	[~,h.(['level',num2str(ilevel)])]=...
		contour(Chl.lon(ilon),Chl.lat(ilat),double(mask_IME_levels(ilat,ilon,ilevel) & ~mask_IME_levels(ilat,ilon,ilevel+1)),...
		[0.5 0.5],'Color',colors_level(ilevel,:),'LineWidth',1);
end
legend([h.level1,h.level2,h.level3],{'IME regions (Chl increase nearby islands < 10%)','IME region (10% < Chl increase nearby islands < 100%)','IME region (Chl increase nearby islands > 100%)'})
xlabel('Longitude'), ylabel('Latitude'), title(['IME detection (',datestr(datenum(1,region_month,1),'mmmm'),')'])
print('-djpeg','-r300','outputs/Marquesas_insert.jpg')



%% -------------- Reproduce Extended Data Fig. 1 (IME and REF regions) -------------- %%

region_lon=[172.5 192]; region_lat=[-25 -10]; region_month=8;
load('inputs/Chl_climatology.mat','Chl')
load('inputs/island_database.mat','islands')

% Chl time extraction
Chl.chl=Chl.chl(:,:,Chl.month==region_month); 
Chl=rmfield(Chl,'month');

% Island space extraction
iok = islands.lon>=region_lon(1) & islands.lon<=region_lon(2) & islands.lat>=region_lat(1) & islands.lat<=region_lat(2);
for varname=fieldnames(islands)', varname=varname{:}; islands.(varname)=islands.(varname)(iok); end

% IME detection
[islands,Chl]=ime_detect_IMEs(Chl,islands); 

% figure
ilon=Chl.lon>=region_lon(1) & Chl.lon<=region_lon(2);
ilat=Chl.lat>=region_lat(1) & Chl.lat<=region_lat(2);
figure, gf=gcf; gf.Position(3)=gf.Position(4)/(region_lat(2)-region_lat(1))*(region_lon(2)-region_lon(1)); hold on
pcolor(Chl.lon(ilon),Chl.lat(ilat),Chl.chl(ilat,ilon)), shading flat
hbar=colorbar; caxis([0 0.15])
set(get(hbar,'title'),'string','mg m^{-3}');
scatter(islands.lon,islands.lat,'k','p')
[~,h1]=contour(Chl.lon(ilon),Chl.lat(ilat),double(Chl.mask_IME_all(ilat,ilon)),[0.5 0.5],'r','LineWidth',2);
[~,h2]=contour(Chl.lon(ilon),Chl.lat(ilat),double(Chl.mask_REF_all(ilat,ilon)),[0.5 0.5],'b','LineWidth',1);
legend([h1,h2],{'IME region','REF region'})
xlabel('Longitude'), ylabel('Latitude'), title('IME detection (red) and REF region (blue)')
print('-djpeg','-r300','outputs/IME_and_REF_regions.jpg')



%% -------------- Run on the full climatological datasets (will be long to run) -------------- %%

load('inputs/Chl_climatology.mat','Chl')
load('inputs/island_database.mat','islands')
liste_outputs={'cChl','Chl_max','Chl_min','Chl_REF','Chl_IME','area_IME','keep_IME','has_IME','reason_stop','islandIME'};

% Prepare monthly outputs
islands_climato=islands;
for varname=liste_outputs(1:length(liste_outputs)-2), varname=varname{:}; islands_climato.(varname)=nan(length(islands.lon),length(Chl.month)); end
for varname=liste_outputs(length(liste_outputs)-1:end), varname=varname{:}; islands_climato.(varname)=cell(length(islands.lon),length(Chl.month)); end

% IME detection, looping on each month
% Note - IME/REF masks are not kept here due to size constrains, but are available as outputs of ime_detect_IMEs.
for imonth=1:length(Chl.month), disp(['Processing month #',num2str(imonth)])
	Chl_month=rmfield(Chl,'month');
	Chl_month.chl=Chl.chl(:,:,imonth);
	islands_month=ime_detect_IMEs(Chl_month,islands); 
	for varname=liste_outputs, varname=varname{:};
		islands_climato.(varname)(:,imonth)=islands_month.(varname);
	end
end

% Save the result
save('outputs/IME_climato.mat','islands_climato')



%% -------------- Calculate IME algorithm statistics -------------- %%

load('outputs/IME_climato.mat','islands_climato')

% Statistics on reasons for the IME algorithm to stop (over 7968 potential detections)
is_processed=true(size(islands_climato.reason_stop));
for reason={'not in Chl domain','in extended continent mask','combined within NaN pixels','already done'}, reason=reason{:};
	is_reason=cellfun(@(x) contains(x,reason),islands_climato.reason_stop(:));
	is_processed(is_reason)=false;
	disp([reason,': ',num2str(sum(is_reason))])
end
N=sum(is_processed(:)); disp('--- Criteria: ---')
for reason={'cChl less than cChl min','exits grid domain','touching continent','hitting rank ratio'}, reason=reason{:};
	is_reason=cellfun(@(x) contains(x,reason),islands_climato.reason_stop(:));
	disp([reason,': ',num2str(sum(is_reason)),' (',num2str(round(sum(is_reason)/N*1000)/10),'%)'])
end



%% -------------- Calculate a few statistics from Table 1 -------------- %%

load('outputs/IME_climato.mat','islands_climato')
Chl_increase_nearby=(islands_climato.Chl_max-islands_climato.Chl_REF)./islands_climato.Chl_REF;

% Stats on islands
N=length(islands_climato.lon);
has_IME = islands_climato.has_IME==1;
has_IME_above10pct = has_IME & Chl_increase_nearby>=0.1;
pct_IME_atleast_onemonth=sum(sum(islands_climato.has_IME>0,2)>0)/N*100
pct_IME_atleast_sixmonths=sum(sum(islands_climato.has_IME>0,2)>=6)/N*100
pct_IME_yearlong=sum(sum(islands_climato.has_IME>0,2)==12)/N*100
pct_IME_detection=sum(has_IME(:))/(N*12)*100
pct_IME_detection_above10pct=sum(has_IME_above10pct(:))/(N*12)*100

% Stats on area - area_IME is only given once per IME (for the lead island, otherwise stays at NaN) so already at NaN for shared IMEs
avg_IME_area=mean(islands_climato.area_IME(:),'omitnan')		
total_IME_area=mean(sum(islands_climato.area_IME,1,'omitnan'))

% Stats on Chl - now excluding shared (ie statistics on IMEs, not on islands)
has_IME = islands_climato.has_IME==1 & ~isnan(islands_climato.keep_IME);
has_IME_above10pct = has_IME & Chl_increase_nearby>=0.1;
avg_Chl_increase_nearby=mean(Chl_increase_nearby(has_IME),'omitnan')*100
avg_Chl_increase_perIME=mean((islands_climato.Chl_IME(has_IME)-islands_climato.Chl_REF(has_IME))./islands_climato.Chl_REF(has_IME),'omitnan')*100	
Chl_increase=(islands_climato.Chl_IME-islands_climato.Chl_REF).*islands_climato.area_IME*1E6/1E3;	% gChl/m (area_IME is in km²)
Chl_increase(~has_IME)=NaN;
avg_total_Chl_increase_perIME=mean(Chl_increase(:)/1E6,'omitnan')	% tons/m
total_Chl_increase=mean(sum(Chl_increase,1,'omitnan'))/1E6
Chl_increase(~has_IME_above10pct)=NaN;
total_Chl_increase_IMEabove10pct=mean(sum(Chl_increase,1,'omitnan'))/1E6