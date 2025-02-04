function [islands,ikeep]=ime_combine_islands(islands,mask)


%% IME_COMBINE_ISLANDS: only keeps the biggest island in each NaN patch, to reduce their numbers.
% This function is mostly used to generate the original island database, but is also useful when unning the IME detection on monthly maps 
% where clouds can create larger NaN patches, hence requiring more islands to be merged.
%
% [islands,ikeep]=ime_combine_islands(islands,mask)
%
% Inputs:
% 	islands:	structure containing (everything as vector of length the number of pixels) .lon, .lat, .Iname (island names, cell)
%						.reefID (unique ID corresponding to an area shallower than 30 m, since these can be shared across islands)
%						.Iarea (island area, km²), .Rarea (area shallower than 30 m including land, km²),
%						.is_NunnDB (true if belonging to the Nunn et al. database).
%	mask: 		
% 
% Outputs: 
% 	islands: 	same type of island structure only containing one island per NaN region in the mask, 
%				with Iarea/Rarea summed such that each island represents all islands within the NaN patch
%	ikeep: 		indices of islands kept
%		
% Monique Messié, 2021 for public version


% Finding all mask objects (adding a row of data to close contours)
[mask.nb_lat,mask.nb_lon]=size(mask.mask);
mask_NaN=ones(mask.nb_lat+2,mask.nb_lon+2); mask_NaN(2:end-1,2:end-1)=double(mask.mask);
mat_lon=ones(mask.nb_lon+2,1); mat_lon(2:end-1)=mask.lon; mat_lon(1)=mat_lon(2); mat_lon(end)=mat_lon(end-1);
mat_lat=ones(mask.nb_lat+2,1); mat_lat(2:end-1)=mask.lat; mat_lat(1)=mat_lat(2); mat_lat(end)=mat_lat(end-1);
objects_mask = bwconncomp(~logical(mask_NaN),8);
ilon_islands=nan(size(islands.lon)); ilat_islands=nan(size(islands.lat));
for iisland=1:length(islands.lon)
	dist_lon=abs(mat_lon-islands.lon(iisland));
	dist_lat=abs(mat_lat-islands.lat(iisland));
	ilon_islands(iisland)=find(dist_lon==min(dist_lon),1);
	ilat_islands(iisland)=find(dist_lat==min(dist_lat),1);
end
ilatlon_ind=sub2ind([length(mat_lat),length(mat_lon)],ilat_islands,ilon_islands);
iremove=false(size(islands.lon));

% loop on ask objects and combine islands within them
for ipts=1:objects_mask.NumObjects
	iislands=find(ismember(ilatlon_ind,objects_mask.PixelIdxList{ipts}));
	if ~isempty(iislands)
		% find which island to keep (iisland_keep) - NunnDB priority, then largest area
		if max(islands.is_NunnDB(iislands))
			iisland_keep=iislands(islands.is_NunnDB(iislands));
		else, iisland_keep=iislands;
		end
		liste_area=islands.Iarea(iisland_keep);
		if min(isnan(liste_area)) || max(liste_area)==0, liste_area=islands.Rarea(iisland_keep); end		% if only reefs present
		iisland_keep=iisland_keep(find(liste_area==max(liste_area),1));
		iremove(iislands(iislands~=iisland_keep))=true;
		% update Iarea/Rarea by summing them
		islands.Iarea(iislands)=sum(islands.Iarea(iislands),'omitnan');
		[~,ireef]=unique(islands.reefID(iislands));
		islands.Rarea(iislands)=max(sum(islands.Rarea(iislands(ireef)),'omitnan'),unique(islands.Iarea(iislands)));
	end
end
ikeep=~iremove;
for varname=fieldnames(islands)', varname=varname{:};
	if size(islands.(varname))==size(ikeep), islands.(varname)=islands.(varname)(ikeep); end
end


return


