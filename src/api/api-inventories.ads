with Ada.Containers.Vectors;

-- Local Packages
with API.Manifest.Tools; use API.Manifest.Tools; -- only for "="

package API.Inventories is
	-- Exceptions
	Item_Not_Found : exception;

	-- Types
	package IDV is new Ada.Containers.Vectors (Natural, Manifest.Tools.Item_Description);
	subtype Item_Description_List is IDV.Vector;

	type Item_Description_List_Bucket_Location_Type_Array is array (Manifest.Tools.Bucket_Location_Type) of Item_Description_List;
	type Item_Description_Bucket_Location_Type_Array is array (Manifest.Tools.Bucket_Location_Type) of Manifest.Tools.Item_Description;
end API.Inventories;
