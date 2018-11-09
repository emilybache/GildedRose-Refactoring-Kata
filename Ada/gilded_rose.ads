with Ada.Containers.Vectors;
with Items;
use Items;

package Gilded_Rose is
  package Item_Vecs is new Ada.Containers.Vectors (
    Element_Type => Item,
    Index_Type => Positive
  );

  type Gilded_Rose is record
    Items : Item_Vecs.Vector;
  end record;
  procedure Update_Quality(Self : in out Gilded_Rose);
end Gilded_Rose;
