type field = {
  name : string;
  change : Output_types.change_type;
  domain_type : Output_types.domain_type;
  oldval : Output_types.field_value option;
  newval : Output_types.field_value option;
}

and item = {
  name : string;
  change : Output_types.change_type;
  domain_type : Output_types.domain_type;
  children : view list;
}

and collection = {
  name : string;
  change : Output_types.change_type;
  domain_type : Output_types.domain_type;
  items : view list;
}

and view =
  | Field of field
  | Item of item
  | Collection of collection
