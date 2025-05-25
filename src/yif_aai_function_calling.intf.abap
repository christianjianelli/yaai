INTERFACE yif_aai_function_calling
  PUBLIC.

  TYPES: BEGIN OF parameters_s,
           type       TYPE string,
           properties TYPE /ui2/cl_json=>json,
           required   TYPE STANDARD TABLE OF string WITH EMPTY KEY,
         END OF parameters_s,

         BEGIN OF function_s,
           name        TYPE string,
           description TYPE string,
           parameters  TYPE parameters_s,
         END OF function_s,

         BEGIN OF tool_s,
           type     TYPE string,
           function TYPE function_s,
         END OF tool_s,

         BEGIN OF method_s,
           class_name  TYPE string,
           method      TYPE string,
           description TYPE string,
         END OF method_s,

         methods_t TYPE STANDARD TABLE OF method_s WITH EMPTY KEY.

  DATA: mt_methods TYPE methods_t READ-ONLY.

  METHODS add_methods IMPORTING i_t_methods TYPE methods_t.

  METHODS get_tools EXPORTING e_tools TYPE string.

  METHODS reset_methods.

  METHODS remove_method IMPORTING i_s_method TYPE method_s.

ENDINTERFACE.
