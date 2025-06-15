INTERFACE yif_aai_func_call_openai
  PUBLIC .

  TYPES: BEGIN OF ty_parameters_s,
           type       TYPE string,
           properties TYPE /ui2/cl_json=>json,
           required   TYPE STANDARD TABLE OF string WITH EMPTY KEY,
         END OF ty_parameters_s,

         BEGIN OF ty_tool_s,
           type        TYPE string,
           name        TYPE string,
           description TYPE string,
           parameters  TYPE ty_parameters_s,
         END OF ty_tool_s,

         ty_tools_t TYPE STANDARD TABLE OF ty_tool_s WITH EMPTY KEY,

         BEGIN OF ty_method_s,
           class_name  TYPE string,
           proxy_class TYPE string,
           method_name TYPE string,
           description TYPE string,
         END OF ty_method_s,

         ty_methods_t TYPE STANDARD TABLE OF ty_method_s WITH EMPTY KEY.

  DATA: mt_methods TYPE ty_methods_t READ-ONLY.

  METHODS add_methods IMPORTING i_t_methods TYPE ty_methods_t.

  METHODS get_tools EXPORTING e_tools TYPE string.

  METHODS reset_methods.

  METHODS remove_method IMPORTING i_s_method TYPE ty_method_s.

  METHODS call_tool
    IMPORTING
              i_tool_name       TYPE string
              i_json            TYPE /ui2/cl_json=>json
    RETURNING VALUE(r_response) TYPE string.

ENDINTERFACE.
