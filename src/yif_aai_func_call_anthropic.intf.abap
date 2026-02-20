INTERFACE yif_aai_func_call_anthropic
  PUBLIC .

  TYPES: BEGIN OF ty_input_schema_s,
           type       TYPE string,
           properties TYPE /ui2/cl_json=>json,
           required   TYPE STANDARD TABLE OF string WITH EMPTY KEY,
         END OF ty_input_schema_s,

         BEGIN OF ty_tool_s,
           name         TYPE string,
           description  TYPE string,
           input_schema TYPE ty_input_schema_s,
         END OF ty_tool_s,

         ty_tools_t TYPE STANDARD TABLE OF ty_tool_s WITH EMPTY KEY,

         BEGIN OF ty_method_s,
           class_name  TYPE string,
           proxy_class TYPE string,
           method_name TYPE string,
           description TYPE string,
           full_schema TYPE abap_bool,
         END OF ty_method_s,

         ty_methods_t TYPE STANDARD TABLE OF ty_method_s WITH EMPTY KEY.

  EVENTS on_tool_call
    EXPORTING
      VALUE(class_name)       TYPE string
      VALUE(method_name)      TYPE string
      VALUE(parameters_table) TYPE abap_parmbind_tab.

  EVENTS on_tool_call_response
    EXPORTING
      VALUE(tool_response) TYPE string.

  EVENTS on_tool_call_error
    EXPORTING
      VALUE(error_text) TYPE string.

  DATA: mt_methods TYPE ty_methods_t READ-ONLY.

  METHODS add_methods IMPORTING i_t_methods TYPE ty_methods_t.

  METHODS get_tools
            IMPORTING
              i_o_agent TYPE REF TO yif_aai_agent OPTIONAL
            EXPORTING
              e_tools TYPE string.

  METHODS reset_methods.

  METHODS remove_method IMPORTING i_s_method TYPE ty_method_s.

  METHODS call_tool
    IMPORTING
              i_tool_name       TYPE string
              i_json            TYPE /ui2/cl_json=>json
    RETURNING VALUE(r_response) TYPE string.

ENDINTERFACE.
