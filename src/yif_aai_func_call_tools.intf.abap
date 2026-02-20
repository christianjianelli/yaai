INTERFACE yif_aai_func_call_tools
  PUBLIC.

  METHODS get_available_tools
    RETURNING VALUE(r_response) TYPE string.

  METHODS request_tools
    IMPORTING
              i_t_tools         TYPE ytt_aai_tool_request
    RETURNING VALUE(r_response) TYPE string.

ENDINTERFACE.
