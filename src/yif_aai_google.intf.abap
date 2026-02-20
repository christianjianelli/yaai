INTERFACE yif_aai_google
  PUBLIC.

  TYPES: ty_response_t TYPE STANDARD TABLE OF string.

  TYPES: BEGIN OF ty_function_call_s,
           name TYPE string,
           args TYPE /ui2/cl_json=>json,
         END OF ty_function_call_s,

         BEGIN OF ty_function_response_s,
           name     TYPE string,
           response TYPE /ui2/cl_json=>json,
         END OF ty_function_response_s,

         BEGIN OF ty_parts_text_s,
           text TYPE string,
         END OF ty_parts_text_s,

         BEGIN OF ty_parts_response_text_s,
           text TYPE string,
         END OF ty_parts_response_text_s,

         BEGIN OF ty_parts_response_func_call_s,
           functioncall TYPE ty_function_call_s,
         END OF ty_parts_response_func_call_s,

         BEGIN OF ty_parts_request_func_call_s,
           function_call TYPE ty_function_call_s,
         END OF ty_parts_request_func_call_s,

         BEGIN OF ty_parts_response_func_resp_s,
           function_response TYPE ty_function_response_s,
         END OF ty_parts_response_func_resp_s,

         BEGIN OF ty_parts_response_s,
           text              TYPE string,
           functioncall      TYPE ty_function_call_s,
           function_response TYPE ty_function_response_s,
         END OF ty_parts_response_s,

         ty_parts_request_t  TYPE STANDARD TABLE OF /ui2/cl_json=>json WITH DEFAULT KEY,
         ty_parts_response_t TYPE STANDARD TABLE OF ty_parts_response_s WITH DEFAULT KEY,
         ty_parts_text_t     TYPE STANDARD TABLE OF ty_parts_text_s WITH DEFAULT KEY,

         BEGIN OF ty_contents_s,
           parts TYPE ty_parts_request_t,
           role  TYPE string,
         END OF ty_contents_s,

         BEGIN OF ty_contents_response_s,
           parts TYPE ty_parts_response_t,
           role  TYPE string,
         END OF ty_contents_response_s,

         BEGIN OF ty_candidates_s,
           content TYPE ty_contents_response_s,
         END OF ty_candidates_s,

         BEGIN OF ty_system_instruction_s,
           parts TYPE ty_parts_text_t,
         END OF ty_system_instruction_s,

         BEGIN OF ty_generation_config_s,
           temperature TYPE p LENGTH 2 DECIMALS 1,
         END OF ty_generation_config_s.

  TYPES: ty_contents_t   TYPE STANDARD TABLE OF ty_contents_s WITH NON-UNIQUE KEY role,
         ty_candidates_t TYPE STANDARD TABLE OF ty_candidates_s WITH DEFAULT KEY.

  TYPES: BEGIN OF ty_google_generate_request_s,
           contents          TYPE ty_contents_t,
           generation_config TYPE ty_generation_config_s,
           tools             TYPE /ui2/cl_json=>json,
         END OF ty_google_generate_request_s,

         BEGIN OF ty_google_generate_req_sys_s,
           system_instruction TYPE ty_system_instruction_s,
           contents           TYPE ty_contents_t,
           generation_config  TYPE ty_generation_config_s,
           tools              TYPE /ui2/cl_json=>json,
         END OF ty_google_generate_req_sys_s,

         BEGIN OF ty_error_s,
           code    TYPE i,
           message TYPE string,
           status  TYPE string,
         END OF ty_error_s,

         BEGIN OF ty_usage_metadata_s,
           total_token_count TYPE i,
         END OF ty_usage_metadata_s,

         BEGIN OF ty_google_generate_response_s,
           candidates     TYPE ty_candidates_t,
           error          TYPE ty_error_s,
           usage_metadata TYPE ty_usage_metadata_s,
         END OF ty_google_generate_response_s.

  DATA: mo_function_calling TYPE REF TO yif_aai_func_call_google READ-ONLY,
        mo_agent            TYPE REF TO yif_aai_agent READ-ONLY.

  DATA: m_endpoint TYPE string READ-ONLY.

  METHODS set_model
    IMPORTING
      i_model TYPE csequence.

  METHODS set_temperature
    IMPORTING
      i_temperature TYPE numeric.

  METHODS set_system_instructions
    IMPORTING
      i_system_instructions TYPE string.

  METHODS set_connection
    IMPORTING
      i_o_connection TYPE REF TO yif_aai_conn.

  METHODS set_endpoint
    IMPORTING
      i_endpoint TYPE csequence.

  METHODS bind_tools
    IMPORTING
      i_o_function_calling TYPE REF TO yif_aai_func_call_google
      i_max_tools_calls    TYPE i DEFAULT 5.

  METHODS set_history
    IMPORTING
      i_t_history TYPE ty_contents_t.

  METHODS set_persistence
    IMPORTING
      i_o_persistence TYPE REF TO yif_aai_db.

  METHODS get_history
    EXPORTING
      e_t_history TYPE ty_contents_t.

  METHODS get_conversation
    RETURNING
      VALUE(r_conversation) TYPE /ui2/cl_json=>json.

  METHODS generate
    IMPORTING
      i_message       TYPE csequence OPTIONAL
      i_new           TYPE abap_bool DEFAULT abap_false
      i_greeting      TYPE csequence OPTIONAL
      i_async_task_id TYPE csequence OPTIONAL
      i_o_prompt      TYPE REF TO yif_aai_prompt OPTIONAL
      i_o_agent       TYPE REF TO yif_aai_agent OPTIONAL
        PREFERRED PARAMETER i_message
    EXPORTING
      e_response      TYPE string
      e_failed        TYPE abap_bool
      e_t_response    TYPE ty_response_t.

ENDINTERFACE.
