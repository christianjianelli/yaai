INTERFACE yif_aai_anthropic
  PUBLIC.

  TYPES: BEGIN OF ty_chat_message_s,
           role    TYPE string,
           content TYPE /ui2/cl_json=>json,
         END OF ty_chat_message_s,

         BEGIN OF ty_chat_message_think_s,
           type TYPE string,
           text TYPE string,
         END OF ty_chat_message_think_s,

         BEGIN OF ty_chat_message_tool_use_s,
           type  TYPE string,
           id    TYPE string,
           name  TYPE string,
           input TYPE /ui2/cl_json=>json,
         END OF ty_chat_message_tool_use_s,

         BEGIN OF ty_chat_message_tool_result_s,
           type        TYPE string,
           tool_use_id TYPE string,
           content     TYPE string,
         END OF ty_chat_message_tool_result_s.

  TYPES: ty_chat_messages_t TYPE STANDARD TABLE OF ty_chat_message_s WITH NON-UNIQUE KEY role.

  TYPES: BEGIN OF ty_anthropic_chat_request_s,
           model       TYPE string,
           stream      TYPE abap_bool,
           system      TYPE string,
           temperature TYPE p LENGTH 2 DECIMALS 1,
           max_tokens  TYPE i,
           messages    TYPE ty_chat_messages_t,
           tools       TYPE /ui2/cl_json=>json,
         END OF ty_anthropic_chat_request_s.

  TYPES: BEGIN OF ty_response_content_s,
           type  TYPE string,
           text  TYPE string,
           id    TYPE string,
           name  TYPE string,
           input TYPE /ui2/cl_json=>json,
         END OF ty_response_content_s.

  TYPES: ty_content_t TYPE STANDARD TABLE OF ty_response_content_s WITH NON-UNIQUE KEY type.

  TYPES: BEGIN OF ty_error_s,
           type    TYPE string,
           message TYPE string,
         END OF ty_error_s.

  TYPES: BEGIN OF ty_anthropic_chat_response_s,
           id          TYPE string,
           type        TYPE string,
           role        TYPE string,
           content     TYPE /ui2/cl_json=>json,
           stop_reason TYPE string,
           error       TYPE ty_error_s,
         END OF ty_anthropic_chat_response_s.

  DATA: mo_function_calling TYPE REF TO yif_aai_func_call_anthropic READ-ONLY.

  DATA m_anthropic_version TYPE string READ-ONLY.

  METHODS set_version
    IMPORTING
      i_version TYPE csequence.

  METHODS set_model
    IMPORTING
      i_model TYPE csequence.

  METHODS set_max_tokens
    IMPORTING
      i_max_tokens TYPE i.

  METHODS set_temperature
    IMPORTING
      i_temperature TYPE numeric.

  METHODS set_system_instructions
    IMPORTING
      i_system_instructions TYPE string.

  METHODS set_connection
    IMPORTING
      i_o_connection TYPE REF TO yif_aai_conn.

  METHODS bind_tools
    IMPORTING
      i_o_function_calling TYPE REF TO yif_aai_func_call_anthropic
      i_max_tools_calls    TYPE i DEFAULT 5.

  METHODS set_history
    IMPORTING
      i_t_history TYPE ty_chat_messages_t.

  METHODS get_history
    EXPORTING
      e_t_history TYPE ty_chat_messages_t.

  METHODS chat
    IMPORTING
      i_message    TYPE csequence
      i_new        TYPE abap_bool DEFAULT abap_false
      i_greeting   TYPE csequence OPTIONAL
    EXPORTING
      e_response   TYPE string
      e_failed     TYPE abap_bool
      e_t_response TYPE rswsourcet.

  METHODS get_conversation
    RETURNING VALUE(rt_messages) TYPE ty_chat_messages_t.

ENDINTERFACE.
