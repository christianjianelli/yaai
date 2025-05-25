INTERFACE yif_aai_ollama
  PUBLIC.

  TYPES: BEGIN OF chat_message_s,
           role    TYPE string,
           content TYPE string,
         END OF chat_message_s.

  TYPES: chat_messages_t TYPE STANDARD TABLE OF chat_message_s WITH NON-UNIQUE KEY role.

  TYPES: BEGIN OF ollama_generate_request_s,
           model    TYPE string,
           prompt   TYPE string,
           system   TYPE string,
           stream   TYPE abap_bool,
         END OF ollama_generate_request_s,

         BEGIN OF ollama_chat_request_s,
           model    TYPE string,
           stream   TYPE abap_bool,
           messages TYPE chat_messages_t,
           tools    TYPE /ui2/cl_json=>json,
         END OF ollama_chat_request_s,

         BEGIN OF ollama_generate_response_s,
           model    TYPE string,
           response TYPE string,
         END OF ollama_generate_response_s,

         BEGIN OF ollama_chat_response_s,
           model   TYPE string,
           message TYPE chat_message_s,
         END OF ollama_chat_response_s.

  DATA mo_function_calling TYPE REF TO yif_aai_function_calling READ-ONLY.

  METHODS set_model
    IMPORTING
      i_model TYPE csequence.

  METHODS set_system_instructions
    IMPORTING
      i_system_instructions TYPE string.

  METHODS bind_tools
    IMPORTING
      i_o_function_calling TYPE REF TO yif_aai_function_calling.

  METHODS chat
    IMPORTING
      i_message    TYPE csequence
      i_new        TYPE abap_bool DEFAULT abap_false
    EXPORTING
      e_response   TYPE string
      e_t_response TYPE rswsourcet.

  METHODS generate
    IMPORTING
      i_message    TYPE csequence
    EXPORTING
      e_response   TYPE string
      e_t_response TYPE rswsourcet.

  METHODS embed
    IMPORTING
      i_message  TYPE string
    EXPORTING
      e_response TYPE string.

ENDINTERFACE.
