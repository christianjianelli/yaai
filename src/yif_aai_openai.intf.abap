INTERFACE yif_aai_openai
  PUBLIC.

  TYPES: BEGIN OF ty_generate_message_s,
           role      TYPE string,
           content   TYPE string,
           type      TYPE string,
           call_id   TYPE string,
           arguments TYPE string,
           name      TYPE string,
           output    TYPE string,
         END OF ty_generate_message_s.

  TYPES: ty_generate_messages_t TYPE STANDARD TABLE OF ty_generate_message_s WITH EMPTY KEY.

  TYPES: BEGIN OF ty_type_message_s,
           role    TYPE string,
           content TYPE string,
           type    TYPE string,
         END OF ty_type_message_s,

         BEGIN OF ty_function_call_s,
           type      TYPE string,
           arguments TYPE string,
           call_id   TYPE string,
           name      TYPE string,
         END OF ty_function_call_s,

         BEGIN OF ty_function_call_output_s,
           type    TYPE string,
           call_id TYPE string,
           output  TYPE string,
         END OF ty_function_call_output_s,

         BEGIN OF ty_function_s,
           name      TYPE string,
           arguments TYPE string,
         END OF ty_function_s,

         BEGIN OF ty_function_call_chat_comp_s,
           id       TYPE string,
           type     TYPE string,
           function TYPE ty_function_s,
         END OF ty_function_call_chat_comp_s,

         ty_function_call_chat_comp_t TYPE STANDARD TABLE OF ty_generate_message_s WITH EMPTY KEY,

         BEGIN OF ty_type_message_chat_comp_s,
           role       TYPE string,
           content    TYPE string,
           tool_calls TYPE ty_function_call_chat_comp_t,
         END OF ty_type_message_chat_comp_s.

  TYPES: BEGIN OF ty_openai_generate_request_s,
           model       TYPE string,
           stream      TYPE abap_bool,
           temperature TYPE p LENGTH 2 DECIMALS 1,
           input       TYPE /ui2/cl_json=>json,
           tools       TYPE /ui2/cl_json=>json,
         END OF ty_openai_generate_request_s.

  TYPES: BEGIN OF ty_openai_completions_req_s,
           model       TYPE string,
           stream      TYPE abap_bool,
           temperature TYPE p LENGTH 2 DECIMALS 1,
           messages    TYPE /ui2/cl_json=>json,
         END OF ty_openai_completions_req_s.

  TYPES: BEGIN OF ty_openai_comp_tools_req_s,
           model       TYPE string,
           stream      TYPE abap_bool,
           temperature TYPE p LENGTH 2 DECIMALS 1,
           messages    TYPE /ui2/cl_json=>json,
           tools       TYPE /ui2/cl_json=>json,
         END OF ty_openai_comp_tools_req_s.

  TYPES: BEGIN OF ty_content_s,
           type TYPE string,
           text TYPE string,
         END OF ty_content_s.

  TYPES content_t TYPE STANDARD TABLE OF ty_content_s WITH NON-UNIQUE KEY text.

  TYPES: BEGIN OF ty_output_s,
           id        TYPE string,
           type      TYPE string,
           role      TYPE string,
           content   TYPE content_t,
           call_id   TYPE string,
           status    TYPE string,
           name      TYPE string,
           arguments TYPE /ui2/cl_json=>json,
           output    TYPE string,
         END OF ty_output_s.

  TYPES: ty_output_t TYPE STANDARD TABLE OF ty_output_s WITH NON-UNIQUE KEY id.

  TYPES: BEGIN OF ty_choices_s,
           index      TYPE i,
           message    TYPE ty_type_message_s,
           tool_calls TYPE ty_type_message_chat_comp_s,
         END OF ty_choices_s.

  TYPES: ty_choices_t TYPE STANDARD TABLE OF ty_choices_s WITH NON-UNIQUE KEY index.

  TYPES: BEGIN OF ty_openai_generate_response_s,
           id          TYPE string,
           status      TYPE string,
           model       TYPE string,
           temperature TYPE string,
           output      TYPE ty_output_t,
         END OF ty_openai_generate_response_s.

  TYPES: BEGIN OF ty_openai_chat_comp_resp_s,
           id      TYPE string,
           object  TYPE string,
           model   TYPE string,
           choices TYPE ty_choices_t,
         END OF ty_openai_chat_comp_resp_s.

  TYPES: BEGIN OF ty_openai_embed_request_s,
           model TYPE string,
           input TYPE string,
         END OF ty_openai_embed_request_s.

  TYPES: ty_embedding_t TYPE STANDARD TABLE OF f WITH DEFAULT KEY.

  TYPES: BEGIN OF ty_openai_embed_data_s,
           object    TYPE string,
           embedding TYPE ty_embedding_t,
           index     TYPE i,
         END OF ty_openai_embed_data_s.

  TYPES: ty_openai_embed_data_t TYPE STANDARD TABLE OF ty_openai_embed_data_s WITH DEFAULT KEY.

  TYPES: BEGIN OF ty_openai_embed_response_s,
           object TYPE string,
           data   TYPE ty_openai_embed_data_t,
           model  TYPE string,
         END OF ty_openai_embed_response_s.

  DATA: mo_function_calling TYPE REF TO yif_aai_func_call_openai READ-ONLY.

  METHODS use_completions
    IMPORTING
      i_use_completions TYPE abap_bool DEFAULT abap_true.

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

  METHODS bind_tools
    IMPORTING
      i_o_function_calling TYPE REF TO yif_aai_func_call_openai
      i_max_tools_calls    TYPE i DEFAULT 5.

  METHODS set_history
    IMPORTING
      i_t_history TYPE ty_generate_messages_t.

  METHODS get_history
    EXPORTING
      e_t_history TYPE ty_generate_messages_t.

  METHODS get_conversation
    RETURNING
      VALUE(r_conversation) TYPE /ui2/cl_json=>json.

  METHODS generate
    IMPORTING
      i_message    TYPE csequence
      i_new        TYPE abap_bool DEFAULT abap_false
      i_greeting   TYPE csequence OPTIONAL
    EXPORTING
      e_response   TYPE string
      e_t_response TYPE rswsourcet.

  METHODS chat_completions
    IMPORTING
      i_message    TYPE csequence
      i_new        TYPE abap_bool DEFAULT abap_false
      i_greeting   TYPE csequence OPTIONAL
    EXPORTING
      e_response   TYPE string
      e_t_response TYPE rswsourcet.

  METHODS embed
    IMPORTING
      i_input      TYPE csequence
    EXPORTING
      e_s_response TYPE ty_openai_embed_response_s.

ENDINTERFACE.
