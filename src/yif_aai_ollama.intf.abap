INTERFACE yif_aai_ollama
  PUBLIC.

  TYPES: BEGIN OF ty_function_s,
           index     TYPE i,
           name      TYPE string,
           arguments TYPE /ui2/cl_json=>json,
         END OF ty_function_s,

         BEGIN OF ty_tool_calls_s,
           function TYPE ty_function_s,
         END OF ty_tool_calls_s,

         BEGIN OF ty_chat_message_s,
           role       TYPE string,
           content    TYPE string,
           tool_calls TYPE STANDARD TABLE OF ty_tool_calls_s WITH DEFAULT KEY,
           tool_name  TYPE string,
         END OF ty_chat_message_s.

  TYPES: ty_chat_messages_t TYPE STANDARD TABLE OF ty_chat_message_s WITH NON-UNIQUE KEY role.

  TYPES: BEGIN OF ty_options_s,
           temperature TYPE p LENGTH 2 DECIMALS 1,
         END OF ty_options_s,

         BEGIN OF ty_ollama_generate_request_s,
           model   TYPE string,
           prompt  TYPE string,
           system  TYPE string,
           stream  TYPE abap_bool,
           options TYPE ty_options_s,
         END OF ty_ollama_generate_request_s,

         BEGIN OF ty_ollama_chat_request_s,
           model    TYPE string,
           stream   TYPE abap_bool,
           messages TYPE ty_chat_messages_t,
           tools    TYPE /ui2/cl_json=>json,
           options  TYPE ty_options_s,
         END OF ty_ollama_chat_request_s,

         BEGIN OF ty_ollama_generate_response_s,
           model    TYPE string,
           response TYPE string,
           error    TYPE string,
         END OF ty_ollama_generate_response_s,

         BEGIN OF ty_ollama_chat_response_s,
           model             TYPE string,
           message           TYPE ty_chat_message_s,
           error             TYPE string,
           prompt_eval_count TYPE i,
           eval_count        TYPE i,
         END OF ty_ollama_chat_response_s.

  TYPES: BEGIN OF ty_ollama_embed_request_s,
           model TYPE string,
           input TYPE string,
         END OF ty_ollama_embed_request_s.

  TYPES: ty_embedding_t TYPE STANDARD TABLE OF f WITH DEFAULT KEY.

  TYPES: BEGIN OF ty_ollama_embed_response_s,
           model             TYPE string,
           embeddings        TYPE STANDARD TABLE OF ty_embedding_t WITH DEFAULT KEY,
           total_duration    TYPE i,
           load_duration     TYPE i,
           prompt_eval_count TYPE i,
           error             TYPE string,
         END OF ty_ollama_embed_response_s.

  DATA: mo_function_calling TYPE REF TO yif_aai_func_call_ollama READ-ONLY,
        mo_agent            TYPE REF TO yif_aai_agent READ-ONLY.

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
      i_o_function_calling TYPE REF TO yif_aai_func_call_ollama
      i_max_tool_calls     TYPE i DEFAULT 5.

  METHODS set_history
    IMPORTING
      i_t_history TYPE ty_chat_messages_t.

  METHODS get_history
    EXPORTING
      e_t_history TYPE ty_chat_messages_t.

  METHODS chat
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
      e_t_response    TYPE rswsourcet.

  METHODS generate
    IMPORTING
      i_message    TYPE csequence OPTIONAL
      i_o_template TYPE REF TO yif_aai_prompt_template OPTIONAL
    EXPORTING
      e_response   TYPE string
      e_failed     TYPE abap_bool
      e_t_response TYPE rswsourcet.

  METHODS embed
    IMPORTING
      i_input      TYPE csequence
    EXPORTING
      e_failed     TYPE abap_bool
      e_s_response TYPE ty_ollama_embed_response_s.

  METHODS get_chat_messages
    RETURNING VALUE(rt_messages) TYPE ty_chat_messages_t.

ENDINTERFACE.
