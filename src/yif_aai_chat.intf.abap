INTERFACE yif_aai_chat
  PUBLIC.

  TYPES: ty_response_t TYPE STANDARD TABLE OF string.

  EVENTS on_message_send.
  EVENTS on_response_received.
  EVENTS on_message_failed
    EXPORTING
      VALUE(error_text) TYPE string.
  EVENTS on_chat_is_blocked.

  METHODS chat
    IMPORTING
      id              TYPE uuid OPTIONAL
      i_message       TYPE csequence OPTIONAL
      i_new           TYPE abap_bool DEFAULT abap_false
      i_greeting      TYPE csequence OPTIONAL
      i_async_task_id TYPE csequence OPTIONAL
      i_o_prompt      TYPE REF TO yif_aai_prompt OPTIONAL
      i_o_agent       TYPE REF TO yif_aai_agent OPTIONAL
    EXPORTING
      e_id            TYPE uuid
      e_response      TYPE string
      e_failed        TYPE abap_bool
      e_t_response    TYPE ty_response_t.

ENDINTERFACE.
