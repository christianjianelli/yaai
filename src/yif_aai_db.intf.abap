INTERFACE yif_aai_db
  PUBLIC.

  TYPES: ty_messages_t TYPE STANDARD TABLE OF yaai_msg   WITH DEFAULT KEY,
         ty_tools_t    TYPE STANDARD TABLE OF yaai_tools WITH DEFAULT KEY.

  DATA: mt_messages TYPE ty_messages_t READ-ONLY,
        mt_tools    TYPE ty_tools_t READ-ONLY.

  METHODS create_id
    RETURNING VALUE(r_id) TYPE uuid.

  METHODS persist_chat
    IMPORTING
      i_id        TYPE uuid OPTIONAL
    EXPORTING
      e_id        TYPE uuid
      e_persisted TYPE abap_bool.

  METHODS persist_system_instructions
    IMPORTING
      i_id                  TYPE uuid OPTIONAL
      i_system_instructions TYPE csequence OPTIONAL
      i_data                TYPE data OPTIONAL
    EXPORTING
      e_id                  TYPE uuid
      e_persisted           TYPE abap_bool.

  METHODS persist_message
    IMPORTING
      i_id            TYPE uuid OPTIONAL
      i_message       TYPE csequence OPTIONAL
      i_data          TYPE data OPTIONAL
      i_prompt        TYPE data OPTIONAL
      i_async_task_id TYPE csequence OPTIONAL
      i_tokens        TYPE yde_aai_tokens OPTIONAL
      i_model         TYPE yde_aai_model OPTIONAL
    EXPORTING
      e_id            TYPE uuid
      e_persisted     TYPE abap_bool.

  METHODS persist_tools
    IMPORTING
      i_t_tools   TYPE ty_tools_t
    EXPORTING
      e_persisted TYPE abap_bool.

  METHODS get_chat
    IMPORTING
      i_id         TYPE uuid OPTIONAL
      i_ui         TYPE abap_bool DEFAULT abap_false
    EXPORTING
      e_t_messages TYPE ty_messages_t
      e_t_tools    TYPE ty_tools_t
      e_t_msg_data TYPE STANDARD TABLE.

  METHODS block_chat
    EXPORTING
      e_blocked TYPE abap_bool.

  METHODS is_chat_blocked
    RETURNING VALUE(r_blocked) TYPE abap_bool.

  METHODS release_chat
    EXPORTING
      e_released TYPE abap_bool.

  METHODS delete_chat
    EXPORTING
      e_deleted TYPE abap_bool.

ENDINTERFACE.
