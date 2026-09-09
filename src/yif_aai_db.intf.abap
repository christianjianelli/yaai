INTERFACE yif_aai_db
  PUBLIC.

  TYPES: ty_messages_t TYPE STANDARD TABLE OF yaai_msg   WITH DEFAULT KEY,
         ty_tools_t    TYPE STANDARD TABLE OF yaai_tools WITH DEFAULT KEY.

  CONSTANTS: mc_scope_one_time TYPE yaai_approval-scope VALUE 'ONE_TIME' ##NO_TEXT,
             mc_scope_chat     TYPE yaai_approval-scope VALUE 'CHAT' ##NO_TEXT.

  DATA: mt_messages TYPE ty_messages_t READ-ONLY,
        mt_tools    TYPE ty_tools_t READ-ONLY.

  DATA: m_api  TYPE string READ-ONLY,
        m_id   TYPE yde_aai_id READ-ONLY,
        m_user TYPE string.

  METHODS create_id
    RETURNING VALUE(r_id) TYPE yde_aai_id.

  METHODS persist_chat
    IMPORTING
      i_id        TYPE yde_aai_id OPTIONAL
    EXPORTING
      e_id        TYPE yde_aai_id
      e_persisted TYPE abap_bool.

  METHODS persist_system_instructions
    IMPORTING
      i_id                  TYPE yde_aai_id OPTIONAL
      i_system_instructions TYPE csequence OPTIONAL
      i_data                TYPE data OPTIONAL
    EXPORTING
      e_id                  TYPE yde_aai_id
      e_persisted           TYPE abap_bool.

  METHODS persist_message
    IMPORTING
      i_id            TYPE yde_aai_id OPTIONAL
      i_message       TYPE csequence OPTIONAL
      i_data          TYPE data OPTIONAL
      i_prompt        TYPE data OPTIONAL
      i_async_task_id TYPE csequence OPTIONAL
      i_tokens        TYPE yde_aai_tokens OPTIONAL
      i_model         TYPE yde_aai_model OPTIONAL
    EXPORTING
      e_id            TYPE yde_aai_id
      e_seqno         TYPE yde_aai_seqno
      e_persisted     TYPE abap_bool.

  METHODS persist_tools
    IMPORTING
      i_t_tools   TYPE ty_tools_t
    EXPORTING
      e_persisted TYPE abap_bool.

  METHODS persist_files
    IMPORTING
      i_seqno     TYPE i
      i_t_files   TYPE ytt_aai_files
    EXPORTING
      e_persisted TYPE abap_bool.

  METHODS get_files
    IMPORTING
      i_seqno   TYPE i
    EXPORTING
      e_t_files TYPE ytt_aai_files.

  METHODS get_chat
    IMPORTING
      i_id         TYPE yde_aai_id OPTIONAL
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

  METHODS request_approval
    IMPORTING
      i_class_name  TYPE csequence
      i_method_name TYPE csequence
    EXPORTING
      e_created     TYPE abap_bool.

  METHODS get_approval
    IMPORTING
      i_class_name  TYPE csequence
      i_method_name TYPE csequence
    EXPORTING
      e_requested   TYPE abap_bool
      e_scope       TYPE yaai_approval-scope
      e_approved    TYPE abap_bool.

  METHODS update_approval
    IMPORTING
      i_class_name  TYPE csequence
      i_method_name TYPE csequence
      i_approved    TYPE abap_bool OPTIONAL
      i_scope       TYPE yaai_approval-scope OPTIONAL
      i_used        TYPE abap_bool OPTIONAL
    EXPORTING
      e_updated     TYPE abap_bool.

ENDINTERFACE.
