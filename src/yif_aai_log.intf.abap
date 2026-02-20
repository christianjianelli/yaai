INTERFACE yif_aai_log
  PUBLIC.

  TYPES: ty_messages_t TYPE STANDARD TABLE OF bapiret2 WITH DEFAULT KEY,
         ty_log_t      TYPE SORTED TABLE OF yaai_log WITH UNIQUE KEY id seqno.

  DATA: mt_messages TYPE ty_messages_t READ-ONLY,
        mt_log      TYPE ty_log_t READ-ONLY.

  DATA m_chat_id TYPE yaai_log-id READ-ONLY.

  METHODS add_message
    IMPORTING
      i_save  TYPE abap_bool DEFAULT abap_true
      i_s_msg TYPE bapiret2.

  METHODS add_messages
    IMPORTING
      i_save  TYPE abap_bool DEFAULT abap_true
      i_t_msg TYPE ty_messages_t.

  METHODS save_log
    IMPORTING
      i_chat_id     TYPE yaai_log-id OPTIONAL
      i_expiry_date TYPE d OPTIONAL
        PREFERRED PARAMETER i_chat_id.

  METHODS load_log
    IMPORTING
      i_chat_id TYPE yaai_log-id OPTIONAL.

  METHODS get_log
    RETURNING VALUE(rt_log) TYPE ty_log_t.

  METHODS get_messages
    RETURNING VALUE(rt_msg) TYPE ty_messages_t.

  METHODS delete_expired_logs.

ENDINTERFACE.
