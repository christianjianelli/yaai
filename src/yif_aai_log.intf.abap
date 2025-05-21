INTERFACE yif_aai_log
  PUBLIC.

  DATA: mt_messages TYPE bapiret2_t.

  METHODS add_message
    IMPORTING
      i_s_msg TYPE bapiret2.

  METHODS add_messages
    IMPORTING
      i_t_msg TYPE bapiret2_t.

ENDINTERFACE.
