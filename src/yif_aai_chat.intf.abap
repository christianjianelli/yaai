INTERFACE yif_aai_chat
  PUBLIC.

  METHODS chat
    IMPORTING
      i_message    TYPE csequence
      i_new        TYPE abap_bool DEFAULT abap_false
      i_greeting   TYPE csequence OPTIONAL
    EXPORTING
      e_response   TYPE string
      e_failed     TYPE abap_bool
      e_t_response TYPE rswsourcet.

ENDINTERFACE.
