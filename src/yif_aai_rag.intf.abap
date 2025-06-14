INTERFACE yif_aai_rag
  PUBLIC.

  METHODS get_context
    IMPORTING
      i_input            TYPE csequence
      i_new_context_only TYPE abap_bool DEFAULT abap_true
    EXPORTING
      e_context          TYPE string.

ENDINTERFACE.
