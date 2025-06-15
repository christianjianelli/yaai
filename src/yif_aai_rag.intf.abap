INTERFACE yif_aai_rag
  PUBLIC.

  METHODS set_prompt_template
    IMPORTING
      io_prompt_template TYPE REF TO yif_aai_prompt_template.

  METHODS get_context
    IMPORTING
      i_input            TYPE csequence
      i_new_context_only TYPE abap_bool DEFAULT abap_true
    EXPORTING
      e_context          TYPE string.

  METHODS augment_prompt
    IMPORTING
      i_prompt           TYPE csequence
      i_new_context_only TYPE abap_bool DEFAULT abap_true
    EXPORTING
      e_augmented_prompt TYPE string.

ENDINTERFACE.
