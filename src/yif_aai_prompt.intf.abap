INTERFACE yif_aai_prompt
  PUBLIC.

  METHODS generate_prompt_from_template
    IMPORTING
              i_o_template    TYPE REF TO yif_aai_prompt_template
              i_s_params      TYPE data
    RETURNING VALUE(r_prompt) TYPE string.


ENDINTERFACE.
