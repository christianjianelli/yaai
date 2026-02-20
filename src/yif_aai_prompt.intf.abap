INTERFACE yif_aai_prompt
  PUBLIC.

  TYPES: BEGIN OF ty_params_basic_s,
           user_message TYPE string,
           context      TYPE string,
         END OF ty_params_basic_s.

  DATA: mo_prompt_template  TYPE REF TO yif_aai_prompt_template READ-ONLY,
        mr_params           TYPE REF TO data READ-ONLY,
        m_placeholder_begin TYPE string READ-ONLY,
        m_placeholder_end   TYPE string READ-ONLY.

  METHODS set_placeholder_pattern
    IMPORTING
      i_placeholder_begin TYPE csequence
      i_placeholder_end   TYPE csequence.

  METHODS generate_prompt_from_template
    IMPORTING
              i_o_template    TYPE REF TO yif_aai_prompt_template
              i_s_params      TYPE data
    RETURNING VALUE(r_prompt) TYPE string.

  METHODS get_prompt
    RETURNING VALUE(r_prompt) TYPE string.

  METHODS get_user_message
    RETURNING VALUE(r_user_message) TYPE string.

  METHODS get_context
    RETURNING VALUE(r_context) TYPE string.

ENDINTERFACE.
