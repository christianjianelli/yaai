INTERFACE yif_aai_prompt_template
  PUBLIC.

  DATA: m_template_id   TYPE c LENGTH 30,
        m_template_text TYPE string READ-ONLY.

  METHODS set_template_text
    IMPORTING
      i_template_text TYPE clike.

  METHODS get_template_text
    RETURNING VALUE(r_template_text) TYPE string.

  CLASS-METHODS create_from_standart_text
    IMPORTING
              i_standart_text_name         TYPE clike
    RETURNING VALUE(r_prompt_template_ref) TYPE REF TO yif_aai_prompt_template.

ENDINTERFACE.
