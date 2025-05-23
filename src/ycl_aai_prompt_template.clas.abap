CLASS ycl_aai_prompt_template DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES yif_aai_prompt_template.
    ALIASES m_template_id FOR yif_aai_prompt_template~m_template_id.
    ALIASES m_template_text FOR yif_aai_prompt_template~m_template_text.
    ALIASES set_template_text FOR yif_aai_prompt_template~set_template_text.
    ALIASES create_from_standart_text FOR yif_aai_prompt_template~create_from_standart_text.

    METHODS constructor IMPORTING i_template_text TYPE clike OPTIONAL.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS ycl_aai_prompt_template IMPLEMENTATION.

  METHOD constructor.

    IF i_template_text IS SUPPLIED.
      me->m_template_text = i_template_text.
    ENDIF.

  ENDMETHOD.

  METHOD yif_aai_prompt_template~set_template_text.

    me->m_template_text = i_template_text.

  ENDMETHOD.

  METHOD yif_aai_prompt_template~get_template_text.

    r_template_text = me->m_template_text.

  ENDMETHOD.

  METHOD yif_aai_prompt_template~create_from_standart_text.

    DATA: tl_lines        TYPE STANDARD TABLE OF tline,
          l_name          TYPE thead-tdname,
          l_template_text TYPE string.

    l_name = i_standart_text_name.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = 'ST'             " Text ID of text to be read
        language                = sy-langu         " Language of text to be read
        name                    = l_name           " Name of text to be read
        object                  = 'TEXT'           " Object of text to be read
      TABLES
        lines                   = tl_lines         " Lines of text read
      EXCEPTIONS
        id                      = 1                " Text ID invalid
        language                = 2                " Invalid language
        name                    = 3                " Invalid text name
        not_found               = 4                " Text not found
        object                  = 5                " Invalid text object
        reference_check         = 6                " Reference chain interrupted
        wrong_access_to_archive = 7                " Archive handle invalid for access
        OTHERS                  = 8.

    IF sy-subrc <> 0.

      "TODO: log

    ENDIF.

    LOOP AT tl_lines ASSIGNING FIELD-SYMBOL(<ls_line>).

      l_template_text = COND #( WHEN l_template_text IS INITIAL
                                THEN <ls_line>-tdline
                                ELSE |{ l_template_text } \n { <ls_line>-tdline }| ).

    ENDLOOP.

    r_prompt_template_ref = NEW ycl_aai_prompt_template( l_template_text ).

  ENDMETHOD.

ENDCLASS.
