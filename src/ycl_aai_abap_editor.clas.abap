CLASS ycl_aai_abap_editor DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS get_code
      IMPORTING
        i_cursor_position TYPE abap_bool DEFAULT abap_false
      EXPORTING
        e_code            TYPE string
        e_t_code          TYPE rswsourcet.

    METHODS get_user_cursor_position
      RETURNING VALUE(r_cursor_position) TYPE i.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS ycl_aai_abap_editor IMPLEMENTATION.

  METHOD get_code.

    FIELD-SYMBOLS <lt_code> TYPE ANY TABLE.

    FREE: e_code,
          e_t_code.

    IF NOT e_code IS REQUESTED AND
       NOT e_t_code IS REQUESTED.
      RETURN.
    ENDIF.

    ASSIGN ('(SAPLLOCAL_EDT1)CONTENT[]') TO <lt_code>.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF i_cursor_position = abap_true.
      DATA(l_cursor_position) = me->get_user_cursor_position( ).
    ENDIF.

    LOOP AT <lt_code> ASSIGNING FIELD-SYMBOL(<l_line>).

      IF sy-tabix = l_cursor_position.

        e_code = e_code && '\n@CURSOR_POSITION@\n'.

        IF e_t_code IS REQUESTED.
          APPEND INITIAL LINE TO e_t_code ASSIGNING FIELD-SYMBOL(<ls_code>).
          <ls_code> = '\n@CURSOR_POSITION@\n'.
        ENDIF.

      ENDIF.

      e_code = e_code && <l_line> && '\n'.

      IF e_t_code IS REQUESTED.
        APPEND INITIAL LINE TO e_t_code ASSIGNING <ls_code>.
        <ls_code> = <l_line>.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD get_user_cursor_position.

    r_cursor_position = 0.

    ASSIGN ('(SAPLLOCAL_EDT1)CURSOR-INDEX') TO FIELD-SYMBOL(<l_cursor_position>).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    TRY.

        r_cursor_position = <l_cursor_position>.

      CATCH cx_sy_conversion_no_number ##NO_HANDLER.
        RETURN.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
