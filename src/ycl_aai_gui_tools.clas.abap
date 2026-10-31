CLASS ycl_aai_gui_tools DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES ty_row TYPE c LENGTH 262143.

    METHODS get_screenshot
      EXPORTING
                e_t_files         TYPE ytt_aai_files
      RETURNING VALUE(r_response) TYPE string.

    METHODS set_clipboard_data
      IMPORTING
                i_clipboard_data  TYPE string
      RETURNING VALUE(r_response) TYPE string.

    METHODS get_clipboard_data
      RETURNING VALUE(r_response) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS YCL_AAI_GUI_TOOLS IMPLEMENTATION.


  METHOD get_clipboard_data.

    DATA lt_data TYPE STANDARD TABLE OF ty_row.

    cl_gui_frontend_services=>clipboard_import(
      IMPORTING
        data                 = lt_data
        length               = DATA(l_length)
      EXCEPTIONS
        cntl_error           = 1                " Control error
        error_no_gui         = 2                " No GUI available
        not_supported_by_gui = 3                " GUI does not support this
        OTHERS               = 4
    ).

    IF sy-subrc <> 0.

      r_response = 'Error while importing data from the clipboard.'.

    ELSE.

      IF lt_data IS INITIAL.
        r_response = 'The clipboard is empty.'.
      ENDIF.

      LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<l_row>).

        IF r_response IS INITIAL.
          r_response = <l_row>.
        ELSE.
          r_response = |{ r_response }{ cl_abap_char_utilities=>newline }{ <l_row> }|.
        ENDIF.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD get_screenshot.

    DATA: l_mime_type      TYPE string,
          l_file_extension TYPE string,
          l_image          TYPE xstring.

    cl_gui_frontend_services=>get_screenshot(
      IMPORTING
        mime_type_str        = l_mime_type
        image                = l_image
      EXCEPTIONS
        access_denied        = 1
        cntl_error           = 2
        error_no_gui         = 3
        not_supported_by_gui = 4
        OTHERS               = 5
    ).

    IF sy-subrc <> 0.

      IF sy-subrc = 1.
        r_response = 'Error: access denied.'.
      ELSE.
        r_response = 'An error occurred while generating screenshot.'.
      ENDIF.

    ELSE.

      r_response = 'Here is the requested screenshot.'.

      SPLIT l_mime_type AT '/' INTO DATA(l_dummy) l_file_extension.

      IF l_file_extension IS NOT INITIAL.
        l_file_extension = condense( '.' && l_file_extension ).
      ENDIF.

      e_t_files = VALUE #( ( content = cl_http_utility=>encode_x_base64( unencoded = l_image )
                             filename = |screenshot{ l_file_extension }|
                             file_size = xstrlen( l_image )
                             file_type = l_mime_type ) ).

    ENDIF.

  ENDMETHOD.


  METHOD set_clipboard_data.

    DATA lt_data TYPE STANDARD TABLE OF ty_row.

    DATA l_rc TYPE i.

    SPLIT i_clipboard_data AT cl_abap_char_utilities=>newline INTO TABLE lt_data.

    cl_gui_frontend_services=>clipboard_export(
      IMPORTING
        data                 = lt_data                 " Data
      CHANGING
        rc                   = l_rc                 " Return Code
      EXCEPTIONS
        cntl_error           = 1                " Control error
        error_no_gui         = 2                " No GUI available
        not_supported_by_gui = 3                " GUI does not support this
        no_authority         = 4                " Authorization check failed
        OTHERS               = 5
    ).

    IF sy-subrc <> 0 OR l_rc <> 0.

      r_response = 'Error while exporting data to the clipboard.'.

    ELSE.

      LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<l_row>).

        IF r_response IS INITIAL.
          r_response = <l_row>.
        ELSE.
          r_response = |{ r_response }{ cl_abap_char_utilities=>newline }{ <l_row> }|.
        ENDIF.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
