CLASS ycl_aai_gui_tools DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS get_screenshot
      EXPORTING
                e_t_files         TYPE ytt_aai_files
      RETURNING VALUE(r_response) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_aai_gui_tools IMPLEMENTATION.

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

ENDCLASS.
