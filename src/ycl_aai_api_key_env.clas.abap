CLASS ycl_aai_api_key_env DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES yif_aai_api_key.

    ALIASES read FOR yif_aai_api_key~read.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS ycl_aai_api_key_env IMPLEMENTATION.

  METHOD yif_aai_api_key~create.
    MESSAGE 'Not available' TYPE 'S'.
  ENDMETHOD.

  METHOD yif_aai_api_key~delete.
    MESSAGE 'Not available' TYPE 'S'.
  ENDMETHOD.

  METHOD yif_aai_api_key~read.

    cl_gui_frontend_services=>environment_get_variable(
      EXPORTING
        variable             = i_id               " Environment Variable Name
      CHANGING
        value                = r_api_key          " Variable Value
      EXCEPTIONS
        cntl_error           = 1                  " Control error
        error_no_gui         = 2                  " No GUI available
        not_supported_by_gui = 3                  " GUI does not support this
        OTHERS               = 4
    ).

    IF sy-subrc = 0.
      CALL METHOD cl_gui_cfw=>flush.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
