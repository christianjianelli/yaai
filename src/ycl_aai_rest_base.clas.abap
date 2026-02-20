CLASS ycl_aai_rest_base DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES yif_aai_rest_resource.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_aai_rest_base IMPLEMENTATION.

  METHOD yif_aai_rest_resource~create.

    i_o_response->set_status(
      EXPORTING
        code = 405
        reason = 'Method not allowed'
    ).

  ENDMETHOD.

  METHOD yif_aai_rest_resource~read.

    i_o_response->set_status(
      EXPORTING
        code = 405
        reason = 'Method not allowed'
    ).

  ENDMETHOD.

  METHOD yif_aai_rest_resource~update.

    i_o_response->set_status(
      EXPORTING
        code = 405
        reason = 'Method not allowed'
    ).

  ENDMETHOD.

  METHOD yif_aai_rest_resource~delete.

    i_o_response->set_status(
      EXPORTING
        code = 405
        reason = 'Method not allowed'
    ).

  ENDMETHOD.

ENDCLASS.
