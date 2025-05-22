*"* use this source file for your ABAP unit test classes
CLASS ltcl_conn DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      connection_test FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_conn IMPLEMENTATION.

  METHOD connection_test.

    DATA(lo_cut) = NEW ycl_aai_conn( i_api = yif_aai_const=>c_ollama ).

    IF lo_cut->create_connection( i_endpoint = '' ) = abap_true.

      lo_cut->do_receive(
        IMPORTING
          e_response = DATA(l_response)
      ).

    ENDIF.

    cl_abap_unit_assert=>assert_not_initial(
      EXPORTING
        act = l_response                    " Actual Data Object
    ).

  ENDMETHOD.

ENDCLASS.
