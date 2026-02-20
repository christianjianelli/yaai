CLASS ycl_aai_rest_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_http_extension.

    CONSTANTS: mc_superclass_name      TYPE c LENGTH 30 VALUE 'YCL_AAI_REST_BASE'            ##NO_TEXT,
               mc_rest_resource_prefix TYPE c LENGTH 30 VALUE 'YCL_AAI_REST'                 ##NO_TEXT,
               mc_create               TYPE string      VALUE 'YIF_AAI_REST_RESOURCE~CREATE' ##NO_TEXT,
               mc_read                 TYPE string      VALUE 'YIF_AAI_REST_RESOURCE~READ'   ##NO_TEXT,
               mc_update               TYPE string      VALUE 'YIF_AAI_REST_RESOURCE~UPDATE' ##NO_TEXT,
               mc_delete               TYPE string      VALUE 'YIF_AAI_REST_RESOURCE~DELETE' ##NO_TEXT.

  PROTECTED SECTION.

  PRIVATE SECTION.

    METHODS get_rest_resource_instance
      IMPORTING
        i_resource   TYPE csequence
      EXPORTING
        e_o_resource TYPE REF TO object
        e_error      TYPE string.

ENDCLASS.



CLASS ycl_aai_rest_handler IMPLEMENTATION.

  METHOD if_http_extension~handle_request.

    DATA(l_path_info) = server->request->get_header_field( name = '~path_info' ).

    SPLIT l_path_info AT '/' INTO TABLE DATA(lt_path_info).

    DELETE lt_path_info WHERE table_line IS INITIAL.

    IF lt_path_info[] IS INITIAL.

      server->response->set_status(
         EXPORTING
           code = 404
           reason = 'Not found'
       ).

      RETURN.

    ENDIF.

    " Logoff
    IF to_lower( lt_path_info[ 1 ] ) = 'logoff'.

      server->logoff(
        EXPORTING
          redirect_url              = '/sap/public/bc/icf/logoff'
        EXCEPTIONS
          logoff_not_possible       = 0
          OTHERS                    = 0
      ).

      RETURN.

    ENDIF.

    " Stateless or Statefull session
    server->set_session_stateful(
      stateful = COND #( WHEN server->request->get_form_field( name = 'statefull' ) IS NOT INITIAL
                         THEN server->co_enabled
                         ELSE server->co_disabled )
      path     = ''
    ).

    " Get Resource Instance
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    me->get_rest_resource_instance(
      EXPORTING
        i_resource   = lt_path_info[ 1 ]
      IMPORTING
        e_o_resource = DATA(lo_resource)
    ).

    IF lo_resource IS NOT BOUND.

      server->response->set_status(
        EXPORTING
          code = 404
          reason = 'Not found'
      ).

      RETURN.

    ENDIF.

    DATA(l_method) = to_upper( server->request->get_method( ) ).

    DATA(l_method_name) = COND string( WHEN l_method = 'POST'   THEN mc_create
                                       WHEN l_method = 'GET'    THEN mc_read
                                       WHEN l_method = 'PUT'    THEN mc_update
                                       WHEN l_method = 'PATCH'  THEN mc_update
                                       WHEN l_method = 'DELETE' THEN mc_delete
                                       ELSE space ).

    IF l_method_name = space.

      server->response->set_status(
        EXPORTING
          code = 405
          reason = 'Not allowed'
      ).

      RETURN.

    ENDIF.

    " Call Handler
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    TRY.

        CALL METHOD lo_resource->(l_method_name)
          EXPORTING
            i_o_request  = server->request
            i_o_response = server->response.

        RETURN.

      CATCH cx_sy_dyn_call_illegal_method
            cx_sy_dyn_call_illegal_type
            cx_sy_dyn_call_param_missing
            cx_sy_dyn_call_param_not_found
            cx_sy_ref_is_initial INTO DATA(lo_exception).

        DATA(l_error) = lo_exception->get_text( ).

    ENDTRY.

    server->response->set_status(
      EXPORTING
        code   = 500
        reason = l_error
    ).

  ENDMETHOD.

  METHOD get_rest_resource_instance.

    SELECT clsname, refclsname
      FROM vseoextend
      INTO TABLE @DATA(lt_classes)
      WHERE refclsname = @mc_superclass_name
        AND version    = '1'
        AND state      = '1'.

    LOOP AT lt_classes ASSIGNING FIELD-SYMBOL(<ls_class>).

      IF <ls_class>-clsname = |{ mc_rest_resource_prefix }_{ to_upper( i_resource ) }|.

        TRY.

            CREATE OBJECT e_o_resource TYPE (<ls_class>-clsname).

          CATCH cx_sy_create_object_error
                cx_sy_dyn_call_illegal_class
                cx_sy_dyn_call_illegal_method
                cx_sy_dyn_call_illegal_type
                cx_sy_dyn_call_param_missing
                cx_sy_dyn_call_param_not_found
                cx_sy_ref_is_initial INTO DATA(lo_ex_create_object).

            e_error = lo_ex_create_object->get_text( ).

            RETURN.

        ENDTRY.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
