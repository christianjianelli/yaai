CLASS ycl_aai_rest_task DEFINITION
  PUBLIC
  INHERITING FROM ycl_aai_rest_base
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_response_create_s,
             created TYPE abap_bool,
             id      TYPE string,
             error   TYPE string,
           END OF ty_response_create_s,

           BEGIN OF ty_response_update_s,
             updated TYPE abap_bool,
             error   TYPE string,
           END OF ty_response_update_s,

           BEGIN OF ty_response_delete_s,
             deleted TYPE abap_bool,
             error   TYPE string,
           END OF ty_response_delete_s,

           BEGIN OF ty_task_s,
             id          TYPE string,
             name        TYPE yaai_task-name,
             description TYPE yaai_task-description,
           END OF ty_task_s,

           ty_task_t TYPE STANDARD TABLE OF ty_task_s WITH EMPTY KEY,

           BEGIN OF ty_response_read_s,
             tasks TYPE ty_task_t,
           END OF ty_response_read_s.

    METHODS yif_aai_rest_resource~create REDEFINITION.

    METHODS yif_aai_rest_resource~read REDEFINITION.

    METHODS yif_aai_rest_resource~update REDEFINITION.

    METHODS yif_aai_rest_resource~delete REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_aai_rest_task IMPLEMENTATION.

  METHOD yif_aai_rest_resource~create.

    DATA l_json TYPE string.

    DATA(ls_response) = VALUE ty_response_create_s( ).

    DATA(l_name) = i_o_request->get_form_field( name = 'name' ).

    DATA(l_description) = i_o_request->get_form_field( name = 'description' ).

    TRY.

        DATA(l_id) = cl_system_uuid=>create_uuid_x16_static( ).

        ls_response-id = l_id.

      CATCH cx_uuid_error ##NO_HANDLER.

        ls_response-error = 'Error while creating the task.'.

        l_json = /ui2/cl_json=>serialize(
          EXPORTING
            data = ls_response
            compress = abap_false
            pretty_name = /ui2/cl_json=>pretty_mode-camel_case
        ).

        i_o_response->set_content_type( content_type = 'application/json' ).

        i_o_response->set_cdata(
          EXPORTING
            data = l_json
        ).

        RETURN.

    ENDTRY.

    INSERT yaai_task FROM @( VALUE #( id = l_id
                                      name = l_name
                                      description = l_description ) ).

    ls_response-created = abap_true.

    l_json = /ui2/cl_json=>serialize(
      EXPORTING
        data = ls_response
        compress = abap_false
        pretty_name = /ui2/cl_json=>pretty_mode-camel_case
    ).

    i_o_response->set_content_type( content_type = 'application/json' ).

    i_o_response->set_cdata(
      EXPORTING
        data = l_json
    ).

  ENDMETHOD.

  METHOD yif_aai_rest_resource~read.

    DATA(ls_response) = VALUE ty_response_read_s( ).

    DATA l_json TYPE string.

    DATA(l_id) = i_o_request->get_form_field( name = 'id' ).

    DATA(l_name) = i_o_request->get_form_field( name = 'name' ).

    DATA(l_description) = i_o_request->get_form_field( name = 'description' ).

    l_name = to_lower( l_name ).
    l_description = to_lower( l_description ).

    IF l_id IS NOT INITIAL.

      SELECT SINGLE id, name, description
        FROM yaai_task
        WHERE id = @l_id
          AND task_flow = @space
        INTO @DATA(ls_task).

      IF sy-subrc = 0.

        APPEND VALUE #( id = ls_task-id
                        name = ls_task-name
                        description = ls_task-description ) TO ls_response-tasks.

      ENDIF.

    ELSE.

      SELECT id, name, description
        FROM yaai_task
        WHERE task_flow = @space
        INTO TABLE @DATA(lt_task).

      LOOP AT lt_task ASSIGNING FIELD-SYMBOL(<ls_task>).

        IF l_name IS NOT INITIAL.

          IF NOT to_lower( <ls_task>-name ) CP |*{ l_name }*|.

            CONTINUE.

          ENDIF.

        ENDIF.

        IF l_description IS NOT INITIAL.

          IF NOT to_lower( <ls_task>-description ) CP |*{ l_description }*|.

            CONTINUE.

          ENDIF.

        ENDIF.

        APPEND VALUE #( id = <ls_task>-id
                        name = <ls_task>-name
                        description = <ls_task>-description ) TO ls_response-tasks.

      ENDLOOP.

    ENDIF.

    l_json = /ui2/cl_json=>serialize(
      EXPORTING
        data = ls_response
        compress = abap_false
        pretty_name = /ui2/cl_json=>pretty_mode-camel_case
    ).

    i_o_response->set_content_type( content_type = 'application/json' ).

    i_o_response->set_cdata(
      EXPORTING
        data = l_json
    ).

  ENDMETHOD.

  METHOD yif_aai_rest_resource~update.

    DATA l_json TYPE string.

    DATA(ls_response) = VALUE ty_response_update_s( ).

    DATA(l_id) = i_o_request->get_form_field( name = 'id' ).

    DATA(l_name) = i_o_request->get_form_field( name = 'name' ).

    DATA(l_description) = i_o_request->get_form_field( name = 'description' ).

    UPDATE yaai_task FROM @( VALUE #( id = l_id
                                      name = l_name
                                      description = l_description ) ).

    IF sy-subrc = 0.
      ls_response-updated = abap_true.
    ENDIF.

    l_json = /ui2/cl_json=>serialize(
      EXPORTING
        data = ls_response
        compress = abap_false
        pretty_name = /ui2/cl_json=>pretty_mode-camel_case
    ).

    i_o_response->set_content_type( content_type = 'application/json' ).

    i_o_response->set_cdata(
      EXPORTING
        data = l_json
    ).

  ENDMETHOD.

  METHOD yif_aai_rest_resource~delete.

    DATA l_json TYPE string.

    DATA(ls_response) = VALUE ty_response_delete_s( ).

    DATA(l_id) = i_o_request->get_form_field( name = 'id' ).

    DELETE FROM yaai_task WHERE id = @l_id.

    IF sy-subrc = 0.
      ls_response-deleted = abap_true.
    ELSE.
      ls_response-error = 'Error while deleting the task.'.
    ENDIF.

    l_json = /ui2/cl_json=>serialize(
      EXPORTING
        data = ls_response
        compress = abap_false
        pretty_name = /ui2/cl_json=>pretty_mode-camel_case
    ).

    i_o_response->set_content_type( content_type = 'application/json' ).

    i_o_response->set_cdata(
      EXPORTING
        data = l_json
    ).

  ENDMETHOD.

ENDCLASS.
