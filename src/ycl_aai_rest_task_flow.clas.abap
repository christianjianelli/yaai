CLASS ycl_aai_rest_task_flow DEFINITION
  PUBLIC
  INHERITING FROM ycl_aai_rest_base
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_step_s,
             id                 TYPE string,
             task_id            TYPE string,
             task_name          TYPE string,
             previous_task_id   TYPE string,
             previous_task_name TYPE string,
           END OF ty_step_s,

           ty_steps_t TYPE STANDARD TABLE OF ty_step_s WITH EMPTY KEY,

           BEGIN OF ty_task_s,
             id          TYPE string,
             name        TYPE string,
             description TYPE string,
             task_flow   TYPE ty_steps_t,
           END OF ty_task_s,

           ty_request_create_s TYPE ty_task_s,

           BEGIN OF ty_response_create_s,
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

           ty_tasks_t TYPE STANDARD TABLE OF ty_task_s WITH EMPTY KEY,

           BEGIN OF ty_response_read_s,
             tasks TYPE ty_tasks_t,
           END OF ty_response_read_s.

    METHODS yif_aai_rest_resource~create REDEFINITION.

    METHODS yif_aai_rest_resource~read REDEFINITION.

    METHODS yif_aai_rest_resource~update REDEFINITION.

    METHODS yif_aai_rest_resource~delete REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS ycl_aai_rest_task_flow IMPLEMENTATION.

  METHOD yif_aai_rest_resource~create.

    DATA: ls_request  TYPE ty_request_create_s,
          ls_response TYPE ty_response_create_s.

    DATA: l_json TYPE string,
          l_id   TYPE yaai_task-id.


    DATA(l_body) = i_o_request->get_cdata( ).

    /ui2/cl_json=>deserialize(
      EXPORTING
        json        = l_body
        pretty_name = /ui2/cl_json=>pretty_mode-camel_case
      CHANGING
        data        = ls_request
    ).

    TRY.

        l_id = cl_system_uuid=>create_uuid_x16_static( ).

      CATCH cx_uuid_error ##NO_HANDLER.

        ls_response-error = 'Error while creating the task flow.'.

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
                                      name = ls_request-name
                                      description = ls_request-description
                                      task_flow = abap_true ) ).
    IF sy-subrc <> 0.

      ls_response-error = 'Error while creating the task flow.'.

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

    ENDIF.

    LOOP AT ls_request-task_flow ASSIGNING FIELD-SYMBOL(<ls_step>).

      INSERT yaai_task_flow FROM @( VALUE #( id = l_id
                                             task_id = <ls_step>-task_id
                                             previous_task_id = <ls_step>-previous_task_id ) ).

      IF sy-subrc <> 0.
        DATA(l_rollback) = abap_true.
      ENDIF.

    ENDLOOP.

    IF l_rollback = abap_true.

      ROLLBACK WORK.

      ls_response-error = 'Error while creating the task flow.'.

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

    ENDIF.

    ls_response-created = abap_true.

    ls_response-id = l_id.

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

    DATA lt_rng_name TYPE RANGE OF yaai_task-name.

    DATA(ls_response) = VALUE ty_response_read_s( ).

    DATA l_json TYPE string.

    DATA(l_id) = i_o_request->get_form_field( name = 'id' ).

    DATA(l_name) = i_o_request->get_form_field( name = 'name' ).

    DATA(l_description) = i_o_request->get_form_field( name = 'description' ).

    IF l_id IS NOT INITIAL.

      SELECT SINGLE id, name, description
        FROM yaai_task
        WHERE id = @l_id
          AND task_flow = @abap_true
        INTO @DATA(ls_task).

      IF sy-subrc = 0.

        APPEND INITIAL LINE TO ls_response-tasks ASSIGNING FIELD-SYMBOL(<ls_task>).

        <ls_task> = CORRESPONDING #( ls_task ).

        SELECT a~id, a~task_id, b~name AS task_name, a~previous_task_id, c~name AS previous_task_name
          FROM yaai_task_flow AS a
          INNER JOIN yaai_task AS b
          ON a~task_id = b~id
          INNER JOIN yaai_task AS c
          ON a~previous_task_id = c~id
          WHERE a~id = @l_id
          INTO TABLE @DATA(lt_task_flow).

        LOOP AT lt_task_flow ASSIGNING FIELD-SYMBOL(<ls_step>).

          APPEND VALUE #( id = <ls_step>-id
                          task_id = <ls_step>-task_id
                          task_name = <ls_step>-task_name
                          previous_task_id = <ls_step>-previous_task_id
                          previous_task_name = <ls_step>-previous_task_name ) TO <ls_task>-task_flow.

        ENDLOOP.

      ENDIF.

    ELSE.

      IF l_name IS NOT INITIAL.
        lt_rng_name = VALUE #( ( sign = 'I' option = 'CP' low = |*{ l_name }*| ) ).
      ENDIF.

      SELECT id, name, description
        FROM yaai_task
        WHERE name IN @lt_rng_name
          AND task_flow = @abap_true
        INTO TABLE @DATA(lt_task).

      LOOP AT lt_task ASSIGNING FIELD-SYMBOL(<ls_task_query>).

        IF l_description IS NOT INITIAL.

          IF NOT <ls_task_query>-description CP |*{ l_description }*|.

            CONTINUE.

          ENDIF.

        ENDIF.

        APPEND VALUE #( id = <ls_task_query>-id
                        name = <ls_task_query>-name
                        description = <ls_task_query>-description ) TO ls_response-tasks.

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

    DATA: ls_request  TYPE ty_request_create_s,
          ls_response TYPE ty_response_update_s.

    DATA: l_json TYPE string,
          l_id   TYPE yaai_task-id.


    DATA(l_body) = i_o_request->get_cdata( ).

    /ui2/cl_json=>deserialize(
      EXPORTING
        json        = l_body
        pretty_name = /ui2/cl_json=>pretty_mode-camel_case
      CHANGING
        data        = ls_request
    ).

    l_id = ls_request-id.

    UPDATE yaai_task FROM @( VALUE #( id = l_id
                                      name = ls_request-name
                                      description = ls_request-description
                                      task_flow = abap_true ) ).
    IF sy-subrc <> 0.

      ls_response-error = 'Error while updating the task flow.'.

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

    ENDIF.

    DELETE FROM yaai_task_flow WHERE id = @l_id.

    LOOP AT ls_request-task_flow ASSIGNING FIELD-SYMBOL(<ls_step>).

      INSERT yaai_task_flow FROM @( VALUE #( id = l_id
                                             task_id = <ls_step>-task_id
                                             previous_task_id = <ls_step>-previous_task_id ) ).

      IF sy-subrc <> 0.
        DATA(l_rollback) = abap_true.
      ENDIF.

    ENDLOOP.

    IF l_rollback = abap_true.

      ROLLBACK WORK.

      ls_response-error = 'Error while updating the task flow.'.

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

    ENDIF.

    ls_response-updated = abap_true.

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

    ls_response-deleted = abap_true.

    DELETE FROM yaai_task WHERE id = @l_id.

    IF sy-subrc <> 0.
      ls_response-deleted = abap_false.
      ls_response-error = 'Error while deleting the task flow.'.
    ELSE.

      DELETE FROM yaai_task_flow WHERE id = @l_id.

      IF sy-subrc <> 0.
        ls_response-deleted = abap_false.
        ls_response-error = 'Error while deleting the task flow.'.
        ROLLBACK WORK.
      ENDIF.

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
