CLASS ycl_aai_rest_async_task DEFINITION
  PUBLIC
  INHERITING FROM ycl_aai_rest_base
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_async_s,
             id         TYPE string,
             chat_id    TYPE string,
             name       TYPE yde_aai_async_task_name,
             status     TYPE yde_aai_async_task_status,
             username   TYPE usnam,
             start_date TYPE yde_aai_async_task_start_date,
             start_time TYPE yde_aai_async_task_start_time,
             end_date   TYPE yde_aai_async_task_end_date,
             end_time   TYPE yde_aai_async_task_end_time,
             response   TYPE yde_aai_response,
             cancelled  TYPE abap_bool,
           END OF ty_async_s,

           ty_async_t TYPE STANDARD TABLE OF ty_async_s WITH EMPTY KEY,

           BEGIN OF ty_response_query_s,
             tasks TYPE ty_async_t,
           END OF ty_response_query_s,

           BEGIN OF ty_response_read_s,
             task TYPE ty_async_s,
           END OF ty_response_read_s,

           BEGIN OF ty_async_task_update_s,
             id      TYPE string,
             updated TYPE abap_bool,
             error   TYPE string,
           END OF ty_async_task_update_s.

    METHODS yif_aai_rest_resource~read REDEFINITION.

    METHODS yif_aai_rest_resource~update REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_aai_rest_async_task IMPLEMENTATION.

  METHOD yif_aai_rest_resource~read.

    DATA: lt_rng_username TYPE RANGE OF yaai_async-username,
          lt_rng_date     TYPE RANGE OF yaai_log-log_date.

    DATA: ls_response_read  TYPE ty_response_read_s,
          ls_response_query TYPE ty_response_query_s.

    DATA l_json TYPE string.

    DATA(l_id) = to_upper( i_o_request->get_form_field( name = 'id' ) ).
    DATA(l_datefrom) = i_o_request->get_form_field( name = 'datefrom' ).
    DATA(l_dateto) = i_o_request->get_form_field( name = 'dateto' ).
    DATA(l_username) = i_o_request->get_form_field( name = 'username' ).

    IF l_id IS NOT INITIAL.

      SELECT SINGLE id, chat_id, name, status, username,
            startdate AS start_date, starttime AS start_time,
            enddate AS end_date, endtime AS end_time, response, cancelled
        FROM yaai_async
       WHERE id = @l_id
        INTO @DATA(ls_async).

      ls_response_read-task = CORRESPONDING #( ls_async ).

      l_json = /ui2/cl_json=>serialize(
        EXPORTING
          data = ls_response_read
          compress = abap_false
          pretty_name = /ui2/cl_json=>pretty_mode-camel_case
      ).

    ELSE.

      IF l_username IS NOT INITIAL.
        lt_rng_username = VALUE #( ( sign = 'I' option = 'EQ' low = l_username ) ).
      ENDIF.

      IF l_datefrom IS NOT INITIAL AND l_dateto IS NOT INITIAL.
        lt_rng_date = VALUE #( ( sign = 'I' option = 'BT' low = l_datefrom high = l_dateto ) ).
      ELSEIF l_datefrom IS NOT INITIAL AND l_dateto IS INITIAL.
        lt_rng_date = VALUE #( ( sign = 'I' option = 'EQ' low = l_datefrom ) ).
      ENDIF.

      SELECT id, chat_id, name, status, username,
             startdate AS start_date, starttime AS start_time, enddate AS end_date,
             endtime AS end_time, response, cancelled
      FROM yaai_async
        WHERE startdate IN @lt_rng_date
          AND username IN @lt_rng_username
          INTO TABLE @DATA(lt_async)
          UP TO 100 ROWS.

      LOOP AT lt_async ASSIGNING FIELD-SYMBOL(<ls_async>).

*        TRY.
*
*            DATA(lo_process_monitor) = cl_bgmc_process_factory=>create_monitor_from_string( <ls_async>-monitor ).
*
*            DATA(l_state) = lo_process_monitor->get_state( ).
*
*            "States of a background process:
*
*            "  The state is unknown.
*            "  Can e.g. happen if the monitored process is too old and the logs have been cleaned up.
*            "  unknown    VALUE IS INITIAL
*
*            "  An application or runtime error has occurred.
*            "  erroneous  VALUE 1
*
*            "  The process has not been started.
*            "  new        VALUE 2
*
*            "  The process is running.
*            "  running    VALUE 3
*
*            "  The process has been executed successfully.
*            "  successful VALUE 4
*
*            CASE l_state.
*
*              WHEN if_bgmc_process_monitor=>gcs_state-unknown.
*
*                IF <ls_async>-status <> yif_aai_async=>mc_task_unknown.
*
*                  <ls_async>-status = yif_aai_async=>mc_task_unknown.
*
*                  UPDATE yaai_async SET status = @<ls_async>-status
*                    WHERE id = @<ls_async>-id.
*
*                ENDIF.
*
*              WHEN if_bgmc_process_monitor=>gcs_state-new.
*
*                IF <ls_async>-status <> yif_aai_async=>mc_task_created.
*
*                  <ls_async>-status = yif_aai_async=>mc_task_created.
*
*                  UPDATE yaai_async SET status = @<ls_async>-status
*                    WHERE id = @<ls_async>-id.
*
*                ENDIF.
*
*              WHEN if_bgmc_process_monitor=>gcs_state-running.
*
*                IF <ls_async>-status <> yif_aai_async=>mc_task_running.
*
*                  <ls_async>-status = yif_aai_async=>mc_task_running.
*
*                  UPDATE yaai_async SET status = @<ls_async>-status
*                    WHERE id = @<ls_async>-id.
*
*                ENDIF.
*
*              WHEN if_bgmc_process_monitor=>gcs_state-successful.
*
*                IF <ls_async>-status <> yif_aai_async=>mc_task_finished.
*
*                  <ls_async>-status = yif_aai_async=>mc_task_finished.
*
*                  UPDATE yaai_async SET status = @<ls_async>-status
*                    WHERE id = @<ls_async>-id.
*
*                ENDIF.
*
*              WHEN if_bgmc_process_monitor=>gcs_state-erroneous.
*
*                IF <ls_async>-status <> yif_aai_async=>mc_task_failed.
*
*                  <ls_async>-status = yif_aai_async=>mc_task_failed.
*
*                  UPDATE yaai_async SET status = @<ls_async>-status
*                    WHERE id = @<ls_async>-id.
*
*                ENDIF.
*
*            ENDCASE.
*
*          CATCH cx_bgmc ##NO_HANDLER.
*            EXIT.
*        ENDTRY.

      ENDLOOP.

      ls_response_query-tasks = CORRESPONDING #( lt_async ).

      l_json = /ui2/cl_json=>serialize(
       EXPORTING
         data = ls_response_query
         compress = abap_false
         pretty_name = /ui2/cl_json=>pretty_mode-camel_case
      ).

    ENDIF.

    i_o_response->set_content_type( content_type = 'application/json' ).

    i_o_response->set_cdata(
      EXPORTING
        data = l_json
    ).

  ENDMETHOD.

  METHOD yif_aai_rest_resource~update.

    DATA ls_response_update TYPE ty_async_task_update_s.

    DATA l_json TYPE string.

    ls_response_update-id = to_upper( i_o_request->get_form_field( name = 'async_task_id' ) ).

    ls_response_update-updated = abap_false.

    DATA(l_action) = to_upper( i_o_request->get_form_field( name = 'action' ) ).

    IF ls_response_update-id IS INITIAL.

      "Not Found
      i_o_response->set_status(
        EXPORTING
          code = 404
          reason = 'Not Found'
      ).

      RETURN.

    ENDIF.

    IF l_action = 'CANCEL'.

      UPDATE yaai_async
        SET cancelled = @abap_true
        WHERE id = @ls_response_update-id.

      IF sy-subrc = 0.

        ls_response_update-updated = abap_true.

        SELECT SINGLE chat_id FROM yaai_async
          WHERE id = @ls_response_update-id
          INTO @DATA(l_chat_id).

        IF sy-subrc = 0.

          NEW ycl_aai_db(
            i_api = space
            i_id = l_chat_id
          )->block_chat( ).

        ENDIF.

      ELSE.
        ls_response_update-error = |Async Task { ls_response_update-id } does not exist.|.
      ENDIF.

    ENDIF.

    l_json = /ui2/cl_json=>serialize(
      EXPORTING
        data = ls_response_update
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
