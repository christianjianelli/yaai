CLASS ycl_aai_async DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES yif_aai_async.

    ALIASES create FOR yif_aai_async~create.
    ALIASES read FOR yif_aai_async~read.
    ALIASES update FOR yif_aai_async~update.
    ALIASES delete FOR yif_aai_async~delete.
    ALIASES run FOR yif_aai_async~run.
    ALIASES update_status FOR yif_aai_async~update_status.
    ALIASES get_status FOR yif_aai_async~get_status.
    ALIASES get_response FOR yif_aai_async~get_response.
    ALIASES update_response FOR yif_aai_async~update_response.
    ALIASES on_end_of_task FOR yif_aai_async~on_end_of_task.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS ycl_aai_async IMPLEMENTATION.

  METHOD yif_aai_async~create.

    CLEAR r_task_id.

    TRY.

        DATA(ls_task) = VALUE yaai_async( id = cl_system_uuid=>create_uuid_x16_static( )
                                          chat_id = i_chat_id
                                          name = i_task_name
                                          status = yif_aai_async~mc_task_created
                                          username = sy-uname
                                          startdate = sy-datlo
                                          starttime = sy-timlo ).

      CATCH cx_uuid_error ##NO_HANDLER.
        RETURN.
    ENDTRY.

    INSERT yaai_async FROM @ls_task.

    IF sy-subrc = 0.
      r_task_id = ls_task-id.
    ENDIF.

  ENDMETHOD.


  METHOD yif_aai_async~read.

    SELECT SINGLE id, chat_id, name, status, username, startdate, starttime
      FROM yaai_async
      WHERE id = @i_task_id
      INTO CORRESPONDING FIELDS OF @e_s_task.

  ENDMETHOD.


  METHOD yif_aai_async~update.

    r_updated = abap_false.

    DATA(ls_task) = CORRESPONDING yaai_async( i_s_task ).

    UPDATE yaai_async FROM @ls_task.

    IF sy-subrc = 0.

      r_updated = abap_true.

    ENDIF.

  ENDMETHOD.


  METHOD yif_aai_async~delete.

    r_deleted = abap_false.

    DELETE FROM yaai_async WHERE id = @i_task_id.

    IF sy-subrc = 0.

      r_deleted = abap_true.

    ENDIF.

  ENDMETHOD.


  METHOD yif_aai_async~run.

    DATA l_task_id TYPE c LENGTH 32.

    r_started = abap_false.

    IF i_debug = abap_false.

      l_task_id = i_task_id.

      CALL FUNCTION 'Y_F_AAI_RUN_ASYNC_TASK' STARTING NEW TASK l_task_id
        CALLING on_end_of_task ON END OF TASK
        EXPORTING
          i_api      = i_api
          i_task_id  = i_task_id
          i_chat_id  = i_chat_id
          i_api_key  = i_api_key
          i_message  = i_message
          i_context  = i_context
          i_agent_id = i_agent_id
          i_model    = i_model
          i_log      = abap_true.

    ELSE.

      CALL FUNCTION 'Y_F_AAI_RUN_ASYNC_TASK'
        EXPORTING
          i_api      = i_api
          i_task_id  = i_task_id
          i_chat_id  = i_chat_id
          i_api_key  = i_api_key
          i_message  = i_message
          i_context  = i_context
          i_agent_id = i_agent_id
          i_model    = i_model
          i_log      = abap_true.

    ENDIF.

    r_started = abap_true.

  ENDMETHOD.


  METHOD yif_aai_async~update_status.

    DATA: l_enddate TYPE d,
          l_endtime TYPE t.

    r_updated = abap_false.

    IF i_status = yif_aai_async=>mc_task_finished OR
       i_status = yif_aai_async=>mc_task_cancelled.

      l_enddate = sy-datlo.
      l_endtime = sy-timlo.

    ENDIF.

    UPDATE yaai_async
      SET status = @i_status,
          enddate = @l_enddate,
          endtime = @l_endtime
      WHERE id = @i_task_id.

    IF sy-subrc = 0.

      r_updated = abap_true.

    ENDIF.

  ENDMETHOD.


  METHOD yif_aai_async~get_status.

    CLEAR r_status.

    SELECT SINGLE status
      FROM yaai_async
      WHERE id = @i_task_id
      INTO @r_status.

  ENDMETHOD.


  METHOD yif_aai_async~get_tasks_by_chat_id.

    SELECT id, chat_id, status, username, startdate, starttime
      FROM yaai_async
      WHERE chat_id = @i_chat_id
      INTO CORRESPONDING FIELDS OF TABLE @r_t_tasks.

  ENDMETHOD.


  METHOD yif_aai_async~get_response.

    CLEAR r_response.

    SELECT SINGLE response
      FROM yaai_async
      WHERE id = @i_task_id
      INTO @r_response.

  ENDMETHOD.


  METHOD yif_aai_async~update_response.

    r_updated = abap_false.

    UPDATE yaai_async
      SET response = @i_response
      WHERE id = @i_task_id.

    IF sy-subrc = 0.

      r_updated = abap_true.

    ENDIF.

  ENDMETHOD.

  METHOD on_end_of_task.

    "TODO

    IF 1 = 2.

    ENDIF.

  ENDMETHOD.

ENDCLASS.
