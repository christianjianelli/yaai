CLASS ycl_aai_agent_task_tools DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun.

    METHODS constructor
      IMPORTING
        i_o_agent TYPE REF TO yif_aai_agent OPTIONAL.

    METHODS get_tasks
      RETURNING VALUE(r_response) TYPE string.

    METHODS update_task_status
      IMPORTING
                i_task_id         TYPE string
                i_task_status     TYPE yde_aai_task_status
      RETURNING VALUE(r_response) TYPE string.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA _o_agent TYPE REF TO yif_aai_agent.

ENDCLASS.



CLASS ycl_aai_agent_task_tools IMPLEMENTATION.

  METHOD constructor.

    IF i_o_agent IS SUPPLIED.

      me->_o_agent = i_o_agent.

    ENDIF.

  ENDMETHOD.

  METHOD get_tasks.

    CLEAR r_response.

    IF me->_o_agent IS NOT BOUND.
      RETURN.
    ENDIF.

    SELECT a~id, a~task_id, a~previous_task_id, a~status, b~name, b~description
      FROM yaai_agent_task AS a
      INNER JOIN yaai_task AS b
      ON a~task_id = b~id
      WHERE a~id = @me->_o_agent->m_agent_id
        AND a~chat_id = @me->_o_agent->m_chat_id
        AND a~blocked = @abap_false
      INTO TABLE @DATA(lt_tasks).

    r_response = |Task Id, Name, Status, Previous Task Id, Description|.

    LOOP AT lt_tasks ASSIGNING FIELD-SYMBOL(<ls_task>).

      r_response = |{ r_response }{ cl_abap_char_utilities=>newline }|.
      r_response = |{ r_response }{ <ls_task>-id },{ <ls_task>-name },{ <ls_task>-status },{ <ls_task>-previous_task_id },{ <ls_task>-description }|.

    ENDLOOP.

  ENDMETHOD.

  METHOD update_task_status.

    CLEAR r_response.

    IF me->_o_agent IS NOT BOUND.
      RETURN.
    ENDIF.

    UPDATE yaai_agent_task
      SET status = @i_task_status
      WHERE id = @me->_o_agent->m_agent_id
        AND chat_id = @me->_o_agent->m_chat_id
        AND task_id = @i_task_id.

    IF sy-subrc = 0.
      r_response = |Task { i_task_id } status updated successfully.|.
    ELSE.
      r_response = |Task { i_task_id } status not updated.|.
    ENDIF.

  ENDMETHOD.

  METHOD if_oo_adt_classrun~main.

    DATA l_response TYPE string.

    me->_o_agent = NEW ycl_aai_agent(
      i_agent_id = '000C2956EF8E1FD18E837D19BD4E6C78'
      i_chat_id  = '00000000000000000000000000000001'
    ).

    DATA(l_get_tasks) = abap_true.
    DATA(l_update_task_status) = abap_false.

    CASE abap_true.

      WHEN l_get_tasks.

        l_response = me->get_tasks( ).

      WHEN l_update_task_status.

        l_response = me->update_task_status(
                       i_task_id     = '000C2956EF8E1FE18FC9738D08658D36'
                       i_task_status = 'X'
                     ).

    ENDCASE.

    out->write( l_response ).

  ENDMETHOD.

ENDCLASS.
