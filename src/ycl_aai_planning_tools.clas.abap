CLASS ycl_aai_planning_tools DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES yif_aai_planning_tools.

    ALIASES create_plan FOR yif_aai_planning_tools~create_plan.
    ALIASES get_plan    FOR yif_aai_planning_tools~get_plan.
    ALIASES update_plan FOR yif_aai_planning_tools~update_plan.
    ALIASES delete_plan FOR yif_aai_planning_tools~delete_plan.

    METHODS constructor
      IMPORTING
        i_o_agent TYPE REF TO yif_aai_agent OPTIONAL.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA _o_agent TYPE REF TO yif_aai_agent.

ENDCLASS.



CLASS ycl_aai_planning_tools IMPLEMENTATION.

  METHOD constructor.

    IF i_o_agent IS SUPPLIED.

      me->_o_agent = i_o_agent.

    ENDIF.

  ENDMETHOD.

  METHOD create_plan.

    CLEAR r_response.

    IF i_plan IS INITIAL.
      r_response = 'The plan content is mandatory.'.
      RETURN.
    ENDIF.

    SELECT SINGLE id, chat_id, rag_id
      FROM yaai_agent_plan
      WHERE id = @me->_o_agent->m_agent_id
        AND chat_id = @me->_o_agent->m_chat_id
        INTO @DATA(ls_agent_plan).

    IF ls_agent_plan IS NOT INITIAL.
      r_response = 'There is already a plan created. Use the get_plan tool (if available) to retrieve it.'.
      RETURN.
    ENDIF.

    TRY.
        NEW ycl_aai_rag_db( )->create(
          EXPORTING
            i_filename    = |{ cl_system_uuid=>create_uuid_x16_static( ) }.md|
            i_content     = i_plan
            i_description = i_description
            i_keywords    = CONV string( me->_o_agent->m_chat_id )
          IMPORTING
            e_id          = DATA(l_id)
            e_error       = DATA(l_error)
        ).

        IF l_error IS NOT INITIAL.
          r_response = |Error: { l_error }|.
          RETURN.
        ENDIF.

      CATCH cx_uuid_error ##NO_HANDLER.
        r_response = 'An error occurred while creating the plan.'.
        RETURN.
    ENDTRY.

    IF l_id IS INITIAL.
      r_response = 'An unexpected error occurred'.
      RETURN.
    ENDIF.

    INSERT yaai_agent_plan FROM @( VALUE #( id = me->_o_agent->m_agent_id
                                             chat_id = me->_o_agent->m_chat_id
                                             rag_id = l_id ) ).

    r_response = 'Plan created successfully!'.

  ENDMETHOD.

  METHOD get_plan.

    CLEAR r_response.

    SELECT SINGLE id, chat_id, rag_id
      FROM yaai_agent_plan
      WHERE id = @me->_o_agent->m_agent_id
        AND chat_id = @me->_o_agent->m_chat_id
        INTO @DATA(ls_agent_plan).

    IF ls_agent_plan-rag_id IS INITIAL.
      r_response = 'No plan found.'.
      RETURN.
    ENDIF.

    NEW ycl_aai_rag_db( )->read(
      EXPORTING
        i_id          = ls_agent_plan-rag_id
      IMPORTING
        e_content     = r_response
        e_error       = DATA(l_error)
    ).

    IF l_error IS NOT INITIAL.
      r_response = |Error: { l_error }|.
      RETURN.
    ENDIF.

    IF r_response IS NOT INITIAL.
      r_response = |No plan found.|.
      RETURN.
    ENDIF.

  ENDMETHOD.

  METHOD update_plan.

    CLEAR r_response.

    SELECT SINGLE id, chat_id, rag_id
      FROM yaai_agent_plan
      WHERE id = @me->_o_agent->m_agent_id
        AND chat_id = @me->_o_agent->m_chat_id
        INTO @DATA(ls_agent_plan).

    IF ls_agent_plan-rag_id IS INITIAL.
      r_response = 'No plan found.'.
      RETURN.
    ENDIF.

    IF i_plan IS INITIAL.
      r_response = 'The plan content is mandatory.'.
      RETURN.
    ENDIF.

    NEW ycl_aai_rag_db( )->update(
      EXPORTING
        i_id          = ls_agent_plan-rag_id
        i_content     = i_plan
        i_description = i_description
        i_append      = i_append
      IMPORTING
        e_updated     = DATA(l_updated)
        e_error       = DATA(l_error)
    ).

    IF l_error IS NOT INITIAL.
      r_response = |Error: { l_error }|.
      RETURN.
    ENDIF.

    IF l_updated = abap_true.
      r_response = r_response = 'Plan updated successfully!'.
    ELSE.
      r_response = 'An unexpected error occurred'.
    ENDIF.

  ENDMETHOD.

  METHOD delete_plan.

    CLEAR r_response.

    SELECT SINGLE id, chat_id, rag_id
      FROM yaai_agent_plan
      WHERE id = @me->_o_agent->m_agent_id
        AND chat_id = @me->_o_agent->m_chat_id
        INTO @DATA(ls_agent_plan).

    IF ls_agent_plan-rag_id IS INITIAL.
      r_response = 'No plan found.'.
      RETURN.
    ENDIF.

    NEW ycl_aai_rag_db( )->delete(
      EXPORTING
        i_id       = ls_agent_plan-rag_id
      IMPORTING
        e_deleted  = DATA(l_deleted)
        e_error    = DATA(l_error)
    ).

    IF l_error IS NOT INITIAL.
      r_response = |Error: { l_error }|.
      RETURN.
    ENDIF.

    IF l_deleted = abap_true.

      DELETE FROM yaai_agent_plan
        WHERE id = @me->_o_agent->m_agent_id
          AND chat_id = @me->_o_agent->m_chat_id.

      r_response = r_response = 'Plan deleted successfully!'.

    ELSE.
      r_response = 'An unexpected error occurred'.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
