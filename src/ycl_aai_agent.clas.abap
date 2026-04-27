CLASS ycl_aai_agent DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES yif_aai_agent.

    ALIASES run_autonomously FOR yif_aai_agent~run_autonomously.
    ALIASES get_system_instructions FOR yif_aai_agent~get_system_instructions.
    ALIASES get_tools FOR yif_aai_agent~get_tools.
    ALIASES get_docs FOR yif_aai_agent~get_docs.
    ALIASES get_model FOR yif_aai_agent~get_model.
    ALIASES get_prompt_template FOR yif_aai_agent~get_prompt_template.
    ALIASES initialize_tasks FOR yif_aai_agent~initialize_tasks.

    ALIASES m_agent_id FOR yif_aai_agent~m_agent_id.
    ALIASES m_chat_id FOR yif_aai_agent~m_chat_id.

    METHODS constructor
      IMPORTING
        i_agent_id   TYPE yaai_agent-id OPTIONAL
        i_chat_id    TYPE yaai_chat-id OPTIONAL
        i_autonomous TYPE abap_bool DEFAULT abap_false.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS ycl_aai_agent IMPLEMENTATION.


  METHOD constructor.

    me->m_agent_id = i_agent_id.
    me->m_chat_id = i_chat_id.

    IF i_autonomous = abap_true.
      me->initialize_tasks( ).
    ENDIF.

  ENDMETHOD.

  METHOD yif_aai_agent~run_autonomously.

    DATA: lo_aai_db TYPE REF TO ycl_aai_db,
          lo_api    TYPE REF TO object.

    DATA: l_task_id    TYPE yaai_async-id,
          l_task_name  TYPE yaai_async-name,
          l_classname  TYPE seoclsname,
          l_methodname TYPE seocpdname VALUE 'RUN' ##NO_TEXT.

    SELECT SINGLE id, name, start_message, hitl_message
      FROM yaai_agent
      WHERE id = @i_agent_id
      INTO @DATA(ls_agent).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    l_task_name = ls_agent-name.

    SELECT chat_id
      FROM yaai_agent_task
      WHERE id = @i_agent_id
        AND status <> 'C'
        INTO @DATA(l_chat_id)
        UP TO 1 ROWS.
    ENDSELECT.

    DATA(lo_aai_async) = NEW ycl_aai_async( ).

    IF l_chat_id IS INITIAL.
      lo_aai_db = NEW ycl_aai_db( i_api = i_api ).
      l_chat_id = lo_aai_db->m_id.
    ENDIF.

    " Create Async Task
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    l_task_id = lo_aai_async->create(
      EXPORTING
        i_chat_id   = l_chat_id
        i_task_name = l_task_name
    ).

    " Human-in-the-loop (HITL)
    IF ls_agent-hitl_message IS NOT INITIAL.

      UPDATE yaai_agent
        SET hitl_message = @space
        WHERE id = @i_agent_id.

    ENDIF.

    CASE i_api.

      WHEN yif_aai_const=>c_openai.

        l_classname = 'YCL_AAI_ASYNC_CHAT_OPENAI'.

      WHEN yif_aai_const=>c_anthropic.

        l_classname = 'YCL_AAI_ASYNC_CHAT_ANTHROPIC'.

      WHEN yif_aai_const=>c_google.

        l_classname = 'YCL_AAI_ASYNC_CHAT_GOOGLE'.

      WHEN yif_aai_const=>c_mistral.

        l_classname = 'YCL_AAI_ASYNC_CHAT_OPENAI'.

      WHEN yif_aai_const=>c_ollama.

        l_classname = 'YCL_AAI_ASYNC_CHAT_OLLAMA'.

    ENDCASE.

    IF l_classname IS NOT INITIAL.

      TRY.

          CREATE OBJECT lo_api TYPE (l_classname).

        CATCH cx_sy_create_object_error ##NO_HANDLER.
          RETURN.
      ENDTRY.

    ENDIF.

    IF lo_api IS BOUND.

      TRY.

          CALL METHOD lo_api->(l_methodname)
              EXPORTING
                i_task_id  = l_task_id
                i_chat_id  = l_chat_id
                i_message  = COND string( WHEN ls_agent-hitl_message IS NOT INITIAL
                                          THEN ls_agent-hitl_message
                                          ELSE ls_agent-start_message )
                i_agent_id = i_agent_id.

        CATCH cx_sy_dyn_call_illegal_class
              cx_sy_dyn_call_illegal_method
              cx_sy_dyn_call_illegal_type
              cx_sy_dyn_call_param_missing
              cx_sy_dyn_call_param_not_found
              cx_sy_dyn_call_excp_not_found
              cx_sy_ref_is_initial
              cx_sy_no_handler ##NO_HANDLER.

          RETURN.
      ENDTRY.

    ENDIF.

  ENDMETHOD.

  METHOD yif_aai_agent~get_system_instructions.

    DATA ls_agent TYPE yaai_agent.

    FREE r_system_instructions.

    IF i_agent_id IS SUPPLIED.
      me->m_agent_id = i_agent_id.
    ENDIF.

    IF me->m_agent_id IS NOT INITIAL.

      SELECT SINGLE id, name, sys_inst_id
        FROM yaai_agent
        WHERE id = @me->m_agent_id
        INTO CORRESPONDING FIELDS OF @ls_agent.

    ELSEIF i_agent_name IS SUPPLIED.

      SELECT id, name, sys_inst_id
        FROM yaai_agent
        WHERE name = @i_agent_name
        INTO CORRESPONDING FIELDS OF @ls_agent
        UP TO 1 ROWS.                                   "#EC CI_NOORDER
      ENDSELECT.

    ENDIF.

    IF ls_agent-sys_inst_id IS INITIAL.
      RETURN.
    ENDIF.

    NEW ycl_aai_rag_db( )->read(
      EXPORTING
        i_id      = ls_agent-sys_inst_id
      IMPORTING
        e_content = r_system_instructions
    ).

  ENDMETHOD.


  METHOD yif_aai_agent~get_tools.

    FREE r_t_agent_tools.

    IF i_agent_id IS SUPPLIED.
      me->m_agent_id = i_agent_id.
    ENDIF.

    IF i_chat_id IS SUPPLIED.
      me->m_chat_id = i_chat_id.
    ENDIF.

    IF me->m_agent_id IS NOT INITIAL.

      SELECT a~id, a~name, b~class_name, b~method_name, b~proxy_class, b~description
        FROM yaai_agent AS a
        INNER JOIN yaai_agent_tool AS b
        ON a~id = b~id
        WHERE a~id = @me->m_agent_id
          AND load_on_demand = @i_load_on_demand_tools
        INTO TABLE @DATA(lt_tools).

    ELSEIF i_agent_name IS SUPPLIED.

      SELECT a~id, a~name, b~class_name, b~method_name, b~proxy_class, b~description
        FROM yaai_agent AS a
        INNER JOIN yaai_agent_tool AS b
        ON a~id = b~id
        WHERE a~name = @i_agent_name
        AND load_on_demand = @i_load_on_demand_tools
        INTO TABLE @lt_tools.                           "#EC CI_NOORDER

      IF sy-subrc = 0.
        me->m_agent_id = lt_tools[ 1 ]-id.              "#EC CI_NOORDER
      ENDIF.

    ENDIF.

    IF i_load_on_demand_tools = abap_false AND
       me->m_chat_id IS NOT INITIAL AND
       me->m_agent_id IS NOT INITIAL.

      SELECT a~id, b~class_name, b~method_name, b~proxy_class, b~description
        FROM yaai_tools AS a
        INNER JOIN yaai_agent_tool AS b
        ON a~class_name = b~class_name
        AND a~method_name = b~method_name
        WHERE a~id = @me->m_chat_id
          AND b~id = @me->m_agent_id
        APPENDING CORRESPONDING FIELDS OF TABLE @lt_tools.

    ENDIF.

    r_t_agent_tools = CORRESPONDING #( lt_tools ).

  ENDMETHOD.


  METHOD yif_aai_agent~get_prompt_template.

    DATA ls_agent TYPE yaai_agent.

    FREE r_prompt_template.

    IF i_agent_id IS SUPPLIED.
      me->m_agent_id = i_agent_id.
    ENDIF.

    IF me->m_agent_id IS NOT INITIAL.

      SELECT SINGLE id, name, prompt_template
        FROM yaai_agent
        WHERE id = @me->m_agent_id
        INTO CORRESPONDING FIELDS OF @ls_agent.

    ELSEIF i_agent_name IS SUPPLIED.

      SELECT id, name, prompt_template
        FROM yaai_agent
        WHERE name = @i_agent_name
        INTO CORRESPONDING FIELDS OF @ls_agent
        UP TO 1 ROWS.                                   "#EC CI_NOORDER
      ENDSELECT.

    ENDIF.

    r_prompt_template = ls_agent-prompt_template.

  ENDMETHOD.

  METHOD yif_aai_agent~get_model.

    CLEAR r_s_model.

    IF i_agent_id IS SUPPLIED.
      me->m_agent_id = i_agent_id.
    ENDIF.

    IF me->m_agent_id IS NOT INITIAL.

      SELECT a~id, b~api, b~model, b~temperature, b~verbosity, b~reasoning, b~max_tool_calls
        FROM yaai_agent AS a
        INNER JOIN yaai_agent_mdl AS b
        ON a~id = b~id
        WHERE ( a~id = @me->m_agent_id OR
                a~name = @i_agent_name )
          AND b~api = @i_api
        INTO CORRESPONDING FIELDS OF @r_s_model
        UP TO 1 ROWS.
      ENDSELECT.

    ENDIF.

  ENDMETHOD.

  METHOD yif_aai_agent~get_docs.

    IF me->m_agent_id IS NOT INITIAL.

      SELECT a~id, b~rag_id, c~filename, c~description, c~keywords
        FROM yaai_agent AS a
        INNER JOIN yaai_agent_rag AS b
        ON a~id = b~id
        INNER JOIN yaai_rag AS c
        ON b~rag_id = c~id
        WHERE a~id = @me->m_agent_id
        INTO TABLE @DATA(lt_docs).

    ELSEIF i_agent_name IS SUPPLIED.

      SELECT a~id, b~rag_id, c~filename, c~description, c~keywords
        FROM yaai_agent AS a
        INNER JOIN yaai_agent_rag AS b
        ON a~id = b~id
        INNER JOIN yaai_rag AS c
        ON b~rag_id = c~id
        WHERE a~name = @i_agent_name
        INTO TABLE @lt_docs.                            "#EC CI_NOORDER

    ENDIF.

    r_t_agent_docs = CORRESPONDING #( lt_docs ).

  ENDMETHOD.

  METHOD yif_aai_agent~initialize_tasks.

    DATA lt_agent_task TYPE STANDARD TABLE OF yaai_agent_task.

    IF me->m_agent_id IS INITIAL OR me->m_chat_id IS INITIAL.
      RETURN.
    ENDIF.

    SELECT SINGLE task_flow_id
      FROM yaai_agent
      WHERE id = @me->m_agent_id
      INTO @DATA(l_task_flow_id).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    SELECT id, task_id, previous_task_id
      FROM yaai_task_flow
      WHERE id = @l_task_flow_id
      INTO TABLE @DATA(lt_task_flow).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    SELECT @abap_true
      FROM yaai_agent_task
      WHERE id = @me->m_agent_id
        AND status <> 'C'
        INTO @DATA(l_exists)
        UP TO 1 ROWS.
    ENDSELECT.

    IF sy-subrc = 0.
      " Already initialized
      RETURN.
    ENDIF.

    DELETE FROM yaai_agent_task
      WHERE id = @me->m_agent_id
        AND chat_id = @me->m_chat_id.

    LOOP AT lt_task_flow ASSIGNING FIELD-SYMBOL(<ls_task_flow>).

      APPEND INITIAL LINE TO lt_agent_task ASSIGNING FIELD-SYMBOL(<ls_agent_task>).

      <ls_agent_task>-id = me->m_agent_id.
      <ls_agent_task>-chat_id = me->m_chat_id.
      <ls_agent_task>-task_id = <ls_task_flow>-task_id.
      <ls_agent_task>-previous_task_id = <ls_task_flow>-previous_task_id.

    ENDLOOP.

    INSERT yaai_agent_task
      FROM TABLE @lt_agent_task
      ACCEPTING DUPLICATE KEYS.

  ENDMETHOD.

ENDCLASS.
