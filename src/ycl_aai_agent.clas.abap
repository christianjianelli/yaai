CLASS ycl_aai_agent DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES yif_aai_agent.

    ALIASES get_system_instructions FOR yif_aai_agent~get_system_instructions.
    ALIASES get_tools FOR yif_aai_agent~get_tools.
    ALIASES get_docs FOR yif_aai_agent~get_docs.
    ALIASES get_model FOR yif_aai_agent~get_model.
    ALIASES get_prompt_template FOR yif_aai_agent~get_prompt_template.
    ALIASES m_agent_id FOR yif_aai_agent~m_agent_id.
    ALIASES m_chat_id FOR yif_aai_agent~m_chat_id.

    METHODS constructor
      IMPORTING
        i_agent_id TYPE yaai_agent-id OPTIONAL
        i_chat_id  TYPE yaai_chat-id OPTIONAL.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS ycl_aai_agent IMPLEMENTATION.


  METHOD constructor.

    me->m_agent_id = i_agent_id.
    me->m_chat_id = i_chat_id.

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

ENDCLASS.
