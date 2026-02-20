CLASS ycl_aai_agent_db DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES yif_aai_agent_db.
    INTERFACES if_oo_adt_classrun.

    ALIASES create FOR yif_aai_agent_db~create.
    ALIASES read FOR yif_aai_agent_db~read.
    ALIASES update FOR yif_aai_agent_db~update.
    ALIASES delete FOR yif_aai_agent_db~delete.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_aai_agent_db IMPLEMENTATION.


  METHOD yif_aai_agent_db~create.

    CLEAR e_id.

    SELECT id FROM yaai_agent
      WHERE name = @i_s_agent-name
      INTO @DATA(l_id)
      UP TO 1 ROWS.
    ENDSELECT.

    IF sy-subrc = 0.
      e_error = |Agent { i_s_agent-name } already exists in the database|.
      RETURN.
    ENDIF.

    DATA(ls_agent) = i_s_agent.

    TRY.

        ls_agent-id = cl_system_uuid=>create_uuid_x16_static( ).

      CATCH cx_uuid_error ##NO_HANDLER.
    ENDTRY.

    INSERT yaai_agent FROM @ls_agent.

    IF sy-subrc <> 0.
      e_error = |Error while saving Agent { i_s_agent-name }|.
      RETURN.
    ENDIF.

    e_id = ls_agent-id.

    IF i_t_agent_tools IS SUPPLIED.

      DATA(lt_agent_tools) = i_t_agent_tools.

      LOOP AT lt_agent_tools ASSIGNING FIELD-SYMBOL(<ls_agent_tool>).
        <ls_agent_tool>-id = ls_agent-id.
      ENDLOOP.

      IF lt_agent_tools IS NOT INITIAL.

        SORT lt_agent_tools BY class_name method_name.

        DELETE ADJACENT DUPLICATES FROM lt_agent_tools COMPARING class_name method_name.

        INSERT yaai_agent_tool FROM TABLE @lt_agent_tools.

        IF sy-subrc <> 0.
          e_error = |Error while saving tools for Agent { i_s_agent-name }|.
          RETURN.
        ENDIF.

      ENDIF.

    ENDIF.

    IF i_t_agent_docs IS SUPPLIED.

      DATA(lt_agent_docs) = i_t_agent_docs.

      LOOP AT lt_agent_docs ASSIGNING FIELD-SYMBOL(<ls_agent_doc>).
        <ls_agent_doc>-id = ls_agent-id.
      ENDLOOP.

      IF lt_agent_docs IS NOT INITIAL.

        SORT lt_agent_docs BY rag_id.

        DELETE ADJACENT DUPLICATES FROM lt_agent_docs COMPARING rag_id.

        INSERT yaai_agent_rag FROM TABLE @lt_agent_docs.

        IF sy-subrc <> 0.
          e_error = |Error while saving docs for Agent { i_s_agent-name }|.
          RETURN.
        ENDIF.

      ENDIF.

    ENDIF.

    IF i_t_agent_models IS SUPPLIED.

      DATA(lt_agent_models) = i_t_agent_models.

      LOOP AT lt_agent_models ASSIGNING FIELD-SYMBOL(<ls_agent_model>).

        <ls_agent_model>-id = ls_agent-id.

        IF <ls_agent_model>-api <> yif_aai_const=>c_openai.

          CLEAR: <ls_agent_model>-verbosity, <ls_agent_model>-reasoning.

        ENDIF.

      ENDLOOP.

      IF lt_agent_models IS NOT INITIAL.

        SORT lt_agent_models BY api.

        DELETE ADJACENT DUPLICATES FROM lt_agent_models COMPARING api.

        INSERT yaai_agent_mdl FROM TABLE @lt_agent_models.

        IF sy-subrc <> 0.
          e_error = |Error while saving models for Agent { i_s_agent-name }|.
        ENDIF.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD yif_aai_agent_db~read.

    FREE: e_error,
          e_s_agent,
          e_t_agent_tools.

    SELECT FROM yaai_agent FIELDS id, name, description, sys_inst_id, rag_ctx_id
      WHERE id = @i_agent_id
         OR name = @i_agent_name
      INTO CORRESPONDING FIELDS OF @e_s_agent
      UP TO 1 ROWS.                                     "#EC CI_NOORDER
    ENDSELECT.

    IF sy-subrc <> 0.

      e_error = COND #( WHEN i_agent_id IS SUPPLIED
                        THEN |Agent { i_agent_id } not found in the database|
                        ELSE |Agent { i_agent_name } not found in the database| ).

      RETURN.

    ENDIF.

    SELECT FROM yaai_agent_tool FIELDS id, class_name, method_name, proxy_class, description
      WHERE id = @e_s_agent-id
      INTO CORRESPONDING FIELDS OF TABLE @e_t_agent_tools.

    SELECT id, rag_id
      FROM yaai_agent_rag
      WHERE id = @e_s_agent-id
       INTO CORRESPONDING FIELDS OF TABLE @e_t_agent_docs.

  ENDMETHOD.


  METHOD yif_aai_agent_db~update.

    e_updated = abap_false.

    SELECT SINGLE @abap_true
      FROM yaai_agent
      WHERE id = @i_s_agent-id
      INTO @DATA(l_exist).

    IF sy-subrc <> 0.

      e_error = |Agent { i_s_agent-id } not found in the database|.

      RETURN.

    ENDIF.

    UPDATE yaai_agent FROM @i_s_agent.

    IF sy-subrc <> 0.
      e_error = |Error while saving Agent { i_s_agent-name }|.
      RETURN.
    ENDIF.

    e_updated = abap_true.

    DELETE FROM yaai_agent_tool WHERE id = @i_s_agent-id.

    IF i_t_agent_tools IS SUPPLIED.

      DATA(lt_agent_tools) = i_t_agent_tools.

      LOOP AT lt_agent_tools ASSIGNING FIELD-SYMBOL(<ls_agent_tool>).
        <ls_agent_tool>-id = i_s_agent-id.
      ENDLOOP.

      IF lt_agent_tools IS NOT INITIAL.

        SORT lt_agent_tools BY class_name method_name.

        DELETE ADJACENT DUPLICATES FROM lt_agent_tools COMPARING class_name method_name.

        INSERT yaai_agent_tool FROM TABLE @lt_agent_tools ACCEPTING DUPLICATE KEYS.

        IF sy-subrc <> 0.
          e_error = |Error while saving tools for Agent { i_s_agent-name }|.
          RETURN.
        ENDIF.

      ENDIF.

    ENDIF.

    DELETE FROM yaai_agent_rag WHERE id = @i_s_agent-id.

    IF i_t_agent_docs IS SUPPLIED.

      DATA(lt_agent_docs) = i_t_agent_docs.

      LOOP AT lt_agent_docs ASSIGNING FIELD-SYMBOL(<ls_agent_doc>).
        <ls_agent_doc>-id = i_s_agent-id.
      ENDLOOP.

      IF lt_agent_docs IS NOT INITIAL.

        SORT lt_agent_docs BY rag_id.

        DELETE ADJACENT DUPLICATES FROM lt_agent_docs COMPARING rag_id.

        INSERT yaai_agent_rag FROM TABLE @lt_agent_docs ACCEPTING DUPLICATE KEYS.

        IF sy-subrc <> 0.
          e_error = |Error while saving docs for Agent { i_s_agent-name }|.
          RETURN.
        ENDIF.

      ENDIF.

    ENDIF.

    IF i_t_agent_models IS SUPPLIED.

      DELETE FROM yaai_agent_mdl WHERE id = @i_s_agent-id.

      DATA(lt_agent_models) = i_t_agent_models.

      LOOP AT lt_agent_models ASSIGNING FIELD-SYMBOL(<ls_agent_model>).
        <ls_agent_model>-id = i_s_agent-id.

        IF <ls_agent_model>-api <> yif_aai_const=>c_openai.
          CLEAR: <ls_agent_model>-reasoning,
                 <ls_agent_model>-verbosity.
        ENDIF.

      ENDLOOP.

      IF lt_agent_models IS NOT INITIAL.

        SORT lt_agent_models BY api.

        DELETE ADJACENT DUPLICATES FROM lt_agent_models COMPARING api.

        INSERT yaai_agent_mdl FROM TABLE @lt_agent_models.

        IF sy-subrc <> 0.
          e_error = |Error while saving models for Agent { i_s_agent-name }|.
        ENDIF.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD yif_aai_agent_db~delete.

    e_deleted = abap_false.

    DELETE FROM yaai_agent_tool WHERE id = @i_agent_id.

    DELETE FROM yaai_agent WHERE id = @i_agent_id.

    IF sy-subrc <> 0.
      e_error = |Error while deleting Agent { i_agent_id }|.
    ENDIF.

    e_deleted = abap_true.

  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.

*    DELETE FROM yaai_agent.
*    DELETE FROM yaai_agent_tool.

*    DATA(ls_agent) = VALUE yaai_agent( name = 'travel-fiori-ai-assistant'
*                                        description = 'Travel Fiori App AI Assistant'
*                                        sys_inst_id = '2A9448FCE52F1FD0AD850377D753B845'
*                                        rag_ctx_id = '7EA3422BA1AC1FE0AD8503D109246C64'
*                                        prompt_template = '**User message**: %USER_MESSAGE% \n\n**Context**:\n\n %CONTEXT% \n\n' ).
*
*    DATA(lt_agent_tools) = VALUE yif_aai_agent_db=>ty_agent_tools_t( ( class_name = 'ycl_aai_rag_tools'
*                                                                        method_name = 'get_documentation'
*                                                                        description = 'Use this method to retrieve the complete documentation of the Travel Fiori App' ) ).
*
*    me->create(
*      EXPORTING
*        i_s_agent       = ls_agent
*        i_t_agent_tools = lt_agent_tools
*      IMPORTING
*        e_id            = DATA(l_id)
*        e_error         = DATA(l_error)
*    ).
*
*    out->write( l_id ).
*    out->write( l_error ).

  ENDMETHOD.
ENDCLASS.
