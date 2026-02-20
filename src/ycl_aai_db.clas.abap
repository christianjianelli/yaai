
CLASS ycl_aai_db DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES yif_aai_db.

    ALIASES create_id FOR yif_aai_db~create_id.
    ALIASES delete_chat FOR yif_aai_db~delete_chat.
    ALIASES persist_chat FOR yif_aai_db~persist_chat.
    ALIASES persist_message FOR yif_aai_db~persist_message.
    ALIASES persist_system_instructions FOR yif_aai_db~persist_system_instructions.
    ALIASES persist_tools FOR yif_aai_db~persist_tools.
    ALIASES get_chat FOR yif_aai_db~get_chat.
    ALIASES block_chat FOR yif_aai_db~block_chat.
    ALIASES release_chat FOR yif_aai_db~release_chat.
    ALIASES is_chat_blocked FOR yif_aai_db~is_chat_blocked.

    ALIASES mt_messages FOR yif_aai_db~mt_messages.
    ALIASES mt_tools FOR yif_aai_db~mt_tools.

    DATA: m_api  TYPE string READ-ONLY,
          m_id   TYPE uuid READ-ONLY,
          m_user TYPE string.


    METHODS constructor
      IMPORTING
        i_api     TYPE csequence
        i_id      TYPE uuid OPTIONAL
        i_preload TYPE abap_bool DEFAULT abap_false.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_aai_db IMPLEMENTATION.


  METHOD constructor.

    me->m_api = i_api.

    me->m_user = cl_abap_context_info=>get_user_technical_name( ).

    IF i_id IS NOT INITIAL.

      me->m_id = i_id.

      IF i_preload = abap_true.

        me->get_chat(
          EXPORTING
            i_id         = me->m_id
          IMPORTING
            e_t_messages = me->mt_messages
            e_t_tools    = me->mt_tools
        ).

      ENDIF.

    ENDIF.

    IF me->m_id IS INITIAL.

      me->persist_chat(
        IMPORTING
          e_id = me->m_id
      ).

    ENDIF.

  ENDMETHOD.


  METHOD yif_aai_db~create_id.

    CLEAR r_id.

    TRY.

        r_id = cl_system_uuid=>create_uuid_x16_static( ).

      CATCH cx_uuid_error ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD yif_aai_db~delete_chat.

    DELETE FROM yaai_chat WHERE id = @me->m_id.

    e_deleted = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).

  ENDMETHOD.


  METHOD yif_aai_db~get_chat.

    DATA l_id TYPE uuid.

    FREE: e_t_messages,
          e_t_msg_data,
          e_t_tools.

    IF i_id IS SUPPLIED.
      l_id = i_id.
    ENDIF.

    IF l_id IS INITIAL.
      l_id = me->m_id.
    ENDIF.

    SELECT SINGLE id
      FROM yaai_chat
      WHERE id = @l_id
        AND api = @me->m_api
      INTO @l_id.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    SELECT id, seqno, msg, msg_date, msg_time
      FROM yaai_msg
      WHERE id = @l_id
      ORDER BY PRIMARY KEY
      INTO CORRESPONDING FIELDS OF TABLE @e_t_messages.

    IF i_ui = abap_false.

      SELECT id, seqno, prompt
        FROM yaai_prompt
        WHERE id = @l_id
        ORDER BY PRIMARY KEY
        INTO TABLE @DATA(lt_prompt).

      LOOP AT lt_prompt ASSIGNING FIELD-SYMBOL(<ls_prompt>).

        READ TABLE e_t_messages ASSIGNING FIELD-SYMBOL(<ls_message>)
          WITH KEY id = <ls_prompt>-id
                   seqno = <ls_prompt>-seqno
          BINARY SEARCH.

        IF sy-subrc = 0.

          <ls_message>-msg = <ls_prompt>-prompt.

        ENDIF.

      ENDLOOP.

    ENDIF.

    SELECT id, class_name, method_name, proxy_class, description
      FROM yaai_tools
      WHERE id = @i_id
      INTO CORRESPONDING FIELDS OF TABLE @e_t_tools.

    IF e_t_msg_data IS REQUESTED.

      DATA(lo_aai_util) = NEW ycl_aai_util( ).

      LOOP AT e_t_messages ASSIGNING FIELD-SYMBOL(<l_msg>).

        APPEND INITIAL LINE TO e_t_msg_data ASSIGNING FIELD-SYMBOL(<ls_msg>).

        lo_aai_util->deserialize(
          EXPORTING
            i_json = <l_msg>-msg
          IMPORTING
            e_data = <ls_msg>
        ).

      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD yif_aai_db~persist_chat.

    DATA l_id TYPE uuid.

    CLEAR: e_id,
           e_persisted.

    IF i_id IS SUPPLIED.
      l_id = i_id.
    ENDIF.

    IF l_id IS INITIAL.
      l_id = me->create_id( ).
    ENDIF.

    DATA(ls_chat) = VALUE yaai_chat( id = l_id
                                     api = me->m_api
                                     username = me->m_user
                                     chat_date = sy-datlo
                                     chat_time = sy-timlo ).

    INSERT yaai_chat FROM @ls_chat.

    e_persisted = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).

    e_id = l_id.

  ENDMETHOD.


  METHOD yif_aai_db~persist_message.

    DATA: l_id     TYPE yde_aai_chat_id,
          l_seqno  TYPE yde_aai_seqno,
          l_tokens TYPE yde_aai_tokens,
          l_model  TYPE yde_aai_model.

    CLEAR: e_id,
           e_persisted.

    IF i_id IS SUPPLIED.

      l_id = i_id.

    ENDIF.

    IF l_id IS INITIAL.

      l_id = me->m_id.

    ENDIF.

    IF l_id IS INITIAL.

      me->persist_chat(
        IMPORTING
          e_id = me->m_id
      ).

      l_id = me->m_id.

      l_seqno = 1.

    ENDIF.

    e_id = l_id.

    IF l_seqno = 0.

      SELECT MAX( seqno )
        FROM yaai_msg
        WHERE id = @l_id
        INTO @l_seqno.

      l_seqno = l_seqno + 1.

    ENDIF.

    IF i_tokens IS SUPPLIED.
      l_tokens = i_tokens.
    ENDIF.

    IF i_model IS SUPPLIED.
      l_model = i_model.
    ENDIF.

    IF i_message IS SUPPLIED.

      DATA(ls_msg) = VALUE yaai_msg( id = l_id
                                     seqno = l_seqno
                                     msg = i_message
                                     tokens = l_tokens
                                     model = l_model ).

    ENDIF.

    IF i_data IS SUPPLIED.

      DATA(lo_aai_util) = NEW ycl_aai_util( ).

      ls_msg = VALUE yaai_msg( id = l_id
                               seqno = l_seqno
                               msg = lo_aai_util->serialize( i_data )
                               tokens = l_tokens
                               model = l_model ).

    ENDIF.

    IF i_prompt IS SUPPLIED AND i_prompt IS NOT INITIAL.

      DATA(ls_prompt) = VALUE yaai_prompt( id = l_id
                                           seqno = l_seqno
                                           prompt = lo_aai_util->serialize( i_prompt ) ).

    ENDIF.

    IF ls_msg IS NOT INITIAL.

      ls_msg-msg_date = sy-datlo.
      ls_msg-msg_time = sy-timlo.
      ls_msg-async_task_id = i_async_task_id.

      INSERT yaai_msg FROM @ls_msg.

      e_persisted = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).

      IF ls_prompt IS NOT INITIAL.

        INSERT yaai_prompt FROM @ls_prompt.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD yif_aai_db~persist_system_instructions.

    DATA: l_id    TYPE uuid,
          l_seqno TYPE i.

    CLEAR: e_id,
           e_persisted.

    IF i_id IS SUPPLIED.

      l_id = i_id.

    ENDIF.

    IF l_id IS INITIAL.

      l_id = me->m_id.

    ENDIF.

    IF l_id IS INITIAL.

      me->persist_chat(
        IMPORTING
          e_id = me->m_id
      ).

      l_id = me->m_id.

    ENDIF.

    e_id = l_id.

    l_seqno = 0.

    SELECT SINGLE @abap_true
      FROM yaai_msg
      WHERE id = @l_id
        AND seqno = @l_seqno
        INTO @DATA(l_exists).

    IF sy-subrc = 0.
      e_persisted = abap_true.
    ENDIF.

    IF i_system_instructions IS SUPPLIED.

      DATA(ls_msg) = VALUE yaai_msg( id = l_id
                                     seqno = l_seqno
                                     msg = i_system_instructions ).

    ENDIF.

    IF i_data IS SUPPLIED.

      DATA(lo_aai_util) = NEW ycl_aai_util( ).

      ls_msg = VALUE yaai_msg( id = l_id
                               seqno = l_seqno
                               msg = lo_aai_util->serialize( i_data ) ).

    ENDIF.

    IF ls_msg IS NOT INITIAL.

      ls_msg-msg_date = sy-datlo.
      ls_msg-msg_time = sy-timlo.

      INSERT yaai_msg FROM @ls_msg.

      e_persisted = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).

    ENDIF.

  ENDMETHOD.


  METHOD yif_aai_db~persist_tools.

    e_persisted = abap_true.

    IF i_t_tools[] IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lt_tools) = i_t_tools.

    LOOP AT lt_tools ASSIGNING FIELD-SYMBOL(<ls_tool>).

      <ls_tool>-id = me->m_id.

    ENDLOOP.

    INSERT yaai_tools FROM TABLE @lt_tools ACCEPTING DUPLICATE KEYS.

  ENDMETHOD.

  METHOD yif_aai_db~block_chat.

    e_blocked = abap_false.

    IF me->m_id IS INITIAL.
      RETURN.
    ENDIF.

    UPDATE yaai_chat
      SET blocked = @abap_true
      WHERE id = @me->m_id.

    e_blocked = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).

  ENDMETHOD.

  METHOD yif_aai_db~release_chat.

    e_released = abap_false.

    IF me->m_id IS INITIAL.
      RETURN.
    ENDIF.

    UPDATE yaai_chat
      SET blocked = @abap_false
      WHERE id = @me->m_id.

    e_released = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).

  ENDMETHOD.

  METHOD is_chat_blocked.

    SELECT SINGLE blocked
      FROM yaai_chat
      WHERE id = @me->m_id
      INTO @r_blocked.

  ENDMETHOD.

ENDCLASS.
