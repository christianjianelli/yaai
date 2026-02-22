CLASS ycl_aai_log DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES yif_aai_log.

    ALIASES add FOR yif_aai_log~add_message.
    ALIASES add_messages FOR yif_aai_log~add_messages.
    ALIASES get_messages FOR yif_aai_log~get_messages.
    ALIASES get_log FOR yif_aai_log~get_log.
    ALIASES save_log FOR yif_aai_log~save_log.
    ALIASES load_log FOR yif_aai_log~load_log.
    ALIASES delete_expired_logs FOR yif_aai_log~delete_expired_logs.
    ALIASES mt_msg FOR yif_aai_log~mt_messages.
    ALIASES mt_log FOR yif_aai_log~mt_log.
    ALIASES m_chat_id FOR yif_aai_log~m_chat_id.

    METHODS constructor
      IMPORTING
        i_chat_id TYPE yde_aai_chat_id OPTIONAL.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.


CLASS ycl_aai_log IMPLEMENTATION.

   METHOD constructor.

    IF i_chat_id IS SUPPLIED.
      me->m_chat_id = i_chat_id.
    ENDIF.

  ENDMETHOD.


  METHOD yif_aai_log~add_message.

    DATA(ls_msg) = i_s_msg.

    IF ls_msg-id IS INITIAL.
      ls_msg-id = yif_aai_const=>c_message_id.
    ENDIF.

    IF ls_msg-type IS INITIAL.
      ls_msg-type = 'E'.
    ENDIF.

    APPEND ls_msg TO me->mt_msg.

    IF i_save = abap_true.
      me->save_log( ).
    ENDIF.

  ENDMETHOD.


  METHOD yif_aai_log~add_messages.

    LOOP AT i_t_msg ASSIGNING FIELD-SYMBOL(<ls_msg>).

      me->add( i_s_msg = <ls_msg> ).

    ENDLOOP.

    IF i_save = abap_true.
      me->save_log( ).
    ENDIF.

  ENDMETHOD.


  METHOD yif_aai_log~save_log.

    IF i_chat_id IS SUPPLIED.
      me->m_chat_id = i_chat_id.
    ENDIF.

    SELECT MAX( seqno ) FROM yaai_log
      WHERE id = @me->m_chat_id
      INTO @DATA(l_seqno).

    DATA(l_expiry_date) = sy-datlo + 7.

    IF i_expiry_date IS SUPPLIED.
      l_expiry_date = i_expiry_date.
    ENDIF.

    LOOP AT me->mt_msg ASSIGNING FIELD-SYMBOL(<ls_msg>) WHERE row IS INITIAL.

      MESSAGE ID <ls_msg>-id
        TYPE <ls_msg>-type
        NUMBER <ls_msg>-number
        WITH <ls_msg>-message_v1
             <ls_msg>-message_v2
             <ls_msg>-message_v3
             <ls_msg>-message_v4
        INTO DATA(l_message).

      l_seqno = l_seqno + 1.

      <ls_msg>-row = l_seqno.

      INSERT yaai_log FROM @( VALUE yaai_log( id = me->m_chat_id
                                                seqno = l_seqno
                                                message = l_message
                                                username = cl_abap_context_info=>get_user_technical_name( )
                                                log_date = sy-datlo
                                                log_time = sy-timlo
                                                expiry_date = l_expiry_date
                                                msgid = <ls_msg>-id
                                                msgno = <ls_msg>-number
                                                msgty = <ls_msg>-type
                                                msgv1 = <ls_msg>-message_v1
                                                msgv2 = <ls_msg>-message_v2
                                                msgv3 = <ls_msg>-message_v3
                                                msgv4 = <ls_msg>-message_v4 ) ).

    ENDLOOP.

  ENDMETHOD.


  METHOD yif_aai_log~load_log.

    IF i_chat_id IS NOT SUPPLIED AND me->m_chat_id IS INITIAL.
      RETURN.
    ENDIF.

    IF i_chat_id IS SUPPLIED.
      me->m_chat_id = i_chat_id.
    ENDIF.

    IF me->m_chat_id IS INITIAL.
      RETURN.
    ENDIF.

    SELECT id, seqno, username, log_date, log_time, expiry_date,
           msgid, msgno, msgv1, msgv2, msgv3, msgv4
      FROM yaai_log
      WHERE id = @me->m_chat_id
      INTO CORRESPONDING FIELDS OF TABLE @me->mt_log.

  ENDMETHOD.


  METHOD yif_aai_log~get_log.

    rt_log = me->mt_log.

  ENDMETHOD.


  METHOD yif_aai_log~get_messages.

    IF me->mt_log IS NOT INITIAL.

      FREE me->mt_msg.

      LOOP AT me->mt_log ASSIGNING FIELD-SYMBOL(<ls_log>).

        APPEND VALUE #( id = <ls_log>-msgid
                        number = <ls_log>-msgno
                        type = <ls_log>-msgty
                        message_v1 = <ls_log>-msgv1
                        message_v2 = <ls_log>-msgv2
                        message_v3 = <ls_log>-msgv3
                        message_v4 = <ls_log>-msgv4 ) TO me->mt_msg.

      ENDLOOP.

    ENDIF.

    rt_msg = me->mt_msg.

  ENDMETHOD.


  METHOD yif_aai_log~delete_expired_logs.

    DELETE FROM yaai_log WHERE expiry_date < @sy-datum.

  ENDMETHOD.

ENDCLASS.
