CLASS ycl_aai_async_chat_ollama DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES yif_aai_async_chat_run.

    ALIASES run FOR yif_aai_async_chat_run~run.

    METHODS on_http_request_send FOR EVENT on_request_send OF ycl_aai_conn.

    METHODS on_http_response_received FOR EVENT on_response_received OF ycl_aai_conn.

    METHODS on_connection_error FOR EVENT on_connection_error OF ycl_aai_conn.

    METHODS on_message_send FOR EVENT on_message_send OF ycl_aai_ollama.

    METHODS on_response_received FOR EVENT on_response_received OF ycl_aai_ollama.

    METHODS on_message_failed FOR EVENT on_message_failed OF ycl_aai_ollama
      IMPORTING
        error_text.

    METHODS on_chat_is_blocked FOR EVENT on_chat_is_blocked OF ycl_aai_ollama.

    METHODS on_tool_call FOR EVENT on_tool_call OF yif_aai_func_call_ollama
      IMPORTING
        class_name
        method_name.

    METHODS on_tool_call_response FOR EVENT on_tool_call_response OF yif_aai_func_call_ollama
      IMPORTING
        tool_response.

    METHODS on_tool_call_error FOR EVENT on_tool_call_error OF yif_aai_func_call_ollama
      IMPORTING
        error_text.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA: _chat_id TYPE yde_aai_chat_id,
          _log     TYPE abap_bool.

ENDCLASS.



CLASS ycl_aai_async_chat_ollama IMPLEMENTATION.

  METHOD yif_aai_async_chat_run~run.

    DATA lo_agent TYPE REF TO yif_aai_agent.

    DATA l_response TYPE string.

    me->_chat_id = i_chat_id.

    me->_log = i_log.

    DATA(lo_aai_conn) = NEW ycl_aai_conn( i_api = yif_aai_const=>c_ollama ).

    lo_aai_conn->set_api_key( i_api_key = i_api_key ).

    DATA(lo_aai_db) = NEW ycl_aai_db( i_api = yif_aai_const=>c_ollama
                                        i_id = i_chat_id ).

    DATA(lo_log) = NEW ycl_aai_log( i_chat_id = i_chat_id ).

    IF i_agent_id IS NOT INITIAL.

      lo_agent = NEW ycl_aai_agent(
        i_agent_id = i_agent_id
        i_chat_id  = lo_aai_db->m_id
      ).

    ENDIF.

    " Connection events handlers
    SET HANDLER me->on_http_request_send FOR ALL INSTANCES.
    SET HANDLER me->on_http_response_received FOR ALL INSTANCES.
    SET HANDLER me->on_connection_error FOR ALL INSTANCES.

    " Chat events handlers
    SET HANDLER me->on_message_send FOR ALL INSTANCES.
    SET HANDLER me->on_response_received FOR ALL INSTANCES.
    SET HANDLER me->on_message_failed FOR ALL INSTANCES.

    " Function Calling events handlers
    SET HANDLER me->on_tool_call FOR ALL INSTANCES.
    SET HANDLER me->on_tool_call_response FOR ALL INSTANCES.
    SET HANDLER me->on_tool_call_error FOR ALL INSTANCES.

    DATA(lo_aai_ollama) = NEW ycl_aai_ollama( i_model = i_model
                                              i_o_connection = lo_aai_conn
                                              i_o_persistence = lo_aai_db
                                              i_o_agent = lo_agent ).

    IF i_context IS INITIAL.

      lo_log->add( VALUE #( number = '003' type = 'S' ) ).

      lo_aai_ollama->chat(
        EXPORTING
          i_message       = i_message
          i_async_task_id = CONV string( i_task_id )
          i_o_agent       = lo_agent
        IMPORTING
          e_response      = l_response
      ).

      lo_log->add( VALUE #( number = '004' type = 'S' ) ).

    ELSE.

      " Default template
      DATA(l_prompt_template) = |**User message**: %USER_MESSAGE% \n\n**Context**:\n\n %CONTEXT% \n\n|.

      IF i_agent_id IS NOT INITIAL.

        l_prompt_template = lo_agent->get_prompt_template( i_agent_id ).

      ENDIF.

      DATA(lo_aai_prompt_template) = NEW ycl_aai_prompt_template(
        i_template_text = l_prompt_template
      ).

      DATA(lo_aai_prompt) = NEW ycl_aai_prompt(
        i_o_prompt_template = lo_aai_prompt_template
        i_s_params = VALUE yif_aai_prompt=>ty_params_basic_s( user_message = i_message
                                                               context = i_context )
      ).

      lo_log->add( VALUE #( number = '003' type = 'S' ) ).

      lo_aai_ollama->chat(
        EXPORTING
          i_o_prompt = lo_aai_prompt
          i_o_agent  = lo_agent
        IMPORTING
          e_response = l_response
      ).

      lo_log->add( VALUE #( number = '004' type = 'S' ) ).

    ENDIF.

    DATA(lo_async) = NEW ycl_aai_async( ).

    lo_async->update_status(
      EXPORTING
        i_task_id = i_task_id
        i_status  = yif_aai_async=>mc_task_finished
    ).

  ENDMETHOD.

  METHOD on_message_send.

    IF me->_log = abap_true.

      DATA(lo_log) = NEW ycl_aai_log( me->_chat_id ).

      lo_log->add( VALUE #( number = '005' type = 'S' ) ).

    ENDIF.

  ENDMETHOD.

  METHOD on_response_received.

    IF me->_log = abap_true.

      DATA(lo_log) = NEW ycl_aai_log( me->_chat_id ).

      lo_log->add( VALUE #( number = '006' type = 'S' ) ).

    ENDIF.

  ENDMETHOD.

  METHOD on_message_failed.

    IF me->_log = abap_true.

      DATA(lo_log) = NEW ycl_aai_log( me->_chat_id ).

      DATA(ls_msg) = VALUE bapiret2( number = '007' type = 'E' message_v1 = error_text ).

      IF strlen( error_text ) > 50.

        NEW ycl_aai_util( )->split_string(
          EXPORTING
            i_string            = error_text
            i_length            = 50
          IMPORTING
            e_t_splitted_string = DATA(lt_splitted_string)
        ).

        LOOP AT lt_splitted_string ASSIGNING FIELD-SYMBOL(<ls_line>).

          CASE sy-tabix.
            WHEN 1.
              ls_msg-message_v1 = <ls_line>.
            WHEN 2.
              ls_msg-message_v2 = <ls_line>.
            WHEN 3.
              ls_msg-message_v3 = <ls_line>.
            WHEN 4.
              ls_msg-message_v4 = <ls_line>.
          ENDCASE.

        ENDLOOP.

      ENDIF.

      lo_log->add( ls_msg ).

    ENDIF.

  ENDMETHOD.

  METHOD on_chat_is_blocked.

    IF me->_log = abap_true.

      DATA(lo_log) = NEW ycl_aai_log( me->_chat_id ).

      lo_log->add( VALUE #( number = '015' type = 'E' message_v1 = me->_chat_id ) ).

    ENDIF.

  ENDMETHOD.

  METHOD on_tool_call.

    IF me->_log = abap_true.

      DATA(lo_log) = NEW ycl_aai_log( me->_chat_id ).

      lo_log->add( VALUE #( number = '008'
                                type = 'S'
                                message_v1 = class_name
                                message_v2 = method_name ) ).

    ENDIF.

  ENDMETHOD.

  METHOD on_tool_call_response.

    IF me->_log = abap_true.

      DATA(lo_log) = NEW ycl_aai_log( me->_chat_id ).

      DATA(ls_msg) = VALUE bapiret2( number = '009' type = 'S' message_v1 = tool_response ).

      IF strlen( tool_response ) > 50.

        NEW ycl_aai_util( )->split_string(
          EXPORTING
            i_string            = tool_response
            i_length            = 50
          IMPORTING
            e_t_splitted_string = DATA(lt_splitted_string)
        ).

        LOOP AT lt_splitted_string ASSIGNING FIELD-SYMBOL(<ls_line>).

          CASE sy-tabix.
            WHEN 1.
              ls_msg-message_v1 = <ls_line>.
            WHEN 2.
              ls_msg-message_v2 = <ls_line>.
            WHEN 3.
              ls_msg-message_v3 = <ls_line>.
            WHEN 4.
              ls_msg-message_v4 = <ls_line>.
          ENDCASE.

        ENDLOOP.

      ENDIF.

      lo_log->add( ls_msg ).

    ENDIF.

  ENDMETHOD.

  METHOD on_tool_call_error.

    IF me->_log = abap_true.

      DATA(lo_log) = NEW ycl_aai_log( me->_chat_id ).

      DATA(ls_msg) = VALUE bapiret2( number = '010' type = 'E' message_v1 = error_text ).

      IF strlen( error_text ) > 50.

        NEW ycl_aai_util( )->split_string(
          EXPORTING
            i_string            = error_text
            i_length            = 50
          IMPORTING
            e_t_splitted_string = DATA(lt_splitted_string)
        ).

        LOOP AT lt_splitted_string ASSIGNING FIELD-SYMBOL(<ls_line>).

          CASE sy-tabix.
            WHEN 1.
              ls_msg-message_v1 = <ls_line>.
            WHEN 2.
              ls_msg-message_v2 = <ls_line>.
            WHEN 3.
              ls_msg-message_v3 = <ls_line>.
            WHEN 4.
              ls_msg-message_v4 = <ls_line>.
          ENDCASE.

        ENDLOOP.

      ENDIF.

      lo_log->add( ls_msg ).

    ENDIF.

  ENDMETHOD.

  METHOD on_connection_error.

    IF me->_log = abap_true.

      DATA(lo_log) = NEW ycl_aai_log( me->_chat_id ).

      lo_log->add( VALUE #( number = '001' type = 'E' ) ).

    ENDIF.

  ENDMETHOD.

  METHOD on_http_request_send.

    DATA(lo_log) = NEW ycl_aai_log( me->_chat_id ).

    lo_log->add( VALUE #( number = '011' type = 'S' ) ).

  ENDMETHOD.

  METHOD on_http_response_received.

    IF me->_log = abap_true.

      DATA(lo_log) = NEW ycl_aai_log( me->_chat_id ).

      lo_log->add( VALUE #( number = '012' type = 'S' ) ).

    ENDIF.

  ENDMETHOD.

ENDCLASS.
