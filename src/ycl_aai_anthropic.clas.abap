CLASS ycl_aai_anthropic DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES yif_aai_anthropic.
    INTERFACES yif_aai_chat.

    ALIASES on_message_send FOR yif_aai_chat~on_message_send.
    ALIASES on_response_received FOR yif_aai_chat~on_response_received.
    ALIASES on_message_failed FOR yif_aai_chat~on_message_failed.

    ALIASES set_model FOR yif_aai_anthropic~set_model.
    ALIASES set_temperature FOR yif_aai_anthropic~set_temperature.
    ALIASES set_system_instructions FOR yif_aai_anthropic~set_system_instructions.
    ALIASES set_connection FOR yif_aai_anthropic~set_connection.
    ALIASES bind_tools FOR yif_aai_anthropic~bind_tools.
    ALIASES chat FOR yif_aai_anthropic~chat.
    ALIASES get_conversation FOR yif_aai_anthropic~get_conversation.

    ALIASES mo_function_calling FOR yif_aai_anthropic~mo_function_calling.
    ALIASES m_anthropic_version FOR yif_aai_anthropic~m_anthropic_version.

    CLASS-DATA m_ref TYPE REF TO ycl_aai_anthropic READ-ONLY.

    CLASS-METHODS get_instance
      IMPORTING
                i_model      TYPE csequence OPTIONAL
      RETURNING VALUE(r_ref) TYPE REF TO ycl_aai_anthropic.

    METHODS constructor
      IMPORTING
        i_model        TYPE csequence OPTIONAL
        i_max_tokens   TYPE i OPTIONAL
        i_o_connection TYPE REF TO yif_aai_conn OPTIONAL.


  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA: _o_connection TYPE REF TO yif_aai_conn.

    DATA: _model               TYPE string,
          _temperature         TYPE p LENGTH 2 DECIMALS 1,
          _max_tokens          TYPE i VALUE 2048,
          _system_instructions TYPE string,
          _chat_messages       TYPE yif_aai_anthropic~ty_chat_messages_t,
          _max_tools_calls     TYPE i.

ENDCLASS.



CLASS ycl_aai_anthropic IMPLEMENTATION.

  METHOD get_instance.

    IF m_ref IS NOT BOUND.
      m_ref = NEW #( ).
    ENDIF.

    IF i_model IS SUPPLIED.
      m_ref->set_model( i_model ).
    ENDIF.

    r_ref = m_ref.

  ENDMETHOD.

  METHOD constructor.

    me->m_anthropic_version = '2023-06-01'.

    me->_model = 'claude-3-7-sonnet-latest'.

    IF me->_model IS NOT INITIAL.
      me->_model = i_model.
    ENDIF.

    me->_max_tokens = '2048'.

    IF i_max_tokens IS NOT INITIAL.
      me->_max_tokens = i_max_tokens.
    ENDIF.

    me->_temperature = 1.

    me->_max_tools_calls = 5.

    IF i_o_connection IS SUPPLIED.
      me->_o_connection = i_o_connection.
    ENDIF.

  ENDMETHOD.

  METHOD yif_aai_anthropic~set_version.

    me->m_anthropic_version = i_version.

  ENDMETHOD.

  METHOD yif_aai_anthropic~set_model.

    me->_model = i_model.

  ENDMETHOD.

  METHOD yif_aai_anthropic~set_max_tokens.

    me->_max_tokens = i_max_tokens.

  ENDMETHOD.

  METHOD yif_aai_anthropic~set_system_instructions.

    me->_system_instructions = i_system_instructions.

  ENDMETHOD.

  METHOD yif_aai_anthropic~set_temperature.

    me->_temperature = i_temperature.

  ENDMETHOD.

  METHOD yif_aai_anthropic~set_connection.

    me->_o_connection = i_o_connection.

  ENDMETHOD.

  METHOD yif_aai_anthropic~bind_tools.

    me->mo_function_calling = i_o_function_calling.

    IF i_max_tools_calls IS SUPPLIED.
      me->_max_tools_calls = i_max_tools_calls.
    ENDIF.

  ENDMETHOD.

  METHOD yif_aai_anthropic~chat.

    FIELD-SYMBOLS <l_data> TYPE string.

    DATA lr_data TYPE REF TO data.

    DATA lt_response_content TYPE yif_aai_anthropic~ty_content_t.

    DATA ls_anthropic_chat_response TYPE yif_aai_anthropic~ty_anthropic_chat_response_s.

    DATA: l_message TYPE string,
          l_tools   TYPE string VALUE '[]'.

    CLEAR: e_response,
           e_failed.

    FREE e_t_response.

    IF me->_model IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lo_aai_util) = NEW ycl_aai_util( ).

    IF i_new = abap_true.

      FREE me->_chat_messages.

    ENDIF.

    IF me->_chat_messages IS INITIAL.

      IF i_greeting IS NOT INITIAL.

        APPEND VALUE #( role = 'assistant' content = lo_aai_util->serialize( i_data = i_greeting ) ) TO me->_chat_messages.

      ENDIF.

    ENDIF.

    APPEND VALUE #( role = 'user' content = lo_aai_util->serialize( i_data = i_message ) ) TO me->_chat_messages.

    IF me->mo_function_calling IS BOUND.

      me->mo_function_calling->get_tools(
        IMPORTING
          e_tools = l_tools
      ).

    ENDIF.

    IF me->_o_connection IS NOT BOUND.

      me->_o_connection = NEW ycl_aai_conn( i_api = yif_aai_const=>c_anthropic ).

    ENDIF.

    me->_o_connection->add_http_header_param(
        EXPORTING
          i_name  = 'anthropic-version'
          i_value = me->m_anthropic_version
      ).

    me->_o_connection->add_http_header_param(
      EXPORTING
        i_name  = 'x-api-key'
        i_value = |{ yif_aai_const=>c_placeholder_pattern }APIKEY{ yif_aai_const=>c_placeholder_pattern }|
    ).

    DO me->_max_tools_calls TIMES.

      IF me->_o_connection->create_connection( i_endpoint = yif_aai_const=>c_anthropic_messages_endpoint ).

        FREE ls_anthropic_chat_response.

        DATA(l_json) = lo_aai_util->serialize( i_data = VALUE yif_aai_anthropic~ty_anthropic_chat_request_s( model = me->_model
                                                                                                             temperature = me->_temperature
                                                                                                             max_tokens = me->_max_tokens
                                                                                                             system = me->_system_instructions
                                                                                                             messages = me->_chat_messages
                                                                                                             tools = l_tools ) ).

        me->_o_connection->set_body( l_json ).

        FREE l_json.

        me->_o_connection->do_receive(
          IMPORTING
            e_response = l_json
            e_failed   = e_failed
        ).

        IF e_failed = abap_true.

          me->_o_connection->get_error_text(
            IMPORTING
              e_error_text = e_response
          ).

          IF e_t_response IS REQUESTED.
            APPEND INITIAL LINE TO e_t_response ASSIGNING FIELD-SYMBOL(<l_response>).
            <l_response> = e_response.
          ENDIF.

          EXIT.

        ENDIF.

        lo_aai_util->deserialize(
          EXPORTING
            i_json = l_json
          IMPORTING
            e_data = ls_anthropic_chat_response
        ).

        IF ls_anthropic_chat_response-type = 'error'.

          e_response = ls_anthropic_chat_response-error-message.

          IF e_t_response IS REQUESTED.
            APPEND INITIAL LINE TO e_t_response ASSIGNING <l_response>.
            <l_response> = e_response.
          ENDIF.

          EXIT.

        ENDIF.

        APPEND VALUE #( role = ls_anthropic_chat_response-role
                        content = ls_anthropic_chat_response-content ) TO me->_chat_messages.

        lo_aai_util->deserialize(
          EXPORTING
            i_json = ls_anthropic_chat_response-content
          IMPORTING
            e_data = lt_response_content
        ).

        IF ls_anthropic_chat_response-stop_reason = 'tool_use' AND me->mo_function_calling IS BOUND.

          LOOP AT lt_response_content ASSIGNING FIELD-SYMBOL(<ls_content>).

            IF <ls_content>-type <> 'tool_use'.
              CONTINUE.
            ENDIF.

            me->mo_function_calling->call_tool(
              EXPORTING
                i_tool_name   = to_upper( <ls_content>-name )
                i_json        = <ls_content>-input
              RECEIVING
                r_response    = DATA(l_tool_response)
            ).

            l_tool_response = lo_aai_util->serialize(
              EXPORTING
                i_data = l_tool_response
            ).

            l_tool_response = '[{"type": "tool_result", "tool_use_id": "' && <ls_content>-id && '","content": ' && l_tool_response && '}]'.

            APPEND VALUE #( role = 'user' content = l_tool_response ) TO me->_chat_messages.

          ENDLOOP.

          CONTINUE.

        ENDIF.

        LOOP AT lt_response_content ASSIGNING <ls_content>.

          CASE <ls_content>-type.

            WHEN 'text'.

              e_response = |{ e_response } { <ls_content>-text }|.

          ENDCASE.

        ENDLOOP.

        EXIT.

      ELSE.

        me->_o_connection->get_error_text(
          IMPORTING
            e_error_text = e_response
        ).

        IF e_t_response IS REQUESTED.
          APPEND INITIAL LINE TO e_t_response ASSIGNING <l_response>.
          <l_response> = e_response.
        ENDIF.

        EXIT.

      ENDIF.

    ENDDO.

    IF e_t_response IS REQUESTED AND ls_anthropic_chat_response-type <> 'error'.

      SPLIT e_response AT cl_abap_char_utilities=>newline INTO TABLE e_t_response.

    ENDIF.

  ENDMETHOD.

  METHOD yif_aai_anthropic~get_conversation.

    rt_messages = me->_chat_messages.

  ENDMETHOD.

  METHOD yif_aai_anthropic~get_history.

    e_t_history = me->_chat_messages.

  ENDMETHOD.

  METHOD yif_aai_anthropic~set_history.

    me->_chat_messages = i_t_history.

  ENDMETHOD.

  METHOD yif_aai_chat~chat.

    me->chat(
      EXPORTING
        i_message    = i_message
        i_new        = i_new
        i_greeting   = i_greeting
      IMPORTING
        e_response   = e_response
        e_failed     = e_failed
        e_t_response = e_t_response
    ).

  ENDMETHOD.

ENDCLASS.
