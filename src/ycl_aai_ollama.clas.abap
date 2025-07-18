CLASS ycl_aai_ollama DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES yif_aai_ollama.
    INTERFACES yif_aai_chat.

    ALIASES on_message_send FOR yif_aai_chat~on_message_send.
    ALIASES on_response_received FOR yif_aai_chat~on_response_received.
    ALIASES on_message_failed FOR yif_aai_chat~on_message_failed.

    ALIASES set_model FOR yif_aai_ollama~set_model.
    ALIASES set_temperature FOR yif_aai_ollama~set_temperature.
    ALIASES set_system_instructions FOR yif_aai_ollama~set_system_instructions.
    ALIASES set_connection FOR yif_aai_ollama~set_connection.
    ALIASES bind_tools FOR yif_aai_ollama~bind_tools.
    ALIASES chat FOR yif_aai_ollama~chat.
    ALIASES generate FOR yif_aai_ollama~generate.
    ALIASES embed FOR yif_aai_ollama~embed.
    ALIASES get_chat_messages FOR yif_aai_ollama~get_chat_messages.

    ALIASES mo_function_calling FOR yif_aai_ollama~mo_function_calling.

    CLASS-DATA m_ref TYPE REF TO ycl_aai_ollama READ-ONLY.

    CLASS-METHODS get_instance
      IMPORTING
                i_model      TYPE csequence OPTIONAL
      RETURNING VALUE(r_ref) TYPE REF TO ycl_aai_ollama.

    METHODS constructor
      IMPORTING
        i_model        TYPE csequence OPTIONAL
        i_o_connection TYPE REF TO yif_aai_conn OPTIONAL.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA: _o_connection TYPE REF TO yif_aai_conn.

    DATA: _model                    TYPE string,
          _temperature              TYPE p LENGTH 2 DECIMALS 1,
          _system_instructions      TYPE string,
          _ollama_chat_response     TYPE yif_aai_ollama~ty_ollama_chat_response_s,
          _ollama_generate_response TYPE yif_aai_ollama~ty_ollama_generate_response_s,
          _chat_messages            TYPE yif_aai_ollama~ty_chat_messages_t,
          _max_tools_calls          TYPE i.

ENDCLASS.



CLASS ycl_aai_ollama IMPLEMENTATION.

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

    me->_model = i_model.

    me->_temperature = 1.

    me->_max_tools_calls = 5.

    IF i_o_connection IS SUPPLIED.
      me->_o_connection = i_o_connection.
    ENDIF.

  ENDMETHOD.

  METHOD yif_aai_ollama~set_model.

    me->_model = i_model.

  ENDMETHOD.

  METHOD yif_aai_ollama~set_temperature.

    me->_temperature = i_temperature.

  ENDMETHOD.

  METHOD yif_aai_ollama~set_system_instructions.

    me->_system_instructions = i_system_instructions.

  ENDMETHOD.

  METHOD yif_aai_ollama~set_history.

    me->_chat_messages = i_t_history.

  ENDMETHOD.

  METHOD yif_aai_ollama~get_history.

    e_t_history = me->_chat_messages.

  ENDMETHOD.

  METHOD yif_aai_ollama~set_connection.

    me->_o_connection = i_o_connection.

  ENDMETHOD.

  METHOD yif_aai_ollama~bind_tools.

    me->mo_function_calling = i_o_function_calling.

    IF i_max_tools_calls IS SUPPLIED.
      me->_max_tools_calls = i_max_tools_calls.
    ENDIF.

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

  METHOD yif_aai_ollama~chat.

    FIELD-SYMBOLS <l_data> TYPE string.

    DATA lr_data TYPE REF TO data.

    DATA l_tools TYPE string VALUE '[]'.

    CLEAR: e_response,
           e_failed.

    FREE e_t_response.

    IF me->_model IS INITIAL.
      RETURN.
    ENDIF.

    IF i_new = abap_true.

      FREE me->_chat_messages.

    ENDIF.

    IF me->_chat_messages IS INITIAL.

      IF me->_system_instructions IS NOT INITIAL.

        APPEND VALUE #( role = 'system' content = me->_system_instructions ) TO me->_chat_messages.

      ENDIF.

      IF i_greeting IS NOT INITIAL.

        APPEND VALUE #( role = 'assistant' content = i_greeting ) TO me->_chat_messages.

      ENDIF.

    ELSE.

      IF me->_system_instructions IS NOT INITIAL.

        READ TABLE me->_chat_messages TRANSPORTING NO FIELDS
          WITH KEY role = 'system'.

        IF sy-subrc <> 0.

          INSERT VALUE #( role = 'system' content = me->_system_instructions ) INTO me->_chat_messages INDEX 1.

        ENDIF.

      ENDIF.

    ENDIF.

    APPEND VALUE #( role = 'user' content = i_message ) TO me->_chat_messages.

    DATA(lo_aai_util) = NEW ycl_aai_util( ).

    IF me->mo_function_calling IS BOUND.

      me->mo_function_calling->get_tools(
        IMPORTING
          e_tools = l_tools
      ).

    ENDIF.

    DO me->_max_tools_calls TIMES.

      IF me->_o_connection IS NOT BOUND.
        me->_o_connection = NEW ycl_aai_conn( i_api = yif_aai_const=>c_ollama ).
      ENDIF.

      IF me->_o_connection->create_connection( i_endpoint = yif_aai_const=>c_ollama_chat_endpoint ).

        FREE me->_ollama_chat_response.

        DATA(l_json) = lo_aai_util->serialize( i_data = VALUE yif_aai_ollama~ty_ollama_chat_request_s( model = me->_model
                                                                                                       options = VALUE #( temperature = me->_temperature )
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
            e_data = me->_ollama_chat_response
        ).

        IF me->_ollama_chat_response-message-tool_calls[] IS NOT INITIAL AND me->mo_function_calling IS BOUND.

          APPEND me->_ollama_chat_response-message TO me->_chat_messages.

          LOOP AT me->_ollama_chat_response-message-tool_calls ASSIGNING FIELD-SYMBOL(<ls_tool>).

            " For some reason, probably a bug in Ollama API, part of the arguments in the JSON are passed as escaped strings.
            " Because just part of the JSON has to be unescaped it is not possible to use the proper deserialize method.
            " So the workaround is to unescape the JSON using the replaces below.
            " Hopefully the Ollama API bug will be fixed and this workaround can be removed.
            REPLACE ALL OCCURRENCES OF '\"' IN <ls_tool>-function-arguments WITH '"'.
            REPLACE ALL OCCURRENCES OF '"[' IN <ls_tool>-function-arguments WITH '['.
            REPLACE ALL OCCURRENCES OF ']"' IN <ls_tool>-function-arguments WITH ']'.

            me->mo_function_calling->call_tool(
              EXPORTING
                i_tool_name   = to_upper( <ls_tool>-function-name )
                i_json        = <ls_tool>-function-arguments
              RECEIVING
                r_response    = DATA(l_tool_response)
            ).

            APPEND VALUE #( role = 'tool' content = l_tool_response ) TO me->_chat_messages.

          ENDLOOP.

          CONTINUE.

        ENDIF.

        IF me->_ollama_chat_response-error IS NOT INITIAL.

          e_response = me->_ollama_chat_response-error.

          IF e_t_response IS REQUESTED.
            APPEND INITIAL LINE TO e_t_response ASSIGNING <l_response>.
            <l_response> = e_response.
          ENDIF.

          EXIT.

        ENDIF.

        me->_ollama_chat_response-message-content = lo_aai_util->replace_unicode_escape_seq( me->_ollama_chat_response-message-content ).

        APPEND me->_ollama_chat_response-message TO me->_chat_messages.

        e_response = me->_ollama_chat_response-message-content.

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

    IF e_t_response IS REQUESTED AND me->_ollama_chat_response-error IS INITIAL.

      SPLIT e_response AT cl_abap_char_utilities=>newline INTO TABLE e_t_response.

    ENDIF.

  ENDMETHOD.

  METHOD yif_aai_ollama~generate.

    CLEAR: e_response,
           e_failed.

    FREE e_t_response.

    IF me->_model IS INITIAL.
      RETURN.
    ENDIF.

    IF me->_o_connection IS NOT BOUND.
      me->_o_connection = NEW ycl_aai_conn( i_api = yif_aai_const=>c_ollama ).
    ENDIF.

    IF me->_o_connection->create_connection( i_endpoint = yif_aai_const=>c_ollama_generate_endpoint ).

      DATA(lo_aai_util) = NEW ycl_aai_util( ).

      DATA(l_json) = lo_aai_util->serialize( i_data = VALUE yif_aai_ollama~ty_ollama_generate_request_s( model = me->_model
                                                                                                         prompt = i_message
                                                                                                         stream = abap_false ) ).

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

        RETURN.

      ELSE.

        lo_aai_util->deserialize(
          EXPORTING
            i_json = l_json
          IMPORTING
            e_data = me->_ollama_generate_response
        ).

        me->_ollama_generate_response-response = lo_aai_util->replace_unicode_escape_seq( me->_ollama_generate_response-response ).

        e_response = me->_ollama_generate_response-response.

      ENDIF.

    ELSE.

      me->_o_connection->get_error_text(
        IMPORTING
          e_error_text = e_response
      ).

      IF e_t_response IS REQUESTED.
        APPEND INITIAL LINE TO e_t_response ASSIGNING <l_response>.
        <l_response> = e_response.
      ENDIF.

      RETURN.

    ENDIF.

    IF e_t_response IS REQUESTED.

      SPLIT e_response AT cl_abap_char_utilities=>newline INTO TABLE e_t_response.

    ENDIF.

  ENDMETHOD.

  METHOD yif_aai_ollama~embed.

    CLEAR: e_s_response,
           e_failed.

    IF me->_o_connection IS NOT BOUND.
      me->_o_connection = NEW ycl_aai_conn( i_api = yif_aai_const=>c_ollama ).
    ENDIF.

    IF me->_o_connection->create_connection( i_endpoint = yif_aai_const=>c_ollama_embed_endpoint ).

      DATA(lo_aai_util) = NEW ycl_aai_util( ).

      DATA(l_json) = lo_aai_util->serialize( i_data = VALUE yif_aai_ollama~ty_ollama_embed_request_s( model = me->_model
                                                                                                      input = i_input ) ).

      me->_o_connection->set_body( l_json ).

      FREE l_json.

      me->_o_connection->do_receive(
        IMPORTING
          e_response = l_json
          e_failed   = e_failed
      ).

      IF e_failed = abap_true.

        RETURN.

      ELSE.

        lo_aai_util->deserialize(
          EXPORTING
            i_json = l_json
          IMPORTING
            e_data = e_s_response
        ).

      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD get_chat_messages.

    rt_messages = me->_chat_messages.

  ENDMETHOD.

ENDCLASS.
