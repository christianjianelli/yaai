CLASS ycl_aai_openai DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES yif_aai_openai.
    INTERFACES yif_aai_chat.

    ALIASES set_model FOR yif_aai_openai~set_model.
    ALIASES use_completions FOR yif_aai_openai~use_completions.
    ALIASES set_temperature FOR yif_aai_openai~set_temperature.
    ALIASES set_system_instructions FOR yif_aai_openai~set_system_instructions.
    ALIASES set_connection FOR yif_aai_openai~set_connection.
    ALIASES bind_tools FOR yif_aai_openai~bind_tools.
    ALIASES generate FOR yif_aai_openai~generate.
    ALIASES chat_completions FOR yif_aai_openai~chat_completions.
    ALIASES embed FOR yif_aai_openai~embed.
    ALIASES chat FOR yif_aai_chat~chat.
    ALIASES set_history FOR yif_aai_openai~set_history.
    ALIASES get_conversation FOR yif_aai_openai~get_conversation.
    ALIASES get_conversation_chat_comp FOR yif_aai_openai~get_conversation_chat_comp.

    ALIASES mo_function_calling FOR yif_aai_openai~mo_function_calling.

    CLASS-DATA m_ref TYPE REF TO ycl_aai_openai.

    CLASS-METHODS get_instance
      IMPORTING
                i_model      TYPE csequence OPTIONAL
      RETURNING VALUE(r_ref) TYPE REF TO ycl_aai_openai.

    METHODS constructor
      IMPORTING
        i_model           TYPE csequence OPTIONAL
        i_use_completions TYPE abap_bool DEFAULT abap_false
        i_t_history       TYPE yif_aai_openai~ty_generate_messages_t OPTIONAL
        i_o_connection    TYPE REF TO yif_aai_conn OPTIONAL.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA: _o_connection TYPE REF TO yif_aai_conn.

    DATA: _model                     TYPE string,
          _use_completions           TYPE abap_bool VALUE abap_false,
          _temperature               TYPE p LENGTH 2 DECIMALS 1,
          _system_instructions       TYPE string,
          _openai_generate_request   TYPE yif_aai_openai~ty_openai_generate_request_s,
          _openai_generate_response  TYPE yif_aai_openai~ty_openai_generate_response_s,
          _openai_chat_comp_response TYPE yif_aai_openai~ty_openai_chat_comp_resp_s,
          _messages                  TYPE yif_aai_openai~ty_generate_messages_t,
          _max_tools_calls           TYPE i.

ENDCLASS.



CLASS ycl_aai_openai IMPLEMENTATION.

  METHOD constructor.

    me->_model = COND #( WHEN i_model IS NOT INITIAL THEN i_model ELSE 'gpt-4.1' ).

    me->_messages = i_t_history.

    me->_temperature = 1.

    me->_max_tools_calls = 5.

    IF i_o_connection IS SUPPLIED.
      me->_o_connection = i_o_connection.
    ENDIF.

  ENDMETHOD.

  METHOD get_instance.

    IF m_ref IS NOT BOUND.
      m_ref = NEW #( ).
    ENDIF.

    IF i_model IS SUPPLIED.
      m_ref->set_model( i_model ).
    ENDIF.

    r_ref = m_ref.

  ENDMETHOD.

  METHOD yif_aai_openai~use_completions.

    me->_use_completions = i_use_completions.

  ENDMETHOD.

  METHOD yif_aai_openai~set_model.

    me->_model = i_model.

  ENDMETHOD.

  METHOD yif_aai_openai~set_temperature.

    me->_temperature = i_temperature.

  ENDMETHOD.

  METHOD yif_aai_openai~set_system_instructions.

    me->_system_instructions = i_system_instructions.

  ENDMETHOD.

  METHOD yif_aai_openai~set_connection.

    me->_o_connection = i_o_connection.

  ENDMETHOD.

  METHOD yif_aai_openai~bind_tools.

    me->mo_function_calling = i_o_function_calling.

    IF i_max_tools_calls IS SUPPLIED.
      me->_max_tools_calls = i_max_tools_calls.
    ENDIF.

  ENDMETHOD.

  METHOD yif_aai_chat~chat.

    IF me->_use_completions = abap_false.

      me->generate(
        EXPORTING
          i_message    = i_message
          i_new        = i_new
          i_greeting   = i_greeting
        IMPORTING
          e_response   = e_response
          e_t_response = e_t_response
      ).

    ELSE.

      me->chat_completions(
        EXPORTING
          i_message    = i_message
          i_new        = i_new
          i_greeting   = i_greeting
        IMPORTING
          e_response   = e_response
          e_t_response = e_t_response
      ).

    ENDIF.

  ENDMETHOD.

  METHOD yif_aai_openai~generate.

    FIELD-SYMBOLS <l_data> TYPE string.

    DATA lr_data TYPE REF TO data.

    DATA l_tools TYPE string VALUE '[]'.

    CLEAR e_response.

    FREE e_t_response.

    IF me->_model IS INITIAL.
      RETURN.
    ENDIF.

    IF i_new = abap_true.
      FREE me->_messages.
    ENDIF.

    IF me->_messages IS INITIAL.

      IF me->_system_instructions IS NOT INITIAL.

        APPEND VALUE #( role = 'developer'
                        content = me->_system_instructions
                        type = 'message' ) TO me->_messages.

      ENDIF.

      IF i_greeting IS NOT INITIAL.

        APPEND VALUE #( role = 'assistant'
                        content = i_greeting
                        type = 'message' ) TO me->_messages.

      ENDIF.

    ELSE.

      IF me->_system_instructions IS NOT INITIAL.

        READ TABLE me->_messages TRANSPORTING NO FIELDS
          WITH KEY role = 'developer'.

        IF sy-subrc <> 0.

          INSERT VALUE #( role = 'developer'
                          content = me->_system_instructions
                          type = 'message' ) INTO me->_messages INDEX 1.

        ENDIF.

      ENDIF.

    ENDIF.

    APPEND VALUE #( role = 'user'
                    content = i_message
                    type = 'message' ) TO me->_messages.

    IF me->mo_function_calling IS BOUND.

      me->mo_function_calling->get_tools(
        IMPORTING
          e_tools = l_tools
      ).

    ENDIF.

    IF me->_o_connection IS NOT BOUND.
      me->_o_connection = NEW ycl_aai_conn( i_api = yif_aai_const=>c_openai ).
    ENDIF.

    DATA(lo_aai_util) = NEW ycl_aai_util( ).

    DO me->_max_tools_calls TIMES.

      IF me->_o_connection->create_connection( i_endpoint = yif_aai_const=>c_openai_generate_endpoint ).

        FREE me->_openai_generate_response.

        DATA(l_json) = lo_aai_util->serialize( i_data = VALUE yif_aai_openai~ty_openai_generate_request_s( model = me->_model
                                                                                                           stream = abap_false
                                                                                                           input = me->get_conversation( )
                                                                                                           tools = l_tools ) ).

        me->_o_connection->set_body( l_json ).

        FREE l_json.

        me->_o_connection->do_receive(
          IMPORTING
            e_response = l_json
        ).

        lo_aai_util->deserialize(
          EXPORTING
            i_json = l_json
          IMPORTING
            e_data = me->_openai_generate_response
        ).

        DATA(l_function_call) = abap_false.

        LOOP AT _openai_generate_response-output ASSIGNING FIELD-SYMBOL(<ls_output>).

          IF <ls_output>-type <> 'function_call'.
            CONTINUE.
          ENDIF.

          l_function_call = abap_true.

          APPEND VALUE #( type = 'function_call'
                          arguments = <ls_output>-arguments
                          call_id = <ls_output>-call_id
                          name = <ls_output>-name ) TO me->_messages.

          " This deserialization is necessary because we need to parse the string passed in the arguments to a JSON.
          " Example: parse this "{\"latitude\":48.8566,\"longitude\":2.3522}" to a JSON like {"latitude": 48.8566, "longitude": 2.3522}
          lo_aai_util->deserialize(
            EXPORTING
              i_json = <ls_output>-arguments
            IMPORTING
              e_data = lr_data
          ).

          ASSIGN lr_data->* TO <l_data>.

          me->mo_function_calling->call_tool(
            EXPORTING
              i_tool_name   = to_upper( <ls_output>-name )
              i_json        = <l_data>
            RECEIVING
              r_response    = DATA(l_tool_response)
          ).

          APPEND VALUE #( type = 'function_call_output'
                          call_id = <ls_output>-call_id
                          output = l_tool_response ) TO me->_messages.

        ENDLOOP.

        IF l_function_call = abap_true.
          CONTINUE.
        ENDIF.

        LOOP AT _openai_generate_response-output ASSIGNING <ls_output>.

          IF <ls_output>-type <> 'message' OR <ls_output>-role <> 'assistant'.
            CONTINUE.
          ENDIF.

          LOOP AT <ls_output>-content ASSIGNING FIELD-SYMBOL(<ls_content>).

            IF <ls_content>-type <> 'output_text'.
              CONTINUE.
            ENDIF.

            <ls_content>-text = lo_aai_util->replace_unicode_escape_seq( <ls_content>-text ).

            APPEND VALUE #( role = <ls_output>-role
                            content = <ls_content>-text
                            type = <ls_output>-type ) TO me->_messages.

            e_response = e_response && <ls_content>-text.

          ENDLOOP.

        ENDLOOP.

        EXIT.

      ENDIF.

    ENDDO.

    IF e_t_response IS REQUESTED.

      SPLIT e_response AT cl_abap_char_utilities=>newline INTO TABLE e_t_response.

    ENDIF.

  ENDMETHOD.

  METHOD yif_aai_openai~chat_completions.

    FIELD-SYMBOLS <l_data> TYPE string.

    DATA lr_data TYPE REF TO data.

    DATA: l_json  TYPE string,
          l_tools TYPE string VALUE '[]'.

    CLEAR e_response.

    FREE e_t_response.

    IF me->_model IS INITIAL.
      RETURN.
    ENDIF.

    IF i_new = abap_true.
      FREE me->_messages.
    ENDIF.

    IF me->_messages IS INITIAL.

      IF me->_system_instructions IS NOT INITIAL.

        APPEND VALUE #( role = 'developer'
                        content = me->_system_instructions
                        type = 'message' ) TO me->_messages.

      ENDIF.

      IF i_greeting IS NOT INITIAL.

        APPEND VALUE #( role = 'assistant'
                        content = i_greeting
                        type = 'message' ) TO me->_messages.

      ENDIF.

    ELSE.

      IF me->_system_instructions IS NOT INITIAL.

        READ TABLE me->_messages TRANSPORTING NO FIELDS
          WITH KEY role = 'developer'.

        IF sy-subrc <> 0.

          INSERT VALUE #( role = 'developer'
                          content = me->_system_instructions
                          type = 'message' ) INTO me->_messages INDEX 1.

        ENDIF.

      ENDIF.

    ENDIF.

    APPEND VALUE #( role = 'user'
                    content = i_message
                    type = 'message' ) TO me->_messages.

    IF me->mo_function_calling IS BOUND.

      me->mo_function_calling->get_tools_chat_completions(
        IMPORTING
          e_tools = l_tools
      ).

    ENDIF.

    IF me->_o_connection IS NOT BOUND.
      me->_o_connection = NEW ycl_aai_conn( i_api = yif_aai_const=>c_openai ).
    ENDIF.

    DATA(lo_aai_util) = NEW ycl_aai_util( ).

    DO me->_max_tools_calls TIMES.

      IF me->_o_connection->create_connection( i_endpoint = yif_aai_const=>c_openai_completions_endpoint ).

        FREE me->_openai_chat_comp_response.

        IF l_tools = '[]'.

          l_json = lo_aai_util->serialize( i_data = VALUE yif_aai_openai~ty_openai_completions_req_s( model = me->_model
                                                                                                      stream = abap_false
                                                                                                      messages = me->get_conversation_chat_comp( ) ) ).

        ELSE.

          l_json = lo_aai_util->serialize( i_data = VALUE yif_aai_openai~ty_openai_comp_tools_req_s( model = me->_model
                                                                                                     stream = abap_false
                                                                                                     messages = me->get_conversation_chat_comp( )
                                                                                                     tools = l_tools ) ).

        ENDIF.

        me->_o_connection->set_body( l_json ).

        FREE l_json.

        me->_o_connection->do_receive(
          IMPORTING
            e_response = l_json
        ).

        lo_aai_util->deserialize(
          EXPORTING
            i_json = l_json
          IMPORTING
            e_data = me->_openai_chat_comp_response
        ).

        DATA(l_function_call) = abap_false.

        LOOP AT me->_openai_chat_comp_response-choices ASSIGNING FIELD-SYMBOL(<ls_choices>).

          IF <ls_choices>-message-tool_calls IS INITIAL.
            CONTINUE.
          ENDIF.

          l_function_call = abap_true.

          LOOP AT <ls_choices>-message-tool_calls ASSIGNING FIELD-SYMBOL(<ls_tool_calls>).

            APPEND VALUE #( role = <ls_choices>-message-role
                            type = 'function_call'
                            arguments = <ls_tool_calls>-function-arguments
                            call_id = <ls_tool_calls>-id
                            name = <ls_tool_calls>-function-name ) TO me->_messages.

*            " This deserialization is necessary because we need to parse the string passed in the arguments to a JSON.
*            " Example: parse this "{\"latitude\":48.8566,\"longitude\":2.3522}" to a JSON like {"latitude": 48.8566, "longitude": 2.3522}
*            lo_aai_util->deserialize(
*              EXPORTING
*                i_json = <ls_tool_calls>-function-arguments
*              IMPORTING
*                e_data = lr_data
*            ).
*
*            ASSIGN lr_data->* TO <l_data>.

            me->mo_function_calling->call_tool(
              EXPORTING
                i_tool_name   = to_upper( <ls_tool_calls>-function-name )
*                i_json        = <l_data>
                i_json        = <ls_tool_calls>-function-arguments
              RECEIVING
                r_response    = DATA(l_tool_response)
            ).

            APPEND VALUE #( role = 'tool'
                            type = 'function_call_output'
                            call_id = <ls_tool_calls>-id
                            output = l_tool_response ) TO me->_messages.

          ENDLOOP.

        ENDLOOP.

        IF l_function_call = abap_true.
          CONTINUE.
        ENDIF.

        LOOP AT me->_openai_chat_comp_response-choices ASSIGNING <ls_choices>.

          IF <ls_choices>-message-role <> 'assistant'.
            CONTINUE.
          ENDIF.

          e_response = lo_aai_util->replace_unicode_escape_seq( <ls_choices>-message-content ).

          APPEND VALUE #( role = <ls_choices>-message-role
                          content = e_response ) TO me->_messages.

        ENDLOOP.

        EXIT.

      ENDIF.

    ENDDO.

    IF e_t_response IS REQUESTED.

      SPLIT e_response AT cl_abap_char_utilities=>newline INTO TABLE e_t_response.

    ENDIF.

  ENDMETHOD.

  METHOD yif_aai_openai~set_history.

    me->_messages = i_t_history.

  ENDMETHOD.

  METHOD yif_aai_openai~get_history.

    e_t_history = me->_messages.

  ENDMETHOD.

  METHOD yif_aai_openai~get_conversation.

    DATA l_json TYPE string.

    CLEAR r_conversation.

    DATA(lo_aai_util) = NEW ycl_aai_util( ).

    LOOP AT me->_messages ASSIGNING FIELD-SYMBOL(<ls_message>).

      CLEAR l_json.

      CASE to_lower( <ls_message>-type ).

        WHEN 'message'.

          DATA(ls_message) = CORRESPONDING yif_aai_openai~ty_type_message_s( <ls_message> ).

          l_json = lo_aai_util->serialize( ls_message ).

        WHEN 'function_call'.

          DATA(ls_function_call) = CORRESPONDING yif_aai_openai~ty_function_call_s( <ls_message> ).

          l_json = lo_aai_util->serialize( ls_function_call ).

        WHEN 'function_call_output'.

          DATA(ls_function_call_output) = CORRESPONDING yif_aai_openai~ty_function_call_output_s( <ls_message> ).

          l_json = lo_aai_util->serialize( ls_function_call_output ).

      ENDCASE.

      IF r_conversation IS INITIAL.
        r_conversation = l_json.
      ELSE.
        r_conversation = |{ r_conversation }, { l_json }|.
      ENDIF.

    ENDLOOP.

    r_conversation = |[{ r_conversation }]|.

  ENDMETHOD.

  METHOD yif_aai_openai~get_conversation_chat_comp.

    DATA l_json TYPE string.

    CLEAR r_conversation.

    DATA(lo_aai_util) = NEW ycl_aai_util( ).

    LOOP AT me->_messages ASSIGNING FIELD-SYMBOL(<ls_message>).

      CLEAR l_json.

      CASE to_lower( <ls_message>-type ).

        WHEN 'message'.

          DATA(ls_message) = CORRESPONDING yif_aai_openai~ty_type_message_chat_comp_nt_s( <ls_message> ).

          l_json = lo_aai_util->serialize( ls_message ).

        WHEN 'function_call'.

          DATA(ls_function_call) = CORRESPONDING yif_aai_openai~ty_type_message_chat_comp_tc_s( <ls_message> ).

          ls_function_call-tool_calls = VALUE #( ( id = <ls_message>-call_id
                                                   type = 'function'
                                                   function = VALUE #( name = <ls_message>-name
                                                                       arguments = <ls_message>-arguments ) ) ).

          l_json = lo_aai_util->serialize( ls_function_call ).

        WHEN 'function_call_output'.

          DATA(ls_function_call_output) = CORRESPONDING yif_aai_openai~ty_type_message_chat_comp_tr_s( <ls_message> ).

          ls_function_call_output-content = <ls_message>-output.
          ls_function_call_output-tool_call_id = <ls_message>-call_id.

          l_json = lo_aai_util->serialize( ls_function_call_output ).

      ENDCASE.

      IF r_conversation IS INITIAL.
        r_conversation = l_json.
      ELSE.
        r_conversation = |{ r_conversation }, { l_json }|.
      ENDIF.

    ENDLOOP.

    r_conversation = |[{ r_conversation }]|.

  ENDMETHOD.

  METHOD yif_aai_openai~embed.

    IF me->_o_connection IS NOT BOUND.
      me->_o_connection = NEW ycl_aai_conn( i_api = yif_aai_const=>c_openai ).
    ENDIF.

    IF me->_o_connection->create_connection( i_endpoint = yif_aai_const=>c_openai_embed_endpoint ).

      DATA(lo_aai_util) = NEW ycl_aai_util( ).

      DATA(l_json) = lo_aai_util->serialize( i_data = VALUE yif_aai_openai~ty_openai_embed_request_s( model = me->_model
                                                                                                      input = i_input ) ).

      me->_o_connection->set_body( l_json ).

      FREE l_json.

      me->_o_connection->do_receive(
        IMPORTING
          e_response = l_json
      ).

      lo_aai_util->deserialize(
        EXPORTING
          i_json = l_json
        IMPORTING
          e_data = e_s_response
      ).

    ENDIF.

  ENDMETHOD.

ENDCLASS.
