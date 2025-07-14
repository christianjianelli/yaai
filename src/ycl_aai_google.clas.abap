CLASS ycl_aai_google DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES yif_aai_google.
    INTERFACES yif_aai_chat.

    ALIASES on_message_send FOR yif_aai_chat~on_message_send.
    ALIASES on_response_received FOR yif_aai_chat~on_response_received.
    ALIASES on_message_failed FOR yif_aai_chat~on_message_failed.

    ALIASES set_model FOR yif_aai_google~set_model.
    ALIASES set_temperature FOR yif_aai_google~set_temperature.
    ALIASES set_system_instructions FOR yif_aai_google~set_system_instructions.
    ALIASES set_connection FOR yif_aai_google~set_connection.
    ALIASES bind_tools FOR yif_aai_google~bind_tools.
    ALIASES chat FOR yif_aai_chat~chat.
    ALIASES generate FOR yif_aai_google~generate.
    ALIASES get_conversation FOR yif_aai_google~get_conversation.

    ALIASES mo_function_calling FOR yif_aai_google~mo_function_calling.

    CLASS-METHODS get_instance
      IMPORTING
                i_model      TYPE csequence OPTIONAL
      RETURNING VALUE(r_ref) TYPE REF TO ycl_aai_google.

    CLASS-DATA m_ref TYPE REF TO ycl_aai_google READ-ONLY.

    METHODS constructor
      IMPORTING
        i_model        TYPE csequence OPTIONAL
        i_o_connection TYPE REF TO yif_aai_conn OPTIONAL.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA: _o_connection TYPE REF TO yif_aai_conn.

    DATA: _model               TYPE string,
          _temperature         TYPE p LENGTH 2 DECIMALS 1,
          _system_instructions TYPE string,
          _chat_messages       TYPE yif_aai_google~ty_contents_t,
          _max_tools_calls     TYPE i.

    METHODS _append_to_history
      IMPORTING
        i_s_response TYPE yif_aai_google~ty_contents_response_s.

ENDCLASS.



CLASS ycl_aai_google IMPLEMENTATION.

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

    me->_model = 'gemini-2.5-flash'.

    IF i_model IS SUPPLIED.
      me->_model = i_model.
    ENDIF.

    me->_temperature = 1.

    me->_max_tools_calls = 5.

    IF i_o_connection IS SUPPLIED.
      me->_o_connection = i_o_connection.
    ENDIF.

  ENDMETHOD.

  METHOD yif_aai_google~generate.

    FIELD-SYMBOLS: <ls_generate_request> TYPE any,
                   <l_data>              TYPE string.

    DATA lr_data TYPE REF TO data.

    DATA: ls_response TYPE yif_aai_google~ty_google_generate_response_s.

    DATA: l_endpoint TYPE string,
          l_greeting TYPE string,
          l_message  TYPE string.

    CLEAR: e_response,
           e_failed.

    FREE e_t_response.


    IF me->_o_connection IS NOT BOUND.
      me->_o_connection = NEW ycl_aai_conn( i_api = yif_aai_const=>c_google ).
    ENDIF.

    DATA(l_apikey_url_placeholder) = |{ yif_aai_const=>c_placeholder_pattern }APIKEY{ yif_aai_const=>c_placeholder_pattern }|.

    "l_endpoint = |/v1beta/models/{ me->_model }:generateContent?key={ l_apikey_url_placeholder }|.
    l_endpoint = |/v1beta/models/{ me->_model }:generateContent|.

    me->_o_connection->add_http_header_param(
      EXPORTING
        i_name  = 'X-goog-api-key'
        i_value = l_apikey_url_placeholder
    ).

    IF i_new = abap_true.

      FREE me->_chat_messages.

    ENDIF.

    DATA(lo_aai_util) = NEW ycl_aai_util( ).

    IF me->_chat_messages IS INITIAL.

      IF i_greeting IS NOT INITIAL.

        l_greeting = '{"text": ' && lo_aai_util->serialize( i_greeting ) && '}'.

        APPEND VALUE #( role = 'model' parts = VALUE #( ( l_greeting ) ) ) TO me->_chat_messages.

      ENDIF.

    ENDIF.

    l_message = '{"text": ' && lo_aai_util->serialize( i_message ) && '}'.

    APPEND VALUE #( role = 'user' parts = VALUE #( ( l_message ) ) ) TO me->_chat_messages.

    DO me->_max_tools_calls TIMES.

      IF me->_o_connection->create_connection( i_endpoint = l_endpoint ).

        DATA(ls_generate_request) = VALUE yif_aai_google~ty_google_generate_request_s( contents = me->_chat_messages ).

        ls_generate_request-tools = '[]'.

        IF me->mo_function_calling IS BOUND.

          me->mo_function_calling->get_tools(
            IMPORTING
              e_tools = ls_generate_request-tools
          ).

        ENDIF.

        ls_generate_request-generation_config-temperature = me->_temperature.

        ASSIGN ls_generate_request TO <ls_generate_request>.

        IF me->_system_instructions IS NOT INITIAL.

          "If the System Instructions is set then we need to pass a different request
          DATA(ls_generate_request_sys) = VALUE yif_aai_google~ty_google_generate_req_sys_s(  ).

          ls_generate_request_sys-system_instruction = VALUE #( parts = VALUE #( ( text = me->_system_instructions ) ) ).

          ls_generate_request_sys-contents = ls_generate_request-contents.
          ls_generate_request_sys-generation_config = CORRESPONDING #( ls_generate_request-generation_config ).
          ls_generate_request_sys-tools = ls_generate_request-tools.

          ASSIGN ls_generate_request_sys TO <ls_generate_request>.

        ENDIF.

        DATA(l_json) = lo_aai_util->serialize( i_data = <ls_generate_request> ).

        me->_o_connection->set_body( l_json ).

        FREE l_json.

        RAISE EVENT on_message_send.

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

          RAISE EVENT on_message_failed.

          EXIT.

        ENDIF.

        lo_aai_util->deserialize(
          EXPORTING
            i_json = l_json
          IMPORTING
            e_data = ls_response
        ).

        LOOP AT ls_response-candidates ASSIGNING FIELD-SYMBOL(<ls_candidates>).

          "Add LLM response to the chat history
          me->_append_to_history( <ls_candidates>-content ).

          RAISE EVENT on_response_received.

          DATA(l_function_call) = abap_false.

          LOOP AT <ls_candidates>-content-parts ASSIGNING FIELD-SYMBOL(<ls_parts>).

            IF <ls_parts>-functioncall IS INITIAL.

              e_response = e_response && lo_aai_util->replace_unicode_escape_seq( <ls_parts>-text ).

              CONTINUE.

            ENDIF.

            l_function_call = abap_true.

            ASSIGN <ls_parts>-functioncall-args TO <l_data>.

            " This deserialization may be necessary depending on how the arguments are passed. We may need to parse an escaped string to a JSON string.
            " Example: parse this "{\"latitude\":48.8566,\"longitude\":2.3522}" to a JSON like {"latitude": 48.8566, "longitude": 2.3522}
            lo_aai_util->deserialize(
              EXPORTING
                i_json = <ls_parts>-functioncall-args
              IMPORTING
                e_data = lr_data
            ).

            DATA(lo_typedescr) = cl_abap_typedescr=>describe_by_data_ref( lr_data ).

            " Make sure the deserialized object is a JSON string before assigning it
            IF lo_typedescr->type_kind = cl_abap_typedescr=>typekind_string.

              ASSIGN lr_data->* TO <l_data>.

            ENDIF.

            me->mo_function_calling->call_tool(
              EXPORTING
                i_tool_name   = to_upper( <ls_parts>-functioncall-name )
                i_json        = <l_data>
              RECEIVING
                r_response    = DATA(l_tool_response)
            ).

            "The response cannot be just a text. It must be an object with any attribute(s) name(s).
            l_tool_response = '{"text":' && lo_aai_util->serialize( l_tool_response ) && '}'.

            me->_append_to_history( i_s_response = VALUE #( parts = VALUE #( ( function_response = VALUE #( name = <ls_parts>-functioncall-name
                                                                                                            response = l_tool_response ) ) ) role = 'model' ) ).

          ENDLOOP.

        ENDLOOP.

        IF l_function_call = abap_true.
          CONTINUE.
        ENDIF.

        IF e_t_response IS REQUESTED.

          SPLIT e_response AT cl_abap_char_utilities=>newline INTO TABLE e_t_response.

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

      ENDIF.

      EXIT.

    ENDDO.

  ENDMETHOD.

  METHOD yif_aai_google~get_history.

    e_t_history = me->_chat_messages.

  ENDMETHOD.

  METHOD yif_aai_google~set_connection.

    me->_o_connection = i_o_connection.

  ENDMETHOD.

  METHOD yif_aai_google~set_history.

    me->_chat_messages = i_t_history.

  ENDMETHOD.

  METHOD yif_aai_google~set_model.

    me->_model = i_model.

  ENDMETHOD.

  METHOD yif_aai_google~set_system_instructions.

    me->_system_instructions = i_system_instructions.

  ENDMETHOD.

  METHOD yif_aai_google~set_temperature.

    me->_temperature = i_temperature.

  ENDMETHOD.

  METHOD yif_aai_chat~chat.

    me->generate(
      EXPORTING
        i_message    = i_message
        i_new        = i_new
        i_greeting   = i_greeting
      IMPORTING
        e_response   = e_response
        e_t_response = e_t_response
    ).

  ENDMETHOD.

  METHOD yif_aai_google~bind_tools.

    me->mo_function_calling = i_o_function_calling.

    IF i_max_tools_calls IS SUPPLIED.
      me->_max_tools_calls = i_max_tools_calls.
    ENDIF.

  ENDMETHOD.

  METHOD yif_aai_google~get_conversation.

    r_conversation = NEW ycl_aai_util( )->serialize(
      EXPORTING
        i_data = me->_chat_messages
    ).

  ENDMETHOD.

  METHOD _append_to_history.

    FIELD-SYMBOLS <ls_data> TYPE any.

    DATA: ls_parts_text        TYPE yif_aai_google~ty_parts_response_text_s,
          ls_function_call     TYPE yif_aai_google~ty_parts_request_func_call_s,
          ls_function_response TYPE yif_aai_google~ty_parts_response_func_resp_s,
          ls_request           TYPE yif_aai_google~ty_contents_s.

    DATA: l_json_parts TYPE /ui2/cl_json=>json.

    DATA(lo_aai_util) = NEW ycl_aai_util( ).

    LOOP AT i_s_response-parts ASSIGNING FIELD-SYMBOL(<ls_parts>).

      ls_parts_text = CORRESPONDING #( <ls_parts> ).
      ls_function_call-function_call = <ls_parts>-functioncall.
      ls_function_response = CORRESPONDING #( <ls_parts> ).

      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN ls_function_call-function_call-args WITH space.

      DO 3 TIMES.

        CASE sy-index.

          WHEN 1.

            ASSIGN ls_parts_text TO <ls_data>.

          WHEN 2.

            ASSIGN ls_function_call TO <ls_data>.

          WHEN 3.

            ASSIGN ls_function_response TO <ls_data>.

        ENDCASE.

        IF <ls_data> IS NOT INITIAL.

          IF l_json_parts IS INITIAL.

            l_json_parts = lo_aai_util->serialize( <ls_data> ).

          ELSE.

            l_json_parts = |{ l_json_parts }, { lo_aai_util->serialize( <ls_data> ) }|.

          ENDIF.

        ENDIF.

      ENDDO.

    ENDLOOP.

    APPEND l_json_parts TO ls_request-parts.

    ls_request-role = i_s_response-role.

    APPEND ls_request TO me->_chat_messages.

  ENDMETHOD.

ENDCLASS.
