CLASS ycl_aai_openai DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun.

    INTERFACES yif_aai_openai.

    ALIASES set_model FOR yif_aai_openai~set_model.
    ALIASES set_temperature FOR yif_aai_openai~set_temperature.
    ALIASES set_system_instructions FOR yif_aai_openai~set_system_instructions.
    ALIASES bind_tools FOR yif_aai_openai~bind_tools.
    ALIASES generate FOR yif_aai_openai~generate.
    ALIASES embed FOR yif_aai_openai~embed.
    ALIASES set_history FOR yif_aai_openai~set_history.
    ALIASES get_conversation FOR yif_aai_openai~get_conversation.

    ALIASES mo_function_calling FOR yif_aai_openai~mo_function_calling.

    CLASS-DATA m_ref TYPE REF TO ycl_aai_openai.

    CLASS-METHODS get_instance
      IMPORTING
                i_model      TYPE csequence OPTIONAL
      RETURNING VALUE(r_ref) TYPE REF TO ycl_aai_openai.

    METHODS constructor
      IMPORTING
        i_model     TYPE csequence OPTIONAL
        i_t_history TYPE yif_aai_openai~ty_generate_messages_t OPTIONAL.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA: _model                    TYPE string,
          _temperature              TYPE p LENGTH 2 DECIMALS 1,
          _openai_generate_request  TYPE yif_aai_openai~ty_openai_generate_request_s,
          _openai_generate_response TYPE yif_aai_openai~ty_openai_generate_response_s,
          _messages                 TYPE yif_aai_openai~ty_generate_messages_t.

ENDCLASS.



CLASS ycl_aai_openai IMPLEMENTATION.

  METHOD constructor.

    me->_model = COND #( WHEN i_model IS NOT INITIAL THEN i_model ELSE 'gpt-4.1' ).

    me->_messages = i_t_history.

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

  METHOD yif_aai_openai~set_model.

    me->_model = i_model.

  ENDMETHOD.

  METHOD yif_aai_openai~set_temperature.

    me->_temperature = i_temperature.

  ENDMETHOD.

  METHOD yif_aai_openai~set_system_instructions.

    APPEND VALUE #( role = 'developer' content = i_system_instructions ) TO me->_messages.

  ENDMETHOD.

  METHOD yif_aai_openai~bind_tools.

    me->mo_function_calling = i_o_function_calling.

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

    FREE me->_messages.

    APPEND VALUE #( role = 'user'
                    content = i_message
                    type = 'message' ) TO me->_messages.

    IF me->mo_function_calling IS BOUND.

      me->mo_function_calling->get_tools(
        IMPORTING
          e_tools = l_tools
      ).

    ENDIF.

    DATA(lo_aai_conn) = NEW ycl_aai_conn( i_api = yif_aai_const=>c_openai ).

    DATA(lo_aai_util) = NEW ycl_aai_util( ).

    DO 5 TIMES.

      IF lo_aai_conn->create_connection( i_endpoint = yif_aai_const=>c_openai_generate_endpoint ).

        FREE me->_openai_generate_response.

        DATA(l_json) = lo_aai_util->serialize( i_data = VALUE yif_aai_openai~ty_openai_generate_request_s( model = me->_model
                                                                                                           stream = abap_false
                                                                                                           input = me->get_conversation( )
                                                                                                           tools = l_tools ) ).

        lo_aai_conn->set_body( l_json ).

        FREE l_json.

        lo_aai_conn->do_receive(
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
                            content = <ls_content>-text ) TO me->_messages.

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

  METHOD yif_aai_openai~set_history.

    me->_messages = i_t_history.

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

  METHOD yif_aai_openai~embed.

  ENDMETHOD.

  METHOD if_oo_adt_classrun~main.

    DATA(lt_history) = VALUE yif_aai_openai~ty_generate_messages_t( ( role = 'user' content = 'What is the weather like in Paris today?' type = 'message' )
                                                                    ( type = 'function_call' arguments = '{\"location\":\"Paris, France\"}' call_id = 'call_dakeZtJNEjLML0ZkSeQCP2od' name = 'get_weather' )
                                                                    ( type = 'function_call_output' call_id = 'call_dakeZtJNEjLML0ZkSeQCP2od' output = 'Sunny, 24ºC' ) ).

    me->set_history( lt_history ).

    DATA(r_conversation) = me->get_conversation( ).

    out->write( r_conversation ).

  ENDMETHOD.

ENDCLASS.
