CLASS ycl_aai_ollama DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES yif_aai_ollama.

    ALIASES set_model FOR yif_aai_ollama~set_model.
    ALIASES set_system_instructions FOR yif_aai_ollama~set_system_instructions.
    ALIASES bind_tools FOR yif_aai_ollama~bind_tools.
    ALIASES chat FOR yif_aai_ollama~chat.
    ALIASES generate FOR yif_aai_ollama~generate.
    ALIASES embed FOR yif_aai_ollama~embed.
    ALIASES mo_function_calling FOR yif_aai_ollama~mo_function_calling.


    CLASS-DATA m_ref TYPE REF TO ycl_aai_ollama.

    CLASS-METHODS get_instance
      IMPORTING
                i_model      TYPE csequence OPTIONAL
      RETURNING VALUE(r_ref) TYPE REF TO ycl_aai_ollama.

    METHODS constructor
      IMPORTING
        i_model TYPE csequence OPTIONAL.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA: _model                    TYPE string,
          _system_instructions      TYPE string,
          _ollama_chat_response     TYPE yif_aai_ollama~ollama_chat_response_s,
          _ollama_generate_response TYPE yif_aai_ollama~ollama_generate_response_s,
          _chat_messages            TYPE yif_aai_ollama~chat_messages_t.

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

  ENDMETHOD.

  METHOD yif_aai_ollama~set_model.

    me->_model = i_model.

  ENDMETHOD.

  METHOD yif_aai_ollama~set_system_instructions.

    APPEND VALUE #( role = 'system' content = i_system_instructions ) TO me->_chat_messages.

  ENDMETHOD.

  METHOD yif_aai_ollama~bind_tools.

    me->mo_function_calling = i_o_function_calling.

  ENDMETHOD.

  METHOD yif_aai_ollama~chat.

    CLEAR e_response.

    FREE e_t_response.

    IF me->_model IS INITIAL.
      RETURN.
    ENDIF.

    IF i_new = abap_true.
      FREE me->_chat_messages.
    ENDIF.

    APPEND VALUE #( role = 'user' content = i_message ) TO me->_chat_messages.

    DATA(lo_aai_conn) = NEW ycl_aai_conn( i_api = yif_aai_const=>c_ollama ).

    IF lo_aai_conn->create_connection( i_endpoint = yif_aai_const=>c_ollama_chat_endpoint ).

      DATA(lo_aai_util) = NEW ycl_aai_util( ).

      IF me->mo_function_calling IS BOUND.

        me->mo_function_calling->get_tools(
          IMPORTING
            e_tools = DATA(l_tools)
        ).

      ENDIF.

      DATA(l_json) = lo_aai_util->serialize( i_data = VALUE yif_aai_ollama~ollama_chat_request_s( model = me->_model
                                                                                                  stream = abap_false
                                                                                                  messages = me->_chat_messages
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
          e_data = me->_ollama_chat_response
      ).

      me->_ollama_chat_response-message-content = lo_aai_util->replace_unicode_escape_seq( me->_ollama_chat_response-message-content ).

      APPEND me->_ollama_chat_response-message TO me->_chat_messages.

      e_response = me->_ollama_chat_response-message-content.

    ENDIF.

    IF e_t_response IS REQUESTED.

      SPLIT e_response AT cl_abap_char_utilities=>newline INTO TABLE e_t_response.

    ENDIF.

  ENDMETHOD.

  METHOD yif_aai_ollama~generate.

    CLEAR e_response.

    FREE e_t_response.

    IF me->_model IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lo_aai_conn) = NEW ycl_aai_conn( i_api = yif_aai_const=>c_ollama ).

    IF lo_aai_conn->create_connection( i_endpoint = yif_aai_const=>c_ollama_generate_endpoint ).

      DATA(lo_aai_util) = NEW ycl_aai_util( ).

      DATA(l_json) = lo_aai_util->serialize( i_data = VALUE yif_aai_ollama~ollama_generate_request_s( model = me->_model
                                                                                                      prompt = i_message
                                                                                                      stream = abap_false ) ).

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
          e_data = me->_ollama_generate_response
      ).

      me->_ollama_generate_response-response = lo_aai_util->replace_unicode_escape_seq( me->_ollama_generate_response-response ).

      e_response = me->_ollama_generate_response-response.

    ENDIF.

    IF e_t_response IS REQUESTED.

      SPLIT e_response AT cl_abap_char_utilities=>newline INTO TABLE e_t_response.

    ENDIF.

  ENDMETHOD.

  METHOD yif_aai_ollama~embed.

  ENDMETHOD.

ENDCLASS.
