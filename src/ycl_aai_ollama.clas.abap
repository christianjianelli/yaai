CLASS ycl_aai_ollama DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES: BEGIN OF chat_message,
             role    TYPE string,
             content TYPE string,
           END OF chat_message.

    TYPES: chat_messages_t TYPE STANDARD TABLE OF chat_message WITH NON-UNIQUE KEY role.

    TYPES: BEGIN OF ollama_chat_request,
             model    TYPE string,
             stream   TYPE abap_bool,
             messages TYPE chat_messages_t,
           END OF ollama_chat_request,

           BEGIN OF ollama_chat_response,
             model   TYPE string,
             message TYPE chat_message,
           END OF ollama_chat_response.

    CLASS-DATA m_ref TYPE REF TO ycl_aai_ollama.

    CLASS-METHODS get_instance
      IMPORTING
                i_model      TYPE clike OPTIONAL
      RETURNING VALUE(r_ref) TYPE REF TO ycl_aai_ollama.

    METHODS constructor
      IMPORTING
        i_model TYPE clike OPTIONAL.

    METHODS set_model
      IMPORTING
        i_model TYPE clike.

    METHODS set_system_instructions
      IMPORTING
        i_system_instructions TYPE string.

    METHODS chat
      IMPORTING
        i_message  TYPE clike
        i_new      TYPE abap_bool DEFAULT abap_false
      EXPORTING
        e_response TYPE string.

    METHODS generate
      IMPORTING
        i_message  TYPE string
      EXPORTING
        e_response TYPE string.

    METHODS embed
      IMPORTING
        i_message  TYPE string
      EXPORTING
        e_response TYPE string.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA: _model               TYPE string,
          _ollama_api_request  TYPE ollama_chat_request,
          _ollama_api_response TYPE ollama_chat_response,
          _chat_messages       TYPE chat_messages_t.

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

  METHOD set_model.

    me->_model = i_model.

  ENDMETHOD.

  METHOD chat.

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

      DATA(l_json) = lo_aai_util->serialize( i_data = VALUE ollama_chat_request( model = me->_model
                                                                                 stream = abap_false
                                                                                 messages = me->_chat_messages ) ).

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
          e_data = me->_ollama_api_response
      ).

      APPEND me->_ollama_api_response-message TO me->_chat_messages.

      e_response = lo_aai_util->replace_unicode_escape_seq( me->_ollama_api_response-message-content ).

    ENDIF.

  ENDMETHOD.

  METHOD constructor.

    me->_model = i_model.

  ENDMETHOD.

  METHOD embed.

  ENDMETHOD.

  METHOD generate.

  ENDMETHOD.

  METHOD set_system_instructions.

  ENDMETHOD.

ENDCLASS.
