CLASS ycl_aai_openai DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES yif_aai_openai.

    ALIASES set_model FOR yif_aai_openai~set_model.
    ALIASES set_system_instructions FOR yif_aai_openai~set_system_instructions.
    ALIASES generate FOR yif_aai_openai~generate.
    ALIASES embed FOR yif_aai_openai~embed.

    CLASS-DATA m_ref TYPE REF TO ycl_aai_openai.

    CLASS-METHODS get_instance
      IMPORTING
                i_model      TYPE csequence OPTIONAL
      RETURNING VALUE(r_ref) TYPE REF TO ycl_aai_openai.

    METHODS constructor
      IMPORTING
        i_model TYPE csequence OPTIONAL.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA: _model                    TYPE string,
          _openai_generate_request  TYPE yif_aai_openai~openai_generate_request_s,
          _openai_generate_response TYPE yif_aai_openai~openai_generate_response_s,
          _messages                 TYPE yif_aai_openai~generate_messages_t.

ENDCLASS.



CLASS ycl_aai_openai IMPLEMENTATION.

  METHOD constructor.

    me->_model = COND #( WHEN i_model IS NOT INITIAL THEN i_model ELSE 'gpt-4.1' ).

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

  METHOD yif_aai_openai~set_system_instructions.

    APPEND VALUE #( role = 'developer' content = i_system_instructions ) TO me->_messages.

  ENDMETHOD.

  METHOD yif_aai_openai~generate.

    CLEAR e_response.

    FREE e_t_response.

    IF me->_model IS INITIAL.
      RETURN.
    ENDIF.

    FREE me->_messages.

    APPEND VALUE #( role = 'user' content = i_message ) TO me->_messages.

    DATA(lo_aai_conn) = NEW ycl_aai_conn( i_api = yif_aai_const=>c_openai ).

    IF lo_aai_conn->create_connection( i_endpoint = yif_aai_const=>c_openai_generate_endpoint ).

      DATA(lo_aai_util) = NEW ycl_aai_util( ).

      DATA(l_json) = lo_aai_util->serialize( i_data = VALUE yif_aai_openai~openai_generate_request_s( model = me->_model
                                                                                                      stream = abap_false
                                                                                                      input = me->_messages ) ).

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

      LOOP AT _openai_generate_response-output ASSIGNING FIELD-SYMBOL(<ls_output>).

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

    ENDIF.

    IF e_t_response IS REQUESTED.

      SPLIT e_response AT cl_abap_char_utilities=>newline INTO TABLE e_t_response.

    ENDIF.

  ENDMETHOD.

  METHOD yif_aai_openai~embed.

  ENDMETHOD.

ENDCLASS.
