CLASS ycl_aai_openai DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF generate_message_s,
             role    TYPE string,
             content TYPE string,
           END OF generate_message_s.

    TYPES: generate_messages_t TYPE STANDARD TABLE OF generate_message_s WITH NON-UNIQUE KEY role.

    TYPES: BEGIN OF openai_generate_request_s,
             model  TYPE string,
             stream TYPE abap_bool,
             input  TYPE generate_messages_t,
           END OF openai_generate_request_s.

    TYPES: BEGIN OF content_s,
             type TYPE string,
             text TYPE string,
           END OF content_s.

    TYPES content_t TYPE STANDARD TABLE OF content_s WITH NON-UNIQUE KEY text.

    TYPES: BEGIN OF output_s,
             id      TYPE string,
             type    TYPE string,
             role    TYPE string,
             status  TYPE string,
             content TYPE content_t,
           END OF output_s.

    TYPES: output_t TYPE STANDARD TABLE OF output_s WITH NON-UNIQUE KEY id.

    TYPES: BEGIN OF openai_generate_response_s,
             id          TYPE string,
             status      TYPE string,
             model       TYPE string,
             temperature TYPE string,
             output      TYPE output_t,
           END OF openai_generate_response_s.

    CLASS-DATA m_ref TYPE REF TO ycl_aai_openai.

    CLASS-METHODS get_instance
      IMPORTING
                i_model      TYPE clike OPTIONAL
      RETURNING VALUE(r_ref) TYPE REF TO ycl_aai_openai.

    METHODS constructor
      IMPORTING
        i_model TYPE clike OPTIONAL.

    METHODS set_model
      IMPORTING
        i_model TYPE clike.

    METHODS set_system_instructions
      IMPORTING
        i_system_instructions TYPE string.

    METHODS generate
      IMPORTING
        i_message  TYPE clike
        i_new      TYPE abap_bool DEFAULT abap_false
      EXPORTING
        e_response   TYPE string
        e_t_response TYPE rswsourcet.

    METHODS embed
      IMPORTING
        i_message  TYPE clike
      EXPORTING
        e_response TYPE string.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA: _model                    TYPE string,
          _openai_generate_request  TYPE openai_generate_request_s,
          _openai_generate_response TYPE openai_generate_response_s,
          _messages                 TYPE generate_messages_t.

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

  METHOD set_model.

    me->_model = i_model.

  ENDMETHOD.

  METHOD set_system_instructions.

    APPEND VALUE #( role = 'developer' content = i_system_instructions ) TO me->_messages.

  ENDMETHOD.

  METHOD embed.

    " TODO

  ENDMETHOD.

  METHOD generate.

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

      DATA(l_json) = lo_aai_util->serialize( i_data = VALUE openai_generate_request_s( model = me->_model
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

ENDCLASS.
