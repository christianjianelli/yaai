CLASS ycl_aai_conn DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES yif_aai_conn.
    ALIASES mr_log FOR yif_aai_conn~mr_log.
    ALIASES mt_msg FOR yif_aai_conn~mt_msg.
    ALIASES m_api FOR yif_aai_conn~m_api.
    ALIASES m_base_url FOR yif_aai_conn~m_base_url.
    ALIASES create_connection FOR yif_aai_conn~create_connection.
    ALIASES set_body FOR yif_aai_conn~set_body.
    ALIASES do_receive FOR yif_aai_conn~do_receive.
    ALIASES get_response FOR yif_aai_conn~get_response.
    ALIASES set_api_key FOR yif_aai_conn~set_api_key.

    METHODS
      constructor
        IMPORTING
          i_api TYPE string.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA: _o_http_client TYPE REF TO if_http_client.

    DATA: _url      TYPE string,
          _api_key  TYPE string,
          _response TYPE string.

    METHODS
      log
        IMPORTING
          i_s_msg              TYPE bapiret2
          i_log_system_message TYPE abap_bool DEFAULT abap_false.


ENDCLASS.



CLASS ycl_aai_conn IMPLEMENTATION.

  METHOD constructor.

    me->m_api = i_api.

    me->set_api_key( new ycl_aai_api_keys( )->read( i_api ) ).

    CASE me->m_api.

      WHEN yif_aai_const=>c_ollama.

        SELECT SINGLE low FROM tvarvc
          WHERE name = @yif_aai_const=>c_ollama_base_url_param
            AND type = 'P'
            AND numb = '0000'
           INTO @me->m_base_url.

      WHEN yif_aai_const=>c_openai.

        SELECT SINGLE low FROM tvarvc
          WHERE name = @yif_aai_const=>c_openai_base_url_param
            AND type = 'P'
            AND numb = '0000'
           INTO @me->m_base_url.

    ENDCASE.

  ENDMETHOD.

  METHOD yif_aai_conn~create_connection.

    r_created = abap_false.

    me->_url = |{ me->m_base_url }/{ i_endpoint }|.

    cl_http_client=>create_by_url(
      EXPORTING
        url                = me->_url         " URL
*        proxy_host         =                 " Logical destination (specified in function call)
*        proxy_service      =                 " Port Number
*        ssl_id             =                 " SSL Identity
*        sap_username       =                 " ABAP System, User Logon Name
*        sap_client         =                 " R/3 system (client number from logon)
*        proxy_user         =                 " Proxy user
*        proxy_passwd       =                 " Proxy password
      IMPORTING
        client             = me->_o_http_client                 " HTTP Client Abstraction
      EXCEPTIONS
        argument_not_found = 1                " Communication parameter (host or service) not available
        plugin_not_active  = 2                " HTTP/HTTPS communication not available
        internal_error     = 3                " Internal error (e.g. name too long)
        OTHERS             = 4
    ).

    IF sy-subrc <> 0.

      me->log( i_s_msg = VALUE #( number = '001' )
               i_log_system_message = abap_true ).

      RETURN.

    ENDIF.

    me->_o_http_client->request->set_method( i_http_method ).

    me->_o_http_client->request->set_header_field(
      EXPORTING
        name  = 'Content-Type'                 " Name of the header field
        value = 'application/json'             " HTTP header field value
    ).

    IF me->_api_key IS NOT INITIAL.

      _o_http_client->request->set_header_field(
        EXPORTING
          name  = 'Authorization'                " Name of the header field
          value = |Bearer { me->_api_key }|      " HTTP header field value
      ).

    ENDIF.

    IF i_body_json IS NOT INITIAL.

      CALL METHOD me->_o_http_client->request->set_cdata
        EXPORTING
          data = i_body_json.

    ENDIF.

    r_created = abap_true.

  ENDMETHOD.

  METHOD yif_aai_conn~set_body.

    CALL METHOD me->_o_http_client->request->set_cdata
      EXPORTING
        data = i_json.

  ENDMETHOD.

  METHOD yif_aai_conn~do_receive.

    IF me->_o_http_client IS NOT BOUND.
      RETURN.
    ENDIF.

    me->_o_http_client->send(
*      EXPORTING
*        timeout                    = co_timeout_default " Timeout of Answer Waiting Time
      EXCEPTIONS
        http_communication_failure = 1                  " Communication Error
        http_invalid_state         = 2                  " Invalid state
        http_processing_failed     = 3                  " Error When Processing Method
        http_invalid_timeout       = 4                  " Invalid Time Entry
        OTHERS                     = 5
    ).

    IF sy-subrc <> 0.

      me->log( i_s_msg = VALUE #( number = '002' )
               i_log_system_message = abap_true ).

      RETURN.

    ENDIF.

    me->_o_http_client->receive(
      EXCEPTIONS
        http_communication_failure = 1                " Communication Error
        http_invalid_state         = 2                " Invalid state
        http_processing_failed     = 3                " Error When Processing Method
        OTHERS                     = 4
    ).

    IF sy-subrc = 0.

      me->_response = me->_o_http_client->response->get_cdata( ).

      e_response = me->_response.

    ELSE.

      me->log( i_s_msg = VALUE #( number = '002' )
               i_log_system_message = abap_true ).

      RETURN.

    ENDIF.

  ENDMETHOD.

  METHOD yif_aai_conn~get_response.

    e_response = me->_o_http_client->response->get_cdata( ).

  ENDMETHOD.

  METHOD log.

    IF me->mr_log IS NOT BOUND.
      me->mr_log = NEW #( ).
    ENDIF.

    me->mr_log->add( i_s_msg = i_s_msg ).

    IF sy-msgid IS NOT INITIAL AND
       sy-msgty IS NOT INITIAL AND
       sy-msgno IS NOT INITIAL.

      me->mr_log->add( VALUE #( id = sy-msgid
                                type = sy-msgty
                                number = sy-msgno
                                message_v1 = sy-msgv1
                                message_v2 = sy-msgv2
                                message_v3 = sy-msgv3
                                message_v4 = sy-msgv4 ) ).
    ENDIF.

  ENDMETHOD.

  METHOD yif_aai_conn~set_api_key.

    me->_api_key = i_api_key.

  ENDMETHOD.

ENDCLASS.
