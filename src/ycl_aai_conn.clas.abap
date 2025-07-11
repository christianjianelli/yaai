CLASS ycl_aai_conn DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES yif_aai_conn.

    ALIASES mo_log FOR yif_aai_conn~mo_log.
    ALIASES mo_api_key FOR yif_aai_conn~mo_api_key.
    ALIASES mt_msg FOR yif_aai_conn~mt_msg.
    ALIASES m_api FOR yif_aai_conn~m_api.
    ALIASES m_base_url FOR yif_aai_conn~m_base_url.
    ALIASES m_ssl_id FOR yif_aai_conn~m_ssl_id.
    ALIASES create_connection FOR yif_aai_conn~create_connection.
    ALIASES set_body FOR yif_aai_conn~set_body.
    ALIASES do_receive FOR yif_aai_conn~do_receive.
    ALIASES get_response FOR yif_aai_conn~get_response.
    ALIASES set_api_key FOR yif_aai_conn~set_api_key.
    ALIASES set_base_url FOR yif_aai_conn~set_base_url.
    ALIASES set_ssl_id FOR yif_aai_conn~set_ssl_id.
    ALIASES get_error_text FOR yif_aai_conn~get_error_text.

    METHODS
      constructor
        IMPORTING
          i_api TYPE string OPTIONAL.

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

    DATA l_name TYPE tvarvc-name.

    me->m_ssl_id = 'ANONYM'.

    me->yif_aai_conn~m_suppress_content_type = abap_false.

    me->m_api = i_api.

    IF i_api IS INITIAL.
      RETURN.
    ENDIF.

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

      WHEN yif_aai_const=>c_google.

        SELECT SINGLE low FROM tvarvc
          WHERE name = @yif_aai_const=>c_google_base_url_param
            AND type = 'P'
            AND numb = '0000'
           INTO @me->m_base_url.

      WHEN OTHERS.

        l_name = |YAAI_{ i_api }|.

        SELECT SINGLE low FROM tvarvc
          WHERE name = @l_name
            AND type = 'P'
            AND numb = '0000'
           INTO @me->m_base_url.

    ENDCASE.

  ENDMETHOD.

  METHOD yif_aai_conn~create_connection.

    r_created = abap_false.

    me->_url = me->m_base_url.

    IF i_endpoint IS NOT INITIAL AND i_endpoint(1) <> '/'.
      me->_url = |{ me->_url }/{ i_endpoint }|.
    ENDIF.

    IF i_endpoint IS NOT INITIAL AND i_endpoint(1) = '/'.
      me->_url = |{ me->_url }{ i_endpoint }|.
    ENDIF.

    IF me->_api_key IS INITIAL AND me->m_api IS NOT INITIAL.

      IF me->mo_api_key IS NOT BOUND.

        me->mo_api_key = NEW ycl_aai_api_key( ).

      ENDIF.

      me->set_api_key( i_api_key = me->mo_api_key->read( me->m_api ) ).

    ENDIF.

    "If the API expects to receive the API Key in the URL
    DATA(l_apikey_url_placeholder) = |{ yif_aai_const=>c_placeholder_pattern }APIKEY{ yif_aai_const=>c_placeholder_pattern }|.

    FIND l_apikey_url_placeholder IN me->_url.

    IF sy-subrc = 0.

      REPLACE l_apikey_url_placeholder IN me->_url WITH me->_api_key.

      "If the API key is in the URL, skip adding the Bearer token to the HTTP header
      DATA(l_skip_bearer_http_header) = abap_true.

    ENDIF.

    cl_http_client=>create_by_url(
      EXPORTING
        url                = me->_url                          " URL
        proxy_host         = me->yif_aai_conn~m_proxy_host     " Logical destination (specified in function call)
        proxy_service      = me->yif_aai_conn~m_proxy_service  " Port Number
        ssl_id             = me->m_ssl_id
        proxy_user         = me->yif_aai_conn~m_proxy_user     " Proxy user
        proxy_passwd       = me->yif_aai_conn~m_proxy_passwd   " Proxy password
      IMPORTING
        client             = me->_o_http_client                " HTTP Client Abstraction
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

    IF me->yif_aai_conn~m_suppress_content_type = abap_false.

      me->_o_http_client->request->set_header_field(
        EXPORTING
          name  = 'Content-Type'                 " Name of the header field
          value = 'application/json'             " HTTP header field value
      ).

    ELSE.

      me->_o_http_client->request->suppress_content_type( me->yif_aai_conn~m_suppress_content_type ).

    ENDIF.

    LOOP AT me->yif_aai_conn~mt_http_header INTO DATA(ls_http_header).

      FIND l_apikey_url_placeholder IN ls_http_header-value.

      IF sy-subrc = 0.

        REPLACE l_apikey_url_placeholder IN ls_http_header-value WITH me->_api_key.

        l_skip_bearer_http_header = abap_true.

      ENDIF.

      _o_http_client->request->set_header_field(
        EXPORTING
          name  = ls_http_header-name          " Name of the header field
          value = ls_http_header-value         " HTTP header field value
      ).

    ENDLOOP.

    IF me->_api_key IS NOT INITIAL AND l_skip_bearer_http_header = abap_false.

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

    "Tip: in case of certificate mismatch errors try to set the parameter "icm/HTTPS/client_sni_enabled" to "TRUE" (Transaction RZ10)

    e_failed = abap_false.

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

      e_failed = abap_true.

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

      me->_o_http_client->close(
        EXCEPTIONS
          http_invalid_state = 0
          OTHERS             = 0
      ).

    ELSE.

      me->log( i_s_msg = VALUE #( number = '002' )
               i_log_system_message = abap_true ).

      e_failed = abap_true.

      RETURN.

    ENDIF.

  ENDMETHOD.

  METHOD yif_aai_conn~get_response.

    e_response = me->_o_http_client->response->get_cdata( ).

  ENDMETHOD.

  METHOD log.

    IF me->mo_log IS NOT BOUND.
      me->mo_log = NEW #( ).
    ENDIF.

    me->mo_log->add( i_s_msg = i_s_msg ).

    IF sy-msgid IS NOT INITIAL AND
       sy-msgty IS NOT INITIAL AND
       sy-msgno IS NOT INITIAL.

      me->mo_log->add( VALUE #( id = sy-msgid
                                type = sy-msgty
                                number = sy-msgno
                                message_v1 = sy-msgv1
                                message_v2 = sy-msgv2
                                message_v3 = sy-msgv3
                                message_v4 = sy-msgv4 ) ).
    ENDIF.

  ENDMETHOD.

  METHOD yif_aai_conn~set_base_url.

    me->m_base_url = i_base_url.

  ENDMETHOD.

  METHOD yif_aai_conn~set_api_key.

    me->_api_key = i_api_key.

  ENDMETHOD.

  METHOD yif_aai_conn~set_proxy.

    me->yif_aai_conn~m_proxy_host = i_proxy_host.
    me->yif_aai_conn~m_proxy_service = i_proxy_service.
    me->yif_aai_conn~m_proxy_user = i_proxy_user.
    me->yif_aai_conn~m_proxy_passwd = i_proxy_passwd.

  ENDMETHOD.

  METHOD yif_aai_conn~suppress_content_type.

    me->yif_aai_conn~m_suppress_content_type = i_suppress_content_type.

  ENDMETHOD.

  METHOD yif_aai_conn~set_ssl_id.

    me->m_ssl_id = i_ssl_id.

  ENDMETHOD.

  METHOD yif_aai_conn~get_error_text.

    DATA l_text TYPE string.

    CLEAR e_error_text.

    LOOP AT me->mo_log->mt_msg ASSIGNING FIELD-SYMBOL(<ls_msg>).

      IF <ls_msg>-id IS INITIAL OR <ls_msg>-type IS INITIAL OR <ls_msg>-number IS INITIAL.
        CONTINUE.
      ENDIF.

      MESSAGE ID <ls_msg>-id
        TYPE <ls_msg>-type
        NUMBER <ls_msg>-number
        WITH <ls_msg>-message_v1
             <ls_msg>-message_v2
             <ls_msg>-message_v3
             <ls_msg>-message_v4
        INTO l_text.

      e_error_text = |{ e_error_text } { l_text }|.

    ENDLOOP.

  ENDMETHOD.

  METHOD yif_aai_conn~add_http_header_param.

    APPEND VALUE #( name = i_name
                    value = i_value ) TO me->yif_aai_conn~mt_http_header.

  ENDMETHOD.

  METHOD yif_aai_conn~remove_http_header_param.

    DELETE me->yif_aai_conn~mt_http_header WHERE name = i_name.

  ENDMETHOD.

ENDCLASS.
