INTERFACE yif_aai_conn
  PUBLIC.

  TYPES: BEGIN OF ty_http_header_s,
           name  TYPE string,
           value TYPE string,
         END OF ty_http_header_s.

  DATA: mo_log     TYPE REF TO ycl_aai_log READ-ONLY,
        mo_api_key TYPE REF TO yif_aai_api_key READ-ONLY.

  DATA: mt_msg         TYPE bapiret2_t READ-ONLY,
        mt_http_header TYPE STANDARD TABLE OF ty_http_header_s READ-ONLY.

  DATA: m_api                   TYPE string READ-ONLY,
        m_base_url              TYPE string READ-ONLY,
        m_endpoint              TYPE string READ-ONLY,
        m_proxy_host            TYPE string READ-ONLY,
        m_proxy_service         TYPE string READ-ONLY,
        m_proxy_user            TYPE string READ-ONLY,
        m_proxy_passwd          TYPE string READ-ONLY,
        m_suppress_content_type TYPE abap_bool READ-ONLY,
        m_ssl_id                TYPE ssfapplssl READ-ONLY.

  EVENTS on_request_send.
  EVENTS on_response_received.
  EVENTS on_connection_error.

  METHODS
    create_connection
      IMPORTING
                i_endpoint       TYPE string
                i_api_key        TYPE string OPTIONAL
                i_http_method    TYPE string DEFAULT if_http_request=>co_request_method_post
                i_body_json      TYPE string OPTIONAL
      RETURNING VALUE(r_created) TYPE abap_bool.

  METHODS
    set_base_url
      IMPORTING
        i_base_url TYPE string.

  METHODS
    set_api_key
      IMPORTING
        i_o_api_key TYPE REF TO yif_aai_api_key OPTIONAL
        i_api_key   TYPE string OPTIONAL.

  METHODS
    set_proxy
      IMPORTING
        i_proxy_host    TYPE string OPTIONAL
        i_proxy_service TYPE string OPTIONAL
        i_proxy_user    TYPE string OPTIONAL
        i_proxy_passwd  TYPE string OPTIONAL.

  METHODS
    set_ssl_id
      IMPORTING
        i_ssl_id TYPE ssfapplssl.

  METHODS
    suppress_content_type
      IMPORTING
        i_suppress_content_type TYPE abap_bool DEFAULT abap_true.

  METHODS
    set_body
      IMPORTING
        i_json TYPE string.

  METHODS
    do_receive
      EXPORTING
        e_response TYPE string
        e_failed   TYPE abap_bool.

  METHODS
    get_response
      EXPORTING
        e_response TYPE string.

  METHODS
    get_error_text
      EXPORTING
        e_error_text TYPE string.

  METHODS add_http_header_param
    IMPORTING
      i_name  TYPE csequence
      i_value TYPE csequence.

  METHODS remove_http_header_param
    IMPORTING
      i_name  TYPE csequence.

ENDINTERFACE.
