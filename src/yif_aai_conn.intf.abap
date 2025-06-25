INTERFACE yif_aai_conn
  PUBLIC.

  DATA mr_log TYPE REF TO ycl_aai_log READ-ONLY.

  DATA mt_msg TYPE bapiret2_t READ-ONLY.

  DATA: m_api                   TYPE string READ-ONLY,
        m_base_url              TYPE string READ-ONLY,
        m_endpoint              TYPE string READ-ONLY,
        m_proxy_host            TYPE string READ-ONLY,
        m_proxy_service         TYPE string READ-ONLY,
        m_proxy_user            TYPE string READ-ONLY,
        m_proxy_passwd          TYPE string READ-ONLY,
        m_suppress_content_type TYPE abap_bool READ-ONLY.

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
        i_api_key TYPE string.

  METHODS
    set_proxy
      IMPORTING
        i_proxy_host    TYPE string OPTIONAL
        i_proxy_service TYPE string OPTIONAL
        i_proxy_user    TYPE string OPTIONAL
        i_proxy_passwd  TYPE string OPTIONAL.

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
        e_response TYPE string.

  METHODS
    get_response
      EXPORTING
        e_response TYPE string.

ENDINTERFACE.
