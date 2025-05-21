INTERFACE yif_aai_conn
  PUBLIC.

  DATA mr_log TYPE REF TO ycl_aai_log READ-ONLY.

  DATA mt_msg TYPE bapiret2_t READ-ONLY.

  DATA: m_api      TYPE string READ-ONLY,
        m_base_url TYPE string READ-ONLY,
        m_endpoint TYPE string READ-ONLY.

  METHODS
    create_connection
      IMPORTING
                i_endpoint       TYPE string
                i_api_key        TYPE string OPTIONAL
                i_http_method    TYPE string DEFAULT if_http_request=>co_request_method_post
      RETURNING VALUE(r_created) TYPE abap_bool.

  METHODS
    do_receive
      EXPORTING
        e_response TYPE string.

  METHODS
    get_response
      EXPORTING
        e_response TYPE string.

ENDINTERFACE.
