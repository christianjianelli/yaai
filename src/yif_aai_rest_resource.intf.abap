INTERFACE yif_aai_rest_resource
  PUBLIC.

  METHODS create
    IMPORTING
      i_o_request  TYPE REF TO if_http_request
      i_o_response TYPE REF TO if_http_response.

  METHODS read
    IMPORTING
      i_o_request  TYPE REF TO if_http_request
      i_o_response TYPE REF TO if_http_response.

  METHODS update
    IMPORTING
      i_o_request  TYPE REF TO if_http_request
      i_o_response TYPE REF TO if_http_response.

  METHODS delete
    IMPORTING
      i_o_request  TYPE REF TO if_http_request
      i_o_response TYPE REF TO if_http_response.

ENDINTERFACE.
