INTERFACE yif_aai_api_keys
  PUBLIC.

  METHODS create
    IMPORTING
              i_id             TYPE clike
              i_api_key        TYPE clike
    RETURNING VALUE(r_created) TYPE abap_bool.

  METHODS read
    IMPORTING
              i_id             TYPE clike
    RETURNING VALUE(r_api_key) TYPE string.

  METHODS delete
    IMPORTING
              i_id             TYPE clike
    RETURNING VALUE(r_deleted) TYPE abap_bool.

ENDINTERFACE.
