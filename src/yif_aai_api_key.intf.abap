INTERFACE yif_aai_api_key
  PUBLIC.

  METHODS create
    IMPORTING
              i_id             TYPE csequence
              i_username       TYPE xubname OPTIONAL
              i_api_key        TYPE csequence
    RETURNING VALUE(r_created) TYPE abap_bool.

  METHODS read
    IMPORTING
              i_id             TYPE csequence
    RETURNING VALUE(r_api_key) TYPE string.

  METHODS delete
    IMPORTING
              i_id             TYPE csequence
    RETURNING VALUE(r_deleted) TYPE abap_bool.

  METHODS encode
    IMPORTING
              i_api_key                TYPE csequence
    RETURNING VALUE(r_encoded_api_key) TYPE string.

  METHODS decode
    IMPORTING
              i_encoded_api_key        TYPE csequence
    RETURNING VALUE(r_decoded_api_key) TYPE string.

ENDINTERFACE.
