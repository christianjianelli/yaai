CLASS ycl_aai_api_keys DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES yif_aai_api_keys.

    ALIASES create FOR yif_aai_api_keys~create.
    ALIASES read FOR yif_aai_api_keys~read.
    ALIASES delete FOR yif_aai_api_keys~delete.

  PROTECTED SECTION.

  PRIVATE SECTION.

    METHODS encode
      IMPORTING
                i_api_key                TYPE string
      RETURNING VALUE(r_encoded_api_key) TYPE string.

    METHODS decode
      IMPORTING
                i_encoded_api_key        TYPE string
      RETURNING VALUE(r_decoded_api_key) TYPE string.


ENDCLASS.



CLASS ycl_aai_api_keys IMPLEMENTATION.

  METHOD encode.

    r_encoded_api_key = cl_http_utility=>if_http_utility~encode_base64( i_api_key ).

  ENDMETHOD.

  METHOD decode.

    r_decoded_api_key = cl_http_utility=>if_http_utility~decode_base64( i_encoded_api_key ).

  ENDMETHOD.

  METHOD yif_aai_api_keys~read.

    CLEAR r_api_key.

    SELECT SINGLE api_key
      FROM yaai_api_key
      WHERE id = @i_id
      INTO @DATA(l_api_key).

    r_api_key = me->decode( l_api_key ).

  ENDMETHOD.

  METHOD yif_aai_api_keys~create.

    r_created = abap_false.

    IF i_id IS INITIAL.
      RETURN.
    ENDIF.

    DATA(ls_aai_api_key) = VALUE yaai_api_key( id = i_id
                                               api_key = me->encode( i_api_key ) ).

    IF ls_aai_api_key-api_key IS INITIAL.
      RETURN.
    ENDIF.

    INSERT yaai_api_key FROM ls_aai_api_key.

    IF sy-subrc = 0.
      r_created = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD yif_aai_api_keys~delete.

    r_deleted = abap_false.

    DELETE FROM yaai_api_key WHERE id = i_id.

    IF sy-subrc = 0.
      r_deleted = abap_true.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
