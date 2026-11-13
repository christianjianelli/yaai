CLASS ycl_aai_api_key DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES yif_aai_api_key.

    ALIASES create FOR yif_aai_api_key~create.
    ALIASES read FOR yif_aai_api_key~read.
    ALIASES delete FOR yif_aai_api_key~delete.
    ALIASES encode FOR yif_aai_api_key~encode.
    ALIASES decode FOR yif_aai_api_key~decode.

  PROTECTED SECTION.

  PRIVATE SECTION.


ENDCLASS.



CLASS ycl_aai_api_key IMPLEMENTATION.

  METHOD yif_aai_api_key~encode.

    r_encoded_api_key = cl_http_utility=>if_http_utility~encode_base64( i_api_key ).

  ENDMETHOD.

  METHOD yif_aai_api_key~decode.

    r_decoded_api_key = cl_http_utility=>if_http_utility~decode_base64( i_encoded_api_key ).

  ENDMETHOD.

  METHOD yif_aai_api_key~read.

    CLEAR r_api_key.

    SELECT SINGLE api_key
      FROM yaai_api_key
      WHERE id = @i_id
        AND username = @sy-uname
      INTO @DATA(l_api_key).

    IF sy-subrc <> 0.

      SELECT SINGLE api_key
        FROM yaai_api_key
        WHERE id = @i_id
        INTO @l_api_key.

    ENDIF.

    r_api_key = me->decode( l_api_key ).

  ENDMETHOD.

  METHOD yif_aai_api_key~create.

    r_created = abap_false.

    IF i_id IS INITIAL.
      RETURN.
    ENDIF.

    DATA(ls_aai_api_key) = VALUE yaai_api_key( id = i_id
                                               username = i_username
                                               api_key = me->encode( i_api_key ) ).

    IF ls_aai_api_key-api_key IS INITIAL.
      RETURN.
    ENDIF.

    INSERT yaai_api_key FROM ls_aai_api_key.

    IF sy-subrc = 0.
      r_created = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD yif_aai_api_key~delete.

    r_deleted = abap_false.

    DELETE FROM yaai_api_key WHERE id = i_id.

    IF sy-subrc = 0.
      r_deleted = abap_true.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
