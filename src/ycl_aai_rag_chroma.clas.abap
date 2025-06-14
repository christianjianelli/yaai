CLASS ycl_aai_rag_chroma DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_chroma_api_request_s,
             input TYPE string,
           END OF ty_chroma_api_request_s,

           BEGIN OF ty_metadata_s,
             source TYPE string,
           END OF ty_metadata_s,

           BEGIN OF ty_chroma_api_response_s,
             id           TYPE string,
             metadata     TYPE ty_metadata_s,
             page_content TYPE string,
             type         TYPE string,
           END OF ty_chroma_api_response_s,

           ty_chroma_api_response_tt TYPE STANDARD TABLE OF ty_chroma_api_response_s WITH DEFAULT KEY.

    INTERFACES yif_aai_rag.

    ALIASES get_context FOR yif_aai_rag~get_context.

    METHODS constructor
      IMPORTING
        i_api      TYPE csequence
        i_endpoint TYPE csequence.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA: _api         TYPE string,
          _endpoint    TYPE string,
          _t_documents TYPE ty_chroma_api_response_tt.

ENDCLASS.



CLASS ycl_aai_rag_chroma IMPLEMENTATION.

  METHOD constructor.

    me->_api = i_api.
    me->_endpoint = i_endpoint.

  ENDMETHOD.

  METHOD yif_aai_rag~get_context.

    DATA lt_metadata TYPE STANDARD TABLE OF ty_metadata_s WITH DEFAULT KEY.

    DATA lt_response TYPE ty_chroma_api_response_tt.

    CLEAR e_context.

    DATA(lo_aai_conn) = NEW ycl_aai_conn( i_api = me->_api ).

    IF lo_aai_conn->create_connection( i_endpoint = me->_endpoint ).

      DATA(lo_aai_util) = NEW ycl_aai_util( ).

      DATA(l_json) = lo_aai_util->serialize( i_data = VALUE ty_chroma_api_request_s( input = i_input ) ).

      lo_aai_conn->set_body( l_json ).

      FREE l_json.

      lo_aai_conn->do_receive(
        IMPORTING
          e_response = l_json
      ).

      lo_aai_util->deserialize(
        EXPORTING
          i_json = l_json
        IMPORTING
          e_data = lt_response
      ).

      LOOP AT lt_response ASSIGNING FIELD-SYMBOL(<ls_response>).

        " If set as TRUE then skip context that was already returned in previous calls
        IF i_new_context_only = abap_true.

          READ TABLE me->_t_documents TRANSPORTING NO FIELDS
            WITH KEY id = <ls_response>-id.

          IF sy-subrc = 0.
            CONTINUE.
          ENDIF.

          APPEND <ls_response> TO me->_t_documents.

        ENDIF.

        e_context = |{ e_context } { <ls_response>-page_content }|.

        APPEND <ls_response>-metadata TO lt_metadata.

      ENDLOOP.

      SORT lt_metadata BY source.

      DELETE ADJACENT DUPLICATES FROM lt_metadata COMPARING source.

      LOOP AT lt_metadata ASSIGNING FIELD-SYMBOL(<ls_metadata>).

        IF sy-tabix = 1.
          e_context = |{ e_context } \n\n**SOURCES**:\n\n|.
        ENDIF.

        e_context = |{ e_context } - { <ls_metadata>-source }\n|.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.

ENDCLASS.
