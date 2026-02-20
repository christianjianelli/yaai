CLASS ycl_aai_rag_tools DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_doc_s,
             id          TYPE string,
             filename    TYPE string,
             description TYPE string,
             keywords    TYPE string,
           END OF ty_doc_s,

           ty_doc_t TYPE STANDARD TABLE OF ty_doc_s WITH EMPTY KEY.

    INTERFACES if_oo_adt_classrun.

    INTERFACES yif_aai_rag_tools.

    ALIASES: get_documentation FOR yif_aai_rag_tools~get_documentation,
             create_documentation FOR yif_aai_rag_tools~create_documentation,
             update_documentation FOR yif_aai_rag_tools~update_documentation,
             get_list_of_documents FOR yif_aai_rag_tools~get_list_of_documents.

    METHODS constructor
      IMPORTING
        i_o_agent TYPE REF TO yif_aai_agent OPTIONAL.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA _o_agent TYPE REF TO yif_aai_agent.

ENDCLASS.



CLASS ycl_aai_rag_tools IMPLEMENTATION.

  METHOD constructor.

    IF i_o_agent IS SUPPLIED.

      me->_o_agent = i_o_agent.

    ENDIF.

  ENDMETHOD.

  METHOD yif_aai_rag_tools~create_documentation.

    CLEAR r_response.

    DATA(l_description) = i_description.
    DATA(l_file_content) = i_content.

    NEW ycl_aai_rag_db( )->create(
      EXPORTING
        i_filename    = space
        i_description = l_description
        i_keywords    = i_keywords
        i_content     = l_file_content
      IMPORTING
        e_id = DATA(l_id)
        e_error = DATA(l_error)
    ).

    IF l_id IS NOT INITIAL.
      r_response = |Documentation created successfully! Id:`{ l_id }`.|.
    ELSE.
      r_response = |Error while creating the documentation. Error: { l_error }|.
    ENDIF.

  ENDMETHOD.

  METHOD yif_aai_rag_tools~get_documentation.

    CLEAR r_response.

    NEW ycl_aai_rag_db( )->read(
      EXPORTING
        i_id = CONV #( i_id )
      IMPORTING
        e_content  = r_response
    ).

  ENDMETHOD.

  METHOD yif_aai_rag_tools~update_documentation.

    CLEAR r_response.

    DATA(l_description) = i_description.
    DATA(l_file_content) = i_content.

    NEW ycl_aai_rag_db( )->update(
      EXPORTING
        i_id          = CONV #( i_id )
        i_description = l_description
        i_keywords    = i_keywords
        i_content     = l_file_content
        i_append      = i_append
      IMPORTING
        e_updated  = DATA(l_updated)
        e_error  = DATA(l_error)
    ).

    IF l_updated = abap_true.
      r_response = 'Documentation updated successfully!'.
    ELSE.
      r_response = |Error while updating the documentation. Error: { l_error }|.
    ENDIF.

  ENDMETHOD.

  METHOD yif_aai_rag_tools~get_list_of_documents.

    CLEAR r_response.

    IF me->_o_agent IS NOT BOUND.
      r_response = 'No documents found.'.
      RETURN.
    ENDIF.

    DATA(lt_documents) = me->_o_agent->get_docs( ).

    IF lt_documents IS INITIAL.
      r_response = 'No documents found.'.
      RETURN.
    ENDIF.

    DATA(lt_docs) = VALUE ty_doc_t( ).

    LOOP AT lt_documents ASSIGNING FIELD-SYMBOL(<ls_document>).
      APPEND VALUE #( id = <ls_document>-rag_id
                      description = <ls_document>-description
                      keywords = <ls_document>-keywords ) TO lt_docs.
    ENDLOOP.

    r_response = /ui2/cl_json=>serialize(
      EXPORTING
        data = lt_docs
        compress = abap_false
        pretty_name = /ui2/cl_json=>pretty_mode-low_case
    ).

  ENDMETHOD.

  METHOD if_oo_adt_classrun~main.

*    me->_o_agent = NEW ycl_aai_agent(
*      i_agent_id = CONV #( 'C6111BEC04B71FE0B9DFCCB09425F70E' )
*    ).
*
*    out->write( me->get_list_of_documents( ) ).
*
*    out->write( me->get_documentation( CONV #( '96BEA4B5DF531FD0B9DFBF81343EBF7A' ) ) ).


     out->write( me->update_documentation(
       EXPORTING
         i_id          = 'AE33CAD4D4B61FE0BFD0963F147A65C5'
         i_content     = 'Here goes some appended content ...'
         i_append      = abap_true
     ) ).

  ENDMETHOD.

ENDCLASS.
