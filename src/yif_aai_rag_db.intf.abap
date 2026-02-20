INTERFACE yif_aai_rag_db
  PUBLIC .

  TYPES: BEGIN OF ty_document_s,
           id          TYPE string,
           filename    TYPE string,
           description TYPE string,
           keywords    TYPE string,
           content     TYPE string,
         END OF ty_document_s,

         BEGIN OF ty_document_list_s,
           id          TYPE string,
           filename    TYPE string,
           description TYPE string,
           keywords    TYPE string,
         END OF ty_document_list_s,

         ty_documents_t TYPE STANDARD TABLE OF ty_document_list_s WITH EMPTY KEY.

  METHODS create
    IMPORTING
      i_filename    TYPE csequence
      i_description TYPE csequence OPTIONAL
      i_keywords    TYPE csequence OPTIONAL
      i_content     TYPE string
    EXPORTING
      e_id          TYPE uuid
      e_error       TYPE string.

  METHODS read
    IMPORTING
      i_id          TYPE uuid OPTIONAL
      i_filename    TYPE csequence OPTIONAL
    EXPORTING
      e_id          TYPE uuid
      e_filename    TYPE csequence
      e_description TYPE csequence
      e_keywords    TYPE csequence
      e_content     TYPE string
      e_error       TYPE string.

  METHODS update
    IMPORTING
      i_id          TYPE uuid OPTIONAL
      i_filename    TYPE csequence OPTIONAL
      i_description TYPE csequence OPTIONAL
      i_keywords    TYPE csequence OPTIONAL
      i_content     TYPE string OPTIONAL
      i_append      TYPE abap_bool DEFAULT abap_true
    EXPORTING
      e_updated     TYPE abap_bool
      e_error       TYPE string.

  METHODS delete
    IMPORTING
      i_id       TYPE uuid OPTIONAL
      i_filename TYPE csequence OPTIONAL
    EXPORTING
      e_deleted  TYPE abap_bool
      e_error    TYPE string.

  METHODS query
    IMPORTING
      i_filename    TYPE csequence OPTIONAL
      i_description TYPE csequence OPTIONAL
      i_keywords    TYPE csequence OPTIONAL
    EXPORTING
      e_t_documents TYPE ty_documents_t.

ENDINTERFACE.
