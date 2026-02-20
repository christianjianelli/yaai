INTERFACE yif_aai_rag_tools
  PUBLIC.

  METHODS get_documentation
    IMPORTING
              i_id              TYPE yde_aai_id_str
    RETURNING VALUE(r_response) TYPE yde_aai_response.

  METHODS create_documentation
    IMPORTING
              i_description     TYPE yde_aai_description
              i_keywords        TYPE yde_aai_keywords OPTIONAL
              i_content         TYPE yde_aai_file_content_str
    RETURNING VALUE(r_response) TYPE yde_aai_response.

  METHODS update_documentation
    IMPORTING
              i_id              TYPE yde_aai_id_str
              i_description     TYPE yde_aai_description OPTIONAL
              i_keywords        TYPE yde_aai_keywords OPTIONAL
              i_content         TYPE yde_aai_file_content_str
              i_append          TYPE abap_bool DEFAULT abap_true
    RETURNING VALUE(r_response) TYPE yde_aai_response.

  METHODS get_list_of_documents
    IMPORTING
              i_description     TYPE yde_aai_description OPTIONAL
              i_keywords        TYPE yde_aai_keywords OPTIONAL
    RETURNING VALUE(r_response) TYPE yde_aai_response.

ENDINTERFACE.
