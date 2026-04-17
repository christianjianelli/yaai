INTERFACE yif_aai_planning_tools
  PUBLIC.

  METHODS create_plan
    IMPORTING
              i_plan            TYPE string
              i_description     TYPE string OPTIONAL
    RETURNING VALUE(r_response) TYPE string.

  METHODS get_plan
    RETURNING VALUE(r_response) TYPE string.

  METHODS update_plan
    IMPORTING
              i_plan            TYPE string
              i_description     TYPE string OPTIONAL
              i_append          TYPE abap_bool DEFAULT abap_true
    RETURNING VALUE(r_response) TYPE string.

  METHODS delete_plan
    RETURNING VALUE(r_response) TYPE string.

ENDINTERFACE.
