INTERFACE yif_aai_openai
  PUBLIC.

  TYPES: BEGIN OF generate_message_s,
           role    TYPE string,
           content TYPE string,
         END OF generate_message_s.

  TYPES: generate_messages_t TYPE STANDARD TABLE OF generate_message_s WITH NON-UNIQUE KEY role.

  TYPES: BEGIN OF openai_generate_request_s,
           model  TYPE string,
           stream TYPE abap_bool,
           input  TYPE generate_messages_t,
         END OF openai_generate_request_s.

  TYPES: BEGIN OF content_s,
           type TYPE string,
           text TYPE string,
         END OF content_s.

  TYPES content_t TYPE STANDARD TABLE OF content_s WITH NON-UNIQUE KEY text.

  TYPES: BEGIN OF output_s,
           id      TYPE string,
           type    TYPE string,
           role    TYPE string,
           status  TYPE string,
           content TYPE content_t,
         END OF output_s.

  TYPES: output_t TYPE STANDARD TABLE OF output_s WITH NON-UNIQUE KEY id.

  TYPES: BEGIN OF openai_generate_response_s,
           id          TYPE string,
           status      TYPE string,
           model       TYPE string,
           temperature TYPE string,
           output      TYPE output_t,
         END OF openai_generate_response_s.

  METHODS set_model
    IMPORTING
      i_model TYPE csequence.

  METHODS set_system_instructions
    IMPORTING
      i_system_instructions TYPE string.

  METHODS generate
    IMPORTING
      i_message    TYPE csequence
      i_new        TYPE abap_bool DEFAULT abap_false
    EXPORTING
      e_response   TYPE string
      e_t_response TYPE rswsourcet.

  METHODS embed
    IMPORTING
      i_message  TYPE csequence
    EXPORTING
      e_response TYPE string.

ENDINTERFACE.
