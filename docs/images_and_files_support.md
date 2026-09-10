# ABAP AI tools - Vision and Files Support

## Vision Support
Image processing is now available in all supported APIs. You can send images to LLM models that have vision capabilities.

Usage example

```abap
*&---------------------------------------------------------------------*
*& Report yaai_r_image_proc_example
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT yaai_r_image_proc_example.

DATA data_tab TYPE solix_tab.

DATA: file_length TYPE i,
      file_type   TYPE string.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
PARAMETERS: p_model TYPE c LENGTH 30 LOWER CASE DEFAULT 'gpt-5.6-luna',
            p_key   TYPE string LOWER CASE,
            p_msg   TYPE c LENGTH 80 LOWER CASE OBLIGATORY DEFAULT 'Hi there! What information does this image have?',
            p_img   TYPE rlgrap-filename OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

INITIALIZATION.

  "Set text for the selection screen fields
  %_p_model_%_app_%-text = 'Model'.
  %_p_key_%_app_%-text = 'API Key'.
  %_p_msg_%_app_%-text = 'Message'.
  %_p_img_%_app_%-text = 'Image'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_img.

  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    CHANGING
      file_name     = p_img
    EXCEPTIONS
      mask_too_long = 0
      OTHERS        = 0.

START-OF-SELECTION.

  cl_gui_frontend_services=>gui_upload(
    EXPORTING
      filename                = CONV #( p_img )
      filetype                = 'BIN'
    IMPORTING
      filelength              = file_length
    CHANGING
      data_tab                = data_tab                  " Transfer table for file contents
    EXCEPTIONS
      file_open_error         = 1                " File does not exist and cannot be opened
      file_read_error         = 2                " Error when reading file
      no_batch                = 3                " Cannot execute front-end function in background
      gui_refuse_filetransfer = 4                " Incorrect front end or error on front end
      invalid_type            = 5                " Incorrect parameter FILETYPE
      no_authority            = 6                " No upload authorization
      unknown_error           = 7                " Unknown error
      bad_data_format         = 8                " Cannot Interpret Data in File
      header_not_allowed      = 9                " Invalid header
      separator_not_allowed   = 10               " Invalid separator
      header_too_long         = 11               " Header information currently restricted to 1023 bytes
      unknown_dp_error        = 12               " Error when calling data provider
      access_denied           = 13               " Access to File Denied
      dp_out_of_memory        = 14               " Not enough memory in data provider
      disk_full               = 15               " Storage medium is full.
      dp_timeout              = 16               " Data provider timeout
      not_supported_by_gui    = 17               " GUI does not support this
      error_no_gui            = 18               " GUI not available
      OTHERS                  = 19
  ).

  IF sy-subrc <> 0.

    MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.

    STOP.

  ENDIF.

  DATA(bin_image) = cl_bcs_convert=>solix_to_xstring(
    EXPORTING
      it_solix = data_tab
  ).

  " Images must be Base 64 encoded
  DATA(base64_image) = cl_http_utility=>encode_x_base64( unencoded = bin_image ).

  file_type = NEW ycl_aai_util( )->get_mime_type( i_filename = p_img ).

  DATA(t_files) = VALUE ytt_aai_files( ( filename = p_img
                                         file_type = file_type
                                         file_size = file_length
                                         content = base64_image ) ).

  " Run the class YCL_AAI_BASIC_SETUP to execute the basic setup of the APIs and models
  DATA(o_aai_conn) = NEW ycl_aai_conn( i_api = yif_aai_const=>c_openai ).

  IF p_key IS NOT INITIAL.
    o_aai_conn->set_api_key( i_api_key = p_key ).
  ENDIF.

  DATA(o_aai_openai) = NEW ycl_aai_openai( i_model = p_model
                                           i_o_connection = o_aai_conn ).

  o_aai_openai->chat(
    EXPORTING
      i_message    = p_msg
      i_t_files    = t_files
    IMPORTING
      e_t_response = DATA(t_response)
  ).

  " Display the LLM response on the screen
  LOOP AT t_response INTO DATA(l_response_line).
    WRITE: / l_response_line.
  ENDLOOP.
```

## Files Support
File processing is now supported by the following APIs:

 - OpenAI
 - Anthropic
 - Google Gemini
 - Mistral
 - Moonshot

Usage example (PDF Processing)

```abap
*&---------------------------------------------------------------------*
*& Report yaai_r_pdf_proc_example
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT yaai_r_pdf_proc_example.

DATA data_tab TYPE solix_tab.

DATA: file_length TYPE i,
      file_type   TYPE string,
      filename    TYPE string.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
PARAMETERS: p_model TYPE c LENGTH 30 LOWER CASE DEFAULT 'gpt-5.6-luna',
            p_key   TYPE string LOWER CASE,
            p_msg   TYPE c LENGTH 80 LOWER CASE OBLIGATORY DEFAULT 'Hi there! What information does this PDF file have?',
            p_pdf   TYPE rlgrap-filename OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

INITIALIZATION.

  "Set text for the selection screen fields
  %_p_model_%_app_%-text = 'Model'.
  %_p_key_%_app_%-text = 'API Key'.
  %_p_msg_%_app_%-text = 'Message'.
  %_p_pdf_%_app_%-text = 'PDF File'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_pdf.

  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    CHANGING
      file_name     = p_pdf
    EXCEPTIONS
      mask_too_long = 0
      OTHERS        = 0.

START-OF-SELECTION.

  cl_bcs_utilities=>split_name(
    EXPORTING
      iv_name = p_pdf
    IMPORTING
      ev_name = filename
  ).

  cl_gui_frontend_services=>gui_upload(
    EXPORTING
      filename                = CONV #( p_pdf )
      filetype                = 'BIN'
    IMPORTING
      filelength              = file_length
    CHANGING
      data_tab                = data_tab                  " Transfer table for file contents
    EXCEPTIONS
      file_open_error         = 1                " File does not exist and cannot be opened
      file_read_error         = 2                " Error when reading file
      no_batch                = 3                " Cannot execute front-end function in background
      gui_refuse_filetransfer = 4                " Incorrect front end or error on front end
      invalid_type            = 5                " Incorrect parameter FILETYPE
      no_authority            = 6                " No upload authorization
      unknown_error           = 7                " Unknown error
      bad_data_format         = 8                " Cannot Interpret Data in File
      header_not_allowed      = 9                " Invalid header
      separator_not_allowed   = 10               " Invalid separator
      header_too_long         = 11               " Header information currently restricted to 1023 bytes
      unknown_dp_error        = 12               " Error when calling data provider
      access_denied           = 13               " Access to File Denied
      dp_out_of_memory        = 14               " Not enough memory in data provider
      disk_full               = 15               " Storage medium is full.
      dp_timeout              = 16               " Data provider timeout
      not_supported_by_gui    = 17               " GUI does not support this
      error_no_gui            = 18               " GUI not available
      OTHERS                  = 19
  ).

  IF sy-subrc <> 0.

    MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.

    STOP.

  ENDIF.

  DATA(bin_image) = cl_bcs_convert=>solix_to_xstring(
    EXPORTING
      it_solix = data_tab
  ).

  " Files must be encoded in Base 64
  DATA(base64_image) = cl_http_utility=>encode_x_base64( unencoded = bin_image ).

  file_type = NEW ycl_aai_util( )->get_mime_type( i_filename = p_pdf ).

  DATA(t_files) = VALUE ytt_aai_files( ( filename = filename
                                         file_type = file_type
                                         file_size = file_length
                                         content = base64_image ) ).

  " Run the class YCL_AAI_BASIC_SETUP to execute the basic setup of the APIs and models
  DATA(o_aai_conn) = NEW ycl_aai_conn( i_api = yif_aai_const=>c_openai ).

  IF p_key IS NOT INITIAL.
    o_aai_conn->set_api_key( i_api_key = p_key ).
  ENDIF.

  DATA(o_aai_openai) = NEW ycl_aai_openai( i_model = p_model
                                           i_o_connection = o_aai_conn ).

  o_aai_openai->chat(
    EXPORTING
      i_message    = p_msg
      i_t_files    = t_files
    IMPORTING
      e_t_response = DATA(t_response)
  ).

  " Display the LLM response on the screen
  LOOP AT t_response INTO DATA(l_response_line).
    WRITE: / l_response_line.
  ENDLOOP.
```