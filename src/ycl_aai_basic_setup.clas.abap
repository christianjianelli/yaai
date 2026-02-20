CLASS ycl_aai_basic_setup DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_aai_basic_setup IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.

    INSERT yaai_api FROM TABLE @( VALUE #( ( id = 'OPENAI' base_url = 'https://api.openai.com' )
                                           ( id = 'ANTHROPIC' base_url = 'https://api.anthropic.com' )
                                           ( id = 'GOOGLE' base_url = 'https://generativelanguage.googleapis.com' )
                                           ( id = 'MISTRAL' base_url = 'https://api.mistral.ai' ) ) ) ACCEPTING DUPLICATE KEYS.

    INSERT yaai_model FROM TABLE @( VALUE #( ( id = 'OPENAI' model = 'gpt-5-nano' default_model = abap_true )
                                             ( id = 'OPENAI' model = 'gpt-5' default_model = abap_false )
                                             ( id = 'ANTHROPIC' model = 'claude-sonnet-4-20250514' default_model = abap_true )
                                             ( id = 'GOOGLE' model = 'gemini-2.5-flash' default_model = abap_true )
                                             ( id = 'MISTRAL' model = 'mistral-medium-latest' default_model = abap_true )
                                             ( id = 'MISTRAL' model = 'mistral-large-latest' default_model = abap_false ) ) ) ACCEPTING DUPLICATE KEYS.

    INSERT yaai_tool FROM TABLE @( VALUE #( ( class_name = 'YCL_AAI_FUNC_CALL_TOOLS'
                                              method_name = 'GET_AVAILABLE_TOOLS'
                                              description = 'Use this tool to retrieve the list of tools available for you to use.' )

                                            ( class_name = 'YCL_AAI_FUNC_CALL_TOOLS'
                                              method_name = 'REQUEST_TOOLS'
                                              description = 'Use this tool to request the schema of the tools you want to use.' )

                                            ( class_name = 'YCL_AAI_RAG_TOOLS'
                                              method_name = 'GET_LIST_OF_DOCUMENTS'
                                              description = 'Use this tool to retrieve the list of documents available for you.' )

                                            ( class_name = 'YCL_AAI_RAG_TOOLS'
                                              method_name = 'GET_DOCUMENTATION'
                                              description = 'Use this tool to retrieve the content of the documentation.' )

                                            ( class_name = 'YCL_AAI_RAG_TOOLS'
                                              method_name = 'CREATE_DOCUMENTATION'
                                              description = 'Use this tool to create a documentation.' )

                                            ( class_name = 'YCL_AAI_RAG_TOOLS'
                                              method_name = 'UPDATE_DOCUMENTATION'
                                              description = 'Use this tool to create a documentation.' )

                                            ( class_name = 'YCL_AAI_PLANNING_TOOLS'
                                              method_name = 'CREATE_PLAN'
                                              description = 'Use this tool to create a plan in markdown format.' )

                                            ( class_name = 'YCL_AAI_PLANNING_TOOLS'
                                              method_name = 'UPDATE_PLAN'
                                              description = 'Use this tool to update a plan in markdown format.' )

                                            ( class_name = 'YCL_AAI_PLANNING_TOOLS'
                                              method_name = 'GET_PLAN'
                                              description = 'Use this tool to retrieve the content of a plan.' )

                                            ( class_name = 'YCL_AAI_PLANNING_TOOLS'
                                              method_name = 'DELETE_PLAN'
                                              description = 'Use this tool to delete a plan.' )

                                                   ) ) ACCEPTING DUPLICATE KEYS.

  ENDMETHOD.
ENDCLASS.
