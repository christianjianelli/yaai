# yaai - ABAP AI tools - OpenAI

## Quickstart

### Running Your First ABAP AI OpenAI Application

This quickstart demonstrates how to create a simple LLM application. It shows you how to connect to the LLM and perform a basic chat interaction.

**Requirements:** 
*   You have a valid OpenAI API Key.
*   Import OpenAI server certificates into SAP trust manager. The [abapGit documentation](https://docs.abapgit.org/user-guide/setup/ssl-setup.html) explains in detail how to do it.

    **Note**: To run the application on SAP NetWeaver AS ABAP Developer Edition, we recommend using NGINX as a reverse proxy to expose a local HTTP endpoint—it’s much simpler than manually configuring SSL on the SAP system.

**Steps:**
1.  Create an ABAP AI Connection instance;
2.  Set the Base URL;
3.  Set the API Key;
4.  Create an ABAP AI OpenAI instance;
5.  Call the CHAT method.

**Example:**

```abap
REPORT yaai_r_simple_llm_app_openai LINE-SIZE 500.

START-OF-SELECTION.

  DATA(o_aai_conn) = NEW ycl_aai_conn( ).

  " The hardcoded base URL in this example is intended for testing and development only. 
  o_aai_conn->set_base_url( i_base_url = 'https://api.openai.com' ).
 
  " The hardcoded API key in this example is intended for testing and development only.
  " Hardcoding API keys directly into your ABAP code is highly discouraged.
  o_aai_conn->set_api_key( i_api_key = 'REPLACE_THIS_TEXT_WITH_YOUR_OPENAI_API_KEY' ).

  DATA(o_aai_openai) = NEW ycl_aai_openai( i_model = 'gpt-4.1' i_o_connection = o_aai_conn ).

  o_aai_openai->chat(
    EXPORTING
      i_message    = 'Hi, there!'
    IMPORTING
      e_t_response = DATA(t_response)
  ).

  " Display the LLM response on the screen
  LOOP AT t_response INTO DATA(l_response_line).
    WRITE: / l_response_line.
  ENDLOOP.
``` 

**Result:**

The following screenshot shows the output you can expect after running the example ABAP AI report. The response from the LLM will be displayed line by line in the SAP GUI output window.

![Output of the ABAP AI LLM quickstart application](../images/QuickstartReportRunOpenAI.png)


### Running a simple ABAP AI OpenAI Chat Application

The ABAP AI Chat stores all conversation exchanges in memory. At any time, you can retrieve the full conversation history, allowing you to review previous messages or continue the dialogue seamlessly.

```abap
REPORT yaai_r_simple_llm_chat_openai.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.

PARAMETERS: p_model  TYPE string DEFAULT 'gpt-4.1' LOWER CASE VISIBLE LENGTH 20,
            p_prompt TYPE string DEFAULT 'What is the capital of France?' LOWER CASE VISIBLE LENGTH 50.

SELECTION-SCREEN: SKIP 1,

BEGIN OF LINE,
PUSHBUTTON 68(10) button USER-COMMAND cli1,
END OF LINE.
SELECTION-SCREEN END OF BLOCK b1.

INITIALIZATION.

  "Set text for the selection screen fields and button
  %_p_model_%_app_%-text = 'Model'.
  %_p_prompt_%_app_%-text = 'Prompt'.
  button = 'Send'.

  DATA(o_aai_conn) = NEW ycl_aai_conn( ).

  o_aai_conn->set_base_url( i_base_url = 'https://api.openai.com' ).

  " The hardcoded API key in this example is intended for testing and development only.
  " Hardcoding API keys directly into your ABAP code is highly discouraged.
  o_aai_conn->set_api_key( i_api_key = 'REPLACE_THIS_TEXT_WITH_YOUR_OPENAI_API_KEY' ).

  DATA(o_aai_openai) = NEW ycl_aai_openai( i_model = p_model i_o_connection = o_aai_conn ).


AT SELECTION-SCREEN.

  o_aai_openai->chat(
    EXPORTING
      i_message = p_prompt
  ).

  DATA(json) = /ui2/cl_json=>serialize(
    EXPORTING
      data        = o_aai_openai->get_conversation( )
      compress    = abap_true
      pretty_name = abap_true

  ).

  IF json IS NOT INITIAL.

    cl_demo_output=>display_json( json ).

  ENDIF.
``` 

**Result:**

The following screenshots show the output you can expect after running the example ABAP AI Chat report.

![Output of the ABAP AI OpenAI Chat quickstart application](../images/QuickstartReportRunOpenAIChat_1.png)

![Output of the ABAP AI OpenAI Chat quickstart application](../images/QuickstartReportRunOpenAIChat_2.png)

![Output of the ABAP AI OpenAI Chat quickstart application](../images/QuickstartReportRunOpenAIChat_3.png)

![Output of the ABAP AI OpenAI Chat quickstart application](../images/QuickstartReportRunOpenAIChat_4.png)


Now that you've run your first OpenAI ABAP AI applications, consider exploring additional features.😊

## Explore ABAP AI Features
  - **Prompt Templates:** Learn how to define and use [prompt templates](prompt_templates.md) for more dynamic and reusable prompts.
  - **LLM System Instructions:** Pass [system instructions](system_instructions.md) to guide the behavior of the language model for specific tasks.
  - **Tool/Function Calling:** Enable LLM applications to trigger ABAP logic or workflows.
  - **Retrieval-Augmented Generation (RAG):** Enhance your LLM applications by incorporating external knowledge sources and retrieval mechanisms.

## OpenAI API Documentation
  - [OpenAI developer platform](https://platform.openai.com/docs/overview)  