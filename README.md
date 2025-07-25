# yaai - ABAP AI tools
This repository provides a set of tools designed to facilitate Artificial Intelligence capabilities within ABAP environments.

## Key Features
- **Multi-Provider LLM API Support**: ABAP AI tools support seamless integration with multiple large language model APIs, giving you flexibility in choosing your provider and deployment model. The list of supported APIs will continue to grow as the project evolves. Currently supported APIs include:
  - **[OpenAI](docs/openai/README.md)** (cloud-based)
  - **[Ollama](docs/ollama/README.md)** (local/self-hosted)
  - **[Google Gemini](docs/google/README.md)** (cloud-based)
  - **[Anthropic](docs/anthropic/README.md)** (cloud-based)
  - APIs from other providers, such as Nvidia and Mistral, that implement the OpenAI API standards also work with ABAP AI tools, possibly requiring minor adjustments.
    - **[Nvidia](docs/openai_compatible/nvidia.md)** (cloud-based)
    - **[Mistral](docs/openai_compatible/mistral.md)** (cloud-based)
- **Conversation Management**: Maintain and manage multi-turn conversations, with full access to conversation history for advanced chat scenarios.
- **Tool/Function Calling**: Integrate ABAP business logic with LLMs using function/tool calling, allowing AI models to trigger ABAP methods.
- **Retrieval-Augmented Generation (RAG)**: Enhance LLM outputs by incorporating enterprise data, enabling more accurate and relevant answers through retrieval-augmented generation workflows.

These features empower you to build intelligent, enterprise-ready ABAP applications that leverage the latest advancements in AI.

This repository focuses on backend AI integration for ABAP. For a ready-to-use user interface—including chat and code assistance apps—check out our companion project: 

  [**yaai ui - ABAP AI User Interface**](https://github.com/christianjianelli/yaai_ui)
  The ABAP AI UI repository provides interfaces to interact with LLMs directly from your SAP system.
  
  <p>
    <img src="docs/images/abap_ai_ui_chat.png" alt="ABAP AI UI Chat" width="500px">
    <br><a href="https://github.com/christianjianelli/yaai_ui">ABAP AI Chat</a>
    <br>
    <br>
    <img src="docs/images/yaai_ui_code_assist.png" alt="ABAP AI UI Code Assistant" width="500px">
    <br><a href="https://github.com/christianjianelli/yaai_ui">ABAP AI Code Assistant</a>
  </p>

## Installation
You can install the ABAP AI tools into your SAP system using abapGit. 

  **Disclaimer:** ABAP AI tools is experimental and released under the MIT License. It is provided "as is", without warranty of any kind, express or implied. This means you use these tools at your own risk, and the authors are not liable for any damages or issues arising from their use.

## Prerequisites
 - **ABAP 7.52+**: You need an SAP system running ABAP version 7.52 or higher.
 - **abapGit**: Ensure that `abapGit` is installed and configured in your ABAP system. If not, you can find the latest version and installation instructions on the official abapGit website: https://docs.abapgit.org/
 - **Developer Access**: You need appropriate developer authorizations in your ABAP system to import objects.

## Installation Steps
1 - **Open abapGit**: In your SAP GUI, execute transaction `ZABAPGIT` (or the equivalent transaction code you have set up for abapGit).

2 - **Add Online Repository**:
  - Click on the `+` button (Add Online Repo) or select "New Online" from the menu.

3 - **Enter Repository URL**:
  - In the "URL" field, paste the URL of this GitHub repository: `https://github.com/christianjianelli/yaai.git`
  - For the **Package**, we recommend creating a new package called `YAAI`. Remember to assign it to a transport request if necessary.
  - Click "OK" or press Enter.

4 - **Clone Repository**:
  - `abapGit` will display the repository details. Review the objects that will be imported.
  - Click the "Clone" button (often represented by a green download icon).

5 - **Activate Objects**:
  - Once the cloning process is complete, `abapGit` will list the imported objects.
  - Activate any inactive objects if prompted.

6 - **Verify Installation**:
  - After activation, all the `ABAP AI tools` objects (interfaces, classes, etc.) will be available in your specified package. You can verify this by checking transaction `SE80` for the package you used.

You have now successfully installed the `ABAP AI tools!`

## Quickstart

### Running Your First ABAP AI Application

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

**Complete Example:**

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

![Output of the ABAP AI LLM quickstart application](docs/images/QuickstartReportRunOpenAI.png)


### Running a simple ABAP AI Chat Application

The ABAP AI Chat stores all conversation exchanges in memory. At any time, you can retrieve the full conversation history, allowing you to review previous messages or continue the dialogue seamlessly.

#### Complete Example

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

![Output of the ABAP AI Chat quickstart application](docs/images/QuickstartReportRunOpenAIChat_1.png)

![Output of the ABAP AI Chat quickstart application](docs/images/QuickstartReportRunOpenAIChat_2.png)

![Output of the ABAP AI Chat quickstart application](docs/images/QuickstartReportRunOpenAIChat_3.png)

![Output of the ABAP AI Chat quickstart application](docs/images/QuickstartReportRunOpenAIChat_4.png)


## Next Steps

  Now that you've run your first ABAP AI applications, consider exploring additional features. 😊

  ### Choose Your API:

  To get started with your preferred API, check out the dedicated guides:

  - [OpenAI Guide](docs/openai/README.md): Learn how to use ABAP AI with OpenAI models.
  - [Ollama Guide](docs/ollama/README.md): Learn how to use ABAP AI with with local or self-hosted Ollama models.
  - [Google Gemini Guide](docs/google/README.md): Learn how to use ABAP AI with Google Gemini models.
  - [Anthropic Guide](docs/anthropic/README.md): Learn how to use ABAP AI with Anthropic models.

  ### Read the documentation

  - [ABAP AI tools Documentation](docs/README.md)