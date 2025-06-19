# ABAP AI Tools
This repository provides a set of tools designed to facilitate Artificial Intelligence capabilities within ABAP environments.

## Installation
You can install the ABAP AI Tools into your SAP system using abapGit. Follow the steps below:

## Prerequisites
 - **abapGit**: Ensure that `abapGit` is installed and configured in your ABAP system. If not, you can find the latest version and installation instructions on the official abapGit website: https://docs.abapgit.org/
 - **Developer Access**: You need appropriate developer authorizations in your ABAP system to import objects.

## Installation Steps
1 - **Open abapGit**: In your SAP GUI, execute transaction `ZABAPGIT` (or the equivalent transaction code you have set up for abapGit).

2 - **Add Online Repository**:
  - Click on the `+` button (Add Online Repo) or select "New Online" from the menu.

3 - **Enter Repository URL**:
  - In the "URL" field, paste the URL of this GitHub repository: `https://github.com/christianjianelli/yaai`
  - For the **Package**, we recommend creating a new package called `YAAI`. Remember to assign it to a transport request if necessary.
  - Click "OK" or press Enter.

4 - **Clone Repository**:
  - `abapGit` will display the repository details. Review the objects that will be imported.
  - Click the "Clone" button (often represented by a green download icon).

5 - **Activate Objects**:
  - Once the cloning process is complete, `abapGit` will list the imported objects.
  - Activate any inactive objects if prompted.

6 - **Verify Installation**:
  - After activation, all the `ABAP AI Tools` objects (interfaces, classes, etc.) will be available in your specified package. You can verify this by checking transaction `SE80` for the package you used.

You have now successfully installed the `ABAP AI Tools!`

## Quickstart

**Running Your First LLM ABAP Application**

This quickstart demonstrates how to create a simple LLM application. It shows you how to connect to the LLM and perform a basic chat interaction.

**Requirements:** 
*   You have a valid OpenAI API Key.
*   Import OpenAI server certificates into SAP trust manager. The [abapGit documentation](https://docs.abapgit.org/user-guide/setup/ssl-setup.html) explains in detail how to do it.

**Steps:**
1.  Create an ABAP AI Connection instance;
2.  Set the Base URL;
3.  Set the API Key;
4.  Create an ABAP AI OpenAI instance;
5.  Call the CHAT method.

**Example:**

```ABAP

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
