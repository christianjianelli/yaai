# yaai - ABAP AI Tools: Retrieval-Augmented Generation (RAG)

## Introduction: Why RAG Matters
Every SAP system is unique, filled with custom "Z-processes" and enhancements that go far beyond standard documentation. A Large Language Model (LLM), no matter how advanced, cannot know your company's specific "ZRET" goods return process. Retrieval-Augmented Generation (RAG) bridges this gap by connecting AI to your organization's private knowledge base.

## What is RAG?
RAG is a technique that boosts the accuracy and relevance of LLM responses by retrieving information from your internal documentation. Instead of relying solely on generic training data, RAG enables the LLM to access up-to-date, company-specific knowledge, making its answers factual and tailored to your processes.

## Implementing RAG using Vector Databases

### Step 1: Building the Knowledge Base
Imagine your company manages customer returns through a custom transaction, "ZRET." The rules are scattered across Word documents and PDFs on internal servers. Before RAG can answer questions, you must build a searchable knowledge base:

1. **Gather and Curate:** Collect all relevant documentation for the "ZRET" process, including Word, PDF, and text files from various repositories.
2. **Chunk and Process:** Break documents into smaller, logical sections (such as paragraphs or topics). This makes retrieval precise and efficient.
3. **Embed and Store:** Use an embedding model to convert each chunk into a numerical vector representing its meaning. Store these vectors and their original text in a specialized **Vector Database**.

This transforms your unstructured documentation into an organized, searchable digital library—ready for real-time queries.

### Step 2: The RAG Workflow
The RAG process consists of three main steps:

1. **Retrieve:** When you ask a question, the system searches your knowledge base for relevant information.
2. **Augment:** The retrieved information is attached to your original query, giving the LLM essential context.
3. **Generate:** The LLM uses this augmented prompt to generate a precise, fact-based answer.

### Example Scenario
Suppose a new team member asks:

*"For the ZRET process, what are the approval steps required for a customer return valued over €10,000?"*

A standard LLM cannot answer this, as "ZRET" is unique to your company. Here’s how RAG solves the problem:

1. **Understanding the Query:** The system converts the user's question into a vector using the same embedding model.
2. **Semantic Search:** It searches the Vector Database for documentation chunks most similar to the query—understanding meaning, not just keywords.
3. **Grounded Answer Generation:** The most relevant text is retrieved and combined with the original question. The LLM receives an augmented prompt like:

    > *Context from internal documents: "For all returns processed via transaction ZRET with a value exceeding €10,000, approval from both the regional Sales Manager and the Finance Controller is mandatory. Approvals must be logged in the ZRET_APPR table."*
    >
    > *User's Question: For the ZRET process, what are the approval steps required for a customer return valued over €10,000?*

Armed with precise, company-specific facts, the LLM can now generate a trustworthy answer and cite its source.

## Using RAG in ABAP AI Tools
ABAP AI tools do not provide a built-in feature to interact directly with vector databases. However, they offer the `yif_aai_rag` interface, which you can use to implement your own RAG solution and integrate it with ABAP AI tools.

The class `ycl_aai_rag_chroma` is provided as a simple example of how to implement a RAG system using a [Chroma](https://docs.trychroma.com/docs/overview/introduction) vector database. The example demonstrates how to retrieve context from a vector database and how to use a prompt template to generate the augmented prompt.

The following report demonstrates a basic implementation of Retrieval-Augmented Generation (RAG) within ABAP AI tools.

```abap
REPORT yaai_r_simple_rag_chroma.

TYPES: BEGIN OF ty_params_s,
         context TYPE string,
         prompt  TYPE string,
       END OF ty_params_s.

DATA s_params TYPE ty_params_s.

PARAMETERS p_prompt TYPE c LENGTH 100 LOWER CASE OBLIGATORY.
PARAMETERS p_model  TYPE c LENGTH 30 LOWER CASE OBLIGATORY DEFAULT 'gpt-4.1'.

DATA system_instructions TYPE string.

START-OF-SELECTION.

  DATA(o_aai_openai) = ycl_aai_openai=>get_instance( p_model ).

  system_instructions = |**Role**: You are a **helpful SAP assistant** specialized in the `ZSD_PLAN_DATES` transaction.|.
  system_instructions = |{ system_instructions } Your task is to answer user questions **only using the provided documentation**.|.
  system_instructions = |{ system_instructions } If the information is not in the documentation, respond politely that you cannot answer.\n|.
  system_instructions = |{ system_instructions } \n\n|.
  system_instructions = |{ system_instructions } **Instructions**:  |.
  system_instructions = |{ system_instructions } 1. **Context-Based Responses**:\n|.
  system_instructions = |{ system_instructions }    - Answer questions **exclusively** using the provided [Documentation] and [FAQ].\n|.
  system_instructions = |{ system_instructions }    - If the answer isn’t in either, say:  \n|.
  system_instructions = |{ system_instructions }      *"I’m sorry, but my knowledge is limited to the official documentation for `ZSD_PLAN_DATES`. |.
  system_instructions = |{ system_instructions }      I don’t have information on this topic. Please consult SAP Support or your system administrator for further help.\n|.
  system_instructions = |{ system_instructions } \n\n|.
  system_instructions = |{ system_instructions } 2. **Strict Adherence**:  \n|.
  system_instructions = |{ system_instructions }    - **Do not** speculate, infer, or invent answers.\n|.
  system_instructions = |{ system_instructions }    - **Do not** use external/pre-trained knowledge about SAP unless it matches the provided docs.\n|.
  system_instructions = |{ system_instructions } \n\n|.
  system_instructions = |{ system_instructions } 3. **Helpful Tone**:  \n|.
  system_instructions = |{ system_instructions }    - Be concise, technical, and user-friendly.\n|.
  system_instructions = |{ system_instructions }    - For complex queries, break answers into bullet points or tables (like the FAQ).\n|.
  system_instructions = |{ system_instructions } \n\n|.
  system_instructions = |{ system_instructions } 4. **Examples**:  \n|.
  system_instructions = |{ system_instructions }    - **User**: *"How is the Estimated Delivery Date calculated?"*\n|.
  system_instructions = |{ system_instructions }      **You**: *"According to the documentation, it’s calculated as: Loading Date + Transportation Lead Time (configured in SPRO)."*\n|.
  system_instructions = |{ system_instructions }    - **User**: *"How do I configure this transaction in S/4HANA Cloud?"*  \n|.
  system_instructions = |{ system_instructions }      **You**: *"I’m sorry, but my knowledge is limited to the provided documentation, which doesn’t cover S/4HANA Cloud. Please contact your IT team."*\n|.

  o_aai_openai->set_system_instructions( system_instructions ).

  s_params-prompt = p_prompt.

  DATA(o_aai_rag) = NEW ycl_aai_rag_chroma( i_api = 'CHROMA' i_endpoint = '' ).

  o_aai_rag->get_context(
    EXPORTING
      i_input   = p_prompt
    IMPORTING
      e_context = s_params-context
  ).

  DATA(o_aai_prompt_template) = NEW ycl_aai_prompt_template(
    i_template_text = |Please answer the question using the documentation provided.\n\n**Question**: %PROMPT% \n\n**Documentation**:\n\n %CONTEXT% \n\n |
  ).

  IF s_params-context IS NOT INITIAL.

    DATA(o_aai_prompt) = NEW ycl_aai_prompt( ).

    o_aai_prompt->generate_prompt_from_template(
      EXPORTING
        i_o_template = o_aai_prompt_template
        i_s_params   = s_params
      RECEIVING
        r_prompt     = s_params-prompt
    ).

  ENDIF.

  o_aai_openai->generate(
    EXPORTING
      i_message  = s_params-prompt
      i_new      = abap_true
    IMPORTING
      e_response = DATA(response)
      e_t_response = DATA(t_response)
  ).

  LOOP AT t_response ASSIGNING FIELD-SYMBOL(<s_line>).
    WRITE : / <s_line>.
  ENDLOOP.
```

For completeness, the ABAP example above interacts with a local API implemented in Python using FastAPI, LangChain, and Ollama. 
The design and development of this API are outside the scope of the ABAP AI tools documentation. 

```python
from fastapi import FastAPI
from langchain_chroma import Chroma
from langchain_ollama import OllamaEmbeddings
from pydantic import BaseModel

# Define a Pydantic model for your expected JSON structure
class Prompt(BaseModel):
    input: str

app = FastAPI()
@app.post("/chroma/")
def chroma_endpoint(prompt: Prompt):
    """
    Endpoint to handle Chroma vector store operations.
    """
    # Here you would typically process the data and interact with the Chroma vector store.

    embeddings = OllamaEmbeddings(model="mxbai-embed-large")

    vector_store = Chroma(
        collection_name="zsd_plan_dates",
        embedding_function=embeddings,
        persist_directory="./zsd_documentation"  # Adjust the path as needed, 
    )

    count = vector_store._collection.count()

    results = vector_store.similarity_search(
        prompt.input,
        k=count
    )

    # Sort by the numeric part of the id
    sorted_results = sorted([dict(item) for item in results], key=lambda x: int(x["id"].split("-")[1]))

    return sorted_results
```